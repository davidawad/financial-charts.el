;;; financial-chart-video.el --- Deterministic frames for the docs videos -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Frames for docs/videos (scripts/record-videos drives this file,
;; rasterizes the frames with rsvg-convert and encodes them with
;; ffmpeg).  Everything runs headless on a virtual clock: eas stream
;; timers are off, `eas-stream-clock' reads the frame's timestamp and
;; each frame is ticked by hand, so a frame's content depends only on
;; the seed and the frame number, never on how long it took to draw.
;;
;; - `financial-chart-video-feed-make' is a seeded order-book feed:
;;   prices on a 0.01 tick, sizes random-walking, levels inserted and
;;   deleted, and the touch moving now and then.
;; - `financial-chart-video-book-frames' pushes it into a live
;;   ladder or depth-live view with `financial-chart-book-push' and
;;   draws each frame with `eas-svg-render' (or `eas-text-render').
;; - `financial-chart-video-candle-frames' streams bars into a
;;   composed candlestick chart, the last bar forming over several
;;   frames, with its indicators recomputed every frame.
;; - `financial-chart-video-text-svg' turns a text rendering into an
;;   SVG terminal screen (monospace, faces as colours).
;;
;; Run `financial-chart-video-main' with OUT-DIR to write every frame.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart)
(require 'financial-chart-eas-book)
(require 'financial-chart-eas-compose)
(require 'eas-svg)
(require 'eas-text)

(defvar eas-live-budget-factor)

(defvar financial-chart-video-fps 30 "Frames per second of every video.")
(defvar financial-chart-video-seconds 12 "Length of every video in seconds.")
(defvar financial-chart-video-levels 20 "Price levels drawn per side of a book.")

;;; Seeded random numbers

(defun financial-chart-video--rand (state)
  "Next uniform number in [0, 1) from STATE, a cons whose car is the seed."
  (setcar state (mod (+ (* (car state) 1103515245) 12345) 2147483648))
  (/ (car state) 2147483648.0))

(defun financial-chart-video--below (state n)
  "A seeded integer in [0, N)."
  (min (1- n) (floor (* n (financial-chart-video--rand state)))))

;;; Order-book feed

(cl-defstruct (financial-chart-video-feed (:constructor financial-chart-video-feed--make) (:copier nil))
  "A simulated book in integer ticks: price-tick -> size per side."
  rng (bids (make-hash-table)) (asks (make-hash-table)))

(defconst financial-chart-video--depth 32
  "Levels kept per side, so deletes never empty the drawn ladder.")

(defun financial-chart-video--price (tick)
  "TICK (hundredths) as a price."
  (/ tick 100.0))

(defun financial-chart-video--size (rng)
  "A fresh level size from RNG: 1 to 120 lots, mostly small."
  (1+ (floor (* 120 (expt (financial-chart-video--rand rng) 2)))))

(defun financial-chart-video-feed-make (&optional seed)
  "A seeded feed around 100.00 (SEED defaults to 20261007)."
  (let ((feed (financial-chart-video-feed--make :rng (list (or seed 20261007)))))
    (let ((rng (financial-chart-video-feed-rng feed)) (bid 9998) (ask 10002))
      (dotimes (_ financial-chart-video--depth)
        (puthash bid (financial-chart-video--size rng) (financial-chart-video-feed-bids feed))
        (puthash ask (financial-chart-video--size rng) (financial-chart-video-feed-asks feed))
        ;; Mostly contiguous, with the odd empty tick.
        (setq bid (- bid 1 (if (< (financial-chart-video--rand rng) 0.15) 1 0))
              ask (+ ask 1 (if (< (financial-chart-video--rand rng) 0.15) 1 0)))))
    feed))

(defun financial-chart-video--levels (table side)
  "TABLE's ticks, best first for SIDE (:bids or :asks)."
  (sort (hash-table-keys table) (if (eq side :bids) #'> #'<)))

(defun financial-chart-video-feed-snapshot (feed)
  "FEED's book as a snapshot for `financial-chart-book-make'."
  (cl-flet ((side (s table) (vconcat (mapcar (lambda (tick) (vector (financial-chart-video--price tick)
                                                                   (gethash tick table)))
                                             (financial-chart-video--levels table s)))))
    (list :bids (side :bids (financial-chart-video-feed-bids feed))
          :asks (side :asks (financial-chart-video-feed-asks feed)))))

(defun financial-chart-video--delta (op side tick &optional size)
  "A delta plist."
  (append (list :op op :side (if (eq side :bids) "bid" "ask") :price (financial-chart-video--price tick))
          (and size (list :size size))))

(defun financial-chart-video--event (feed side)
  "One random change to FEED's SIDE, applied; return its deltas."
  (let* ((rng (financial-chart-video-feed-rng feed))
         (table (if (eq side :bids) (financial-chart-video-feed-bids feed) (financial-chart-video-feed-asks feed)))
         (other (if (eq side :bids) (financial-chart-video-feed-asks feed) (financial-chart-video-feed-bids feed)))
         (levels (financial-chart-video--levels table side))
         (best (car levels))
         (other-best (car (financial-chart-video--levels other (if (eq side :bids) :asks :bids))))
         (away (if (eq side :bids) -1 1))
         (roll (financial-chart-video--rand rng))
         ;; Activity concentrates near the touch.
         (pick (nth (floor (* (length levels) (expt (financial-chart-video--rand rng) 2.2))) levels)))
    (cond
     ;; Size random walk: about 1 in 4 lots up or down.
     ((< roll 0.72)
      (let ((size (max 1 (round (* (gethash pick table)
                                   (exp (* 0.5 (- (financial-chart-video--rand rng) 0.5))))))))
        (if (= size (gethash pick table)) nil
          (puthash pick size table)
          (list (financial-chart-video--delta "update" side pick size)))))
     ;; Insert into an empty tick behind the touch, or improve it; a
     ;; spread wider than 2 ticks is mostly improved, so it mean-reverts.
     ((< roll 0.84)
      (let ((tick (if (and (> (abs (- other-best best)) 2)
                           (< (financial-chart-video--rand rng) 0.8))
                      (- best away)
                    (+ best (* away (- (financial-chart-video--below rng 14) 1))))))
        (when (and (not (gethash tick table))
                   (if (eq side :bids) (< tick other-best) (> tick other-best)))
          (let ((size (financial-chart-video--size rng)))
            (puthash tick size table)
            (list (financial-chart-video--delta "insert" side tick size))))))
     ;; Delete a level that is not the touch.
     ((< roll 0.95)
      (when (and (not (eql pick best)) (> (length levels) financial-chart-video--depth))
        (remhash pick table)
        (list (financial-chart-video--delta "delete" side pick))))
     ;; The touch is taken out (while the spread is under 5 ticks): the best
     ;; level goes, a level is refilled far out.
     (t
      (when (and (> (length levels) (+ 2 financial-chart-video-levels))
                 (< (abs (- other-best best)) 5))
        (let ((far (+ (car (last levels)) away)) (size (financial-chart-video--size rng)))
          (remhash best table)
          (puthash far size table)
          (list (financial-chart-video--delta "delete" side best)
                (financial-chart-video--delta "insert" side far size))))))))

(defun financial-chart-video-feed-step (feed)
  "The next frame's delta batch from FEED (a vector, possibly empty)."
  (let ((rng (financial-chart-video-feed-rng feed)) out)
    (dotimes (_ (financial-chart-video--below rng 5))
      (setq out (append out (financial-chart-video--event
                             feed (if (< (financial-chart-video--rand rng) 0.5) :bids :asks)))))
    ;; Top a thinned side back up so the ladder stays full.
    (dolist (side '(:bids :asks))
      (let* ((table (if (eq side :bids) (financial-chart-video-feed-bids feed) (financial-chart-video-feed-asks feed)))
             (levels (financial-chart-video--levels table side)))
        (when (< (length levels) financial-chart-video--depth)
          (let ((far (+ (car (last levels)) (if (eq side :bids) -1 1)))
                (size (financial-chart-video--size rng)))
            (puthash far size table)
            (setq out (append out (list (financial-chart-video--delta "insert" side far size))))))))
    (vconcat out)))

;;; Book frames

(defun financial-chart-video--time (i)
  "Timestamp of frame I."
  (/ (float i) financial-chart-video-fps))

(cl-defun financial-chart-video-book-frames (template frames sink &key (seed 20261007) (backend 'svg)
                                                      width height (levels financial-chart-video-levels))
  "Draw FRAMES frames of the seeded feed on a live TEMPLATE book view.
TEMPLATE is \"ladder\" or \"depth-live\".  Each frame pushes that
frame's deltas with `financial-chart-book-push' at the frame's
timestamp, takes the stream frame and calls SINK with the frame
index, the drawing (SVG string, or text with BACKEND `text') and the
view.  WIDTH and HEIGHT are pixels, or columns and rows for text;
LEVELS are drawn per side."
  (let* ((feed (financial-chart-video-feed-make seed))
         (now 0.0)
         (eas-stream-use-timers nil)
         ;; Offline frames at fixed timestamps: never drop one for its real cost.
         (eas-live-budget-factor 0)
         (eas-stream-clock (lambda () (+ now 0.001)))
         (view (financial-chart-book-open
                (financial-chart-video-feed-snapshot feed)
                :template template :levels levels :flash 0.5
                :max-fps financial-chart-video-fps :subject (format "video-%s" (random))
                :title "Simulated feed, 0.01 tick" :target backend
                :size (if (eq backend 'text) (list :cols width :rows height)
                        (and width height (cons width height))))))
    (unwind-protect
        (dotimes (i frames)
          (setq now (financial-chart-video--time i))
          (financial-chart-book-push view (financial-chart-video-feed-step feed) now)
          (eas-stream-tick view (+ now 0.001))
          (funcall sink i (if (eq backend 'text) (eas-text-render (eas-view-scene view))
                            (eas-svg-render (eas-view-scene view)))
                   view))
      (financial-chart-book-close view))))

;;; Candle frames

(defun financial-chart-video--bars (n)
  "N seeded daily bars from 2026-01-05, each with its intrabar close path."
  (let ((rng (list 20261007)) (close 100.0) (day (eas-time-ms 2026 1 5)) bars)
    (dotimes (i n)
      (let* ((drift (* 0.8 (sin (/ i 11.0))))
             (open close)
             (next (/ (fround (* (+ open drift (* 2.6 (- (financial-chart-video--rand rng) 0.5))) 100)) 100))
             (high (/ (fround (* (+ (max open next) (* 1.1 (financial-chart-video--rand rng))) 100)) 100))
             (low (/ (fround (* (- (min open next) (* 1.1 (financial-chart-video--rand rng))) 100)) 100)))
        (push (list :time (format-time-string "%Y-%m-%d" (/ day 1000) t)
                    :open open :high high :low low :close next
                    :volume (+ 700000 (round (* 900000 (financial-chart-video--rand rng)))))
              bars)
        (setq day (+ day (* 86400000 (if (= (mod i 5) 4) 3 1))) close next)))
    (nreverse bars)))

(defun financial-chart-video--forming (bar progress)
  "BAR as seen PROGRESS (0 to 1) of the way through its session."
  (if (>= progress 1) bar
    (let* ((open (plist-get bar :open)) (close (plist-get bar :close))
           ;; Visit the low then the high before settling on the close.
           (path (cond ((< progress 0.35) (+ open (* (/ progress 0.35) (- (plist-get bar :low) open))))
                       ((< progress 0.7) (+ (plist-get bar :low)
                                            (* (/ (- progress 0.35) 0.35) (- (plist-get bar :high) (plist-get bar :low)))))
                       (t (+ (plist-get bar :high) (* (/ (- progress 0.7) 0.3) (- close (plist-get bar :high)))))))
           (last (/ (fround (* path 100)) 100)))
      (list :time (plist-get bar :time) :open open
            :high (max open last (if (>= progress 0.7) (plist-get bar :high) open))
            :low (min open last (if (>= progress 0.35) (plist-get bar :low) open))
            :close last :volume (max 1 (round (* progress (plist-get bar :volume))))))))

(defun financial-chart-video--chart (bars)
  "The composed chart description drawing BARS."
  (list :title "Bars streaming in: SMA 10/30, volume, RSI 14, MACD"
        :bars (vconcat bars)
        :price '(:style "candles"
                 :series [(:indicator "sma" :params [10]) (:indicator "sma" :params [30] :dash [4 2])]
                 :fills [(:between ["sma-10" "sma-30"] :above "#26a69a" :below "#ef5350" :opacity 0.25)])
        :panes '[(:volume t :height 50)
                 (:series [(:indicator "rsi" :params [14])] :rules [30 70])
                 (:series [(:indicator "macd" :output "macd") (:indicator "macd" :output "macd-signal")
                           (:indicator "macd" :output "macd-histogram" :above "#26a69a" :below "#ef5350")]
                  :rules [0])]))

(cl-defun financial-chart-video-candle-frames (frames sink &key (backend 'svg) width height
                                                      (frames-per-bar 15) (window 60) (history 60))
  "Draw FRAMES frames of bars streaming into a composed candle chart.
HISTORY bars are there at the start; a new bar opens every
FRAMES-PER-BAR frames and forms over them.  The last WINDOW bars are
drawn, indicators computed over them.  SINK gets the
frame index and the drawing from `financial-chart-compose-render'."
  (let ((bars (financial-chart-video--bars (+ history 2 (ceiling frames frames-per-bar)))))
    (dotimes (i frames)
      (let* ((done (+ history (/ i frames-per-bar)))
             (progress (/ (float (1+ (% i frames-per-bar))) frames-per-bar))
             (shown (append (seq-take bars done)
                            (list (financial-chart-video--forming (nth done bars) progress))))
             (shown (seq-subseq shown (max 0 (- (length shown) window)))))
        (funcall sink i (financial-chart-compose-render
                         (financial-chart-video--chart shown)
                         :backend backend :width width :height height))))))

;;; Text frames as SVG

(defvar financial-chart-video-text-colors
  '(:background "#1d1f21" :foreground "#c5c8c6" :axis "#707880")
  "Terminal colours for text frames.")

(defun financial-chart-video--face-attrs (face)
  "FACE (a face symbol, attribute plist or list of those) as (FG BG BOLD)."
  (cond ((null face) (list nil nil nil))
        ((eq face 'eas-axis) (list (plist-get financial-chart-video-text-colors :axis) nil nil))
        ((eq face 'eas-title) (list nil nil t))
        ((symbolp face) (list nil nil nil))
        ((keywordp (car-safe face))
         (list (plist-get face :foreground) (plist-get face :background)
               (memq (plist-get face :weight) '(bold semi-bold extra-bold ultra-bold))))
        ((consp face)
         (let ((out (list nil nil nil)))
           (dolist (f face out)
             (cl-mapc (lambda (i v) (unless (nth i out) (setf (nth i out) v)))
                      '(0 1 2) (financial-chart-video--face-attrs f)))))
        (t (list nil nil nil))))

(defun financial-chart-video--xml (s)
  "S escaped for SVG text."
  (replace-regexp-in-string
   "[<>&]" (lambda (m) (pcase m ("<" "&lt;") (">" "&gt;") (_ "&amp;"))) s t t))

(cl-defun financial-chart-video-text-svg (text &key cols rows width height (font "Source Code Pro"))
  "TEXT (a propertized text rendering) as a WIDTH x HEIGHT SVG terminal.
The text grid is COLS by ROWS cells; each face run is placed at its
cell, coloured by its face, on a dark background."
  (let* ((cw (/ (float width) cols)) (ch (/ (float height) rows))
         (size (* ch 0.8)) (y-off (* ch 0.78))
         (bg0 (plist-get financial-chart-video-text-colors :background))
         (fg0 (plist-get financial-chart-video-text-colors :foreground))
         (out (list (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"><rect width=\"100%%\" height=\"100%%\" fill=\"%s\"/><g font-family=\"%s, DejaVu Sans Mono, monospace\" font-size=\"%.2f\" xml:space=\"preserve\">"
                            width height width height bg0 font size))))
    (cl-loop for line in (split-string text "\n") for r from 0 below rows do
             (let ((pos 0) (col 0) (len (length line)))
               (while (< pos len)
                 (let* ((next (or (next-single-property-change pos 'face line) len))
                        (run (substring-no-properties line pos next))
                        (attrs (financial-chart-video--face-attrs (get-text-property pos 'face line)))
                        (x (* col cw)) (y (* r ch)))
                   (when (nth 1 attrs)
                     (push (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" fill=\"%s\"/>"
                                   x y (* cw (length run)) ch (nth 1 attrs))
                           out))
                   ;; One element per glyph keeps it on its cell whatever the font.
                   (cl-loop for c across run for k from 0
                            unless (memq c '(?\s ?\t))
                            do (push (format "<text x=\"%.1f\" y=\"%.1f\" fill=\"%s\"%s>%s</text>"
                                             (+ x (* k cw)) (+ y y-off) (or (nth 0 attrs) fg0)
                                             (if (nth 2 attrs) " font-weight=\"bold\"" "")
                                             (financial-chart-video--xml (string c)))
                                     out))
                   (setq col (+ col (length run)) pos next)))))
    (push "</g></svg>" out)
    (apply #'concat (nreverse out))))

;;; Driver

(defun financial-chart-video--writer (dir &optional text-size)
  "A sink writing frame I to DIR/NNNNN.svg; TEXT-SIZE (COLS ROWS W H) wraps text."
  (make-directory dir t)
  (lambda (i drawing &rest _)
    (let ((coding-system-for-write 'utf-8))
      (write-region (if text-size
                        (financial-chart-video-text-svg drawing :cols (nth 0 text-size) :rows (nth 1 text-size)
                                                        :width (nth 2 text-size) :height (nth 3 text-size))
                      drawing)
                    nil (expand-file-name (format "%05d.svg" i) dir) nil 'silent))))

(defvar financial-chart-video-clips
  '("ladder" "depth-live" "book-pair" "book-text" "candles" "candles-text")
  "Clips `financial-chart-video-main' can draw.")

(defun financial-chart-video-write (clip dir)
  "Write every frame of CLIP into DIR (SVG files)."
  (let ((frames (* financial-chart-video-fps financial-chart-video-seconds)))
    (pcase clip
      ((or "ladder" "depth-live")
       (financial-chart-video-book-frames clip frames (financial-chart-video--writer dir)
                                          :width 1280 :height 720))
      ("book-pair"
       (financial-chart-video-book-frames "ladder" frames (financial-chart-video--writer (expand-file-name "left" dir))
                                          :width 640 :height 720)
       (financial-chart-video-book-frames "depth-live" frames (financial-chart-video--writer (expand-file-name "right" dir))
                                          :width 640 :height 720))
      ("book-text"
       (financial-chart-video-book-frames "ladder" frames (financial-chart-video--writer dir '(128 45 1280 720))
                                          :backend 'text :width 128 :height 45 :levels 18))
      ("candles"
       (financial-chart-video-candle-frames frames (financial-chart-video--writer dir) :width 1280 :height 720))
      ("candles-text"
       (financial-chart-video-candle-frames frames (financial-chart-video--writer dir '(128 45 1280 720))
                                            :backend 'text :width 128 :height 45))
      (_ (error "Unknown clip %S; clips: %s" clip (string-join financial-chart-video-clips ", "))))))

(defun financial-chart-video-main ()
  "Batch entry: OUT-DIR [CLIP...] from `command-line-args-left'."
  (let* ((args command-line-args-left)
         (dir (or (car args) (error "Usage: OUT-DIR [CLIP...]")))
         (clips (or (cdr args) financial-chart-video-clips)))
    (setq command-line-args-left nil)
    (dolist (clip clips)
      (let ((start (float-time)))
        (financial-chart-video-write clip (expand-file-name clip dir))
        (message "%s: %d frames in %.1fs" clip
                 (* financial-chart-video-fps financial-chart-video-seconds) (- (float-time) start))))))

(provide 'financial-chart-video)
;;; financial-chart-video.el ends here
