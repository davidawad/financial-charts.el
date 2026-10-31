;;; financial-chart-3d.el --- 3D variants of financial-chart's templates via graphics.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every financial-chart template has a 3D variant in templates3d/:
;; the template with its body unchanged plus an `x-eas.graphics'
;; naming a graphics.el mark (graphics.el's
;; docs/design/eas-integration.md).  Loading this file registers the
;; directory with eas under the namespace "fc3d", so "fc3d/ohlc" is the
;; variant of "ohlc"; eas still draws it in 2D, graphics.el in 3D.
;; scripts/templates3d.py writes the variants; docs/3d.md shows them.
;;
;; graphics.el is an optional dependency.  Without it this file still
;; loads, the variants still draw in 2D, and every 3D command signals
;; `financial-chart-3d-unavailable' saying how to install it.
;;
;;   (financial-chart-3d-open "ohlc" :bindings '(:bars ...) :show t)
;;   M-x financial-chart-3d-gallery     ; every variant, a grid of windows
;;   M-x financial-chart-3d-demo-book   ; the order-book surface, streaming
;;   (financial-chart-3d-book-open SNAPSHOT) + `financial-chart-3d-book-push'
;;
;; `financial-chart-3d-bindings-from-metric' turns what `invest metric
;; run ID' emits into a template's bindings, one adapter per metric and
;; template in each group's file (`financial-chart-3d-metric-templates'
;; lists the pairs).
;;
;; The marks graphics.el lacks are registered here with
;; `graphics-eas-define-mark' when a 3D command loads graphics.el (or at
;; load, when graphics.el is already loaded):
;;
;;   fc-candle3d          candle3d's geometry (bodies as deep as their
;;                        volume) with scales over the whole series, a
;;                        replay colouring the bars in place
;;   fc-panes3d           price, volume and RSI panels stacked on one
;;                        time axis
;;   fc-volume-profile3d  candles plus horizontal volume-at-price bars
;;   fc-drawdown3d        the underwater curve hanging below the
;;                        high-water line
;;   fc-payoff3d          a P/L heightfield over price and days to
;;                        expiry (or curve); expiry-only rows are priced
;;                        back in time (`financial-chart-3d-payoff-surface')
;;   fc-depth3d           cumulative depth walls per side
;;   fc-diverging3d       bar3d coloured by sign
;;
;; The series variants replay along a looping `timeline' whose position
;; the marks read (`financial-chart-3d--phase'): every frame draws all
;; the rows, those not reached yet as a ghost, so no moment of a replay
;; is one bar or one row.  SPC plays or pauses in the 3D buffer.  Each
;; view's camera is fitted to its data (`financial-chart-3d-camera-fit').
;; Headless frames and the goldens show a replay's end (and its middle,
;; test/golden/3d/t0.5/).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'financial-chart)
(require 'financial-chart-eas-book)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function graphics-eas-primitives "graphics-eas-marks" (graphics rows &optional params domain-rows))
(declare-function graphics-eas-variant-spec "graphics-eas" (name &optional bindings))
(declare-function graphics-eas-open "graphics-eas" (source &rest args))
(declare-function graphics-eas-frame "graphics-eas" (view cols rows &optional palette))
(declare-function graphics-eas-show "graphics-eas" (view &optional cols rows window))
(declare-function graphics-eas-timeline "graphics-eas" (view))
(declare-function graphics-eas-timeline-set "graphics-eas" (view &rest args))
(declare-function graphics-eas-braille-text "graphics-eas" (frame cols))
(declare-function graphics-eas-ensure-core "graphics-eas" ())
(declare-function graphics-eas-get "graphics-eas" (view))
(declare-function graphics-eas-view-graphics "graphics-eas" (g))
(declare-function graphics-eas-attach "graphics-eas" (view graphics))
(declare-function graphics-eas-mark-inputs "graphics-eas" (view))
(declare-function graphics-eas-camera "graphics-eas-marks" (graphics params))
(declare-function graphics-anim-bounds "graphics-anim" (primitives))
(declare-function graphics-ensure-core "graphics" (&optional core))
(declare-function graphics-frame-write-file "graphics-frame" (path frame cols rows))
(declare-function eas-play-detach "eas-play" (view))
(declare-function eas-stream-attach "eas-stream" (view &optional options))

(defvar graphics-eas-domain-rows)
(defvar graphics-eas-mark-params)

;;; Templates

(defconst financial-chart-3d-namespace "fc3d"
  "The eas namespace of the 3D variants: \"fc3d/ohlc\" is ohlc's.")

(defconst financial-chart-3d-templates-directory
  (expand-file-name "templates3d" financial-chart-root)
  "Directory of the 3D variants of financial-chart's templates.")

(eas-template-add-directory financial-chart-3d-templates-directory financial-chart-3d-namespace)

(defun financial-chart-3d-variants ()
  "Names of the 3D variants (\"ohlc\", ...), one per financial-chart template."
  (mapcar #'file-name-base
          (directory-files financial-chart-3d-templates-directory nil "\\.json\\'")))

(defun financial-chart-3d--name (name)
  "The registry name of variant NAME (\"ohlc\" or \"fc3d/ohlc\")."
  (let ((prefix (concat financial-chart-3d-namespace "/")))
    (if (string-prefix-p prefix name) name (concat prefix name))))

(defun financial-chart-3d--base (name)
  "The financial-chart template variant NAME draws in 3D (\"ohlc\")."
  (string-remove-prefix (concat financial-chart-3d-namespace "/") name))

;;; graphics.el, optionally

(define-error 'financial-chart-3d-unavailable
  "financial-chart: 3D charts need graphics.el" 'financial-chart-error)

(defvar financial-chart-3d-graphics-feature 'graphics-eas
  "The graphics.el feature the 3D commands load.")

(defun financial-chart-3d-available-p ()
  "Non-nil when graphics.el (its eas integration) can be loaded.
Loading it registers this package's marks (`financial-chart-3d-marks')."
  (when (require financial-chart-3d-graphics-feature nil t)
    (financial-chart-3d--register-marks)
    t))

(defun financial-chart-3d--ensure ()
  "Load graphics.el or signal `financial-chart-3d-unavailable'."
  (unless (financial-chart-3d-available-p)
    (signal 'financial-chart-3d-unavailable
            (list (concat "3D charts need graphics.el, which is not on `load-path': clone "
                          "gitlab.com/davidawad/graphics-el, build its Rust module (make module) "
                          "and add its lisp/ to `load-path' (2D charts need none of it)")
                  :code "GRAPHICS_MISSING"
                  :feature (symbol-name financial-chart-3d-graphics-feature)))))

;;; Pure helpers: rows, fields, scales

(defun financial-chart-3d--key (name)
  "Plist keyword for NAME (a string or keyword)."
  (if (keywordp name) name (intern (concat ":" name))))

(defun financial-chart-3d--opt (graphics name &optional default)
  "Option NAME (a string) of GRAPHICS, DEFAULT when absent or null."
  (let ((v (plist-get graphics (financial-chart-3d--key name))))
    (if (or (null v) (eq v :null)) default v)))

(defun financial-chart-3d--field (graphics channel default)
  "The field keyword of CHANNEL in GRAPHICS's encoding, else DEFAULT's."
  (let* ((spec (plist-get (financial-chart-3d--opt graphics "encoding") (financial-chart-3d--key channel)))
         (name (cond ((stringp spec) spec)
                     ((consp spec) (plist-get spec :field))
                     (t default))))
    (financial-chart-3d--key name)))

(defun financial-chart-3d--value (row key)
  "ROW's value at KEY; nil for null, false or missing."
  (let ((v (plist-get row key)))
    (unless (memq v '(:null :false)) v)))

(defun financial-chart-3d--rows (rows)
  "ROWS as a list: a vector or list of plists, or a data/v1 object."
  (if (keywordp (car-safe rows)) (append (plist-get rows :rows) nil) (append rows nil)))

(defun financial-chart-3d--categories (values)
  "Distinct VALUES: sorted when all are numbers, else in first-seen order."
  (let ((seen (delete-dups (copy-sequence (delq nil values)))))
    (if (seq-every-p #'numberp seen) (sort seen #'<) seen)))

(defun financial-chart-3d--r3 (v)
  "V rounded to 3 decimals, as graphics.el's marks round."
  (/ (fround (* v 1000.0)) 1000.0))

(defun financial-chart-3d--domain (rows)
  "ROWS and the rows a time encoding's scales span (`graphics-eas-domain-rows')."
  (if (and (boundp 'graphics-eas-domain-rows) graphics-eas-domain-rows)
      (append graphics-eas-domain-rows rows)
    rows))

(defun financial-chart-3d--mark-params ()
  "The params the mark is lowered under, if any."
  (and (boundp 'graphics-eas-mark-params) graphics-eas-mark-params))

;; A replay is a `timeline' (duration, loop, autoplay) whose position
;; the marks read from its param.  Every frame draws all the rows: those
;; replayed so far in colour, the rest as a dim ghost, so a frame never
;; collapses to one bar or one row of a surface and the camera, fitted
;; to all of them, holds still.  (A time encoding hands a mark only the
;; rows so far.)

(defconst financial-chart-3d-replay-window (/ 1.0 3)
  "Fraction of a series a replay starts from (at least 8 bars).")

(defconst financial-chart-3d-ghost-color "#4a5568"
  "Colour of the rows a replay has not reached yet.")

(defun financial-chart-3d--phase (graphics)
  "How far GRAPHICS's timeline is, 0 (its start) to 1 (its end).
Read from the timeline's param (`timeline.param', default \"time\",
in seconds) over `timeline.duration'; 1 without a timeline or a
position, so a mark lowered alone draws its whole data."
  (let* ((timeline (financial-chart-3d--opt graphics "timeline"))
         (timeline (and (consp timeline) timeline))
         (duration (plist-get timeline :duration))
         (seconds (plist-get (financial-chart-3d--mark-params)
                             (financial-chart-3d--key (or (plist-get timeline :param) "time")))))
    (if (and (numberp duration) (> duration 0) (numberp seconds))
        (max 0.0 (min 1.0 (/ seconds (float duration))))
      1.0)))

(defun financial-chart-3d--replayed (graphics n)
  "How many of N rows a replay has reached at GRAPHICS's phase.
From `financial-chart-3d-replay-window' of them (at least 8) at the
start to all N at the end."
  (let ((from (min n (max 8 (ceiling (* n financial-chart-3d-replay-window))))))
    (+ from (round (* (financial-chart-3d--phase graphics) (- n from))))))

(defun financial-chart-3d--ghost (primitives)
  "PRIMITIVES (a sequence) but their axes, in `financial-chart-3d-ghost-color'.
Their ids prefixed with \"ghost-\"."
  (let ((ghost financial-chart-3d-ghost-color))
    (delq nil
          (mapcar (lambda (p)
                    (unless (string-suffix-p "axes" (or (plist-get p :id) ""))
                      (let ((p (copy-sequence p)))
                        (setq p (plist-put p :id (concat "ghost-" (plist-get p :id))))
                        (dolist (k '(:color :low_color :high_color))
                          (when (plist-member p k) (setq p (plist-put p k ghost))))
                        (when (plist-get p :colors)
                          (setq p (plist-put p :colors (make-vector (length (plist-get p :colors)) ghost))))
                        (if (or (plist-member p :color) (plist-member p :low_color)) p
                          (plist-put p :color ghost)))))
                  (append primitives nil)))))

(defun financial-chart-3d--replay (graphics rows field mark)
  "Primitives of MARK (a function of GRAPHICS and rows) over ROWS, replayed.
For a mark written for a cumulative time encoding over a field:
GRAPHICS's option `replay', default FIELD (neither: MARK over ROWS as
they are).  Of the field's distinct values (numbers sorted, else in
the rows' order), MARK draws the rows of the first ones in colour,
from `financial-chart-3d-replay-window' of them (at least one) at
GRAPHICS's phase 0 to all at 1 (`financial-chart-3d--phase'), all ROWS
its scales' domain (`graphics-eas-domain-rows'); until the end it
draws all ROWS again behind them as a ghost (`financial-chart-3d--ghost'),
so every frame spans the whole chart.
The core keeps the first of equally near dots: the colour."
  (let* ((rows (append rows nil))
         (field (financial-chart-3d--opt graphics "replay" field))
         (field (and field (financial-chart-3d--key field)))
         (keys (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r field)) rows)))
         (n (length keys))
         ;; From a third (at least one): the ghost spans the rest.
         (from (min n (max 1 (ceiling (* n financial-chart-3d-replay-window)))))
         (shown (+ from (round (* (financial-chart-3d--phase graphics) (- n from)))))
         (seen (let ((h (make-hash-table :test #'equal)))
                 (dolist (k (seq-take keys shown) h) (puthash k t h))))
         (graphics-eas-domain-rows rows))
    (if (or (not field) (>= shown n)) (funcall mark graphics rows)
      (vconcat (funcall mark graphics (seq-filter (lambda (r) (gethash (financial-chart-3d--value r field) seen)) rows))
               (financial-chart-3d--ghost (funcall mark graphics rows))))))

(defun financial-chart-3d--shift (primitives dy prefix)
  "PRIMITIVES (a sequence) raised by DY, their ids prefixed with PREFIX."
  (mapcar
   (lambda (p)
     (let ((p (copy-sequence p)))
       (setq p (plist-put p :id (concat prefix (plist-get p :id))))
       (pcase (plist-get p :type)
         ((or "segments" "boxes")
          (let ((key (if (equal (plist-get p :type) "boxes") :boxes :segments)))
            (plist-put p key (vconcat (mapcar (lambda (s)
                                                (let ((s (copy-sequence s)))
                                                  (aset s 1 (financial-chart-3d--r3 (+ (aref s 1) dy)))
                                                  (aset s 4 (financial-chart-3d--r3 (+ (aref s 4) dy)))
                                                  s))
                                              (plist-get p key))))))
         ("mesh"
          (plist-put p :vertices (vconcat (mapcar (lambda (v) (vector (aref v 0) (financial-chart-3d--r3 (+ (aref v 1) dy))
                                                                      (aref v 2)))
                                                  (plist-get p :vertices)))))
         ("heightfield" (plist-put p :base (financial-chart-3d--r3 (+ (or (plist-get p :base) 0) dy))))
         (_ p))))
   primitives))

(defun financial-chart-3d--axes (id x0 x1 y1 z0 z1 &optional color)
  "Axis segments ID along the back left corner of X0..X1, 0..Y1, Z0..Z1.
COLOR defaults to graphics.el's axis grey."
  (list :id id :type "segments"
        :segments (vector (vector x0 0 z0 x1 0 z0) (vector x0 0 z0 x0 0 z1)
                          (vector x0 0 z0 x0 (financial-chart-3d--r3 y1) z0))
        :color (or color "#d2ddeb")))

;;; Candles on whole-series scales

(defconst financial-chart-3d-up-color "#26a69a" "Rising candle colour.")
(defconst financial-chart-3d-down-color "#ef5350" "Falling candle colour.")

(defun financial-chart-3d--candle-layout (graphics rows &optional tall)
  "Layout of ROWS' candles under GRAPHICS, over the whole series.
The price axis is `height' high, by default 0.5 per bar (TALL per bar
when given), at least 4.
A plist: :keys (the bars' x values in order, from the domain rows and
ROWS), :index (x value -> position), :x0, :lo, :hi, :height, :half,
:dmax, :vmax.  With a time encoding the domain rows are every bar, so
each replay frame keeps the full series' positions and price scale."
  (let* ((xk (financial-chart-3d--field graphics "x" "time"))
         (dk (financial-chart-3d--field graphics "depth" "volume"))
         (dom (financial-chart-3d--domain rows))
         (keys (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (index (make-hash-table :test #'equal))
         (n (max 1 (length keys)))
         (lows (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r :low))) (and (numberp v) v))) dom)))
         (highs (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r :high))) (and (numberp v) v))) dom)))
         (lo (if lows (apply #'min lows) 0.0))
         (hi (if highs (apply #'max highs) 1.0))
         (half (/ (financial-chart-3d--opt graphics "band" 0.6) 2.0)))
    (cl-loop for k in keys for i from 0 do (puthash k i index))
    (list :xk xk :dk dk :keys keys :index index :n n :x0 (/ (1- n) 2.0)
          :lo (float lo) :hi (float (if (> hi lo) hi (1+ lo)))
          :height (float (financial-chart-3d--opt graphics "height" (max 4.0 (* (or tall 0.5) n))))
          :half half
          :dmax (float (financial-chart-3d--opt graphics "depth" (max (* 2 half) (* 0.06 n))))
          :vmax (float (apply #'max 0 (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r dk)))
                                                                       (and (numberp v) v)))
                                                        dom)))))))

(defun financial-chart-3d--price-y (layout price)
  "World height of PRICE in candle LAYOUT."
  (financial-chart-3d--r3 (* (- price (plist-get layout :lo))
                             (/ (plist-get layout :height) (- (plist-get layout :hi) (plist-get layout :lo))))))

(defun financial-chart-3d--bar-x (layout row)
  "World x of bar ROW in candle LAYOUT, or nil."
  (when-let* ((i (gethash (financial-chart-3d--value row (plist-get layout :xk)) (plist-get layout :index))))
    (- i (plist-get layout :x0))))

(defun financial-chart-3d--candles (graphics layout rows &optional shown)
  "Primitives of ROWS' candles in LAYOUT under GRAPHICS: axes, wicks, bodies.
Bodies are as deep as the bar's volume (`depth' channel) when it has
one, as graphics.el's candle3d draws them.  With SHOWN, the bars after
the first SHOWN are ghosts (`financial-chart-3d-ghost-color')."
  (let* ((half (plist-get layout :half)) (height (plist-get layout :height))
         (dmax (plist-get layout :dmax)) (vmax (plist-get layout :vmax)) (dk (plist-get layout :dk))
         (x0 (plist-get layout :x0))
         (up (financial-chart-3d--opt graphics "up_color" financial-chart-3d-up-color))
         (down (financial-chart-3d--opt graphics "down_color" financial-chart-3d-down-color))
         bodies wicks colors data)
    (cl-loop for r in rows for i from 0 do
             (let ((x (financial-chart-3d--bar-x layout r))
                   (o (financial-chart-3d--value r :open)) (h (financial-chart-3d--value r :high))
                   (l (financial-chart-3d--value r :low)) (c (financial-chart-3d--value r :close)))
               (unless (and (numberp o) (numberp h) (numberp l) (numberp c))
                 (error "Bar %d needs numeric open, high, low and close (financial-chart-3d)" i))
               (when x
                 (let* ((b0 (financial-chart-3d--price-y layout (min o c)))
                        (b1 (max (financial-chart-3d--price-y layout (max o c)) (+ b0 (* 0.004 height))))
                        (v (financial-chart-3d--value r dk))
                        (d (if (and (numberp v) (> vmax 0)) (+ half (* (- dmax half) (/ v vmax))) half)))
                   (push (vector (financial-chart-3d--r3 (- x half)) b0 (financial-chart-3d--r3 (- d))
                                 (financial-chart-3d--r3 (+ x half)) (financial-chart-3d--r3 b1) (financial-chart-3d--r3 d))
                         bodies)
                   (push (vector (financial-chart-3d--r3 x) (financial-chart-3d--price-y layout l) 0
                                 (financial-chart-3d--r3 x) (financial-chart-3d--price-y layout h) 0)
                         wicks)
                   (push (cond ((and shown (>= i shown)) financial-chart-3d-ghost-color)
                               ((>= c o) up)
                               (t down))
                         colors)
                   (push r data)))))
    (let ((colors (vconcat (nreverse colors)))
          (data (vconcat (nreverse data))))
      (list (financial-chart-3d--axes "axes" (- -0.5 x0) (+ 0.5 x0) height 0 dmax)
            (list :id "wicks" :type "segments" :segments (vconcat (nreverse wicks)) :colors colors :data data)
            (list :id "bodies" :type "boxes" :boxes (vconcat (nreverse bodies)) :colors colors :data data)))))

(defun financial-chart-3d--candle-mark (graphics rows)
  "Primitives of the fc-candle3d mark: GRAPHICS over bar ROWS.
Laid out over every bar; a replay colours the bars so far."
  ;; Taller than wide in price, as the bars rise along a diagonal.
  (vconcat (financial-chart-3d--candles graphics (financial-chart-3d--candle-layout graphics rows 0.8) rows
                                        (financial-chart-3d--replayed graphics (length rows)))))

;;; Panes: price, volume and RSI on one time axis

(defun financial-chart-3d--panes-mark (graphics rows)
  "Primitives of the fc-panes3d mark: GRAPHICS over bar ROWS in three panels.
The panels stack upward on one time axis (RSI at the bottom, then
volume, then price), each bar at the same x in all three, as the panes
template stacks them down the page.  A replay colours the bars so far
and their RSI."
  (let* ((layout (financial-chart-3d--candle-layout graphics rows))
         (height (plist-get layout :height))
         (half (plist-get layout :half)) (x0 (plist-get layout :x0))
         (panel (* 0.3 height)) (gap (* 0.12 height))
         (vmax (plist-get layout :vmax))
         (period (financial-chart-3d--opt graphics "rsi" 14))
         (rsi (financial-chart-rsi rows period))
         (shown (financial-chart-3d--replayed graphics (length rows)))
         (left (- -0.5 x0)) (right (+ 0.5 x0))
         volume volume-colors line ghost bands)
    (cl-loop for r in rows for i from 0 do
      (when-let* ((x (financial-chart-3d--bar-x layout r)))
        (let ((v (financial-chart-3d--value r (plist-get layout :dk))))
          (when (and (numberp v) (> vmax 0))
            (push (vector (financial-chart-3d--r3 (- x half)) 0 (financial-chart-3d--r3 (- half))
                          (financial-chart-3d--r3 (+ x half)) (financial-chart-3d--r3 (* panel (/ v vmax)))
                          (financial-chart-3d--r3 half))
                  volume)
            (push (cond ((>= i shown) financial-chart-3d-ghost-color)
                        ((>= (financial-chart-3d--value r :close) (financial-chart-3d--value r :open))
                         financial-chart-3d-up-color)
                        (t financial-chart-3d-down-color))
                  volume-colors)))))
    (cl-loop for (a b) on (cl-mapcar #'cons rows rsi) while b for i from 1
             when (and (cdr a) (cdr b))
             do (push (vector (financial-chart-3d--r3 (financial-chart-3d--bar-x layout (car a)))
                              (financial-chart-3d--r3 (* panel (/ (cdr a) 100.0))) 0
                              (financial-chart-3d--r3 (financial-chart-3d--bar-x layout (car b)))
                              (financial-chart-3d--r3 (* panel (/ (cdr b) 100.0))) 0)
                      (if (< i shown) line ghost)))
    (dolist (level '(30 70))
      (let ((y (financial-chart-3d--r3 (* panel (/ level 100.0)))))
        (push (vector left y 0 right y 0) bands)))
    (vconcat
     (list (financial-chart-3d--axes "rsi-axes" left right panel 0 half)
           (list :id "rsi-bands" :type "segments" :segments (vconcat bands) :color "#5b6b86")
           (list :id "rsi" :type "segments" :segments (vconcat (nreverse line)) :color "#b279a2")
           (list :id "rsi-ghost" :type "segments" :segments (vconcat (nreverse ghost))
                 :color financial-chart-3d-ghost-color))
     (financial-chart-3d--shift
      (list (financial-chart-3d--axes "axes" left right panel 0 half)
            (list :id "bars" :type "boxes" :boxes (vconcat (nreverse volume))
                  :colors (vconcat (nreverse volume-colors))))
      (+ panel gap) "volume-")
     (financial-chart-3d--shift (financial-chart-3d--candles graphics layout rows shown)
                                (* 2 (+ panel gap)) "price-"))))

;;; Volume profile beside the candles

(defun financial-chart-3d-profile-volumes (bars low high bins)
  "Volume of BARS per price level: BINS equal levels from LOW to HIGH.
Each bar's volume is spread evenly over the part of its low-high range
in each level, as `financial-chart-volume-profile' estimates it; over
the bars' own range the two agree.  A vector of BINS numbers."
  (let* ((step (/ (- high low) (float bins)))
         (volumes (make-vector bins 0.0)))
    (dolist (bar bars)
      (let* ((l (min (plist-get bar :low) (plist-get bar :high)))
             (h (max (plist-get bar :low) (plist-get bar :high)))
             (v (or (financial-chart-3d--value bar :volume) 0)))
        (if (= l h)
            (let ((i (min (1- bins) (max 0 (floor (/ (- l low) step))))))
              (aset volumes i (+ (aref volumes i) v)))
          (dotimes (i bins)
            (let* ((b0 (+ low (* i step)))
                   (overlap (max 0 (- (min h (+ b0 step)) (max l b0)))))
              (when (> overlap 0)
                (aset volumes i (+ (aref volumes i) (* v (/ overlap (- h l)))))))))))
    volumes))

(defun financial-chart-3d--volume-profile-mark (graphics rows)
  "Primitives of the fc-volume-profile3d mark: GRAPHICS over bar ROWS.
ROWS are bars; the profile's `bins' levels span the whole series' price
range, and each level is a horizontal bar right of the last candle, as
long as its volume, the point of control bright and the last close
ruled across.  A replay colours the bars so far and builds their
profile."
  (let* ((layout (financial-chart-3d--candle-layout graphics rows 0.8))
         (all rows)
         (shown (financial-chart-3d--replayed graphics (length rows)))
         (rows (seq-take rows shown))
         (bins (max 1 (financial-chart-3d--opt graphics "bins" 24)))
         (lo (plist-get layout :lo)) (hi (plist-get layout :hi))
         (volumes (financial-chart-3d-profile-volumes rows lo hi bins))
         (vmax (seq-reduce #'max volumes 0.0))
         (poc (and (> vmax 0) (seq-position volumes vmax)))
         (n (plist-get layout :n))
         (start (+ (- (1- n) (plist-get layout :x0)) 1.0))
         (width (max 3.0 (* 0.35 n)))
         (half (plist-get layout :half))
         (step (/ (- hi lo) bins))
         (last-close (financial-chart-3d--value (car (last rows)) :close))
         boxes colors)
    (dotimes (i bins)
      (let ((v (aref volumes i)))
        (when (> v 0)
          (push (vector (financial-chart-3d--r3 start)
                        (financial-chart-3d--price-y layout (+ lo (* i step)))
                        (financial-chart-3d--r3 (- half))
                        (financial-chart-3d--r3 (+ start (* width (/ v vmax))))
                        (financial-chart-3d--r3 (- (financial-chart-3d--price-y layout (+ lo (* (1+ i) step)))
                                                   (* 0.15 (financial-chart-3d--price-y layout (+ lo step)))))
                        (financial-chart-3d--r3 half))
                boxes)
          (push (if (eql i poc) "#ffb74d" "#4c78a8") colors))))
    (vconcat
     (financial-chart-3d--candles graphics layout all shown)
     (list (list :id "profile" :type "boxes" :boxes (vconcat (nreverse boxes)) :colors (vconcat (nreverse colors)))
           (list :id "profile-close" :type "segments" :color "#d2ddeb"
                 :segments (if (numberp last-close)
                               (let ((y (financial-chart-3d--price-y layout last-close)))
                                 (vector (vector (financial-chart-3d--r3 start) y 0
                                                 (financial-chart-3d--r3 (+ start width)) y 0)))
                             []))))))

;;; Drawdown: the underwater curve

(defun financial-chart-3d-drawdown-rows (rows xk yk)
  "ROWS with :depth, the percent below the running high of field YK.
Rows without a number at XK or YK are dropped.  Depth is 0 at a new
high and positive below it."
  (let ((peak nil))
    (delq nil (mapcar (lambda (r)
                        (let ((x (financial-chart-3d--value r xk)) (y (financial-chart-3d--value r yk)))
                          (when (and x (numberp y))
                            (setq peak (if peak (max peak y) y))
                            (list :x x :depth (if (> peak 0) (* 100.0 (/ (- peak y) (float peak))) 0.0)))))
                      rows))))

(defun financial-chart-3d--drawdown-mark (graphics rows)
  "Primitives of the fc-drawdown3d mark: GRAPHICS over value ROWS.
The percent drawdown from the running high as an area3d slab, turned
upside down: its flat top is the high-water line and the curve hangs
below it, deepest at the maximum drawdown.  A replay colours the
points so far; the rest of the curve is a ghost."
  (let* ((xk (financial-chart-3d--field graphics "x" "x"))
         (yk (financial-chart-3d--field graphics "y" "y"))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (sub (lambda (color)
                (list :mark "area3d" :encoding (list :x "x" :y "depth")
                      :height height :zero t
                      :extent (financial-chart-3d--opt graphics "extent" [12 0]) :color color)))
         (all (financial-chart-3d-drawdown-rows rows xk yk))
         (shown (financial-chart-3d--replayed graphics (length all)))
         (prims (append (graphics-eas-primitives (funcall sub (financial-chart-3d--opt graphics "color" "#4c78a8"))
                                                 (seq-take all shown) (financial-chart-3d--mark-params) all)
                        (and (< shown (length all))
                             (financial-chart-3d--shift
                              (seq-remove (lambda (p) (equal (plist-get p :id) "axes"))
                                          (graphics-eas-primitives (funcall sub financial-chart-3d-ghost-color)
                                                                   (nthcdr (max 0 (1- shown)) all)
                                                                   (financial-chart-3d--mark-params) all))
                              0 "ghost-"))))
         (flip (lambda (y) (financial-chart-3d--r3 (- height y)))))
    (vconcat
     (mapcar (lambda (p)
               (pcase (plist-get p :type)
                 ("mesh" (plist-put (copy-sequence p) :vertices
                                    (vconcat (mapcar (lambda (v) (vector (aref v 0) (funcall flip (aref v 1)) (aref v 2)))
                                                     (plist-get p :vertices)))))
                 ("segments" (plist-put (copy-sequence p) :segments
                                        (vconcat (mapcar (lambda (s) (vector (aref s 0) (funcall flip (aref s 1)) (aref s 2)
                                                                             (aref s 3) (funcall flip (aref s 4)) (aref s 5)))
                                                         (plist-get p :segments)))))
                 (_ p)))
             prims))))

;;; Payoff surface over price and days to expiry

(defun financial-chart-3d--payoff-pairs (payoff)
  "PAYOFF as a price-sorted list of (PRICE . PNL).
PAYOFF is rows {price, pnl} (a vector or list, or data/v1) or (PRICE
PNL) pairs."
  (sort (delq nil (mapcar (lambda (p)
                            (let ((price (if (keywordp (car-safe p)) (plist-get p :price) (car-safe p)))
                                  (pnl (if (keywordp (car-safe p)) (plist-get p :pnl) (car-safe (cdr-safe p)))))
                              (and (numberp price) (numberp pnl) (cons (float price) (float pnl)))))
                          (financial-chart-3d--rows payoff)))
        (lambda (a b) (< (car a) (car b)))))

(defun financial-chart-3d--interpolate (pairs s)
  "The P/L of sorted (PRICE . PNL) PAIRS at price S.
Linear between them and extended along the end segments beyond them."
  (let* ((n (length pairs)) (v (vconcat pairs)))
    (if (= n 1) (cdr (aref v 0))
      (let* ((i (cond ((<= s (car (aref v 0))) 0)
                      ((>= s (car (aref v (1- n)))) (- n 2))
                      (t (cl-loop for i from 0 below (1- n)
                                  when (<= (car (aref v i)) s (car (aref v (1+ i)))) return i))))
             (a (aref v i)) (b (aref v (1+ i)))
             (dx (- (car b) (car a))))
        (if (zerop dx) (cdr a)
          (+ (cdr a) (* (- s (car a)) (/ (- (cdr b) (cdr a)) dx))))))))

(defun financial-chart-3d--round4 (v)
  "V rounded to 4 decimals."
  (/ (fround (* v 10000.0)) 10000.0))

(cl-defun financial-chart-3d-payoff-surface (payoff &key vol days)
  "P/L rows {price, dte, day, pnl} of expiry PAYOFF at earlier DAYS.
PAYOFF is the expiry P/L by price (rows {price, pnl} or (PRICE PNL)
pairs, as the payoff template takes).  Each of DAYS (days to expiry,
default 60 45 30 21 14 7 3 0) prices it back: the expected expiry P/L
when the underlying, now at the row's price, moves lognormally at
annualised volatility VOL (default 0.3) with a zero rate -- the exact
value of a position of same-expiry options and stock, whose expiry P/L
is a function of price alone.  Day 0 is PAYOFF itself.  The expiry P/L
is linear between PAYOFF's prices and along its end segments beyond
them.  Rows run furthest DAYS first, prices ascending; day counts days
elapsed since the furthest."
  (let* ((pairs (financial-chart-3d--payoff-pairs payoff))
         (vol (float (or vol 0.3)))
         (days (append (or days [60 45 30 21 14 7 3 0]) nil))
         (furthest (apply #'max days))
         ;; z from -5 to 5: the normal weights, normalised.
         (zs (cl-loop for i from -40 to 40 collect (/ i 8.0)))
         (ws (let ((raw (mapcar (lambda (z) (exp (* -0.5 z z))) zs)))
               (mapcar (lambda (w) (/ w (apply #'+ raw))) raw))))
    (unless pairs
      (signal 'financial-chart-invalid-data
              (list "payoff surface needs rows {price, pnl}" :code "empty_payoff" :index nil :field "data")))
    (cl-loop for dte in days
             append (let* ((tau (/ dte 365.0)) (sd (* vol (sqrt tau))) (mu (* -0.5 sd sd)))
                      (mapcar (lambda (p)
                                (list :price (car p) :dte dte :day (- furthest dte)
                                      :pnl (financial-chart-3d--round4
                                            (if (<= dte 0) (cdr p)
                                              (cl-loop for z in zs for w in ws
                                                       sum (* w (financial-chart-3d--interpolate
                                                                 pairs (* (car p) (exp (+ mu (* sd z)))))))))))
                              pairs)))))

(defun financial-chart-3d--payoff-mark (graphics rows)
  "Primitives of the fc-payoff3d mark: GRAPHICS over ROWS as a P/L heightfield.
Channels x (price), z (days to expiry, or a curve label) and y (P/L).
Columns are the distinct x values, rows the z values (numbers
furthest first, labels as they come), height the P/L, coloured red to
green over the whole P/L range with the zero (breakeven) level framed.
Rows that carry no z value (the payoff template's expiry rows) are
priced back to the `days' at volatility `vol' first
\(`financial-chart-3d-payoff-surface').  The whole surface is always
drawn; a bright trace across it marks where the timeline is, sweeping
from the furthest row to the last (expiry), between rows on the
surface itself."
  (let* ((xk (financial-chart-3d--field graphics "x" "price"))
         (zk (financial-chart-3d--field graphics "z" "dte"))
         (yk (financial-chart-3d--field graphics "y" "pnl"))
         (has-z (seq-some (lambda (r) (financial-chart-3d--value r zk)) rows))
         (rows (if (or has-z (null rows)) rows
                 (progn (setq xk :price zk :dte yk :pnl)
                        (financial-chart-3d-payoff-surface
                         (mapcar (lambda (r) (list (financial-chart-3d--value r xk)
                                                   (financial-chart-3d--value r yk)))
                                 rows)
                         :vol (financial-chart-3d--opt graphics "vol")
                         :days (financial-chart-3d--opt graphics "days")))))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) rows)))
         (zs (let ((zs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zk)) rows))))
               (if (and zs (seq-every-p #'numberp zs)) (reverse zs) zs)))
         (ys (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r yk))) (and (numberp v) (float v))))
                               rows)))
         (lo (if ys (apply #'min ys) 0.0))
         (hi (if ys (apply #'max ys) 1.0))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (if (> hi lo) (/ height (- hi lo)) 1.0))
         (base (financial-chart-3d--r3 (- (* lo scale))))
         (extent (financial-chart-3d--opt graphics "extent" [12 9]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (cells (make-hash-table :test #'equal)))
    (dolist (r rows)
      (let ((x (financial-chart-3d--value r xk)) (z (financial-chart-3d--value r zk))
            (y (financial-chart-3d--value r yk)))
        (when (and x z (numberp y))
          (puthash (cons x z) (float y) cells))))
    (let* ((nx (length xs)) (nz (length zs))
           (x-at (lambda (i) (financial-chart-3d--r3 (if (> nx 1) (- (* 2 w (/ i (float (1- nx)))) w) 0))))
           (heights (vconcat (mapcar (lambda (z) (vconcat (mapcar (lambda (x) (gethash (cons x z) cells lo)) xs)))
                                     zs)))
           ;; The trace: between rows J and J + 1, U of the way.
           (at (* (financial-chart-3d--phase graphics) (max 0 (1- nz))))
           (j (min (max 0 (- nz 2)) (floor at)))
           (u (- at j))
           (z (financial-chart-3d--r3 (if (> nz 1) (- (* 2 d (/ at (1- nz))) d) 0)))
           (trace (lambda (i) (let ((a (aref (aref heights j) i))
                                    (b (aref (aref heights (min (1- nz) (1+ j))) i)))
                                ;; Lifted a little: above the surface, not in it.
                                (financial-chart-3d--r3 (+ base (* 0.03 height) (* scale (+ a (* u (- b a)))))))))
           (zero (and (< lo 0) (> hi 0) base)))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (if (and (>= nx 2) (>= nz 1))
           (list :id "surface" :type "heightfield" :x (vector (- w) w) :z (vector (- d) d)
                 :base base :y_scale (financial-chart-3d--r3 scale)
                 :heights (vconcat (mapcar (lambda (row) (vconcat (mapcar #'financial-chart-3d--r3 row)))
                                           (if (= nz 1) (list (aref heights 0) (aref heights 0)) heights)))
                 :low_color (financial-chart-3d--opt graphics "low_color" financial-chart-3d-down-color)
                 :high_color (financial-chart-3d--opt graphics "high_color" financial-chart-3d-up-color)
                 :color_range (vector lo hi))
         (list :id "surface" :type "segments" :segments []))
       (list :id "front" :type "segments" :color "#ffffff"
             :segments (if (and (>= nx 2) (>= nz 1))
                           (vconcat (cl-loop for i from 0 below (1- nx)
                                             collect (vector (funcall x-at i) (funcall trace i) z
                                                             (funcall x-at (1+ i)) (funcall trace (1+ i)) z)))
                         []))
       (list :id "zero" :type "segments" :color "#8a94a6"
             :segments (if zero
                           (vector (vector (- w) zero (- d) w zero (- d)) (vector w zero (- d) w zero d)
                                   (vector w zero d (- w) zero d) (vector (- w) zero d (- w) zero (- d)))
                         []))))))

;;; Depth walls and diverging bars

(defun financial-chart-3d-cumulative-depth (rows &optional xk yk sk)
  "Order-book ROWS {side, price, size} as cumulative depth rows.
Each side's size is summed outward from its best price (bids down from
the highest, asks up from the lowest), as the depth template draws
them; rows {price, cumulative, side}, bids then asks, by price.  XK,
YK and SK name the price, size and side fields."
  (let ((xk (or xk :price)) (yk (or yk :size)) (sk (or sk :side)))
    (cl-loop for (side . descending) in '(("bid" . t) ("ask" . nil))
             append (let* ((levels (seq-filter (lambda (r) (and (equal (financial-chart-3d--value r sk) side)
                                                                 (numberp (financial-chart-3d--value r xk))
                                                                 (numberp (financial-chart-3d--value r yk))))
                                               rows))
                           (levels (sort (copy-sequence levels)
                                         (lambda (a b) (funcall (if descending #'> #'<)
                                                                (financial-chart-3d--value a xk)
                                                                (financial-chart-3d--value b xk)))))
                           (total 0.0)
                           (out (mapcar (lambda (r)
                                          (setq total (+ total (financial-chart-3d--value r yk)))
                                          (list :price (float (financial-chart-3d--value r xk))
                                                :cumulative total :side side))
                                        levels)))
                      (if descending (nreverse out) out)))))

(defun financial-chart-3d--depth-mark (graphics rows)
  "Primitives of the fc-depth3d mark: GRAPHICS over order-book ROWS.
An area3d slab per side over price, bids in front and asks behind,
each rising away from the mid."
  (graphics-eas-primitives
   (list :mark "area3d" :encoding (list :x "price" :y "cumulative" :z "side")
         :zero t :height (financial-chart-3d--opt graphics "height" 4)
         :extent (financial-chart-3d--opt graphics "extent" [12 0])
         :colors (financial-chart-3d--opt graphics "colors" '(:bid "#26a69a" :ask "#ef5350")))
   (financial-chart-3d-cumulative-depth rows (financial-chart-3d--field graphics "x" "price")
                                        (financial-chart-3d--field graphics "y" "size")
                                        (financial-chart-3d--field graphics "color" "side"))
   (financial-chart-3d--mark-params)))

(defun financial-chart-3d--diverging-mark (graphics rows)
  "Primitives of the fc-diverging3d mark: GRAPHICS over ROWS {label, value}.
graphics.el's bar3d, gains green and losses red."
  (let ((yk (financial-chart-3d--field graphics "y" "value")))
    (graphics-eas-primitives
     (list :mark "bar3d"
           :encoding (list :x (substring (symbol-name (financial-chart-3d--field graphics "x" "label")) 1)
                           :y (substring (symbol-name yk) 1) :color "sign")
           :colors (list :gain (financial-chart-3d--opt graphics "up_color" financial-chart-3d-up-color)
                         :loss (financial-chart-3d--opt graphics "down_color" financial-chart-3d-down-color)))
     (mapcar (lambda (r)
               (let ((v (financial-chart-3d--value r yk)))
                 (append (list :sign (if (and (numberp v) (< v 0)) "loss" "gain")) r)))
             rows)
     (financial-chart-3d--mark-params))))

(defconst financial-chart-3d-marks
  '(("fc-candle3d" . financial-chart-3d--candle-mark)
    ("fc-panes3d" . financial-chart-3d--panes-mark)
    ("fc-volume-profile3d" . financial-chart-3d--volume-profile-mark)
    ("fc-drawdown3d" . financial-chart-3d--drawdown-mark)
    ("fc-payoff3d" . financial-chart-3d--payoff-mark)
    ("fc-depth3d" . financial-chart-3d--depth-mark)
    ("fc-diverging3d" . financial-chart-3d--diverging-mark))
  "The 3D marks this package adds to graphics.el: (MARK . FUNCTION).")

(defun financial-chart-3d--register-marks ()
  "Register `financial-chart-3d-marks' with graphics.el."
  (dolist (m financial-chart-3d-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; graphics.el loaded first: register now; else when a 3D command loads
;; it (`financial-chart-3d-available-p').
(when (featurep 'graphics-eas-marks)
  (financial-chart-3d--register-marks))
(require 'financial-chart-3d-vol-surface)
(with-eval-after-load 'financial-chart-3d (require 'financial-chart-3d-valuation))
(require 'financial-chart-3d-risk)
(require 'financial-chart-3d-options)
(require 'financial-chart-3d-market)

;;; Opening variants

(defun financial-chart-3d-bindings (name &optional bindings)
  "BINDINGS for variant NAME (default its example), completed for 3D.
The payoff variant's `surface' is computed from its `data', `vol' and
`days' when not given (`financial-chart-3d-payoff-surface'), so it
replays toward expiry."
  (let* ((name (financial-chart-3d--name name))
         (bindings (copy-sequence (or bindings (eas-template-example name)))))
    (when (and (equal (financial-chart-3d--base name) "payoff")
               (not (plist-get bindings :surface)))
      (let ((slots (plist-get (plist-get (eas-template-get name) :meta) :slots)))
        (setq bindings
              (plist-put bindings :surface
                         (vconcat (financial-chart-3d-payoff-surface
                                   (plist-get bindings :data)
                                   :vol (or (plist-get bindings :vol) (plist-get (plist-get slots :vol) :default))
                                   :days (or (plist-get bindings :days)
                                             (plist-get (plist-get slots :days) :default))))))))
    bindings))

(defun financial-chart-3d-spec (name &optional bindings)
  "The chart/v1 spec of 3D variant NAME bound to BINDINGS (default its example).
The template resolved by eas plus its x-eas.graphics with slots filled
\(graphics.el's `graphics-eas-variant-spec')."
  (financial-chart-3d--ensure)
  (let ((name (financial-chart-3d--name name)))
    (graphics-eas-variant-spec name (financial-chart-3d-bindings name bindings))))

;;; Framing: the camera fitted to the data

(defconst financial-chart-3d-fill 0.92
  "Fraction of a pane's shorter side the data spans at the fitted camera.")

(defconst financial-chart-3d-pane-aspect 1.2
  "Least width / height of the panes the fitted camera frames.
The core scales a frame to its shorter side, so a wider pane shows
more on either side; the fit lets the data use this much width.")

(defun financial-chart-3d--cross (a b)
  "The cross product of 3-vectors A and B."
  (vector (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
          (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
          (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0)))))

(defun financial-chart-3d--dot (a b)
  "The dot product of 3-vectors A and B."
  (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 1)) (* (aref a 2) (aref b 2))))

(defun financial-chart-3d-camera-fit (points camera)
  "The orbit camera `target' and `distance' framing POINTS tightly, a plist.
POINTS are [X Y Z] vectors (`financial-chart-3d--points'); CAMERA the
plist with its :yaw, :pitch and :fov (default 40).  Seen from there,
the points span `financial-chart-3d-fill' of a pane's shorter side and
at most `financial-chart-3d-pane-aspect' times that across, centred.
The core's own fit frames the bounding sphere of their box instead,
which leaves a long, flat or diagonal chart small in its pane."
  (let* ((yaw (degrees-to-radians (float (or (plist-get camera :yaw) 30))))
         (pitch (degrees-to-radians (float (or (plist-get camera :pitch) 25))))
         (tan (tan (/ (degrees-to-radians (float (or (plist-get camera :fov) 40))) 2)))
         (forward (vector (- (* (cos pitch) (sin yaw))) (- (sin pitch)) (- (* (cos pitch) (cos yaw)))))
         (right (let* ((r (financial-chart-3d--cross forward [0.0 1.0 0.0]))
                       (n (sqrt (financial-chart-3d--dot r r))))
                  (if (< n 1e-9) (vector (cos yaw) 0.0 (- (sin yaw)))
                    (vector (/ (aref r 0) n) (/ (aref r 1) n) (/ (aref r 2) n)))))
         (up (financial-chart-3d--cross right forward))
         (center (let ((n (float (length points))))
                   (vector (/ (cl-loop for p in points sum (aref p 0)) n) (/ (cl-loop for p in points sum (aref p 1)) n)
                           (/ (cl-loop for p in points sum (aref p 2)) n))))
         (corners (mapcar (lambda (p) (vector (- (aref p 0) (aref center 0)) (- (aref p 1) (aref center 1))
                                              (- (aref p 2) (aref center 2))))
                          points))
         (us (mapcar (lambda (c) (financial-chart-3d--dot c right)) corners))
         (vs (mapcar (lambda (c) (financial-chart-3d--dot c up)) corners))
         (ws (mapcar (lambda (c) (financial-chart-3d--dot c forward)) corners))
         (um (/ (+ (apply #'min us) (apply #'max us)) 2))
         (vm (/ (+ (apply #'min vs) (apply #'max vs)) 2))
         (across (* tan financial-chart-3d-fill financial-chart-3d-pane-aspect))
         (down (* tan financial-chart-3d-fill))
         ;; Each corner, W nearer or further, is within the frame at D.
         (distance (cl-loop for u in us for v in vs for w in ws
                            maximize (- (max (/ (abs (- u um)) across) (/ (abs (- v vm)) down)) w))))
    (list :target (vector (financial-chart-3d--r3 (+ (aref center 0) (* um (aref right 0)) (* vm (aref up 0))))
                          (financial-chart-3d--r3 (+ (aref center 1) (* um (aref right 1)) (* vm (aref up 1))))
                          (financial-chart-3d--r3 (+ (aref center 2) (* um (aref right 2)) (* vm (aref up 2)))))
          :distance (financial-chart-3d--r3 (max distance (- 0.1 (apply #'min ws)))))))

(defun financial-chart-3d--points (primitives)
  "The points of PRIMITIVES but their axes (ids ending in \"axes\"), [X Y Z].
Boxes give their corners, segments their ends, meshes their vertices,
heightfields their grid; any other primitive the corners of its
bounds (`graphics-anim-bounds')."
  (let (points)
    (cl-flet ((pt (x y z) (push (vector (float x) (float y) (float z)) points)))
      (dolist (p primitives)
        (unless (string-suffix-p "axes" (or (plist-get p :id) ""))
          (pcase (plist-get p :type)
            ("boxes" (seq-doseq (b (plist-get p :boxes))
                       (dolist (x (list (elt b 0) (elt b 3)))
                         (dolist (y (list (elt b 1) (elt b 4)))
                           (dolist (z (list (elt b 2) (elt b 5)))
                             (pt x y z))))))
            ("segments" (seq-doseq (s (plist-get p :segments))
                          (if (= (length s) 4)
                              (progn (pt (elt s 0) (elt s 1) 0) (pt (elt s 2) (elt s 3) 0))
                            (pt (elt s 0) (elt s 1) (elt s 2)) (pt (elt s 3) (elt s 4) (elt s 5)))))
            ("mesh" (seq-doseq (v (plist-get p :vertices)) (pt (elt v 0) (elt v 1) (elt v 2))))
            ("heightfield"
             (let* ((x (plist-get p :x)) (z (plist-get p :z)) (heights (plist-get p :heights))
                    (nz (length heights)) (base (or (plist-get p :base) 0)) (scale (or (plist-get p :y_scale) 1)))
               (seq-do-indexed
                (lambda (row j)
                  (let ((nx (length row)))
                    (seq-do-indexed
                     (lambda (h i)
                       (pt (+ (elt x 0) (* (- (elt x 1) (elt x 0)) (if (> nx 1) (/ i (float (1- nx))) 0.5)))
                           (+ base (* h scale))
                           (+ (elt z 0) (* (- (elt z 1) (elt z 0)) (if (> nz 1) (/ j (float (1- nz))) 0.5)))))
                     row)))
                heights)))
            (_ (when-let* ((b (graphics-anim-bounds (list p))))
                 (dolist (x (list (aref b 0) (aref b 3)))
                   (dolist (y (list (aref b 1) (aref b 4)))
                     (dolist (z (list (aref b 2) (aref b 5)))
                       (pt x y z))))))))))
    points))

(defun financial-chart-3d--fit (view)
  "Fit eas VIEW's camera to its data, unless its x-eas.graphics sets one.
Its target and distance; its zoom is 1 (the fit's).
The mark is lowered over every row at its timeline's end, so the
camera holds still while a replay plays or a stream updates.  Before
VIEW is first drawn."
  (when-let* ((g (graphics-eas-get view)))
    (let* ((graphics (graphics-eas-view-graphics g))
           (camera (financial-chart-3d--opt graphics "camera"))
           (timeline (financial-chart-3d--opt graphics "timeline"))
           (params (append (and (consp timeline)
                                (list (financial-chart-3d--key (or (plist-get timeline :param) "time"))
                                      (or (plist-get timeline :duration) 0)))
                           (graphics-eas-mark-inputs view)))
           (points (and (not (plist-get camera :target)) (not (plist-get camera :distance))
                        (financial-chart-3d--points
                         (append (graphics-eas-primitives graphics (plist-get (eas-view-data view) :rows) params)
                                 nil)))))
      (when points
        ;; The fit frames the data at zoom 1: a template's zoom would crop it.
        (graphics-eas-attach
         view (plist-put (copy-sequence graphics) :camera
                         (append (financial-chart-3d-camera-fit points (graphics-eas-camera graphics nil))
                                 (plist-put (copy-sequence camera) :zoom 1))))))))

(cl-defun financial-chart-3d-open (name &key bindings id show)
  "Open 3D variant NAME (\"ohlc\" or \"fc3d/ohlc\") and return its eas view.
BINDINGS default to the variant's example; ID names the view (default
\"fc3d/NAME\").  Its camera is fitted to the data
\(`financial-chart-3d-camera-fit').  With SHOW, draw it in a
graphics.el buffer.  Signals `financial-chart-3d-unavailable' without
graphics.el."
  (let* ((name (financial-chart-3d--name name))
         (spec (financial-chart-3d-spec name bindings))
         (view (graphics-eas-open spec :id (or id name))))
    (financial-chart-3d--fit view)
    (when show (graphics-eas-show view))
    view))

(defun financial-chart-3d--close (view)
  "Close VIEW and its graphics.el buffer, if any."
  (when-let* ((buffer (get-buffer (format "*graphics-eas %s*" (eas-view-id view)))))
    (kill-buffer buffer))
  (eas-play-detach view)
  (eas-view-close view))

(defun financial-chart-3d-seek (view phase)
  "Move VIEW's replay, if it has one, to PHASE (0 to 1) of it and pause it."
  (when-let* ((timeline (graphics-eas-timeline view)))
    (graphics-eas-timeline-set view :seconds (* phase (or (plist-get timeline :duration) 0)) :playing nil)))

(defun financial-chart-3d-seek-end (view)
  "Move VIEW's replay, if it has one, to its end and pause it."
  (financial-chart-3d-seek view 1.0))

(defun financial-chart-3d-frame (name &optional cols rows palette phase)
  "The FRAME of 3D variant NAME with its example, at PHASE of its replay.
COLS x ROWS default to 60 x 24 and PALETTE to `dark' (the goldens'),
PHASE (0 to 1) to 1, its end; drawn at the fitted camera by the loaded
core.  Headless."
  (financial-chart-3d--ensure)
  (let ((view (financial-chart-3d-open name :id (format "financial-chart-3d-frame:%s" name))))
    (unwind-protect
        (progn (financial-chart-3d-seek view (or phase 1.0))
               (graphics-eas-frame view (or cols 60) (or rows 24) palette))
      (financial-chart-3d--close view))))

;;; Goldens and the gallery page

(defconst financial-chart-3d-golden-directory
  (expand-file-name "test/golden/3d" financial-chart-root)
  "Where the 3D goldens live: NAME.frame and NAME.txt, 60x24, dark.
At the end of a replay; the set at its middle is in t0.5/
\(`financial-chart-3d-golden-phases').")

(defconst financial-chart-3d-golden-phases '((1.0 . "") (0.5 . "t0.5"))
  "The goldens' replay phases: (PHASE . SUBDIRECTORY).")

(defun financial-chart-3d-write-goldens (&optional directory names)
  "Write DIRECTORY/NAME.frame and NAME.txt for each 3D variant NAME.
DIRECTORY defaults to `financial-chart-3d-golden-directory', NAMES to
every variant; 60 x 24, dark, the fitted camera, the Rust core; one
set per `financial-chart-3d-golden-phases'."
  (financial-chart-3d--ensure)
  (require 'graphics-frame)
  (graphics-ensure-core 'rs)
  (pcase-dolist (`(,phase . ,sub) financial-chart-3d-golden-phases)
    (let ((directory (expand-file-name sub (or directory financial-chart-3d-golden-directory))))
      (make-directory directory t)
      (dolist (name (or names (financial-chart-3d-variants)))
        (let ((frame (financial-chart-3d-frame name nil nil nil phase)))
          (graphics-frame-write-file (expand-file-name (concat name ".frame") directory) frame 60 24)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (graphics-eas-braille-text frame 60) nil
                          (expand-file-name (concat name ".txt") directory) nil 'silent)))))))

(defun financial-chart-3d--describe (name)
  "Variant NAME's mark, reading and timeline as a plist."
  (let* ((meta (plist-get (eas-template-get (financial-chart-3d--name name)) :meta))
         (graphics (plist-get meta :graphics))
         (doc (plist-get meta :doc)))
    (list :name name :mark (plist-get graphics :mark)
          :reading (and (string-match ": \\(.*?\\)\\.  2D:" doc) (match-string 1 doc))
          :replay (and (plist-get graphics :timeline) t)
          :example (plist-get meta :example))))

(defun financial-chart-3d-write-gallery (&optional file directory)
  "Write FILE (default docs/3d.md): every variant with its golden render.
The renders are read from DIRECTORY (default
`financial-chart-3d-golden-directory'); run
`financial-chart-3d-write-goldens' first."
  (let ((file (or file (expand-file-name "docs/3d.md" financial-chart-root)))
        (directory (or directory financial-chart-3d-golden-directory))
        (names (financial-chart-3d-variants)))
    (with-temp-file file
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert "# 3D financial charts (graphics.el)\n\n"
              "Generated by `make golden-3d` (`financial-chart-3d-write-gallery`); do not\n"
              "edit by hand.\n\n"
              "Every financial-chart template has a 3D variant in `templates3d/`: the\n"
              "template with its body unchanged plus an `x-eas.graphics` naming a\n"
              "[graphics.el](https://gitlab.com/davidawad/graphics-el) mark\n"
              "(graphics.el's `docs/design/eas-integration.md`).\n"
              "`src/financial-chart-3d.el` registers them with eas under the namespace\n"
              "`fc3d`, so `fc3d/ohlc` is the variant of `ohlc`. eas still draws each in\n"
              "2D; graphics.el draws it in 3D.\n\n"
              "graphics.el is optional: without it everything 2D works as before and the\n"
              "3D commands signal `financial-chart-3d-unavailable`, saying how to install\n"
              "it.\n\n"
              "```elisp\n"
              "(require 'financial-chart-3d)\n"
              "(financial-chart-3d-open \"ohlc\" :show t)                 ; its example\n"
              "(financial-chart-3d-open \"ohlc\" :bindings '(:bars BARS) :show t)\n"
              "M-x financial-chart-3d-gallery     ; every variant, a grid of windows\n"
              "M-x financial-chart-3d-demo-book   ; the order-book surface, streaming\n"
              "(setq v (financial-chart-3d-book-open SNAPSHOT :show t))  ; live book\n"
              "(financial-chart-3d-book-push v DELTAS)\n"
              "```\n\n"
              "In a 3D buffer the arrows orbit, `+`/`-` zoom and `0` resets; a replay\n"
              "plays and pauses with `SPC`, steps with `,` and `.` and changes speed\n"
              "with `[` and `]`.\n\n"
              "Marks: `bar3d` and `depth3d` are graphics.el's own. The `fc-` marks are\n"
              "registered by `financial-chart-3d.el` with `graphics-eas-define-mark`:\n"
              "candles whose scales span the whole series, stacked panes, a volume\n"
              "profile beside candles, the underwater curve, a payoff heightfield, depth\n"
              "walls and sign-coloured bars.\n\n"
              "A replay draws every row at every moment: a series starts from its first\n"
              "third in colour, the rest a dim ghost the replay colours bar by bar; a\n"
              "payoff surface stays whole while a bright trace sweeps it to expiry. The\n"
              "camera is fitted to the data (`financial-chart-3d-camera-fit`), so it\n"
              "fills its pane at any size and holds still while a replay plays.\n\n"
              "Each render below is the variant's golden (`test/golden/3d/NAME.txt`): its\n"
              "example at 60x24 cells, dark palette, the fitted camera, at the end of its\n"
              "replay, drawn by the Rust core; a replay's middle follows it\n"
              "(`test/golden/3d/t0.5/NAME.txt`). The Braille text keeps the dots, not the\n"
              "colours. `make test-3d` checks every variant against its template, draws\n"
              "its goldens bit for bit and checks it reads at five sizes and five phases\n"
              "(`test/financial-chart-3d-framing-test.el`).\n\n"
              "| variant | mark | replay | 3D reading |\n|---|---|---|---|\n")
      (dolist (name names)
        (let ((d (financial-chart-3d--describe name)))
          (insert (format "| [%s](#%s) | `%s` | %s | %s |\n" name name (plist-get d :mark)
                          (if (plist-get d :replay) "yes" "") (plist-get d :reading)))))
      (dolist (name names)
        (let ((d (financial-chart-3d--describe name)))
          (insert (format "\n## %s\n\n`fc3d/%s`, mark `%s`, example `%s`. %s.\n\n```text\n"
                          name name (plist-get d :mark) (plist-get d :example)
                          (financial-chart-3d--capitalize (plist-get d :reading))))
          (insert-file-contents (expand-file-name (concat name ".txt") directory))
          (goto-char (point-max))
          (insert "```\n")
          (when (plist-get d :replay)
            (insert "\nHalfway through its replay:\n\n```text\n")
            (insert-file-contents (expand-file-name (concat name ".txt") (expand-file-name "t0.5" directory)))
            (goto-char (point-max))
            (insert "```\n")))))))

(defun financial-chart-3d--capitalize (string)
  "STRING with its first letter upper case."
  (if (string-empty-p string) string
    (concat (upcase (substring string 0 1)) (substring string 1))))

;;; The gallery

(defconst financial-chart-3d-pane-shape 2.5
  "The width / height, in cells, of the gallery pane the grid aims for.
A cell is about twice as high as wide, so this is about 5:4 on screen.")

(defun financial-chart-3d--grid (n width height)
  "The panes per row of a gallery of N panes in WIDTH x HEIGHT cells.
A list, one count per row.  Every row is split evenly, so no pane spans
two columns: N rows times columns when that gives panes near
`financial-chart-3d-pane-shape', else rows whose counts differ by one
\(11 as 4, 4, 3)."
  (let (best)
    (dotimes (i n)
      (let* ((rows (1+ i))
             (cols (ceiling n (float rows)))
             (counts (cl-loop for r below rows
                              collect (+ (/ n rows) (if (< r (% n rows)) 1 0))))
             (score (+ (abs (log (/ (/ (float width) cols)
                                    (* financial-chart-3d-pane-shape (/ (float height) rows)))))
                       (if (zerop (% n rows)) 0 1.0))))
        (when (or (null best) (< score (car best)))
          (setq best (cons score counts)))))
    (cdr best)))

(defun financial-chart-3d--split (window n side)
  "Split WINDOW into N windows side by side (SIDE `right' or `below').
Each split leaves WINDOW its share and splits the rest again, so the
N are as equal as the cells allow.  Return them in order."
  (let ((out (list window)))
    (cl-loop for k downfrom n above 1
             do (setq window (split-window window (/ (if (eq side 'below) (window-total-height window)
                                                         (window-total-width window))
                                                       k)
                                           side))
             (push window out))
    (nreverse out)))

;;;###autoload
(defun financial-chart-3d-gallery (&optional names)
  "Open every 3D variant with its example in a grid of windows.
NAMES default to all of `financial-chart-3d-variants'; with a prefix
argument, read them.  The panes tile the frame row by row, each row
split evenly (`financial-chart-3d--grid').  Each window orbits with its
own keys; replays play with SPC."
  (interactive
   (list (and current-prefix-arg
              (completing-read-multiple "Variants: " (financial-chart-3d-variants) nil t))))
  (financial-chart-3d--ensure)
  (graphics-eas-ensure-core)
  (let* ((names (or names (financial-chart-3d-variants)))
         (window-min-height 2) (window-min-width 8)
         (_ (delete-other-windows))
         (counts (financial-chart-3d--grid (length names) (window-total-width) (window-total-height)))
         (panes (mapcan (lambda (row) (financial-chart-3d--split row (pop counts) 'right))
                        (financial-chart-3d--split (selected-window) (length counts) 'below))))
    (balance-windows)
    (cl-loop for name in names for pane in panes
             do (let ((old (gethash (financial-chart-3d--name name) eas-views)))
                  ;; A gallery opened before: its views go first.
                  (when old (financial-chart-3d--close old))
                  (graphics-eas-show (financial-chart-3d-open name) nil nil pane)))
    (select-window (car panes))))

;;; Live order books in 3D

(defvar financial-chart-3d--books (make-hash-table :test 'equal)
  "Live 3D book views by view id.
Each is a plist (:view :book :levels :step :history :template).")

(defun financial-chart-3d--book-rows (book levels template step)
  "BOOK's LEVELS per side for TEMPLATE, stamped with STEP.
As `financial-chart-book-rows' draws them.
Depth-live rows carry :step, the surface's depth axis."
  (let ((rows (financial-chart-book-rows book :levels levels :flash nil)))
    (if (equal template "depth-live")
        (vconcat (mapcar (lambda (r) (append r (list :step step))) rows))
      rows)))

(defun financial-chart-3d--book-window (live rows)
  "The eas stream window of LIVE given a frame of ROWS."
  (if (equal (plist-get live :template) "depth-live")
      (* (plist-get live :history) (1+ (* 2 (plist-get live :levels))))
    (length rows)))

(cl-defun financial-chart-3d-book-open (snapshot &key (template "depth-live") levels (history 60) id show)
  "Open a live 3D view of order book SNAPSHOT and return it.
TEMPLATE is \"depth-live\" (default: a surface of cumulative depth by
price and step, the newest HISTORY steps) or \"ladder\" (3D bars of the
current book).  SNAPSHOT and LEVELS are as in
`financial-chart-book-open'.  ID names the view; SHOW draws it.  Feed
it with `financial-chart-3d-book-push'."
  (unless (member template financial-chart-book-templates)
    (signal 'financial-chart-invalid-book
            (list (format "No 3D order-book template %S; templates: %s" template
                          (string-join financial-chart-book-templates ", "))
                  :code "UNKNOWN_TEMPLATE" :path "/template")))
  (financial-chart-3d--ensure)
  (require 'eas-stream)
  (when-let* ((old (and id (gethash id financial-chart-3d--books))))
    (when (eq (gethash id eas-views) (plist-get old :view))
      (financial-chart-3d-book-close id)))
  (let* ((book (if (financial-chart-book-p snapshot) snapshot (financial-chart-book-make snapshot)))
         (levels (or levels financial-chart-book-levels))
         (rows (financial-chart-3d--book-rows book levels template 0))
         (view (financial-chart-3d-open template :bindings (list :data rows)
                                        :id (or id (concat "fc3d/book:" template))))
         (live (list :view view :book book :levels levels :step 0 :history history :template template)))
    (eas-stream-attach view (list :max-fps (or financial-chart-book-max-fps
                                               (financial-chart-book-default-fps levels))
                                  :window (financial-chart-3d--book-window live rows)))
    (puthash (eas-view-id view) live financial-chart-3d--books)
    (when show (graphics-eas-show view))
    view))

(defun financial-chart-3d--book-live (view)
  "The live 3D book of VIEW (an id or view), or signal NO_BOOK."
  (let* ((id (if (eas-view-p view) (eas-view-id view) view))
         (live (gethash id financial-chart-3d--books)))
    (unless (and live (eq (gethash id eas-views) (plist-get live :view)))
      (remhash id financial-chart-3d--books)
      (signal 'financial-chart-invalid-book
              (list (format "View %S is not a live 3D order book; open one with financial-chart-3d-book-open" id)
                    :code "NO_BOOK" :path "")))
    live))

(defun financial-chart-3d-book-push (view deltas &optional now)
  "Apply order-book DELTAS to 3D book VIEW at NOW and stream the new frame.
DELTAS are as in `financial-chart-book-push'.  A depth-live view adds
the book as the next step of its surface; a ladder view replaces its
bars, which grow and shrink to the new sizes.  Returns the book's
summary with :step."
  (let* ((live (financial-chart-3d--book-live view))
         (book (plist-get live :book)))
    (financial-chart-book-apply book deltas now)
    (plist-put live :step (1+ (plist-get live :step)))
    (let ((rows (financial-chart-3d--book-rows book (plist-get live :levels) (plist-get live :template)
                                               (plist-get live :step))))
      (eas-stream-attach (plist-get live :view)
                         (list :max-fps (or financial-chart-book-max-fps
                                            (financial-chart-book-default-fps (plist-get live :levels)))
                               :window (financial-chart-3d--book-window live rows)))
      (eas-push (plist-get live :view) rows))
    (append (list :view (eas-view-id (plist-get live :view)) :step (plist-get live :step))
            (financial-chart-book-summary book))))

(defun financial-chart-3d-book-close (view)
  "Stop 3D book VIEW and close it."
  (let ((live (financial-chart-3d--book-live view)))
    (remhash (eas-view-id (plist-get live :view)) financial-chart-3d--books)
    (financial-chart-3d--close (plist-get live :view))))

;;; A deterministic book replay (examples, the demo)

(defun financial-chart-3d--lcg (seed)
  "A deterministic generator of floats in [0, 1) from integer SEED."
  (let ((state seed))
    (lambda ()
      (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
      (/ state 2147483648.0))))

(cl-defun financial-chart-3d-book-deltas (snapshot steps &key (seed 7))
  "Return STEPS batches of deltas that make SNAPSHOT's liquidity breathe.
SNAPSHOT is a plist (:bids [[PRICE SIZE] ...] :asks ...).  Each
batch updates every level of the book: its size follows a slow
wave that travels outward from the mid, plus noise (a generator
seeded with SEED), never below 0.5.  Prices never change, so the book
never crosses.  A list of delta vectors, deterministic."
  (let* ((random (financial-chart-3d--lcg seed))
         (levels (cl-loop for (side . key) in '(("bid" . :bids) ("ask" . :asks))
                          append (cl-loop for l across (vconcat (plist-get snapshot key))
                                          for i from 0
                                          collect (list side (float (elt l 0)) (float (elt l 1)) i)))))
    (cl-loop for step from 1 to steps
             collect (vconcat
                      (mapcar (lambda (l)
                                (pcase-let ((`(,side ,price ,size ,i) l))
                                  (list :op "update" :side side :price price
                                        :size (/ (fround (* 100 (max 0.5 (* size (+ 1.0 (* 0.6 (sin (- (/ step 4.0) (/ i 1.5))))
                                                                                     (* 0.3 (- (funcall random) 0.5)))))))
                                                 100.0))))
                              levels)))))

(cl-defun financial-chart-3d-book-history (snapshot steps &key levels (seed 7))
  "Return depth-live rows of SNAPSHOT replayed through STEPS delta batches.
Step 0 is SNAPSHOT; each later step applies the next batch of
`financial-chart-3d-book-deltas' (SEED).  A vector of
`financial-chart-book-rows' (LEVELS per side), each with :step."
  (let* ((book (financial-chart-book-make snapshot))
         (levels (or levels financial-chart-book-levels))
         (frames (list (financial-chart-3d--book-rows book levels "depth-live" 0))))
    (cl-loop for batch in (financial-chart-3d-book-deltas snapshot steps :seed seed)
             for step from 1
             do (financial-chart-book-apply book batch 0)
             (push (financial-chart-3d--book-rows book levels "depth-live" step) frames))
    (apply #'vconcat (nreverse frames))))

(defun financial-chart-3d--example-snapshot ()
  "The order-book snapshot of examples/order-book/book.json."
  (eas-json-read-file (expand-file-name "examples/order-book/book.json" financial-chart-root)))

;;;###autoload
(defun financial-chart-3d-demo-book (&optional steps interval)
  "Stream the example order book into a live 3D depth surface.
STEPS batches of deltas (default 240), one every INTERVAL seconds
\(default 0.2), from `financial-chart-3d-book-deltas'; killing the
buffer stops it."
  (interactive)
  (let* ((snapshot (financial-chart-3d--example-snapshot))
         (batches (financial-chart-3d-book-deltas snapshot (or steps 240)))
         (view (financial-chart-3d-book-open snapshot :id "fc3d/book-demo" :show t))
         (buffer (get-buffer (format "*graphics-eas %s*" (eas-view-id view))))
         (timer nil))
    (setq timer (run-with-timer
                 0 (or interval 0.2)
                 (lambda ()
                   (if (or (null batches) (not (buffer-live-p buffer)))
                       (cancel-timer timer)
                     (financial-chart-3d-book-push view (pop batches))))))
    view))

;;; Example data

(defun financial-chart-3d--write-json (file object)
  "Write plist OBJECT to FILE as JSON, an array of rows one row per line."
  (with-temp-file file
    (set-buffer-file-coding-system 'utf-8-unix)
    (let ((json-encoding-pretty-print nil))
      (insert "{\n")
      (cl-loop for (key value) on object by #'cddr for first = t then nil
               do (insert (if first "" ",\n") "  " (json-encode (substring (symbol-name key) 1)) ": ")
               (if (and (vectorp value) (> (length value) 0) (keywordp (car-safe (aref value 0))))
                   (progn (insert "[\n")
                          (cl-loop for row across value for i from 0
                                   do (insert (if (> i 0) ",\n" "") "    " (json-encode row)))
                          (insert "\n  ]"))
                 (insert (json-encode value))))
      (insert "\n}\n"))))

(defun financial-chart-3d--iron-condor ()
  "Expiry P/L of a short 90/95/105/110 iron condor for a 2.5 credit, 80 to 120."
  (cl-loop for s from 80 to 120 by 2
           collect (list s (- 2.5 (max 0 (- 95 s)) (- (max 0 (- 90 s))) (max 0 (- s 105)) (- (max 0 (- s 110)))))))

(defun financial-chart-3d-write-examples (&optional directory)
  "Write the 3D variants' own examples into DIRECTORY (examples/3d/).
payoff (the payoff example with its surface), payoff-curves (a short
iron condor at eight dates to expiry) and depth-live (40 steps of the
example book)."
  (let* ((directory (or directory (expand-file-name "examples/3d" financial-chart-root)))
         (payoff (eas-json-read-file (expand-file-name "examples/payoff.data.json" financial-chart-root)))
         (surface (financial-chart-3d-payoff-surface (plist-get payoff :data))))
    (make-directory directory t)
    (financial-chart-3d--write-json
     (expand-file-name "payoff.data.json" directory)
     (list :title "Long straddle, decaying to expiry"
           :data (plist-get payoff :data)
           :surface (vconcat surface)))
    (financial-chart-3d--write-json
     (expand-file-name "payoff-curves.data.json" directory)
     (list :title "Short iron condor by days to expiry"
           :data (vconcat
                  (mapcar (lambda (r)
                            (list :curve (if (zerop (plist-get r :dte)) "expiry"
                                           (format "T-%d" (plist-get r :dte)))
                                  :price (plist-get r :price) :pnl (plist-get r :pnl)))
                          (financial-chart-3d-payoff-surface
                           (financial-chart-3d--iron-condor) :vol 0.25)))))
    (financial-chart-3d--write-json
     (expand-file-name "depth-live.data.json" directory)
     (list :title "Order book, 40 steps"
           :data (financial-chart-3d-book-history (financial-chart-3d--example-snapshot) 39)))))

;;; From an invest metric's output to a template's bindings

(defconst financial-chart-3d-metric-adapter-groups
  '(financial-chart-3d-risk-metric-adapters financial-chart-3d-options-metric-adapters
    financial-chart-3d-positioning-metric-adapters financial-chart-3d-rates-metric-adapters
    financial-chart-3d-market-metric-adapters financial-chart-3d-valuation-metric-adapters
    financial-chart-3d-vol-surface-metric-adapters)
  "Each group's adapters: a list of (METRIC-ID TEMPLATE FUNCTION).
FUNCTION takes TEMPLATE, the metric's OUTPUT and ARGS (a plist) and
returns TEMPLATE's bindings; each lives in its group's file.")

(defun financial-chart-3d--metric-adapters ()
  "Every adapter, (METRIC-ID TEMPLATE FUNCTION), of each group.
The groups are `financial-chart-3d-metric-adapter-groups'."
  (require 'financial-chart-3d-valuation)
  (apply #'append (mapcar #'symbol-value financial-chart-3d-metric-adapter-groups)))

(defun financial-chart-3d-metric-templates ()
  "Which templates each invest metric feeds: an alist (METRIC-ID TEMPLATE ...).
Metric ids are `invest metric run' ids; every pair is one
`financial-chart-3d-bindings-from-metric' call."
  (let (alist)
    (pcase-dolist (`(,metric ,template ,_) (financial-chart-3d--metric-adapters))
      (let ((cell (assoc metric alist)))
        (if cell (nconc cell (list template)) (push (list metric template) alist))))
    (nreverse alist)))

(defun financial-chart-3d-bindings-from-metric (template metric-id output &rest args)
  "Bindings for TEMPLATE from OUTPUT, what `invest metric run METRIC-ID' emits.
OUTPUT is the metric's JSON parsed as eas parses it (plists with
keyword keys, vectors, :null).  It is one run's output, or a series
of snapshots: a list or vector of entries {output, as_of|time|date,
...}, each an output with what stamps it (its date, and for some
metrics its tenor, expiry, holding, scenario, strategy or symbol; the
group docs, docs/3d-*.md, name them) plus, for metrics whose output
does not echo it, the input it was run on.  ARGS is a plist of the
same stamps for a single output, of the adapter's options, and of
TEMPLATE's own slots (:title, :spot, ...), which are added last (the
rest are not bindings), except a value outside the slot's enum that
the adapter resolved itself (cds-term-structure's :measure \"auto\").
Returns the plist `financial-chart-3d-open' and `eas-resolve' take as
bindings, the same in 3D and 2D.  `financial-chart-3d-metric-templates'
lists the pairs.  Signals `financial-chart-invalid-data' with :code, :index (the
entry) and :field when OUTPUT is not what METRIC-ID emits."
  (let ((adapter (seq-find (lambda (a) (and (equal (car a) metric-id) (equal (nth 1 a) template)))
                           (financial-chart-3d--metric-adapters))))
    (unless adapter
      (let ((feeds (cdr (assoc metric-id (financial-chart-3d-metric-templates)))))
        (financial-chart--invalid
         nil "metric" "unknown_metric"
         (if feeds
             "%s does not feed %s; it feeds %s"
           "%s feeds no 3D template%s%s; see `financial-chart-3d-metric-templates'")
         metric-id (if feeds template "") (if feeds (string-join feeds ", ") ""))))
    (let ((bindings (funcall (nth 2 adapter) template output args))
          (slots (plist-get (plist-get (eas-template-get template) :meta) :slots)))
      ;; An option spelled like a slot that the slot would refuse (cds
      ;; :measure "auto") is the adapter's: it bound what it resolved.
      (cl-loop for (k v) on args by #'cddr
               when (and (plist-member slots k)
                         (not (and (plist-member bindings k)
                                   (plist-get (plist-get slots k) :enum)
                                   (not (seq-contains-p (plist-get (plist-get slots k) :enum) v)))))
               do (setq bindings (plist-put bindings k v)))
      bindings)))

(defun financial-chart-3d-metric-entries (output)
  "OUTPUT's snapshots: a list of entry plists, each with :output.
OUTPUT is a sequence of entries {output, ...} (a series), or one
output, which becomes the one entry (:output OUTPUT)."
  (if (and (sequencep output) (not (stringp output)) (not (keywordp (car-safe output)))
           (> (length output) 0)
           (seq-every-p (lambda (e) (and (keywordp (car-safe e)) (plist-member e :output))) output))
      (append output nil)
    (list (list :output output))))

(defun financial-chart-3d-metric-stamp (entry args keys &optional default)
  "The first of KEYS ENTRY carries, else ARGS carries, else DEFAULT.
KEYS are keywords; ARGS the plist `financial-chart-3d-bindings-from-metric'
was given, read at the same keys and their dashed spellings (:as-of
for :as_of)."
  (or (seq-some (lambda (k) (financial-chart-3d--value entry k)) keys)
      (seq-some (lambda (k)
                  (or (financial-chart-3d--value args k)
                      (financial-chart-3d--value
                       args (intern (replace-regexp-in-string "_" "-" (symbol-name k))))))
                keys)
      default))

(defun financial-chart-3d-metric-get (object &rest keys)
  "OBJECT's value at the path KEYS (keywords); nil for null or missing."
  (let ((v object))
    (dolist (k keys v)
      (setq v (and (keywordp (car-safe v)) (financial-chart-3d--value v k))))))

(defun financial-chart-3d-metric-list (v)
  "V, a JSON array (vector) or list, as a list; nil otherwise."
  (cond ((vectorp v) (append v nil))
        ((and (listp v) (not (keywordp (car-safe v)))) v)))

(defun financial-chart-3d-metric-round (v digits)
  "V rounded to DIGITS decimals, a float."
  (let ((m (expt 10.0 digits))) (/ (fround (* v m)) m)))

(defun financial-chart-3d-metric-key-name (key)
  "KEY, a keyword of a parsed JSON object, as the name it had (no colon)."
  (substring (symbol-name key) 1))

(provide 'financial-chart-3d)
(require 'financial-chart-3d-positioning)
;;; financial-chart-3d.el ends here
(require 'financial-chart-3d-rates)
