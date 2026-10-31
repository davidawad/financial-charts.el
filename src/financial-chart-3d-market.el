;;; financial-chart-3d-market.el --- 3D market structure: rotation, breadth, liquidity, session profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Four market-structure charts with 3D variants (templates3d/) and 2D
;; fallbacks (templates/financial/), loaded by financial-chart-3d.el:
;;
;;   rrg                relative rotation graph: each sector's RS-Ratio
;;                      (across) by RS-Momentum (back), its trail of the
;;                      last `tail' dates rising to its newest point;
;;                      replays date by date (mark fc-rrg3d)
;;   sector-breadth     breadth (percent of members above their moving
;;                      average) by sector and date as a surface that
;;                      grows toward the viewer, the newest date traced
;;                      bright (mark fc-breadth3d)
;;   liquidity-heatmap  resting order-book size by price and time, a
;;                      Bookmap-style surface with the mid price traced
;;                      across it; replays step by step and streams live
;;                      (`financial-chart-3d-liquidity-open',
;;                      mark fc-liquidity3d)
;;   session-profile    volume at price per session, one profile slab
;;                      per session, point of control capped bright and
;;                      the sessions' VWAPs joined; builds session by
;;                      session (mark fc-session-profile3d)
;;
;; Each template takes the rows its invest-metrics metric emits
;; (docs/3d-market.md has the contracts); the helpers here build them
;; from prices, bars or a book for callers that hold those instead:
;; `financial-chart-3d-rrg-rows', `financial-chart-3d-session-profile'
;; and `financial-chart-3d-liquidity-rows'.  The marks register with
;; graphics.el when it loads (`graphics-eas-define-mark').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart)
(require 'financial-chart-eas-book)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function graphics-eas-open "graphics-eas" (source &rest args))
(declare-function graphics-eas-show "graphics-eas" (view &optional cols rows window))
(declare-function eas-stream-attach "eas-stream" (view &optional options))
(declare-function financial-chart-3d--opt "financial-chart-3d" (graphics name &optional default))
(declare-function financial-chart-3d--field "financial-chart-3d" (graphics channel default))
(declare-function financial-chart-3d--value "financial-chart-3d" (row key))
(declare-function financial-chart-3d--rows "financial-chart-3d" (rows))
(declare-function financial-chart-3d--categories "financial-chart-3d" (values))
(declare-function financial-chart-3d--r3 "financial-chart-3d" (v))
(declare-function financial-chart-3d--domain "financial-chart-3d" (rows))
(declare-function financial-chart-3d--replay "financial-chart-3d" (graphics rows field mark))
(declare-function financial-chart-3d--axes "financial-chart-3d" (id x0 x1 y1 z0 z1 &optional color))
(declare-function financial-chart-3d--lcg "financial-chart-3d" (seed))
(declare-function financial-chart-3d--close "financial-chart-3d" (view))
(declare-function financial-chart-3d--ensure "financial-chart-3d" ())
(declare-function financial-chart-3d--write-json "financial-chart-3d" (file object))
(declare-function financial-chart-3d-profile-volumes "financial-chart-3d" (bars low high bins))
(declare-function financial-chart-3d-spec "financial-chart-3d" (name &optional bindings))

(defvar financial-chart-3d-up-color)
(defvar financial-chart-3d-down-color)

;;; Shared helpers

(defconst financial-chart-3d-market-colors
  ["#4c78a8" "#f58518" "#e45756" "#72b7b2" "#54a24b" "#eeca3b"
   "#b279a2" "#ff9da6" "#9d755d" "#bab0ac" "#8cd17d" "#d4a6c8"]
  "Series colours of the market-structure marks, by first appearance.")

(defun financial-chart-3d-market--round (v places)
  "V rounded to PLACES decimals."
  (let ((k (expt 10.0 places))) (/ (fround (* v k)) k)))

(defun financial-chart-3d-market--keys (values)
  "Distinct non-nil VALUES sorted: numbers ascending, ISO dates in order."
  (let ((seen (delete-dups (copy-sequence (delq nil values)))))
    (cond ((seq-every-p #'numberp seen) (sort seen #'<))
          ((seq-every-p #'stringp seen) (sort seen #'string<))
          (t seen))))

(defun financial-chart-3d-market--index (keys)
  "A hash table from each of KEYS to its position."
  (let ((index (make-hash-table :test #'equal)))
    (cl-loop for k in keys for i from 0 do (puthash k i index))
    index))

(defun financial-chart-3d-market--colors (graphics keys)
  "KEY -> colour alist for KEYS: GRAPHICS's `colors' map, else the palette."
  (let ((map (financial-chart-3d--opt graphics "colors")))
    (cl-loop for k in keys for i from 0
             collect (cons k (or (and map (plist-get map (intern (format ":%s" k))))
                                 (aref financial-chart-3d-market-colors
                                       (% i (length financial-chart-3d-market-colors))))))))

(defun financial-chart-3d-market--spread (rows key centre)
  "The largest distance of ROWS' KEY from CENTRE, padded, at least 0.5."
  (* 1.1 (max 0.5 (apply #'max 0 (mapcar (lambda (r) (abs (- (financial-chart-3d--value r key) centre)))
                                         rows)))))

(defun financial-chart-3d-market--seg (x0 y0 z0 x1 y1 z1)
  "A segment, its coordinates rounded as graphics.el's marks round."
  (vector (financial-chart-3d--r3 x0) (financial-chart-3d--r3 y0) (financial-chart-3d--r3 z0)
          (financial-chart-3d--r3 x1) (financial-chart-3d--r3 y1) (financial-chart-3d--r3 z1)))

(defun financial-chart-3d-market--rect (y x0 z0 x1 z1)
  "The outline of X0..X1 by Z0..Z1 at height Y, as four segments."
  (list (financial-chart-3d-market--seg x0 y z0 x1 y z0) (financial-chart-3d-market--seg x1 y z0 x1 y z1)
        (financial-chart-3d-market--seg x1 y z1 x0 y z1) (financial-chart-3d-market--seg x0 y z1 x0 y z0)))

;;; Relative rotation graph

(defun financial-chart-3d-market--mean (values)
  "The mean of the numbers VALUES."
  (/ (apply #'+ values) (float (length values))))

(cl-defun financial-chart-3d-rrg-rows (closes benchmark &key dates (ratio 10) (momentum 5))
  "Relative rotation rows of CLOSES against BENCHMARK.
CLOSES is an alist (SYMBOL . PRICES), BENCHMARK the benchmark's prices,
all oldest first and the same length; DATES labels each period
\(default its index).  For each period with enough history, one row per
symbol {date, symbol, rs_ratio, rs_momentum}, both centred on 100:
rs = close / benchmark, rs_ratio = 100 * rs / mean(rs over the last
RATIO periods) and rs_momentum = 100 * rs_ratio / mean(rs_ratio over
the last MOMENTUM periods) -- an open approximation of JdK's RS-Ratio
and RS-Momentum, rotating clockwise through leading, weakening, lagging
and improving.  Rows run date by date, symbols in CLOSES's order."
  (let* ((bench (vconcat benchmark))
         (n (length bench))
         (dates (if dates (vconcat dates) (vconcat (number-sequence 0 (1- n)))))
         (series
          (mapcar
           (lambda (entry)
             (let* ((prices (vconcat (cdr entry)))
                    (rs (cl-loop for i below n collect (/ (float (aref prices i)) (aref bench i))))
                    (ratios (cl-loop for i below n
                                     collect (and (>= i (1- ratio))
                                                  (* 100 (/ (nth i rs)
                                                            (financial-chart-3d-market--mean
                                                             (seq-subseq rs (- i (1- ratio)) (1+ i))))))))
                    (moms (cl-loop for i below n
                                   collect (let ((window (and (>= i (1- momentum))
                                                              (seq-subseq ratios (- i (1- momentum)) (1+ i)))))
                                             (and window (seq-every-p #'numberp window)
                                                  (* 100 (/ (nth i ratios)
                                                            (financial-chart-3d-market--mean window))))))))
               (list (car entry) (vconcat ratios) (vconcat moms))))
           closes)))
    (unless (= (length dates) n)
      (signal 'financial-chart-invalid-data
              (list "rrg rows need one date per benchmark price" :code "length_mismatch"
                    :index nil :field "dates")))
    (cl-loop for i below n
             append (cl-loop for (symbol ratios moms) in series
                             when (aref moms i)
                             collect (list :date (aref dates i) :symbol symbol
                                           :rs_ratio (financial-chart-3d-market--round (aref ratios i) 2)
                                           :rs_momentum (financial-chart-3d-market--round (aref moms i) 2))))))

(defun financial-chart-3d-market--rrg-mark (graphics rows)
  "Primitives of the fc-rrg3d mark: GRAPHICS over RRG ROWS.
ROWS are {date, symbol, rs_ratio, rs_momentum}.  The RRG lies on the
floor: RS-Ratio across (x), RS-Momentum away from the viewer (z), 100
at the centre, each axis scaled to the farthest point of the whole
series so a replay never rescales; the floor's edges are coloured by
quadrant: leading green, weakening yellow, lagging red, improving
blue.  Each symbol's trail of its last `tail' dates climbs from the
floor to its newest point, a cube with a stem down to where it stands
on the RRG today."
  (let* ((xk (financial-chart-3d--field graphics "x" "rs_ratio"))
         (zk (financial-chart-3d--field graphics "z" "rs_momentum"))
         (sk (financial-chart-3d--field graphics "color" "symbol"))
         (tk (financial-chart-3d--field graphics "time" "date"))
         (valid (lambda (rs) (seq-filter (lambda (r) (and (numberp (financial-chart-3d--value r xk))
                                                          (numberp (financial-chart-3d--value r zk))
                                                          (financial-chart-3d--value r sk)
                                                          (financial-chart-3d--value r tk)))
                                         rs)))
         (rows (funcall valid rows))
         (dom (funcall valid (financial-chart-3d--domain rows)))
         (tail (max 2 (financial-chart-3d--opt graphics "tail" 8)))
         (height (float (financial-chart-3d--opt graphics "height" 2.5)))
         (extent (financial-chart-3d--opt graphics "extent" [10 10]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (centre (float (financial-chart-3d--opt graphics "centre" 100)))
         (sx (/ w (financial-chart-3d-market--spread dom xk centre)))
         (sz (/ d (financial-chart-3d-market--spread dom zk centre)))
         (symbols (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r sk)) dom)))
         (colors (financial-chart-3d-market--colors graphics symbols))
         (shown (last (financial-chart-3d-market--keys (mapcar (lambda (r) (financial-chart-3d--value r tk)) rows))
                      tail))
         (index (financial-chart-3d-market--index shown))
         (k (length shown))
         (pos (lambda (r)
                (let ((age (- k 1 (gethash (financial-chart-3d--value r tk) index))))
                  (list (* sx (- (financial-chart-3d--value r xk) centre))
                        (* height (+ 0.1 (* 0.9 (- 1 (/ age (float (1- tail)))))))
                        (- (* sz (- (financial-chart-3d--value r zk) centre)))))))
         (r (financial-chart-3d--r3 (* 0.035 (min w d))))
         trails trail-colors stems heads head-colors floor)
    (dolist (symbol symbols)
      (let* ((color (cdr (assoc symbol colors)))
             (points (mapcar pos (sort (seq-filter (lambda (row) (and (equal (financial-chart-3d--value row sk) symbol)
                                                                      (gethash (financial-chart-3d--value row tk) index)))
                                                   rows)
                                       (lambda (a b) (< (gethash (financial-chart-3d--value a tk) index)
                                                        (gethash (financial-chart-3d--value b tk) index)))))))
        (cl-loop for (a b) on points while b
                 do (push (apply #'financial-chart-3d-market--seg (append a b)) trails)
                 (push color trail-colors))
        (when-let* ((head (car (last points))))
          (pcase-let ((`(,x ,y ,z) head))
            (push (financial-chart-3d-market--seg x y z x 0 z) stems)
            (push (vector (financial-chart-3d--r3 (- x r)) (financial-chart-3d--r3 (- y r)) (financial-chart-3d--r3 (- z r))
                          (financial-chart-3d--r3 (+ x r)) (financial-chart-3d--r3 (+ y r)) (financial-chart-3d--r3 (+ z r)))
                  heads)
            (push (vector (financial-chart-3d--r3 x) 0 (financial-chart-3d--r3 z)) floor)
            (push color head-colors)))))
    (let ((head-colors (vconcat (nreverse head-colors)))
          (leading (financial-chart-3d--opt graphics "leading_color" "#26a69a"))
          (weakening (financial-chart-3d--opt graphics "weakening_color" "#eeca3b"))
          (lagging (financial-chart-3d--opt graphics "lagging_color" "#ef5350"))
          (improving (financial-chart-3d--opt graphics "improving_color" "#4c78a8")))
      (vector
       ;; Momentum grows away from the viewer (-z): the back edge is the RRG's top.
       (list :id "quadrants" :type "segments"
             :segments (vector (financial-chart-3d-market--seg 0 0 (- d) w 0 (- d)) (financial-chart-3d-market--seg w 0 (- d) w 0 0)
                               (financial-chart-3d-market--seg w 0 0 w 0 d) (financial-chart-3d-market--seg w 0 d 0 0 d)
                               (financial-chart-3d-market--seg 0 0 d (- w) 0 d) (financial-chart-3d-market--seg (- w) 0 d (- w) 0 0)
                               (financial-chart-3d-market--seg (- w) 0 0 (- w) 0 (- d)) (financial-chart-3d-market--seg (- w) 0 (- d) 0 0 (- d)))
             :colors (vector leading leading weakening weakening lagging lagging improving improving))
       (list :id "cross" :type "segments" :color "#5b6b86"
             :segments (vector (financial-chart-3d-market--seg (- w) 0 0 w 0 0) (financial-chart-3d-market--seg 0 0 (- d) 0 0 d)
                               (financial-chart-3d-market--seg (- w) 0 (- d) (- w) height (- d))))
       (list :id "trails" :type "segments" :segments (vconcat (nreverse trails))
             :colors (vconcat (nreverse trail-colors)))
       (list :id "stems" :type "segments" :segments (vconcat (nreverse stems)) :colors head-colors)
       (list :id "heads" :type "boxes" :boxes (vconcat (nreverse heads)) :colors head-colors)
       (list :id "now" :type "points" :points (vconcat (nreverse floor)) :size 2 :colors head-colors)))))

;;; Surfaces: a lit heightfield, or ridges that read in Braille

(defun financial-chart-3d-market--surface (graphics surface &optional across)
  "SURFACE, a heightfield primitive plist, drawn in GRAPHICS's `style'.
\"surface\" is the heightfield itself; \"ridges\" (the default) draws
it as lines, one along each of its columns (with ACROSS, each of its
rows), every segment coloured on the heightfield's low to high
gradient: a wire surface whose lines stay apart in Braille, where a lit
surface fills in."
  (if (equal (financial-chart-3d--opt graphics "style" "ridges") "surface") surface
    (let* ((heights (plist-get surface :heights))
           (nr (length heights)) (nc (if (> nr 0) (length (aref heights 0)) 0))
           (x (plist-get surface :x)) (z (plist-get surface :z))
           (base (or (plist-get surface :base) 0)) (scale (or (plist-get surface :y_scale) 1))
           (range (plist-get surface :color_range))
           (lo (float (elt range 0))) (hi (float (elt range 1)))
           (low (plist-get surface :low_color)) (high (plist-get surface :high_color))
           (at (lambda (v0 v1 i n) (if (> n 1) (+ v0 (* (- v1 v0) (/ i (float (1- n))))) v0)))
           (point (lambda (r c) (list (funcall at (aref x 0) (aref x 1) c nc)
                                      (+ base (* scale (aref (aref heights r) c)))
                                      (funcall at (aref z 0) (aref z 1) r nr))))
           segments colors)
      (cl-loop for (r0 c0 r1 c1) in (if across
                                        (cl-loop for r below nr append (cl-loop for c below (1- nc) collect (list r c r (1+ c))))
                                      (cl-loop for c below nc append (cl-loop for r below (1- nr) collect (list r c (1+ r) c))))
               do (let ((a (aref (aref heights r0) c0)) (b (aref (aref heights r1) c1)))
                    (push (apply #'financial-chart-3d-market--seg (append (funcall point r0 c0) (funcall point r1 c1)))
                          segments)
                    (push (financial-chart-3d-market--mix
                           low high (if (> hi lo) (max 0.0 (min 1.0 (/ (- (/ (+ a b) 2.0) lo) (- hi lo)))) 1.0))
                          colors)))
      (list :id (plist-get surface :id) :type "segments"
            :segments (vconcat (nreverse segments)) :colors (vconcat (nreverse colors))))))

;;; Sector breadth over time

(defun financial-chart-3d-market--breadth-mark (graphics rows)
  "Primitives of the fc-breadth3d mark: GRAPHICS over ROWS {date, sector, pct}.
A heightfield: sectors across (x, in order of first appearance), dates
from the back toward the viewer (z, oldest at the back), height and
colour the breadth on `domain' (default 0 to 100, red to green).  The
`mid' level (50) is framed, the newest date traced bright across the
sectors, and the mean breadth over the sectors runs up the right-hand
wall.  Under a time encoding the grid and scales span every date and
the surface grows date by date toward the front."
  (let* ((xk (financial-chart-3d--field graphics "x" "sector"))
         (zk (financial-chart-3d--field graphics "z" "date"))
         (yk (financial-chart-3d--field graphics "y" "pct"))
         (valid (lambda (rs) (seq-filter (lambda (r) (and (financial-chart-3d--value r xk) (financial-chart-3d--value r zk)
                                                          (numberp (financial-chart-3d--value r yk))))
                                         rs)))
         (rows (funcall valid rows))
         (dom (funcall valid (financial-chart-3d--domain rows)))
         (sectors (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (dates (financial-chart-3d-market--keys (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom)))
         (shown (financial-chart-3d-market--keys (mapcar (lambda (r) (financial-chart-3d--value r zk)) rows)))
         (range (financial-chart-3d--opt graphics "domain" [0 100]))
         (lo (float (elt range 0))) (hi (float (elt range 1)))
         (mid (float (financial-chart-3d--opt graphics "mid" (/ (+ lo hi) 2))))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (extent (financial-chart-3d--opt graphics "extent" [8 12]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (scale (/ height (- hi lo)))
         (base (financial-chart-3d--r3 (- (* lo scale))))
         (nx (length sectors)) (nz (length dates)) (k (length shown))
         (x-at (lambda (i) (financial-chart-3d--r3 (if (> nx 1) (- (* 2 w (/ i (float (1- nx)))) w) 0))))
         (z-at (lambda (j) (financial-chart-3d--r3 (if (> nz 1) (- (* 2 d (/ j (float (1- nz)))) d) 0))))
         (cells (make-hash-table :test #'equal))
         (y-of (lambda (v) (financial-chart-3d--r3 (+ base (* scale v)))))
         (last-row nil)
         (grid nil))
    (dolist (r rows)
      (puthash (cons (financial-chart-3d--value r xk) (financial-chart-3d--value r zk))
               (max lo (min hi (float (financial-chart-3d--value r yk)))) cells))
    ;; A missing cell repeats its sector's previous date (else the floor).
    (dolist (date shown)
      (setq last-row (vconcat (cl-loop for s in sectors for i from 0
                                       collect (financial-chart-3d--r3
                                                (gethash (cons s date) cells (if last-row (aref last-row i) lo))))))
      (push last-row grid))
    (setq grid (nreverse grid))
    (let* ((newest (car (last grid)))
           (z-new (and newest (funcall z-at (1- k)))))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (if (and (>= nx 2) (>= k 2))
           (financial-chart-3d-market--surface
            graphics
            (list :id "surface" :type "heightfield" :x (vector (- w) w) :z (vector (- d) z-new)
                  :base base :y_scale (financial-chart-3d--r3 scale) :heights (vconcat grid)
                  :low_color (financial-chart-3d--opt graphics "low_color" financial-chart-3d-down-color)
                  :high_color (financial-chart-3d--opt graphics "high_color" financial-chart-3d-up-color)
                  :color_range (vector lo hi)))
         (list :id "surface" :type "segments" :segments []))
       (list :id "front" :type "segments" :color "#ffffff"
             :segments (if newest
                           (vconcat (cl-loop for i from 0 below (1- nx)
                                             collect (financial-chart-3d-market--seg
                                                      (funcall x-at i) (funcall y-of (aref newest i)) z-new
                                                      (funcall x-at (1+ i)) (funcall y-of (aref newest (1+ i))) z-new)))
                         []))
       (list :id "mid" :type "segments" :color "#8a94a6"
             :segments (vconcat (financial-chart-3d-market--rect (funcall y-of mid) (- w) (- d) w d)))))))

;;; Liquidity heatmap

(defun financial-chart-3d-market--tick-grid (prices)
  "The regular price grid spanning sorted distinct PRICES, highest first.
Its step is the smallest gap between them; with more than 400 levels,
PRICES themselves."
  (let* ((gaps (cl-loop for (a b) on prices while b collect (- b a)))
         (tick (and gaps (apply #'min gaps)))
         (count (and tick (> tick 0) (1+ (round (/ (- (car (last prices)) (car prices)) tick))))))
    (if (and count (<= count 400))
        (cl-loop for i from (1- count) downto 0
                 collect (financial-chart-3d-market--round (+ (car prices) (* i tick)) 6))
      (reverse prices))))

(defun financial-chart-3d-market--liquidity-mark (graphics rows)
  "Primitives of the fc-liquidity3d mark: GRAPHICS over book ROWS.
ROWS are order-book levels by step {step, side, price, size, mid}
\(`financial-chart-3d-liquidity-rows'), {side: \"mid\"} rows giving the
mid.  Time runs across (x, the newest `max_rows' steps), price away
from the viewer (z, highest at the back, on the book's tick grid).  In
`style' \"walls\" (the default) each level resting at least `wall'
\(3) times the median size stands up as a block as tall as its size,
dark blue to amber: the bright bands of Bookmap's heatmap, and the mid
price runs across the floor between them in white.  \"surface\"
draws every level as a heightfield (the heatmap raised) with the mid
traced over it, \"ridges\" one line per price."
  (let* ((xk (financial-chart-3d--field graphics "x" "step"))
         (zk (financial-chart-3d--field graphics "z" "price"))
         (yk (financial-chart-3d--field graphics "y" "size"))
         (sk (financial-chart-3d--field graphics "color" "side"))
         (book-p (lambda (r) (and (member (financial-chart-3d--value r sk) '("bid" "ask"))
                                  (numberp (financial-chart-3d--value r xk))
                                  (numberp (financial-chart-3d--value r zk))
                                  (numberp (financial-chart-3d--value r yk)))))
         (dom (seq-filter book-p (financial-chart-3d--domain rows)))
         (history (max 2 (financial-chart-3d--opt graphics "max_rows" 60)))
         (all-steps (last (financial-chart-3d-market--keys (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom))
                          history))
         (steps (last (financial-chart-3d-market--keys
                       (mapcar (lambda (r) (and (funcall book-p r) (financial-chart-3d--value r xk))) rows))
                      history))
         (kept (lambda (rs keep) (let ((index (financial-chart-3d-market--index keep)))
                                   (seq-filter (lambda (r) (gethash (financial-chart-3d--value r xk) index)) rs))))
         (dom (funcall kept dom all-steps))
         (book (funcall kept (seq-filter book-p rows) steps))
         (price-of (lambda (r) (financial-chart-3d-market--round (float (financial-chart-3d--value r zk)) 6)))
         (grid (and dom (financial-chart-3d-market--tick-grid
                         (financial-chart-3d-market--keys (mapcar price-of dom)))))
         (p-hi (car grid)) (p-lo (car (last grid)))
         (vmax (float (apply #'max 1e-9 (mapcar (lambda (r) (financial-chart-3d--value r yk)) dom))))
         (height (float (financial-chart-3d--opt graphics "height" 2)))
         (extent (financial-chart-3d--opt graphics "extent" [14 10]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (scale (/ height vmax))
         (nx (length all-steps)) (k (length steps))
         (x-at (lambda (i) (financial-chart-3d--r3 (if (> nx 1) (- (* 2 w (/ i (float (1- nx)))) w) 0))))
         ;; A step's column among every step's: a replay grows from the
         ;; left, a stream (whose rows are every step) scrolls.
         (column (let ((index (financial-chart-3d-market--index all-steps)))
                   (lambda (step) (gethash step index (- nx k)))))
         (z-of (lambda (p) (if (and p-hi (> p-hi p-lo)) (- (* 2 d (/ (- p-hi p) (- p-hi p-lo))) d) 0.0)))
         (cells (make-hash-table :test #'equal))
         (mids (make-hash-table :test #'equal)))
    (dolist (r book)
      (puthash (cons (financial-chart-3d--value r xk) (funcall price-of r))
               (float (financial-chart-3d--value r yk)) cells))
    (dolist (r rows)
      (let ((m (financial-chart-3d--value r (financial-chart-3d--field graphics "mid" "mid")))
            (s (financial-chart-3d--value r xk)))
        (when (and (numberp m) (numberp s) (not (gethash s mids)))
          (puthash s (float m) mids))))
    (let* ((heights (vconcat (mapcar (lambda (p) (vconcat (mapcar (lambda (s) (financial-chart-3d--r3 (gethash (cons s p) cells 0.0)))
                                                                  steps)))
                                     grid)))
           (surface-y (lambda (j p)
                        ;; The surface's height at step J and price P, between grid rows.
                        (let* ((n (length grid))
                               (u (if (> n 1) (/ (* (- p-hi p) (1- n)) (- p-hi p-lo)) 0))
                               (i0 (max 0 (min (1- n) (floor u)))) (i1 (min (1- n) (1+ i0)))
                               (f (- u i0)))
                          (* scale (+ (* (- 1 f) (aref (aref heights i0) j)) (* f (aref (aref heights i1) j)))))))
           (style (financial-chart-3d--opt graphics "style" "walls"))
           (low (financial-chart-3d--opt graphics "low_color" "#1a2a6c"))
           (high (financial-chart-3d--opt graphics "high_color" "#ffca28"))
           ;; On the floor between the walls, else on the surface.
           (trace (cl-loop for s in steps for j from 0
                           for m = (gethash s mids)
                           when (and m grid (<= p-lo m p-hi))
                           collect (list (funcall x-at (funcall column s))
                                         (if (equal style "walls") 0.04 (+ 0.06 (funcall surface-y j m)))
                                         (funcall z-of m)))))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (list :id "floor" :type "segments" :color "#5b6b86"
             :segments (vconcat (financial-chart-3d-market--rect 0 (- w) (- d) w d)))
       (cond ((not (and (>= k 2) (>= (length grid) 2)))
              (list :id "surface" :type "segments" :segments []))
             ((equal style "walls")
              (let* ((hx (* 0.42 (if (> nx 1) (/ (* 2 w) (1- nx)) w)))
                     (hz (* 0.42 (if (> (length grid) 1) (/ (* 2 d) (1- (length grid))) d)))
                     (sizes (sort (cl-loop for row across heights append (seq-filter #'cl-plusp row)) #'<))
                     (least (* (financial-chart-3d--opt graphics "wall" 3)
                               (if sizes (nth (/ (length sizes) 2) sizes) 0)))
                     boxes colors)
                (cl-loop for p in grid for i from 0
                         do (cl-loop for j from 0 below k for s in steps
                                     for v = (aref (aref heights i) j)
                                     when (>= v least)
                                     do (let ((x (funcall x-at (funcall column s))) (z (funcall z-of p)))
                                          (push (vector (financial-chart-3d--r3 (- x hx)) 0 (financial-chart-3d--r3 (- z hz))
                                                        (financial-chart-3d--r3 (+ x hx)) (financial-chart-3d--r3 (* scale v))
                                                        (financial-chart-3d--r3 (+ z hz)))
                                                boxes)
                                          (push (financial-chart-3d-market--mix low high (/ v vmax)) colors))))
                (list :id "surface" :type "boxes" :boxes (vconcat (nreverse boxes)) :colors (vconcat (nreverse colors)))))
             (t (financial-chart-3d-market--surface
                 graphics
                 (list :id "surface" :type "heightfield"
                       :x (vector (funcall x-at (funcall column (car steps))) (funcall x-at (funcall column (car (last steps)))))
                       :z (vector (- d) d)
                       :y_scale (financial-chart-3d--r3 scale) :heights heights
                       :low_color low :high_color high :color_range (vector 0 vmax))
                 t)))
       (list :id "mid" :type "segments" :color (financial-chart-3d--opt graphics "mid_color" "#ffffff")
             :segments (vconcat (cl-loop for (a b) on trace while b
                                         collect (apply #'financial-chart-3d-market--seg (append a b)))))))))

;;; Volume profile by session

(defun financial-chart-3d-market--session-of (bar)
  "BAR's session: the date of its time (an ISO string, or epoch ms UTC)."
  (let ((time (plist-get bar :time)))
    (cond ((stringp time) (substring time 0 (min 10 (length time))))
          ((numberp time) (format-time-string "%F" (seconds-to-time (/ time 1000.0)) t))
          (t (error "Bar has no time: %S (financial-chart-3d-session-profile)" bar)))))

(cl-defun financial-chart-3d-session-profile (bars &key (bins 24) session)
  "Volume-at-price rows per session of OHLCV BARS (oldest first).
BINS equal price levels span every bar's range, shared by all
sessions; each bar's volume spreads over its low-high range as in
`financial-chart-3d-profile-volumes'.  SESSION maps a bar to its
session (default the date of its :time).  Rows {session, price, low,
high, volume, poc, vwap}, session by session and price ascending:
price is the level's middle, poc 1 at the session's largest level (else
0), vwap the session's volume-weighted typical price."
  (let* ((bars (financial-chart-3d--rows bars))
         (session (or session #'financial-chart-3d-market--session-of))
         (lo (apply #'min (mapcar (lambda (b) (plist-get b :low)) bars)))
         (hi (apply #'max (mapcar (lambda (b) (plist-get b :high)) bars)))
         (step (/ (- hi lo) (float bins)))
         (groups nil))
    (dolist (b bars)
      (let* ((key (funcall session b)) (cell (assoc key groups)))
        (unless cell (setq cell (list key)) (push cell groups))
        (push b (cdr cell))))
    (cl-loop for (key . members) in (nreverse groups)
             append (let* ((members (nreverse members))
                           (volumes (financial-chart-3d-profile-volumes members lo hi bins))
                           (vmax (seq-reduce #'max volumes 0.0))
                           (total (apply #'+ (mapcar (lambda (b) (or (plist-get b :volume) 0)) members)))
                           (vwap (if (> total 0)
                                     (/ (apply #'+ (mapcar (lambda (b) (* (or (plist-get b :volume) 0)
                                                                         (/ (+ (plist-get b :high) (plist-get b :low)
                                                                               (plist-get b :close))
                                                                            3.0)))
                                                           members))
                                        total)
                                   (plist-get (car (last members)) :close)))
                           (poc (seq-position volumes vmax)))
                      (cl-loop for i below bins
                               collect (list :session key
                                             :price (financial-chart-3d-market--round (+ lo (* (+ i 0.5) step)) 4)
                                             :low (financial-chart-3d-market--round (+ lo (* i step)) 4)
                                             :high (financial-chart-3d-market--round (+ lo (* (1+ i) step)) 4)
                                             :volume (financial-chart-3d-market--round (aref volumes i) 0)
                                             :poc (if (and (> vmax 0) (eql i poc)) 1 0)
                                             :vwap (financial-chart-3d-market--round vwap 4)))))))

(defun financial-chart-3d-market--session-profile-mark (graphics rows)
  "Primitives of the fc-session-profile3d mark: GRAPHICS over profile ROWS.
ROWS are {session, price, volume, vwap}.  Price across (x), sessions
from the back toward the viewer (z, oldest at the back), one slab per
session whose top is its volume at each price (height over the largest
level of every session, so a heavy session stands taller), coloured old
\(blue) to new (amber).  Each session's point of control is capped by
a bright bar and the sessions' VWAPs are joined across the floor: value
migrating session by session."
  (let* ((xk (financial-chart-3d--field graphics "x" "price"))
         (zk (financial-chart-3d--field graphics "z" "session"))
         (yk (financial-chart-3d--field graphics "y" "volume"))
         (vk (financial-chart-3d--field graphics "vwap" "vwap"))
         (valid (lambda (rs) (seq-filter (lambda (r) (and (numberp (financial-chart-3d--value r xk))
                                                          (financial-chart-3d--value r zk)
                                                          (numberp (financial-chart-3d--value r yk))))
                                         rs)))
         (rows (funcall valid rows))
         (dom (funcall valid (financial-chart-3d--domain rows)))
         (sessions (financial-chart-3d-market--keys (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom)))
         (prices (financial-chart-3d-market--keys (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (p-lo (if prices (float (car prices)) 0.0)) (p-hi (if prices (float (car (last prices))) 1.0))
         (vmax (float (apply #'max 1e-9 (mapcar (lambda (r) (financial-chart-3d--value r yk)) dom))))
         (height (float (financial-chart-3d--opt graphics "height" 3.5)))
         (extent (financial-chart-3d--opt graphics "extent" [12 10]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (ns (length sessions))
         (gap (if (> ns 1) (/ (* 2 d) (1- ns)) (* 2 d)))
         (half (* 0.5 gap (financial-chart-3d--opt graphics "band" 0.3)))
         (x-of (lambda (p) (if (> p-hi p-lo) (- (* 2 w (/ (- p p-lo) (- p-hi p-lo))) w) 0.0)))
         (z-of (lambda (j) (if (> ns 1) (- (* gap j) d) 0.0)))
         (y-of (lambda (v) (* height (/ v vmax))))
         (stops (list (financial-chart-3d--opt graphics "old_color" "#4c78a8")
                      (financial-chart-3d--opt graphics "new_color" "#f58518")))
         (ramp (lambda (j) (financial-chart-3d-market--mix (car stops) (cadr stops)
                                                           (if (> ns 1) (/ j (float (1- ns))) 1.0))))
         (verts nil) (tris nil) (tri-colors nil) (n 0)
         tops top-colors pocs vwaps)
    (cl-loop for s in sessions for j from 0
             for points = (sort (mapcar (lambda (r) (cons (float (financial-chart-3d--value r xk))
                                                          (float (financial-chart-3d--value r yk))))
                                        (seq-filter (lambda (r) (equal (financial-chart-3d--value r zk) s)) rows))
                                (lambda (a b) (< (car a) (car b))))
             when points
             do (let* ((z (funcall z-of j)) (color (funcall ramp j)) (first n)
                       (zb (financial-chart-3d--r3 (- z half))) (zf (financial-chart-3d--r3 (+ z half)))
                       (quad (lambda (a b c e)
                               (push (vector a b c) tris) (push (vector a c e) tris)
                               (push color tri-colors) (push color tri-colors))))
                  ;; Four vertices per level: top back, top front, floor back, floor front.
                  (dolist (p points)
                    (let ((x (financial-chart-3d--r3 (funcall x-of (car p))))
                          (y (financial-chart-3d--r3 (funcall y-of (cdr p)))))
                      (push (vector x y zb) verts) (push (vector x y zf) verts)
                      (push (vector x 0 zb) verts) (push (vector x 0 zf) verts)
                      (cl-incf n 4)))
                  (cl-loop for i from first below (- n 4) by 4
                           do (funcall quad i (+ i 4) (+ i 5) (1+ i))
                           (funcall quad (+ i 1) (+ i 5) (+ i 7) (+ i 3))
                           (funcall quad i (+ i 2) (+ i 6) (+ i 4)))
                  (cl-loop for (a b) on points while b
                           do (push (financial-chart-3d-market--seg (funcall x-of (car a)) (funcall y-of (cdr a)) zf
                                                                    (funcall x-of (car b)) (funcall y-of (cdr b)) zf)
                                    tops)
                           (push color top-colors))
                  (let ((poc (seq-reduce (lambda (best p) (if (> (cdr p) (cdr best)) p best)) points (car points))))
                    (push (financial-chart-3d-market--seg (funcall x-of (car poc)) (+ 0.08 (funcall y-of (cdr poc))) (- z half)
                                                          (funcall x-of (car poc)) (+ 0.08 (funcall y-of (cdr poc))) (+ z half))
                          pocs))
                  (when-let* ((v (seq-some (lambda (r) (and (equal (financial-chart-3d--value r zk) s)
                                                            (numberp (financial-chart-3d--value r vk))
                                                            (financial-chart-3d--value r vk)))
                                           rows)))
                    (push (list (funcall x-of (max p-lo (min p-hi (float v)))) z) vwaps))))
    (setq vwaps (nreverse vwaps))
    (vector
     (financial-chart-3d--axes "axes" (- w) w height (- d) d)
     (list :id "profiles" :type "mesh" :vertices (vconcat (nreverse verts))
           :triangles (vconcat (nreverse tris)) :colors (vconcat (nreverse tri-colors)))
     (list :id "tops" :type "segments" :segments (vconcat (nreverse tops)) :colors (vconcat (nreverse top-colors)))
     (list :id "poc" :type "segments" :color (financial-chart-3d--opt graphics "poc_color" "#ffffff")
           :segments (vconcat (nreverse pocs)))
     (list :id "vwap" :type "segments" :color (financial-chart-3d--opt graphics "vwap_color" "#ffb74d")
           :segments (vconcat (append
                               (cl-loop for (a b) on vwaps while b
                                        collect (financial-chart-3d-market--seg (car a) 0.02 (cadr a)
                                                                                (car b) 0.02 (cadr b)))
                               (mapcar (lambda (p) (financial-chart-3d-market--seg (car p) 0.02 (- (cadr p) half)
                                                                                   (car p) 0.02 (+ (cadr p) half)))
                                       vwaps)))))))

(defun financial-chart-3d-market--mix (a b u)
  "The colour U of the way from \"#rrggbb\" A to B."
  (let ((rgb (lambda (c) (mapcar (lambda (i) (string-to-number (substring c i (+ i 2)) 16)) '(1 3 5)))))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (p q) (max 0 (min 255 (round (+ p (* (- q p) u))))))
                      (funcall rgb a) (funcall rgb b)))))

;;; Registration

(defun financial-chart-3d-market--breadth-replay (graphics rows)
  "The fc-breadth3d mark replayed date by date (`financial-chart-3d--replay')."
  (financial-chart-3d--replay graphics rows "date" #'financial-chart-3d-market--breadth-mark))

(defun financial-chart-3d-market--liquidity-replay (graphics rows)
  "The fc-liquidity3d mark replayed step by step (`financial-chart-3d--replay')."
  (financial-chart-3d--replay graphics rows "step" #'financial-chart-3d-market--liquidity-mark))

(defun financial-chart-3d-market--session-profile-replay (graphics rows)
  "The fc-session-profile3d mark replayed session by session.
See `financial-chart-3d--replay'."
  (financial-chart-3d--replay graphics rows "session" #'financial-chart-3d-market--session-profile-mark))

(defconst financial-chart-3d-market-marks
  '(("fc-rrg3d" . financial-chart-3d-market--rrg-mark)
    ("fc-breadth3d" . financial-chart-3d-market--breadth-replay)
    ("fc-liquidity3d" . financial-chart-3d-market--liquidity-replay)
    ("fc-session-profile3d" . financial-chart-3d-market--session-profile-replay))
  "The market-structure 3D marks: (MARK . FUNCTION).")

(defun financial-chart-3d-market--register-marks (&rest _)
  "Register `financial-chart-3d-market-marks' if graphics.el is loaded.
Non-nil when it did; until then it waits on `after-load-functions'."
  (when (featurep 'graphics-eas-marks)
    (remove-hook 'after-load-functions #'financial-chart-3d-market--register-marks)
    (dolist (m financial-chart-3d-market-marks)
      (graphics-eas-define-mark (car m) (cdr m)))
    t))

;; Now if graphics.el is loaded, else when a 3D command loads it.
(unless (financial-chart-3d-market--register-marks)
  (add-hook 'after-load-functions #'financial-chart-3d-market--register-marks))

;;; From invest metric outputs to bindings

(declare-function financial-chart-3d-metric-entries "financial-chart-3d" (output))
(declare-function financial-chart-3d-metric-stamp "financial-chart-3d" (entry args keys &optional default))
(declare-function financial-chart-3d-metric-get "financial-chart-3d" (object &rest keys))
(declare-function financial-chart-3d-metric-list "financial-chart-3d" (v))

(defun financial-chart-3d-market--from-rel-strength (_template output args)
  "rrg bindings from ta.rel_strength OUTPUT, one entry per symbol.
Each entry {symbol, output}: value.relative_series (close over the
benchmark's, oldest first) is the symbol's rs, from which
`financial-chart-3d-rrg-rows' makes RS-Ratio and RS-Momentum.  ARGS:
:dates (one per observation, default the index), :ratio and
:momentum (default 10 and 5, shortened so a short series still
gives its last observation)."
  (let* ((series (cl-loop for e in (financial-chart-3d-metric-entries output) for i from 0
                          collect (cons (format "%s" (financial-chart-3d-metric-stamp e args '(:symbol :name)
                                                                                      (format "S%d" (1+ i))))
                                        (or (financial-chart-3d-metric-list
                                             (financial-chart-3d-metric-get (plist-get e :output) :value :relative_series))
                                            (financial-chart--invalid i "relative_series" "missing_series"
                                                                      "ta.rel_strength output needs value.relative_series")))))
         (n (length (cdar series)))
         (ratio (or (plist-get args :ratio) (if (>= n 14) 10 (max 1 (/ (1+ n) 2)))))
         (momentum (or (plist-get args :momentum) (if (>= n 14) 5 (max 1 (- n ratio -1))))))
    (cl-loop for s in series for i from 0
             unless (= (length (cdr s)) n)
             do (financial-chart--invalid i "relative_series" "length_mismatch"
                                         "every symbol's relative_series needs %d observations" n))
    (list :data (vconcat (financial-chart-3d-rrg-rows series (make-list n 1.0)
                                                      :dates (plist-get args :dates)
                                                      :ratio ratio :momentum momentum)))))

(defun financial-chart-3d-market--from-breadth (_template output args)
  "sector-breadth bindings from market.breadth_pct_above_200d OUTPUT.
Each entry {date, sector, output}: value is the percent of the
sector's members above their 200-session average; an unavailable
value (null, too little coverage) is left out."
  (list :data
        (vconcat
         (cl-loop for e in (financial-chart-3d-metric-entries output) for i from 0
                  for date = (financial-chart-3d-metric-stamp e args '(:date :as_of :time))
                  for sector = (financial-chart-3d-metric-stamp e args '(:sector))
                  for pct = (financial-chart-3d-metric-get (plist-get e :output) :value)
                  do (unless (and date sector)
                       (financial-chart--invalid i "sector" "missing_stamp" "each entry needs its date and sector"))
                  when (numberp pct)
                  collect (list :date date :sector sector :pct (financial-chart-3d-market--round pct 2))))))

(defconst financial-chart-3d-market-metric-adapters
  '(("ta.rel_strength" "rrg" financial-chart-3d-market--from-rel-strength)
    ("market.breadth_pct_above_200d" "sector-breadth" financial-chart-3d-market--from-breadth))
  "The market templates' metric adapters, (METRIC-ID TEMPLATE FUNCTION):
`financial-chart-3d-bindings-from-metric' calls them.")

;;; Live liquidity heatmaps

(defvar financial-chart-3d-market--liquidity (make-hash-table :test 'equal)
  "Live liquidity heatmaps by view id.
Each is a plist (:view :book :levels :step :history).")

(defun financial-chart-3d-liquidity-rows (book levels step)
  "BOOK's LEVELS per side as liquidity-heatmap rows, each with :step STEP.
`financial-chart-book-rows' (no flashing): asks, the mid row, bids."
  (vconcat (mapcar (lambda (r) (append r (list :step step)))
                   (financial-chart-book-rows book :levels levels :flash nil :now 0))))

(defun financial-chart-3d-market--live-spec (bindings history)
  "The liquidity-heatmap spec for BINDINGS keeping HISTORY steps, no replay.
A live view shows its newest step, so the time encoding goes."
  (let* ((spec (financial-chart-3d-spec "liquidity-heatmap" bindings))
         (x-eas (plist-get spec :x-eas))
         (graphics (copy-sequence (plist-get x-eas :graphics)))
         (encoding (copy-sequence (plist-get graphics :encoding))))
    (cl-remf encoding :time)
    (cl-remf graphics :timeline)
    (setq graphics (plist-put (plist-put graphics :encoding encoding) :max_rows history))
    (plist-put (copy-sequence spec) :x-eas (plist-put (copy-sequence x-eas) :graphics graphics))))

(cl-defun financial-chart-3d-liquidity-open (snapshot &key levels (history 60) id show)
  "Open a live 3D liquidity heatmap of order book SNAPSHOT and return its view.
Resting size by price and step, the newest HISTORY steps, as the
liquidity-heatmap variant draws it.  SNAPSHOT and LEVELS (per side) are
as in `financial-chart-book-open'.  ID names the view (default
\"fc3d/liquidity\"); SHOW draws it.  Feed it with
`financial-chart-3d-liquidity-push'."
  (require 'financial-chart-3d)
  (financial-chart-3d--ensure)
  (require 'eas-stream)
  (let* ((id (or id "fc3d/liquidity"))
         (old (gethash id financial-chart-3d-market--liquidity)))
    (when (and old (eq (gethash id eas-views) (plist-get old :view)))
      (financial-chart-3d-liquidity-close id))
    (let* ((book (if (financial-chart-book-p snapshot) snapshot (financial-chart-book-make snapshot)))
           (levels (or levels financial-chart-book-levels))
           (rows (financial-chart-3d-liquidity-rows book levels 0))
           (view (graphics-eas-open (financial-chart-3d-market--live-spec (list :data rows) history) :id id))
           (live (list :view view :book book :levels levels :step 0 :history history)))
      (eas-stream-attach view (list :max-fps (or financial-chart-book-max-fps (financial-chart-book-default-fps levels))
                                    :window (* history (1+ (* 2 levels)))))
      (puthash id live financial-chart-3d-market--liquidity)
      (when show (graphics-eas-show view))
      view)))

(defun financial-chart-3d-market--liquidity-live (view)
  "The live liquidity heatmap of VIEW (an id or view), or signal NO_BOOK."
  (let* ((id (if (eas-view-p view) (eas-view-id view) view))
         (live (gethash id financial-chart-3d-market--liquidity)))
    (unless (and live (eq (gethash id eas-views) (plist-get live :view)))
      (remhash id financial-chart-3d-market--liquidity)
      (signal 'financial-chart-invalid-book
              (list (format "View %S is not a live liquidity heatmap; open one with financial-chart-3d-liquidity-open" id)
                    :code "NO_BOOK" :path "")))
    live))

(defun financial-chart-3d-liquidity-push (view deltas &optional now)
  "Apply order-book DELTAS to liquidity heatmap VIEW at NOW; stream the step.
DELTAS are as in `financial-chart-book-push'.  Returns the book's
summary with :step."
  (let* ((live (financial-chart-3d-market--liquidity-live view))
         (book (plist-get live :book)))
    (financial-chart-book-apply book deltas now)
    (plist-put live :step (1+ (plist-get live :step)))
    (eas-push (plist-get live :view) (financial-chart-3d-liquidity-rows book (plist-get live :levels)
                                                                         (plist-get live :step)))
    (append (list :view (eas-view-id (plist-get live :view)) :step (plist-get live :step))
            (financial-chart-book-summary book))))

(defun financial-chart-3d-liquidity-close (view)
  "Stop liquidity heatmap VIEW and close it."
  (let ((live (financial-chart-3d-market--liquidity-live view)))
    (remhash (eas-view-id (plist-get live :view)) financial-chart-3d-market--liquidity)
    (financial-chart-3d--close (plist-get live :view))))

;;; Deterministic examples (the fixtures, the demo)

(defun financial-chart-3d-market--dates (start count &optional step weekdays)
  "COUNT ISO dates from START (\"YYYY-MM-DD\"), STEP days apart (default 1).
With WEEKDAYS, Saturdays and Sundays are skipped."
  (let ((time (encode-time (list 0 0 12 (string-to-number (substring start 8 10))
                                 (string-to-number (substring start 5 7))
                                 (string-to-number (substring start 0 4)) nil nil t)))
        out)
    (while (< (length out) count)
      (unless (and weekdays (memq (nth 6 (decode-time time t)) '(0 6)))
        (push (format-time-string "%F" time t) out))
      (setq time (time-add time (* 86400 (or step 1)))))
    (nreverse out)))

(defconst financial-chart-3d-market-sectors
  '(("XLK" 0.07 26 0.0 0.004) ("XLF" 0.05 22 1.2 0.002) ("XLE" 0.09 30 2.9 -0.002)
    ("XLV" 0.04 24 4.1 0.000) ("XLI" 0.05 20 0.6 0.001) ("XLY" 0.06 18 3.4 0.002)
    ("XLP" 0.03 28 5.0 -0.001) ("XLU" 0.04 32 4.6 -0.001) ("XLB" 0.06 21 2.2 0.000)
    ("XLRE" 0.05 25 3.9 -0.002) ("XLC" 0.06 23 1.7 0.003))
  "The eleven SPDR sectors: (SYMBOL AMPLITUDE PERIOD PHASE DRIFT) of the
example's relative-strength cycle (weeks, radians, per week).")

(cl-defun financial-chart-3d-market-rrg-example (&key (weeks 40) (seed 11))
  "Weekly RRG rows of the eleven sectors against a benchmark, WEEKS long.
The benchmark walks (seed SEED); each sector's price is the benchmark's
times a slow relative-strength cycle and drift
\(`financial-chart-3d-market-sectors') with noise.  Rows from
`financial-chart-3d-rrg-rows' with its defaults, so the first 13 weeks
are warm-up."
  (let* ((random (financial-chart-3d--lcg seed))
         (noise (lambda (scale) (* scale (- (funcall random) 0.5))))
         (dates (financial-chart-3d-market--dates "2026-01-02" weeks 7))
         (bench (let ((p 480.0))
                  (cl-loop repeat weeks
                           collect (prog1 (financial-chart-3d-market--round p 2)
                                     (setq p (* p (+ 1.0015 (funcall noise 0.04))))))))
         (closes (mapcar (lambda (s)
                           (pcase-let ((`(,symbol ,amp ,period ,phase ,drift) s))
                             (cons symbol
                                   (cl-loop for b in bench for i from 0
                                            collect (financial-chart-3d-market--round
                                                     (* 0.1 b (exp (+ (* amp (sin (+ phase (/ (* 2 float-pi i) period))))
                                                                      (* drift i) (funcall noise 0.012))))
                                                     2)))))
                         financial-chart-3d-market-sectors)))
    (financial-chart-3d-rrg-rows closes bench :dates dates)))

(cl-defun financial-chart-3d-market-breadth-example (&key (days 63) (seed 5))
  "Daily breadth rows {date, sector, pct} of the eleven sectors, DAYS long.
Percent of members above their 50-day average: a broad selloff a third
of the way in (defensives hold up best), then a recovery led by
technology and discretionary; noise from SEED."
  (let* ((random (financial-chart-3d--lcg seed))
         (dates (financial-chart-3d-market--dates "2026-07-01" days 1 t))
         (beta '(("XLK" 1.2 4) ("XLF" 1.0 0) ("XLE" 0.8 -6) ("XLV" 0.5 6) ("XLI" 1.0 2) ("XLY" 1.2 -2)
                 ("XLP" 0.4 8) ("XLU" 0.3 10) ("XLB" 0.9 -4) ("XLRE" 0.7 -8) ("XLC" 1.1 3))))
    (cl-loop for date in dates for i from 0
             append (let ((market (+ 14 (* 0.25 i) (* -52 (exp (- (expt (/ (- i 22) 7.0) 2)))))))
                      (cl-loop for (sector b base) in beta for s from 0
                               collect (list :date date :sector sector
                                             :pct (financial-chart-3d-market--round
                                                   (max 2.0 (min 98.0 (+ 50 base (* b market)
                                                                         (* 6 (sin (+ s (/ i 9.0))))
                                                                         (* 3 (- (funcall random) 0.5)))))
                                                   1)))))))

(defconst financial-chart-3d-market-walls
  '((4998.5 38 0 1000 1000) (5003.0 45 0 26 31) (5002.0 30 34 1000 1000) (5007.5 35 38 62 68))
  "Resting walls of the example book: (PRICE SIZE FROM FADE END).
Each rests from step FROM, is eaten from step FADE and gone at END.")

(cl-defun financial-chart-3d-market-liquidity-snapshots (&key (steps 48) (levels 16) (seed 3))
  "STEPS order-book snapshots {bids, asks, tick} around a drifting mid.
A 0.25 tick book (an index future near 5000) with LEVELS per side, its
size thickening a little away from the mid, plus the walls of
`financial-chart-3d-market-walls': a bid wall at 4998.50; an ask wall
at 5003.00 that holds the rally until it is absorbed around step 30 and
the price breaks through; a bid wall at 5002.00 that turns the old
resistance into support; an ask wall at 5007.50 that caps the next leg
until step 68.  Noise from SEED."
  (let ((random (financial-chart-3d--lcg seed)) (tick 0.25) (mid 5000.0) out)
    (dotimes (s steps)
      (let* ((target (cond ((< s 12) (+ 5000.0 (* 0.12 s)))
                           ((< s 30) (min 5002.5 (+ 5001.0 (* 0.1 (- s 12)))))
                           ((< s 66) (min 5007.0 (+ 5002.5 (* 0.14 (- s 30)))))
                           (t (+ 5007.5 (* 0.12 (- s 66))))))
             (move (* tick (round (/ (+ (* 0.5 (- target mid)) (* 0.8 (- (funcall random) 0.5))) tick)))))
        (setq mid (+ mid move))
        (let* ((best-bid (* tick (ffloor (/ (- mid (* 0.5 tick)) tick))))
               (best-ask (+ best-bid tick))
               (wall (lambda (price)
                       (cl-loop for (p size from fade end) in financial-chart-3d-market-walls
                                when (and (= price p) (>= s from))
                                sum (cond ((< s fade) size)
                                          ((< s end) (* size (/ (- end s) (float (- end fade)))))
                                          (t 0)))))
               (size (lambda (price dist)
                       (financial-chart-3d-market--round
                        (+ 2 (* 0.3 dist) (* 3 (funcall random)) (funcall wall price))
                        1))))
          (push (list :bids (vconcat (cl-loop for i below levels
                                              collect (let ((p (- best-bid (* i tick)))) (vector p (funcall size p i)))))
                      :asks (vconcat (cl-loop for i below levels
                                              collect (let ((p (+ best-ask (* i tick)))) (vector p (funcall size p i)))))
                      :tick tick)
                out))))
    (nreverse out)))

(defun financial-chart-3d-market-snapshot-deltas (from to)
  "Order-book deltas that turn snapshot FROM into snapshot TO.
Set for every level TO has whose size differs, delete for levels it
lost."
  (let (out)
    (dolist (side '((:bids . "bid") (:asks . "ask")))
      (let ((old (make-hash-table :test #'equal)) (new (make-hash-table :test #'equal)))
        (seq-doseq (l (plist-get from (car side))) (puthash (elt l 0) (elt l 1) old))
        (seq-doseq (l (plist-get to (car side))) (puthash (elt l 0) (elt l 1) new))
        (maphash (lambda (p _) (unless (gethash p new) (push (list :op "delete" :side (cdr side) :price p) out))) old)
        (maphash (lambda (p v) (unless (equal (gethash p old) v)
                                 (push (list :op "set" :side (cdr side) :price p :size v) out)))
                 new)))
    (vconcat (sort out (lambda (a b) (< (plist-get a :price) (plist-get b :price)))))))

(cl-defun financial-chart-3d-market-liquidity-example (&key (steps 48) (levels 16))
  "Liquidity-heatmap rows of `financial-chart-3d-market-liquidity-snapshots'.
Each snapshot's book as `financial-chart-3d-liquidity-rows' (LEVELS per
side), step 0 first."
  (apply #'vconcat
         (cl-loop for snapshot in (financial-chart-3d-market-liquidity-snapshots :steps steps :levels levels)
                  for step from 0
                  collect (financial-chart-3d-liquidity-rows (financial-chart-book-make snapshot) levels step))))

(cl-defun financial-chart-3d-market-session-bars (&key (sessions 8) (seed 9))
  "Half-hour OHLCV bars of SESSIONS regular sessions (13 each) near 185.
Each session gaps a little from the last close, drifts with a trend
that turns mid-week, and trades heaviest at the open and the close;
noise from SEED."
  (let ((random (financial-chart-3d--lcg seed))
        (price 185.0)
        (days (financial-chart-3d-market--dates "2026-10-19" sessions 1 t))
        out)
    (cl-loop for day in days for s from 0
             do (setq price (+ price (* 1.2 (- (funcall random) 0.5))))
             (let ((trend (if (< s 4) 0.09 -0.07))
                   (session-volume (+ 0.8 (* 0.5 (funcall random)))))
               (dotimes (i 13)
                 (let* ((open price)
                        (close (+ open trend (* 0.9 (- (funcall random) 0.5))))
                        (high (+ (max open close) (* 0.35 (funcall random))))
                        (low (- (min open close) (* 0.35 (funcall random))))
                        (u (- (/ i 6.0) 1.0))
                        (volume (* session-volume (+ 60000 (* 90000 u u) (* 30000 (funcall random))))))
                   (push (list :time (format "%sT%02d:%02d:00" day (+ 9 (/ (+ 30 (* 30 i)) 60)) (% (+ 30 (* 30 i)) 60))
                               :open (financial-chart-3d-market--round open 2)
                               :high (financial-chart-3d-market--round high 2)
                               :low (financial-chart-3d-market--round low 2)
                               :close (financial-chart-3d-market--round close 2)
                               :volume (round volume))
                         out)
                   (setq price close)))))
    (nreverse out)))

(defconst financial-chart-3d-market-fixtures-directory
  (expand-file-name "test/fixtures/3d/market" financial-chart-root)
  "Where the market-structure templates' examples live.")

(defun financial-chart-3d-market-write-fixtures (&optional directory)
  "Write the four market-structure examples into DIRECTORY.
Default `financial-chart-3d-market-fixtures-directory': rrg, sector-breadth,
liquidity-heatmap and session-profile .data.json, each the bindings its
template's example names."
  (require 'financial-chart-3d)
  (let ((directory (or directory financial-chart-3d-market-fixtures-directory)))
    (make-directory directory t)
    (financial-chart-3d--write-json
     (expand-file-name "rrg.data.json" directory)
     (list :title "Sector rotation vs SPY, weekly" :tail 8
           :data (vconcat (financial-chart-3d-market-rrg-example))))
    (financial-chart-3d--write-json
     (expand-file-name "sector-breadth.data.json" directory)
     (list :title "Sector breadth: % above 50-day average"
           :data (vconcat (financial-chart-3d-market-breadth-example))))
    (financial-chart-3d--write-json
     (expand-file-name "liquidity-heatmap.data.json" directory)
     (list :title "Resting liquidity, 48 steps"
           :data (financial-chart-3d-market-liquidity-example)))
    (financial-chart-3d--write-json
     (expand-file-name "session-profile.data.json" directory)
     (list :title "Volume profile by session"
           :data (vconcat (financial-chart-3d-session-profile (financial-chart-3d-market-session-bars) :bins 20))))))

;;;###autoload
(defun financial-chart-3d-demo-liquidity (&optional interval)
  "Stream the example book's snapshots into a live 3D liquidity heatmap.
One step every INTERVAL seconds (default 0.25), the deltas between the
snapshots of `financial-chart-3d-market-liquidity-snapshots'; killing
the buffer stops it."
  (interactive)
  (let* ((snapshots (financial-chart-3d-market-liquidity-snapshots :steps 120))
         (view (financial-chart-3d-liquidity-open (car snapshots) :levels 16 :id "fc3d/liquidity-demo" :show t))
         (buffer (get-buffer (format "*graphics-eas %s*" (eas-view-id view))))
         (previous (car snapshots))
         (pending (cdr snapshots))
         (timer nil))
    (setq timer (run-with-timer
                 0 (or interval 0.25)
                 (lambda ()
                   (if (or (null pending) (not (buffer-live-p buffer)))
                       (cancel-timer timer)
                     (let ((next (pop pending)))
                       (financial-chart-3d-liquidity-push
                        view (financial-chart-3d-market-snapshot-deltas previous next))
                       (setq previous next))))))
    view))

(provide 'financial-chart-3d-market)
;;; financial-chart-3d-market.el ends here
