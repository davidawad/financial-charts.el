;;; financial-chart-3d-risk.el --- 3D portfolio-risk charts via graphics.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Portfolio-risk templates, each a 2D template in templates/financial/
;; (what eas draws without graphics.el) and its 3D variant in
;; templates3d/ (loaded by financial-chart-3d.el, which loads this
;; file):
;;
;;   risk-correlation          rolling correlation matrix, signed bars,
;;                             animated window by window
;;   risk-contribution         risk contribution ribbons over time
;;   risk-factor-exposure      factor beta ribbons around a zero plane
;;   risk-stress               scenario by position P/L grid with a net
;;                             column
;;   risk-underwater           drawdown slabs across strategies
;;   risk-return-distribution  return histograms across horizons, VaR
;;                             ruled
;;   risk-frontier             efficient frontiers as a surface over
;;                             volatility and date, coloured by Sharpe
;;
;; Each takes the rows docs/3d-risk.md specifies, which the
;; invest-metrics metrics (stats.correlation, stats.risk_contribution,
;; stats.factor_regression, port.stress, stats.max_drawdown,
;; stats.var_cvar.*, port.frontier) lower to; this package never fetches
;; them.  `financial-chart-3d-risk-matrix-rows' lowers correlation
;; matrices.  `financial-chart-3d-risk-open' draws a template in 3D, or
;; in 2D when graphics.el is absent.
;;
;; The marks are registered with `graphics-eas-define-mark' when
;; graphics.el's marks load (now, if they already have):
;;
;;   fc-risk-grid3d       signed bars on a category by category floor,
;;                        coloured by sign and magnitude
;;   fc-risk-ribbons3d    one signed slab per series from a zero plane
;;                        along x (`transform' "underwater": the percent
;;                        below each series' running high)
;;   fc-risk-histogram3d  a histogram per lane on shared bins, VaR ruled
;;   fc-risk-frontier3d   curves of y over x per z, joined into a surface
;;                        coloured by a fourth field, its maxima marked
;;
;; This file needs nothing of financial-chart-3d.el at load (that file
;; loads it), so it keeps its own small helpers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function graphics-eas-show "graphics-eas" (view &optional cols rows window))
(declare-function financial-chart-3d-available-p "financial-chart-3d" ())
(declare-function financial-chart-3d-open "financial-chart-3d" (name &rest args))
(declare-function eas-show "eas-mode" (view &optional target))

(defvar graphics-eas-domain-rows)

(defconst financial-chart-3d-risk-templates
  '("risk-correlation" "risk-contribution" "risk-factor-exposure" "risk-stress"
    "risk-underwater" "risk-return-distribution" "risk-frontier")
  "The portfolio-risk templates (templates/financial/ and templates3d/).")

;;; Helpers

(defun financial-chart-3d-risk--key (name)
  "Plist keyword for NAME (a string or keyword)."
  (if (keywordp name) name (intern (concat ":" name))))

(defun financial-chart-3d-risk--opt (graphics name &optional default)
  "Option NAME (a string) of GRAPHICS, DEFAULT when absent or null."
  (let ((v (plist-get graphics (financial-chart-3d-risk--key name))))
    (if (or (null v) (eq v :null)) default v)))

(defun financial-chart-3d-risk--on (graphics name default)
  "Boolean option NAME of GRAPHICS (JSON false is false), DEFAULT when absent."
  (let ((v (financial-chart-3d-risk--opt graphics name :absent)))
    (cond ((eq v :absent) default)
          ((memq v '(nil :false)) nil)
          (t t))))

(defun financial-chart-3d-risk--field (graphics channel default)
  "The field keyword of CHANNEL in GRAPHICS's encoding, else DEFAULT's."
  (let* ((spec (plist-get (financial-chart-3d-risk--opt graphics "encoding")
                          (financial-chart-3d-risk--key channel)))
         (name (cond ((stringp spec) spec)
                     ((consp spec) (plist-get spec :field))
                     (t default))))
    (financial-chart-3d-risk--key name)))

(defun financial-chart-3d-risk--value (row key)
  "ROW's value at KEY; nil for null, false or missing."
  (let ((v (plist-get row key)))
    (unless (memq v '(:null :false)) v)))

(defun financial-chart-3d-risk--number (row key)
  "ROW's value at KEY as a float, or nil when it is not a number."
  (let ((v (financial-chart-3d-risk--value row key)))
    (and (numberp v) (float v))))

(defun financial-chart-3d-risk--categories (values)
  "Distinct VALUES: sorted when all are numbers, else in first-seen order."
  (let ((seen (delete-dups (copy-sequence (delq nil values)))))
    (if (seq-every-p #'numberp seen) (sort seen #'<) seen)))

(defun financial-chart-3d-risk--r3 (v)
  "V rounded to 3 decimals, as graphics.el's marks round."
  (/ (fround (* v 1000.0)) 1000.0))

(defun financial-chart-3d-risk--rows (rows)
  "ROWS as a list: a vector or list of plists, or a data/v1 object."
  (if (keywordp (car-safe rows)) (append (plist-get rows :rows) nil) (append rows nil)))

(defun financial-chart-3d-risk--domain (rows)
  "ROWS and the rows a time encoding's scales span (`graphics-eas-domain-rows')."
  (if (and (boundp 'graphics-eas-domain-rows) graphics-eas-domain-rows)
      (append graphics-eas-domain-rows rows)
    rows))

(defun financial-chart-3d-risk--index (values)
  "A hash table of VALUES to their positions."
  (let ((index (make-hash-table :test #'equal)))
    (cl-loop for v in values for i from 0 do (puthash v i index))
    index))

(defun financial-chart-3d-risk--spread (i n half)
  "Position I of N spread evenly over -HALF..HALF (0 for a single one)."
  (if (> n 1) (- (* 2 half (/ i (float (1- n)))) half) 0.0))

(defun financial-chart-3d-risk--rgb (color)
  "COLOR \"#rrggbb\" as a list of three numbers."
  (mapcar (lambda (i) (string-to-number (substring color i (+ i 2)) 16)) '(1 3 5)))

(defun financial-chart-3d-risk--blend (from to u)
  "The colour U (0 to 1) of the way from colour FROM to TO."
  (let ((u (max 0.0 (min 1.0 u))))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (a b) (round (+ a (* u (- b a)))))
                      (financial-chart-3d-risk--rgb from) (financial-chart-3d-risk--rgb to)))))

(defun financial-chart-3d-risk--ramp (stops u)
  "The colour at U in [0, 1] of the evenly spaced colour STOPS."
  (let* ((n (1- (length stops)))
         (p (* (max 0.0 (min 1.0 u)) n))
         (i (min (1- n) (floor p))))
    (financial-chart-3d-risk--blend (nth i stops) (nth (1+ i) stops) (- p i))))

(defconst financial-chart-3d-risk-up-color "#26a69a" "Positive values: gains, long exposure.")
(defconst financial-chart-3d-risk-down-color "#ef5350" "Negative values: losses, short exposure.")
(defconst financial-chart-3d-risk-muted-color "#3b4a63" "A value near zero; the diagonal.")
(defconst financial-chart-3d-risk-axis-color "#d2ddeb" "Axes, as graphics.el draws them.")
(defconst financial-chart-3d-risk-grid-color "#344769" "Floor grids, as graphics.el draws them.")
(defconst financial-chart-3d-risk-mark-color "#ffb74d" "Markers: VaR, the worst drawdown, the best Sharpe.")

(defconst financial-chart-3d-risk-series-colors
  ["#4c78a8" "#f58518" "#e45756" "#72b7b2" "#54a24b"
   "#eeca3b" "#b279a2" "#ff9da6" "#9d755d" "#bab0ac"]
  "Vega's tableau10, the colours eas gives the same series in 2D.")

(defun financial-chart-3d-risk--signed-color (graphics v top)
  "The colour of V on GRAPHICS's diverging scale, full at |V| = TOP."
  (financial-chart-3d-risk--blend
   (financial-chart-3d-risk--opt graphics "mid_color" financial-chart-3d-risk-muted-color)
   (if (< v 0)
       (financial-chart-3d-risk--opt graphics "down_color" financial-chart-3d-risk-down-color)
     (financial-chart-3d-risk--opt graphics "up_color" financial-chart-3d-risk-up-color))
   (if (> top 0) (+ 0.25 (* 0.75 (/ (abs v) top))) 1.0)))

(defun financial-chart-3d-risk--axes (x0 x1 y0 y1 z0 z1)
  "Axis segments along the back left corner: X0..X1 and Z0..Z1 at y 0, Y0..Y1 up."
  (list :id "axes" :type "segments" :color financial-chart-3d-risk-axis-color
        :segments (vector (vector x0 0 z0 x1 0 z0) (vector x0 0 z0 x0 0 z1)
                          (vector x0 (financial-chart-3d-risk--r3 y0) z0
                                  x0 (financial-chart-3d-risk--r3 y1) z0))))

(defun financial-chart-3d-risk--rectangle (x0 x1 y z0 z1)
  "The four segments framing X0..X1 by Z0..Z1 at height Y."
  (vector (vector x0 y z0 x1 y z0) (vector x1 y z0 x1 y z1)
          (vector x1 y z1 x0 y z1) (vector x0 y z1 x0 y z0)))

;;; Correlation matrices as rows

(defun financial-chart-3d-risk--matrix-labels (object)
  "The labels of correlation OBJECT (:labels, :assets, :names or :symbols)."
  (append (or (plist-get object :labels) (plist-get object :assets)
              (plist-get object :names) (plist-get object :symbols))
          nil))

(defun financial-chart-3d-risk--matrix-values (object)
  "The square matrix of correlation OBJECT (:matrix, :correlation or :values)."
  (or (plist-get object :matrix) (plist-get object :correlation) (plist-get object :values)))

(defun financial-chart-3d-risk-matrix-rows (object &optional window)
  "Rows {window, row, column, value} of correlation OBJECT.
OBJECT is one matrix, a plist (:labels [...] :matrix [[...] ...])
\(:assets, :names or :symbols for the labels; :correlation or
:values for the matrix), as stats.correlation emits it; or a series
of them, (:windows [M ...]) or a vector of M, each M such a matrix
with its :window (or :date, :end, :asof).  WINDOW stamps a single
matrix (default 0).  Rows run window by window, row by row, so they
replay as the risk-correlation template's animation.  Signals
`financial-chart-invalid-data' when a matrix is not square over its
labels."
  (let ((series (cond ((plist-get object :windows) (append (plist-get object :windows) nil))
                      ((and (sequencep object) (not (keywordp (car-safe object)))) (append object nil)))))
    (if series
        (cl-loop for m in series for i from 0
                 append (financial-chart-3d-risk-matrix-rows
                         m (or (plist-get m :window) (plist-get m :date) (plist-get m :end)
                               (plist-get m :asof) i)))
      (let* ((labels (financial-chart-3d-risk--matrix-labels object))
             (matrix (append (financial-chart-3d-risk--matrix-values object) nil))
             (n (length labels)))
        (unless (and (> n 0) (= (length matrix) n)
                     (seq-every-p (lambda (r) (= (length r) n)) matrix))
          (signal 'financial-chart-invalid-data
                  (list (format "correlation matrix must be %d x %d over its labels" n n)
                        :code "not_square" :index nil :field "matrix")))
        (cl-loop for row-label in labels for row in matrix
                 append (cl-loop for column-label in labels for v across (vconcat row)
                                 collect (list :window (or window 0) :row row-label
                                               :column column-label :value v)))))))

;;; fc-risk-grid3d: signed bars on a category grid

(defun financial-chart-3d-risk--grid-mark (graphics rows)
  "Primitives of the fc-risk-grid3d mark: GRAPHICS over ROWS.
Channels x and z (categories: the floor's columns and rows) and y (a
signed number: a bar up from the floor or down through it, coloured
from muted to green or red by magnitude).  `domain' [LO HI] fixes the
height scale (|y| up to the larger of |LO| and |HI| fills `height');
else it spans every row's |y|, the replay's too.  `diagonal' false
draws the cells whose x equals z as flat muted tiles (a correlation
matrix's ones).  `net' NAME adds a column NAME per z, the sum of its
row.  Every cell keeps its place and its bar in each frame (an empty
cell has a flat one), so a frame-by-frame replay tweens bar by bar."
  (let* ((xk (financial-chart-3d-risk--field graphics "x" "column"))
         (zk (financial-chart-3d-risk--field graphics "z" "row"))
         (yk (financial-chart-3d-risk--field graphics "y" "value"))
         (net (financial-chart-3d-risk--opt graphics "net"))
         (diagonal (financial-chart-3d-risk--on graphics "diagonal" t))
         (dom (financial-chart-3d-risk--domain rows))
         (xs (financial-chart-3d-risk--categories
              (mapcar (lambda (r) (financial-chart-3d-risk--value r xk)) dom)))
         (zs (financial-chart-3d-risk--categories
              (mapcar (lambda (r) (financial-chart-3d-risk--value r zk)) dom)))
         (cells (lambda (rows)
                  (let ((h (make-hash-table :test #'equal)))
                    (dolist (r rows h)
                      (let ((x (financial-chart-3d-risk--value r xk)) (z (financial-chart-3d-risk--value r zk))
                            (y (financial-chart-3d-risk--number r yk)))
                        (when (and x z y)
                          (puthash (cons x z) y h)
                          (when net (puthash (cons net z) (+ y (gethash (cons net z) h 0.0)) h))))))))
         (shown (funcall cells rows))
         (all (if (eq dom rows) shown (funcall cells dom)))
         (cols (if net (append xs (list net)) xs))
         (domain (financial-chart-3d-risk--opt graphics "domain"))
         (top (if domain (max (abs (elt domain 0)) (abs (elt domain 1)))
                (let ((m 0.0)) (maphash (lambda (_ v) (setq m (max m (abs v)))) all) m)))
         (top (if (> top 0) top 1.0))
         (nx (length cols)) (nz (length zs))
         (height (float (financial-chart-3d-risk--opt graphics "height" (* 0.45 (max nx nz 4)))))
         (scale (/ height top))
         (half (/ (financial-chart-3d-risk--opt graphics "band" 0.7) 2.0))
         (x0 (/ (1- nx) 2.0)) (z0 (/ (1- nz) 2.0))
         (left (- -0.5 x0)) (right (+ 0.5 x0)) (back (- -0.5 z0)) (front (+ 0.5 z0))
         bars colors tiles grid)
    (cl-loop for z in zs for j from 0 do
             (cl-loop for x in cols for i from 0 do
                      (let ((cx (- i x0)) (cz (- j z0)))
                        (if (and (not diagonal) (equal x z))
                            (push (vector (financial-chart-3d-risk--r3 (- cx half)) 0 (financial-chart-3d-risk--r3 (- cz half))
                                          (financial-chart-3d-risk--r3 (+ cx half)) (financial-chart-3d-risk--r3 (* 0.02 height))
                                          (financial-chart-3d-risk--r3 (+ cz half)))
                                  tiles)
                          (let* ((v (gethash (cons x z) shown 0.0)) (y (* scale v))
                                 (w (if (equal x net) (* 0.6 half) half)))
                            (push (vector (financial-chart-3d-risk--r3 (- cx w)) (financial-chart-3d-risk--r3 (min 0 y))
                                          (financial-chart-3d-risk--r3 (- cz w))
                                          (financial-chart-3d-risk--r3 (+ cx w)) (financial-chart-3d-risk--r3 (max 0 y))
                                          (financial-chart-3d-risk--r3 (+ cz w)))
                                  bars)
                            (push (financial-chart-3d-risk--signed-color graphics v top) colors))))))
    (dotimes (i (1+ nx))
      (let ((x (financial-chart-3d-risk--r3 (- i x0 0.5))))
        (push (vector x 0 back x 0 front) grid)))
    (dotimes (j (1+ nz))
      (let ((z (financial-chart-3d-risk--r3 (- j z0 0.5))))
        (push (vector left 0 z right 0 z) grid)))
    (when net
      ;; The net column stands apart: a rule between it and the positions.
      (let ((x (financial-chart-3d-risk--r3 (- nx 1.5 x0))))
        (push (vector x 0.01 back x 0.01 front) grid)))
    (vector (list :id "grid" :type "segments" :segments (vconcat (nreverse grid))
                  :color financial-chart-3d-risk-grid-color)
            (financial-chart-3d-risk--axes left right (- (if (seq-some (lambda (v) (< v 0)) (hash-table-values all)) height 0))
                                           height back front)
            (list :id "diagonal" :type "boxes" :boxes (vconcat (nreverse tiles))
                  :color financial-chart-3d-risk-muted-color)
            (list :id "bars" :type "boxes" :boxes (vconcat (nreverse bars))
                  :colors (vconcat (nreverse colors))))))

;;; fc-risk-ribbons3d: signed slabs per series

(defun financial-chart-3d-risk-underwater-rows (rows &optional xk sk yk)
  "ROWS with :drawdown, the percent below each series' running high.
XK, SK and YK name the x, series and value fields (default :date,
:strategy, :equity).  Rows keep their order; each series' high runs
over its rows in that order, oldest first.  Drawdown is 0 at a new high
and negative below it.  Rows without a number value are dropped."
  (let ((xk (or xk :date)) (sk (or sk :strategy)) (yk (or yk :equity))
        (peaks (make-hash-table :test #'equal)))
    (delq nil (mapcar (lambda (r)
                        (let ((s (financial-chart-3d-risk--value r sk))
                              (y (financial-chart-3d-risk--number r yk)))
                          (when (and s y)
                            (let ((peak (max y (gethash s peaks y))))
                              (puthash s peak peaks)
                              (list xk (financial-chart-3d-risk--value r xk) sk s yk y
                                    :drawdown (if (> peak 0)
                                                  (financial-chart-3d-risk--r3 (* 100.0 (- (/ y peak) 1)))
                                                0.0))))))
                      rows))))

(defun financial-chart-3d-risk--ribbons-mark (graphics rows)
  "Primitives of the fc-risk-ribbons3d mark: GRAPHICS over ROWS.
One slab per series (channel z, in first-seen order, the series
colours eas gives them in 2D) along x (its values evenly spaced, in
order), from a zero plane up to y, or down to it when y is negative,
its top edge drawn bright.  Scales span every row, a replay's too.
`transform' \"underwater\" draws each series' percent below its
running high instead of y (`financial-chart-3d-risk-underwater-rows'),
the deepest point of each marked.  `height' (default 4) is the larger
of the tallest and the deepest slab, `extent' [x, 0] the x span
\(default 12), `spacing' the distance between series (default 2.2)
and `band' the share of it a slab is deep (default 0.35).  `stack' S
lifts each series' zero plane S times `height' above the next one's
toward the front, a ridgeline whose slabs never hide each other."
  (let* ((xk (financial-chart-3d-risk--field graphics "x" "date"))
         (zk (financial-chart-3d-risk--field graphics "z" "series"))
         (yk (financial-chart-3d-risk--field graphics "y" "value"))
         (underwater (equal (financial-chart-3d-risk--opt graphics "transform") "underwater"))
         (prepare (lambda (rows)
                    (if underwater
                        (financial-chart-3d-risk-underwater-rows rows xk zk yk)
                      rows)))
         (yk (if underwater :drawdown yk))
         (rows (funcall prepare rows))
         (dom (if (and (boundp 'graphics-eas-domain-rows) graphics-eas-domain-rows)
                  (append (funcall prepare graphics-eas-domain-rows) rows)
                rows))
         (xs (financial-chart-3d-risk--categories (mapcar (lambda (r) (financial-chart-3d-risk--value r xk)) dom)))
         (keys (financial-chart-3d-risk--categories (mapcar (lambda (r) (financial-chart-3d-risk--value r zk)) dom)))
         (ys (delq nil (mapcar (lambda (r) (financial-chart-3d-risk--number r yk)) dom)))
         (lo (apply #'min 0.0 ys)) (hi (apply #'max 0.0 ys))
         (height (float (financial-chart-3d-risk--opt graphics "height" 4)))
         (scale (if (> (max hi (- lo)) 0) (/ height (max hi (- lo))) 1.0))
         (w (/ (float (elt (financial-chart-3d-risk--opt graphics "extent" [12 0]) 0)) 2))
         (gap (float (financial-chart-3d-risk--opt graphics "spacing" 2.2)))
         (half (/ (* gap (financial-chart-3d-risk--opt graphics "band" 0.35)) 2))
         (stack (* height (financial-chart-3d-risk--opt graphics "stack" 0)))
         (nz (length keys)) (d (/ (* gap nz) 2))
         (x-index (financial-chart-3d-risk--index xs)) (nx (length xs))
         (x-at (lambda (x) (financial-chart-3d-risk--r3 (financial-chart-3d-risk--spread (gethash x x-index 0) nx w))))
         (y-at (lambda (v) (financial-chart-3d-risk--r3 (* scale v))))
         (verts nil) (tris nil) (tri-colors nil) (n 0) (lines nil) (line-colors nil) (marks nil) (zeros nil))
    (cl-loop for key in keys for k from 0 do
             (let* ((z (- (* gap (+ 0.5 k)) d))
                    (base (financial-chart-3d-risk--r3 (* stack (- nz 1 k))))
                    (zb (financial-chart-3d-risk--r3 (- z half))) (zf (financial-chart-3d-risk--r3 (+ z half)))
                    (color (or (financial-chart-3d-risk--opt graphics "color")
                               (aref financial-chart-3d-risk-series-colors
                                     (mod k (length financial-chart-3d-risk-series-colors)))))
                    (points (delq nil (mapcar (lambda (r)
                                                (when (equal (financial-chart-3d-risk--value r zk) key)
                                                  (let ((x (financial-chart-3d-risk--value r xk))
                                                        (y (financial-chart-3d-risk--number r yk)))
                                                    (and x y (cons (funcall x-at x)
                                                                   (financial-chart-3d-risk--r3 (+ base (funcall y-at y))))))))
                                              rows)))
                    (first n)
                    (quad (lambda (a b c e)
                            (push (vector a b c) tris) (push (vector a c e) tris)
                            (push color tri-colors) (push color tri-colors))))
               ;; Four vertices per point: top back, top front, zero back, zero front.
               (dolist (p points)
                 (push (vector (car p) (cdr p) zb) verts) (push (vector (car p) (cdr p) zf) verts)
                 (push (vector (car p) base zb) verts) (push (vector (car p) base zf) verts)
                 (cl-incf n 4))
               (cl-loop for i from first below (- n 4) by 4 do
                        (funcall quad i (+ i 4) (+ i 5) (1+ i))
                        (funcall quad (1+ i) (+ i 5) (+ i 7) (+ i 3))
                        (funcall quad i (+ i 2) (+ i 6) (+ i 4)))
               (when points
                 (funcall quad first (1+ first) (+ first 3) (+ first 2))
                 (funcall quad (- n 4) (- n 2) (- n 1) (- n 3)))
               (cl-loop for (a b) on points while b do
                        (push (vector (car a) (cdr a) zf (car b) (cdr b) zf) lines)
                        (push (financial-chart-3d-risk--blend color "#ffffff" 0.55) line-colors))
               (when (> stack 0)
                 (push (vector (- w) base zb w base zb) zeros)
                 (push (vector (- w) base zf w base zf) zeros))
               (when (and underwater points)
                 (let ((worst (seq-reduce (lambda (m p) (if (< (cdr p) (cdr m)) p m)) points (car points))))
                   (when (< (cdr worst) base)
                     (push (vector (car worst) base zf (car worst) (cdr worst) zf) marks)
                     (push (vector (car worst) (cdr worst) zb (car worst) (cdr worst) zf) marks))))))
    (let ((y-lo (* scale lo)) (y-hi (+ (* scale hi) (* stack (max 0 (1- nz))))) (z0 (- d)) (z1 d))
      (vector (financial-chart-3d-risk--axes (- w) w y-lo (max y-hi (if (< lo 0) 0 height)) z0 z1)
              (list :id "zero" :type "segments" :color financial-chart-3d-risk-grid-color
                    :segments (cond ((> stack 0) (vconcat (nreverse zeros)))
                                    ((< lo 0) (financial-chart-3d-risk--rectangle (- w) w 0 z0 z1))
                                    (t [])))
              (list :id "slabs" :type "mesh" :vertices (vconcat (nreverse verts))
                    :triangles (vconcat (nreverse tris)) :colors (vconcat (nreverse tri-colors)))
              (list :id "edges" :type "segments" :segments (vconcat (nreverse lines))
                    :colors (vconcat (nreverse line-colors)))
              (list :id "worst" :type "segments" :segments (vconcat (nreverse marks))
                    :color financial-chart-3d-risk-mark-color)))))

;;; fc-risk-histogram3d: histograms across lanes

(defun financial-chart-3d-risk-quantile (values p)
  "The P quantile (0 to 1) of VALUES, linear between order statistics."
  (let* ((v (vconcat (sort (mapcar #'float values) #'<)))
         (n (length v)))
    (cond ((= n 0) nil)
          ((= n 1) (aref v 0))
          (t (let* ((h (* p (1- n))) (i (min (- n 2) (floor h))))
               (+ (aref v i) (* (- h i) (- (aref v (1+ i)) (aref v i)))))))))

(cl-defun financial-chart-3d-risk-histograms (rows &key (xk :return) (zk :horizon) (bins 21) (level 0.95) domain)
  "Histograms of ROWS' XK values per ZK lane on shared bins.
BINS equal bins span the XK values of DOMAIN (default ROWS).  A plist:
:low, :high and :step of the bins, :lanes (the ZK values in first-seen
order of DOMAIN) and :histograms, one plist per lane present in ROWS
\(:lane, :count, :shares, a vector of each bin's share of the lane's
values, :var, the historical value-at-risk at LEVEL: minus the lane's
1 - LEVEL quantile, so a positive loss, and :cvar, minus the mean of
the values at or below that quantile)."
  (let* ((domain (or domain rows))
         (xs (delq nil (mapcar (lambda (r) (financial-chart-3d-risk--number r xk)) domain)))
         (low (if xs (apply #'min xs) 0.0))
         (high (if xs (apply #'max xs) 1.0))
         (high (if (> high low) high (1+ low)))
         (step (/ (- high low) bins))
         (lanes (financial-chart-3d-risk--categories (mapcar (lambda (r) (financial-chart-3d-risk--value r zk)) domain))))
    (list :low low :high high :step step :lanes lanes
          :histograms
          (delq nil
                (mapcar (lambda (lane)
                          (let ((values (delq nil (mapcar (lambda (r) (and (equal (financial-chart-3d-risk--value r zk) lane)
                                                                          (financial-chart-3d-risk--number r xk)))
                                                          rows))))
                            (when values
                              (let ((counts (make-vector bins 0))
                                    (q (financial-chart-3d-risk-quantile values (- 1 level))))
                                (dolist (v values)
                                  (let ((i (max 0 (min (1- bins) (floor (/ (- v low) step))))))
                                    (aset counts i (1+ (aref counts i)))))
                                (list :lane lane :count (length values)
                                      :shares (vconcat (mapcar (lambda (c) (/ c (float (length values)))) counts))
                                      :var (- q)
                                      :cvar (let ((tail (seq-filter (lambda (v) (<= v q)) values)))
                                              (- (/ (apply #'+ tail) (float (length tail))))))))))
                        lanes)))))

(defun financial-chart-3d-risk--histogram-mark (graphics rows)
  "Primitives of the fc-risk-histogram3d mark: GRAPHICS over ROWS.
Channel x (a number: returns) binned into `bins' (default 21) shared
bins over every row, one lane per z value (horizons, first-seen order)
from front to back, each bar its bin's share of the lane's rows (so
lanes of any size compare), bins below zero red and above green.  Each
lane's tallest bar is `height' (its shape compares across horizons; a
long horizon's spread is not flattened); `scale' \"shared\" puts every
lane on the tallest one's scale instead.  Each
lane's value-at-risk at `level' (default 0.95) is ruled across it, a
gate standing at its quantile; a rule on the floor marks zero.  The
height scale spans every lane, a replay's too."
  (let* ((xk (financial-chart-3d-risk--field graphics "x" "return"))
         (zk (financial-chart-3d-risk--field graphics "z" "horizon"))
         (bins (max 2 (financial-chart-3d-risk--opt graphics "bins" 21)))
         (level (float (financial-chart-3d-risk--opt graphics "level" 0.95)))
         (dom (financial-chart-3d-risk--domain rows))
         (hist (financial-chart-3d-risk-histograms rows :xk xk :zk zk :bins bins :level level :domain dom))
         (peaks (mapcar (lambda (h) (cons (plist-get h :lane) (seq-reduce #'max (plist-get h :shares) 0.0)))
                        (plist-get (financial-chart-3d-risk-histograms dom :xk xk :zk zk :bins bins :level level)
                                   :histograms)))
         (shared (equal (financial-chart-3d-risk--opt graphics "scale") "shared"))
         (tallest (apply #'max 0.0 (mapcar #'cdr peaks)))
         (lanes (plist-get hist :lanes)) (nz (max 1 (length lanes)))
         (height (float (financial-chart-3d-risk--opt graphics "height" 4)))
         (w (/ (float (elt (financial-chart-3d-risk--opt graphics "extent" [12 0]) 0)) 2))
         (gap (float (financial-chart-3d-risk--opt graphics "spacing" 1.6)))
         (half (/ (* gap (financial-chart-3d-risk--opt graphics "band" 0.6)) 2))
         (d (/ (* gap nz) 2))
         (low (plist-get hist :low)) (step (plist-get hist :step))
         (x-at (lambda (v) (financial-chart-3d-risk--r3 (- (* 2 w (/ (- v low) (* step bins))) w))))
         (bar-half (* 0.42 (/ (* 2 w) bins)))
         boxes colors gates)
    (dolist (h (plist-get hist :histograms))
      (let* ((k (cl-position (plist-get h :lane) lanes :test #'equal))
             ;; The first lane in front, the longest horizon at the back.
             (z (- d (* gap (+ 0.5 k))))
             (zb (financial-chart-3d-risk--r3 (- z half))) (zf (financial-chart-3d-risk--r3 (+ z half)))
             (top (let ((top (if shared tallest (or (cdr (assoc (plist-get h :lane) peaks)) tallest))))
                    (if (> top 0) top 1.0)))
             (lane-top 0.0))
        (cl-loop for share across (plist-get h :shares) for i from 0
                 when (> share 0) do
                 (let* ((center (+ low (* step (+ i 0.5))))
                        (cx (funcall x-at center))
                        (y (financial-chart-3d-risk--r3 (* height (/ share top)))))
                   (setq lane-top (max lane-top y))
                   (push (vector (financial-chart-3d-risk--r3 (- cx bar-half)) 0 zb
                                 (financial-chart-3d-risk--r3 (+ cx bar-half)) y zf)
                         boxes)
                   (push (if (< center 0)
                             (financial-chart-3d-risk--opt graphics "down_color" financial-chart-3d-risk-down-color)
                           (financial-chart-3d-risk--opt graphics "up_color" financial-chart-3d-risk-up-color))
                         colors)))
        (let ((x (funcall x-at (- (plist-get h :var))))
              (y (financial-chart-3d-risk--r3 (+ (* 0.15 height) lane-top))))
          (push (vector x 0 zb x y zb) gates)
          (push (vector x 0 zf x y zf) gates)
          (push (vector x y zb x y zf) gates))))
    (let ((x0 (funcall x-at 0.0)))
      (vector (financial-chart-3d-risk--axes (- w) w 0 height (- d) d)
              (list :id "zero" :type "segments" :color financial-chart-3d-risk-axis-color
                    :segments (if (<= low 0 (plist-get hist :high))
                                  (vector (vector x0 0 (- d) x0 0 d))
                                []))
              (list :id "bars" :type "boxes" :boxes (vconcat (nreverse boxes)) :colors (vconcat (nreverse colors)))
              (list :id "var" :type "segments" :segments (vconcat (nreverse gates))
                    :color financial-chart-3d-risk-mark-color)))))

;;; fc-risk-frontier3d: frontiers as a surface coloured by Sharpe

(defun financial-chart-3d-risk--frontier-mark (graphics rows)
  "Primitives of the fc-risk-frontier3d mark: GRAPHICS over ROWS.
Channels x (volatility, across), y (return, up), z (date: one curve
per value, first-seen order, front to back) and color (Sharpe).  Each
curve's points run in y order; consecutive curves with as many points
are joined into a surface, each cell coloured by its corners' mean
color value on a blue to amber ramp over every row's.  `fill' (default
0.55) is the share of the way to the next curve the surface covers:
below 1 it is a ribbon per curve with a gap before the next.  The newest
curve is drawn white, every curve's point of greatest color value
marked and joined across the curves.  Scales span every row, a
replay's too."
  (let* ((xk (financial-chart-3d-risk--field graphics "x" "vol"))
         (yk (financial-chart-3d-risk--field graphics "y" "ret"))
         (zk (financial-chart-3d-risk--field graphics "z" "date"))
         (ck (financial-chart-3d-risk--field graphics "color" "sharpe"))
         (dom (financial-chart-3d-risk--domain rows))
         (num (lambda (k) (delq nil (mapcar (lambda (r) (financial-chart-3d-risk--number r k)) dom))))
         (xs (funcall num xk)) (ys (funcall num yk)) (cs (funcall num ck))
         (x-lo (if xs (apply #'min xs) 0.0)) (x-hi (if xs (apply #'max xs) 1.0))
         (y-lo (if ys (apply #'min ys) 0.0)) (y-hi (if ys (apply #'max ys) 1.0))
         (c-lo (if cs (apply #'min cs) 0.0)) (c-hi (if cs (apply #'max cs) 1.0))
         (dates (financial-chart-3d-risk--categories (mapcar (lambda (r) (financial-chart-3d-risk--value r zk)) dom)))
         (nz (length dates))
         (height (float (financial-chart-3d-risk--opt graphics "height" 5)))
         (extent (financial-chart-3d-risk--opt graphics "extent" [12 8]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (stops (list (financial-chart-3d-risk--opt graphics "low_color" "#2c3e7a")
                      (financial-chart-3d-risk--opt graphics "mid_color" "#4c9eb0")
                      (financial-chart-3d-risk--opt graphics "high_color" "#ffb74d")))
         (color-at (lambda (c) (financial-chart-3d-risk--ramp stops (if (> c-hi c-lo) (/ (- c c-lo) (- c-hi c-lo)) 1.0))))
         (point (lambda (r z)
                  (vector (financial-chart-3d-risk--r3 (- (* 2 w (/ (- (financial-chart-3d-risk--number r xk) x-lo)
                                                                    (if (> x-hi x-lo) (- x-hi x-lo) 1.0)))
                                                         w))
                          (financial-chart-3d-risk--r3 (+ (* 0.05 height)
                                                          (* 0.95 height (/ (- (financial-chart-3d-risk--number r yk) y-lo)
                                                                            (if (> y-hi y-lo) (- y-hi y-lo) 1.0)))))
                          z)))
         (curves (delq nil
                       (cl-loop for date in dates for j from 0
                                collect (let ((pts (seq-filter (lambda (r) (and (equal (financial-chart-3d-risk--value r zk) date)
                                                                                (financial-chart-3d-risk--number r xk)
                                                                                (financial-chart-3d-risk--number r yk)))
                                                               rows)))
                                          (when pts
                                            ;; The first date in front.
                                            (let ((z (financial-chart-3d-risk--r3 (- (financial-chart-3d-risk--spread j nz d)))))
                                              (mapcar (lambda (r) (cons (funcall point r z)
                                                                        (or (financial-chart-3d-risk--number r ck) c-lo)))
                                                      (sort pts (lambda (a b) (< (financial-chart-3d-risk--number a yk)
                                                                                 (financial-chart-3d-risk--number b yk)))))))))))
         (fill (float (financial-chart-3d-risk--opt graphics "fill" 0.55)))
         (verts nil) (tris nil) (tri-colors nil) (n 0) (lines nil) (line-colors nil) (best nil))
    (cl-loop for (a b) on curves while b
             when (= (length a) (length b)) do
             (let ((base n))
               (dolist (p a) (push (car p) verts) (cl-incf n))
               (cl-loop for p in a for q in b
                        do (push (vconcat (cl-mapcar (lambda (u v) (financial-chart-3d-risk--r3 (+ u (* fill (- v u)))))
                                                     (car p) (car q)))
                                 verts)
                        (cl-incf n))
               (let ((m (length a)))
                 (dotimes (i (1- m))
                   (let* ((p0 (+ base i)) (p1 (1+ p0)) (q0 (+ base m i)) (q1 (1+ q0))
                          (c (/ (+ (cdr (nth i a)) (cdr (nth (1+ i) a)) (cdr (nth i b)) (cdr (nth (1+ i) b))) 4))
                          (color (funcall color-at c)))
                     (push (vector p0 p1 q1) tris) (push (vector p0 q1 q0) tris)
                     (push color tri-colors) (push color tri-colors))))))
    (cl-loop for curve in curves for newest = (null (cdr (memq curve curves))) do
             (cl-loop for (p q) on curve while q do
                      (push (vconcat (car p) (car q)) lines)
                      (push (if newest "#ffffff" (funcall color-at (/ (+ (cdr p) (cdr q)) 2))) line-colors))
             (push (car (seq-reduce (lambda (m p) (if (> (cdr p) (cdr m)) p m)) curve (car curve))) best))
    (setq best (nreverse best))
    (vector (financial-chart-3d-risk--axes (- w) w 0 height (- d) d)
            (list :id "surface" :type "mesh" :vertices (vconcat (nreverse verts))
                  :triangles (vconcat (nreverse tris)) :colors (vconcat (nreverse tri-colors)))
            (list :id "curves" :type "segments" :segments (vconcat (nreverse lines))
                  :colors (vconcat (nreverse line-colors)))
            (list :id "best-path" :type "segments" :color financial-chart-3d-risk-mark-color
                  :segments (vconcat (cl-loop for (p q) on best while q collect (vconcat p q))))
            (list :id "best" :type "points" :points (vconcat best) :size 3
                  :color financial-chart-3d-risk-mark-color))))

;;; Registration

(defconst financial-chart-3d-risk-marks
  '(("fc-risk-grid3d" . financial-chart-3d-risk--grid-mark)
    ("fc-risk-ribbons3d" . financial-chart-3d-risk--ribbons-mark)
    ("fc-risk-histogram3d" . financial-chart-3d-risk--histogram-mark)
    ("fc-risk-frontier3d" . financial-chart-3d-risk--frontier-mark))
  "The portfolio-risk 3D marks this file adds to graphics.el: (MARK . FUNCTION).")

(defun financial-chart-3d-risk--register-marks ()
  "Register `financial-chart-3d-risk-marks' with graphics.el."
  (dolist (m financial-chart-3d-risk-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; Now if graphics.el's marks are loaded, else when they load.
(with-eval-after-load 'graphics-eas-marks
  (financial-chart-3d-risk--register-marks))

;;; Opening, in 3D or 2D

(cl-defun financial-chart-3d-risk-open (name &key bindings id show)
  "Open risk template NAME (\"risk-correlation\", ...) and return its eas view.
In 3D (its fc3d/ variant) when graphics.el is available, else the 2D
template: the fallback draws the same BINDINGS (default the example).
ID names the view; SHOW draws it, in a graphics.el buffer or an eas
one."
  (unless (member name financial-chart-3d-risk-templates)
    (signal 'financial-chart-invalid-data
            (list (format "No risk template %S; templates: %s" name
                          (string-join financial-chart-3d-risk-templates ", "))
                  :code "unknown_template" :index nil :field "name")))
  (require 'financial-chart-3d)
  (if (financial-chart-3d-available-p)
      (financial-chart-3d-open name :bindings bindings :id id :show show)
    (let ((view (eas-view-open name :bindings (or bindings (eas-template-example name))
                               :id (or id (concat "fc2d/" name)))))
      (when show (eas-show view))
      view)))

(provide 'financial-chart-3d-risk)
;;; financial-chart-3d-risk.el ends here
