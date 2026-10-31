;;; financial-chart-3d-valuation.el --- 3D valuation surfaces via graphics.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Valuation surfaces, each a template in templates/financial/ (the 2D
;; fallback) with its 3D variant in templates3d/:
;;
;;   dcf-sensitivity  eq.dcf over WACC x terminal growth -> value per
;;                    share, the current price (eq.price_target) a
;;                    plane through it and the fair-value line where
;;                    they cross; snapshots (as_of) morph in turn
;;   revenue-surface  eq.revenue.possibility_surface: revenue by year x
;;                    outcome quantile, the median traced; it grows
;;                    year by year
;;   kelly-surface    stats.kelly.binary / stats.kelly.continuous: log
;;                    growth over edge x bet fraction, the Kelly
;;                    fraction traced along its ridge and the zero
;;                    growth line where over-betting starts to lose
;;
;; docs/3d-valuation.md is the data contract of each (the JSON its
;; invest-metrics metric emits); test/fixtures/3d/valuation/ holds an
;; example of each.  The marks (fc-dcf3d, fc-revenue3d, fc-kelly3d) are
;; one lowering, `financial-chart-3d-valuation--surface-mark', a lit
;; heightfield over an x by z grid with an optional reference plane,
;; the contour where the surface crosses it and a traced row or ridge.
;; They are registered with `graphics-eas-define-mark' when graphics.el
;; loads, as financial-chart-3d.el registers its own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-3d)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))

;;; Kelly growth rates

(defun financial-chart-3d-valuation--round4 (v)
  "V rounded to 4 decimals."
  (/ (fround (* v 10000.0)) 10000.0))

(defun financial-chart-3d-kelly-growth (model edge fraction &optional sigma odds)
  "Expected log growth per period of betting FRACTION at EDGE under MODEL.
MODEL \"continuous\" (stats.kelly.continuous): an asset of excess
return EDGE and volatility SIGMA (default 0.2), held at FRACTION of the
bankroll: FRACTION * EDGE - SIGMA^2 FRACTION^2 / 2.  MODEL \"binary\"
\(stats.kelly.binary): a bet paying net ODDS (default 1) whose expected
return per unit staked is EDGE, so it wins with p = (1 + EDGE) / (1 +
ODDS): p log(1 + ODDS FRACTION) + (1 - p) log(1 - FRACTION); nil at
FRACTION 1 or more (ruin)."
  (let ((edge (float edge)) (f (float fraction)))
    (pcase model
      ("binary"
       (let* ((b (float (or odds 1))) (p (/ (+ 1 edge) (+ 1 b))))
         (when (< f 1)
           (+ (* p (log (+ 1 (* b f)))) (* (- 1 p) (log (- 1 f)))))))
      (_ (let ((s (float (or sigma 0.2))))
           (- (* f edge) (/ (* s s f f) 2)))))))

(defun financial-chart-3d-kelly-fraction (model edge &optional sigma odds)
  "The Kelly fraction at EDGE under MODEL: EDGE / SIGMA^2, or EDGE / ODDS.
As `financial-chart-3d-kelly-growth' reads MODEL, SIGMA and ODDS; the
fraction where its growth is greatest."
  (pcase model
    ("binary" (/ (float edge) (or odds 1)))
    (_ (let ((s (float (or sigma 0.2)))) (/ (float edge) (* s s))))))

(cl-defun financial-chart-3d-kelly-surface (&key (model "continuous") edges fractions sigma odds (floor -1.0))
  "Rows {edge, fraction, growth} of MODEL's growth over EDGES x FRACTIONS.
The grid the kelly-surface template draws, from the parameters
stats.kelly.binary or stats.kelly.continuous take
\(`financial-chart-3d-kelly-growth': SIGMA, ODDS).  EDGES default to 1%
to 12%; FRACTIONS to 0 to 3 by 0.25 (continuous) or 0 to 0.95 by 0.05
\(binary).  Growth is rounded to 4 decimals and never below FLOOR
\(ruin is FLOOR).  Rows run by edge, then fraction."
  (let ((edges (append (or edges (cl-loop for i from 1 to 12 collect (/ i 100.0))) nil))
        (fractions (append (or fractions
                               (if (equal model "binary")
                                   (cl-loop for i from 0 to 19 collect (/ i 20.0))
                                 (cl-loop for i from 0 to 12 collect (/ i 4.0))))
                           nil)))
    (cl-loop for e in edges
             append (mapcar (lambda (f)
                              (let ((g (financial-chart-3d-kelly-growth model e f sigma odds)))
                                (list :edge e :fraction f
                                      :growth (financial-chart-3d-valuation--round4 (if g (max floor g) floor)))))
                            fractions))))

;;; The surface

(defun financial-chart-3d-valuation--crossings (grid xs zs level)
  "Segments [X0 Z0 X1 Z1] where GRID crosses LEVEL (marching squares).
GRID is a vector of rows (one per z), each a vector of numbers (one per
x); XS and ZS are the world positions of its columns and rows.  A
corner at LEVEL counts as above it; a saddle joins its crossings in
edge order."
  (let ((nz (length grid)) (nx (if (> (length grid) 0) (length (aref grid 0)) 0)) out)
    (dotimes (j (1- nz))
      (dotimes (i (1- nx))
        (let* ((corners (list (list (aref xs i) (aref zs j) (aref (aref grid j) i))
                              (list (aref xs (1+ i)) (aref zs j) (aref (aref grid j) (1+ i)))
                              (list (aref xs (1+ i)) (aref zs (1+ j)) (aref (aref grid (1+ j)) (1+ i)))
                              (list (aref xs i) (aref zs (1+ j)) (aref (aref grid (1+ j)) i))))
               (points
                (cl-loop for k from 0 below 4
                         for a = (nth k corners) for b = (nth (mod (1+ k) 4) corners)
                         for va = (nth 2 a) for vb = (nth 2 b)
                         when (not (eq (>= va level) (>= vb level)))
                         collect (let ((u (/ (- level va) (- vb va))))
                                   (cons (+ (car a) (* u (- (car b) (car a))))
                                         (+ (cadr a) (* u (- (cadr b) (cadr a)))))))))
          (cl-loop for (p q) on points by #'cddr
                   when q do (push (vector (car p) (cdr p) (car q) (cdr q)) out)))))
    (nreverse out)))

(defun financial-chart-3d-valuation--level (graphics rows)
  "The reference level of GRAPHICS over ROWS, or nil.
The first number in ROWS' `level_field' (a row's own price), else
GRAPHICS's `level'."
  (let ((lk (and (financial-chart-3d--opt graphics "level_field")
                 (financial-chart-3d--key (financial-chart-3d--opt graphics "level_field"))))
        (level (financial-chart-3d--opt graphics "level")))
    (or (and lk (seq-some (lambda (r) (let ((v (financial-chart-3d--value r lk))) (and (numberp v) (float v))))
                          rows))
        (and (numberp level) (float level)))))

(defun financial-chart-3d-valuation--surface-mark (graphics rows)
  "Primitives of a valuation surface: GRAPHICS over ROWS.
Channels x and z (numbers, sorted) are the grid, y the height.  The
distinct x and z values over every row (all of a replay's) are its
columns and rows, so a replay keeps its grid and scale: frames draw
the cells present, the rest of the grid empty.  Options:
  level, level_field  a reference level (or a row's field holding it):
                      a wire plane at that height, the surface
                      coloured `low_color' below it to `high_color'
                      above it, symmetrically, and the contour where it
                      crosses the plane (`contour', default on)
  trace               \"max\": the ridge, each z row's highest cell; a
                      number: the row at that z value (`trace_color')
  front               non-nil: the newest column outlined, as a
                      replay along x grows
  height, extent      the surface's world size (4, [12 9])"
  (let* ((xk (financial-chart-3d--field graphics "x" "x"))
         (zk (financial-chart-3d--field graphics "z" "z"))
         (yk (financial-chart-3d--field graphics "y" "y"))
         (dom (financial-chart-3d--domain rows))
         (num (lambda (r k) (let ((v (financial-chart-3d--value r k))) (and (numberp v) v))))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (funcall num r xk)) dom)))
         (zs (financial-chart-3d--categories (mapcar (lambda (r) (funcall num r zk)) dom)))
         (lk (and (financial-chart-3d--opt graphics "level_field")
                  (financial-chart-3d--key (financial-chart-3d--opt graphics "level_field"))))
         (levels (delq nil (cons (let ((l (financial-chart-3d--opt graphics "level"))) (and (numberp l) l))
                                 (and lk (mapcar (lambda (r) (funcall num r lk)) dom)))))
         (level (financial-chart-3d-valuation--level graphics rows))
         (ys (append (delq nil (mapcar (lambda (r) (funcall num r yk)) dom)) levels))
         (lo (float (if ys (apply #'min ys) 0)))
         (hi (float (if ys (apply #'max ys) 1)))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (financial-chart-3d--r3 (if (> hi lo) (/ height (- hi lo)) 1.0)))
         (base (financial-chart-3d--r3 (- (* lo scale))))
         (wy (lambda (v) (financial-chart-3d--r3 (+ base (* scale v)))))
         (extent (financial-chart-3d--opt graphics "extent" [12 9]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (nx (length xs)) (nz (length zs))
         (x-at (lambda (i) (financial-chart-3d--r3 (if (> nx 1) (- (* 2 w (/ i (float (1- nx)))) w) 0))))
         (z-at (lambda (j) (financial-chart-3d--r3 (if (> nz 1) (- (* 2 d (/ j (float (1- nz)))) d) 0))))
         (cells (make-hash-table :test #'equal))
         is js)
    (dolist (r rows)
      (let ((x (funcall num r xk)) (z (funcall num r zk)) (y (funcall num r yk)))
        (when (and x z y)
          (puthash (cons x z) (float y) cells)
          (push (seq-position xs x) is)
          (push (seq-position zs z) js))))
    (let* ((i0 (if is (apply #'min is) 0)) (i1 (if is (apply #'max is) -1))
           (j0 (if js (apply #'min js) 0)) (j1 (if js (apply #'max js) -1))
           (cols (cl-loop for i from i0 to i1 collect (nth i xs)))
           (grid (vconcat (cl-loop for j from j0 to j1
                                   collect (vconcat (mapcar (lambda (x) (gethash (cons x (nth j zs)) cells lo))
                                                            cols)))))
           (wx (vconcat (cl-loop for i from i0 to i1 collect (funcall x-at i))))
           (wz (vconcat (cl-loop for j from j0 to j1 collect (funcall z-at j))))
           (spread (and level (max (- hi level) (- level lo))))
           (contour-p (not (eq (financial-chart-3d--opt graphics "contour" t) :false)))
           (trace (financial-chart-3d--opt graphics "trace"))
           (lift (* 0.01 height)))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (if (and (>= (length wx) 2) (>= (length wz) 2))
           (list :id "surface" :type "heightfield"
                 :x (vector (aref wx 0) (aref wx (1- (length wx))))
                 :z (vector (aref wz 0) (aref wz (1- (length wz))))
                 :base base :y_scale scale
                 :heights (vconcat (mapcar (lambda (row) (vconcat (mapcar #'financial-chart-3d--r3 row))) grid))
                 :low_color (financial-chart-3d--opt graphics "low_color" financial-chart-3d-down-color)
                 :high_color (financial-chart-3d--opt graphics "high_color" financial-chart-3d-up-color)
                 :color_range (if (and spread (> spread 0))
                                  (vector (- level spread) (+ level spread))
                                (vector lo hi)))
         (list :id "surface" :type "segments" :segments []))
       ;; The reference plane, a wire grid over the whole extent.
       (list :id "plane" :type "segments" :color (financial-chart-3d--opt graphics "plane_color" "#ffb74d")
             :segments (if level
                           (let ((y (funcall wy level)) (n 6))
                             (vconcat
                              (cl-loop for k from 0 to n
                                       for u = (financial-chart-3d--r3 (- (* 2 w (/ k (float n))) w))
                                       for v = (financial-chart-3d--r3 (- (* 2 d (/ k (float n))) d))
                                       append (list (vector u y (- d) u y d) (vector (- w) y v w y v)))))
                         []))
       (list :id "contour" :type "segments" :color (financial-chart-3d--opt graphics "contour_color" "#ffffff")
             :segments (if (and level contour-p (>= (length wx) 2) (>= (length wz) 2))
                           (let ((y (financial-chart-3d--r3 (+ (funcall wy level) lift))))
                             (vconcat (mapcar (lambda (s) (vector (financial-chart-3d--r3 (aref s 0)) y
                                                                  (financial-chart-3d--r3 (aref s 1))
                                                                  (financial-chart-3d--r3 (aref s 2)) y
                                                                  (financial-chart-3d--r3 (aref s 3))))
                                              (financial-chart-3d-valuation--crossings grid wx wz level))))
                         []))
       (list :id "trace" :type "segments" :color (financial-chart-3d--opt graphics "trace_color" "#ffffff")
             :segments
             (let ((points
                    (cond
                     ((equal trace "max")
                      (cl-loop for row across grid for z across wz
                               collect (let ((k (seq-position row (seq-reduce #'max row (aref row 0)))))
                                         (vector (aref wx k) (funcall wy (aref row k)) z))))
                     ((numberp trace)
                      (when-let* ((j (seq-position zs trace)) ((<= j0 j j1)))
                        (cl-loop for v across (aref grid (- j j0)) for x across wx
                                 collect (vector x (funcall wy v) (funcall z-at j))))))))
               (vconcat (cl-loop for (a b) on points while b
                                 collect (vector (aref a 0) (financial-chart-3d--r3 (+ (aref a 1) lift)) (aref a 2)
                                                 (aref b 0) (financial-chart-3d--r3 (+ (aref b 1) lift)) (aref b 2))))))
       (list :id "front" :type "segments" :color (financial-chart-3d--opt graphics "front_color" "#8fd3ff")
             :segments (if (and (financial-chart-3d--opt graphics "front") (> (length wz) 1) (> (length wx) 0))
                           (let ((k (1- (length wx))))
                             (vconcat (cl-loop for j from 0 below (1- (length wz))
                                               collect (vector (aref wx k) (funcall wy (aref (aref grid j) k)) (aref wz j)
                                                               (aref wx k) (funcall wy (aref (aref grid (1+ j)) k))
                                                               (aref wz (1+ j))))))
                         []))))))

(defconst financial-chart-3d-valuation-marks
  '(("fc-dcf3d" . financial-chart-3d-valuation--surface-mark)
    ("fc-revenue3d" . financial-chart-3d-valuation--surface-mark)
    ("fc-kelly3d" . financial-chart-3d-valuation--surface-mark))
  "The valuation marks this file adds to graphics.el: (MARK . FUNCTION).
One lowering; each template sets its own options and camera.")

(defun financial-chart-3d-valuation--register-marks ()
  "Register `financial-chart-3d-valuation-marks' with graphics.el."
  (dolist (m financial-chart-3d-valuation-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; Now when graphics.el is loaded, else when a 3D command loads it.
(with-eval-after-load 'graphics-eas-marks
  (financial-chart-3d-valuation--register-marks))

(provide 'financial-chart-3d-valuation)
;;; financial-chart-3d-valuation.el ends here
