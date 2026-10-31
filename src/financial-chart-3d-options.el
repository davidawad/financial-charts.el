;;; financial-chart-3d-options.el --- Options surfaces in 3D: greeks, position P/L, probability of profit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Three options surfaces, each a 2D template in templates/financial/
;; (the fallback without graphics.el) and its 3D variant in templates3d/
;; (scripts/templates3d_options.py writes them):
;;
;;   greeks-surface        one greek (slot `greek': delta, gamma, vega,
;;                         theta, vanna or charm) over spot and days to
;;                         expiry; replays toward expiry
;;   position-pnl-surface  a multi-leg position's P/L over spot and days
;;                         to expiry, the expiry payoff its front edge;
;;                         replays day by day, theta decay visible
;;   pop-surface           probability of profit (slot `metric': pop or
;;                         prob_touch) over strike and expiry, on an
;;                         absolute 0..1 scale with the even-odds level
;;
;; Each takes the rows an invest-metrics grid metric emits
;; (opt.greeks_grid, opt.payoff_grid, opt.pop_grid; docs/3d-options.md
;; is their data contract and the request for those metrics);
;; `financial-chart-3d-options-bindings' turns a metric run's JSON into
;; bindings, and `financial-chart-3d-options-show' draws them in 3D, or
;; in 2D when graphics.el is absent.
;;
;; financial-chart-3d.el loads this file; its marks register with
;; graphics.el when graphics.el loads:
;;
;;   fc-greeks3d  a heightfield of the `y' field over x (spot) and z
;;                (days to expiry, furthest at the back), a cool to warm
;;                gradient, the zero level framed when the greek changes
;;                sign and the slice at the current spot traced
;;   fc-pnl3d     the same over P/L, red to green, the expiry payoff
;;                drawn dim at the front from the first frame so the
;;                surface decays toward it
;;   fc-pop3d     the same over a probability, coloured red at 0 to
;;                green at 1 whatever the rows' range, the 0.5 level
;;                framed

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function graphics-eas-show "graphics-eas" (view &optional cols rows window))
(declare-function eas-show "eas-mode" (view &optional target))
(declare-function financial-chart-3d-available-p "financial-chart-3d" ())
(declare-function financial-chart-3d-open "financial-chart-3d" (name &rest args))
(declare-function financial-chart-3d--opt "financial-chart-3d" (graphics name &optional default))
(declare-function financial-chart-3d--field "financial-chart-3d" (graphics channel default))
(declare-function financial-chart-3d--value "financial-chart-3d" (row key))
(declare-function financial-chart-3d--categories "financial-chart-3d" (values))
(declare-function financial-chart-3d--r3 "financial-chart-3d" (v))
(declare-function financial-chart-3d--domain "financial-chart-3d" (rows))
(declare-function financial-chart-3d--axes "financial-chart-3d" (id x0 x1 y1 z0 z1 &optional color))

(defconst financial-chart-3d-options-templates
  '("greeks-surface" "position-pnl-surface" "pop-surface")
  "The options surface templates (2D in templates/financial/, 3D in templates3d/).")

(defconst financial-chart-3d-options-greeks '("delta" "gamma" "vega" "theta" "vanna" "charm")
  "The greeks a greeks-surface row carries and its `greek' slot selects.")

;;; The surface every mark draws

(defun financial-chart-3d-options--at (xs heights v)
  "The height at V of the row HEIGHTS over sorted numbers XS, or nil.
Linear between columns; nil outside them."
  (let ((n (length xs)))
    (cl-loop for i from 0 below (1- n)
             for a = (nth i xs) for b = (nth (1+ i) xs)
             when (<= a v b)
             return (cons (+ i (if (= a b) 0 (/ (- v a) (float (- b a)))))
                          (let ((ha (aref heights i)) (hb (aref heights (1+ i))))
                            (+ ha (* (if (= a b) 0 (/ (- v a) (float (- b a)))) (- hb ha))))))))

(cl-defun financial-chart-3d-options--surface (graphics rows &key x z y level ghost low high colors)
  "Primitives of an options surface: GRAPHICS over ROWS as a heightfield.
X, Z and Y are the default fields of the x (columns), z (rows, numbers
furthest first, so the nearest is the front edge) and y (height)
channels.  The height spans the rows' range.  LEVEL, when inside it,
is framed (zero P/L, even odds); with GHOST the z row nearest expiry is
drawn dim from the first frame.  LOW and HIGH are the gradient's
colours over COLORS (default the rows' range).  Each z row is a strip
of the heightfield, the `seam' fraction (0.4) of the gap behind it left
open, so the rows read apart even without colour.  Under a time
encoding the grid and scale span every row
\(`graphics-eas-domain-rows'), the rows present so far are drawn in
place and the newest traced bright.  Option `spot' traces the slice at
that x across the shown rows, dropped to the floor at the front."
  (let* ((xk (financial-chart-3d--field graphics "x" x))
         (zk (financial-chart-3d--field graphics "z" z))
         (yk (financial-chart-3d--field graphics "y" y))
         (dom (financial-chart-3d--domain rows))
         (num (lambda (r k) (let ((v (financial-chart-3d--value r k))) (and (numberp v) v))))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (funcall num r xk)) dom)))
         (zs (reverse (financial-chart-3d--categories (mapcar (lambda (r) (funcall num r zk)) dom))))
         (ys (delq nil (mapcar (lambda (r) (let ((v (funcall num r yk))) (and v (float v)))) dom)))
         (lo (if ys (apply #'min ys) 0.0))
         (hi (if ys (apply #'max ys) 1.0))
         (hi (if (> hi lo) hi (1+ lo)))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (/ height (- hi lo)))
         (base (financial-chart-3d--r3 (- (* lo scale))))
         (extent (financial-chart-3d--opt graphics "extent" [12 7]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (spot (financial-chart-3d--opt graphics "spot"))
         (cells (make-hash-table :test #'equal))
         (present nil))
    ;; Cells from every row (the ghost reads ahead), shown rows from ROWS.
    (dolist (r dom)
      (let ((xv (funcall num r xk)) (zv (funcall num r zk)) (yv (funcall num r yk)))
        (when (and xv zv yv)
          (puthash (cons xv zv) (float yv) cells))))
    (dolist (r rows)
      (when (and (funcall num r xk) (funcall num r yk))
        (push (funcall num r zk) present)))
    (let* ((shown (seq-filter (lambda (zv) (memql zv present)) zs))
           (nx (length xs)) (nz (length zs)) (k (length shown))
           (x-at (lambda (i) (financial-chart-3d--r3 (if (> nx 1) (- (* 2 w (/ i (float (1- nx)))) w) 0))))
           (z-at (lambda (j) (financial-chart-3d--r3 (if (> nz 1) (- (* 2 d (/ j (float (1- nz)))) d) 0))))
           (y-at (lambda (v) (financial-chart-3d--r3 (+ base (* scale (max lo (min hi v)))))))
           (heights (lambda (zv) (vconcat (mapcar (lambda (xv) (financial-chart-3d--r3 (gethash (cons xv zv) cells lo)))
                                                  xs))))
           (trace (lambda (zv j)
                    (let ((hs (funcall heights zv)) (zz (funcall z-at j)))
                      (vconcat (cl-loop for i from 0 below (1- nx)
                                        collect (vector (funcall x-at i) (funcall y-at (aref hs i)) zz
                                                        (funcall x-at (1+ i)) (funcall y-at (aref hs (1+ i))) zz))))))
           (ghost-z (and ghost (car (last zs))))
           (level (and level (< lo level hi) (funcall y-at level)))
           (slice (and (numberp spot) (> nx 1) (<= (car xs) spot (car (last xs)))
                       (cl-loop for zv in shown for j from 0
                                collect (let ((at (financial-chart-3d-options--at xs (funcall heights zv) spot)))
                                          (list (financial-chart-3d--r3 (- (* 2 w (/ (car at) (1- nx))) w))
                                                (funcall y-at (cdr at)) (funcall z-at j)))))))
      (vconcat
       (list (financial-chart-3d--axes "axes" (- w) w height (- d) d))
       (if (and (>= nx 2) (>= k 2))
                 (let ((rows (vconcat (mapcar heights shown))) (seam (financial-chart-3d--opt graphics "seam" 0.4)))
                   (vconcat
                    (cl-loop for j from 0 below (1- k)
                             collect (let* ((a (aref rows j)) (b (aref rows (1+ j)))
                                            (z0 (funcall z-at j)) (z1 (funcall z-at (1+ j))))
                                       (list :id (format "surface-%d" j) :type "heightfield" :x (vector (- w) w)
                                             :z (vector (financial-chart-3d--r3 (+ z0 (* seam (- z1 z0)))) z1)
                                             :base base :y_scale (financial-chart-3d--r3 scale)
                                             :heights (vector (vconcat (cl-mapcar (lambda (p q) (financial-chart-3d--r3 (+ p (* seam (- q p)))))
                                                                                  a b))
                                                              b)
                                             :low_color (financial-chart-3d--opt graphics "low_color" low)
                                             :high_color (financial-chart-3d--opt graphics "high_color" high)
                                             :color_range (or colors (vector lo hi)))))))
         (list (list :id "surface-0" :type "segments" :segments [])))
       (list
       (list :id "ghost" :type "segments" :color "#8a94a6"
             :segments (if (and ghost-z (not (eql ghost-z (car (last shown))))) (funcall trace ghost-z (1- nz)) []))
       (list :id "front" :type "segments" :color "#ffffff"
             :segments (if shown (funcall trace (car (last shown)) (1- k)) []))
       (list :id "spot" :type "segments" :color "#ffb74d"
             :segments (if slice
                           (vconcat (cl-loop for (a b) on slice while b
                                             collect (vector (nth 0 a) (nth 1 a) (nth 2 a) (nth 0 b) (nth 1 b) (nth 2 b)))
                                    (let ((f (car (last slice))))
                                      (list (vector (nth 0 f) (nth 1 f) (nth 2 f) (nth 0 f) 0 (nth 2 f)))))
                         []))
       (list :id "level" :type "segments" :color "#8a94a6"
             :segments (if level
                           (vector (vector (- w) level (- d) w level (- d)) (vector w level (- d) w level d)
                                   (vector w level d (- w) level d) (vector (- w) level d (- w) level (- d)))
                         [])))))))

;;; The marks

(defun financial-chart-3d-options--greeks-mark (graphics rows)
  "Primitives of the fc-greeks3d mark: GRAPHICS over greeks-grid ROWS.
Channels x (spot), z (dte) and y (the greek, default gamma)."
  (financial-chart-3d-options--surface graphics rows :x "spot" :z "dte" :y "gamma" :level 0
                                       :low "#3b6fb6" :high "#f2a541"))

(defun financial-chart-3d-options--pnl-mark (graphics rows)
  "Primitives of the fc-pnl3d mark: GRAPHICS over payoff-grid ROWS.
Channels x (spot), z (dte) and y (pnl); the expiry row is the front
edge, drawn dim until the replay reaches it."
  (financial-chart-3d-options--surface graphics rows :x "spot" :z "dte" :y "pnl" :level 0 :ghost t
                                       :low "#ef5350" :high "#26a69a"))

(defun financial-chart-3d-options--pop-mark (graphics rows)
  "Primitives of the fc-pop3d mark: GRAPHICS over pop-grid ROWS.
Channels x (strike), z (dte) and y (pop); coloured on the absolute
scale, red at 0 to green at 1, the even-odds level framed."
  (financial-chart-3d-options--surface graphics rows :x "strike" :z "dte" :y "pop" :level 0.5
                                       :low "#ef5350" :high "#26a69a" :colors [0 1]))

(defconst financial-chart-3d-options-marks
  '(("fc-greeks3d" . financial-chart-3d-options--greeks-mark)
    ("fc-pnl3d" . financial-chart-3d-options--pnl-mark)
    ("fc-pop3d" . financial-chart-3d-options--pop-mark))
  "The options surface marks this file adds to graphics.el: (MARK . FUNCTION).")

(defun financial-chart-3d-options--register-marks ()
  "Register `financial-chart-3d-options-marks' with graphics.el."
  (dolist (m financial-chart-3d-options-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; Now when graphics.el is loaded, else when it loads.
(with-eval-after-load 'graphics-eas-marks
  (financial-chart-3d-options--register-marks))

;;; From an invest metric run to bindings

(defun financial-chart-3d-options-with-day (rows)
  "ROWS (plists with :dte) each with :day, days from the furthest dte.
The 3D replay steps through :day; rows that carry one keep it."
  (let* ((rows (append rows nil))
         (dtes (delq nil (mapcar (lambda (r) (let ((v (plist-get r :dte))) (and (numberp v) v))) rows)))
         (furthest (and dtes (apply #'max dtes))))
    (vconcat (mapcar (lambda (r)
                       (if (or (plist-get r :day) (not (numberp (plist-get r :dte)))) r
                         (append r (list :day (- furthest (plist-get r :dte))))))
                     rows))))

(defun financial-chart-3d-options-bindings (template output &rest slots)
  "Bindings for options TEMPLATE from invest metric run OUTPUT.
OUTPUT is the metric's JSON as a plist (docs/3d-options.md): its :rows
become `data' (with :day added where missing,
`financial-chart-3d-options-with-day'), :params :spot becomes `spot'
and :title `title'.  SLOTS (a plist such as (:greek \"vega\")) are
added last and win."
  (unless (member template financial-chart-3d-options-templates)
    (signal 'financial-chart-invalid-data
            (list (format "No options surface template %S; templates: %s" template
                          (string-join financial-chart-3d-options-templates ", "))
                  :code "unknown_template" :index nil :field "template")))
  (let* ((rows (plist-get output :rows))
         (params (plist-get output :params))
         (spot (plist-get params :spot))
         (bindings (list :data (financial-chart-3d-options-with-day rows))))
    (unless (and rows (> (length rows) 0))
      (signal 'financial-chart-invalid-data
              (list (format "%s needs the metric's rows" template) :code "empty_grid" :index nil :field "rows")))
    (when (numberp spot) (setq bindings (plist-put bindings :spot spot)))
    (when (stringp (plist-get output :title)) (setq bindings (plist-put bindings :title (plist-get output :title))))
    (cl-loop for (k v) on slots by #'cddr do (setq bindings (plist-put bindings k v)))
    bindings))

;;;###autoload
(defun financial-chart-3d-options-show (template bindings)
  "Show options TEMPLATE with BINDINGS: in 3D, else in 2D.
With graphics.el the 3D variant (\"fc3d/TEMPLATE\") opens in a
graphics.el buffer; without it the 2D template opens in an eas buffer.
Returns the view."
  (if (and (fboundp 'financial-chart-3d-available-p) (financial-chart-3d-available-p))
      (financial-chart-3d-open template :bindings bindings :show t)
    (require 'eas-mode)
    (let ((view (eas-view-open template :bindings bindings :id (concat "fc/" template))))
      (eas-show view)
      view)))

(provide 'financial-chart-3d-options)
;;; financial-chart-3d-options.el ends here
