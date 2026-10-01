;;; financial-chart-payoff-curves.el --- Multiple payoff curves -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Render labeled payoff curves that share one ascending price grid.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)

(defun financial-chart-payoff-curves--invalid (index fmt &rest args)
  "Signal invalid curves at INDEX with formatted reason FMT and ARGS."
  (signal 'financial-chart-invalid-data
          (list (format "curve %d: %s" index (apply #'format fmt args))
                :code "invalid_data" :index index)))

(defun financial-chart-payoff-curves--same-grid-p (a b)
  "Return non-nil when price grids A and B are numerically equal."
  (and (= (length a) (length b))
       (cl-every #'identity (cl-mapcar #'= a b))))

(defun financial-chart-payoff-curves--validate (data)
  "Signal unless DATA is labeled payoffs on the same ascending price grid."
  (unless (listp data)
    (signal 'financial-chart-invalid-data
            (list (format "payoff-curves must be a list, got %S" data)
                  :code "invalid_data")))
  (let (grid have-grid)
    (cl-loop for curve in data for i from 0
             do (unless (and (consp curve) (stringp (car curve)))
                  (financial-chart-payoff-curves--invalid
                   i "expected (LABEL . PAYOFF) with a string label"))
             (condition-case err
                 (financial-chart--validate-payoff (cdr curve))
               (financial-chart-invalid-data
                (financial-chart-payoff-curves--invalid i "%s" (cadr err))))
             (let ((xs (financial-chart-series-xs (cdr curve))))
               (if have-grid
                   (unless (financial-chart-payoff-curves--same-grid-p grid xs)
                     (financial-chart-payoff-curves--invalid
                      i "prices must match the first curve's grid"))
                 (setq grid xs
                       have-grid t))))))

(defconst financial-chart-payoff-curves--extra-text-faces
  '(font-lock-constant-face font-lock-type-face
    font-lock-function-name-face font-lock-keyword-face)
  "Additional default text faces assigned after the up/down/accent faces.")

(defconst financial-chart-payoff-curves--braille-bits
  '((1 8) (2 16) (4 32) (64 128))
  "Braille bit values, indexed by row then column within a cell.")

(defun financial-chart-payoff-curves--dot (x y face dot-width masks owners)
  "Plot one braille dot at X,Y in MASKS and set its cell face in OWNERS."
  (let* ((cell-width (/ dot-width 2))
         (index (+ (* (/ y 4) cell-width) (/ x 2)))
         (bit (nth (% x 2) (nth (% y 4)
                                financial-chart-payoff-curves--braille-bits))))
    (aset masks index (logior (aref masks index) bit))
    (when face (aset owners index face))))

(defun financial-chart-payoff-curves--line (samples lo hi face dot-width dot-height
                                                    masks owners)
  "Draw SAMPLES mapped from LO..HI with FACE into the braille pixel grid."
  (let* ((last-x (1- (length samples)))
         (scale (max 0.001 (- hi lo)))
         (y-of (lambda (value)
                 (round (* (- 1.0 (/ (float (- value lo)) scale))
                           (1- dot-height))))))
    (if (= last-x 0)
        (financial-chart-payoff-curves--dot
         0 (funcall y-of (car samples)) face dot-width masks owners)
      (cl-loop for (a b) on samples
               for x0 from 0 below last-x
               for y0 = (funcall y-of a)
               for y1 = (funcall y-of b)
               do (financial-chart-payoff-curves--dot
                   x0 y0 face dot-width masks owners)
               (financial-chart-payoff-curves--dot
                (1+ x0) y1 face dot-width masks owners)))))

(cl-defun financial-chart-text-payoff-curves
    (curves &key (width 60) (height financial-chart-plot-height) (unit "$")
            (label-width 7) (up-face 'financial-chart-up)
            (down-face 'financial-chart-down) (dim-face 'financial-chart-dim)
            (accent-face 'financial-chart-accent) curve-faces (footer t)
            &allow-other-keys)
  "Render labeled CURVES sharing a price grid as an overlaid braille plot.
Each curve gets a face from CURVE-FACES (default: a distinct built-in
face per curve).  The legend names each curve, the baseline marks zero,
and FOOTER reports breakevens of the first curve.  WIDTH and HEIGHT are
braille character columns and rows."
  (when (and curves (cl-some (lambda (curve) (cdr curve)) curves))
    (let* ((width (max 1 (round width)))
           (height (max 1 (round height)))
           (dot-width (* 2 width))
           (dot-height (* 4 height))
           (faces (or curve-faces
                      (append (list up-face down-face accent-face)
                              financial-chart-payoff-curves--extra-text-faces)))
           (values (apply #'append
                          (mapcar (lambda (curve)
                                    (financial-chart-series-values (cdr curve)))
                                  curves)))
           (range (financial-chart-range values t))
           (lo (car range))
           (hi (cdr range))
           (flat (= lo hi))
           (lo (if flat -0.001 lo))
           (hi (if flat 0.001 hi))
           (masks (make-vector (* width height) 0))
           (owners (make-vector (* width height) nil))
           (scale (max 0.001 (- hi lo)))
           (zero-y (round (* (- 1.0 (/ (float (- 0 lo)) scale))
                             (1- dot-height))))
           (first-payoff (cdar curves))
           (first-xs (financial-chart-series-xs first-payoff)))
      (dotimes (x dot-width)
        (financial-chart-payoff-curves--dot x zero-y nil dot-width masks owners))
      (cl-loop for curve in curves for i from 0
               for face = (nth (% i (length faces)) faces)
               do (financial-chart-payoff-curves--line
                   (financial-chart-interpolate (cdr curve) dot-width)
                   lo hi face dot-width dot-height masks owners))
      (concat
       (mapconcat
        (lambda (row)
          (let ((label (cond ((zerop row) (financial-chart-fmt-money hi unit))
                             ((= row (1- height)) (financial-chart-fmt-money lo unit))
                             ((= row (/ zero-y 4)) "0")
                             (t ""))))
            (concat
             (financial-chart-text--label label label-width dim-face)
             (mapconcat #'identity
                        (cl-loop for col from 0 below width
                                 for index = (+ (* row width) col)
                                 for mask = (aref masks index)
                                 for face = (or (aref owners index) dim-face)
                                 collect (propertize (string (+ #x2800 mask)) 'face face))
                        ""))))
        (number-sequence 0 (1- height)) "\n")
       (when first-xs
         (concat "\n" (financial-chart-text--label "" label-width dim-face)
                 (format "%s%s%s%s"
                         unit (financial-chart-fmt (car first-xs))
                         (make-string (max 1 (- width 12)) ?\s)
                         (concat unit (financial-chart-fmt (car (last first-xs)))))))
       "\n"
       (financial-chart-text--label "" label-width dim-face)
       (mapconcat #'identity
                  (cl-loop for curve in curves for i from 0
                           collect (concat
                                    (propertize "●" 'face
                                                (nth (% i (length faces)) faces))
                                    " " (format "%s" (car curve))))
                  "  ")
       (when footer
         (let ((bes (financial-chart-payoff-breakevens first-payoff)))
           (concat "\n\n" (financial-chart-text--label "" label-width dim-face)
                   (propertize
                    (if bes
                        (format "breakeven %s"
                                (mapconcat (lambda (b)
                                             (concat unit (financial-chart-fmt b)))
                                           bes ", "))
                      "no breakeven")
                    'face accent-face))))))))

(defun financial-chart-payoff-curves--svg-x (x x0 width minimum span)
  "Map price X using plot origin X0, WIDTH, MINIMUM and SPAN."
  (financial-chart-svg--n (+ x0 (* width (/ (- x minimum) span)))))

(defun financial-chart-payoff-curves--svg-y (y y0 height minimum span)
  "Map payoff Y using plot origin Y0, HEIGHT, MINIMUM and SPAN."
  (financial-chart-svg--n (+ y0 (* height (- 1 (/ (- y minimum) span))))))

(cl-defun financial-chart-svg-payoff-curves
    (curves &key (width 600) (height 260) (unit "$") title curve-colors
            &allow-other-keys)
  "SVG string of labeled CURVES as distinct polylines on a zero baseline.
CURVE-COLORS overrides the default ten-color palette."
  (when (and curves (cl-some (lambda (curve) (cdr curve)) curves))
    (let* ((first-payoff (cdar curves))
           (xs (financial-chart-series-xs first-payoff))
           (values (apply #'append
                          (mapcar (lambda (curve)
                                    (financial-chart-series-values (cdr curve)))
                                  curves)))
           (range (financial-chart-range values t))
           (lo (car range))
           (hi (cdr range))
           (flat (= lo hi))
           (lo (if flat -0.001 lo))
           (hi (if flat 0.001 hi))
           (palette
            (or curve-colors
                (cl-loop for index below (length curves)
                         collect (financial-chart-svg--series-color index)))))
      (when (and xs (cdr xs))
        (let* ((frame (financial-chart-svg--frame width height title))
               (x0 (nth 0 frame))
               (y0 (nth 1 frame))
               (w (nth 2 frame))
               (h (max 1 (- (nth 3 frame) 52)))
               (x1 (+ x0 w))
               (y1 (+ y0 h))
               (span-x (float (max 0.001 (- (car (last xs)) (car xs)))))
               (span-y (float (max 0.001 (- hi lo))))
               (zy (financial-chart-payoff-curves--svg-y 0 y0 h lo span-y))
               (y-ticks (mapcar (lambda (value)
                                  (list (financial-chart-payoff-curves--svg-y
                                         value y0 h lo span-y)
                                        (financial-chart-fmt-money value unit)))
                                (list hi 0 lo)))
               (x-ticks `((0 ,(concat unit (financial-chart-fmt (car xs))))
                          (0.5 ,(concat unit
                                        (financial-chart-fmt
                                         (/ (+ (car xs) (car (last xs))) 2.0))))
                          (1 ,(concat unit (financial-chart-fmt (car (last xs)))))))
               (svg (financial-chart-svg--canvas width height title))
               (legend-x x0))
          (financial-chart-svg--horizontal-ticks svg y-ticks x0 x1)
          (financial-chart-svg--vertical-ticks svg x-ticks x0 y0 w h)
          (svg-line svg x0 zy x1 zy :stroke (financial-chart-svg--color 'grid)
                    :stroke-dasharray "4 3")
          (cl-loop for curve in curves for i from 0
                   for color = (nth (% i (length palette)) palette)
                   for curve-xs = (financial-chart-series-xs (cdr curve))
                   for curve-ys = (financial-chart-series-values (cdr curve))
                   for points = (cl-mapcar (lambda (x y)
                                             (cons (financial-chart-payoff-curves--svg-x
                                                    x x0 w (car xs) span-x)
                                                   (financial-chart-payoff-curves--svg-y
                                                    y y0 h lo span-y)))
                                           curve-xs curve-ys)
                   do (svg-polyline svg points :fill "none" :stroke color
                                    :stroke-width 2)
                   (cl-loop for x in curve-xs for y in curve-ys
                            do (financial-chart-svg--point-target
                                svg
                                (financial-chart-payoff-curves--svg-x
                                 x x0 w (car xs) span-x)
                                (financial-chart-payoff-curves--svg-y y y0 h lo span-y)
                                (format "%s: price %s, P/L %s"
                                        (car curve) (financial-chart-fmt x)
                                        (financial-chart-fmt-money y unit))))
                   (svg-line svg legend-x (+ y1 29) (+ legend-x 14) (+ y1 29)
                             :stroke color :stroke-width 2)
                   (financial-chart-svg--text svg (format "%s" (car curve))
                                              (+ legend-x 19) (+ y1 33) "start" color)
                   (setq legend-x (+ legend-x 34
                                     (* 7 (string-width (format "%s" (car curve)))))))
          (financial-chart-svg--string svg))))))

(defun financial-chart-payoff-curves--values (data _props)
  "Every P/L value across the curves in DATA, for summaries."
  (apply #'append
         (mapcar (lambda (curve) (financial-chart-series-values (cdr curve))) data)))

(defun financial-chart-payoff-curves--from-json (data)
  "JSON-parsed DATA ([LABEL, PAYOFF] pairs) as (LABEL . PAYOFF) curves."
  (mapcar (lambda (curve) (cons (car curve) (cdr curve))) data))

(defun financial-chart-payoff-curves--example ()
  "Build three deterministic option payoff curves on 21 prices."
  (let ((prices (number-sequence 80 120 2))
        (curves
         `(("Long straddle" . ,(lambda (price) (- (abs (- price 100)) 8)))
           ("Call spread" . ,(lambda (price) (- (min 24 (max 0 (- price 100))) 6)))
           ("Put spread" . ,(lambda (price) (- (min 16 (max 0 (- 100 price))) 5))))))
    (mapcar (lambda (curve)
              (cons (car curve)
                    (mapcar (lambda (price)
                              (list price (funcall (cdr curve) price)))
                            prices)))
            curves)))

(add-to-list 'financial-chart-shapes
             '(payoff-curves
               :doc "String-labeled (LABEL . PAYOFF) curves over one ascending price grid."
               :example (("T+0" . ((90 30) (100 -20) (110 30)))
                         ("T+15" . ((90 20) (100 -5) (110 40)))
                         ("T+30" . ((90 10) (100 10) (110 20))))
               :validator financial-chart-payoff-curves--validate
               :values financial-chart-payoff-curves--values
               :from-json financial-chart-payoff-curves--from-json))

(setf (plist-get (alist-get 'payoff-curves financial-chart-shapes) :example)
      (financial-chart-payoff-curves--example))

(financial-chart-register-kind
 'payoff-curves
 :shape 'payoff-curves
 :text #'financial-chart-text-payoff-curves
 :svg #'financial-chart-svg-payoff-curves
 :doc "Overlaid P/L curves sharing a price grid, with a legend and zero line.")

(provide 'financial-chart-payoff-curves)
;;; financial-chart-payoff-curves.el ends here
