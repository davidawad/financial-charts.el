;;; financial-chart-multi.el --- Multi-series chart kind for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Compare named series on a shared scale.  Optional normalization rebases
;; each series to the same starting value, which is useful for comparing
;; ticker performance.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-series)
(require 'financial-chart-text)
(require 'financial-chart-svg)
(require 'financial-chart-plot)

(defconst financial-chart-multi-text-faces
  '(financial-chart-up financial-chart-down font-lock-keyword-face financial-chart-accent)
  "Text faces assigned to series in order, then cycled.")

(defun financial-chart-multi--label (label)
  "Return LABEL as display text with control characters replaced by spaces."
  (replace-regexp-in-string "[[:cntrl:]]" " " (format "%s" label)))

(defun financial-chart-multi--svg-colors (count)
  "Return COUNT distinct SVG colors from the shared palette."
  (cl-loop for index below count
           collect (financial-chart-svg--series-color index)))

(defun financial-chart-multi--prepare (data normalize)
  "Return DATA as (LABEL . VALUES) entries, rebased to NORMALIZE when set."
  (unless (or (null normalize) (numberp normalize))
    (signal 'financial-chart-invalid-data
            (list ":normalize must be nil or a number" :code "invalid_data")))
  (cl-loop for (label . series) in data
           for index from 0
           for values = (financial-chart-series-values series)
           collect
           (cons label
                 (if (and normalize values)
                     (let ((base (car values)))
                       (when (zerop base)
                         (signal 'financial-chart-invalid-data
                                 (list (format "series %S starts at zero; choose another base before normalizing"
                                               label)
                                       :code "invalid_data" :index index)))
                       (mapcar (lambda (value)
                                 (* normalize (/ (float value) base)))
                               values))
                   values))))

(defun financial-chart-multi--validate (data)
  "Signal unless DATA is a list of (LABEL . SERIES) entries."
  (unless (and (listp data) (integerp (proper-list-p data)))
    (financial-chart--invalid 0 "multi-series data must be a proper list"))
  (cl-loop for entry in data
           for index from 0
           do (unless (and (consp entry)
                           (or (stringp (car entry)) (symbolp (car entry)))
                           (or (listp (cdr entry)) (vectorp (cdr entry))))
                (financial-chart--invalid index
                                          "expected (LABEL . SERIES), with a string or symbol LABEL; got %S"
                                          entry))
           do (condition-case err
                  (financial-chart--validate-series (cdr entry))
                (error
                 (financial-chart--invalid index "series %S is invalid: %s"
                                           (car entry) (error-message-string err)))))
  t)

(defun financial-chart-multi--sample (values count)
  "Sample VALUES to COUNT points, interpolating short series across the width."
  (let ((length (length values)))
    (cond
     ((zerop length) nil)
     ((= length 1) (make-list count (car values)))
     ((>= length count) (financial-chart-resample values count))
     (t
      (let ((vector (vconcat values)))
        (cl-loop for i from 0 below count
                 for position = (/ (* i (1- length)) (float (1- count)))
                 for lower = (floor position)
                 for upper = (min (1- length) (1+ lower))
                 for fraction = (- position lower)
                 collect (+ (aref vector lower)
                            (* fraction (- (aref vector upper) (aref vector lower))))))))))

(defun financial-chart-multi--range (series)
  "Return the shared numeric range of prepared SERIES, or nil when empty."
  (let ((values (apply #'append (mapcar #'cdr series))))
    (when values (financial-chart-range values))))

(cl-defun financial-chart-text-multi
    (data &key (width 60) (height financial-chart-plot-height) (unit "") normalize
          (label-width 6) (up-face 'financial-chart-up) (down-face 'financial-chart-down)
          (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
          &allow-other-keys)
  "Render named DATA series as a shared-scale braille line chart.
Each entry is (LABEL . SERIES), where SERIES uses the `series' shape.
NORMALIZE, when numeric, rebases each series to that value at its first
numeric point.  Series faces cycle through `financial-chart-multi-text-faces';
UNIT suffixes the Y-axis and legend values."
  (let* ((series (financial-chart-multi--prepare data normalize))
         (sampled (mapcar (lambda (entry)
                            (cons (car entry)
                                  (financial-chart-multi--sample (cdr entry) (* 2 width))))
                          series)))
    (when-let* ((range (financial-chart-multi--range sampled)))
      (let* ((lo (car range))
             (hi (cdr range))
             (span (max 0.001 (- hi lo)))
             (dots (* height 4))
             (ncols width)
             (grid (make-vector (* height ncols) 0))
             (faces (make-vector (* height ncols) dim-face))
             (palette (copy-sequence financial-chart-multi-text-faces)))
        (setf (nth 0 palette) up-face
              (nth 1 palette) down-face
              (nth 3 palette) accent-face)
        (cl-labels ((dot (x y face)
                      (let* ((cell (+ (* (/ y 4) ncols) (/ x 2)))
                             (bits (aref (aref financial-chart-text--braille-bits (% y 4))
                                         (% x 2))))
                        (aset grid cell (logior (aref grid cell) bits))
                        (aset faces cell face))))
          (cl-loop for entry in sampled
                   for series-index from 0
                   for values = (cdr entry)
                   for ys = (mapcar (lambda (value)
                                      (- dots 1 (round (* (1- dots)
                                                          (/ (float (- value lo)) span)))))
                                    values)
                   for face = (nth (% series-index (length palette)) palette)
                   do (cl-loop for (y0 y1) on ys
                               for x from 0
                               do (dot x y0 face)
                               when y1
                               do (cl-loop for y from (min y0 y1) to (max y0 y1)
                                           do (dot (if (< y (/ (+ y0 y1) 2.0)) x (1+ x))
                                                   y face))))
        (concat
         (mapconcat
          (lambda (row)
            (concat
             (financial-chart-text--label
              (cond ((= row 0) (concat (financial-chart-fmt hi) unit))
                    ((= row (1- height)) (concat (financial-chart-fmt lo) unit))
                    (t ""))
              label-width dim-face)
             (apply #'concat
                    (cl-loop for col from 0 below ncols
                             for cell = (+ (* row ncols) col)
                             collect (propertize
                                      (string (+ #x2800 (aref grid cell)))
                                      'face (aref faces cell))))
             "\n"))
          (number-sequence 0 (1- height)) "")
         (mapconcat
          (lambda (index)
            (let* ((entry (nth index series))
                   (face (nth (% index (length palette)) palette))
                   (values (cdr entry)))
              (propertize
               (format "%s %s%s"
                       (financial-chart-multi--label (car entry))
                       (if values (financial-chart-fmt (car (last values))) "—")
                       (if values unit ""))
               'face face)))
          (number-sequence 0 (1- (length series)))
          "  ")
         "\n"))))))

(cl-defun financial-chart-svg-multi
    (data &key (width 600) (height 240) (unit "") title normalize &allow-other-keys)
  "Render named DATA series as a shared-scale SVG line chart.
NORMALIZE, when numeric, rebases each series to that value at its first
numeric point.  UNIT suffixes the Y-axis and legend values."
  (let* ((series (financial-chart-multi--prepare data normalize))
         (frame (financial-chart-svg--frame width height title))
         (x0 (nth 0 frame))
         (y0 (nth 1 frame))
         (w (nth 2 frame))
         (plot-height (max 1 (- (nth 3 frame) 46)))
         (colors (financial-chart-multi--svg-colors (length series)))
         (range (financial-chart-multi--range series)))
    (when range
      (let* ((lo (car range))
             (hi (cdr range))
             (span (max 0.001 (- hi lo)))
             (svg (financial-chart-svg--canvas width height title))
             (ticks (list hi (/ (+ hi lo) 2.0) lo))
             (x-ticks (financial-chart-svg--series-x-ticks (cdar series) w))
             (legend-y (+ y0 plot-height 38)))
        (financial-chart-svg--horizontal-ticks
         svg (cl-loop for value in ticks for index from 0
                      collect (list (+ y0 (* plot-height (/ index 2.0)))
                                    (concat (financial-chart-fmt value) unit)))
         x0 (+ x0 w))
        (when x-ticks
          (financial-chart-svg--series-x-axis svg x-ticks x0 y0 w plot-height nil))
        (cl-loop for entry in series
                 for index from 0
                 for values = (financial-chart-multi--sample
                               (cdr entry) (max 2 (floor w)))
                 for color = (nth index colors)
                 when values
                 do (let* ((last-index (1- (length values)))
                           (points
                            (cl-loop for value in values
                                     for i from 0
                                     collect
                                     (cons (financial-chart-svg--n
                                            (+ x0 (* w (/ i (float (max 1 last-index))))))
                                           (financial-chart-svg--n
                                            (+ y0 (* plot-height
                                                     (- 1 (/ (float (- value lo)) span)))))))))
                      (svg-polyline svg points :fill "none" :stroke color :stroke-width 1.8)
                      (cl-loop for value in (cdr entry) for point-index from 0
                               for point-x = (+ x0 (* w (/ point-index
                                                           (float (max 1 (1- (length (cdr entry))))))))
                               for point-y = (+ y0 (* plot-height
                                                      (- 1 (/ (float (- value lo)) span))))
                               do (financial-chart-svg--point-target
                                   svg point-x point-y
                                   (format "%s: %s%s"
                                           (financial-chart-multi--label (car entry))
                                           (financial-chart-fmt value) unit)))))
        (let ((step (/ (float w) (max 1 (length series)))))
          (cl-loop for entry in series
                   for index from 0
                   for color = (nth index colors)
                   for values = (cdr entry)
                   for x = (+ x0 (* index step))
                   do (svg-line svg x (- legend-y 4) (+ x 14) (- legend-y 4)
                                :stroke color :stroke-width 2)
                   do (financial-chart-svg--text
                       svg (format "%s %s%s"
                                   (financial-chart-multi--label (car entry))
                                   (if values (financial-chart-fmt (car (last values))) "—")
                                   (if values unit ""))
                       (+ x 19) legend-y "start" color)))
        (financial-chart-svg--string svg)))))

(defun financial-chart-multi--values (data props)
  "Every plotted value in DATA, rebased per PROPS' :normalize."
  (apply #'append
         (mapcar #'cdr (financial-chart-multi--prepare data (plist-get props :normalize)))))

(defun financial-chart-multi--check (data props)
  "Validate PROPS' :normalize against DATA (signals like the validator)."
  (financial-chart-multi--prepare data (plist-get props :normalize))
  t)

(defun financial-chart-multi--from-json (data)
  "JSON-parsed DATA ([LABEL, SERIES] pairs) as (LABEL . SERIES) entries."
  (if (listp data)
      (mapcar (lambda (p) (if (and (listp p) (= (length p) 2)) (cons (car p) (cadr p)) p))
              data)
    data))

(defun financial-chart-multi--to-json (data)
  "(LABEL . SERIES) entries as a JSON array of [LABEL, [[X, Y] ...]]."
  (apply #'vector
   (mapcar (lambda (entry)
            (vector (car entry)
                    (apply #'vector
                           (mapcar (lambda (point)
                                     (if (consp point)
                                         (vector (car point)
                                                 (if (consp (cdr point)) (cadr point) (cdr point)))
                                       point))
                                   (append (cdr entry) nil)))))
          data)))

(unless (assq 'multi-series financial-chart-shapes)
  (push '(multi-series
          :doc "A list of (LABEL . SERIES) entries; LABEL is a string or symbol and SERIES uses the series shape."
          :example (("AAPL" . ((1 100) (2 102) (3 101) (4 105)))
                    ("SPY" . ((1 100) (2 99) (3 102) (4 104))))
          :validator financial-chart-multi--validate
          :values financial-chart-multi--values
          :from-json financial-chart-multi--from-json
          :to-json financial-chart-multi--to-json)
        financial-chart-shapes))

(setf (plist-get (alist-get 'multi-series financial-chart-shapes) :example)
      (list (cons "AAPL"
                  (financial-chart--example-series
                   190.0 48 [0.52 -0.2 0.16 -0.35 0.7 -0.12 0.32 -0.46 0.4]))
            (cons "SPY"
                  (financial-chart--example-series
                   450.0 48 [0.4 -0.24 0.28 -0.3 0.52 -0.14 0.3 -0.38 0.2]))
            (cons "QQQ"
                  (financial-chart--example-series
                   380.0 48 [0.64 -0.3 0.14 -0.5 0.72 -0.06 0.35 -0.48 0.46]))))

(financial-chart-register-kind
 'multi :shape 'multi-series
 :text #'financial-chart-text-multi
 :svg #'financial-chart-svg-multi
 :check #'financial-chart-multi--check
 :doc "Shared-scale braille or SVG comparison of named series, optionally rebased.")

(provide 'financial-chart-multi)
;;; financial-chart-multi.el ends here
