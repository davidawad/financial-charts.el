;;; financial-chart-returns.el --- Returns charts for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure transformations and text/SVG renderers for running drawdowns and
;; simple period-return distributions.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)

(defconst financial-chart-returns-default-bins 20
  "Default number of bins in a period-return histogram.")

(defun financial-chart-returns--invalid (index fmt &rest args)
  "Signal `financial-chart-invalid-data' at INDEX with formatted FMT and ARGS."
  (signal 'financial-chart-invalid-data
          (list (format "element %d: %s" index (apply #'format fmt args))
                :code "invalid_data" :index index)))

(defun financial-chart-returns--points (series)
  "Return numeric SERIES observations as plists with :index, :x and :value."
  (financial-chart-validate 'area series)
  (let (points)
    (cl-loop for point across (vconcat series)
             for index from 0
             for value = (financial-chart-series--point-y point)
             when (numberp value)
             do (push (list :index index
                            :x (financial-chart-series--point-x point)
                            :value value)
                      points))
    (nreverse points)))

(defun financial-chart-returns--drawdown-records (series)
  "Return SERIES observations annotated with their running drawdown."
  (let ((peak nil)
        records)
    (dolist (point (financial-chart-returns--points series))
      (let ((value (plist-get point :value))
            (index (plist-get point :index)))
        (when (< value 0)
          (financial-chart-returns--invalid
           index "prices must be nonnegative to calculate drawdown"))
        (unless peak
          (unless (> value 0)
            (financial-chart-returns--invalid
             index "the first price must be positive to establish a high-water mark"))
          (setq peak value))
        (when (> value peak)
          (setq peak value))
        (push (append point (list :drawdown (/ (- value peak) (float peak))))
              records)))
    (nreverse records)))

(defun financial-chart-drawdowns (series)
  "Return running drawdowns for SERIES as (X . FRACTION) points.
X is each supplied series coordinate, or its zero-based source index
when the point has no coordinate.  A drawdown of -0.2 means -20%."
  (mapcar (lambda (point)
            (cons (or (plist-get point :x) (plist-get point :index))
                  (plist-get point :drawdown)))
          (financial-chart-returns--drawdown-records series)))

(defun financial-chart-returns (series)
  "Return simple period returns for consecutive non-nil values in SERIES.
Each result is a fraction: 0.05 means a 5% return.  A zero prior value
signals `financial-chart-invalid-data' because simple return is undefined."
  (let ((points (financial-chart-returns--points series))
        previous
        returns)
    (dolist (point points)
      (let ((value (plist-get point :value))
            (index (plist-get point :index)))
        (when (< value 0)
          (financial-chart-returns--invalid
           index "prices must be nonnegative to calculate simple returns"))
        (when previous
          (when (zerop previous)
            (financial-chart-returns--invalid
             index "the preceding price is zero; simple return is undefined"))
          (push (/ (- value previous) (float previous)) returns))
        (setq previous value)))
    (nreverse returns)))

(defun financial-chart-histogram-bins (returns &optional bins)
  "Split numeric RETURNS into BINS equal-width buckets.
Return a list of (LOWER UPPER COUNT) triples; the final bucket includes
its upper edge.  BINS defaults to `financial-chart-returns-default-bins'."
  (setq bins (or bins financial-chart-returns-default-bins))
  (unless (and (integerp bins) (> bins 0))
    (signal 'financial-chart-invalid-data
            (list "bins must be a positive integer"
                  :code "invalid_data")))
  (unless (or (listp returns) (vectorp returns))
    (signal 'financial-chart-invalid-data
            (list "returns must be a list or vector" :code "invalid_data")))
  (let ((values (append returns nil)))
    (cl-loop for value in values for index from 0
             unless (numberp value)
             do (financial-chart-returns--invalid index "expected a numeric return"))
    (when values
      (let* ((low (apply #'min values))
             (high (apply #'max values))
             (counts (make-vector bins 0)))
        (if (= low high)
            (aset counts (/ bins 2) (length values))
          (let ((span (- high low)))
            (dolist (value values)
              (let ((index (min (1- bins)
                                (floor (* bins (/ (- value low) (float span)))))))
                (cl-incf (aref counts index))))))
        (cl-loop for index from 0 below bins
                 for bin-low = (if (= low high) low
                                 (+ low (* (- high low) (/ index (float bins)))))
                 for bin-high = (if (= low high) high
                                  (+ low (* (- high low) (/ (1+ index) (float bins)))))
                 collect (list bin-low bin-high (aref counts index)))))))

(defun financial-chart-returns--statistics (returns)
  "Return (MEAN . SAMPLE-STDEV) of RETURNS; a lone value has stdev zero."
  (when returns
    (let* ((n (length returns))
           (mean (/ (apply #'+ returns) (float n)))
           (variance (if (< n 2) 0.0
                       (/ (cl-loop for value in returns
                                   sum (expt (- value mean) 2))
                          (float (1- n)))))
           (stdev (sqrt variance)))
      (cons mean stdev))))

(defun financial-chart-returns--percent (value &optional signed)
  "Format fractional VALUE as a percentage, with a sign when SIGNED."
  (let ((text (financial-chart-fmt (* value 100))))
    (concat (if (and signed (> value 0)) "+" "") text "%")))

(defun financial-chart-returns--location (point)
  "Human-readable date/coordinate or source index for drawdown POINT."
  (let ((x (plist-get point :x)))
    (cond
     ((null x) (format "index %d" (plist-get point :index)))
     ((and (numberp x) (>= (abs x) 1000000000))
      (format-time-string "%Y-%m-%d"
                          (/ x (if (> (abs x) 100000000000) 1000.0 1.0)) t))
     ((numberp x) (format "index %s" x))
     (t (format "%s" x)))))

(cl-defun financial-chart-text-drawdown
    (series &key (width 60) (height 12) (label-width 7)
            (down-face 'financial-chart-down)
            (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
            &allow-other-keys)
  "Render SERIES as a running percent drawdown chart.
The high-water mark is 0% at the top and drawdowns extend below it.
WIDTH/HEIGHT are plot columns/rows.  The footer names the largest
drawdown and its date/coordinate, or its zero-based source index."
  (let* ((records (financial-chart-returns--drawdown-records series))
         (width (max 1 (truncate width)))
         (height (max 1 (truncate height)))
         (worst (car records)))
    (when records
      (dolist (point (cdr records))
        (when (< (plist-get point :drawdown) (plist-get worst :drawdown))
          (setq worst point)))
      (let* ((drawdowns
              (mapcar (lambda (point)
                        (cons (or (plist-get point :x) (plist-get point :index))
                              (* 100 (plist-get point :drawdown))))
                      records))
             (values (mapcar #'cdr drawdowns))
             (flat (cl-every #'zerop values))
             (plot
              (if flat
                  (apply
                   #'concat
                   (cl-loop for row from (1- height) downto 0
                            collect
                            (concat
                             (financial-chart-text--label
                              (if (or (= row (1- height)) (= row 0)) "0%" "")
                              label-width dim-face)
                             (propertize
                              (if (= row (1- height))
                                  (make-string width ?⠉)
                                (make-string width ?\s))
                              'face down-face)
                             "\n")))
                (financial-chart-text-line
                 drawdowns :width width :height height :label-width label-width
                 :unit "%" :footer nil :up-face down-face :down-face down-face
                 :dim-face dim-face :accent-face accent-face)))
             (footer
              (concat (financial-chart-text--label "" label-width dim-face)
                      (propertize
                       (format "max drawdown %s at %s"
                               (financial-chart-returns--percent
                                (plist-get worst :drawdown))
                               (financial-chart-returns--location worst))
                       'face accent-face))))
        (concat plot "\n" footer)))))

(cl-defun financial-chart-text-histogram
    (series &key (width 60) (height 10)
            (bins financial-chart-returns-default-bins) (label-width 5)
            (up-face 'financial-chart-up) (dim-face 'financial-chart-dim)
            (accent-face 'financial-chart-accent) &allow-other-keys)
  "Render the distribution of consecutive simple returns in SERIES.
BINS is the number of histogram intervals.  The footer gives mean, sample
standard deviation, and observation count."
  (let* ((returns (financial-chart-returns series))
         (histogram (financial-chart-histogram-bins returns bins))
         (stats (financial-chart-returns--statistics returns)))
    (when histogram
      (let* ((width (max 1 (truncate width)))
             (height (max 1 (truncate height)))
             (bin-count (length histogram))
             (counts (mapcar #'caddr histogram))
             (max-count (max 1 (apply #'max counts)))
             (columns
              (if (>= width bin-count)
                  (cl-loop for column from 0 below width
                           for bin = (min (1- bin-count)
                                          (floor (* column (/ (float bin-count) width))))
                           collect (nth bin counts))
                (let ((grouped (make-vector width 0)))
                  (cl-loop for count in counts for index from 0
                           for column = (min (1- width)
                                             (floor (* index (/ (float width) bin-count))))
                           do (cl-incf (aref grouped column) count))
                  (append grouped nil))))
             (rows
              (cl-loop for row from (1- height) downto 0
                       for floor-cells = (* row 8)
                       collect
                       (concat
                        (financial-chart-text--label
                         (if (= row (1- height)) (number-to-string max-count)
                           (if (= row 0) "0" "")) label-width dim-face)
                        (propertize
                         (mapconcat
                          (lambda (count)
                            (let ((level
                                   (max 0 (min (* height 8)
                                               (round (* height 8
                                                         (/ count (float max-count))))))))
                              (string (aref financial-chart-blocks
                                            (max 0 (min 8 (- level floor-cells)))))))
                          columns "")
                         'face up-face)
                        "\n")))
             (low (caar histogram))
             (high (cadr (car (last histogram))))
             (low-label (financial-chart-returns--percent low))
             (high-label (financial-chart-returns--percent high))
             (x-axis
              (propertize
               (concat (make-string (1+ label-width) ?\s) low-label
                       (make-string (max 1 (- width (length low-label)
                                              (length high-label))) ?\s)
                       high-label)
               'face dim-face))
             (footer
              (concat (financial-chart-text--label "" label-width dim-face)
                      (propertize
                       (format "mean %s"
                               (financial-chart-returns--percent (car stats) t))
                       'face accent-face)
                      (propertize
                       (format "   stdev %s   n %d"
                               (financial-chart-returns--percent (cdr stats))
                               (length returns))
                       'face dim-face))))
        (concat (apply #'concat rows) "\n" x-axis "\n" footer)))))

(cl-defun financial-chart-svg-drawdown
    (series &key (width 600) (height 260) title &allow-other-keys)
  "Return an SVG running drawdown chart for SERIES, with 0% at the top."
  (let ((records (financial-chart-returns--drawdown-records series)))
    (when records
      (let* ((drawdowns (mapcar (lambda (point) (plist-get point :drawdown)) records))
             (low (apply #'min drawdowns))
             (display-low (if (= low 0) -0.01 low)))
        (pcase-let* ((`(,x0 ,y0 ,w ,h) (financial-chart-svg--frame width height title))
                     (xmax (max 1 (1- (length records))))
                     (sx (lambda (index)
                           (financial-chart-svg--n (+ x0 (* w (/ index (float xmax)))))))
                     (sy (lambda (value)
                           (financial-chart-svg--n
                            (+ y0 (* h (/ (- value) (- display-low)))))))
                     (points (cl-loop for point in records for index from 0
                                      collect (cons (funcall sx index)
                                                    (funcall sy (plist-get point :drawdown)))))
                     (svg (financial-chart-svg--canvas width height title))
                     (worst (car records)))
          (dolist (point (cdr records))
            (when (< (plist-get point :drawdown) (plist-get worst :drawdown))
              (setq worst point)))
          (svg-polygon svg (append (list (cons x0 y0)) points
                                   (list (cons (+ x0 w) y0)))
                       :fill (financial-chart-svg--color 'down)
                       :fill-opacity 0.14 :stroke "none")
          (svg-line svg x0 y0 (+ x0 w) y0
                    :stroke (financial-chart-svg--color 'grid)
                    :stroke-dasharray "4 3")
          (svg-polyline svg points :fill "none"
                        :stroke (financial-chart-svg--color 'down) :stroke-width 1.5)
          (financial-chart-svg--text svg "0%" (- x0 6) (+ y0 4) "end")
          (financial-chart-svg--text svg
                                     (financial-chart-returns--percent low)
                                     (- x0 6) (+ y0 h) "end")
          (financial-chart-svg--text
           svg (format "max drawdown %s at %s"
                       (financial-chart-returns--percent (plist-get worst :drawdown))
                       (financial-chart-returns--location worst))
           (+ x0 w) (+ y0 h 18) "end")
          (financial-chart-svg--string svg))))))

(cl-defun financial-chart-svg-histogram
    (series &key (width 600) (height 260)
            (bins financial-chart-returns-default-bins) title &allow-other-keys)
  "Return an SVG histogram of simple period returns in SERIES."
  (let* ((returns (financial-chart-returns series))
         (histogram (financial-chart-histogram-bins returns bins))
         (stats (financial-chart-returns--statistics returns)))
    (when histogram
      (pcase-let* ((`(,x0 ,y0 ,w ,plot-height) (financial-chart-svg--frame width height title))
                   (h (max 1 (- plot-height 20)))
                   (low (caar histogram))
                   (high (cadr (car (last histogram))))
                   (span (if (= low high) 1.0 (- high low)))
                   (count-max (max 1 (apply #'max (mapcar #'caddr histogram))))
                   (svg (financial-chart-svg--canvas width height title)))
        (cl-loop for (bin-low bin-high count) in histogram for index from 0
                 for bar-x = (if (= low high)
                                 (+ x0 (* w (/ (+ index 0.25) (float (length histogram)))))
                               (+ x0 (* w (/ (- bin-low low) span))))
                 for bar-end = (if (= low high)
                                   (+ x0 (* w (/ (+ index 0.75) (float (length histogram)))))
                                 (+ x0 (* w (/ (- bin-high low) span))))
                 for bar-y = (+ y0 (* h (- 1 (/ count (float count-max)))))
                 do (svg-rectangle svg bar-x bar-y (max 0.5 (- bar-end bar-x))
                                   (- (+ y0 h) bar-y)
                                   :fill (financial-chart-svg--color 'up)))
        (when (and (< low 0) (> high 0))
          (let ((zero-x (+ x0 (* w (/ (- low) span)))))
            (svg-line svg zero-x y0 zero-x (+ y0 h)
                      :stroke (financial-chart-svg--color 'grid)
                      :stroke-dasharray "3 3")))
        (svg-line svg x0 (+ y0 h) (+ x0 w) (+ y0 h)
                  :stroke (financial-chart-svg--color 'grid))
        (financial-chart-svg--text svg (financial-chart-returns--percent low)
                                   x0 (+ y0 h 14) "start")
        (financial-chart-svg--text svg (financial-chart-returns--percent high)
                                   (+ x0 w) (+ y0 h 14) "end")
        (financial-chart-svg--text
         svg (format "mean %s   stdev %s   n %d"
                     (financial-chart-returns--percent (car stats) t)
                     (financial-chart-returns--percent (cdr stats))
                     (length returns))
         (+ x0 w) (+ y0 h 28) "end")
        (financial-chart-svg--string svg)))))

(financial-chart-register-kind
 'drawdown :shape 'series :text #'financial-chart-text-drawdown
 :svg #'financial-chart-svg-drawdown
 :doc "Running percent decline from the high-water mark, with maximum drawdown.")

(financial-chart-register-kind
 'histogram :shape 'series :text #'financial-chart-text-histogram
 :svg #'financial-chart-svg-histogram
 :doc "Distribution of consecutive simple returns with mean and sample deviation.")

(provide 'financial-chart-returns)
;;; financial-chart-returns.el ends here
