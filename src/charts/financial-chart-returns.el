;;; financial-chart-returns.el --- Returns charts for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Running drawdowns and simple period-return distributions: the pure
;; transformations, and the drawdown and histogram kinds drawn by eas.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defconst financial-chart-returns-default-bins 20
  "Default number of bins in a period-return histogram.")

(defun financial-chart-returns--invalid (index code fmt &rest args)
  "Signal `financial-chart-invalid-data' at INDEX, field y: CODE, FMT and ARGS."
  (apply #'financial-chart--invalid index "y" code fmt args))

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
           index "negative_price" "prices must be nonnegative to calculate drawdown"))
        (unless peak
          (unless (> value 0)
            (financial-chart-returns--invalid
             index "nonpositive_start" "the first price must be positive to establish a high-water mark"))
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
           index "negative_price" "prices must be nonnegative to calculate simple returns"))
        (when previous
          (when (zerop previous)
            (financial-chart-returns--invalid
             index "zero_price" "the preceding price is zero; simple return is undefined"))
          (push (/ (- value previous) (float previous)) returns))
        (setq previous value)))
    (nreverse returns)))

(defun financial-chart-histogram-bins (returns &optional bins)
  "Split numeric RETURNS into BINS equal-width buckets.
Return a list of (LOWER UPPER COUNT) triples; the final bucket includes
its upper edge.  BINS defaults to `financial-chart-returns-default-bins'."
  (setq bins (or bins financial-chart-returns-default-bins))
  (unless (and (integerp bins) (> bins 0))
    (financial-chart--invalid nil "bins" "invalid_bins" "bins must be a positive integer"))
  (unless (or (listp returns) (vectorp returns))
    (financial-chart--invalid nil nil "not_a_list" "returns must be a list or vector"))
  (let ((values (append returns nil)))
    (cl-loop for value in values for index from 0
             unless (numberp value)
             do (financial-chart-returns--invalid index "not_a_number" "expected a numeric return"))
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

(financial-chart-register-kind
 'drawdown :shape 'series :template "drawdown" :adapter "series"
 :bindings #'financial-chart--series-bindings
 :check (lambda (data _props) (financial-chart-drawdowns data) t)
 :doc "Running percent decline from the high-water mark, with maximum drawdown.")

(financial-chart-register-kind
 'histogram :shape 'series :template "histogram" :adapter "series" :props '((:bins . :bins))
 :check (lambda (data _props) (financial-chart-returns data) t)
 :doc "Distribution of consecutive simple returns with mean and sample deviation.")

(provide 'financial-chart-returns)
;;; financial-chart-returns.el ends here
