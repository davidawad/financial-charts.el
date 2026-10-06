;;; financial-chart-multi.el --- Multi-series chart kind for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Compare named series on a shared scale.  Optional normalization rebases
;; each series to the same starting value, which is useful for comparing
;; ticker performance.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-series)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defun financial-chart-multi--prepare (data normalize)
  "Return DATA as (LABEL . VALUES) entries, rebased to NORMALIZE when set."
  (unless (or (null normalize) (numberp normalize))
    (financial-chart--invalid nil "normalize" "invalid_normalize" ":normalize must be nil or a number"))
  (cl-loop for (label . series) in data
           for index from 0
           for values = (financial-chart-series-values series)
           collect
           (cons label
                 (if (and normalize values)
                     (let ((base (car values)))
                       (when (zerop base)
                         (financial-chart--invalid
                          index "normalize" "zero_base"
                          "series %S starts at zero; choose another base before normalizing" label))
                       (mapcar (lambda (value)
                                 (* normalize (/ (float value) base)))
                               values))
                   values))))

(defun financial-chart-multi--validate (data)
  "Signal unless DATA is a list of (LABEL . SERIES) entries."
  (unless (and (listp data) (integerp (proper-list-p data)))
    (financial-chart--invalid nil nil "not_a_list" "multi-series data must be a proper list"))
  (cl-loop for entry in data
           for index from 0
           do (unless (and (consp entry)
                           (or (stringp (car entry)) (symbolp (car entry)))
                           (or (listp (cdr entry)) (vectorp (cdr entry))))
                (financial-chart--invalid index "label" "invalid_label"
                                          "expected (LABEL . SERIES), with a string or symbol LABEL; got %S"
                                          entry))
           do (condition-case err
                  (financial-chart--validate-series (cdr entry))
                (financial-chart-invalid-data
                 (let ((inner (financial-chart-error-data err)))
                   (financial-chart--invalid
                    index (format "series[%s].%s" (plist-get inner :index) (plist-get inner :field))
                    (plist-get inner :code) "series %S is invalid: %s"
                    (car entry) (plist-get inner :message))))))
  t)

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

;; The example is already rebased to 100 (what `:normalize 100' does to
;; raw prices), so the comparison is visible; series at very different
;; price levels plot as flat lines on a shared scale.
(setf (plist-get (alist-get 'multi-series financial-chart-shapes) :example)
      (list (cons "AAPL"
                  (financial-chart--example-series
                   100.0 48 [0.52 -0.2 0.16 -0.35 0.7 -0.12 0.32 -0.46 0.4]))
            (cons "SPY"
                  (financial-chart--example-series
                   100.0 48 [0.4 -0.24 0.28 -0.3 0.52 -0.14 0.3 -0.38 0.2]))
            (cons "QQQ"
                  (financial-chart--example-series
                   100.0 48 [0.64 -0.3 0.14 -0.5 0.72 -0.06 0.35 -0.48 0.46]))))

(financial-chart-register-kind
 'multi :shape 'multi-series :template "multi" :adapter "multi-series"
 :props '((:normalize . :normalize))
 :check #'financial-chart-multi--check
 :doc "Shared-scale comparison of named series, optionally rebased.")

(provide 'financial-chart-multi)
;;; financial-chart-multi.el ends here
