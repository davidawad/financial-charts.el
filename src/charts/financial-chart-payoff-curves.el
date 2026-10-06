;;; financial-chart-payoff-curves.el --- Multiple payoff curves -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Labeled payoff curves that share one ascending price grid, drawn by
;; eas's payoff-curves template.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defun financial-chart-payoff-curves--invalid (index field code fmt &rest args)
  "Signal invalid curve INDEX: FIELD, CODE and message FMT/ARGS."
  (signal 'financial-chart-invalid-data
          (list (format "curve %d: %s" index (apply #'format fmt args))
                :code code :index index :field field)))

(defun financial-chart-payoff-curves--same-grid-p (a b)
  "Return non-nil when price grids A and B are numerically equal."
  (and (= (length a) (length b))
       (cl-every #'identity (cl-mapcar #'= a b))))

(defun financial-chart-payoff-curves--validate (data)
  "Signal unless DATA is labeled payoffs on the same ascending price grid."
  (unless (listp data)
    (financial-chart--invalid nil nil "not_a_list" "payoff-curves must be a list, got %S" data))
  (let (grid have-grid)
    (cl-loop for curve in data for i from 0
             do (unless (and (consp curve) (stringp (car curve)))
                  (financial-chart-payoff-curves--invalid
                   i "label" "invalid_label" "expected (LABEL . PAYOFF) with a string label"))
             (condition-case err
                 (financial-chart--validate-payoff (cdr curve))
               (financial-chart-invalid-data
                (let ((inner (financial-chart-error-data err)))
                  (financial-chart-payoff-curves--invalid
                   i (format "payoff[%s].%s" (plist-get inner :index) (plist-get inner :field))
                   (plist-get inner :code) "%s" (plist-get inner :message)))))
             (let ((xs (financial-chart-series-xs (cdr curve))))
               (if have-grid
                   (unless (financial-chart-payoff-curves--same-grid-p grid xs)
                     (financial-chart-payoff-curves--invalid
                      i "price" "grid_mismatch" "prices must match the first curve's grid"))
                 (setq grid xs
                       have-grid t))))))

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
 'payoff-curves :shape 'payoff-curves :template "payoff-curves" :adapter "payoff-curves"
 :doc "Overlaid P/L curves sharing a price grid, with a legend and zero line.")

(provide 'financial-chart-payoff-curves)
;;; financial-chart-payoff-curves.el ends here
