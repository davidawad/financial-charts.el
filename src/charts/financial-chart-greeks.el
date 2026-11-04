;;; financial-chart-greeks.el --- Options-book Greeks by strike -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Caller-supplied dollar Greeks for an options book, faceted by expiry.

;;; Code:

(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defun financial-chart-greeks--validate (data)
  "Signal unless DATA is a list of dollar-Greek rows by expiry and strike."
  (unless (proper-list-p data)
    (financial-chart--invalid nil nil "not_a_list" "Greeks data must be a list of row plists"))
  (cl-loop for row in data for i from 0
           do (unless (and (proper-list-p row) (cl-evenp (length row))
                           (keywordp (car row)))
                (financial-chart--invalid i nil "not_a_plist"
                                          "each Greeks row must be a plist"))
           do (dolist (field '(:expiry :strike :delta :gamma :vega :theta))
                (unless (plist-member row field)
                  (financial-chart--invalid i (substring (symbol-name field) 1)
                                            "missing_field" "required field %s is missing" field)))
           do (unless (and (stringp (plist-get row :expiry))
                           (not (string-empty-p (plist-get row :expiry))))
                (financial-chart--invalid i "expiry" "invalid_expiry"
                                          "expiry must be a non-empty date or label string"))
           do (unless (and (financial-chart--finite-p (plist-get row :strike))
                           (> (plist-get row :strike) 0))
                (financial-chart--invalid i "strike" "not_positive"
                                          "strike must be a positive finite number"))
           do (dolist (field '(:delta :gamma :vega :theta))
                (unless (financial-chart--finite-p (plist-get row field))
                  (financial-chart--invalid i (substring (symbol-name field) 1)
                                            "not_a_number"
                                            "%s must be a finite dollar exposure" field))))
  t)

(defun financial-chart-greeks--values (data _props)
  "The four dollar Greek exposures in DATA, for chart summaries."
  (apply #'append
         (mapcar (lambda (row) (mapcar (lambda (key) (plist-get row key))
                                       '(:delta :gamma :vega :theta))) data)))

(defun financial-chart-greeks--from-json (data)
  "JSON rows DATA as plist rows for the `greeks-by-strike' kind."
  (mapcar (lambda (row)
            (list :expiry (alist-get 'expiry row) :strike (alist-get 'strike row)
                  :delta (alist-get 'delta row) :gamma (alist-get 'gamma row)
                  :vega (alist-get 'vega row) :theta (alist-get 'theta row)))
          (append data nil)))

(defun financial-chart-greeks--to-json (data)
  "Greeks DATA as JSON-compatible row objects."
  (vconcat
   (mapcar (lambda (row)
             `((expiry . ,(plist-get row :expiry)) (strike . ,(plist-get row :strike))
               (delta . ,(plist-get row :delta)) (gamma . ,(plist-get row :gamma))
               (vega . ,(plist-get row :vega)) (theta . ,(plist-get row :theta)))) data)))

(add-to-list 'financial-chart-shapes
             '(greeks-by-strike
               :doc "Rows (:expiry STRING :strike POSITIVE-NUMBER :delta DOLLARS :gamma DOLLARS :vega DOLLARS :theta DOLLARS); each Greek is the portfolio's dollar exposure at that expiry and strike. JSON: [{expiry, strike, delta, gamma, vega, theta}]."
               :example ((:expiry "2026-12-18" :strike 95 :delta 1250 :gamma -320 :vega 85 :theta -42)
                         (:expiry "2026-12-18" :strike 100 :delta 2100 :gamma 540 :vega 160 :theta -88)
                         (:expiry "2027-01-15" :strike 100 :delta 740 :gamma 190 :vega 230 :theta -31))
               :validator financial-chart-greeks--validate
               :values financial-chart-greeks--values
               :from-json financial-chart-greeks--from-json
               :to-json financial-chart-greeks--to-json))

(financial-chart-register-kind
 'greeks-by-strike :shape 'greeks-by-strike :template "greeks-by-strike"
 :adapter "greeks-by-strike"
 :doc "Dollar delta, gamma, vega and theta by strike, one panel per expiry.")

(provide 'financial-chart-greeks)
;;; financial-chart-greeks.el ends here
