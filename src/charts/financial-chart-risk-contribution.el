;;; financial-chart-risk-contribution.el --- Portfolio risk contribution bars -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Signed Euler risk contributions, grouped by position name, asset class
;; or risk factor and drawn with the diverging-bars template.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)
(require 'financial-chart-validate)
(require 'financial-chart-eas)
(require 'eas)

(defun financial-chart-risk-contribution--validate (data)
  "Signal unless DATA is a portfolio total and signed contribution rows."
  (unless (and (proper-list-p data) (cl-evenp (length data))
               (keywordp (car data)) (plist-member data :portfolio-vol)
               (plist-member data :contributions))
    (financial-chart--invalid nil nil "not_a_risk_contribution"
                              "expected (:portfolio-vol NUMBER :contributions ROWS)"))
  (let ((total (plist-get data :portfolio-vol))
        (rows (plist-get data :contributions))
        (sum 0))
    (unless (financial-chart--finite-p total)
      (financial-chart--invalid nil "portfolio-vol" "not_a_number"
                                ":portfolio-vol must be a finite number"))
    (unless (proper-list-p rows)
      (financial-chart--invalid nil "contributions" "not_a_list"
                                ":contributions must be a list of row plists"))
    (cl-loop for row in rows for i from 0
             do (unless (and (proper-list-p row) (cl-evenp (length row))
                             (keywordp (car row)))
                  (financial-chart--invalid i nil "not_a_plist"
                                            "each contribution must be a plist with :name, :class, :factor and :contribution"))
             do (dolist (field '(:name :class :factor))
                  (let ((value (plist-get row field)))
                    (unless (and (plist-member row field)
                                 (or (stringp value) (symbolp value)))
                      (financial-chart--invalid i (substring (symbol-name field) 1)
                                                "invalid_label"
                                                "%s must be a string or symbol label"
                                                field))))
             do (let ((value (plist-get row :contribution)))
                  (unless (and (plist-member row :contribution)
                               (financial-chart--finite-p value))
                    (financial-chart--invalid i "contribution" "not_a_number"
                                              ":contribution must be a finite signed number"))
                  (setq sum (+ sum value))))
    (unless (<= (abs (- sum total)) (* 1e-8 (max 1 (abs total))))
      (financial-chart--invalid nil "portfolio-vol" "contributions_do_not_sum"
                                "signed contributions sum to %s, not portfolio volatility %s; correct the rows or total"
                                sum total)))
  t)

(defun financial-chart-risk-contribution--values (data _props)
  "Every signed contribution in DATA, for explain and SVG provenance."
  (mapcar (lambda (row) (plist-get row :contribution))
          (plist-get data :contributions)))

(defun financial-chart-risk-contribution--check (data props)
  "Check DATA's grouping property in PROPS."
  (let ((by (or (plist-get props :by) 'name)))
    (unless (memq by '(name class factor))
      (financial-chart--invalid nil "by" "invalid_group"
                                ":by must be name, class or factor, got %S" by))))

(defun financial-chart-risk-contribution--bindings (data props)
  "Aggregate DATA by PROPS' :by and lower it through the labeled adapter."
  (let* ((by (or (plist-get props :by) 'name))
         (field (intern (concat ":" (symbol-name by))))
         (groups (make-hash-table :test #'equal))
         labels)
    (dolist (row (plist-get data :contributions))
      (let* ((label (plist-get row field))
             (key (format "%s" label)))
        (unless (gethash key groups) (push key labels))
        (puthash key (+ (gethash key groups 0) (plist-get row :contribution)) groups)))
    (list :data (eas-data-rows
                 (eas-data-from "labeled"
                                (mapcar (lambda (label)
                                          (cons label (gethash label groups)))
                                        (nreverse labels))))
          :x_title "portfolio volatility contribution")))

(setf (alist-get 'risk-contributions financial-chart-shapes)
      '(:doc "(:portfolio-vol NUMBER :contributions ((:name LABEL :class LABEL :factor LABEL :contribution SIGNED-NUMBER) ...)); the signed contributions sum to portfolio volatility."
        :example (:portfolio-vol 0.12
                  :contributions ((:name "AAPL" :class "equity" :factor "market" :contribution 0.08)
                                  (:name "TLT" :class "rates" :factor "duration" :contribution 0.06)
                                  (:name "GLD" :class "commodity" :factor "market" :contribution -0.02)))
        :validator financial-chart-risk-contribution--validate
        :values financial-chart-risk-contribution--values))

(financial-chart-register-kind
 'risk-contribution :shape 'risk-contributions :template "diverging-bars"
 :bindings #'financial-chart-risk-contribution--bindings
 :check #'financial-chart-risk-contribution--check
 :doc "Signed portfolio volatility contributions grouped by position name, asset class or risk factor.")

(provide 'financial-chart-risk-contribution)
;;; financial-chart-risk-contribution.el ends here
