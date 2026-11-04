;;; financial-chart-frontier.el --- Efficient-frontier chart kind -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Caller-supplied risk/return points for an efficient frontier, candidate
;; construction methods and the current portfolio.

;;; Code:

(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defun financial-chart-frontier--invalid (index field code fmt &rest args)
  "Signal invalid frontier row INDEX, FIELD, CODE and formatted message."
  (apply #'financial-chart--invalid index field code fmt args))

(defun financial-chart-frontier--validate (data)
  "Validate DATA, a list of frontier, method and current portfolio rows."
  (unless (and (proper-list-p data) data)
    (financial-chart-frontier--invalid nil nil "not_a_list"
                                       "frontier data must be a nonempty list of point plists"))
  (let ((frontier-count 0) (method-count 0) (current-count 0) previous-risk)
    (cl-loop for row in data for index from 0
             do (unless (and (proper-list-p row) (cl-evenp (length row))
                             (keywordp (car row))
                             (cl-every #'keywordp
                                       (cl-loop for (key _value) on row by #'cddr collect key)))
                  (financial-chart-frontier--invalid index nil "not_a_plist"
                                                     "each frontier point must be a plist"))
             do (let ((kind (plist-get row :kind)))
                  (unless (memq kind '(:frontier :method :current))
                    (financial-chart-frontier--invalid index "kind" "invalid_kind"
                                                       ":kind must be :frontier, :method or :current"))
                  (dolist (field '(:risk :return))
                    (unless (plist-member row field)
                      (financial-chart-frontier--invalid index (substring (symbol-name field) 1)
                                                         "missing_field" "%s is required" field))
                    (unless (financial-chart--finite-p (plist-get row field))
                      (financial-chart-frontier--invalid index (substring (symbol-name field) 1)
                                                         "not_a_number" "%s must be a finite number" field)))
                  (pcase kind
                    (:frontier
                     (cl-incf frontier-count)
                     (when (and previous-risk (< (plist-get row :risk) previous-risk))
                       (financial-chart-frontier--invalid index "risk" "risk_not_ascending"
                                                          "frontier risk must ascend"))
                     (setq previous-risk (plist-get row :risk)))
                    ((or :method :current)
                     (unless (and (plist-member row :label) (stringp (plist-get row :label))
                                  (not (string-empty-p (plist-get row :label))))
                       (financial-chart-frontier--invalid index "label" "invalid_label"
                                                          "method and current points need a nonempty string label"))
                     (if (eq kind :method) (cl-incf method-count) (cl-incf current-count)))))
             do (dolist (key (cl-loop for (key _value) on row by #'cddr collect key))
                  (unless (memq key '(:kind :risk :return :label))
                    (financial-chart-frontier--invalid index (substring (symbol-name key) 1)
                                                       "unknown_field" "unsupported field %s" key))))
    (unless (>= frontier-count 2)
      (financial-chart-frontier--invalid nil "frontier" "too_few_points"
                                         "the efficient frontier needs at least two points"))
    (unless (> method-count 0)
      (financial-chart-frontier--invalid nil "method" "missing_methods"
                                         "supply at least one construction method point"))
    (unless (= current-count 1)
      (financial-chart-frontier--invalid nil "current" "current_count"
                                         "supply exactly one current portfolio point"))
    t))

(defun financial-chart-frontier--values (data _props)
  "Expected-return values in frontier DATA for explain summaries."
  (mapcar (lambda (row) (plist-get row :return)) data))

(add-to-list 'financial-chart-shapes
             '(frontier
               :doc "Point plists (:kind :frontier|:method|:current :risk NUMBER :return NUMBER [:label STRING]); frontier rows ascend by risk, with at least two points, one or more method points and exactly one current portfolio point. Risk and return use percent units."
               :example ((:kind :frontier :risk 2.0 :return 4.0)
                         (:kind :frontier :risk 5.0 :return 8.0)
                         (:kind :frontier :risk 9.0 :return 11.0)
                         (:kind :method :risk 4.0 :return 6.0 :label "Minimum variance")
                         (:kind :method :risk 7.0 :return 9.0 :label "Maximum Sharpe")
                         (:kind :current :risk 6.0 :return 5.5 :label "Current portfolio"))
               :validator financial-chart-frontier--validate
               :values financial-chart-frontier--values))

(financial-chart-register-kind
 'frontier :shape 'frontier :template "efficient-frontier" :adapter "frontier"
 :doc "Efficient frontier with labeled construction methods and current portfolio.")

(provide 'financial-chart-frontier)
;;; financial-chart-frontier.el ends here
