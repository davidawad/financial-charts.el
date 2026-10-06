;;; financial-chart-matrix.el --- Matrix and volume-profile charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Heatmaps for labeled numeric matrices, and OHLCV volume profiles
;; (`financial-chart-volume-profile' spreads each bar's volume evenly
;; across the price bins its low-high range touches; an OHLCV
;; approximation, not trade-level data), both drawn by eas templates.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defun financial-chart-matrix--sequence-p (value)
  "Whether VALUE is a finite list or a non-string vector."
  (or (proper-list-p value)
      (and (vectorp value) (not (stringp value)))))

(defun financial-chart-matrix--validate (data)
  "Signal unless DATA is a labeled rectangular numeric matrix."
  (unless (and (proper-list-p data) (cl-evenp (length data))
               (plist-member data :labels) (plist-member data :rows))
    (financial-chart--invalid nil nil "not_a_matrix" "expected (:labels LABELS :rows ROWS)"))
  (let ((labels (plist-get data :labels))
        (column-labels (plist-get data :column-labels))
        (rows (plist-get data :rows)))
    (unless (and (financial-chart-matrix--sequence-p labels)
                 (> (length labels) 0))
      (financial-chart--invalid nil "labels" "not_a_list" ":labels must be a non-empty list or vector"))
    (cl-loop for label in (append labels nil)
             for label-index from 0
             unless (or (stringp label) (symbolp label) (numberp label))
             do (financial-chart--invalid label-index "labels" "invalid_label"
                                          "matrix labels must be strings, symbols or numbers"))
    (unless (and (financial-chart-matrix--sequence-p rows)
                 (= (length rows) (length labels)))
      (financial-chart--invalid nil "rows" "row_count" ":rows must contain one row per label"))
    (unless (and (financial-chart-matrix--sequence-p (car (append rows nil)))
                 (> (length (car (append rows nil))) 0))
      (financial-chart--invalid 0 "rows" "empty_row" "matrix rows must contain at least one numeric value"))
    (let ((column-count (length (car (append rows nil)))))
      (when column-labels
        (unless (and (financial-chart-matrix--sequence-p column-labels)
                     (= (length column-labels) column-count))
          (financial-chart--invalid nil "column-labels" "column_count" ":column-labels must contain one label per column"))
        (cl-loop for label in (append column-labels nil)
                 for label-index from 0
                 unless (or (stringp label) (symbolp label) (numberp label))
                 do (financial-chart--invalid label-index "column-labels" "invalid_label"
                                              "column labels must be strings, symbols or numbers")))
    (cl-loop for row in (append rows nil)
             for row-index from 0
             do
      (unless (and (financial-chart-matrix--sequence-p row)
                   (= (length row) column-count))
        (financial-chart--invalid row-index "rows" "column_count" "row must contain %d values" column-count))
      (cl-loop for value in (append row nil)
               for column-index from 0
               do
        (unless (numberp value)
          (financial-chart--invalid row-index (format "rows[%d]" column-index) "not_a_number"
                                    "cell %d must be numeric, got %S" column-index value)))))))

(defun financial-chart-matrix--values (data _props)
  "Every cell value in matrix DATA, for summaries."
  (apply #'append (mapcar (lambda (row) (append row nil))
                          (append (plist-get data :rows) nil))))

(defun financial-chart-matrix--from-json (data)
  "JSON-parsed matrix DATA (labels, rows, column_labels) as a matrix plist."
  (append (list :labels (alist-get 'labels data))
          (when (alist-get 'column_labels data)
            (list :column-labels (alist-get 'column_labels data)))
          (list :rows (alist-get 'rows data))))

(defun financial-chart-matrix--to-json (data)
  "Matrix DATA as a JSON object."
  (append `((labels . ,(vconcat (plist-get data :labels)))
            (rows . ,(vconcat (mapcar #'vconcat (plist-get data :rows)))))
          (when (plist-get data :column-labels)
            `((column_labels . ,(vconcat (plist-get data :column-labels)))))))

(setf (alist-get 'matrix financial-chart-shapes)
      '(:doc "Numeric matrix: (:labels (ROW-LABEL ...) :rows ((VALUE ...) ...));
optional :column-labels names columns, otherwise square matrices reuse :labels.
JSON: {\"labels\": [...], \"rows\": [[...], ...], \"column_labels\": [...]}."
        :example (:labels ("SPY" "QQQ" "TLT" "GLD" "USO")
                  :rows ((1.0 0.84 -0.26 0.03 0.22)
                         (0.84 1.0 -0.18 -0.04 0.17)
                         (-0.26 -0.18 1.0 0.12 -0.31)
                         (0.03 -0.04 0.12 1.0 0.14)
                         (0.22 0.17 -0.31 0.14 1.0)))
        :validator financial-chart-matrix--validate
        :values financial-chart-matrix--values
        :from-json financial-chart-matrix--from-json
        :to-json financial-chart-matrix--to-json))

(defun financial-chart-matrix--check-bins (data props)
  "Validate PROPS' :bins for a volume profile of DATA."
  (financial-chart-volume-profile data (or (plist-get props :bins) 24))
  t)

(financial-chart-register-kind
 'heatmap :shape 'matrix :template "heatmap" :adapter "matrix"
 :doc "Diverging heatmap for a labeled numeric matrix.")
(financial-chart-register-kind
 'volume-profile :shape 'ohlc :template "volume-profile" :slot :bars :props '((:bins . :bins))
 :check #'financial-chart-matrix--check-bins
 :doc "Estimated OHLCV volume by price level, with point of control and last close.")

(provide 'financial-chart-matrix)
;;; financial-chart-matrix.el ends here
