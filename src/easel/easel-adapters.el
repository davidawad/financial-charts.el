;;; easel-adapters.el --- json, csv, tsv and bar/v1 adapters; appends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L0 adapters beyond plist rows.  Text adapters accept the text itself,
;; a file name, "-" for stdin, or (:file F) / (:text S) to be explicit.
;; `easel-data-append' is the streaming contract: live data and batch
;; data go through the same schema check.

;;; Code:

(require 'easel-data)

(defun easel-adapters--text (input)
  "Return the text INPUT designates: itself, a file's contents or stdin."
  (cond
   ((and (easel-object-p input) (plist-get input :text)) (plist-get input :text))
   ((and (easel-object-p input) (plist-get input :file))
    (easel-adapters--file (plist-get input :file)))
   ((equal input "-") (easel-adapters--file "/dev/stdin"))
   ((and (stringp input) (not (string-match-p "[\n{[]" input)) (file-exists-p input))
    (easel-adapters--file input))
   ((stringp input) input)
   (t (easel-shape-invalid "Give text, a file name, \"-\", (:file F) or (:text S)" nil))))

(defun easel-adapters--file (file)
  "Return FILE's contents or signal NOT_FOUND."
  (unless (file-readable-p file)
    (easel-signal "NOT_FOUND" (format "No readable file %s" file) :path file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; json

(defun easel-adapters--json (input)
  "Convert JSON INPUT (rows array, {\"values\": rows} or data/v1) to data/v1."
  (let ((value (if (or (vectorp input) (and (easel-object-p input) (plist-get input :values)))
                   input
                 (easel-json-parse (easel-adapters--text input)))))
    (cond
     ((vectorp value) (easel-data-from "plist" value))
     ((easel-data-p value) (easel-data-from "data/v1" value))
     ((vectorp (plist-get value :values)) (easel-data-from "plist" (plist-get value :values)))
     (t (easel-shape-invalid
         "JSON data must be an array of row objects, {\"values\": [...]} or data/v1" nil)))))

(easel-register-adapter
 "json" :doc "JSON text, file or stdin: an array of rows, {\"values\": rows} or data/v1."
 :convert #'easel-adapters--json
 :example "[{\"x\": 1, \"y\": 2}, {\"x\": 2, \"y\": 3}]")

;;; csv / tsv

(defconst easel-adapters--number-regexp
  "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "A cell that reads as a number.")

(defun easel-adapters-parse-delimited (text separator)
  "Parse TEXT into a list of records (lists of strings) split on SEPARATOR.
Double-quoted cells may contain SEPARATOR, newlines and \"\" escapes."
  (let ((records nil) (record nil) (cell nil) (i 0) (n (length text)) (quoted nil))
    (cl-flet ((end-cell () (push (apply #'string (nreverse cell)) record) (setq cell nil))
              (end-record () (push (nreverse record) records) (setq record nil)))
      (while (< i n)
        (let ((c (aref text i)))
          (cond
           (quoted
            (cond ((and (eq c ?\") (< (1+ i) n) (eq (aref text (1+ i)) ?\"))
                   (push ?\" cell) (setq i (1+ i)))
                  ((eq c ?\") (setq quoted nil))
                  (t (push c cell))))
           ((and (eq c ?\") (null cell)) (setq quoted t))
           ((eq c separator) (end-cell))
           ((eq c ?\r))
           ((eq c ?\n) (end-cell) (end-record))
           (t (push c cell))))
        (setq i (1+ i)))
      (when (or cell record) (end-cell) (end-record)))
    (seq-remove (lambda (r) (equal r '(""))) (nreverse records))))

(defun easel-adapters--cell (text)
  "Return cell TEXT as a number, `:null' when empty, else the string."
  (cond ((string-empty-p text) :null)
        ((string-match-p easel-adapters--number-regexp text) (string-to-number text))
        (t text)))

(defun easel-adapters--delimited (input separator)
  "Convert delimited INPUT with a header row, split on SEPARATOR, to data/v1."
  (let* ((records (easel-adapters-parse-delimited (easel-adapters--text input) separator))
         (header (car records))
         (keys (mapcar #'easel-key header))
         (index 0) rows)
    (unless header (easel-shape-invalid "Delimited data needs a header row" nil))
    (dolist (record (cdr records))
      (unless (= (length record) (length keys))
        (easel-shape-invalid
         (format "Row %d has %d cells but the header has %d; quote cells containing the separator"
                 index (length record) (length keys))
         index))
      (push (cl-loop for key in keys for cell in record
                     append (list key (easel-adapters--cell cell)))
            rows)
      (setq index (1+ index)))
    (easel-data-make (vconcat (nreverse rows)))))

(easel-register-adapter
 "csv" :doc "Comma-separated text, file or stdin with a header row; numeric cells become numbers."
 :convert (lambda (input) (easel-adapters--delimited input ?,))
 :example "x,y\n1,2\n2,3\n")

(easel-register-adapter
 "tsv" :doc "Tab-separated text, file or stdin with a header row."
 :convert (lambda (input) (easel-adapters--delimited input ?\t))
 :example "x\ty\n1\t2\n2\t3\n")

;;; bar/v1

(defconst easel-adapters--bar-schema
  [(:name "time" :type "temporal") (:name "open" :type "quantitative")
   (:name "high" :type "quantitative") (:name "low" :type "quantitative")
   (:name "close" :type "quantitative") (:name "volume" :type "quantitative")]
  "Schema of bar/v1 rows.")

(defun easel-adapters--bars (input)
  "Convert bar/v1 INPUT (OHLCV plists, oldest first) to data/v1."
  (unless (or (listp input) (vectorp input))
    (easel-shape-invalid "bar/v1 data is a list of (:open :high :low :close ...) plists" nil))
  (let ((index 0) rows)
    (seq-doseq (bar input)
      (unless (easel-object-p bar)
        (easel-shape-invalid (format "Bar %d is not a plist" index) index))
      (dolist (key '(:open :high :low :close))
        (unless (numberp (plist-get bar key))
          (easel-shape-invalid (format "Bar %d needs a numeric %s" index key)
                               index (easel-key-name key))))
      (let ((volume (plist-get bar :volume)) (time (plist-get bar :time)))
        (when (and volume (not (and (numberp volume) (>= volume 0))))
          (easel-shape-invalid (format "Bar %d volume must be a non-negative number" index)
                               index "volume"))
        (when (and time (not (or (numberp time) (easel-time-string-p time))))
          (easel-shape-invalid (format "Bar %d time must be epoch ms or an ISO date" index)
                               index "time"))
        (when (< (plist-get bar :high) (plist-get bar :low))
          (easel-shape-invalid (format "Bar %d has high below low" index) index "high"))
        (push (append (list :time (or time index))
                      (cl-loop for key in '(:open :high :low :close)
                               append (list key (plist-get bar key)))
                      (list :volume (or volume :null)))
              rows))
      (setq index (1+ index)))
    (easel-data-make (vconcat (nreverse rows)) easel-adapters--bar-schema)))

(easel-register-adapter
 "bar/v1" :doc "OHLCV bars: (:open :high :low :close [:volume] [:time epoch-ms]) oldest first."
 :convert #'easel-adapters--bars
 :example '((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
            (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000)))

;;; Streaming

(defun easel-data--value-fits (type value)
  "Non-nil when VALUE fits schema TYPE."
  (or (memq value '(nil :null))
      (pcase type
        ("quantitative" (numberp value))
        ("temporal" (or (numberp value) (easel-time-string-p value)))
        (_ (or (stringp value) (numberp value) (memq value '(t :false)))))))

(defun easel-data-append (data rows)
  "Return DATA (data/v1) with ROWS appended, after a schema check.
Each new row must be a flat object using only schema columns with
values of the column's type; failures are SHAPE_INVALID whose :index
counts within ROWS."
  (let ((schema (plist-get data :schema)) (index 0))
    (seq-doseq (row rows)
      (unless (and row (easel-object-p row))
        (easel-shape-invalid (format "Pushed row %d is not an object" index) index))
      (cl-loop for (key value) on row by #'cddr
               for name = (easel-key-name key)
               for type = (easel-data-field-type data name)
               do (cond
                   ((null type)
                    (easel-shape-invalid
                     (format "Pushed row %d has column %s, which the schema lacks; columns: %s"
                             index name (mapconcat (lambda (c) (plist-get c :name)) schema ", "))
                     index name))
                   ((not (easel-data--value-fits type value))
                    (easel-shape-invalid
                     (format "Pushed row %d column %s must be %s" index name type)
                     index name))))
      (setq index (1+ index)))
    (list :schema schema :rows (vconcat (plist-get data :rows) rows))))

(provide 'easel-adapters)
;;; easel-adapters.el ends here
