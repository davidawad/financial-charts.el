;;; easel-data.el --- data/v1: tidy rows and the adapter registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L0.  data/v1 is (:schema [(:name N :type T [:unit U]) ...] :rows [ROW ...])
;; where every ROW is a flat plist with keyword keys and TYPE is one of
;; quantitative, temporal, ordinal, nominal.
;;
;; Adapters turn caller data into data/v1.  Each is registered once
;; with `easel-register-adapter' and fails as data: its validator
;; signals SHAPE_INVALID whose props carry :index and :field.
;; `easel-data-check' returns that failure as a plist instead.

;;; Code:

(require 'easel-core)
(require 'easel-time)

(defvar easel-adapters nil
  "Registered adapters: alist of (NAME . PLIST).
PLIST has :doc, :convert (VALUE -> data/v1) and optionally :example.")

(cl-defun easel-register-adapter (name &key doc convert example)
  "Register adapter NAME (a string) that turns caller data into data/v1.
CONVERT is called with the caller's value and returns data/v1, or
signals SHAPE_INVALID with :index and :field.  DOC is one line for
describe.  EXAMPLE is a small input value that CONVERT accepts."
  (unless (and (stringp name) (functionp convert))
    (easel-signal "INVALID_INPUT" "An adapter needs a string NAME and a :convert function"
                  :adapter name))
  (setf (alist-get name easel-adapters nil nil #'equal)
        (list :name name :doc doc :convert convert :example example))
  name)

(defun easel-adapter (name)
  "Return the adapter plist for NAME or signal NOT_FOUND."
  (or (alist-get name easel-adapters nil nil #'equal)
      (easel-signal "NOT_FOUND"
                    (format "No adapter %S; registered adapters: %s" name
                            (mapconcat #'car easel-adapters ", "))
                    :adapter name)))

(defun easel-data-from (name value)
  "Convert VALUE to data/v1 with adapter NAME."
  (funcall (plist-get (easel-adapter name) :convert) value))

(defun easel-data-check (name value)
  "Return nil when adapter NAME accepts VALUE, else the failure plist.
The plist is (:code :message :index :field ...), never signalled."
  (condition-case err
      (progn (easel-data-from name value) nil)
    (easel-error (easel-error-plist err))))

(defun easel-shape-invalid (message index &optional field &rest props)
  "Signal SHAPE_INVALID with MESSAGE for element INDEX and FIELD."
  (apply #'easel-signal "SHAPE_INVALID" message :index index :field field props))

;;; data/v1

(defun easel-data-p (value)
  "Non-nil when VALUE looks like data/v1."
  (and (easel-object-p value) (plist-member value :rows) (plist-member value :schema)))

(defun easel-data-rows (data)
  "Return the rows vector of DATA."
  (plist-get data :rows))

(defun easel-data-make (rows &optional schema)
  "Return data/v1 for ROWS (a vector or list of plists).
SCHEMA, when nil, is inferred from the rows."
  (let ((rows (if (vectorp rows) rows (vconcat rows))))
    (list :schema (or schema (easel-data-infer-schema rows)) :rows rows)))

(defun easel-data-infer-type (values)
  "Return the measurement type that fits every non-null value in VALUES."
  (let ((values (seq-remove (lambda (v) (memq v '(nil :null))) values)))
    (cond ((null values) "nominal")
          ((seq-every-p #'numberp values) "quantitative")
          ((seq-every-p #'easel-time-string-p values) "temporal")
          (t "nominal"))))

(defun easel-data-columns (rows)
  "Return the column keys of ROWS in first-seen order."
  (let (keys)
    (seq-doseq (row rows)
      (dolist (key (easel-plist-keys row))
        (unless (memq key keys) (push key keys))))
    (nreverse keys)))

(defun easel-data-infer-schema (rows)
  "Return a schema vector inferred from ROWS."
  (vconcat
   (mapcar (lambda (key)
             (list :name (easel-key-name key)
                   :type (easel-data-infer-type
                          (seq-map (lambda (row) (plist-get row key)) rows))))
           (easel-data-columns rows))))

(defun easel-data-field-type (data field)
  "Return the schema type of FIELD (a string) in DATA, or nil."
  (seq-some (lambda (col) (and (equal (plist-get col :name) field) (plist-get col :type)))
            (plist-get data :schema)))

;;; Built-in adapters

(defun easel-data--plist-rows (value)
  "Convert a list or vector of row objects to data/v1."
  (unless (or (vectorp value) (listp value))
    (easel-shape-invalid "Rows must be a list or array of objects" nil))
  (let ((index 0))
    (seq-doseq (row value)
      (unless (and (easel-object-p row) row)
        (easel-shape-invalid
         (format "Row %d is not an object; give each row as {\"field\": value, ...}" index)
         index))
      (dolist (key (easel-plist-keys row))
        (let ((v (plist-get row key)))
          (unless (or (numberp v) (stringp v) (memq v '(t :false :null nil)))
            (easel-shape-invalid
             (format "Row %d field %s is nested; rows must be flat" index (easel-key-name key))
             index (easel-key-name key)))))
      (setq index (1+ index))))
  (easel-data-make value))

(defun easel-data--passthrough (value)
  "Accept VALUE that is already data/v1, checking its rows."
  (unless (easel-data-p value)
    (easel-shape-invalid "data/v1 needs :schema and :rows" nil))
  (let ((rows (plist-get (easel-data--plist-rows (plist-get value :rows)) :rows)))
    (list :schema (plist-get value :schema) :rows rows)))

(easel-register-adapter
 "plist" :doc "List or array of flat row objects (Lisp plists or JSON objects)."
 :convert #'easel-data--plist-rows
 :example '((:x 1 :y 2) (:x 2 :y 3)))

(easel-register-adapter
 "data/v1" :doc "Already tidy: (:schema [...] :rows [...])."
 :convert #'easel-data--passthrough
 :example '(:schema [(:name "x" :type "quantitative")] :rows [(:x 1)]))

(provide 'easel-data)
;;; easel-data.el ends here
