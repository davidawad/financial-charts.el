;;; financial-chart-indicator-api.el --- Provider-neutral indicator series -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Technical indicators consume normalized bar/v1 data and produce
;; indicator-series/v1 values.  Providers and calculators share this
;; boundary; this file has no broker dependency.

;;; Code:

(require 'cl-lib)

(define-error 'financial-chart-invalid-indicator
  "financial-chart: invalid indicator series" 'financial-chart-error)

(defvar financial-chart-indicator-registry nil
  "Registered indicators as (NAME :fn FN :label LABEL ...).
Functions receive normalized bars followed by caller parameters.")

(defun financial-chart-register-indicator (name function &rest metadata)
  "Register indicator NAME implemented by FUNCTION with METADATA.
Metadata is a plist and may include :label, :unit, :panel, :scale,
:bounds, and :description.  FUNCTION receives BARS and user parameters.
Re-registering NAME replaces its earlier entry.  Return the entry."
  (unless (and (symbolp name) (functionp function) (zerop (% (length metadata) 2)))
    (signal 'financial-chart-invalid-indicator
            (list "register needs a symbol, function, and keyword plist" :name name)))
  (let ((entry (append (list name :fn function) metadata)))
    (setq financial-chart-indicator-registry
          (cons entry (assq-delete-all name financial-chart-indicator-registry)))
    entry))

(defun financial-chart-list-indicators ()
  "Return registered indicator metadata without implementation functions."
  (mapcar (lambda (entry)
            (append (list :name (car entry))
                    (cl-loop for (key value) on (cdr entry) by #'cddr
                             unless (eq key :fn) append (list key value))))
          (reverse financial-chart-indicator-registry)))

(defun financial-chart-normalize-indicator-series (series)
  "Validate and return an indicator-series/v1 SERIES plist.

Required fields are :name and :values.  :values is an oldest-first list
or vector of numbers and nil warm-up/missing values.  Optional
:timestamps is a same-length list/vector of epoch-millisecond numbers
or nils.  :unit, :panel, :scale, :bounds, :params and :source are
provider-neutral display/provenance metadata; unknown fields are kept.
This function performs shape validation only, not financial semantics."
  (let* ((name (plist-get series :name))
         (values (plist-get series :values))
         (timestamps (plist-get series :timestamps)))
    (unless (and (listp series) (keywordp (car series))
                 (or (symbolp name) (stringp name))
                 (or (listp values) (vectorp values))
                 (not (stringp values)))
      (signal 'financial-chart-invalid-indicator
              (list "series needs :name and a numeric :values list or vector"
                    :name name)))
    (setq values (append values nil))
    (unless (cl-every (lambda (value) (or (null value) (numberp value))) values)
      (signal 'financial-chart-invalid-indicator
              (list "series values must be numbers or nil" :name name)))
    (when timestamps
      (unless (and (or (listp timestamps) (vectorp timestamps))
                   (not (stringp timestamps))
                   (= (length timestamps) (length values))
                   (cl-every (lambda (time) (or (null time) (numberp time)))
                             (append timestamps nil)))
        (signal 'financial-chart-invalid-indicator
                (list "timestamps must be numeric or nil and align with values"
                      :name name))))
    (let ((normalized (copy-sequence series)))
      (setq normalized (plist-put normalized :schema 'indicator-series/v1))
      (setq normalized (plist-put normalized :values values))
      (when timestamps
        (setq normalized (plist-put normalized :timestamps
                                    (append timestamps nil))))
      normalized)))

(defun financial-chart-indicator-series-data (series)
  "Convert normalized SERIES to generic chart data, preserving timestamps."
  (let* ((series (financial-chart-normalize-indicator-series series))
         (values (plist-get series :values))
         (timestamps (plist-get series :timestamps)))
    (if timestamps
        (cl-mapcar (lambda (time value)
                     (if time (list time value) value)) timestamps values)
      values)))

(defun financial-chart-indicator-chart-spec (series &optional title)
  "Return a renderable line-chart spec for normalized SERIES and TITLE."
  (let* ((series (financial-chart-normalize-indicator-series series))
         (data (financial-chart-indicator-series-data series)))
    (list :kind 'line :data data
          :title (or title (plist-get series :label)
                     (format "%s" (plist-get series :name)))
          :unit (plist-get series :unit))))

(defun financial-chart--indicator-output-p (value)
  "Non-nil when VALUE is a named output descriptor from an indicator."
  (and (listp value) (keywordp (car value)) (plist-member value :values)))

(defun financial-chart--indicator-times (bars)
  "Return timestamps from BARS when every bar has a usable :time."
  (let ((times (mapcar (lambda (bar) (plist-get bar :time)) bars)))
    (and (cl-every #'numberp times) times)))

(defun financial-chart-indicator-evaluate (name bars &rest parameters)
  "Evaluate registered indicator NAME on normalized BARS.

PARAMETERS are passed unchanged to the registered implementation.
Return one normalized series plist, or a list of them for a multi-output
indicator.  Each result stays aligned with BARS; nil denotes warm-up or
missing data.  Computation is local and synchronous."
  (let* ((entry (assq name financial-chart-indicator-registry))
         (function (plist-get (cdr entry) :fn)))
    (unless entry
      (signal 'financial-chart-invalid-indicator
              (list (format "unknown indicator `%s'" name) :name name)))
    (let* ((raw (apply function bars parameters))
           (outputs (if (and (listp raw) (financial-chart--indicator-output-p
                                           (car raw)))
                        raw
                      (list (list :values raw))))
           (metadata (cdr entry))
           (times (financial-chart--indicator-times bars)))
      (let ((results
             (mapcar
              (lambda (output)
                (let* ((output-name (or (plist-get output :name) name))
                       (output-label (or (plist-get output :label)
                                         (plist-get metadata :label)
                                         (format "%s" output-name)))
                       (values (plist-get output :values)))
                  (unless (= (length values) (length bars))
                    (signal 'financial-chart-invalid-indicator
                            (list "computed values must align with input bars"
                                  :name output-name)))
                  (financial-chart-normalize-indicator-series
                   (append (list :name output-name
                                 :label output-label
                                 :values values
                                 :timestamps times
                                 :params parameters)
                           (cl-loop for (key value) on metadata by #'cddr
                                    unless (eq key :fn) append (list key value))
                           (cl-loop for (key value) on output by #'cddr
                                    unless (memq key '(:name :label :values))
                                    append (list key value))))))
              outputs)))
        (if (cdr results) results (car results))))))

(provide 'financial-chart-indicator-api)
;;; financial-chart-indicator-api.el ends here
