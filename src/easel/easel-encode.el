;;; easel-encode.el --- encoding channels: normalize, derive, evaluate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A unit view's encoding becomes a plist of normalized
;; channel definitions.  Encoding-level timeUnit, bin and aggregate are
;; turned into the implicit transforms Vega-Lite derives, with
;; Vega-Lite's output field names, so every later step only reads plain
;; fields.  Conditions are evaluated through
;; `easel-encode-param-test-function', which the runtime replaces with
;; selection semantics.

;;; Code:

(require 'easel-core)
(require 'easel-data)
(require 'easel-expr)
(require 'easel-transform)

(defvar easel-encode-param-test-function
  (lambda (_param _row empty) empty)
  "Function (PARAM ROW EMPTY) -> non-nil when ROW is in selection PARAM.
EMPTY is the answer for an empty selection.  The runtime binds this
around compile with the view's current param state.")

(defun easel-encode--infer-type (def rows)
  "Return DEF's measurement type, inferring it from DEF and ROWS."
  (or (plist-get def :type)
      (cond ((plist-get def :aggregate) "quantitative")
            ((easel-true-p (plist-get def :bin)) "quantitative")
            ((plist-get def :timeUnit) "temporal")
            ((plist-get def :field)
             (easel-data-infer-type
              (seq-map (lambda (r) (plist-get r (easel-key (plist-get def :field)))) rows)))
            (t "nominal"))))

(defun easel-encode--def (def rows)
  "Normalize one channel DEF against ROWS."
  (if (not (easel-object-p def)) def
    (let ((out def))
      (when (or (plist-get def :field) (plist-get def :aggregate))
        (setq out (easel-plist-put out :type (easel-encode--infer-type def rows))))
      (when-let* ((c (plist-get def :condition)))
        (setq out (easel-plist-put out :condition
                                   (if (vectorp c) (vconcat (mapcar (lambda (d) (easel-encode--def d rows)) c))
                                     (easel-encode--def c rows)))))
      out)))

(defun easel-encode-normalize (encoding rows)
  "Normalize ENCODING's channel definitions against ROWS."
  (cl-loop for (channel def) on encoding by #'cddr
           append (list channel
                        (if (vectorp def)
                            (vconcat (mapcar (lambda (d) (easel-encode--def d rows)) def))
                          (easel-encode--def def rows)))))

;;; Implicit transforms

(defun easel-encode--bin-name (def)
  "Vega-Lite's output field for binned DEF."
  (let ((opts (plist-get def :bin)))
    (format "bin_maxbins_%s_%s" (or (and (easel-object-p opts) (plist-get opts :maxbins)) 10)
            (plist-get def :field))))

(defun easel-encode-derive (encoding rows env)
  "Apply ENCODING's implicit timeUnit, bin and aggregate to ROWS.
Return (ENCODING' . ROWS') where ENCODING' points at derived fields.
ENV holds param values for expressions."
  (let ((enc encoding) (transforms nil))
    ;; timeUnit and bin
    (cl-loop for (channel def) on encoding by #'cddr
             when (and (easel-object-p def) (plist-get def :field))
             do (cond
                 ((plist-get def :timeUnit)
                  (let ((as (format "%s_%s" (plist-get def :timeUnit) (plist-get def :field))))
                    (push (list :timeUnit (plist-get def :timeUnit) :field (plist-get def :field) :as as)
                          transforms)
                    (setq enc (easel-plist-put enc channel
                                               (append (list :field as :source (plist-get def :field)
                                                             :derived "timeUnit")
                                                       (easel--plist-without
                                                        (easel--plist-without def :field) :timeUnit))))))
                 ((easel-true-p (plist-get def :bin))
                  (let ((as (easel-encode--bin-name def)))
                    (push (list :bin (plist-get def :bin) :field (plist-get def :field) :as as) transforms)
                    (setq enc (easel-plist-put enc channel
                                               (append (list :field as :source (plist-get def :field)
                                                             :derived "bin" :bin-end (concat as "_end"))
                                                       (easel--plist-without
                                                        (easel--plist-without def :field) :bin))))))))
    (let ((rows (easel-transform-run (vconcat (nreverse transforms)) rows env "/encoding")))
      (easel-encode--aggregate enc rows))))

(defun easel-encode--aggregate (encoding rows)
  "Apply ENCODING's aggregate channels to ROWS; return (ENCODING . ROWS)."
  (let (ops groupby (enc encoding))
    (cl-loop for (channel def) on encoding by #'cddr
             for defs = (if (vectorp def) (append def nil) (list def))
             do (dolist (d defs)
                  (when (easel-object-p d)
                    (if-let* ((op (plist-get d :aggregate)))
                        (let ((as (if (and (equal op "count") (null (plist-get d :field)))
                                      "__count"
                                    (format "%s_%s" op (plist-get d :field)))))
                          (push (list :op op :field (plist-get d :field) :as as) ops)
                          (unless (vectorp def)
                            (setq enc (easel-plist-put
                                       enc channel (append (list :field as :source (plist-get d :field)
                                                                 :derived "aggregate" :op op)
                                                           (easel--plist-without
                                                            (easel--plist-without d :field) :aggregate))))))
                      (dolist (f (list (plist-get d :field) (plist-get d :bin-end)))
                        (when (and f (not (member f groupby))) (push f groupby)))))))
    (if (null ops)
        (cons encoding rows)
      (cons enc (easel-transform-aggregate
                 (list :aggregate (vconcat (delete-dups (nreverse ops)))
                       :groupby (vconcat (nreverse groupby)))
                 rows "/encoding")))))

;;; Evaluation

(defun easel-encode-field (def)
  "The row key DEF reads, or nil."
  (and (easel-object-p def) (plist-get def :field) (easel-key (plist-get def :field))))

(defun easel-encode-raw (def row)
  "DEF's raw data value for ROW (field, datum or nil)."
  (cond ((not (easel-object-p def)) nil)
        ((plist-get def :field) (plist-get row (easel-encode-field def)))
        ((plist-member def :datum) (plist-get def :datum))))

(defun easel-encode--condition-holds (cond row env)
  "Non-nil when condition COND applies to ROW."
  (cond
   ((plist-get cond :param)
    (funcall easel-encode-param-test-function (plist-get cond :param) row
             (not (eq (plist-get cond :empty) :false))))
   ((plist-get cond :test) (easel-transform-predicate (plist-get cond :test) row env))
   (t nil)))

(defun easel-encode-active (def row env)
  "Return the definition that applies to ROW: a matching condition or DEF."
  (let ((conds (plist-get def :condition)))
    (or (seq-find (lambda (c) (easel-encode--condition-holds c row env))
                  (cond ((vectorp conds) conds) (conds (list conds))))
        def)))

(defun easel-encode-discrete-p (def)
  "Non-nil when DEF's type is nominal or ordinal."
  (member (plist-get def :type) '("nominal" "ordinal")))

;;; Titles and tooltips

(defconst easel-encode--op-titles
  '(("mean" . "Average") ("average" . "Average") ("sum" . "Sum") ("count" . "Count")
    ("median" . "Median") ("min" . "Min") ("max" . "Max") ("distinct" . "Distinct")
    ("variance" . "Variance") ("stdev" . "Stdev") ("q1" . "Q1") ("q3" . "Q3"))
  "Vega-Lite's titles for aggregate ops.")

(defun easel-encode-title (def)
  "Vega-Lite's default title for DEF, or DEF's explicit :title."
  (let ((title (plist-get def :title)))
    (cond
     ((stringp title) title)
     ((memq title '(:null :false)) nil)
     ((equal (plist-get def :derived) "aggregate")
      (if (and (equal (plist-get def :op) "count") (null (plist-get def :source)))
          "Count of Records"
        (format "%s of %s" (or (cdr (assoc (plist-get def :op) easel-encode--op-titles))
                               (plist-get def :op))
                (plist-get def :source))))
     ((equal (plist-get def :derived) "bin") (format "%s (binned)" (plist-get def :source)))
     ((equal (plist-get def :derived) "timeUnit")
      (format "%s (%s)" (plist-get def :source)
              (string-join (easel-time-unit-components (car (split-string (plist-get def :field) "_"))) "-")))
     (t (plist-get def :field)))))

(defun easel-encode-format-value (def value)
  "Format VALUE for a tooltip per DEF (temporal as \"%b %d, %Y\")."
  (cond
   ((memq value '(nil :null)) "null")
   ((and (equal (plist-get def :type) "temporal") (easel-time-parse value))
    (let ((system-time-locale "C"))
      (easel-time-format (easel-time-parse value) "%b %d, %Y")))
   (t (easel-expr--string value))))

(defun easel-encode-tooltip (encoding mark row)
  "Return ROW's tooltip under ENCODING and MARK, or nil.
The tooltip is a vector of (:title TITLE :value FORMATTED) in order."
  (let* ((tip (plist-get encoding :tooltip))
         (defs (cond ((vectorp tip) (append tip nil))
                     ((and tip (easel-object-p tip) (plist-get tip :field)) (list tip))
                     ((easel-true-p (plist-get mark :tooltip))
                      (cl-loop for (ch d) on encoding by #'cddr
                               when (and (memq ch '(:x :y :x2 :y2 :color :fill :stroke :size :text))
                                         (easel-object-p d) (plist-get d :field))
                               collect d)))))
    (when defs
      (vconcat (mapcar (lambda (d)
                         (list :title (or (easel-encode-title d) (plist-get d :field))
                               :value (easel-encode-format-value d (easel-encode-raw d row))))
                       defs)))))

(provide 'easel-encode)
;;; easel-encode.el ends here
