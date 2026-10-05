;;; easel-transform-domain.el --- registered domain transforms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L1, the extension half.  A domain transform is an
;; {"x-easel:transform": NAME, ...} entry in a spec's transform array.
;; Resolve materializes it into plain columns, so the resolved spec is
;; pure Vega-Lite.  Domain packages register their own transforms
;; (technical indicators, reference ranges); describe lists each schema.

;;; Code:

(require 'easel-core)

(defvar easel-transforms nil
  "Registered domain transforms: alist of (NAME . PLIST).
PLIST has :doc, :schema and :fn.")

(cl-defun easel-register-transform (name &key schema fn doc)
  "Register domain transform NAME (a string).
FN is called with ROWS (a vector of row plists) and PARAMS (the
transform object as a plist) and returns the new rows vector.  SCHEMA
is a plist mapping each parameter keyword to a plist
\(:type TYPE [:required t] [:doc STRING] [:default V]); TYPE is a JSON
type name (\"string\" \"number\" \"integer\" \"boolean\" \"array\"
\"object\") or \"any\".  DOC is one line for describe."
  (unless (and (stringp name) (functionp fn))
    (easel-signal "INVALID_INPUT" "A transform needs a string NAME and an :fn"
                  :transform name))
  (setf (alist-get name easel-transforms nil nil #'equal)
        (list :name name :doc doc :schema schema :fn fn))
  name)

(defun easel-transform-get (name &optional path)
  "Return the transform plist for NAME, or signal TRANSFORM_UNKNOWN at PATH."
  (or (alist-get name easel-transforms nil nil #'equal)
      (easel-signal "TRANSFORM_UNKNOWN"
                    (format "No domain transform %S; registered: %s" name
                            (if easel-transforms (mapconcat #'car easel-transforms ", ")
                              "none"))
                    :transform name :path path)))

(defun easel-json-type-p (type value)
  "Non-nil when VALUE has JSON TYPE (a string such as \"number\")."
  (pcase type
    ("any" t)
    ("string" (stringp value))
    ("number" (numberp value))
    ("integer" (integerp value))
    ("boolean" (memq value '(t :false)))
    ("array" (vectorp value))
    ("object" (easel-object-p value))
    (_ nil)))

(defun easel-transform-check-params (transform params path)
  "Signal INVALID_INPUT at PATH unless PARAMS fit TRANSFORM's schema."
  (let ((schema (plist-get transform :schema)))
    (cl-loop for (key spec) on schema by #'cddr
             for value = (plist-get params key)
             do (cond
                 ((and (null value) (plist-get spec :required))
                  (easel-signal "INVALID_INPUT"
                                (format "Transform %s needs %s (%s)" (plist-get transform :name)
                                        (easel-key-name key) (or (plist-get spec :doc) ""))
                                :path (concat path "/" (easel-key-name key))))
                 ((and value (not (easel-json-type-p (plist-get spec :type) value)))
                  (easel-signal "INVALID_INPUT"
                                (format "Transform %s: %s must be a %s" (plist-get transform :name)
                                        (easel-key-name key) (plist-get spec :type))
                                :path (concat path "/" (easel-key-name key))))))))

(defun easel-transform-apply-domain (params rows &optional path)
  "Apply the domain transform described by PARAMS to ROWS; return new rows.
PATH is the transform's JSON pointer, used in failures."
  (let* ((name (plist-get params :x-easel:transform))
         (transform (easel-transform-get name path))
         (schema (plist-get transform :schema))
         (filled params))
    (cl-loop for (key spec) on schema by #'cddr
             when (and (null (plist-get params key)) (plist-member spec :default))
             do (setq filled (easel-plist-put filled key (plist-get spec :default))))
    (easel-transform-check-params transform filled path)
    (let ((out (funcall (plist-get transform :fn) rows filled)))
      (if (vectorp out) out (vconcat out)))))

(provide 'easel-transform-domain)
;;; easel-transform-domain.el ends here
