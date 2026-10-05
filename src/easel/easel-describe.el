;;; easel-describe.el --- the registries as data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `easel-describe' answers "what can this engine do" without reading
;; source: every template with slots, example and path; every domain
;; transform with its schema; every adapter; and the chart/v1 version.
;; The agent surface (fc-qx1.10) wraps it in the chart/v1 envelope.

;;; Code:

(require 'easel-core)
(require 'easel-spec)
(require 'easel-data)
(require 'easel-transform-domain)
(require 'easel-template)

(defvar easel-describe-functions nil
  "Functions of no arguments returning extra describe plist entries.
Later layers (conformance, runtime) add their sections here.")

(defun easel-describe--schema-json (schema)
  "Return transform SCHEMA as a JSON-ready object."
  (cl-loop for (key spec) on schema by #'cddr
           append (list key (cl-loop for (k v) on spec by #'cddr
                                     append (list k (if (eq v t) t v))))))

(defun easel-describe (&optional section)
  "Return the engine's registries as a JSON-ready plist.
SECTION, one of templates, transforms or adapters (symbol or string),
limits the answer to that registry."
  (let* ((section (and section (format "%s" section)))
         (all (append
               (list :vega-lite easel-spec-vega-lite-version
                     :templates (vconcat (mapcar #'easel-template-describe (easel-template-names)))
                     :transforms (vconcat
                                  (mapcar (lambda (entry)
                                            (list :name (car entry) :doc (plist-get (cdr entry) :doc)
                                                  :schema (easel-describe--schema-json
                                                           (plist-get (cdr entry) :schema))))
                                          (sort (copy-sequence easel-transforms)
                                                (lambda (a b) (string< (car a) (car b))))))
                     :adapters (vconcat
                                (mapcar (lambda (entry)
                                          (list :name (car entry) :doc (plist-get (cdr entry) :doc)))
                                        (sort (copy-sequence easel-adapters)
                                              (lambda (a b) (string< (car a) (car b)))))))
               (apply #'append (mapcar #'funcall easel-describe-functions)))))
    (if section
        (let ((key (easel-key section)))
          (unless (plist-member all key)
            (easel-signal "NOT_FOUND"
                          (format "No describe section %s; sections: %s" section
                                  (mapconcat #'easel-key-name (easel-plist-keys all) ", "))
                          :section section))
          (list key (plist-get all key)))
      all)))

(provide 'easel-describe)
;;; easel-describe.el ends here
