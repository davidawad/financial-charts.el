;;; easel-resolve.el --- resolve: template + bindings -> pure Vega-Lite -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L3, second half.  `easel-resolve' binds data to slots, fills
;; defaults, substitutes slot placeholders, materializes domain
;; transforms into columns, inlines the data and strips every x-easel
;; key.  The output is a complete, standalone Vega-Lite spec that
;; `bin/chart build' renders as-is.  It is pure and deterministic, and
;; `easel-resolve-hash' content-hashes it.

;;; Code:

(require 'easel-core)
(require 'easel-spec)
(require 'easel-template)
(require 'easel-transform-domain)

(defun easel-resolve--slot-value (values name path)
  "Return slot NAME's value from VALUES (data slots give their rows).
PATH locates the placeholder for failures."
  (let ((key (easel-key name)))
    (unless (plist-member values key)
      (easel-signal "SLOT_MISSING"
                    (format "Placeholder at %s names slot %s, which the template does not declare"
                            path name)
                    :slot name :path path))
    (let ((value (plist-get values key)))
      (if (easel-data-p value) (plist-get value :rows) value))))

(defun easel-resolve--substitute (node values path)
  "Replace slot placeholders and named data in NODE using slot VALUES."
  (cond
   ((vectorp node)
    (let ((i -1) out)
      (seq-doseq (el node)
        (setq i (1+ i))
        (let ((epath (format "%s/%d" path i)))
          (if (and (easel-object-p el) (plist-get el :x-easel:when))
              (when (easel-true-p (easel-resolve--slot-value
                                   values (plist-get el :x-easel:when) epath))
                (push (easel-resolve--substitute (plist-get el :spec) values epath) out))
            (push (easel-resolve--substitute el values epath) out))))
      (vconcat (nreverse out))))
   ((and (easel-object-p node) node (plist-get node :x-easel:slot))
    (easel-resolve--slot-value values (plist-get node :x-easel:slot) path))
   ((and (easel-object-p node) node)
    (cl-loop for (key value) on node by #'cddr
             for kpath = (concat path "/" (easel-key-name key))
             append (list key
                          (if (and (eq key :data) (stringp (plist-get value :name))
                                   (easel-data-p (plist-get values (easel-key (plist-get value :name)))))
                              (list :values (easel-resolve--slot-value
                                             values (plist-get value :name) kpath))
                            (easel-resolve--substitute value values kpath)))))
   (t node)))

(defun easel-resolve--materialize (view cell path)
  "Run VIEW's domain transforms on its data, recursively; return the view.
CELL is a one-element list holding the nearest inherited rows (or nil).
Domain transforms must precede native transforms in an array."
  (let* ((own (plist-get (plist-get view :data) :values))
         (cell (if (vectorp own) (list own) cell))
         (native-seen nil) kept (i -1))
    (seq-doseq (tr (plist-get view :transform))
      (setq i (1+ i))
      (let ((tpath (format "%s/transform/%d" path i)))
        (cond
         ((not (plist-get tr :x-easel:transform))
          (setq native-seen t)
          (push tr kept))
         (native-seen
          (easel-signal "UNSUPPORTED_FEATURE"
                        "Domain transforms must come before native transforms; move it up"
                        :path tpath :feature "transform/x-easel-order"))
         ((null cell)
          (easel-signal "INVALID_INPUT"
                        "A domain transform needs inline data (data.values or a data slot) in scope"
                        :path tpath))
         (t (setcar cell (easel-transform-apply-domain tr (car cell) tpath))))))
    (let ((out view))
      (when (plist-member view :transform)
        (setq out (if kept
                      (easel-plist-put out :transform (vconcat (nreverse kept)))
                    (easel--plist-without out :transform))))
      (dolist (key '(:layer :vconcat :hconcat))
        (when (vectorp (plist-get view key))
          (let ((j -1))
            (setq out (easel-plist-put
                       out key
                       (vconcat (mapcar (lambda (child)
                                          (setq j (1+ j))
                                          (easel-resolve--materialize
                                           child cell (format "%s/%s/%d" path
                                                              (easel-key-name key) j)))
                                        (plist-get view key))))))))
      (when (vectorp own)
        (setq out (easel-plist-put out :data (easel-plist-put (plist-get view :data)
                                                              :values (car cell)))))
      out)))

(defun easel-resolve--strip (node)
  "Return NODE without x-easel keys, recursively."
  (cond
   ((vectorp node) (vconcat (mapcar #'easel-resolve--strip node)))
   ((and (easel-object-p node) node)
    (cl-loop for (key value) on node by #'cddr
             unless (string-prefix-p ":x-easel" (symbol-name key))
             append (list key (easel-resolve--strip value))))
   (t node)))

(defun easel-resolve-spec (spec &optional values)
  "Resolve chart/v1 SPEC to pure Vega-Lite using slot VALUES (a plist).
VALUES come from `easel-template-bind'; nil for a plain spec."
  (let* ((spec (easel-spec-parse spec))
         (body (easel-resolve--substitute spec values ""))
         (body (easel-resolve--materialize body nil ""))
         (body (easel-resolve--strip body)))
    (if (plist-get body :$schema)
        body
      (cons :$schema (cons easel-spec-schema-url body)))))

(defun easel-resolve (template bindings)
  "Resolve TEMPLATE (a name or template plist) with BINDINGS.
Return a complete, standalone Vega-Lite spec."
  (let ((template (if (stringp template) (easel-template-get template) template)))
    (easel-resolve-spec (plist-get template :spec)
                        (easel-template-bind template bindings))))

(defun easel-resolve-hash (resolved)
  "Return the content hash of the RESOLVED spec."
  (easel-content-hash resolved))

(provide 'easel-resolve)
;;; easel-resolve.el ends here
