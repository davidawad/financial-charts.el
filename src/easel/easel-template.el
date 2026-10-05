;;; easel-template.el --- template/v1: chart/v1 specs with typed slots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L3, first half.  A template is a chart/v1 spec whose "x-easel" key
;; carries template, version, doc, slots and example.  A slot is one of
;;
;;   {"shape": ADAPTER}            data, converted by the named adapter
;;   {"type": T [, "items": T2]}   string number integer boolean array
;;                                 object, or "field" (a column name)
;;   {"enum": [V ...]}             one of the listed values
;;
;; plus optional "required", "default", "doc" and, for field slots,
;; "of": the data slot whose columns it names.
;;
;; In the spec body {"x-easel:slot": NAME} is replaced by the slot's
;; value and an array element {"x-easel:when": NAME, "spec": X} is kept
;; (as X) only when the slot is truthy.  {"name": NAME} data refers to a
;; data slot.  Templates are JSON files in `easel-template-directories',
;; loaded on first use.

;;; Code:

(require 'easel-core)
(require 'easel-spec)
(require 'easel-data)

(defconst easel-template--root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (directory-file-name
      (file-name-directory (or load-file-name buffer-file-name default-directory))))))
  "The repository root (two levels above src/easel/).")

(defvar easel-template-directories
  (list (expand-file-name "templates" easel-template--root))
  "Directories whose *.json files are easel templates.")

(defvar easel--templates nil
  "Loaded templates: alist of (NAME . PLIST) with :spec :meta :path.
nil until the first lookup loads `easel-template-directories'.")

(defun easel-template--meta-check (meta path)
  "Signal INVALID_INPUT unless META (an x-easel object) declares a template."
  (dolist (key '(:template :version :slots))
    (unless (plist-get meta key)
      (easel-signal "INVALID_INPUT"
                    (format "Template %s needs x-easel.%s" path (easel-key-name key))
                    :path (concat "/x-easel/" (easel-key-name key)) :file path)))
  (cl-loop for (slot def) on (plist-get meta :slots) by #'cddr
           unless (or (plist-get def :shape) (plist-get def :type) (plist-get def :enum))
           do (easel-signal "INVALID_INPUT"
                            (format "Slot %s needs a shape, type or enum" (easel-key-name slot))
                            :path (concat "/x-easel/slots/" (easel-key-name slot)) :file path)))

(defun easel-template-register (spec &optional path)
  "Register template SPEC (a parsed chart/v1 value) read from PATH."
  (let* ((spec (easel-spec-validate spec))
         (meta (plist-get spec :x-easel)))
    (easel-template--meta-check meta (or path "<inline>"))
    (setf (alist-get (plist-get meta :template) easel--templates nil nil #'equal)
          (list :name (plist-get meta :template) :spec spec :meta meta :path path))
    (plist-get meta :template)))

(defun easel-template-load (file)
  "Load and register the template in FILE; return its name."
  (easel-template-register (easel-json-read-file file) (expand-file-name file)))

(defun easel-template-reload ()
  "Forget loaded templates and load every directory again."
  (setq easel--templates nil)
  (dolist (dir easel-template-directories)
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.json\\'"))
        (easel-template-load file))))
  (mapcar #'car easel--templates))

(defun easel-template-names ()
  "Return the sorted names of every template."
  (unless easel--templates (easel-template-reload))
  (sort (mapcar #'car easel--templates) #'string<))

(defun easel-template-get (name)
  "Return the template plist NAME or signal NOT_FOUND."
  (unless easel--templates (easel-template-reload))
  (or (alist-get name easel--templates nil nil #'equal)
      (easel-signal "NOT_FOUND"
                    (format "No template %S; templates: %s" name
                            (string-join (easel-template-names) ", "))
                    :template name)))

(defun easel-template-slots (template)
  "Return TEMPLATE's slots plist."
  (plist-get (plist-get template :meta) :slots))

(defun easel-template-example-file (template)
  "Return the absolute file of TEMPLATE's example bindings, or nil."
  (when-let* ((example (plist-get (plist-get template :meta) :example))
              (path (plist-get template :path)))
    (expand-file-name example (file-name-directory
                               (directory-file-name (file-name-directory path))))))

(defun easel-template-example (name)
  "Return the example bindings of template NAME (they render as-is)."
  (let* ((template (easel-template-get name))
         (file (easel-template-example-file template)))
    (unless file
      (easel-signal "NOT_FOUND" (format "Template %s declares no example" name)
                    :template name))
    (easel-json-read-file file)))

(defun easel-template-describe (name)
  "Return the describe plist for template NAME."
  (let* ((template (easel-template-get name))
         (meta (plist-get template :meta)))
    (list :name name :version (plist-get meta :version) :doc (plist-get meta :doc)
          :slots (plist-get meta :slots)
          :example (easel-template-example-file template)
          :path (plist-get template :path))))

;;; Binding

(defun easel-template--type-ok (type value)
  "Non-nil when VALUE fits slot TYPE."
  (pcase type
    ("field" (stringp value))
    ("array" (vectorp value))
    (_ (easel-json-type-p type value))))

(defun easel-template--check-value (slot def value)
  "Signal SLOT_TYPE unless VALUE fits slot SLOT's DEF; return VALUE."
  (let ((type (plist-get def :type))
        (enum (plist-get def :enum))
        (name (easel-key-name slot)))
    (when (and enum (not (seq-contains-p enum value)))
      (easel-signal "SLOT_TYPE"
                    (format "Slot %s must be one of %s" name (easel-json-encode enum))
                    :slot name :expected enum))
    (when (and type (not (easel-template--type-ok type value)))
      (easel-signal "SLOT_TYPE" (format "Slot %s must be a %s" name type)
                    :slot name :expected type))
    (when-let* ((items (and (equal type "array") (plist-get def :items))))
      (seq-do-indexed
       (lambda (item i)
         (unless (easel-template--type-ok items item)
           (easel-signal "SLOT_TYPE"
                         (format "Slot %s item %d must be a %s" name i items)
                         :slot name :index i :expected items)))
       value))
    value))

(defun easel-template--data-value (slot def value)
  "Convert data slot SLOT's VALUE through its DEF :shape adapter."
  (condition-case err
      (easel-data-from (plist-get def :shape) value)
    (easel-error
     (signal (car err) (append (cdr err) (list :slot (easel-key-name slot)))))))

(defun easel-template-bind (template bindings)
  "Check BINDINGS against TEMPLATE's slots and fill defaults.
BINDINGS is a plist (or parsed JSON object) keyed by slot.  Data
slots come back as data/v1.  Signals SLOT_MISSING, SLOT_TYPE,
SHAPE_INVALID, FIELD_MISSING or INVALID_INPUT naming the slot."
  (let* ((slots (easel-template-slots template))
         (bindings (if (vectorp bindings) (easel-signal "INVALID_INPUT"
                                                        "Bindings must be an object keyed by slot")
                     bindings))
         bound)
    (dolist (key (easel-plist-keys bindings))
      (unless (plist-member slots key)
        (easel-signal "INVALID_INPUT"
                      (format "Template %s has no slot %s; slots: %s"
                              (plist-get template :name) (easel-key-name key)
                              (mapconcat #'easel-key-name (easel-plist-keys slots) ", "))
                      :slot (easel-key-name key))))
    (cl-loop for (slot def) on slots by #'cddr
             for given = (plist-member bindings slot)
             for value = (if given (plist-get bindings slot) (plist-get def :default))
             do (cond
                 ((and (not given) (not (plist-member def :default)))
                  (when (easel-true-p (plist-get def :required))
                    (easel-signal "SLOT_MISSING"
                                  (format "Template %s needs slot %s%s" (plist-get template :name)
                                          (easel-key-name slot)
                                          (if (plist-get def :doc)
                                              (concat ": " (plist-get def :doc)) ""))
                                  :slot (easel-key-name slot))))
                 ((plist-get def :shape)
                  (push (cons slot (easel-template--data-value slot def value)) bound))
                 (t (push (cons slot (easel-template--check-value
                                      slot def (if (and (listp value) (equal (plist-get def :type) "array"))
                                                   (vconcat value) value)))
                          bound))))
    (easel-template--check-fields slots bound)
    (cl-loop for (slot . value) in (nreverse bound) append (list slot value))))

(defun easel-template--check-fields (slots bound)
  "Signal FIELD_MISSING when a field slot in SLOTS names no column.
BOUND is an alist of slot values."
  (cl-loop for (slot def) on slots by #'cddr
           for of = (plist-get def :of)
           for data = (and of (alist-get (easel-key of) bound))
           for field = (alist-get slot bound)
           when (and (equal (plist-get def :type) "field") data (stringp field)
                     (not (easel-data-field-type data field)))
           do (easel-signal "FIELD_MISSING"
                            (format "Slot %s names field %s, which %s does not have; columns: %s"
                                    (easel-key-name slot) field of
                                    (mapconcat (lambda (c) (plist-get c :name))
                                               (plist-get data :schema) ", "))
                            :slot (easel-key-name slot) :field field)))

(provide 'easel-template)
;;; easel-template.el ends here
