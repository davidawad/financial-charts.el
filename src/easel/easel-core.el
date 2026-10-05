;;; easel-core.el --- easel errors, JSON values and hashing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The conventions every easel layer shares.
;;
;; JSON values in Lisp: objects are plists with keyword keys, arrays
;; are vectors, JSON null is `:null' and JSON false is `:false'.  So
;; `nil' is the empty object, never an empty array, and every value
;; round-trips through `easel-json-parse' / `easel-json-encode'.
;;
;; Failures are data: `easel-signal' raises an `easel-error' child
;; whose data is (MESSAGE :code CODE ...), and the MESSAGE says how to
;; fix the input.  The codes are the design's section 5 reason codes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(define-error 'easel-error "easel error")

(defconst easel-reason-codes
  '(("INVALID_INPUT" easel-invalid-input "Invalid input")
    ("PARSE_ERROR" easel-parse-error "Parse error")
    ("NOT_FOUND" easel-not-found "Not found")
    ("SLOT_MISSING" easel-slot-missing "Template slot missing")
    ("SLOT_TYPE" easel-slot-type "Template slot has the wrong type")
    ("SHAPE_INVALID" easel-shape-invalid "Data does not match its shape")
    ("FIELD_MISSING" easel-field-missing "Field missing from data")
    ("UNSUPPORTED_FEATURE" easel-unsupported-feature "Unsupported feature")
    ("TRANSFORM_UNKNOWN" easel-transform-unknown "Unknown transform")
    ("VIEW_NOT_FOUND" easel-view-not-found "View not found")
    ("EVENT_INVALID" easel-event-invalid "Invalid event")
    ("ENGINE_FAILED" easel-engine-failed "Engine failed"))
  "Stable reason codes: (CODE ERROR-SYMBOL TITLE).
Codes shared with bin/chart mean the same thing in both.")

(dolist (entry easel-reason-codes)
  (define-error (nth 1 entry) (nth 2 entry) 'easel-error))

(defun easel-signal (code message &rest props)
  "Signal the `easel-error' child for CODE with MESSAGE and PROPS.
CODE is a reason code string from `easel-reason-codes'.  The error
data is (MESSAGE :code CODE . PROPS)."
  (let ((entry (assoc code easel-reason-codes)))
    (unless entry
      (error "Unknown easel reason code %S" code))
    (signal (nth 1 entry) (cons message (cons :code (cons code props))))))

(defun easel-error-plist (err)
  "Return the condition ERR as a plist (:code :message ...).
ERR is the value bound by `condition-case'.  Errors that are not
`easel-error' children are reported as ENGINE_FAILED."
  (let ((data (cdr err)))
    (if (and (stringp (car-safe data)) (plist-get (cdr data) :code))
        (append (list :code (plist-get (cdr data) :code) :message (car data))
                (easel--plist-without (cdr data) :code))
      (list :code "ENGINE_FAILED" :message (error-message-string err)))))

(defun easel--plist-without (plist key)
  "Return a copy of PLIST with KEY removed."
  (let (out)
    (while plist
      (unless (eq (car plist) key)
        (setq out (cons (cadr plist) (cons (car plist) out))))
      (setq plist (cddr plist)))
    (nreverse out)))

;;; JSON values

(defun easel-json-parse (string)
  "Parse JSON STRING into an easel JSON value.
Signals PARSE_ERROR naming the position when STRING is not JSON."
  (condition-case err
      (json-parse-string string :object-type 'plist :array-type 'array
                         :null-object :null :false-object :false)
    (json-error
     (easel-signal "PARSE_ERROR"
                   (format "Not valid JSON (%s); fix the syntax and retry"
                           (error-message-string err))
                   :detail (cdr err)))))

(defun easel-json-read-file (file)
  "Parse the JSON in FILE.  Signals NOT_FOUND or PARSE_ERROR."
  (unless (file-readable-p file)
    (easel-signal "NOT_FOUND" (format "No readable file %s; check the path" file)
                  :path file))
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (easel-json-parse (buffer-string)))
    (easel-parse-error
     (signal (car err) (append (cdr err) (list :path file))))))

(defun easel-json-encode (value)
  "Encode the easel JSON VALUE as a compact JSON string."
  (json-serialize value :null-object :null :false-object :false))

(defun easel-json-pretty (value)
  "Encode VALUE as indented JSON with sorted keys and a trailing newline.
Arrays of scalars stay on one line.  Used for goldens and humans."
  (concat (easel-json--pretty (easel-json-canonical value) "") "\n"))

(defun easel-json--scalar-p (value)
  "Non-nil when VALUE encodes as a JSON scalar."
  (not (or (vectorp value) (and (consp value) (keywordp (car value))))))

(defun easel-json--pretty (value indent)
  "Pretty-print VALUE at INDENT."
  (let ((inner (concat indent "  ")))
    (cond
     ((and (vectorp value) (seq-every-p #'easel-json--scalar-p value))
      (concat "[" (mapconcat #'easel-json-encode value ", ") "]"))
     ((vectorp value)
      (concat "[\n" inner
              (mapconcat (lambda (v) (easel-json--pretty v inner)) value (concat ",\n" inner))
              "\n" indent "]"))
     ((and (consp value) (keywordp (car value))
           (cl-loop for (_ v) on value by #'cddr always (easel-json--scalar-p v))
           (< (length (easel-json-encode value)) 100))
      (concat "{"
              (mapconcat (lambda (pair)
                           (concat (easel-json-encode (easel-key-name (car pair))) ": "
                                   (easel-json-encode (cdr pair))))
                         (cl-loop for (k v) on value by #'cddr collect (cons k v))
                         ", ")
              "}"))
     ((and (consp value) (keywordp (car value)))
      (concat "{\n" inner
              (mapconcat (lambda (pair)
                           (concat (easel-json-encode (easel-key-name (car pair))) ": "
                                   (easel-json--pretty (cdr pair) inner)))
                         (cl-loop for (k v) on value by #'cddr collect (cons k v))
                         (concat ",\n" inner))
              "\n" indent "}"))
     ((null value) "{}")
     (t (easel-json-encode value)))))

(defun easel-object-p (value)
  "Non-nil when VALUE is a JSON object (a keyword plist, or nil)."
  (or (null value) (and (consp value) (keywordp (car value)))))

(defun easel-json-canonical (value)
  "Return VALUE with every object's keys sorted, recursively."
  (cond
   ((vectorp value) (vconcat (mapcar #'easel-json-canonical value)))
   ((and (consp value) (keywordp (car value)))
    (let (pairs)
      (while value
        (push (cons (car value) (easel-json-canonical (cadr value))) pairs)
        (setq value (cddr value)))
      (cl-loop for (k . v) in (sort pairs (lambda (a b)
                                            (string< (symbol-name (car a))
                                                     (symbol-name (car b)))))
               append (list k v))))
   (t value)))

(defun easel-content-hash (value)
  "Return \"sha256:HEX\" of VALUE's canonical compact JSON encoding."
  (concat "sha256:"
          (secure-hash 'sha256
                       (encode-coding-string
                        (easel-json-encode (easel-json-canonical value)) 'utf-8))))

(defun easel-true-p (value)
  "Non-nil when JSON VALUE is truthy (not nil, `:false', `:null', 0 or \"\")."
  (not (or (memq value '(nil :false :null)) (eql value 0) (equal value ""))))

(defvar easel--key-cache (make-hash-table :test 'equal :size 512)
  "Field name -> keyword, so hot loops never intern.")

(defun easel-key (name)
  "Return the plist keyword for JSON key or field NAME (a string or symbol)."
  (cond ((keywordp name) name)
        ((symbolp name) (intern (concat ":" (symbol-name name))))
        (t (or (gethash name easel--key-cache)
               (puthash name (intern (concat ":" name)) easel--key-cache)))))

(defun easel-key-name (key)
  "Return the JSON name of plist KEY."
  (substring (symbol-name key) 1))

(defun easel-plist-keys (plist)
  "Return the keys of PLIST in order."
  (cl-loop for (k _) on plist by #'cddr collect k))

(defun easel-plist-put (plist key value)
  "Return a copy of PLIST with KEY set to VALUE (appended when new)."
  (if (plist-member plist key)
      (cl-loop for (k v) on plist by #'cddr
               append (list k (if (eq k key) value v)))
    (append plist (list key value))))

(defun easel-seq-list (value)
  "Return VALUE (a vector or list) as a list."
  (if (vectorp value) (append value nil) value))

(defun easel-path (&rest parts)
  "Join PARTS into a JSON pointer such as \"/layer/0/mark\"."
  (mapconcat (lambda (part) (format "/%s" part)) parts ""))

(provide 'easel-core)
;;; easel-core.el ends here
