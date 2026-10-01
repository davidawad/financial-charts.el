;;; financial-chart-batch.el --- JSON command line for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The package for callers outside Emacs.  bin/financial-chart runs
;;
;;   emacs -Q --batch -L DIR -l financial-chart-batch -f financial-chart-batch-main CMD [FILE]
;;
;; CMD is one of:
;;
;;   render   [FILE|-]  chart SPEC (JSON) -> the chart, text or SVG, on stdout
;;   explain  [FILE|-]  chart SPEC -> the plan as JSON (`financial-chart-explain')
;;   validate [FILE|-]  chart SPEC -> {"ok":true} or the error envelope
;;   example  KIND      a ready-to-edit SPEC for KIND, as JSON
;;   kinds              every kind with its shape and doc, as JSON
;;   describe           the whole package as JSON (`financial-chart-describe')
;;   doctor             health rows as JSON
;;
;; A SPEC is a JSON object: {"kind": "area", "data": [...], ...props},
;; props being the keyword arguments of `financial-chart-plot' without
;; the colon ("backend": "text", "width": 60, "unit": "$", "title": ...).
;; Data per shape: series [1,2,3] or [[x,y],...]; payoff [[price,pnl],...];
;; labeled [["AAPL",1200],...] or {"AAPL":1200}; ohlc
;; [{"open":..,"high":..,"low":..,"close":..,"volume":..,"time":..},...].
;;
;; Failures print {"ok":false,"error":{"code","message"}} on stdout and
;; exit 1.

;;; Code:

(require 'json)
(require 'financial-chart)

(defconst financial-chart-batch--symbol-props '(:backend)
  "Props whose JSON string value is a Lisp symbol.")

(defun financial-chart-batch--keyword (key)
  "JSON object KEY (a symbol) as a keyword."
  (intern (concat ":" (symbol-name key))))

(defun financial-chart-batch--data (shape data)
  "JSON-parsed DATA converted to SHAPE's Lisp form."
  (pcase shape
    ('ohlc (mapcar (lambda (bar)
                     (cl-loop for (k . v) in bar
                              append (list (financial-chart-batch--keyword k) v)))
                   data))
    ('labeled (mapcar (lambda (p)
                        (if (and (consp p) (symbolp (car p)) (not (listp (cdr p))))
                            (cons (symbol-name (car p)) (cdr p))
                          (cons (car p) (cadr p))))
                      data))
    (_ data)))

(defun financial-chart-batch-spec (json)
  "Chart spec plist from JSON (an alist from `json-parse-string')."
  (let* ((kind-name (or (alist-get 'kind json)
                        (signal 'financial-chart-error
                                (list "spec needs \"kind\"; run `financial-chart kinds'"
                                      :code "bad_request"))))
         (kind (intern kind-name))
         (shape (plist-get (financial-chart--kind kind) :shape)))
    (append
     (list :kind kind :data (financial-chart-batch--data shape (alist-get 'data json)))
     (cl-loop for (k . v) in json
              for key = (financial-chart-batch--keyword k)
              unless (memq k '(kind data))
              append (list key (if (and (memq key financial-chart-batch--symbol-props)
                                        (stringp v))
                                   (intern v)
                                 v))))))

(defun financial-chart-batch--read (file)
  "Parse the JSON in FILE (\"-\" or nil = stdin)."
  (with-temp-buffer
    (insert-file-contents (if (or (null file) (equal file "-")) "/dev/stdin" file))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object :json-false)))

(defun financial-chart-batch--json (value)
  "Print VALUE as JSON on stdout."
  (princ (json-encode value))
  (terpri))

(defun financial-chart-batch--spec-props (spec)
  "SPEC's render props (everything but :kind and :data)."
  (financial-chart-plot--plist-drop spec :kind :data))

(defun financial-chart-batch--example (kind)
  "A SPEC for KIND built from its shape's example, as a JSON-able alist."
  (let* ((d (financial-chart-describe-kind kind))
         (ex (plist-get d :example)))
    `((kind . ,(symbol-name kind))
      (data . ,(apply #'vector
                      (pcase (plist-get d :shape)
                        ('ohlc (mapcar (lambda (b)
                                         (cl-loop for (k v) on b by #'cddr
                                                  collect (cons (substring (symbol-name k) 1) v)))
                                       ex))
                        ('labeled (mapcar (lambda (p) (vector (car p) (cdr p))) ex))
                        (_ (mapcar (lambda (p) (if (consp p) (apply #'vector p) p)) ex)))))
      (backend . "text"))))

(defun financial-chart-batch--error-code (err)
  "Stable code for ERR: its :code, else derived from its symbol."
  (or (plist-get (cddr err) :code)
      (pcase (car err)
        ((pred (lambda (s) (string-prefix-p "market-data" (symbol-name s))))
         (replace-regexp-in-string "-" "_" (symbol-name (car err))))
        ('json-parse-error "bad_json")
        ('file-missing "file_missing")
        (_ "error"))))

(defun financial-chart-batch-run (cmd &optional arg)
  "Run batch command CMD with ARG; return the process exit status."
  (condition-case err
      (progn
        (pcase cmd
          ("render"
           (let ((out (financial-chart-plot-spec
                       (financial-chart-batch-spec (financial-chart-batch--read arg)))))
             (princ (if (stringp out) (substring-no-properties out) financial-chart-empty-text))
             (terpri)))
          ("explain"
           (let ((spec (financial-chart-batch-spec (financial-chart-batch--read arg))))
             (financial-chart-batch--json
              (apply #'financial-chart-explain (plist-get spec :kind) (plist-get spec :data)
                     (financial-chart-batch--spec-props spec)))))
          ("validate"
           (let ((spec (financial-chart-batch-spec (financial-chart-batch--read arg))))
             (financial-chart-validate (plist-get spec :kind) (plist-get spec :data))
             (financial-chart-batch--json '((ok . t)))))
          ("example"
           (financial-chart-batch--json
            (financial-chart-batch--example (intern (or arg "area")))))
          ("kinds" (financial-chart-batch--json (plist-get (financial-chart-describe) :kinds)))
          ("describe" (financial-chart-batch--json (financial-chart-describe)))
          ("doctor" (financial-chart-batch--json (apply #'vector (financial-chart-doctor-checks))))
          (_ (signal 'financial-chart-error
                     (list (format "unknown command %S; use render, explain, validate, example, kinds, describe or doctor" cmd)
                           :code "bad_request"))))
        0)
    (error
     (financial-chart-batch--json
      `((ok . :json-false)
        (error . ((code . ,(financial-chart-batch--error-code err))
                  (message . ,(if (plist-get (cddr err) :code) (cadr err)
                                (error-message-string err)))))))
     1)))

(defun financial-chart-batch-main ()
  "Entry point for `emacs --batch -f financial-chart-batch-main CMD [ARG]'."
  (let ((cmd (pop command-line-args-left))
        (arg (pop command-line-args-left)))
    (kill-emacs (financial-chart-batch-run (or cmd "describe") arg))))

(provide 'financial-chart-batch)
;;; financial-chart-batch.el ends here
