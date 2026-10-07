;;; melpa-layout-check.el --- drive the flat MELPA layout check -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by scripts/melpa-layout-check with the flat package
;; directory (FC_FLAT) and eas on `load-path'.  Loads financial-chart
;; from there, with `default-directory' elsewhere, and renders every
;; catalog template's example, every composed-chart example and every
;; indicator example in SVG and text.  Exits 1 on the first problem.

;;; Code:

(setq load-prefer-newer t)
(defvar fc-flat (file-name-as-directory (getenv "FC_FLAT")))
(setq default-directory temporary-file-directory)
(require 'financial-chart)
(require 'financial-chart-eas-catalog)
(require 'eas-agent)

(defvar fc-failures 0)
(defvar fc-checks 0)

(defun fc-check (what ok &optional detail)
  "Report WHAT; count it as a failure unless OK.  DETAIL says why."
  (setq fc-checks (1+ fc-checks))
  (unless ok (setq fc-failures (1+ fc-failures)))
  (princ (format "%s %s%s\n" (if ok "ok  " "FAIL") what (if detail (format ": %s" detail) ""))))

(defun fc-render (what fn)
  "Report WHAT, which renders by calling FN with a backend symbol."
  (dolist (backend '(svg text))
    (let ((out (condition-case err (funcall fn backend) (error err))))
      (fc-check (format "%s %s" what backend)
                (and (stringp out) (> (length out) 0)
                     (or (eq backend 'text) (string-match-p "<svg" out)))
                (unless (stringp out) (format "%S" out))))))

(fc-check "financial-chart-root is the flat directory"
          (file-equal-p financial-chart-root fc-flat) financial-chart-root)
(fc-check "financial-chart loaded from the flat directory"
          (file-in-directory-p (locate-library "financial-chart") fc-flat)
          (locate-library "financial-chart"))
(fc-check "no template failed to load" (null eas-template-load-errors)
          (format "%S" eas-template-load-errors))

(dolist (name (eas-template-names))
  (when (eas-template-example-file (eas-template-get name))
    (fc-render (format "template %s" name)
               (lambda (backend)
                 (let ((env (eas-agent "render" name :data (eas-template-example name)
                                       :backend (symbol-name backend))))
                   (if (eq (plist-get env :ok) t) (plist-get (plist-get env :data) :output)
                     (list :reason (plist-get env :reason) :evidence (plist-get env :evidence))))))))

(dolist (file (directory-files financial-chart-compose-examples-directory nil "\\.json\\'"))
  (let ((style (file-name-sans-extension file)))
    (fc-render (format "compose %s" style)
               (lambda (backend)
                 (financial-chart-compose-render (financial-chart-compose-example style)
                                                 :backend backend)))))

(dolist (name (financial-chart-catalog-examples))
  (fc-render (format "indicator %s" name)
             (lambda (backend)
               (financial-chart-compose-render (financial-chart-catalog-example name)
                                               :backend backend))))

(let ((failing (seq-filter (lambda (row) (eq (plist-get row :status) 'fail))
                           (financial-chart-doctor-checks))))
  (fc-check "doctor has no failing rows" (null failing)
            (mapconcat (lambda (row) (format "%s" (plist-get row :name))) failing ", ")))

(princ (format "%d checks, %d failure(s)\n" fc-checks fc-failures))
(kill-emacs (if (zerop fc-failures) 0 1))

;;; melpa-layout-check.el ends here
