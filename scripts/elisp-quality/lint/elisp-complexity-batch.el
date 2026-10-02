;;; elisp-complexity-batch.el --- per-function cyclomatic/cognitive complexity report -*- lexical-binding: t; -*-

;; Drives two vendored, non-MELPA tree-sitter-based tools over a list of
;; files and prints one JSON object per top-level `defun'-shaped form:
;;   {"file":..,"name":..,"line":..,"cyclomatic":N,"cognitive":N}
;;
;; The two tools use two DIFFERENT, mutually incompatible tree-sitter
;; stacks and must be loaded/driven differently:
;;
;;   codemetrics (vendor/codemetrics/) uses the OLD third-party
;;   `tree-sitter'/`tsc' package (emacs-tree-sitter org) -- a dynamic
;;   module (tsc-dyn) plus a per-language grammar bundle
;;   (tree-sitter-langs). That package's own README says outright: "For
;;   Emacs 29+, please use the built-in integration instead of this
;;   package" -- but codemetrics itself was never ported off it, so it's
;;   what we have to drive. `codemetrics-complexity' is toggled between
;;   `cyclomatic' and `cognitive' (both metrics come from the same tool).
;;
;;   cognitive-complexity (vendor/cognitive-complexity/) uses Emacs's
;;   OWN built-in `treesit' (29+) and a `elisp' grammar installed via
;;   `treesit-install-language-grammar' (Wilfred/tree-sitter-elisp).
;;   Also computes both metrics via `cognitive-complexity-metric'.
;;
;; Neither tool ships a real "list every top-level function and its
;; score" batch entry point -- codemetrics-buffer/cognitive-complexity-buffer
;; return a whole-buffer node-score list, and the tools' own "method"
;; display scope (codemetrics--display-nodes / cognitive-complexity--display-nodes)
;; only knows generic node types (function_declaration, method, ...) that
;; don't exist in either elisp tree-sitter grammar. So this driver finds
;; top-level `(defun ...)'-family forms itself (plain `forward-sexp'
;; walking, no tree-sitter needed for that part) and calls each tool's
;; own `-region' entry point (a real, documented, non-interactive
;; function) on each form's buffer substring -- reusing the tools'
;; actual scoring logic without depending on their broken display-scope
;; node-type list.

(defvar elisp-complexity-batch--defun-heads
  '("defun" "defmacro" "defsubst" "cl-defun" "cl-defmacro" "cl-defmethod"
    "cl-defgeneric" "defadvice" "define-minor-mode" "define-derived-mode")
  "Symbol heads treated as a scoreable top-level function/macro definition.")

(defun elisp-complexity-batch--top-level-forms (buffer)
  "Return a list of (NAME LINE BEG END) for each top-level defun-shaped form in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (let ((beg (point)))
          (condition-case nil
              (progn
                (forward-sexp)
                (let* ((end (point))
                       (form (save-excursion
                               (goto-char beg)
                               (ignore-errors (read (current-buffer)))))
                       (head (and (consp form) (symbolp (car form)) (symbol-name (car form))))
                       (name (and head
                                  (member head elisp-complexity-batch--defun-heads)
                                  (consp (cdr form))
                                  (symbolp (cadr form))
                                  (symbol-name (cadr form)))))
                  (when name
                    (push (list name (line-number-at-pos beg) beg end) forms))))
            (scan-error (goto-char (point-max))))))
      (nreverse forms))))

(defun elisp-complexity-batch--codemetrics-score (content metric)
  "Score CONTENT (a string, one top-level form) for METRIC via codemetrics."
  (let ((codemetrics-complexity metric))
    (with-temp-buffer
      (emacs-lisp-mode)
      (tree-sitter-mode 1)
      (car (codemetrics-analyze content 'emacs-lisp-mode)))))

(defun elisp-complexity-batch--cognitive-complexity-score (content metric)
  "Score CONTENT (a string, one top-level form) for METRIC via cognitive-complexity."
  (let ((cognitive-complexity-metric metric))
    (with-temp-buffer
      (emacs-lisp-mode)
      (treesit-parser-create 'elisp)
      (car (cognitive-complexity-analyze content 'emacs-lisp-mode)))))

(defun elisp-complexity-batch--json-escape (s)
  (replace-regexp-in-string "\"" "\\\\\"" (replace-regexp-in-string "\\\\" "\\\\\\\\" s)))

(defun elisp-complexity-batch--run (files)
  "Analyze FILES, printing one JSON line per top-level defun-shaped form."
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (dolist (entry (elisp-complexity-batch--top-level-forms (current-buffer)))
        (cl-destructuring-bind (name line beg end) entry
          (let* ((content (buffer-substring-no-properties beg end))
                 (cyclomatic (elisp-complexity-batch--codemetrics-score content 'cyclomatic))
                 (cognitive (elisp-complexity-batch--cognitive-complexity-score content 'cognitive)))
            (princ (format "{\"file\":\"%s\",\"name\":\"%s\",\"line\":%d,\"cyclomatic\":%d,\"cognitive\":%d}\n"
                            (elisp-complexity-batch--json-escape file)
                            (elisp-complexity-batch--json-escape name)
                            line cyclomatic cognitive))))))))

(elisp-complexity-batch--run command-line-args-left)

(provide 'elisp-complexity-batch)
;;; elisp-complexity-batch.el ends here
