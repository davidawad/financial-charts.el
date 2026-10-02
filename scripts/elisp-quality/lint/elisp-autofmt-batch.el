;;; elisp-autofmt-batch.el --- non-mutating elisp-autofmt --check driver -*- lexical-binding: t; -*-

;; gofmt --check / rustfmt --check equivalent: for each file passed on the
;; command line, run `elisp-autofmt-buffer' in a scratch buffer and compare
;; the result against the file's on-disk content WITHOUT EVER SAVING -- the
;; buffer is killed unsaved after each file, so this never mutates a file on
;; disk (see check-elisp-format.py's own docstring for why that matters:
;; this is a new, unproven-on-this-tree gate and a silent mass-reformat
;; would be an unreviewed diff across most of the repo).
;;
;; elisp-autofmt-python-bin, elisp-autofmt-cache-directory, and load-path
;; are expected to already be set via --eval before this file is loaded
;; (see check-elisp-format.py's `_run_batch').
;;
;; Output (stdout), one line per file with a verdict:
;;   DIFF <file>          -- would be reformatted
;;   ERROR <file>: <msg>  -- elisp-autofmt could not format it at all
;;   (files that are already compliant print nothing)
;; plus a final summary line. Always exits 0 -- this driver only reports;
;; WARN-vs-block posture lives in the Python wrapper, not here.

(require 'elisp-autofmt)

(defun elisp-autofmt-batch--check-file (file)
  "Return `diff', `ok', or (`error' . MSG) for FILE, without writing to disk."
  (let ((original nil))
    (with-temp-buffer
      (insert-file-contents file)
      (setq original (buffer-string)))
    (with-current-buffer (find-file-noselect file)
      (unwind-protect
          (progn
            (setq buffer-undo-list t)
            (condition-case err
                (progn
                  (elisp-autofmt-buffer)
                  (if (string-equal original (buffer-substring-no-properties (point-min) (point-max)))
                      'ok
                    'diff))
              (error (cons 'error (error-message-string err)))))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(defun elisp-autofmt-batch--run (files)
  "Check FILES, printing a DIFF/ERROR line per non-compliant file."
  (let ((diff-count 0)
        (error-count 0))
    (dolist (file files)
      (pcase (elisp-autofmt-batch--check-file file)
        ('diff
         (setq diff-count (1+ diff-count))
         (princ (format "DIFF %s\n" file)))
        (`(error . ,msg)
         (setq error-count (1+ error-count))
         (princ (format "ERROR %s: %s\n" file msg)))
        ('ok nil)))
    (princ (format "SUMMARY checked=%d diff=%d error=%d\n" (length files) diff-count error-count))))

(elisp-autofmt-batch--run command-line-args-left)

(provide 'elisp-autofmt-batch)
;;; elisp-autofmt-batch.el ends here
