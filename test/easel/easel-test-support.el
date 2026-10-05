;;; easel-test-support.el --- shared helpers for easel ERT tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Goldens live in test/easel/golden/.  Set EASEL_UPDATE_GOLDEN=1 to
;; rewrite them, then review the diff.

;;; Code:

(require 'ert)
(require 'easel-core)

(defconst easel-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (directory-file-name
                          (file-name-directory (or load-file-name buffer-file-name))))))
  "The repository root.")

(defun easel-test-file (&rest parts)
  "Return the absolute repository file joined from PARTS."
  (expand-file-name (string-join parts "/") easel-test-root))

(defun easel-test-golden (name actual)
  "Compare string ACTUAL with golden NAME under test/easel/golden/."
  (let ((file (easel-test-file "test/easel/golden" name)))
    (if (or (getenv "EASEL_UPDATE_GOLDEN") (not (file-exists-p file)))
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert actual))
          (unless (getenv "EASEL_UPDATE_GOLDEN")
            (ert-fail (format "Golden %s was missing and has been written; review and rerun"
                              name))))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     actual)))))

(defmacro easel-test-should-code (code &rest body)
  "Assert BODY signals an `easel-error' with reason CODE; return its plist."
  (declare (indent 1))
  `(let ((err (should-error (progn ,@body) :type 'easel-error)))
     (should (equal (plist-get (easel-error-plist err) :code) ,code))
     (easel-error-plist err)))

(defvar easel-test-chart-program "chart"
  "The bin/chart executable used as the static oracle.")

(defun easel-test-skip (reason)
  "Skip the current test with REASON, printed so `make test' shows it.
With TEST_SKIP_LOG set, also append \"TEST: REASON\" to that file."
  (let ((name (ert-test-name (ert-running-test))))
    (message "SKIP %s: %s" name reason)
    (when-let* ((log (getenv "TEST_SKIP_LOG")))
      (write-region (format "%s: %s\n" name reason) nil log t 'silent))
    (ert-skip reason)))

(defun easel-test-require-chart ()
  "Skip the current test, with a visible reason, unless bin/chart is on PATH."
  (unless (executable-find easel-test-chart-program)
    (easel-test-skip (format "bin/chart (%s) not on PATH; install bin/chart to run this oracle check"
                             easel-test-chart-program))))

(provide 'easel-test-support)
;;; easel-test-support.el ends here
