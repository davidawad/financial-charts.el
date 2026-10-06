;;; financial-chart-test-support.el --- golden helper for the eas template tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Goldens of financial-chart's templates drawn by eas live in
;; test/golden/eas-templates/.  Set EAS_UPDATE_GOLDEN=1 to rewrite
;; them, then review the diff.  (eas.el keeps its own goldens for the
;; engine and its generic templates.)

;;; Code:

(require 'ert)

(defconst financial-chart-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The financial-chart repository root.")

(defun financial-chart-test-golden (name actual)
  "Compare string ACTUAL with golden NAME under test/golden/eas-templates/."
  (let ((file (expand-file-name name (expand-file-name "test/golden/eas-templates"
                                                       financial-chart-test-root))))
    (if (or (getenv "EAS_UPDATE_GOLDEN") (not (file-exists-p file)))
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert actual))
          (unless (getenv "EAS_UPDATE_GOLDEN")
            (ert-fail (format "Golden %s was missing and has been written; review and rerun"
                              name))))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     actual)))))

(provide 'financial-chart-test-support)
;;; financial-chart-test-support.el ends here
