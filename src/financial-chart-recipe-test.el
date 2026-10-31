;;; financial-chart-recipe-test.el --- the MELPA recipe stays one well-formed form -*- lexical-binding: t; -*-

;;; Commentary:

;; Parallel branches append to recipes/financial-charts; a merge once left
;; an unbalanced line that `read' silently dropped.  Every runtime data
;; directory the 3D package reads must also ship.

;;; Code:

(require 'ert)

(defconst financial-chart-recipe-test--file
  (expand-file-name "../recipes/financial-charts"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest financial-chart-recipe-is-one-form ()
  (with-temp-buffer
    (insert-file-contents financial-chart-recipe-test--file)
    (let ((recipe (read (current-buffer))))
      (should (eq (car recipe) 'financial-charts))
      (skip-chars-forward " \t\n")
      (should (eobp)))))

(ert-deftest financial-chart-recipe-ships-3d-fixtures ()
  (let* ((recipe (with-temp-buffer
                   (insert-file-contents financial-chart-recipe-test--file)
                   (read (current-buffer))))
         (files (plist-get (cdr recipe) :files))
         (root (expand-file-name "../test/fixtures/3d"
                                 (file-name-directory financial-chart-recipe-test--file))))
    (dolist (dir (directory-files root nil "\\`[^.]"))
      (should (assoc (concat "test/fixtures/3d/" dir) (seq-filter #'consp files))))))

(provide 'financial-chart-recipe-test)
;;; financial-chart-recipe-test.el ends here
