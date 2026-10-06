;;; financial-chart-returns-test.el --- Returns chart tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-returns-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-returns-test--dir))
(require 'financial-chart)

(defconst financial-chart-returns-test--series
  '(("Jan" 100) ("Feb" 120) ("Mar" 90) ("Apr" 105) ("May" 80) ("Jun" 110)))

(ert-deftest financial-chart-returns-drawdowns-preserve-coordinates ()
  (let ((drawdowns (financial-chart-drawdowns financial-chart-returns-test--series)))
    (should (= (length drawdowns) 6))
    (should (equal (car (nth 4 drawdowns)) "May"))
    (should (= (cdr (nth 0 drawdowns)) 0))
    (should (= (cdr (nth 2 drawdowns)) -0.25))
    (should (= (cdr (nth 4 drawdowns)) (- (/ 1.0 3))))))

(ert-deftest financial-chart-returns-drawdowns-use-source-indexes ()
  (should (equal (financial-chart-drawdowns '(100 80 90))
                 '((0 . 0.0) (1 . -0.2) (2 . -0.1))))
  (should (equal (financial-chart-drawdowns '((a 100) (b nil) (c 80)))
                 '((a . 0.0) (c . -0.2))))
  (let ((err (should-error (financial-chart-drawdowns '(0 1))
                           :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 0))))

(ert-deftest financial-chart-returns-calculates-consecutive-simple-returns ()
  (let ((returns (financial-chart-returns financial-chart-returns-test--series)))
    (should (= (length returns) 5))
    (should (= (nth 0 returns) 0.2))
    (should (= (nth 1 returns) -0.25))
    (should (= (nth 2 returns) (/ 1.0 6)))
    (should (= (nth 3 returns) (- (/ 25.0 105))))
    (should (= (nth 4 returns) 0.375)))
  (should (equal (financial-chart-returns '((a 100) (b nil) (c 125)))
                 '(0.25)))
  (should-not (financial-chart-returns '(100)))
  (let ((err (should-error (financial-chart-returns '(5 0 2))
                           :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 2))))

(ert-deftest financial-chart-returns-histogram-bins-cover-range-and-counts ()
  (let* ((bins (financial-chart-histogram-bins '(-1 -0.5 0.1 0.6 1.0) 4)))
    (should (= (length bins) 4))
    (should (= (caar bins) -1))
    (should (= (cadr (car (last bins))) 1))
    (should (equal (mapcar #'caddr bins) '(1 1 1 2)))
    (should (= (apply #'+ (mapcar #'caddr bins)) 5)))
  (should (equal (mapcar #'caddr (financial-chart-histogram-bins '(0.1 0.1 0.1) 4))
                 '(0 0 3 0)))
  (should (= (length (financial-chart-histogram-bins '(1 2)))
             financial-chart-returns-default-bins))
  (should-not (financial-chart-histogram-bins nil))
  (should-error (financial-chart-histogram-bins '(1 2) 0)
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-returns-kinds-validate-their-math ()
  (let ((err (should-error (financial-chart-validate 'drawdown '(100 -5 90))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "negative_price"))
    (should (= (plist-get (cddr err) :index) 1)))
  (let ((err (should-error (financial-chart-validate 'histogram '(100 0 90))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "zero_price"))
    (should (= (plist-get (cddr err) :index) 2))))

(ert-deftest financial-chart-returns-registered-and-doctor-ready ()
  (dolist (kind '(drawdown histogram))
    (let ((description (financial-chart-describe-kind kind)))
      (should (plist-get description :template-defined))
      (should (eq t (financial-chart-validate kind
                                              (plist-get description :example))))))
  (should (stringp (financial-chart-plot 'drawdown '(100 90) :backend 'text)))
  (should (string-match-p "kind drawdown renders" (format "%S" (financial-chart-doctor-checks))))
  (should (string-match-p "kind histogram renders" (format "%S" (financial-chart-doctor-checks)))))

(provide 'financial-chart-returns-test)
;;; financial-chart-returns-test.el ends here
