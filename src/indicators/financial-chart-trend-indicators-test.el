;;; financial-chart-trend-indicators-test.el --- Trend indicator tests -*- lexical-binding: t; -*-

(require 'ert)
(defvar financial-chart-trend-indicators-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path
             (expand-file-name ".." financial-chart-trend-indicators-test--dir))
(require 'financial-chart-indicator-api)
(require 'financial-chart-trend-indicators)

(defun financial-chart-trend-test--bars (values)
  "Create normalized bars with CLOSE values VALUES."
  (mapcar (lambda (value) (list :close value)) values))

(ert-deftest financial-chart-trend-wma-hand-vector ()
  (let ((values (plist-get
                 (financial-chart-indicator-evaluate
                  'wma (financial-chart-trend-test--bars '(1 2 3 4 5)) 2)
                 :values)))
    (should (equal (car values) nil))
    (should (< (abs (- (nth 1 values) (/ 5.0 3))) 1e-12))
    (should (< (abs (- (nth 4 values) (/ 14.0 3))) 1e-12))))

(ert-deftest financial-chart-trend-dema-hand-vector ()
  (should (equal (plist-get
                  (financial-chart-indicator-evaluate
                   'dema (financial-chart-trend-test--bars '(1 2 3 4 5)) 2)
                  :values)
                 '(nil nil 3.0 4.0 5.0))))

(ert-deftest financial-chart-trend-tema-hand-vector ()
  (should (equal (plist-get
                  (financial-chart-indicator-evaluate
                   'tema (financial-chart-trend-test--bars '(1 2 3 4 5)) 2)
                  :values)
                 '(nil nil nil 4.0 5.0))))

(ert-deftest financial-chart-trend-hma-hand-vector ()
  (should (equal (plist-get
                  (financial-chart-indicator-evaluate
                   'hma (financial-chart-trend-test--bars '(1 2 3 4 5 6 7)) 4)
                  :values)
                  '(nil nil nil nil 5.0 6.0 7.0))))

(ert-deftest financial-chart-trend-kama-hand-vector ()
  (let ((values (plist-get
                 (financial-chart-indicator-evaluate
                  'kama (financial-chart-trend-test--bars '(1 2 3 4))
                  2 2 30)
                 :values)))
    (should (equal (car values) nil))
    (should (equal (cadr values) nil))
    (should (= (nth 2 values) 3.0))
    (should (< (abs (- (nth 3 values) 3.4444444444444446)) 1e-12))))

(ert-deftest financial-chart-trend-ema-based-indicators-recover-after-gap ()
  (should (equal (plist-get
                  (financial-chart-indicator-evaluate
                   'dema (financial-chart-trend-test--bars '(1 2 nil 4 5 6)) 2)
                  :values)
                 '(nil nil nil nil nil 6.0))))

(ert-deftest financial-chart-trend-indicators-reject-invalid-period ()
  (should
   (condition-case nil
       (progn
         (financial-chart-indicator-evaluate
          'wma (financial-chart-trend-test--bars '(1 2)) 0)
         nil)
     (financial-chart-invalid-indicator t))))

(provide 'financial-chart-trend-indicators-test)
;;; financial-chart-trend-indicators-test.el ends here
