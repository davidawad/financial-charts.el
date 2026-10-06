;;; financial-chart-multi-test.el --- Tests for multi-series charts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-multi-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-multi-test--dir))
(require 'financial-chart)

(defconst financial-chart-multi-test--data
  '(("AAPL" . ((1 100) (2 104) (3 102) (4 110)))
    ("SPY" . ((1 50) (2 51) (3 51.5) (4 53))))
  "Two series with different starting values, for normalization fixtures.")

(ert-deftest financial-chart-multi-test-registry-and-explain ()
  (let ((plan (financial-chart-explain 'multi financial-chart-multi-test--data
                                       :backend 'svg :normalize 100 :unit "%")))
    (should (eq (plist-get plan :shape) 'multi-series))
    (should (equal (plist-get plan :template) "multi"))
    (should (eq (plist-get plan :valid) t))
    (should (= (plist-get plan :points) 8))
    (should (= (plist-get plan :min) 100))
    (should (< (abs (- (plist-get plan :max) 110)) 1e-8))))

(ert-deftest financial-chart-multi-test-validator-indexes-bad-series ()
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-validate
                    'multi '(("AAPL" . (1 2)) ("BROKEN" . (1 "bad"))))
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)
    (should (equal (plist-get (nthcdr 2 err) :index) 1))))

(ert-deftest financial-chart-multi-test-normalization-and-zero-base ()
  (let ((prepared (financial-chart-multi--prepare financial-chart-multi-test--data 100)))
    (should (= (car (cdr (car prepared))) 100))
    (should (< (abs (- (car (last (cdr (car prepared)))) 110)) 1e-8))
    (should (= (car (cdr (cadr prepared))) 100))
    (should (< (abs (- (car (last (cdr (cadr prepared)))) 106)) 1e-8)))
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-multi--prepare
                    '(("A" . (1 2)) ("B" . (0 1))) 100)
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)
    (should (equal (plist-get (nthcdr 2 err) :index) 1))))

(ert-deftest financial-chart-multi-test-explain-rejects-zero-normalization-base ()
  (let ((plan (financial-chart-explain 'multi '(("X" . (0 10)))
                                       :backend 'text :normalize 100)))
    (should (string-match-p "starts at zero" (plist-get (plist-get plan :valid) :message)))
    (should (equal (plist-get (plist-get plan :valid) :code) "zero_base"))
    (should-not (plist-member plan :min)))
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-plot 'multi '(("X" . (0 10)))
                                         :backend 'text :normalize 100)
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)))

(ert-deftest financial-chart-multi-test-rejects-invalid-normalize-type ()
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-plot 'multi financial-chart-multi-test--data
                                         :backend 'text :normalize "100")
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)
    (should (string-match-p "must be nil or a number" (cadr err)))))

(provide 'financial-chart-multi-test)
;;; financial-chart-multi-test.el ends here
