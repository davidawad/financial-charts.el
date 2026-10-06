;;; financial-chart-payoff-test.el --- payoff renderer tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart)

(defconst financial-chart-payoff-test--payoff
  '((90 50) (95 0) (100 -100) (105 0) (110 50)))

(defconst financial-chart-payoff-test--curves
  '(("T+0" . ((90 30) (100 -20) (110 30)))
    ("T+15" . ((90 20) (100 -5) (110 40)))
    ("T+30" . ((90 10) (100 10) (110 20)))))

(ert-deftest financial-chart-payoff-test-curves-reject-different-price-grids ()
  (let ((err (should-error
              (financial-chart-validate
               'payoff-curves
               '(("T+0" . ((90 1) (100 2)))
                 ("T+15" . ((90 1) (105 2)))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 1))
    (should (equal (plist-get (cddr err) :code) "grid_mismatch"))))

(ert-deftest financial-chart-payoff-test-curves-locate-bad-inner-points ()
  (let ((err (should-error
              (financial-chart-validate
               'payoff-curves '(("T+0" . ((90 1) (100 2))) ("T+15" . ((90 1) (100 "x")))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 1))
    (should (equal (plist-get (cddr err) :field) "payoff[1].y"))
    (should (equal (plist-get (cddr err) :code) "not_a_number"))))

(provide 'financial-chart-payoff-test)
;;; financial-chart-payoff-test.el ends here
