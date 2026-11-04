;;; financial-chart-risk-contribution-test.el --- Portfolio risk contribution bars -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-risk-contribution)

(defconst financial-chart-risk-contribution-test--data
  '(:portfolio-vol 0.12
    :contributions ((:name "AAPL" :class "equity" :factor "market" :contribution 0.08)
                    (:name "TLT" :class "rates" :factor "duration" :contribution 0.06)
                    (:name "GLD" :class "commodity" :factor "market" :contribution -0.02))))

(ert-deftest financial-chart-risk-contribution-is-discoverable-and-valid ()
  (let ((description (financial-chart-describe-kind 'risk-contribution)))
    (should (eq (plist-get description :shape) 'risk-contributions))
    (should (equal (plist-get description :template) "diverging-bars"))
    (should (plist-get description :template-defined)))
  (should (eq (financial-chart-check 'risk-contribution
                                     financial-chart-risk-contribution-test--data)
              t)))

(ert-deftest financial-chart-risk-contribution-requires-signed-sum-to-match-vol ()
  (let ((bad (copy-tree financial-chart-risk-contribution-test--data)))
    (setf (plist-get (car (plist-get bad :contributions)) :contribution) 0.09)
    (let ((error (financial-chart-check 'risk-contribution bad)))
      (should (equal (plist-get error :code) "contributions_do_not_sum"))
      (should (equal (plist-get error :field) "portfolio-vol")))))

(ert-deftest financial-chart-risk-contribution-groups-through-bars-adapter ()
  (let* ((rows (plist-get (financial-chart-eas-bindings
                           'risk-contribution financial-chart-risk-contribution-test--data
                           '(:by class)) :data))
         (labels (mapcar (lambda (row) (plist-get row :label)) rows))
         (values (mapcar (lambda (row) (plist-get row :value)) rows)))
    (should (equal labels '("equity" "rates" "commodity")))
    (should (equal values '(0.08 0.06 -0.02))))
  (let ((plan (financial-chart-explain 'risk-contribution
                                       financial-chart-risk-contribution-test--data
                                       :by 'factor :backend 'text)))
    (should (eq (plist-get plan :valid) t))
    (should (equal (plist-get plan :template) "diverging-bars"))
    (should (equal (plist-get plan :min) -0.02))
    (should (equal (plist-get plan :max) 0.08))))

(ert-deftest financial-chart-risk-contribution-renders-text ()
  (let ((chart (financial-chart-plot 'risk-contribution
                                     financial-chart-risk-contribution-test--data
                                     :by 'factor :backend 'text)))
    (should (stringp chart))
    (should (string-match-p "market" chart))
    (should (string-match-p "duration" chart))))

(provide 'financial-chart-risk-contribution-test)
;;; financial-chart-risk-contribution-test.el ends here
