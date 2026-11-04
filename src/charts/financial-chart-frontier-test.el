;;; financial-chart-frontier-test.el --- efficient frontier tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart-frontier)

(defconst financial-chart-frontier-test--valid
  '((:kind :frontier :risk 2 :return 4)
    (:kind :frontier :risk 5 :return 8)
    (:kind :frontier :risk 9 :return 11)
    (:kind :method :risk 4 :return 6 :label "Minimum variance")
    (:kind :method :risk 7 :return 9 :label "Maximum Sharpe")
    (:kind :current :risk 6 :return 5.5 :label "Current portfolio")))

(ert-deftest financial-chart-frontier-validates-and-registers-kind ()
  (should (financial-chart-validate 'frontier financial-chart-frontier-test--valid))
  (let ((entry (financial-chart--kind 'frontier)))
    (should (eq (plist-get entry :shape) 'frontier))
    (should (equal (plist-get entry :template) "efficient-frontier"))
    (should (equal (length (plist-get (alist-get 'frontier financial-chart-shapes) :example)) 6))))

(ert-deftest financial-chart-frontier-rejects-invalid-risk-order-and-labels ()
  (let ((err (should-error
              (financial-chart-validate
               'frontier
               (cons (nth 1 financial-chart-frontier-test--valid)
                     (cons '(:kind :frontier :risk 1 :return 3)
                           (nthcdr 2 financial-chart-frontier-test--valid))))
              :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "risk_not_ascending"))
    (should (= (plist-get (cddr err) :index) 1)))
  (let ((err (should-error
              (financial-chart-validate
               'frontier
               (cons '(:kind :method :risk 4 :return 6)
                     (cdr financial-chart-frontier-test--valid)))
              :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :field) "label"))))

(ert-deftest financial-chart-frontier-check-returns-structured-error ()
  (should (equal (plist-get (financial-chart-check 'frontier 'bad) :code)
                 "not_a_list")))

(provide 'financial-chart-frontier-test)
;;; financial-chart-frontier-test.el ends here
