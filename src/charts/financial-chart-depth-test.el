;;; financial-chart-depth-test.el --- ERT tests for depth charts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'financial-chart)

(defconst financial-chart-depth-test--book
  '(:bids ((99.5 4.0) (100.0 2.0) (99.0 6.0))
    :asks ((101.0 3.0) (100.5 1.0) (101.5 5.0)))
  "A deliberately unsorted book to check best-price ordering.")

(ert-deftest financial-chart-depth-test-kind-is-registered ()
  (let ((description (financial-chart-describe-kind 'depth)))
    (should (eq (plist-get description :shape) 'order-book))
    (should (plist-get description :template-defined))
    (should (financial-chart-validate 'depth (plist-get description :example))))
  (should (member "depth"
                  (mapcar (lambda (entry) (plist-get entry :kind))
                          (append (plist-get (financial-chart-describe) :kinds) nil))))
  (should (plist-get (financial-chart--data-summary 'order-book
                                                     financial-chart-depth-test--book)
                     :points))
  (should (= 6 (plist-get (financial-chart--data-summary
                           'order-book financial-chart-depth-test--book)
                          :points))))

(ert-deftest financial-chart-depth-test-validates-levels-and-locates-errors ()
  (should (financial-chart-validate 'depth financial-chart-depth-test--book))
  (let ((err (should-error
              (financial-chart-validate
               'depth '(:bids ((100 1)) :asks ((101 1) (102 "large"))))
              :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "not_positive"))
    (should (equal (plist-get (cddr err) :field) "asks.size"))
    (should (= (plist-get (cddr err) :index) 1)))
  (let ((err (should-error
              (financial-chart-validate
               'depth '(:bids ((100 1)) :asks ((101))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 0)))
  (should-error (financial-chart-validate 'depth '(:bids nil))
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-depth-test-rejects-crossed-book-with-index ()
  (let ((err (should-error
              (financial-chart-validate
               'depth '(:bids ((99 1) (101 1)) :asks ((100 1))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 1))
    (should (string-match-p "best bid 101 exceeds best ask 100"
                            (cadr err))))
  (should (financial-chart-validate
           'depth '(:bids ((100 1)) :asks ((100 2))))))

(ert-deftest financial-chart-depth-test-one-sided-and-empty-books ()
  (let ((one-sided '(:bids ((100 2)) :asks nil))
        (empty '(:bids nil :asks nil)))
    (should (stringp (financial-chart-plot 'depth one-sided :backend 'text)))
    (should (stringp (financial-chart-plot 'depth one-sided :backend 'svg)))
    (should (eq t (financial-chart-validate 'depth empty)))))

(ert-deftest financial-chart-depth-test-explain-reports-price-range ()
  (let ((plan (financial-chart-explain 'depth financial-chart-depth-test--book
                                       :backend 'svg)))
    (should (eq (plist-get plan :shape) 'order-book))
    (should (= (plist-get plan :points) 6))
    (should (= (plist-get plan :min) 99.0))
    (should (= (plist-get plan :max) 101.5))))

(ert-deftest financial-chart-depth-test-requires-both-book-sides ()
  (should-error (financial-chart-validate 'depth '(:bids ((100 2))))
                :type 'financial-chart-invalid-data))

(provide 'financial-chart-depth-test)
;;; financial-chart-depth-test.el ends here
