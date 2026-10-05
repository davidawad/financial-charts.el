;;; financial-chart-easel-test.el --- financial-chart shapes and indicators on easel -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart)

(ert-deftest financial-chart-easel-lowers-every-shape ()
  (dolist (case '(("series" ((1 40.0) 41.0) [(:x 1 :y 40.0) (:x 1 :y 41.0)])
                  ("payoff" ((90 50) (100 -100)) [(:price 90 :pnl 50) (:price 100 :pnl -100)])
                  ("labeled" (("AAPL" . 1200)) [(:label "AAPL" :value 1200)])
                  ("matrix" (:labels ("A" "B") :rows ((1.0 0.5) (0.5 1.0)))
                   [(:row "A" :column "A" :value 1.0) (:row "A" :column "B" :value 0.5)
                    (:row "B" :column "A" :value 0.5) (:row "B" :column "B" :value 1.0)])
                  ("order-book" (:bids ((100 2.0)) :asks ((101 1.5)))
                   [(:side "bid" :price 100 :size 2.0) (:side "ask" :price 101 :size 1.5)])
                  ("payoff-curves" (("T+0" . ((90 30) (100 -20))))
                   [(:curve "T+0" :price 90 :pnl 30) (:curve "T+0" :price 100 :pnl -20)])
                  ("multi-series" (("SPY" . ((1 100) (2 99))))
                   [(:series "SPY" :x 1 :y 100) (:series "SPY" :x 2 :y 99)])))
    (should (equal (cons (car case) (easel-data-rows (easel-data-from (car case) (nth 1 case))))
                   (cons (car case) (nth 2 case))))))

(ert-deftest financial-chart-easel-shape-failures-carry-index ()
  (let ((failure (easel-data-check "payoff" '((1 2) ("x" 3)))))
    (should (equal (plist-get failure :code) "SHAPE_INVALID"))
    (should (equal (plist-get failure :index) 1))
    (should (equal (plist-get failure :shape) "payoff"))))

(ert-deftest financial-chart-easel-adapter-examples-validate ()
  (dolist (name '("series" "payoff" "labeled" "matrix" "order-book" "payoff-curves" "multi-series"))
    (should-not (easel-data-check name (plist-get (easel-adapter name) :example)))))

(ert-deftest financial-chart-easel-indicator-transform-wraps-the-api ()
  (let* ((bars (plist-get (alist-get 'ohlc financial-chart-shapes) :example))
         (rows (easel-data-rows (easel-data-from "bar/v1" bars)))
         (out (easel-transform-run [(:x-easel:transform "indicator" :name "sma" :params [3] :as "sma3")]
                                   rows))
         (expected (financial-chart-indicator-evaluate 'sma bars 3)))
    (should (equal (seq-map (lambda (r) (let ((v (plist-get r :sma3))) (if (eq v :null) nil v))) out)
                   (append (plist-get expected :values) nil))))
  (let* ((bars (plist-get (alist-get 'ohlc financial-chart-shapes) :example))
         (row (aref (easel-transform-run [(:x-easel:transform "indicator" :name "macd")]
                                         (easel-data-rows (easel-data-from "bar/v1" bars)))
                    40)))
    (should (cl-some (lambda (k) (string-prefix-p ":macd_" (symbol-name k)))
                     (cl-loop for (k _) on row by #'cddr collect k))))
  (should (equal (plist-get (easel-data-check "plist" []) :code) nil))
  (let ((err (should-error (easel-transform-run [(:x-easel:transform "indicator" :name "nope")]
                                                [(:open 1 :high 1 :low 1 :close 1)])
                           :type 'easel-error)))
    (should (equal (plist-get (easel-error-plist err) :code) "INVALID_INPUT"))))

(ert-deftest financial-chart-easel-describe-lists-the-indicator-schema ()
  (let ((transforms (plist-get (easel-describe 'transforms) :transforms)))
    (should (cl-find "indicator" transforms :key (lambda (x) (plist-get x :name)) :test #'equal))
    (should (plist-get (plist-get (plist-get (cl-find "indicator" transforms
                                                      :key (lambda (x) (plist-get x :name))
                                                      :test #'equal)
                                             :schema)
                                  :name)
                       :required))))

(provide 'financial-chart-easel-test)
;;; financial-chart-easel-test.el ends here
