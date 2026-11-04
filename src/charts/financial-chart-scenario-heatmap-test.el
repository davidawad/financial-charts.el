;;; financial-chart-scenario-heatmap-test.el --- scenario heatmap tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart)

(ert-deftest financial-chart-scenario-heatmap-registers-and-explains-kind ()
  (should (eq (plist-get (financial-chart-describe-kind 'scenario-heatmap) :shape)
              'scenario-heatmap))
  (should (financial-chart-validate 'scenario-heatmap financial-chart-scenario-heatmap--example))
  (let ((summary (financial-chart-explain 'scenario-heatmap
                                          financial-chart-scenario-heatmap--example
                                          :backend 'text)))
    (should (equal (plist-get summary :template) "scenario-heatmap"))
    (should (= (plist-get summary :points) 9))
    (should (= (plist-get summary :min) -1450))
    (should (= (plist-get summary :max) 1980))))

(ert-deftest financial-chart-scenario-heatmap-rejects-invalid-cells ()
  (should (equal (financial-chart-check 'scenario-heatmap
                                        '((:spot-shock 0 :iv-shock 0 :pnl 1
                                           :delta 2 :gamma 3 :vega 4 :theta 5)))
                 t))
  (let ((result (financial-chart-check
                 'scenario-heatmap
                 '((:spot-shock 0 :iv-shock 0 :pnl 1 :delta 2 :gamma 3 :vega 4)))))
    (should (equal (plist-get result :code) "missing_field"))
    (should (equal (plist-get result :field) "theta")))
  (let ((result (financial-chart-check
                 'scenario-heatmap
                 '((:spot-shock 0 :iv-shock 0 :pnl 1 :delta 2 :gamma 3 :vega 4 :theta 5)
                   (:spot-shock 0 :iv-shock 0 :pnl 2 :delta 2 :gamma 3 :vega 4 :theta 5)))))
    (should (equal (plist-get result :code) "duplicate_cell"))
    (should (= (plist-get result :index) 1)))
  (should-error
   (financial-chart-validate 'scenario-heatmap
                             '((:spot-shock 0 :iv-shock 0 :pnl 1 :delta 2
                                :gamma 3 :vega 4 :theta 1.0e+INF)))
   :type 'financial-chart-invalid-data))

(provide 'financial-chart-scenario-heatmap-test)
;;; financial-chart-scenario-heatmap-test.el ends here
