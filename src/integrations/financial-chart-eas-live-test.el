;;; financial-chart-eas-live-test.el --- markup events in the readout, live verbs, doctor rows -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty.10.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-agent)

(ert-deftest financial-chart-live-spec-carries-markups ()
  (let* ((spec (financial-chart-compose (financial-chart-catalog-example "preset-swing-trader")))
         (markups (append (plist-get (plist-get spec :x-eas) :markups) nil))
         (hammer (seq-find (lambda (e) (equal (plist-get e :id) "pattern.hammer@2026-04-27")) markups)))
    (should hammer)
    (should (equal (list (plist-get hammer :x) (plist-get hammer :label) (plist-get hammer :marker)) '(60 "H" t)))
    (should (seq-find (lambda (c) (equal (plist-get c :component) "markup-events"))
                      (plist-get (plist-get (plist-get spec :x-eas) :readout) :children)))
    ;; Hand-placed annotations only: no markups, no component.
    (let ((plain (financial-chart-compose (financial-chart-catalog-example "annotations"))))
      (should-not (plist-get (plist-get plain :x-eas) :markups))
      (should-not (seq-find (lambda (c) (equal (plist-get c :component) "markup-events"))
                            (plist-get (plist-get (plist-get plain :x-eas) :readout) :children))))))

(ert-deftest financial-chart-live-readout-shows-the-bar-s-events ()
  (let* ((events [(:id "pattern.hammer@x" :x 60 :marker t :label "H" :direction "bullish" :strength 0.3)
                  (:id "fib.retracement@x" :x 60 :marker :false :label "retracement" :direction "bullish" :strength 0.1)
                  (:id "pattern.doji@y" :x 61 :marker t :label "D" :direction "neutral" :strength 0.5)])
         (node (list :component "markup-events" :props (list :events events :up "#00ff00" :down "#ff0000")))
         (spans (car (eas-component-fit (eas-component-render node (list :datum '(:time 60 :close 1.0) :path "")) 80 1)))
         (text (mapconcat #'car spans "")))
    (should (equal text "H 0.30"))
    (should (seq-find (lambda (s) (equal (plist-get (cdr s) :color) "#00ff00")) spans))))

(ert-deftest financial-chart-live-verbs-list-and-focus ()
  (let ((eas-views (make-hash-table :test 'equal)) (inhibit-message t))
    (eas-view-open (financial-chart-compose (financial-chart-catalog-example "preset-swing-trader"))
                   :id "fc:live" :target 'text :size '(:cols 100 :rows 40))
    (let ((m (eas-agent "markups" "fc:live")))
      (should (eq (plist-get m :ok) t))
      (should (> (plist-get (plist-get m :data) :n) 10)))
    (let* ((f (eas-agent "focus" "fc:live" "pattern.hammer@2026-04-27"))
           (price (seq-find (lambda (v) (equal (plist-get v :id) "price")) (plist-get (plist-get f :data) :views))))
      (should (eq (plist-get f :ok) t))
      (should (equal (plist-get (plist-get price :domains) :x) [50.0 70.0])))
    (should (equal (plist-get (eas-agent "focus" "fc:live" "nope@1") :reason) "NOT_FOUND"))))

(ert-deftest financial-chart-live-doctor-rows ()
  (let ((rows (financial-chart-markups-doctor-checks)))
    (should (equal (mapcar (lambda (r) (plist-get r :name)) rows)
                   '("markups:registry" "markups:oracle" "markups:presets" "markups:evidence")))
    (should (eq (plist-get (nth 0 rows) :status) 'pass))
    (should (eq (plist-get (nth 1 rows) :status) 'pass))
    (should (eq (plist-get (nth 2 rows) :status) 'pass))
    (should (memq (plist-get (nth 3 rows) :status) '(pass skip)))
    (should (seq-find (lambda (r) (equal (plist-get r :name) "markups:registry")) (financial-chart-doctor-checks)))))

(provide 'financial-chart-eas-live-test)
;;; financial-chart-eas-live-test.el ends here
