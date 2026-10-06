;;; financial-chart-eas-link-test.el --- panes template on eas: linked views (fc-qx1.6) -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of financial-chart's `panes' template (price, volume, RSI)
;; drawn by eas.el: one crosshair and one zoom across the panes, and
;; two tickers linked on a bus.  Moved here from eas's own link tests
;; when the engine was extracted.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'financial-chart-eas-link-demo)
(require 'eas-agent)

(defmacro financial-chart-eas-link-test--fresh (&rest body)
  "Run BODY with empty eas view and bus registries."
  (declare (indent 0))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-link-buses (make-hash-table :test 'equal)))
     ,@body))

(defun financial-chart-eas-link-test--view (view id)
  "Scene view ID of live VIEW."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get (eas-view-scene view) :views)))

(defun financial-chart-eas-link-test--px (view id &optional fx)
  "A pixel inside scene view ID of VIEW, FX (default 0.5) across its plot."
  (let ((b (plist-get (financial-chart-eas-link-test--view view id) :bounds)))
    (vector (+ (aref b 0) (* (or fx 0.5) (aref b 2))) (+ (aref b 1) (/ (aref b 3) 2.0)))))

(defun financial-chart-eas-link-test--domain (view id)
  "Scene view ID's x domain in VIEW."
  (plist-get (plist-get (plist-get (financial-chart-eas-link-test--view view id) :scales) :x) :domain))

(defun financial-chart-eas-link-test--store (view name)
  "VIEW's store for selection NAME."
  (plist-get (plist-get (eas-view-state (eas-view-get view)) :params) (eas-key name)))

(ert-deftest financial-chart-eas-link-panes-template ()
  "price + volume + RSI: hovering one pane draws the crosshair in all three; zoom moves all."
  (financial-chart-eas-link-test--fresh
    (let ((v (eas-view-open "panes" :bindings (eas-template-example "panes") :subject "TSM")))
      (should (equal (mapcar (lambda (sv) (plist-get sv :id)) (plist-get (eas-view-scene v) :views))
                     '("price" "volume" "rsi")))
      (eas-dispatch v (list :type "pointermove" :px (financial-chart-eas-link-test--px v "volume" 0.5)))
      (dolist (id '("price" "volume" "rsi"))
        (let ((rule (car (last (append (plist-get (financial-chart-eas-link-test--view v id) :marks) nil)))))
          (should (= 1 (length (plist-get rule :items))))))
      (eas-dispatch v '(:type "key" :key "+"))
      (should (equal (financial-chart-eas-link-test--domain v "price")
                     (financial-chart-eas-link-test--domain v "rsi")))
      (should (equal (financial-chart-eas-link-test--domain v "price")
                     (financial-chart-eas-link-test--domain v "volume")))
      (let ((rsi (plist-get (aref (plist-get (eas-inspect v) :views) 2) :visible)))
        (should (equal (plist-get rsi :field) "rsi"))
        (should (<= 0 (plist-get rsi :min) (plist-get rsi :max) 100))))))

(ert-deftest financial-chart-eas-link-panes-golden ()
  (financial-chart-test-golden "resolve-panes.json"
                               (eas-json-pretty (eas-resolve "panes" (eas-template-example "panes"))))
  (financial-chart-test-golden "text-panes.txt"
                               (eas-text-render (eas-compile (eas-resolve "panes" (eas-template-example "panes"))
                                                             :target 'text :size '(:cols 80 :rows 36)))))

(ert-deftest financial-chart-eas-link-demo-two-tickers ()
  "The demo's two tickers share hover and zoom over the bus \"tickers\"."
  (financial-chart-eas-link-test--fresh
    (pcase-let ((`(,tsm ,demo) (financial-chart-eas-link-demo-open)))
      (should (equal (eas-view-id tsm) "panes:TSM"))
      (should (equal (eas-view-id demo) "panes:DEMO"))
      (eas-dispatch tsm (list :type "pointermove" :px (financial-chart-eas-link-test--px tsm "price" 0.5)))
      (should (financial-chart-eas-link-test--store demo "crosshair"))
      (should (equal (plist-get (financial-chart-eas-link-test--store demo "crosshair") :values)
                     (plist-get (financial-chart-eas-link-test--store tsm "crosshair") :values)))
      (eas-dispatch demo '(:type "key" :key "+"))
      (should (equal (financial-chart-eas-link-test--domain tsm "rsi")
                     (financial-chart-eas-link-test--domain demo "price"))))))

(provide 'financial-chart-eas-link-test)
;;; financial-chart-eas-link-test.el ends here
