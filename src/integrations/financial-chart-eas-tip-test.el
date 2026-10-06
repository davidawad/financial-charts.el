;;; financial-chart-eas-tip-test.el --- ohlc template on eas: tooltips and click actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of financial-chart's `ohlc' template (the candlesticks demo)
;; drawn by eas.el: its resolved golden, the hover tooltip and the
;; `echo' action bound to the candles.  Moved here from eas's own tip
;; tests when the engine was extracted.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-agent)

(defmacro financial-chart-eas-tip-test--with-view (var template &rest body)
  "Open TEMPLATE with its example bindings as VAR in a fresh registry; run BODY."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-action-inhibit nil))
     (let ((,var (eas-view-open ,template :bindings (eas-template-example ,template) :id "t")))
       ,@body)))

(defun financial-chart-eas-tip-test--centre (view mark-id i)
  "Pixel centre of item I of MARK-ID in VIEW's scene."
  (let ((item (aref (plist-get (eas-scene-mark (eas-view-scene view) mark-id) :items) i)))
    (if (plist-member item :w)
        (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))
      (vector (plist-get item :x) (plist-get item :y)))))

(ert-deftest financial-chart-eas-tip-ohlc-template-golden ()
  (financial-chart-test-golden "resolve-ohlc.json"
                               (eas-json-pretty (eas-resolve "ohlc" (eas-template-example "ohlc"))))
  (should (equal (plist-get (plist-get (plist-get (eas-template-get "ohlc") :meta) :actions) :candles) "echo")))

(ert-deftest financial-chart-eas-tip-ohlc-hot-spots-carry-help-echo ()
  (financial-chart-eas-tip-test--with-view v "ohlc"
    (eas-view-resize v '(:cols 60 :rows 16) 'text)
    (let ((text (eas-text-render (eas-view-scene v))))
      (should (seq-some (lambda (i) (let ((h (get-text-property i 'help-echo text)))
                                      (and h (string-match-p "close: 416" h))))
                        (number-sequence 0 (1- (length text))))))))

(ert-deftest financial-chart-eas-tip-ohlc-template-actions-run-on-mouse-1 ()
  ;; mouse-1 is pointerdown + pointerup; the template binds candles to echo.
  (financial-chart-eas-tip-test--with-view v "ohlc"
    (let* ((px (financial-chart-eas-tip-test--centre v "candles" 0)) (inhibit-message t))
      (eas-dispatch v (list :type "pointerdown" :px px))
      (let ((click (plist-get (eas-dispatch v (list :type "pointerup" :px px)) :click)))
        (should (equal (plist-get click :mark) "candles"))
        (should (equal (plist-get click :action) "echo"))
        (should (eq (plist-get click :ran) t))
        (should (string-match-p "date: Aug 20, 2026\nopen: 408.6\nhigh: 417.96\nlow: 407.72\nclose: 416"
                                (plist-get click :result))))
      ;; A press that moves past the click slop is a drag, not a click.
      (eas-dispatch v '(:type "click" :px [0 0]))
      (eas-dispatch v (list :type "pointerdown" :px px))
      (eas-dispatch v (list :type "pointerup" :px (vector (+ 10 (aref px 0)) (aref px 1))))
      (should (eq (plist-get (eas-inspect v) :click) :null)))))

(provide 'financial-chart-eas-tip-test)
;;; financial-chart-eas-tip-test.el ends here
