;;; financial-chart-eas-shift-test.el --- shifted series and future bars -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.3: "shift" moves a series later (or earlier); a forward shift
;; grows the chart past its last bar.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defun financial-chart-shift-test--bars ()
  "The ohlc shape's example bars (48, epoch-ms times)."
  (plist-get (alist-get 'ohlc financial-chart-shapes) :example))

(defun financial-chart-shift-test--layer (spec pane name)
  "The layer NAME of SPEC's PANE."
  (cl-find name (append (plist-get (aref (plist-get spec :vconcat) pane) :layer) nil)
           :key (lambda (l) (plist-get l :name)) :test #'equal))

(ert-deftest financial-chart-shift-values-and-future-positions ()
  (should (equal (financial-chart-shift-values [1 2 3] 2 5) [nil nil 1 2 3]))
  (should (equal (financial-chart-shift-values [1 2 3] -1 3) [2 3 nil]))
  (should (equal (financial-chart-shift-future-xs [0 1 2] "quantitative" 3) '(3 4 5)))
  ;; Daily weekday bars continue on weekdays: Thu, Fri, then Mon, Tue.
  (let* ((day 86400000)
         (xs (vconcat (mapcar #'eas-time-parse '("2026-09-28" "2026-09-29" "2026-09-30" "2026-10-01")))))
    (should (equal (mapcar (lambda (ms) (format-time-string "%a %F" (/ ms 1000) t))
                           (financial-chart-shift-future-xs xs "temporal" 3))
                   '("Fri 2026-10-02" "Mon 2026-10-05" "Tue 2026-10-06")))
    ;; Bars that include weekends keep every day.
    (should (equal (financial-chart-shift-future-xs (vconcat (list 0 day (* 2 day))) "temporal" 2)
                   (list (* 3 day) (* 4 day)))))
  (should-not (financial-chart-shift-future-xs [0 1] "quantitative" 0)))

(ert-deftest financial-chart-shift-moves-a-series-and-grows-the-chart ()
  (let* ((bars (financial-chart-shift-test--bars))
         (spec (financial-chart-compose
                (list :bars bars :price '(:series [(:indicator "sma" :params [5])
                                                    (:indicator "sma" :params [5] :shift 3 :id "dma")]))))
         (rows (append (plist-get (plist-get spec :data) :values) nil))
         (sma (plist-get (financial-chart-indicator-evaluate 'sma bars 5) :values))
         (dma (financial-chart-shift-test--layer spec 0 "series-dma"))
         (own (append (plist-get (plist-get dma :data) :values) nil)))
    ;; A shifted series is its own column: value at bar i is the SMA of bar i-3.
    (should (equal (mapcar (lambda (r) (let ((v (plist-get r :s1))) (if (eq v :null) nil v))) rows)
                   (append (make-list 3 nil) (seq-take sma 45))))
    ;; Its layer carries rows to three bars past the last one.
    (should (= (length rows) 48))
    (should (= (length own) (- 51 7)))
    (should (equal (plist-get (car (last own)) :s1) (car (last sma))))
    (should (> (plist-get (car (last own)) :time) (plist-get (car (last bars)) :time)))
    ;; An unshifted series reads the shared rows.
    (should-not (plist-get (financial-chart-shift-test--layer spec 0 "series-sma-5") :data))
    (should (stringp (financial-chart-compose-render
                      (list :bars bars :price '(:series [(:indicator "sma" :params [5] :shift 3)]))
                      :width 60 :height 16)))))

(ert-deftest financial-chart-shift-ichimoku-spans-run-past-the-last-bar ()
  (let* ((chart (financial-chart-compose-example "candles"))
         (bars (append (plist-get chart :bars) nil))
         (spec (financial-chart-compose (list :bars (vconcat bars) :price '(:series ["ichimoku"]))))
         (a (financial-chart-shift-test--layer spec 0 "series-ichimoku.ichimoku-senkou-a"))
         (chikou (financial-chart-shift-test--layer spec 0 "series-ichimoku.ichimoku-chikou"))
         (rows (plist-get (plist-get spec :data) :values))
         (last-time (plist-get (aref rows (1- (length rows))) :time))
         (future (cl-remove-if-not (lambda (r) (> (plist-get r :time) last-time))
                                   (append (plist-get (plist-get a :data) :values) nil))))
    (should (= (length future) 26))
    ;; Chikou is drawn 26 bars back: the last 26 rows have none.
    (should-not (plist-get chikou :data))
    (let ((col (intern (concat ":" (plist-get (plist-get (plist-get chikou :encoding) :y) :field)))))
      (should (cl-every (lambda (r) (eq (plist-get r col) :null)) (seq-drop (append rows nil) (- 80 26))))
      (should (equal (plist-get (aref rows 0) col) (plist-get (nth 26 bars) :close))))))

(ert-deftest financial-chart-shift-must-be-whole-bars ()
  (dolist (shift '(1.5 "2" 501))
    (let ((err (should-error (financial-chart-compose
                              (list :bars (financial-chart-shift-test--bars)
                                    :price (list :series (vector (list :indicator "sma" :shift shift)))))
                             :type 'financial-chart-invalid-chart)))
      (should (equal (plist-get (cddr err) :code) "INVALID_SHIFT"))
      (should (equal (plist-get (cddr err) :path) "/price/series/0/shift")))))

(provide 'financial-chart-eas-shift-test)
;;; financial-chart-eas-shift-test.el ends here
