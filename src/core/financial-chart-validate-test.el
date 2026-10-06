;;; financial-chart-validate-test.el --- strict validation of supplied data -*- lexical-binding: t; -*-

;; Every failure is data: (MESSAGE :code CODE :index INDEX :field FIELD),
;; naming the offending row and field.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'financial-chart)

(defun financial-chart-validate-test--bars (n)
  "N consistent bar/v1 plists, one day apart."
  (cl-loop for i from 0 below n
           collect (list :open (+ 100 i) :high (+ 104 i) :low (+ 98 i) :close (+ 101 i)
                         :volume (* 10 (1+ i)) :time (+ 1700000000000 (* i 86400000)))))

(defun financial-chart-validate-test--failure (kind data &rest props)
  "The `financial-chart-check' answer for KIND DATA PROPS (a plist or t)."
  (apply #'financial-chart-check kind data props))

(defmacro financial-chart-validate-test--fails (expected kind data &rest props)
  "Assert KIND DATA PROPS fails with EXPECTED (:code :index :field) and a message."
  `(let ((failure (financial-chart-validate-test--failure ,kind ,data ,@props)))
     (should (equal (list (plist-get failure :code) (plist-get failure :index)
                          (plist-get failure :field))
                    ,expected))
     (should (stringp (plist-get failure :message)))))

(ert-deftest financial-chart-validate-test-good-bars-pass ()
  (should (eq t (financial-chart-validate 'ohlc (financial-chart-validate-test--bars 5))))
  ;; :time is optional when no bar has it; equal open/close/high/low is a valid bar
  (should (eq t (financial-chart-validate 'ohlc '((:open 1 :high 1 :low 1 :close 1)
                                                  (:open 1 :high 2 :low 0.5 :close 2)))))
  (should (eq t (financial-chart-check 'ohlc nil))))

(ert-deftest financial-chart-validate-test-bar-shape ()
  (financial-chart-validate-test--fails '("not_a_list" nil nil) 'ohlc [1 2])
  (financial-chart-validate-test--fails '("not_a_plist" 1 nil) 'ohlc
                                        (list (car (financial-chart-validate-test--bars 1)) '(1 2 3)))
  (financial-chart-validate-test--fails '("missing_field" 0 "close") 'ohlc
                                        '((:open 1 :high 2 :low 0)))
  (financial-chart-validate-test--fails '("not_a_number" 0 "high") 'ohlc
                                        '((:open 1 :high "2" :low 0 :close 1)))
  (financial-chart-validate-test--fails '("not_a_number" 0 "low") 'ohlc
                                        '((:open 1 :high 2 :low 0.0e+NaN :close 1)))
  (financial-chart-validate-test--fails '("not_a_number" 0 "open") 'ohlc
                                        '((:open 1.0e+INF :high 2 :low 0 :close 1))))

(ert-deftest financial-chart-validate-test-price-ordering ()
  ;; high >= max(open, close) >= min(open, close) >= low
  (financial-chart-validate-test--fails '("high_below_body" 1 "high") 'ohlc
                                        '((:open 1 :high 2 :low 0 :close 1)
                                          (:open 1 :high 1.5 :low 0 :close 1.8)))
  (financial-chart-validate-test--fails '("high_below_body" 0 "high") 'ohlc
                                        '((:open 3 :high 2 :low 0 :close 1)))
  (financial-chart-validate-test--fails '("low_above_body" 0 "low") 'ohlc
                                        '((:open 1 :high 2 :low 1.2 :close 1.5)))
  (financial-chart-validate-test--fails '("low_above_body" 0 "low") 'ohlc
                                        '((:open 1.5 :high 2 :low 1.2 :close 1))))

(ert-deftest financial-chart-validate-test-volume ()
  (financial-chart-validate-test--fails '("negative_volume" 0 "volume") 'ohlc
                                        '((:open 1 :high 2 :low 0 :close 1 :volume -1)))
  (financial-chart-validate-test--fails '("negative_volume" 0 "volume") 'ohlc
                                        '((:open 1 :high 2 :low 0 :close 1 :volume "lots"))))

(ert-deftest financial-chart-validate-test-time-strictly-increases ()
  (let ((bars (financial-chart-validate-test--bars 4)))
    (setf (plist-get (nth 2 bars) :time) (plist-get (nth 1 bars) :time))
    (financial-chart-validate-test--fails '("time_not_increasing" 2 "time") 'ohlc bars))
  (financial-chart-validate-test--fails '("time_not_increasing" 1 "time") 'ohlc
                                        (reverse (financial-chart-validate-test--bars 2)))
  (financial-chart-validate-test--fails '("missing_field" 1 "time") 'ohlc
                                        '((:open 1 :high 2 :low 0 :close 1 :time 10)
                                          (:open 1 :high 2 :low 0 :close 1)))
  (financial-chart-validate-test--fails '("not_a_number" 0 "time") 'ohlc
                                        '((:open 1 :high 2 :low 0 :close 1 :time "2026-01-02"))))

(ert-deftest financial-chart-validate-test-every-bar-kind-and-entry-point-checks ()
  (let ((bad '((:open 1 :high 0.5 :low 0 :close 1))))
    (dolist (kind '(ohlc volume-profile))
      (should (equal (plist-get (financial-chart-check kind bad) :code) "high_below_body")))
    (should-error (financial-chart-render bad) :type 'financial-chart-invalid-data)
    (should-error (financial-chart-render-svg bad) :type 'financial-chart-invalid-data)
    (should-error (financial-chart-plot-spec (list :kind 'ohlc :data bad))
                  :type 'financial-chart-invalid-data)))

(ert-deftest financial-chart-validate-test-indicator-alignment ()
  (let ((bars (financial-chart-validate-test--bars 3)))
    (should (financial-chart-validate-indicator-series '(nil 1 2) bars))
    (should (financial-chart-validate-indicator-series
             (list :name 'sma :values [nil 1 2] :timestamps (mapcar (lambda (b) (plist-get b :time)) bars))
             bars))
    (let ((err (should-error (financial-chart-validate-indicator-series '(1 2) bars "SMA 3")
                             :type 'financial-chart-invalid-data)))
      (should (equal (financial-chart-error-data err)
                     '(:code "indicator_length" :index 2 :field "SMA 3"
                       :message "element 2 (SMA 3): SMA 3 has 2 values for 3 bars; give one per bar (nil for none)"))))
    (let ((err (should-error (financial-chart-validate-indicator-series
                              (list :name 'rsi :values '(1 2 3) :timestamps '(1 2 3))
                              bars)
                             :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :code) "indicator_misaligned"))
      (should (equal (plist-get (cddr err) :index) 0))
      (should (equal (plist-get (cddr err) :field) "rsi")))
    (let ((err (should-error (financial-chart-validate-indicator-series '(1 "x" 3) bars)
                             :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :code) "not_a_number"))
      (should (equal (plist-get (cddr err) :index) 1)))))

(ert-deftest financial-chart-validate-test-overlays-must-align-with-bars ()
  (let ((bars (financial-chart-validate-test--bars 5)))
    (let ((financial-chart-indicators (list (list :fn (lambda (b) (cdr (financial-chart-sma b 2)))
                                                  :label "short"))))
      (let ((err (should-error (financial-chart-render bars) :type 'financial-chart-invalid-data)))
        (should (equal (plist-get (cddr err) :code) "indicator_length"))
        (should (equal (plist-get (cddr err) :field) "short"))))
    (let ((financial-chart-oscillators (list (list :fn (lambda (b) (financial-chart-rsi b 2))))))
      (should (stringp (financial-chart-render bars))))))

(ert-deftest financial-chart-validate-test-normalized-indicator-series-errors-are-data ()
  (let ((err (should-error (financial-chart-normalize-indicator-series '(:name x :values (1 "b")))
                           :type 'financial-chart-invalid-indicator)))
    (should (equal (financial-chart-error-data err)
                   '(:code "not_a_number" :index 1 :field "values"
                     :message "series x value 1 must be a number or nil, got \"b\""))))
  (let ((err (should-error (financial-chart-normalize-indicator-series
                            '(:name x :values (1 2) :timestamps (1)))
                           :type 'financial-chart-invalid-indicator)))
    (should (equal (plist-get (cddr err) :code) "indicator_length"))
    (should (equal (plist-get (cddr err) :field) "timestamps"))))

(ert-deftest financial-chart-validate-test-series-payoff-labeled ()
  (financial-chart-validate-test--fails '("not_a_list" nil nil) 'area "1 2 3")
  (financial-chart-validate-test--fails '("invalid_point" 1 nil) 'line '(1 "2" 3))
  (financial-chart-validate-test--fails '("not_a_number" 1 "y") 'area '((1 2) (2 "x")))
  (should (eq t (financial-chart-validate 'area '((1 2) (2 nil) (3 4)))))
  (financial-chart-validate-test--fails '("price_not_ascending" 2 "price") 'payoff
                                        '((90 1) (110 2) (100 3)))
  (financial-chart-validate-test--fails '("not_a_number" 0 "pnl") 'payoff '((90 nil)))
  (financial-chart-validate-test--fails '("not_a_number" 0 "price") 'payoff '((a 1)))
  (financial-chart-validate-test--fails '("not_a_number" 1 "value") 'bars '(("A" . 1) ("B" . "x")))
  (financial-chart-validate-test--fails '("invalid_label" 0 "label") 'bars '(((1 2) . 1))))

(ert-deftest financial-chart-validate-test-depth-and-matrix-shapes ()
  (financial-chart-validate-test--fails '("not_an_order_book" nil nil) 'depth '(:bids nil))
  (financial-chart-validate-test--fails '("not_positive" 0 "bids.price") 'depth
                                        '(:bids ((-1 2)) :asks ((101 1))))
  (financial-chart-validate-test--fails '("invalid_level" 0 "asks") 'depth
                                        '(:bids ((100 1)) :asks ((101))))
  (financial-chart-validate-test--fails '("crossed_book" 0 "bids.price") 'depth
                                        '(:bids ((102 1)) :asks ((101 1))))
  (financial-chart-validate-test--fails '("not_a_matrix" nil nil) 'heatmap '(1 2))
  (financial-chart-validate-test--fails '("row_count" nil "rows") 'heatmap
                                        '(:labels ("A" "B") :rows ((1 2))))
  (financial-chart-validate-test--fails '("column_count" 1 "rows") 'heatmap
                                        '(:labels ("A" "B") :rows ((1 2) (3))))
  (financial-chart-validate-test--fails '("not_a_number" 1 "rows[0]") 'heatmap
                                        '(:labels ("A" "B") :rows ((1 2) ("x" 4)))))

(ert-deftest financial-chart-validate-test-nested-shapes-locate-the-inner-row ()
  (financial-chart-validate-test--fails '("not_a_number" 1 "series[2].y") 'multi
                                        '(("A" . (1 2 3)) ("B" . ((1 1) (2 2) (3 "x")))))
  (financial-chart-validate-test--fails '("zero_base" 0 "normalize") 'multi
                                        '(("A" . (0 2))) :normalize 100)
  (financial-chart-validate-test--fails '("price_not_ascending" 0 "payoff[1].price") 'payoff-curves
                                        '(("T+0" . ((100 1) (90 2))))))

(ert-deftest financial-chart-validate-test-eas-door-reports-the-row ()
  ;; Data handed to the eas adapters fails as eas data with the same row.
  (let ((err (should-error (eas-data-from "payoff" '((90 1) (80 2))) :type 'eas-error)))
    (should (equal (plist-get (cddr err) :code) "SHAPE_INVALID"))
    (should (equal (plist-get (cddr err) :index) 1))
    (should (equal (plist-get (cddr err) :field) "price"))))

(provide 'financial-chart-validate-test)
;;; financial-chart-validate-test.el ends here
