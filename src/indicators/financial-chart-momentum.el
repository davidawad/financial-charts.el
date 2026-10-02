;;; financial-chart-momentum.el --- Momentum indicators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Provider-neutral momentum calculators over normalized bar plists.
;; The calculations are local and synchronous; data fetching and provider
;; adapters belong to callers.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-core)
(require 'financial-chart-indicator-api)

(defun financial-chart--momentum-period (period name)
  "Return PERIOD or signal an indicator error naming NAME."
  (unless (and (integerp period) (> period 0))
    (signal 'financial-chart-invalid-indicator
            (list (format "%s period must be a positive integer" name)
                  :period period)))
  period)

(defun financial-chart--momentum-field-values (bars field)
  "Extract numeric FIELD values from BARS, preserving missing entries as nil."
  (mapcar (lambda (bar)
            (let ((value (plist-get bar field)))
              (and (numberp value) value)))
          bars))

(defun financial-chart-momentum (bars &optional period field)
  "Price momentum over PERIOD bars (default 10), using FIELD (default :close).
Each value is the current field minus its value PERIOD bars earlier.  Values
are aligned with BARS; warm-up and missing input produce nil."
  (setq period (financial-chart--momentum-period (or period 10) "Momentum")
        field (or field :close))
  (let ((values (financial-chart--momentum-field-values bars field)))
    (cl-loop for i from 0 below (length values)
             for current = (nth i values)
             for prior-index = (- i period)
             for prior = (and (>= prior-index 0) (nth prior-index values))
             collect (and current prior (- current prior)))))

(defun financial-chart-rate-of-change (bars &optional period field)
  "Percentage rate of change over PERIOD bars (default 12).
FIELD defaults to :close.  A zero reference value, warm-up, or missing input
produces nil.  Output is percentage points, so 0.1 means 0.1 percent."
  (setq period (financial-chart--momentum-period (or period 12) "ROC")
        field (or field :close))
  (let ((values (financial-chart--momentum-field-values bars field)))
    (cl-loop for i from 0 below (length values)
             for current = (nth i values)
             for prior-index = (- i period)
             for prior = (and (>= prior-index 0) (nth prior-index values))
             collect (and current prior (not (zerop prior))
                          (* 100.0 (/ (- current prior) prior))))))

(defun financial-chart-commodity-channel-index (bars &optional period)
  "Commodity Channel Index over PERIOD bars (default 20).
Uses typical price (high + low + close) / 3 and the conventional 0.015
constant.  Warm-up or missing OHLC data returns nil; a flat window returns 0."
  (setq period (financial-chart--momentum-period
                (or period 20) "Commodity Channel Index"))
  (let* ((typicals
          (mapcar (lambda (bar)
                    (let ((high (plist-get bar :high))
                          (low (plist-get bar :low))
                          (close (plist-get bar :close)))
                      (when (and (numberp high) (numberp low) (numberp close))
                        (/ (+ high low close) 3.0))))
                  bars))
         (n (length typicals)))
    (cl-loop for i from 0 below n
             collect
             (if (< i (1- period))
                 nil
               (let ((window (cl-subseq typicals (1+ (- i period)) (1+ i))))
                 (when (cl-every #'numberp window)
                   (let* ((mean (/ (apply #'+ window) (float period)))
                          (deviation
                           (/ (cl-loop for value in window
                                       sum (abs (- value mean)))
                              (float period))))
                     (if (zerop deviation)
                         0.0
                       (/ (- (nth i typicals) mean)
                          (* 0.015 deviation))))))))))

(defun financial-chart--momentum-ema (values period)
  "Return EMA of VALUES seeded by PERIOD contiguous values.
Nil input resets the seed; outputs remain aligned with VALUES."
  (let ((result (make-list (length values) nil))
        (alpha (/ 2.0 (1+ period)))
        (seed nil)
        (previous nil))
    (cl-loop for value in values
             for i from 0
             do
             (if (not (numberp value))
                 (setq seed nil previous nil)
               (if previous
                   (setq previous (+ (* alpha value)
                                     (* (- 1.0 alpha) previous)))
                 (push value seed)
                 (when (= (length seed) period)
                   (setq previous (/ (apply #'+ (nreverse seed))
                                     (float period))
                         seed nil)))
               (when previous
                 (setf (nth i result) previous))))
    result))

(defun financial-chart-macd (bars &optional fast-period slow-period signal-period field)
  "Return MACD line, signal line, and histogram for BARS.
FAST-PERIOD defaults to 12, SLOW-PERIOD to 26, SIGNAL-PERIOD to 9, and
FIELD to :close.  Output is a list of named descriptors accepted by
`financial-chart-indicator-evaluate'.  Each values list aligns with BARS;
warm-up and missing inputs are nil."
  (setq fast-period (financial-chart--momentum-period (or fast-period 12) "MACD fast")
        slow-period (financial-chart--momentum-period (or slow-period 26) "MACD slow")
        signal-period (financial-chart--momentum-period
                       (or signal-period 9) "MACD signal")
        field (or field :close))
  (when (>= fast-period slow-period)
    (signal 'financial-chart-invalid-indicator
            (list "MACD fast period must be less than slow period"
                  :fast-period fast-period :slow-period slow-period)))
  (let* ((values (financial-chart--momentum-field-values bars field))
         (fast (financial-chart--momentum-ema values fast-period))
         (slow (financial-chart--momentum-ema values slow-period))
         (macd (cl-mapcar (lambda (a b) (and a b (- a b))) fast slow))
         (signal-values (financial-chart--momentum-ema macd signal-period))
         (histogram (cl-mapcar (lambda (line signal-line)
                                 (and line signal-line (- line signal-line)))
                               macd signal-values)))
    (list (list :name 'macd :label "MACD" :values macd)
          (list :name 'macd-signal :label "MACD Signal" :values signal-values)
          (list :name 'macd-histogram :label "MACD Histogram" :values histogram))))

(financial-chart-register-indicator
 'momentum #'financial-chart-momentum
 :label "Momentum" :unit :price :panel :oscillator :scale :linear
 :description "Price difference from N bars earlier.")
(financial-chart-register-indicator
 'roc #'financial-chart-rate-of-change
 :label "ROC" :unit :percent :panel :oscillator :scale :linear
 :description "Percentage price change from N bars earlier.")
(financial-chart-register-indicator
 'cci #'financial-chart-commodity-channel-index
 :label "CCI" :unit :index :panel :oscillator :scale :linear
 :description "Commodity Channel Index from typical price.")
(financial-chart-register-indicator
 'macd #'financial-chart-macd
 :label "MACD" :unit :price :panel :oscillator :scale :linear
 :description "Moving Average Convergence Divergence, signal, and histogram." )

(provide 'financial-chart-momentum)
;;; financial-chart-momentum.el ends here
