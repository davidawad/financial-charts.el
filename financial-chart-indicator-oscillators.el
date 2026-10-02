;;; financial-chart-indicator-oscillators.el --- Pure OHLC oscillators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Provider-neutral Stochastic, Williams %R, and Ultimate Oscillator
;; calculations over canonical :high, :low, and :close bar fields.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)

(defun financial-chart--oscillator-period (value default name)
  "Return positive integer VALUE or DEFAULT; identify invalid NAME."
  (setq value (or value default))
  (unless (and (integerp value) (> value 0))
    (signal 'wrong-type-argument (list (format "%s must be a positive integer" name)
                                       value)))
  value)

(defun financial-chart--oscillator-window-extreme (bars index window field mode)
  "Get MODE extreme of FIELD over BARS ending at INDEX for WINDOW bars.
Return nil if the window is incomplete or contains missing/non-numeric data."
  (when (>= index (1- window))
    (let ((start (- index window -1))
          (extreme nil)
          (valid t))
      (while (and valid (<= start index))
        (let ((value (plist-get (nth start bars) field)))
          (if (not (numberp value))
              (setq valid nil)
            (setq extreme
                  (if extreme
                      (if (eq mode :min) (min extreme value) (max extreme value))
                    value))))
        (setq start (1+ start)))
      (and valid extreme))))

(defun financial-chart--oscillator-sma-at (values index window)
  "Return mean of WINDOW values ending at INDEX, or nil if unavailable."
  (when (>= index (1- window))
    (let ((start (- index window -1))
          (sum 0.0)
          (valid t))
      (while (and valid (<= start index))
        (let ((value (nth start values)))
          (if (numberp value)
              (setq sum (+ sum value))
            (setq valid nil)))
        (setq start (1+ start)))
      (and valid (/ sum window)))))

(defun financial-chart-stochastic (bars &optional k-period d-period)
  "Return Stochastic %K and %D descriptors over BARS.
K-PERIOD defaults to 14 and D-PERIOD to 3.  Missing high, low, or
close values invalidate the affected window.  %D is the simple moving
average of %K.  Results stay aligned with BARS and use nil for warm-up."
  (setq k-period (financial-chart--oscillator-period k-period 14 "k-period")
        d-period (financial-chart--oscillator-period d-period 3 "d-period"))
  (let* ((k-values
          (cl-loop for index from 0 below (length bars)
                   for highest = (financial-chart--oscillator-window-extreme
                                  bars index k-period :high :max)
                   for lowest = (financial-chart--oscillator-window-extreme
                                 bars index k-period :low :min)
                   for close = (plist-get (nth index bars) :close)
                   collect
                   (when (and highest lowest (numberp close)
                              (/= highest lowest))
                     (* 100.0 (/ (float (- close lowest))
                                  (- highest lowest))))))
         (d-values (cl-loop for index from 0 below (length bars)
                            collect (financial-chart--oscillator-sma-at
                                     k-values index d-period))))
    (list (list :name 'stochastic-k :label "%K" :values k-values
                :unit :percent :panel :oscillator :scale :fixed
                :bounds '(0 . 100))
          (list :name 'stochastic-d :label "%D" :values d-values
                :unit :percent :panel :oscillator :scale :fixed
                :bounds '(0 . 100)))))

(defun financial-chart-williams-r (bars &optional period)
  "Return Williams %R descriptor over BARS (PERIOD defaults to 14).
Missing high, low, or close values invalidate the affected window;
results stay aligned with BARS and use nil for warm-up."
  (setq period (financial-chart--oscillator-period period 14 "period"))
  (cl-loop for index from 0 below (length bars)
           for highest = (financial-chart--oscillator-window-extreme
                          bars index period :high :max)
           for lowest = (financial-chart--oscillator-window-extreme
                         bars index period :low :min)
           for close = (plist-get (nth index bars) :close)
           collect (when (and highest lowest (numberp close)
                              (/= highest lowest))
                     (* -100.0 (/ (float (- highest close))
                                  (- highest lowest))))))

(defun financial-chart-ultimate-oscillator
    (bars &optional short-period medium-period long-period)
  "Return Ultimate Oscillator descriptor over BARS.
Periods default to 7, 14, and 28.  True range and buying pressure
require the prior bar's :close; missing source values invalidate each
affected window.  Results stay aligned with BARS and use nil for warm-up."
  (setq short-period (financial-chart--oscillator-period short-period 7 "short-period")
        medium-period (financial-chart--oscillator-period medium-period 14 "medium-period")
        long-period (financial-chart--oscillator-period long-period 28 "long-period"))
  (let* ((buying-pressure (make-list (length bars) nil))
         (true-range (make-list (length bars) nil)))
    (cl-loop for index from 1 below (length bars)
             for bar = (nth index bars)
             for previous-close = (plist-get (nth (1- index) bars) :close)
             for high = (plist-get bar :high)
             for low = (plist-get bar :low)
             for close = (plist-get bar :close)
             when (and (numberp previous-close) (numberp high)
                       (numberp low) (numberp close))
             do (setf (nth index buying-pressure)
                      (- close (min low previous-close))
                      (nth index true-range)
                      (- (max high previous-close) (min low previous-close))))
    (cl-loop for index from 0 below (length bars)
                   for bp-short = (financial-chart--oscillator-sma-at
                                   buying-pressure index short-period)
                   for tr-short = (financial-chart--oscillator-sma-at
                                   true-range index short-period)
                   for bp-medium = (financial-chart--oscillator-sma-at
                                    buying-pressure index medium-period)
                   for tr-medium = (financial-chart--oscillator-sma-at
                                    true-range index medium-period)
                   for bp-long = (financial-chart--oscillator-sma-at
                                  buying-pressure index long-period)
                   for tr-long = (financial-chart--oscillator-sma-at
                                  true-range index long-period)
                   collect
                   (when (and bp-short tr-short bp-medium tr-medium
                              bp-long tr-long (/= tr-short 0)
                              (/= tr-medium 0) (/= tr-long 0))
                     (* 100.0
                           (/ (+ (* 4.0 (/ (float bp-short) tr-short))
                              (* 2.0 (/ (float bp-medium) tr-medium))
                              (/ (float bp-long) tr-long))
                           7.0))))))

(financial-chart-register-indicator
 'stochastic #'financial-chart-stochastic
 :label "Stochastic" :unit :percent :panel :oscillator :scale :fixed
 :bounds '(0 . 100)
 :description "Stochastic %K and %D over normalized OHLC bars")
(financial-chart-register-indicator
 'williams-r #'financial-chart-williams-r
 :label "Williams %R" :unit :percent :panel :oscillator :scale :fixed
 :bounds '(-100 . 0)
 :description "Williams %R over normalized OHLC bars")
(financial-chart-register-indicator
 'ultimate-oscillator #'financial-chart-ultimate-oscillator
 :label "Ultimate Oscillator" :unit :percent :panel :oscillator :scale :fixed
 :bounds '(0 . 100)
 :description "Ultimate Oscillator over normalized OHLC bars")

(provide 'financial-chart-indicator-oscillators)
;;; financial-chart-indicator-oscillators.el ends here
