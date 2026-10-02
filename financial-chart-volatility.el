;;; financial-chart-volatility.el --- Volatility and channel indicators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provider-neutral volatility and price-channel indicators over normalized
;; oldest-first bar data.  Calculators are pure Elisp and do no I/O.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)

(defun financial-chart--volatility-check-window (window)
  "Signal unless WINDOW is a positive integer."
  (unless (and (integerp window) (> window 0))
    (signal 'args-out-of-range (list window 1 nil))))

(defun financial-chart--volatility-contiguous-window (values end window)
  "Return WINDOW values ending at END, or nil if any value is missing."
  (when (>= end (1- window))
    (let ((slice (cl-subseq values (1+ (- end window)) (1+ end))))
      (and (cl-every #'numberp slice) slice))))

(defun financial-chart--volatility-mean (values)
  "Return arithmetic mean of numeric VALUES."
  (/ (apply #'+ values) (float (length values))))

(defun financial-chart--volatility-extreme (bars field window predicate)
  "Return rolling extreme of FIELD across BARS over WINDOW.
PREDICATE is `max' or `min'.  Missing values and warmup are nil."
  (let ((values (mapcar (lambda (bar) (plist-get bar field)) bars)))
    (cl-loop for i from 0 below (length bars)
             for slice = (financial-chart--volatility-contiguous-window
                          values i window)
             collect (when slice (apply predicate slice)))))

(defun financial-chart--volatility-true-ranges (bars)
  "Return bar-aligned true ranges for BARS.
The first bar uses high minus low; later bars require the preceding close."
  (let ((previous-close nil)
        (first t))
    (mapcar
     (lambda (bar)
       (let* ((high (plist-get bar :high))
              (low (plist-get bar :low))
              (close (plist-get bar :close))
              (true-range
               (when (and (numberp high) (numberp low)
                          (or first (numberp previous-close)))
                 (if first
                     (- high low)
                   (max (- high low)
                        (abs (- high previous-close))
                        (abs (- low previous-close)))))))
         (setq previous-close close)
         (setq first nil)
         true-range))
    bars)))

(defun financial-chart--volatility-wilder-average (values period)
  "Return Wilder-smoothed averages of VALUES using PERIOD.
Seed each contiguous run with a simple average; missing data resets it."
  (let ((result nil)
        (run nil)
        (previous nil))
    (dolist (value values)
      (cond
       ((not (numberp value))
        (push nil result)
        (setq run nil previous nil))
       (previous
        (setq previous (+ (/ (+ (* previous (1- period)) value)
                             (float period))))
        (push previous result))
       (t
        (push value run)
        (if (= (length run) period)
            (progn
              (setq previous (financial-chart--volatility-mean (nreverse run))
                    run nil)
              (push previous result))
          (push nil result)))))
    (nreverse result)))

(defun financial-chart-atr (bars &optional period)
  "Average true range of BARS using Wilder smoothing.
PERIOD defaults to 14.  Values align with BARS and are nil until PERIOD
consecutive true ranges are available; missing inputs reset the warmup."
  (setq period (or period 14))
  (financial-chart--volatility-check-window period)
  (financial-chart--volatility-wilder-average
   (financial-chart--volatility-true-ranges bars) period))

(defun financial-chart-bollinger-bands (bars &optional period deviations field)
  "Return Bollinger upper, middle, and lower bands for BARS.
PERIOD defaults to 20, DEVIATIONS to 2, and FIELD to :close.  Standard
population standard deviation is used.  Each output is bar-aligned; warmup
or missing values are nil."
  (setq period (or period 20)
        deviations (or deviations 2.0)
        field (or field :close))
  (financial-chart--volatility-check-window period)
  (unless (and (numberp deviations) (>= deviations 0))
    (signal 'args-out-of-range (list deviations 0 nil)))
  (let* ((values (mapcar (lambda (bar) (plist-get bar field)) bars))
         (bands (cl-loop for i from 0 below (length bars)
                         for slice = (financial-chart--volatility-contiguous-window
                                      values i period)
                         collect
                         (when slice
                           (let* ((mean (financial-chart--volatility-mean slice))
                                  (variance
                                   (/ (cl-loop for value in slice
                                               sum (expt (- value mean) 2))
                                      (float period)))
                                  (width (* deviations (sqrt variance))))
                             (list (+ mean width) mean (- mean width))))))
         (upper (mapcar (lambda (band) (nth 0 band)) bands))
         (middle (mapcar (lambda (band) (nth 1 band)) bands))
         (lower (mapcar (lambda (band) (nth 2 band)) bands)))
    (list (list :name 'bollinger-upper :label "Bollinger Upper" :values upper)
          (list :name 'bollinger-middle :label "Bollinger Middle" :values middle)
          (list :name 'bollinger-lower :label "Bollinger Lower" :values lower))))

(defun financial-chart--volatility-ema (values period)
  "Return EMA of VALUES over PERIOD, resetting after missing values."
  (let ((result nil)
        (run nil)
        (previous nil)
        (alpha (/ 2.0 (1+ period))))
    (dolist (value values)
      (cond
       ((not (numberp value))
        (push nil result)
        (setq run nil previous nil))
       (previous
        (setq previous (+ (* alpha value) (* (- 1 alpha) previous)))
        (push previous result))
       (t
        (push value run)
        (if (= (length run) period)
            (progn
              (setq previous (financial-chart--volatility-mean (nreverse run))
                    run nil)
              (push previous result))
          (push nil result)))))
    (nreverse result)))

(defun financial-chart-keltner-channels (bars &optional period atr-period multiplier)
  "Return Keltner upper, middle, and lower channels for BARS.
PERIOD defaults to 20 for the EMA centerline, ATR-PERIOD to 10, and
MULTIPLIER to 2.  Channels are nil unless both centerline and ATR exist."
  (setq period (or period 20)
        atr-period (or atr-period 10)
        multiplier (or multiplier 2.0))
  (financial-chart--volatility-check-window period)
  (financial-chart--volatility-check-window atr-period)
  (unless (and (numberp multiplier) (>= multiplier 0))
    (signal 'args-out-of-range (list multiplier 0 nil)))
  (let* ((closes (mapcar (lambda (bar) (plist-get bar :close)) bars))
         (ema (financial-chart--volatility-ema closes period))
         (atr (financial-chart-atr bars atr-period))
         (middle ema)
         (upper (cl-mapcar (lambda (center range)
                             (when (and center range)
                               (+ center (* multiplier range)))) ema atr))
         (lower (cl-mapcar (lambda (center range)
                             (when (and center range)
                               (- center (* multiplier range)))) ema atr)))
    (list (list :name 'keltner-upper :label "Keltner Upper" :values upper)
          (list :name 'keltner-middle :label "Keltner Middle" :values middle)
          (list :name 'keltner-lower :label "Keltner Lower" :values lower))))

(defun financial-chart-donchian-channels (bars &optional period)
  "Return Donchian upper, middle, and lower channels for BARS.
PERIOD defaults to 20; bounds are rolling high and low, with their
midpoint as the centerline.  Missing values and warmup are nil."
  (setq period (or period 20))
  (financial-chart--volatility-check-window period)
  (let* ((upper (financial-chart--volatility-extreme bars :high period #'max))
         (lower (financial-chart--volatility-extreme bars :low period #'min))
         (middle (cl-mapcar (lambda (high low)
                              (when (and high low) (/ (+ high low) 2.0)))
                            upper lower)))
    (list (list :name 'donchian-upper :label "Donchian Upper" :values upper)
          (list :name 'donchian-middle :label "Donchian Middle" :values middle)
          (list :name 'donchian-lower :label "Donchian Lower" :values lower))))

(financial-chart-register-indicator
 'atr #'financial-chart-atr
 :label "ATR" :unit :price :panel :oscillator :scale :linear
 :description "Average true range with Wilder smoothing.")
(financial-chart-register-indicator
 'bollinger-bands #'financial-chart-bollinger-bands
 :label "Bollinger Bands" :unit :price :panel :overlay :scale :linear
 :description "Rolling mean and population-standard-deviation bands.")
(financial-chart-register-indicator
 'keltner-channels #'financial-chart-keltner-channels
 :label "Keltner Channels" :unit :price :panel :overlay :scale :linear
 :description "EMA centerline and ATR-scaled channels.")
(financial-chart-register-indicator
 'donchian-channels #'financial-chart-donchian-channels
 :label "Donchian Channels" :unit :price :panel :overlay :scale :linear
 :description "Rolling high, low, and midpoint channels.")

(provide 'financial-chart-volatility)
;;; financial-chart-volatility.el ends here
