;;; financial-chart-trend-indicators.el --- Trend-strength indicators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provider-neutral trend-strength calculators for normalized OHLC bars:
;; Directional Movement (DMI/ADX), Aroon, and Parabolic SAR.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)

(defun financial-chart--trend-number (bar field)
  "Return BAR's numeric FIELD, or nil when missing or nonnumeric."
  (let ((value (plist-get bar field)))
    (and (numberp value) value)))

(defun financial-chart-dmi (bars &optional period)
  "Return +DI, -DI, and ADX outputs for BARS using Wilder PERIOD.
PERIOD defaults to 14.  Each output is aligned to BARS and uses nil
for warmup or missing data.  The first DI values appear after PERIOD
valid changes; ADX appears after PERIOD valid DX values."
  (let* ((period (or period 14))
         (n (length bars))
         (plus (make-list n nil))
         (minus (make-list n nil))
         (adx (make-list n nil))
         (tr-sum 0.0) (plus-sum 0.0) (minus-sum 0.0)
         (change-count 0)
         (dx-seed 0.0) (dx-count 0) (adx-value nil)
         (prev-high nil) (prev-low nil) (prev-close nil))
    (unless (and (integerp period) (> period 0))
      (error "PERIOD must be a positive integer"))
    (cl-loop for bar in bars for i from 0
             for high = (financial-chart--trend-number bar :high)
             for low = (financial-chart--trend-number bar :low)
             for close = (financial-chart--trend-number bar :close)
             do
             (if (and high low close prev-high prev-low prev-close)
                 (let* ((up (- high prev-high))
                        (down (- prev-low low))
                        (plus-dm (if (and (> up down) (> up 0)) up 0.0))
                        (minus-dm (if (and (> down up) (> down 0)) down 0.0))
                        (tr (max (- high low)
                                 (abs (- high prev-close))
                                 (abs (- low prev-close)))))
                   (if (< change-count period)
                       (setq tr-sum (+ tr-sum tr)
                             plus-sum (+ plus-sum plus-dm)
                             minus-sum (+ minus-sum minus-dm)
                             change-count (1+ change-count))
                     (setq tr-sum (+ (- tr-sum (/ tr-sum period)) tr)
                           plus-sum (+ (- plus-sum (/ plus-sum period)) plus-dm)
                           minus-sum (+ (- minus-sum (/ minus-sum period)) minus-dm)))
                   (when (>= change-count period)
                     (let* ((plus-di (if (zerop tr-sum) 0.0
                                       (* 100.0 (/ plus-sum tr-sum))))
                            (minus-di (if (zerop tr-sum) 0.0
                                        (* 100.0 (/ minus-sum tr-sum))))
                            (denominator (+ plus-di minus-di))
                            (dx (if (zerop denominator) 0.0
                                  (* 100.0 (/ (abs (- plus-di minus-di))
                                              denominator)))))
                       (setf (nth i plus) plus-di
                             (nth i minus) minus-di)
                       (if adx-value
                           (setq adx-value
                                 (/ (+ (* (1- period) adx-value) dx)
                                    (float period)))
                         (setq dx-seed (+ dx-seed dx)
                               dx-count (1+ dx-count))
                         (when (= dx-count period)
                           (setq adx-value (/ dx-seed (float period)))))
                       (when adx-value (setf (nth i adx) adx-value)))))
               (setq tr-sum 0.0 plus-sum 0.0 minus-sum 0.0
                     change-count 0 dx-seed 0.0 dx-count 0 adx-value nil))
             do (setq prev-high high prev-low low prev-close close))
    (list (list :name 'plus-di :label "+DI" :values plus)
          (list :name 'minus-di :label "-DI" :values minus)
          (list :name 'adx :label "ADX" :values adx))))

(defun financial-chart-aroon (bars &optional period)
  "Return Aroon Up and Aroon Down for BARS over PERIOD bars.
PERIOD defaults to 25.  Uses PERIOD+1 bars per standard lookback,
with the newest tied extreme winning.  Results align with BARS."
  (let* ((period (or period 25))
         (n (length bars))
         (up-values (make-list n nil))
         (down-values (make-list n nil)))
    (unless (and (integerp period) (> period 0))
      (error "PERIOD must be a positive integer"))
    (cl-loop for i from period below n
             for start = (- i period)
             for window = (cl-subseq bars start (1+ i))
             for highs = (mapcar (lambda (bar)
                                  (financial-chart--trend-number bar :high))
                                window)
             for lows = (mapcar (lambda (bar)
                                 (financial-chart--trend-number bar :low))
                               window)
             unless (or (memq nil highs) (memq nil lows))
             do
             (let* ((since-high (cl-position (apply #'max highs) highs
                                             :from-end t :test #'=))
                    (since-low (cl-position (apply #'min lows) lows
                                            :from-end t :test #'=)))
               (setf (nth i up-values)
                     (* 100.0 (/ (- period (- period since-high))
                                 (float period)))
                     (nth i down-values)
                     (* 100.0 (/ (- period (- period since-low))
                                 (float period))))))
    (list (list :name 'aroon-up :label "Aroon Up" :values up-values)
          (list :name 'aroon-down :label "Aroon Down" :values down-values))))

(defun financial-chart-parabolic-sar (bars &optional acceleration maximum)
  "Return Parabolic SAR values aligned to BARS.
ACCELERATION defaults to 0.02 and MAXIMUM to 0.2.  The direction is
seeded from the first two valid bars; missing OHLC data restarts the
calculation, leaving nil through the next seed bar."
  (let* ((acceleration (or acceleration 0.02))
         (maximum (or maximum 0.2))
         (n (length bars))
         (result (make-list n nil))
         (prior-bar nil)
         (direction nil) (sar nil) (extreme nil) (af acceleration)
         (older-low nil) (older-high nil))
    (unless (and (numberp acceleration) (> acceleration 0)
                 (numberp maximum) (>= maximum acceleration))
      (error "MAXIMUM must be numeric and at least positive ACCELERATION"))
    (cl-loop for bar in bars for i from 0
             for high = (financial-chart--trend-number bar :high)
             for low = (financial-chart--trend-number bar :low)
             for close = (financial-chart--trend-number bar :close)
             do
             (if (not (and high low close))
                 (setq prior-bar nil direction nil sar nil extreme nil
                       af acceleration older-low nil older-high nil)
               (if (not prior-bar)
                   (setq prior-bar bar older-low low older-high high)
                 (if (not direction)
                     (progn
                       (setq direction (if (>= close (plist-get prior-bar :close))
                                           'up 'down)
                             sar (if (eq direction 'up)
                                     (plist-get prior-bar :low)
                                   (plist-get prior-bar :high))
                             extreme (if (eq direction 'up) high low)
                             af acceleration)
                       (setf (nth i result) sar))
                   (let ((candidate (+ sar (* af (- extreme sar)))))
                     (if (eq direction 'up)
                         (progn
                           (setq candidate (min candidate
                                                (plist-get prior-bar :low)
                                                older-low))
                           (if (< low candidate)
                               (setq direction 'down sar extreme extreme low
                                     af acceleration)
                             (setq sar candidate)
                             (when (> high extreme)
                               (setq extreme high
                                     af (min maximum (+ af acceleration)))))
                           (setf (nth i result) sar))
                       (setq candidate (max candidate
                                            (plist-get prior-bar :high)
                                            older-high))
                       (if (> high candidate)
                           (setq direction 'up sar extreme extreme high
                                 af acceleration)
                         (setq sar candidate)
                         (when (< low extreme)
                           (setq extreme low
                                 af (min maximum (+ af acceleration)))))
                       (setf (nth i result) sar)))
                 (setq older-low (plist-get prior-bar :low)
                       older-high (plist-get prior-bar :high)
                       prior-bar bar)))))
    result))

(financial-chart-register-indicator
 'dmi #'financial-chart-dmi
 :label "DMI / ADX" :unit :percent :panel :oscillator :scale :bounded
 :bounds '(0 . 100) :description "Directional Movement Index and ADX.")
(financial-chart-register-indicator
 'aroon #'financial-chart-aroon
 :label "Aroon" :unit :percent :panel :oscillator :scale :bounded
 :bounds '(0 . 100) :description "Aroon Up and Aroon Down.")
(financial-chart-register-indicator
 'parabolic-sar #'financial-chart-parabolic-sar
 :label "Parabolic SAR" :unit :price :panel :overlay :scale :linear
 :description "Wilder's Parabolic Stop and Reverse.")

(provide 'financial-chart-trend-strength)
;;; financial-chart-trend-indicators.el ends here

