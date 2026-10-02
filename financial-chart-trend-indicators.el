;;; financial-chart-trend-indicators.el --- Trend indicators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Pure-Elisp, provider-neutral moving-average indicators.  Inputs are
;; normalized bars with :close values; outputs stay aligned with the bars
;; and use nil for warm-up periods or missing data.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)

(defun financial-chart-trend--closes (bars)
  "Extract closes from normalized BARS."
  (mapcar (lambda (bar) (plist-get bar :close)) bars))

(defun financial-chart-trend--positive-period (period default name)
  "Return PERIOD or DEFAULT, signaling if it is not a positive integer."
  (setq period (or period default))
  (unless (and (integerp period) (> period 0))
    (signal 'financial-chart-invalid-indicator
            (list (format "%s period must be a positive integer" name)
                  :period period)))
  period)

(defun financial-chart-trend--wma-values (values period)
  "Compute PERIOD weighted moving averages over VALUES.
Each complete window is weighted from 1 for oldest to PERIOD for newest."
  (let ((weight-sum (/ (* period (1+ period)) 2.0))
        (result nil))
    (dotimes (index (length values))
      (let ((start (- index (1- period)))
            (weighted 0.0)
            (weight 1)
            (valid t))
        (if (< start 0)
            (push nil result)
          (dolist (item (cl-subseq values start (1+ index)))
            (if (numberp item)
                (setq weighted (+ weighted (* weight item)))
              (setq valid nil))
            (setq weight (1+ weight)))
          (push (and valid (/ weighted weight-sum)) result))))
    (nreverse result)))

(defun financial-chart-trend--ema-values (values period)
  "Compute an EMA with PERIOD and an initial simple-average seed.
A nil input emits nil and resets the seed, so later values can recover."
  (let ((alpha (/ 2.0 (1+ period)))
        (seed nil)
        (ema nil)
        (result nil))
    (dolist (value values)
      (cond
       ((not (numberp value))
        (setq seed nil ema nil)
        (push nil result))
       (ema
        (setq ema (+ (* alpha value) (* (- 1.0 alpha) ema)))
        (push ema result))
       (t
        (push value seed)
        (if (= (length seed) period)
            (progn
              (setq ema (/ (apply #'+ seed) (float period)))
              (setq seed nil)
              (push ema result))
          (push nil result)))))
    (nreverse result)))

(defun financial-chart-trend-wma (bars &optional period)
  "Return weighted moving average of BARS, defaulting to period 20."
  (setq period (financial-chart-trend--positive-period period 20 "WMA"))
  (financial-chart-trend--wma-values
   (financial-chart-trend--closes bars) period))

(defun financial-chart-trend-dema (bars &optional period)
  "Return double exponential moving average of BARS, defaulting to 20."
  (setq period (financial-chart-trend--positive-period period 20 "DEMA"))
  (let* ((close (financial-chart-trend--closes bars))
         (ema1 (financial-chart-trend--ema-values close period))
         (ema2 (financial-chart-trend--ema-values ema1 period)))
    (cl-mapcar (lambda (one two) (and one two (- (* 2.0 one) two)))
               ema1 ema2)))

(defun financial-chart-trend-tema (bars &optional period)
  "Return triple exponential moving average of BARS, defaulting to 20."
  (setq period (financial-chart-trend--positive-period period 20 "TEMA"))
  (let* ((close (financial-chart-trend--closes bars))
         (ema1 (financial-chart-trend--ema-values close period))
         (ema2 (financial-chart-trend--ema-values ema1 period))
         (ema3 (financial-chart-trend--ema-values ema2 period)))
    (cl-mapcar (lambda (one two three)
                 (and one two three (- (+ (* 3.0 one) three) (* 3.0 two))))
               ema1 ema2 ema3)))

(defun financial-chart-trend-hma (bars &optional period)
  "Return Hull moving average of BARS, defaulting to period 16."
  (setq period (financial-chart-trend--positive-period period 16 "HMA"))
  (let* ((half (max 1 (/ period 2)))
         (root (max 1 (floor (sqrt period))))
         (close (financial-chart-trend--closes bars))
         (half-wma (financial-chart-trend--wma-values close half))
         (full-wma (financial-chart-trend--wma-values close period))
         (diff (cl-mapcar (lambda (half-value full-value)
                            (and half-value full-value
                                 (- (* 2.0 half-value) full-value)))
                          half-wma full-wma)))
    (financial-chart-trend--wma-values diff root)))

(defun financial-chart-trend-kama (bars &optional period fast slow)
  "Return Kaufman adaptive moving average of BARS.
PERIOD defaults to 10; FAST and SLOW smoothing periods default to 2 and 30.
The first output appears after PERIOD price changes, seeded from the close
at that point.  Missing closes reset the seed and warm-up."
  (setq period (financial-chart-trend--positive-period period 10 "KAMA")
        fast (financial-chart-trend--positive-period fast 2 "KAMA fast")
        slow (financial-chart-trend--positive-period slow 30 "KAMA slow"))
  (let* ((closes (financial-chart-trend--closes bars))
         (fast-alpha (/ 2.0 (1+ fast)))
         (slow-alpha (/ 2.0 (1+ slow)))
         (history nil)
         (count 0)
         (previous nil)
         (result nil))
    (dolist (close closes)
      (if (not (numberp close))
          (progn (setq history nil count 0 previous nil) (push nil result))
        (push close history)
        (setq count (1+ count))
        (when (> (length history) (1+ period))
          (setq history (butlast history)))
        (if (<= count period)
            (push nil result)
          (let* ((window (cl-subseq history 0 (1+ period)))
                 (change (abs (- (car window) (car (last window)))))
                 (volatility
                  (cl-loop for newer in window
                           for older in (cdr window)
                           sum (abs (- newer older))))
                 (efficiency (if (zerop volatility) 0.0
                               (/ change volatility)))
                 (smoothing
                  (expt (+ slow-alpha
                           (* efficiency (- fast-alpha slow-alpha))) 2)))
            (setq previous (if previous
                               (+ previous (* smoothing (- close previous)))
                             close))
            (push previous result)))))
    (nreverse result)))

(financial-chart-register-indicator
 'wma #'financial-chart-trend-wma
 :label "WMA" :unit :price :panel :overlay :scale :price
 :description "Weighted moving average")
(financial-chart-register-indicator
 'dema #'financial-chart-trend-dema
 :label "DEMA" :unit :price :panel :overlay :scale :price
 :description "Double exponential moving average")
(financial-chart-register-indicator
 'tema #'financial-chart-trend-tema
 :label "TEMA" :unit :price :panel :overlay :scale :price
 :description "Triple exponential moving average")
(financial-chart-register-indicator
 'hma #'financial-chart-trend-hma
 :label "HMA" :unit :price :panel :overlay :scale :price
 :description "Hull moving average")
(financial-chart-register-indicator
 'kama #'financial-chart-trend-kama
 :label "KAMA" :unit :price :panel :overlay :scale :price
 :description "Kaufman adaptive moving average")

(provide 'financial-chart-trend-indicators)
;;; financial-chart-trend-indicators.el ends here
