;;; financial-chart-setups.el --- indicators behind the named setups: squeeze, impulse, force index -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The indicators the named presets of presets/ need that the catalog
;; did not have (fc-nyt), each from a published definition:
;;
;;   wilder-rsi     RSI with Wilder's smoothing (Wilder 1978, New
;;                  Concepts in Technical Trading Systems).  The
;;                  registered `rsi' is the simple-average variant.
;;   force-index    Elder's force index (Elder 1993, Trading for a
;;                  Living; 2002, Come Into My Trading Room):
;;                  EMA of (close - previous close) x volume.
;;   range-52w      the highest high and lowest low of the last 252 bars
;;                  (a trading year), the "52-week" lines of Minervini
;;                  2013.  Until 252 bars exist the window is the bars
;;                  so far.
;;   ttm-squeeze    John Carter's squeeze (2005, Mastering the Trade): Bollinger Bands inside Keltner Channels,
;;                  and the momentum histogram.
;;   elder-impulse  Elder's impulse system (2002, Come Into My Trading
;;                  Room): the slope of the EMA and of the MACD
;;                  histogram agreeing.
;;
;; Every value at bar I depends only on bars 0..I (the harness in
;; financial-chart-setups-test.el truncates and compares).  Missing
;; inputs and warm-up are nil.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)
(require 'financial-chart-momentum)
(require 'financial-chart-volatility)

(defun financial-chart-setups--period (value default name)
  "VALUE (or DEFAULT when nil) as a positive integer called NAME."
  (let ((value (or value default)))
    (unless (and (integerp value) (> value 0))
      (signal 'financial-chart-invalid-indicator
              (list (format "%s must be a positive integer, got %S" name value))))
    value))

(defun financial-chart-setups--positive (value default name)
  "VALUE (or DEFAULT when nil) as a positive number called NAME."
  (let ((value (or value default)))
    (unless (and (numberp value) (> value 0))
      (signal 'financial-chart-invalid-indicator
              (list (format "%s must be a positive number, got %S" name value))))
    value))

(defun financial-chart-setups--closes (bars)
  "The numeric closes of BARS, nil where missing."
  (mapcar (lambda (b) (let ((c (plist-get b :close))) (and (numberp c) c))) bars))

;;; Wilder RSI

(defun financial-chart-wilder-rsi (bars &optional period)
  "Wilder's RSI of BARS' closes over PERIOD (14) bars.
The first average gain and loss are simple means of the first PERIOD
changes; each later one is (previous x (PERIOD - 1) + current) / PERIOD.
RSI is 100 when the average loss is zero.  A missing close restarts it."
  (let* ((period (financial-chart-setups--period period 14 "period"))
         (previous nil) (gain nil) (loss nil) (seed nil) result)
    (dolist (close (financial-chart-setups--closes bars))
      (push
       (cond
        ((null close) (setq previous nil gain nil loss nil seed nil) nil)
        ((null previous) (setq previous close) nil)
        (t
         (let ((change (- close previous)))
           (setq previous close)
           (if gain
               (setq gain (/ (+ (* gain (1- period)) (max change 0.0)) period)
                     loss (/ (+ (* loss (1- period)) (max (- change) 0.0)) period))
             (push change seed)
             (when (= (length seed) period)
               (setq gain (/ (apply #'+ (mapcar (lambda (c) (max c 0.0)) seed)) (float period))
                     loss (/ (apply #'+ (mapcar (lambda (c) (max (- c) 0.0)) seed)) (float period)))))
           (and gain (if (zerop loss) 100.0 (- 100.0 (/ 100.0 (+ 1.0 (/ gain loss)))))))))
       result))
    (nreverse result)))

;;; Force index

(defun financial-chart-force-index (bars &optional period)
  "Elder's force index of BARS: the PERIOD (13) EMA of (close - previous
close) x volume.  PERIOD 2 is the short-term form, 13 the intermediate.
A bar without a volume or a close has no value."
  (let* ((period (financial-chart-setups--period period 13 "period"))
         (previous nil)
         (raw (mapcar (lambda (b)
                        (let ((c (plist-get b :close)) (v (plist-get b :volume)))
                          (prog1 (and (numberp c) (numberp v) previous (* (- c previous) v))
                            (setq previous (and (numberp c) c)))))
                      bars)))
    (financial-chart--volatility-ema raw period)))

;;; 52-week range

(defun financial-chart-range-52w (bars &optional period)
  "Highest high and lowest low of the last PERIOD (252) bars of BARS.
Until PERIOD bars exist the window is the bars so far."
  (let* ((period (financial-chart-setups--period period 252 "period"))
         (v (vconcat bars)) (highs nil) (lows nil))
    (dotimes (i (length v))
      (let ((window (cl-loop for j from (max 0 (- i period -1)) to i collect (aref v j))))
        (push (cl-loop for b in window for h = (plist-get b :high) when (numberp h) maximize h) highs)
        (push (cl-loop for b in window for l = (plist-get b :low) when (numberp l) minimize l) lows)))
    (list (list :name 'range-high :label "52-week high" :values (nreverse highs))
          (list :name 'range-low :label "52-week low" :values (nreverse lows)))))

;;; TTM Squeeze

(defun financial-chart-setups--linreg-end (ys)
  "The least-squares line through YS (x = 0..n-1) evaluated at its last x."
  (let* ((n (length ys)) (xm (/ (1- n) 2.0)) (ym (/ (apply #'+ ys) (float n)))
         (sxx (cl-loop for i from 0 below n sum (expt (- i xm) 2)))
         (sxy (cl-loop for y in ys for i from 0 sum (* (- i xm) (- y ym))))
         (slope (if (zerop sxx) 0.0 (/ sxy sxx))))
    (+ ym (* slope (- (1- n) xm)))))

(defun financial-chart-ttm-squeeze (bars &optional period bb-multiplier kc-multiplier)
  "Carter's squeeze of BARS over PERIOD (20) bars.
Bollinger Bands (PERIOD, BB-MULTIPLIER 2 population deviations) against
Keltner Channels (SMA PERIOD +- KC-MULTIPLIER 1.5 x Wilder ATR PERIOD).
The squeeze is on while both Bollinger bands are inside the Keltner
channel.  The momentum is the least-squares line, taken at its last
bar, through the last PERIOD values of close - mean(mean(highest
high, lowest low), SMA).  Outputs: squeeze-bb-upper and -lower,
squeeze-kc-upper and -lower, squeeze-on and squeeze-off (0.0 on the
bars in that state, else nil, to draw as dots on a zero line),
squeeze-momentum and the same momentum split by sign and slope:
squeeze-mom-pos-rising, -pos-falling, -neg-falling, -neg-rising."
  (let* ((period (financial-chart-setups--period period 20 "period"))
         (bb-m (financial-chart-setups--positive bb-multiplier 2.0 "bb-multiplier"))
         (kc-m (financial-chart-setups--positive kc-multiplier 1.5 "kc-multiplier"))
         (n (length bars))
         (closes (financial-chart-setups--closes bars))
         (mean (lambda (i) (let ((s (financial-chart--volatility-contiguous-window closes i period)))
                             (and s (financial-chart--volatility-mean s)))))
         (sma (cl-loop for i below n collect (funcall mean i)))
         (sd (cl-loop for i below n for m in sma
                      collect (and m (let ((s (financial-chart--volatility-contiguous-window closes i period)))
                                       (sqrt (/ (cl-loop for c in s sum (expt (- c m) 2)) (float period)))))))
         (atr (financial-chart-atr bars period))
         (hh (financial-chart--volatility-extreme bars :high period #'max))
         (ll (financial-chart--volatility-extreme bars :low period #'min))
         (bb-up (cl-mapcar (lambda (m s) (and m (+ m (* bb-m s)))) sma sd))
         (bb-lo (cl-mapcar (lambda (m s) (and m (- m (* bb-m s)))) sma sd))
         (kc-up (cl-mapcar (lambda (m a) (and m a (+ m (* kc-m a)))) sma atr))
         (kc-lo (cl-mapcar (lambda (m a) (and m a (- m (* kc-m a)))) sma atr))
         (on (cl-mapcar (lambda (bu bl ku kl) (and bu ku (and (< bu ku) (> bl kl)))) bb-up bb-lo kc-up kc-lo))
         (known (cl-mapcar (lambda (bu ku) (and bu ku)) bb-up kc-up))
         (delta (cl-mapcar (lambda (c h l m) (and c h l m (- c (/ (+ (/ (+ h l) 2.0) m) 2.0))))
                           closes hh ll sma))
         (momentum (cl-loop for i below n
                            for s = (financial-chart--volatility-contiguous-window delta i period)
                            collect (and s (financial-chart-setups--linreg-end s))))
         (previous (cons nil momentum))
         (klass (lambda (want)
                  (cl-mapcar
                   (lambda (m p)
                     (and m p
                          (eq want (if (>= m 0) (if (> m p) 'pos-rising 'pos-falling)
                                     (if (< m p) 'neg-falling 'neg-rising)))
                          m))
                   momentum previous))))
    (list (list :name 'squeeze-bb-upper :label "BB upper" :values bb-up)
          (list :name 'squeeze-bb-lower :label "BB lower" :values bb-lo)
          (list :name 'squeeze-kc-upper :label "KC upper" :values kc-up)
          (list :name 'squeeze-kc-lower :label "KC lower" :values kc-lo)
          (list :name 'squeeze-on :label "Squeeze on"
                :values (cl-mapcar (lambda (k o) (and k o 0.0)) known on))
          (list :name 'squeeze-off :label "Squeeze off"
                :values (cl-mapcar (lambda (k o) (and k (not o) 0.0)) known on))
          (list :name 'squeeze-momentum :label "Squeeze momentum" :values momentum)
          (list :name 'squeeze-mom-pos-rising :label "Momentum up, rising" :values (funcall klass 'pos-rising))
          (list :name 'squeeze-mom-pos-falling :label "Momentum up, falling" :values (funcall klass 'pos-falling))
          (list :name 'squeeze-mom-neg-falling :label "Momentum down, falling" :values (funcall klass 'neg-falling))
          (list :name 'squeeze-mom-neg-rising :label "Momentum down, rising" :values (funcall klass 'neg-rising)))))

;;; Elder impulse

(defun financial-chart-elder-impulse (bars &optional ema-period fast slow signal)
  "Elder's impulse system of BARS.
A bar is bullish when both the EMA-PERIOD (13) EMA of the close and the
MACD (FAST 12, SLOW 26, SIGNAL 9) histogram are above their previous
values, bearish when both are below, else neutral.  Outputs
impulse-bull, impulse-neutral and impulse-bear: 0.0 on the bars in that
state, else nil (dots on a strip); nil everywhere a slope is unknown."
  (let* ((ema-period (financial-chart-setups--period ema-period 13 "ema-period"))
         (ema (financial-chart--volatility-ema (financial-chart-setups--closes bars) ema-period))
         (hist (plist-get (nth 2 (financial-chart-macd bars fast slow signal)) :values))
         (state (cl-mapcar (lambda (e h pe ph)
                             (and e h pe ph
                                  (cond ((and (> e pe) (> h ph)) 'bull)
                                        ((and (< e pe) (< h ph)) 'bear)
                                        (t 'neutral))))
                           ema hist (cons nil ema) (cons nil hist)))
         (only (lambda (want) (mapcar (lambda (s) (and (eq s want) 0.0)) state))))
    (list (list :name 'impulse-bull :label "Impulse bull" :values (funcall only 'bull))
          (list :name 'impulse-neutral :label "Impulse neutral" :values (funcall only 'neutral))
          (list :name 'impulse-bear :label "Impulse bear" :values (funcall only 'bear)))))

(financial-chart-register-indicator
 'wilder-rsi #'financial-chart-wilder-rsi
 :label "RSI (Wilder)" :unit :percent :panel :oscillator :scale :bounded :bounds '(0 . 100)
 :description "Relative strength index with Wilder's smoothing (Wilder 1978).")
(financial-chart-register-indicator
 'force-index #'financial-chart-force-index
 :label "Force index" :unit :volume :panel :oscillator :scale :linear
 :description "Elder's force index: EMA of (close - previous close) x volume.")
(financial-chart-register-indicator
 'range-52w #'financial-chart-range-52w
 :label "52-week range" :unit :price :panel :overlay :scale :price
 :description "Highest high and lowest low of the last 252 bars (the bars so far until then).")
(financial-chart-register-indicator
 'ttm-squeeze #'financial-chart-ttm-squeeze
 :label "TTM Squeeze" :unit :price :panel :oscillator :scale :linear
 :description "Bollinger Bands inside Keltner Channels (squeeze on) and Carter's momentum histogram.")
(financial-chart-register-indicator
 'elder-impulse #'financial-chart-elder-impulse
 :label "Elder impulse" :unit :state :panel :oscillator :scale :linear
 :description "EMA and MACD-histogram slopes agreeing: bull, bear or neutral.")

(provide 'financial-chart-setups)
;;; financial-chart-setups.el ends here
