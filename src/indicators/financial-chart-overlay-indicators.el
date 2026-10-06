;;; financial-chart-overlay-indicators.el --- Ichimoku, envelopes, VWAP bands, pivots, SuperTrend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Price-scale indicators for the composition DSL's catalog (fc-gbo.3),
;; computed from the bars the caller supplies; nothing is fetched.
;;
;;   ichimoku       tenkan, kijun, senkou A/B and chikou; the senkou
;;                  spans carry :shift DISPLACEMENT (drawn that many bars
;;                  ahead, past the last bar) and chikou :shift
;;                  -DISPLACEMENT, so values stay aligned with the bars
;;   envelopes      a moving average and bands PERCENT above and below
;;   vwap-bands     cumulative VWAP with volume-weighted deviation bands
;;   pivot-points   classic, fibonacci, woodie or camarilla levels from the
;;                  previous day, week, month, year or block of N bars
;;   supertrend     the ATR trailing stop, split into up and down legs
;;
;; Each registers with `financial-chart-register-indicator'.  A bad
;; parameter signals `financial-chart-invalid-indicator' saying which.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)
(require 'financial-chart-volatility)

(defun financial-chart-overlay--fail (format-string &rest args)
  "Signal `financial-chart-invalid-indicator' with FORMAT-STRING and ARGS."
  (signal 'financial-chart-invalid-indicator (list (apply #'format format-string args))))

(defun financial-chart-overlay--period (value default name)
  "VALUE (or DEFAULT when nil) checked as a positive integer called NAME."
  (let ((value (or value default)))
    (unless (and (integerp value) (> value 0))
      (financial-chart-overlay--fail "%s must be a positive integer, got %S" name value))
    value))

(defun financial-chart-overlay--positive (value default name)
  "VALUE (or DEFAULT when nil) checked as a non-negative number called NAME."
  (let ((value (or value default)))
    (unless (and (numberp value) (>= value 0))
      (financial-chart-overlay--fail "%s must be a non-negative number, got %S" name value))
    value))

(defun financial-chart-overlay--midpoints (bars period)
  "Midpoint of the highest high and lowest low of BARS over PERIOD bars."
  (cl-mapcar (lambda (hi lo) (and hi lo (/ (+ hi lo) 2.0)))
             (financial-chart--volatility-extreme bars :high period #'max)
             (financial-chart--volatility-extreme bars :low period #'min)))

;;; Ichimoku

(defun financial-chart-ichimoku (bars &optional tenkan kijun senkou displacement)
  "Ichimoku Kinko Hyo of BARS.
TENKAN (9), KIJUN (26) and SENKOU (52) are the conversion, base and
leading-span-B periods; each line is the midpoint of the period's
highest high and lowest low.  Senkou A is the mean of tenkan and kijun.
Values align with the bars they are computed from: the two senkou
spans carry :shift DISPLACEMENT (26), drawn that many bars later, and
chikou (the close) :shift -DISPLACEMENT, drawn that many bars earlier."
  (let* ((tenkan (financial-chart-overlay--period tenkan 9 "tenkan"))
         (kijun (financial-chart-overlay--period kijun 26 "kijun"))
         (senkou (financial-chart-overlay--period senkou 52 "senkou"))
         (displacement (financial-chart-overlay--period displacement 26 "displacement"))
         (_ (when (> displacement 500)
              (financial-chart-overlay--fail "displacement must be at most 500 bars, got %d" displacement)))
         (conversion (financial-chart-overlay--midpoints bars tenkan))
         (base (financial-chart-overlay--midpoints bars kijun)))
    (list (list :name 'ichimoku-tenkan :label "Tenkan-sen" :values conversion)
          (list :name 'ichimoku-kijun :label "Kijun-sen" :values base)
          (list :name 'ichimoku-senkou-a :label "Senkou Span A" :shift displacement
                :values (cl-mapcar (lambda (c b) (and c b (/ (+ c b) 2.0))) conversion base))
          (list :name 'ichimoku-senkou-b :label "Senkou Span B" :shift displacement
                :values (financial-chart-overlay--midpoints bars senkou))
          (list :name 'ichimoku-chikou :label "Chikou Span" :shift (- displacement)
                :values (mapcar (lambda (b) (let ((c (plist-get b :close))) (and (numberp c) c)))
                                bars)))))

;;; Envelopes

(defun financial-chart-envelopes (bars &optional period percent)
  "Moving-average envelopes of BARS' closes.
The middle is the PERIOD-bar (20) simple average; the bands sit PERCENT
\(2.5) percent above and below it."
  (let* ((period (financial-chart-overlay--period period 20 "period"))
         (percent (financial-chart-overlay--positive percent 2.5 "percent"))
         (closes (mapcar (lambda (b) (plist-get b :close)) bars))
         (middle (cl-loop for i from 0 below (length bars)
                          for slice = (financial-chart--volatility-contiguous-window closes i period)
                          collect (and slice (financial-chart--volatility-mean slice))))
         (k (/ percent 100.0)))
    (list (list :name 'envelope-upper :label "Envelope Upper"
                :values (mapcar (lambda (m) (and m (* m (+ 1 k)))) middle))
          (list :name 'envelope-middle :label "Envelope Middle" :values middle)
          (list :name 'envelope-lower :label "Envelope Lower"
                :values (mapcar (lambda (m) (and m (* m (- 1 k)))) middle)))))

;;; VWAP bands

(defun financial-chart-vwap-bands (bars &rest multipliers)
  "Cumulative VWAP of BARS with deviation bands at MULTIPLIERS (1 and 2).
Each bar's typical price (high+low+close)/3 is weighted by its volume;
the deviation is the volume-weighted standard deviation of typical
price about the VWAP so far.  A bar without volume has no value and
does not count.  Outputs: vwap, then vwap-upper-M and vwap-lower-M."
  (let ((multipliers (or multipliers '(1 2)))
        (pv 0.0) (pv2 0.0) (vol 0.0) vwaps devs)
    (dolist (m multipliers)
      (unless (and (numberp m) (> m 0))
        (financial-chart-overlay--fail "vwap-bands multipliers must be positive numbers, got %S" m)))
    (dolist (b bars)
      (let ((v (plist-get b :volume)) (h (plist-get b :high))
            (l (plist-get b :low)) (c (plist-get b :close)))
        (if (not (and (numberp v) (numberp h) (numberp l) (numberp c)))
            (progn (push nil vwaps) (push nil devs))
          (let ((tp (/ (+ h l c) 3.0)))
            (setq pv (+ pv (* tp v)) pv2 (+ pv2 (* tp tp v)) vol (+ vol v))
            (if (zerop vol)
                (progn (push nil vwaps) (push nil devs))
              (let ((mean (/ pv vol)))
                (push mean vwaps)
                (push (sqrt (max 0.0 (- (/ pv2 vol) (* mean mean)))) devs)))))))
    (setq vwaps (nreverse vwaps) devs (nreverse devs))
    (cons (list :name 'vwap :label "VWAP" :values vwaps)
          (cl-loop for m in multipliers
                   for tag = (format "%s" m)
                   append
                   (list (list :name (intern (concat "vwap-upper-" tag)) :label (format "VWAP +%sσ" m)
                               :values (cl-mapcar (lambda (w d) (and w (+ w (* m d)))) vwaps devs))
                         (list :name (intern (concat "vwap-lower-" tag)) :label (format "VWAP -%sσ" m)
                               :values (cl-mapcar (lambda (w d) (and w (- w (* m d)))) vwaps devs)))))))

;;; Pivot points

(defconst financial-chart-pivot-methods '("classic" "fibonacci" "woodie" "camarilla")
  "Pivot point formulas `financial-chart-pivot-points' knows.")

(defun financial-chart-pivot-levels (method high low close)
  "Pivot levels (PP R1 R2 R3 S1 S2 S3) by METHOD from HIGH, LOW and CLOSE."
  (let ((range (- high low)))
    (pcase method
      ("classic"
       (let ((pp (/ (+ high low close) 3.0)))
         (list pp (- (* 2 pp) low) (+ pp range) (+ high (* 2 (- pp low)))
               (- (* 2 pp) high) (- pp range) (- low (* 2 (- high pp))))))
      ("fibonacci"
       (let ((pp (/ (+ high low close) 3.0)))
         (list pp (+ pp (* 0.382 range)) (+ pp (* 0.618 range)) (+ pp range)
               (- pp (* 0.382 range)) (- pp (* 0.618 range)) (- pp range))))
      ("woodie"
       (let ((pp (/ (+ high low (* 2 close)) 4.0)))
         (list pp (- (* 2 pp) low) (+ pp range) (+ high (* 2 (- pp low)))
               (- (* 2 pp) high) (- pp range) (- low (* 2 (- high pp))))))
      ("camarilla"
       (let ((r (* 1.1 range)))
         (list (/ (+ high low close) 3.0)
               (+ close (/ r 12)) (+ close (/ r 6)) (+ close (/ r 4))
               (- close (/ r 12)) (- close (/ r 6)) (- close (/ r 4)))))
      (_ (financial-chart-overlay--fail "Unknown pivot method %S; methods: %s" method
                                        (mapconcat #'identity financial-chart-pivot-methods ", "))))))

(defun financial-chart-pivot--auto-period (bars)
  "The pivot period for BARS: day intraday, month daily, year beyond, else 20."
  (let* ((times (mapcar (lambda (b) (plist-get b :time)) bars))
         (gaps (and (cl-every #'numberp times)
                    (sort (cl-loop for (a b) on times while b collect (- b a)) #'<)))
         (gap (and gaps (nth (/ (length gaps) 2) gaps))))
    (cond ((not gap) 20)
          ((< gap 86400000) "day")
          ((< gap (* 7 86400000)) "month")
          (t "year"))))

(defun financial-chart-pivot--key (period time index)
  "The PERIOD bucket of a bar at TIME (epoch ms) and INDEX."
  (if (integerp period) (/ index period)
    (unless (numberp time)
      (financial-chart-overlay--fail "Pivot period %S needs bars with a numeric time; use a bar count" period))
    (let* ((days (floor time 86400000))
           (decoded (decode-time (floor time 1000) t)))
      (pcase period
        ("day" days)
        ;; Weeks start on Monday; 1970-01-01 was a Thursday.
        ("week" (floor (+ days 3) 7))
        ("month" (+ (* 12 (decoded-time-year decoded)) (decoded-time-month decoded)))
        (_ (decoded-time-year decoded))))))

(defun financial-chart-pivot-points (bars &optional method period)
  "Pivot points of BARS by METHOD over PERIOD.
METHOD is classic (default), fibonacci, woodie or camarilla.  PERIOD is
\"day\", \"week\", \"month\", \"year\", a bar count, or \"auto\" (the
default: day for intraday bars, month for daily, year beyond; 20 bars
without times).  Every bar of a period gets the levels computed from
the previous period's high, low and last close; the first period has
none.  Outputs pivot-pp, pivot-r1..r3 and pivot-s1..s3."
  (let* ((method (or method "classic"))
         (period (if (member period '(nil "auto")) (financial-chart-pivot--auto-period bars) period))
         (levels nil) (key nil) prev-levels high low close)
    (financial-chart-pivot-levels method 1 0 0.5)
    (unless (or (and (integerp period) (> period 0)) (member period '("day" "week" "month" "year")))
      (financial-chart-overlay--fail "Pivot period %S; give day, week, month, year, auto or a bar count"
                                     period))
    (cl-loop for b in bars for i from 0
             for k = (financial-chart-pivot--key period (plist-get b :time) i)
             do (unless (equal k key)
                  (setq prev-levels (and high (financial-chart-pivot-levels method high low close))
                        key k high nil low nil close nil))
             do (let ((h (plist-get b :high)) (l (plist-get b :low)) (c (plist-get b :close)))
                  (when (and (numberp h) (numberp l) (numberp c))
                    (setq high (if high (max high h) h) low (if low (min low l) l) close c)))
             do (push prev-levels levels))
    (setq levels (nreverse levels))
    (cl-loop for name in '(pp r1 r2 r3 s1 s2 s3) for i from 0
             collect (list :name (intern (format "pivot-%s" name))
                           :label (if (eq name 'pp) "P" (upcase (symbol-name name)))
                           :values (mapcar (lambda (l) (and l (nth i l))) levels)))))

;;; SuperTrend

(defun financial-chart-supertrend (bars &optional period multiplier)
  "SuperTrend of BARS: an ATR trailing stop that flips with the trend.
PERIOD (10) is the Wilder ATR period and MULTIPLIER (3) its width
around (high+low)/2.  The lower band only rises and the upper band only
falls while price stays inside them; a close through the active band
flips the trend.  Outputs supertrend (the active band), supertrend-up
\(the band while rising, else nil) and supertrend-down."
  (let* ((period (financial-chart-overlay--period period 10 "period"))
         (multiplier (financial-chart-overlay--positive multiplier 3 "multiplier"))
         (atr (financial-chart-atr bars period))
         (n (length bars))
         (line (make-vector n nil)) (up (make-vector n nil)) (down (make-vector n nil))
         final-upper final-lower trend prev-close)
    (cl-loop for b in bars for a in atr for i from 0
             for h = (plist-get b :high) for l = (plist-get b :low) for c = (plist-get b :close)
             do (if (not (and a (numberp h) (numberp l) (numberp c) (numberp prev-close)))
                    (setq final-upper nil final-lower nil trend nil)
                  (let* ((mid (/ (+ h l) 2.0))
                         (basic-upper (+ mid (* multiplier a)))
                         (basic-lower (- mid (* multiplier a))))
                    (setq final-upper (if (or (null final-upper) (< basic-upper final-upper)
                                              (> prev-close final-upper))
                                          basic-upper final-upper)
                          final-lower (if (or (null final-lower) (> basic-lower final-lower)
                                              (< prev-close final-lower))
                                          basic-lower final-lower))
                    (setq trend (cond ((null trend) (if (>= c mid) 'up 'down))
                                      ((and (eq trend 'up) (< c final-lower)) 'down)
                                      ((and (eq trend 'down) (> c final-upper)) 'up)
                                      (t trend)))
                    (let ((value (if (eq trend 'up) final-lower final-upper)))
                      (aset line i value)
                      (aset (if (eq trend 'up) up down) i value))))
             do (setq prev-close (and (numberp c) c)))
    (list (list :name 'supertrend :label "SuperTrend" :values (append line nil))
          (list :name 'supertrend-up :label "SuperTrend Up" :values (append up nil))
          (list :name 'supertrend-down :label "SuperTrend Down" :values (append down nil)))))

(financial-chart-register-indicator
 'ichimoku #'financial-chart-ichimoku
 :label "Ichimoku" :unit :price :panel :overlay :scale :linear
 :description "Ichimoku cloud: tenkan, kijun, senkou A/B (shifted ahead) and chikou (shifted back).")
(financial-chart-register-indicator
 'envelopes #'financial-chart-envelopes
 :label "Envelopes" :unit :price :panel :overlay :scale :linear
 :description "SMA envelopes PERCENT above and below.")
(financial-chart-register-indicator
 'vwap-bands #'financial-chart-vwap-bands
 :label "VWAP Bands" :unit :price :panel :overlay :scale :linear
 :description "Cumulative VWAP with volume-weighted standard-deviation bands.")
(financial-chart-register-indicator
 'pivot-points #'financial-chart-pivot-points
 :label "Pivot Points" :unit :price :panel :overlay :scale :linear
 :description "Classic, Fibonacci, Woodie or Camarilla pivots from the previous period.")
(financial-chart-register-indicator
 'supertrend #'financial-chart-supertrend
 :label "SuperTrend" :unit :price :panel :overlay :scale :linear
 :description "ATR trailing stop with up and down legs.")

(provide 'financial-chart-overlay-indicators)
;;; financial-chart-overlay-indicators.el ends here
