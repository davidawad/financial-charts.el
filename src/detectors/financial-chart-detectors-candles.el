;;; financial-chart-detectors-candles.el --- candlestick pattern detectors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One detector per candlestick pattern, pattern.NAME (fc-gty.2).
;;
;; Shapes are Nison's (Nison, S. 1991, "Japanese Candlestick Charting
;; Techniques", New York Institute of Finance), measured the way TA-Lib
;; measures them (TA-Lib 0.6, ta_CDL*.c; Fortier, M. 2001-): "long",
;; "short", "very short" and "near" compare a bar's body or shadow with
;; the average of the 10 (or 5) bars before it, so the same pattern means
;; the same thing on any price scale.  With "trend": 0 each detector
;; equals its TA-Lib function bar for bar on the oracle bars
;; (test/oracles/talib-goldens).
;;
;; Nison's reversal patterns only mean something after the trend they
;; reverse ("the hammer ... must appear after a decline").  TA-Lib leaves
;; that to the caller; here "trend" (default 10 bars) requires it: the
;; close before the pattern's first bar against the close "trend" bars
;; earlier, in units of the average bar range, must fall for a bullish
;; reversal and rise for a bearish one.  So with the default each
;; detector fires on a subset of TA-Lib's bars, the subset after the
;; right trend.  Tweezers have no TA-Lib function; they follow Nison
;; with TA-Lib's "equal" tolerance.
;;
;; Every event is confirmed on its own last bar (lag 0) and depends on
;; no later bar.  Its evidence holds each threshold test as a margin in
;; units of the 10-bar average range (positive = passed by that much),
;; the prior slope when the trend is tested and "strict" for engulfing
;; and harami; its strength is the mean of the margins clamped to
;; [0, 1], times 0.8 for a non-strict engulfing or harami (TA-Lib's 80).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-detector-api)

;;; Bar arithmetic

(defsubst financial-chart-candle--o (v i) (plist-get (aref v i) :open))
(defsubst financial-chart-candle--h (v i) (plist-get (aref v i) :high))
(defsubst financial-chart-candle--l (v i) (plist-get (aref v i) :low))
(defsubst financial-chart-candle--c (v i) (plist-get (aref v i) :close))

(defun financial-chart-candle--body (v i)
  "Bar I's real body."
  (abs (- (financial-chart-candle--c v i) (financial-chart-candle--o v i))))

(defun financial-chart-candle--upper (v i)
  "Bar I's upper shadow."
  (- (financial-chart-candle--h v i) (max (financial-chart-candle--o v i) (financial-chart-candle--c v i))))

(defun financial-chart-candle--lower (v i)
  "Bar I's lower shadow."
  (- (min (financial-chart-candle--o v i) (financial-chart-candle--c v i)) (financial-chart-candle--l v i)))

(defun financial-chart-candle--white (v i)
  "Non-nil when bar I closes at or above its open (TA-Lib's colour)."
  (>= (financial-chart-candle--c v i) (financial-chart-candle--o v i)))

(defun financial-chart-candle--range-of (v i kind)
  "Bar I's range of KIND: body, hl or shadows."
  (pcase kind
    ('body (financial-chart-candle--body v i))
    ('hl (- (financial-chart-candle--h v i) (financial-chart-candle--l v i)))
    ('shadows (+ (financial-chart-candle--upper v i) (financial-chart-candle--lower v i)))))

(defconst financial-chart-candle-settings
  '((body-long body 10 1.0) (body-short body 10 1.0) (body-doji hl 10 0.1)
    (shadow-long body 0 1.0) (shadow-very-short hl 10 0.1)
    (near hl 5 0.2) (far hl 5 0.6) (equal hl 5 0.05))
  "TA-Lib's default candle settings as (NAME KIND PERIOD FACTOR).
A setting compares with FACTOR times the mean KIND range of the PERIOD
bars before the bar (PERIOD 0: the bar's own range).")

(defun financial-chart-candle--avg (v setting i)
  "SETTING's threshold for bar I (see `financial-chart-candle-settings')."
  (pcase-let ((`(,_ ,kind ,period ,factor) (assq setting financial-chart-candle-settings)))
    (/ (* factor (if (zerop period) (financial-chart-candle--range-of v i kind)
                   (/ (cl-loop for j from (- i period) below i
                               sum (financial-chart-candle--range-of v j kind))
                      (float period))))
       (if (eq kind 'shadows) 2.0 1.0))))

(defun financial-chart-candle--scale (v i)
  "The unit of evidence at bar I: the mean high-low range of the 10 bars before it."
  (let ((r (/ (cl-loop for j from (max 0 (- i 10)) below (max 1 i)
                       sum (financial-chart-candle--range-of v j 'hl))
              (float (min 10 (max 1 i))))))
    (if (> r 0) r 1.0)))

;;; Predicates with evidence

(defvar financial-chart-candle--evidence nil
  "Evidence collected by `financial-chart-candle--test' for the current bar.")

(defvar financial-chart-candle--unit 1.0
  "The unit of the current bar's evidence (`financial-chart-candle--scale').")

(defun financial-chart-candle--test (name lhs op rhs)
  "Record whether LHS OP RHS holds as evidence NAME; return that.
OP is one of < <= > >=.  The evidence is the margin (passing positive)
in units of `financial-chart-candle--unit'."
  (let ((margin (/ (if (memq op '(> >=)) (- lhs rhs) (- rhs lhs)) financial-chart-candle--unit)))
    (push (cons name margin) financial-chart-candle--evidence)
    (funcall op lhs rhs)))

(defun financial-chart-candle--prior-slope (v first k)
  "The slope into bar FIRST over K bars in average ranges per bar, or nil.
The close before FIRST against the close K bars before that."
  (let ((a (- first 1 k)) (b (1- first)))
    (when (>= a 0)
      (/ (- (financial-chart-candle--c v b) (financial-chart-candle--c v a))
         (* k (let ((r (/ (cl-loop for j from a to b sum (financial-chart-candle--range-of v j 'hl))
                          (float (1+ k)))))
                (if (> r 0) r 1.0)))))))

;;; Definition

(defun financial-chart-candle--detect (v params bars-back lookback trend shape)
  "Raw events of SHAPE over V.
SHAPE is called with V and I after `financial-chart-candle--test'
state is reset and returns nil, or (DIRECTION STRICT) when it fires.
BARS-BACK is the pattern's bars before I; LOOKBACK the first index it
may fire at (TA-Lib's lookback); TREND `down' or `up' when the pattern
needs that trend before it (window PARAMS :trend, 0 for none)."
  (let ((k (or (plist-get params :trend) 0)) events)
    (cl-loop
     for i from lookback below (length v)
     do (let* ((financial-chart-candle--evidence nil)
               (financial-chart-candle--unit (financial-chart-candle--scale v i))
               (hit (funcall shape v i))
               (slope (and hit trend (> k 0) (financial-chart-candle--prior-slope v (- i bars-back) k))))
          (when (and hit (or (null trend) (zerop k)
                             (and slope (if (eq trend 'down) (< slope 0) (> slope 0)))))
            (let* ((margins (mapcar (lambda (e) (min 1.0 (max 0.0 (cdr e)))) financial-chart-candle--evidence))
                   (margins (if slope (cons (min 1.0 (abs slope)) margins) margins))
                   (strength (* (if (eq (car (last hit)) 'loose) 0.8 1.0)
                                (if margins (/ (apply #'+ margins) (length margins)) 1.0))))
              (push (list :index i :confirmed i :direction (car hit) :strength strength
                          :geometry (list :at i :y (if (eq (car hit) 'bullish) (financial-chart-candle--l v i)
                                                     (financial-chart-candle--h v i)))
                          :evidence (append
                                     (cl-loop for (name . m) in (reverse financial-chart-candle--evidence)
                                              append (list (intern (concat ":" (symbol-name name))) m))
                                     (when slope (list :prior_slope slope :prior_window k))
                                     (when (memq (car (last hit)) '(strict loose))
                                       (list :strict (if (eq (car (last hit)) 'strict) t :false)))))
                    events)))))
    (nreverse events)))

(defconst financial-chart-candle--trend-param
  '(:name trend :type integer :default 10 :min 0
          :doc "bars of prior trend the reversal needs (Nison); 0 tests none (TA-Lib)")
  "The trend parameter of reversal patterns.")

(defmacro financial-chart-define-candle (name &rest spec)
  "Register candlestick detector pattern.NAME from SPEC.
SPEC keys: :label, :direction, :bars-back, :lookback, :trend (down, up
or nil), :oracle, :oracle-sign, :definition, :deviations, :example and
:shape, a lambda of V and I (see `financial-chart-candle--detect')."
  (declare (indent 1))
  (let* ((trend (plist-get spec :trend))
         (id (intern (format "pattern.%s" name))))
    `(financial-chart-register-detector
      ',id
      (lambda (v params)
        (financial-chart-candle--detect v params ,(or (plist-get spec :bars-back) 0)
                                        ,(plist-get spec :lookback) ',trend ,(plist-get spec :shape)))
      :kind 'pattern :direction ',(plist-get spec :direction)
      :params (when ',trend (list financial-chart-candle--trend-param))
      :lag 0 :lookback ,(plist-get spec :lookback)
      :needs '(open high low close)
      :definition ,(plist-get spec :definition)
      :oracle ,(plist-get spec :oracle)
      :oracle-params ',(when trend '(:trend 0))
      :oracle-sign ,(plist-get spec :oracle-sign)
      :deviations ,(plist-get spec :deviations)
      :invariance '(scale shift)
      :variants (when ',trend '(((:trend 0) scale shift) ((:trend 4) scale shift)))
      :mark '(:type "marker" :label ,(plist-get spec :label))
      :example "examples/indicators/patterns.json"
      :doc ,(format "%s candlestick pattern" name))))

;;; One bar

(defconst financial-chart-candle--trend-deviation
  "Nison's prior trend (\"trend\" bars) is required; TA-Lib has no trend test, so with the default this fires on the subset of TA-Lib's bars that follow the trend."
  "The deviation every trend-filtered pattern lists.")

(financial-chart-define-candle hammer
  :label "H" :direction bullish :lookback 11 :trend down
  :oracle "talib:CDLHAMMER" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Hammer (Nison 1991, ch. 4): after a decline, a small real body at the top of the range with a lower shadow at least as long as the body (TA-Lib: body < 10-bar average body; lower shadow > body; upper shadow < 0.1 x 10-bar average range; body near or below the prior bar's low, within 0.2 x 5-bar average range)."
  :shape (lambda (v i)
           (and (financial-chart-candle--test 'body_short (financial-chart-candle--body v i) '< (financial-chart-candle--avg v 'body-short i))
                (financial-chart-candle--test 'lower_shadow_long (financial-chart-candle--lower v i) '> (financial-chart-candle--avg v 'shadow-long i))
                (financial-chart-candle--test 'upper_shadow_very_short (financial-chart-candle--upper v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                (financial-chart-candle--test 'near_prior_low (min (financial-chart-candle--o v i) (financial-chart-candle--c v i)) '<=
                                              (+ (financial-chart-candle--l v (1- i)) (financial-chart-candle--avg v 'near (1- i))))
                '(bullish))))

(financial-chart-define-candle hanging-man
  :label "HM" :direction bearish :lookback 11 :trend up
  :oracle "talib:CDLHANGINGMAN" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Hanging man (Nison 1991, ch. 4): the hammer's shape after an advance (TA-Lib: the hammer's body and shadow tests with the body at or above the prior bar's high less 0.2 x 5-bar average range)."
  :shape (lambda (v i)
           (and (financial-chart-candle--test 'body_short (financial-chart-candle--body v i) '< (financial-chart-candle--avg v 'body-short i))
                (financial-chart-candle--test 'lower_shadow_long (financial-chart-candle--lower v i) '> (financial-chart-candle--avg v 'shadow-long i))
                (financial-chart-candle--test 'upper_shadow_very_short (financial-chart-candle--upper v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                (financial-chart-candle--test 'near_prior_high (min (financial-chart-candle--o v i) (financial-chart-candle--c v i)) '>=
                                              (- (financial-chart-candle--h v (1- i)) (financial-chart-candle--avg v 'near (1- i))))
                '(bearish))))

(defun financial-chart-candle--body-gap (v i up)
  "Non-nil when bar I's body gaps UP (or down) from bar I-1's body."
  (let ((o (financial-chart-candle--o v i)) (c (financial-chart-candle--c v i))
        (po (financial-chart-candle--o v (1- i))) (pc (financial-chart-candle--c v (1- i))))
    (if up (and (> o po) (> o pc) (> c po) (> c pc))
      (and (< o po) (< o pc) (< c po) (< c pc)))))

(financial-chart-define-candle inverted-hammer
  :label "IH" :direction bullish :lookback 11 :trend down
  :oracle "talib:CDLINVERTEDHAMMER" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Inverted hammer (Nison 1991, ch. 5): after a decline, a small body gapping down from the prior body with a long upper shadow and almost no lower shadow (TA-Lib: body < 10-bar average body; upper shadow > body; lower shadow < 0.1 x 10-bar average range)."
  :shape (lambda (v i)
           (and (financial-chart-candle--body-gap v i nil)
                (financial-chart-candle--test 'body_short (financial-chart-candle--body v i) '< (financial-chart-candle--avg v 'body-short i))
                (financial-chart-candle--test 'upper_shadow_long (financial-chart-candle--upper v i) '> (financial-chart-candle--avg v 'shadow-long i))
                (financial-chart-candle--test 'lower_shadow_very_short (financial-chart-candle--lower v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                '(bullish))))

(financial-chart-define-candle shooting-star
  :label "SS" :direction bearish :lookback 11 :trend up
  :oracle "talib:CDLSHOOTINGSTAR" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Shooting star (Nison 1991, ch. 5): after an advance, a small body gapping up from the prior body with a long upper shadow and almost no lower shadow (TA-Lib: as the inverted hammer, gapping up)."
  :shape (lambda (v i)
           (and (financial-chart-candle--body-gap v i t)
                (financial-chart-candle--test 'body_short (financial-chart-candle--body v i) '< (financial-chart-candle--avg v 'body-short i))
                (financial-chart-candle--test 'upper_shadow_long (financial-chart-candle--upper v i) '> (financial-chart-candle--avg v 'shadow-long i))
                (financial-chart-candle--test 'lower_shadow_very_short (financial-chart-candle--lower v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                '(bearish))))

(defun financial-chart-candle--doji-p (v i)
  "TA-Lib's doji test on bar I, recorded as evidence."
  (financial-chart-candle--test 'body_doji (financial-chart-candle--body v i) '<= (financial-chart-candle--avg v 'body-doji i)))

(financial-chart-define-candle doji
  :label "D" :direction neutral :lookback 10
  :oracle "talib:CDLDOJI"
  :definition "Doji (Nison 1991, ch. 8): open and close (nearly) equal, a session of indecision (TA-Lib: body <= 0.1 x 10-bar average range)."
  :shape (lambda (v i) (and (financial-chart-candle--doji-p v i) '(neutral))))

(financial-chart-define-candle dragonfly-doji
  :label "DD" :direction bullish :lookback 10
  :oracle "talib:CDLDRAGONFLYDOJI"
  :definition "Dragonfly doji (Nison 1991, ch. 8): a doji at the top of the range with a long lower shadow (TA-Lib: doji; upper shadow < and lower shadow > 0.1 x 10-bar average range).  Bullish as Nison reads it; TA-Lib reports +100 without a direction."
  :shape (lambda (v i)
           (and (financial-chart-candle--doji-p v i)
                (financial-chart-candle--test 'upper_shadow_very_short (financial-chart-candle--upper v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                (financial-chart-candle--test 'lower_shadow (financial-chart-candle--lower v i) '> (financial-chart-candle--avg v 'shadow-very-short i))
                '(bullish))))

(financial-chart-define-candle gravestone-doji
  :label "GD" :direction bearish :lookback 10
  :oracle "talib:CDLGRAVESTONEDOJI"
  :definition "Gravestone doji (Nison 1991, ch. 8): a doji at the bottom of the range with a long upper shadow (TA-Lib: doji; lower shadow < and upper shadow > 0.1 x 10-bar average range).  Bearish as Nison reads it; TA-Lib reports +100 without a direction."
  :shape (lambda (v i)
           (and (financial-chart-candle--doji-p v i)
                (financial-chart-candle--test 'lower_shadow_very_short (financial-chart-candle--lower v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                (financial-chart-candle--test 'upper_shadow (financial-chart-candle--upper v i) '> (financial-chart-candle--avg v 'shadow-very-short i))
                '(bearish))))

(defun financial-chart-candle--marubozu (v i white)
  "TA-Lib's marubozu test on bar I of colour WHITE."
  (and (eq (and (financial-chart-candle--white v i) t) white)
       (financial-chart-candle--test 'body_long (financial-chart-candle--body v i) '> (financial-chart-candle--avg v 'body-long i))
       (financial-chart-candle--test 'upper_shadow_very_short (financial-chart-candle--upper v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
       (financial-chart-candle--test 'lower_shadow_very_short (financial-chart-candle--lower v i) '< (financial-chart-candle--avg v 'shadow-very-short i))))

(financial-chart-define-candle bullish-marubozu
  :label "BuM" :direction bullish :lookback 10
  :oracle "talib:CDLMARUBOZU" :oracle-sign t
  :definition "White marubozu (Nison 1991, ch. 9): a long white body with no shadows (TA-Lib: body > 10-bar average body; both shadows < 0.1 x 10-bar average range)."
  :shape (lambda (v i) (and (financial-chart-candle--marubozu v i t) '(bullish))))

(financial-chart-define-candle bearish-marubozu
  :label "BeM" :direction bearish :lookback 10
  :oracle "talib:CDLMARUBOZU" :oracle-sign t
  :definition "Black marubozu (Nison 1991, ch. 9): a long black body with no shadows (TA-Lib: as the white marubozu, closing below the open)."
  :shape (lambda (v i) (and (financial-chart-candle--marubozu v i nil) '(bearish))))

;;; Two bars

(financial-chart-define-candle bullish-engulfing
  :label "BuE" :direction bullish :lookback 2 :bars-back 1 :trend down
  :oracle "talib:CDLENGULFING" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Bullish engulfing (Nison 1991, ch. 4): in a downtrend a white body that wraps the prior black body (TA-Lib: one end may be equal, which TA-Lib scores 80 and this marks not strict)."
  :shape (lambda (v i)
           (let ((o (financial-chart-candle--o v i)) (c (financial-chart-candle--c v i))
                 (po (financial-chart-candle--o v (1- i))) (pc (financial-chart-candle--c v (1- i))))
             (and (financial-chart-candle--white v i) (not (financial-chart-candle--white v (1- i)))
                  (or (and (>= c po) (< o pc)) (and (> c po) (<= o pc)))
                  (financial-chart-candle--test 'engulf_top c '>= po)
                  (financial-chart-candle--test 'engulf_bottom o '<= pc)
                  (list 'bullish (if (and (/= o pc) (/= c po)) 'strict 'loose))))))

(financial-chart-define-candle bearish-engulfing
  :label "BeE" :direction bearish :lookback 2 :bars-back 1 :trend up
  :oracle "talib:CDLENGULFING" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Bearish engulfing (Nison 1991, ch. 4): in an uptrend a black body that wraps the prior white body (TA-Lib: one end may be equal, scored 80, not strict)."
  :shape (lambda (v i)
           (let ((o (financial-chart-candle--o v i)) (c (financial-chart-candle--c v i))
                 (po (financial-chart-candle--o v (1- i))) (pc (financial-chart-candle--c v (1- i))))
             (and (not (financial-chart-candle--white v i)) (financial-chart-candle--white v (1- i))
                  (or (and (>= o pc) (< c po)) (and (> o pc) (<= c po)))
                  (financial-chart-candle--test 'engulf_top o '>= pc)
                  (financial-chart-candle--test 'engulf_bottom c '<= po)
                  (list 'bearish (if (and (/= o pc) (/= c po)) 'strict 'loose))))))

(defun financial-chart-candle--harami (v i white-first)
  "TA-Lib's harami on bar I whose first bar's colour is WHITE-FIRST."
  (let* ((top (max (financial-chart-candle--o v i) (financial-chart-candle--c v i)))
         (bot (min (financial-chart-candle--o v i) (financial-chart-candle--c v i)))
         (ptop (max (financial-chart-candle--o v (1- i)) (financial-chart-candle--c v (1- i))))
         (pbot (min (financial-chart-candle--o v (1- i)) (financial-chart-candle--c v (1- i)))))
    (and (eq (and (financial-chart-candle--white v (1- i)) t) white-first)
         (financial-chart-candle--test 'first_body_long (financial-chart-candle--body v (1- i)) '>
                                       (financial-chart-candle--avg v 'body-long (1- i)))
         (financial-chart-candle--test 'second_body_short (financial-chart-candle--body v i) '<=
                                       (financial-chart-candle--avg v 'body-short i))
         (financial-chart-candle--test 'inside_top top '<= ptop)
         (financial-chart-candle--test 'inside_bottom bot '>= pbot)
         (list (if white-first 'bearish 'bullish)
               (if (and (< top ptop) (> bot pbot)) 'strict 'loose)))))

(financial-chart-define-candle bullish-harami
  :label "BuH" :direction bullish :lookback 11 :bars-back 1 :trend down
  :oracle "talib:CDLHARAMI" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Bullish harami (Nison 1991, ch. 6): in a downtrend a long black body followed by a small body inside it (TA-Lib: first body > 10-bar average body, second <= 10-bar average body, second body within the first; an equal end scores 80, not strict)."
  :shape (lambda (v i) (financial-chart-candle--harami v i nil)))

(financial-chart-define-candle bearish-harami
  :label "BeH" :direction bearish :lookback 11 :bars-back 1 :trend up
  :oracle "talib:CDLHARAMI" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Bearish harami (Nison 1991, ch. 6): in an uptrend a long white body followed by a small body inside it (TA-Lib's harami with a white first bar)."
  :shape (lambda (v i) (financial-chart-candle--harami v i t)))

(financial-chart-define-candle piercing-line
  :label "PL" :direction bullish :lookback 11 :bars-back 1 :trend down
  :oracle "talib:CDLPIERCING" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Piercing line (Nison 1991, ch. 4): in a downtrend a long black bar, then a long white bar opening below its low and closing above the middle of its body (TA-Lib: both bodies > 10-bar average body; close below the first open)."
  :shape (lambda (v i)
           (let ((p (1- i)))
             (and (not (financial-chart-candle--white v p)) (financial-chart-candle--white v i)
                  (financial-chart-candle--test 'gap_below_low (financial-chart-candle--o v i) '< (financial-chart-candle--l v p))
                  (financial-chart-candle--test 'first_body_long (financial-chart-candle--body v p) '> (financial-chart-candle--avg v 'body-long p))
                  (financial-chart-candle--test 'second_body_long (financial-chart-candle--body v i) '> (financial-chart-candle--avg v 'body-long i))
                  (financial-chart-candle--test 'below_first_open (financial-chart-candle--c v i) '< (financial-chart-candle--o v p))
                  (financial-chart-candle--test 'penetration (financial-chart-candle--c v i) '>
                                                (+ (financial-chart-candle--c v p) (* 0.5 (financial-chart-candle--body v p))))
                  '(bullish)))))

(financial-chart-define-candle dark-cloud-cover
  :label "DCC" :direction bearish :lookback 11 :bars-back 1 :trend up
  :oracle "talib:CDLDARKCLOUDCOVER" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Dark cloud cover (Nison 1991, ch. 4): in an uptrend a long white bar, then a black bar opening above its high and closing below the middle of its body (TA-Lib: first body > 10-bar average body, penetration 0.5, close above the first open)."
  :shape (lambda (v i)
           (let ((p (1- i)))
             (and (financial-chart-candle--white v p) (not (financial-chart-candle--white v i))
                  (financial-chart-candle--test 'gap_above_high (financial-chart-candle--o v i) '> (financial-chart-candle--h v p))
                  (financial-chart-candle--test 'first_body_long (financial-chart-candle--body v p) '> (financial-chart-candle--avg v 'body-long p))
                  (financial-chart-candle--test 'above_first_open (financial-chart-candle--c v i) '> (financial-chart-candle--o v p))
                  (financial-chart-candle--test 'penetration (financial-chart-candle--c v i) '<
                                                (- (financial-chart-candle--c v p) (* 0.5 (financial-chart-candle--body v p))))
                  '(bearish)))))

(financial-chart-register-detector
 'pattern.tweezer-bottom
 (lambda (v params)
   (financial-chart-candle--detect
    v params 1 6 'down
    (lambda (v i)
      (and (not (financial-chart-candle--white v (1- i))) (financial-chart-candle--white v i)
           (financial-chart-candle--test 'equal_lows (abs (- (financial-chart-candle--l v i) (financial-chart-candle--l v (1- i))))
                                         '<= (financial-chart-candle--avg v 'equal i))
           '(bullish)))))
 :kind 'pattern :direction 'bullish :params (list financial-chart-candle--trend-param)
 :lag 0 :lookback 6 :needs '(open high low close)
 :definition "Tweezer bottom (Nison 1991, ch. 7): in a downtrend a black bar and a white bar with matching lows.  No TA-Lib function; the lows match within TA-Lib's \"equal\" tolerance (0.05 x 5-bar average range)."
 :oracle nil :invariance '(scale shift)
 :variants '(((:trend 0) scale shift))
 :mark '(:type "marker" :label "TB") :example "examples/indicators/patterns.json"
 :doc "tweezer-bottom candlestick pattern")

(financial-chart-register-detector
 'pattern.tweezer-top
 (lambda (v params)
   (financial-chart-candle--detect
    v params 1 6 'up
    (lambda (v i)
      (and (financial-chart-candle--white v (1- i)) (not (financial-chart-candle--white v i))
           (financial-chart-candle--test 'equal_highs (abs (- (financial-chart-candle--h v i) (financial-chart-candle--h v (1- i))))
                                         '<= (financial-chart-candle--avg v 'equal i))
           '(bearish)))))
 :kind 'pattern :direction 'bearish :params (list financial-chart-candle--trend-param)
 :lag 0 :lookback 6 :needs '(open high low close)
 :definition "Tweezer top (Nison 1991, ch. 7): in an uptrend a white bar and a black bar with matching highs, within TA-Lib's \"equal\" tolerance.  No TA-Lib function."
 :oracle nil :invariance '(scale shift)
 :variants '(((:trend 0) scale shift))
 :mark '(:type "marker" :label "TT") :example "examples/indicators/patterns.json"
 :doc "tweezer-top candlestick pattern")

;;; Three and five bars

(defun financial-chart-candle--star (v i morning)
  "TA-Lib's morning (MORNING non-nil) or evening star ending at bar I."
  (let ((a (- i 2)) (b (1- i)))
    (and (if morning (and (not (financial-chart-candle--white v a)) (financial-chart-candle--white v i))
           (and (financial-chart-candle--white v a) (not (financial-chart-candle--white v i))))
         (let ((ob (financial-chart-candle--o v b)) (cb (financial-chart-candle--c v b))
               (oa (financial-chart-candle--o v a)) (ca (financial-chart-candle--c v a)))
           (if morning (and (< ob oa) (< ob ca) (< cb oa) (< cb ca))
             (and (> ob oa) (> ob ca) (> cb oa) (> cb ca))))
         (financial-chart-candle--test 'first_body_long (financial-chart-candle--body v a) '> (financial-chart-candle--avg v 'body-long a))
         (financial-chart-candle--test 'star_body_short (financial-chart-candle--body v b) '<= (financial-chart-candle--avg v 'body-short b))
         (financial-chart-candle--test 'third_body (financial-chart-candle--body v i) '> (financial-chart-candle--avg v 'body-short i))
         (if morning
             (financial-chart-candle--test 'penetration (financial-chart-candle--c v i) '>
                                           (+ (financial-chart-candle--c v a) (* 0.3 (financial-chart-candle--body v a))))
           (financial-chart-candle--test 'penetration (financial-chart-candle--c v i) '<
                                         (- (financial-chart-candle--c v a) (* 0.3 (financial-chart-candle--body v a)))))
         (list (if morning 'bullish 'bearish)))))

(financial-chart-define-candle morning-star
  :label "MS" :direction bullish :lookback 12 :bars-back 2 :trend down
  :oracle "talib:CDLMORNINGSTAR" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Morning star (Nison 1991, ch. 5): in a downtrend a long black body, a small body gapping below it, then a white body closing well into the first (TA-Lib: penetration 0.3 of the first body)."
  :shape (lambda (v i) (financial-chart-candle--star v i t)))

(financial-chart-define-candle evening-star
  :label "ES" :direction bearish :lookback 12 :bars-back 2 :trend up
  :oracle "talib:CDLEVENINGSTAR" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Evening star (Nison 1991, ch. 5): in an uptrend a long white body, a small body gapping above it, then a black body closing well into the first (TA-Lib: penetration 0.3)."
  :shape (lambda (v i) (financial-chart-candle--star v i nil)))

(financial-chart-define-candle three-white-soldiers
  :label "3WS" :direction bullish :lookback 12 :bars-back 2 :trend down
  :oracle "talib:CDL3WHITESOLDIERS" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Three white soldiers (Nison 1991, ch. 7): after a decline three white bars, each closing higher and opening within the prior body, near their highs (TA-Lib: upper shadows < 0.1 x 10-bar average range; opens within 0.2 x 5-bar average range of the prior close; no body shrinking by more than 0.6 x 5-bar average range; last body > 10-bar average body)."
  :shape (lambda (v i)
           (let ((a (- i 2)) (b (1- i)))
             (and (financial-chart-candle--white v a) (financial-chart-candle--white v b) (financial-chart-candle--white v i)
                  (financial-chart-candle--test 'upper_1 (financial-chart-candle--upper v a) '< (financial-chart-candle--avg v 'shadow-very-short a))
                  (financial-chart-candle--test 'upper_2 (financial-chart-candle--upper v b) '< (financial-chart-candle--avg v 'shadow-very-short b))
                  (financial-chart-candle--test 'upper_3 (financial-chart-candle--upper v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                  (> (financial-chart-candle--c v i) (financial-chart-candle--c v b)) (> (financial-chart-candle--c v b) (financial-chart-candle--c v a))
                  (> (financial-chart-candle--o v b) (financial-chart-candle--o v a))
                  (financial-chart-candle--test 'open_2_near (financial-chart-candle--o v b) '<=
                                                (+ (financial-chart-candle--c v a) (financial-chart-candle--avg v 'near a)))
                  (> (financial-chart-candle--o v i) (financial-chart-candle--o v b))
                  (financial-chart-candle--test 'open_3_near (financial-chart-candle--o v i) '<=
                                                (+ (financial-chart-candle--c v b) (financial-chart-candle--avg v 'near b)))
                  (financial-chart-candle--test 'body_2_not_far (financial-chart-candle--body v b) '>
                                                (- (financial-chart-candle--body v a) (financial-chart-candle--avg v 'far a)))
                  (financial-chart-candle--test 'body_3_not_far (financial-chart-candle--body v i) '>
                                                (- (financial-chart-candle--body v b) (financial-chart-candle--avg v 'far b)))
                  (financial-chart-candle--test 'body_3_not_short (financial-chart-candle--body v i) '> (financial-chart-candle--avg v 'body-short i))
                  '(bullish)))))

(financial-chart-define-candle three-black-crows
  :label "3BC" :direction bearish :lookback 13 :bars-back 2 :trend up
  :oracle "talib:CDL3BLACKCROWS" :oracle-sign t
  :deviations (list financial-chart-candle--trend-deviation)
  :definition "Three black crows (Nison 1991, ch. 7): after an advance three black bars, each closing lower and opening within the prior body, near their lows (TA-Lib: a white bar before them whose high is above the first crow's close; lower shadows < 0.1 x 10-bar average range)."
  :shape (lambda (v i)
           (let ((z (- i 3)) (a (- i 2)) (b (1- i)))
             (and (financial-chart-candle--white v z)
                  (not (financial-chart-candle--white v a)) (not (financial-chart-candle--white v b))
                  (not (financial-chart-candle--white v i))
                  (financial-chart-candle--test 'lower_1 (financial-chart-candle--lower v a) '< (financial-chart-candle--avg v 'shadow-very-short a))
                  (financial-chart-candle--test 'lower_2 (financial-chart-candle--lower v b) '< (financial-chart-candle--avg v 'shadow-very-short b))
                  (financial-chart-candle--test 'lower_3 (financial-chart-candle--lower v i) '< (financial-chart-candle--avg v 'shadow-very-short i))
                  (< (financial-chart-candle--o v b) (financial-chart-candle--o v a)) (> (financial-chart-candle--o v b) (financial-chart-candle--c v a))
                  (< (financial-chart-candle--o v i) (financial-chart-candle--o v b)) (> (financial-chart-candle--o v i) (financial-chart-candle--c v b))
                  (> (financial-chart-candle--h v z) (financial-chart-candle--c v a))
                  (> (financial-chart-candle--c v a) (financial-chart-candle--c v b)) (> (financial-chart-candle--c v b) (financial-chart-candle--c v i))
                  '(bearish)))))

(defun financial-chart-candle--three-methods (v i rising)
  "TA-Lib's rising (RISING non-nil) or falling three methods ending at bar I."
  (let* ((a (- i 4)) (mid (list (- i 3) (- i 2) (1- i)))
         (w (lambda (k) (and (financial-chart-candle--white v k) t)))
         (s (if rising 1 -1)))
    (and (eq (funcall w a) rising) (eq (funcall w i) rising)
         (cl-every (lambda (k) (eq (funcall w k) (not rising))) mid)
         (financial-chart-candle--test 'first_body_long (financial-chart-candle--body v a) '> (financial-chart-candle--avg v 'body-long a))
         (cl-every (lambda (k)
                     (and (financial-chart-candle--test (intern (format "body_%d_short" (- k a)))
                                                        (financial-chart-candle--body v k) '< (financial-chart-candle--avg v 'body-short k))
                          (or (< (financial-chart-candle--o v k) (financial-chart-candle--h v a))
                              (< (financial-chart-candle--c v k) (financial-chart-candle--h v a)))
                          (or (> (financial-chart-candle--o v k) (financial-chart-candle--l v a))
                              (> (financial-chart-candle--c v k) (financial-chart-candle--l v a)))))
                   mid)
         (< (* s (financial-chart-candle--c v (- i 2))) (* s (financial-chart-candle--c v (- i 3))))
         (< (* s (financial-chart-candle--c v (1- i))) (* s (financial-chart-candle--c v (- i 2))))
         (> (* s (financial-chart-candle--o v i)) (* s (financial-chart-candle--c v (1- i))))
         (financial-chart-candle--test 'beyond_first_close (* s (financial-chart-candle--c v i)) '> (* s (financial-chart-candle--c v a)))
         (financial-chart-candle--test 'last_body_long (financial-chart-candle--body v i) '> (financial-chart-candle--avg v 'body-long i))
         (list (if rising 'bullish 'bearish)))))

(financial-chart-define-candle rising-three-methods
  :label "R3M" :direction bullish :lookback 14 :bars-back 4 :trend up
  :oracle "talib:CDLRISEFALL3METHODS" :oracle-sign t
  :deviations (list "Nison's continuation needs the trend it continues: with the default the rise before the pattern is required, a subset of TA-Lib's bars.")
  :definition "Rising three methods (Nison 1991, ch. 7): in an uptrend a long white bar, three small falling bars within its range, then a long white bar closing above the first (TA-Lib: long = > 10-bar average body, small = < 10-bar average body)."
  :shape (lambda (v i) (financial-chart-candle--three-methods v i t)))

(financial-chart-define-candle falling-three-methods
  :label "F3M" :direction bearish :lookback 14 :bars-back 4 :trend down
  :oracle "talib:CDLRISEFALL3METHODS" :oracle-sign t
  :deviations (list "Nison's continuation needs the trend it continues: with the default the fall before the pattern is required, a subset of TA-Lib's bars.")
  :definition "Falling three methods (Nison 1991, ch. 7): in a downtrend a long black bar, three small rising bars within its range, then a long black bar closing below the first."
  :shape (lambda (v i) (financial-chart-candle--three-methods v i nil)))

(provide 'financial-chart-detectors-candles)
;;; financial-chart-detectors-candles.el ends here
