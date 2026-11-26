;;; financial-chart-patterns.el --- Swings, support/resistance and candlestick patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure detectors over bar/v1 bars (plists with :open :high :low
;; :close, oldest first), with no drawing (fc-gty).  The composed
;; chart's markup annotations (`financial-chart-eas-markups') draw
;; what these find:
;;
;;   `financial-chart-swing-points'       swing highs and lows (pivots)
;;   `financial-chart-zigzag'             alternating pivots at a % move
;;   `financial-chart-support-resistance' swing pivots clustered into
;;                                        levels, ranked by touches
;;   `financial-chart-candle-patterns'    candlestick patterns
;;   `financial-chart-anchored-vwap'      VWAP from an anchor bar
;;
;; Each signals `financial-chart-invalid-indicator' on bad arguments.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-core)
(require 'financial-chart-indicator-api)

(defun financial-chart-patterns--fail (format-string &rest args)
  "Signal `financial-chart-invalid-indicator' with FORMAT-STRING and ARGS."
  (signal 'financial-chart-invalid-indicator (list (apply #'format format-string args))))

(defun financial-chart-patterns--check-count (name n &optional minimum)
  "Signal unless N, argument NAME, is an integer of at least MINIMUM (1)."
  (unless (and (integerp n) (>= n (or minimum 1)))
    (financial-chart-patterns--fail "%s must be an integer >= %d, got %S" name (or minimum 1) n)))

;;; Swings

(defun financial-chart-swing-points (bars &optional left right)
  "Swing highs and lows of BARS as (INDEX KIND PRICE), oldest first.
A swing high's high is above the LEFT bars before it (default 3) and
not below the RIGHT bars after it (default LEFT); a swing low mirrors
it on lows.  KIND is `high' or `low'.  The last RIGHT bars cannot be
confirmed and are never swings."
  (let* ((left (or left 3)) (right (or right left))
         (v (vconcat bars)) (n (length v)))
    (financial-chart-patterns--check-count "left" left)
    (financial-chart-patterns--check-count "right" right)
    (cl-flet ((at (i key) (plist-get (aref v i) key)))
      (cl-loop for i from left below (- n right)
               for hi = (at i :high) for lo = (at i :low)
               when (and (cl-loop for j from (- i left) below i always (> hi (at j :high)))
                         (cl-loop for j from (1+ i) to (+ i right) always (>= hi (at j :high))))
               collect (list i 'high hi)
               when (and (cl-loop for j from (- i left) below i always (< lo (at j :low)))
                         (cl-loop for j from (1+ i) to (+ i right) always (<= lo (at j :low))))
               collect (list i 'low lo)))))

(defun financial-chart-zigzag (bars &optional deviation)
  "Zigzag pivots of BARS as (INDEX KIND PRICE), alternating high and low.
A new pivot needs price to reverse DEVIATION percent (default 5) from
the last extreme.  The last pivot is the running extreme, unconfirmed."
  (let* ((deviation (or deviation 5))
         (v (vconcat bars)) (n (length v)) pivots
         (dir nil) (ext-i 0) (ext 0.0))
    (unless (and (numberp deviation) (> deviation 0))
      (financial-chart-patterns--fail "deviation is a percent above 0, got %S" deviation))
    (when (> n 0)
      (let ((d (/ deviation 100.0))
            (hi-i 0) (lo-i 0))
        (cl-flet ((h (i) (plist-get (aref v i) :high)) (l (i) (plist-get (aref v i) :low)))
          (cl-loop for i from 1 below n
                   do (pcase dir
                        ('nil
                         (when (> (h i) (h hi-i)) (setq hi-i i))
                         (when (< (l i) (l lo-i)) (setq lo-i i))
                         (cond ((>= (h hi-i) (* (l lo-i) (+ 1 d)))
                                (if (< lo-i hi-i)
                                    (progn (push (list lo-i 'low (l lo-i)) pivots)
                                           (setq dir 'up ext-i hi-i ext (h hi-i)))
                                  (push (list hi-i 'high (h hi-i)) pivots)
                                  (setq dir 'down ext-i lo-i ext (l lo-i))))))
                        ('up
                         (cond ((> (h i) ext) (setq ext-i i ext (h i)))
                               ((<= (l i) (* ext (- 1 d)))
                                (push (list ext-i 'high ext) pivots)
                                (setq dir 'down ext-i i ext (l i)))))
                        ('down
                         (cond ((< (l i) ext) (setq ext-i i ext (l i)))
                               ((>= (h i) (* ext (+ 1 d)))
                                (push (list ext-i 'low ext) pivots)
                                (setq dir 'up ext-i i ext (h i))))))))
        (when dir (push (list ext-i (if (eq dir 'up) 'high 'low) ext) pivots))))
    (nreverse pivots)))

;;; Support and resistance

(defun financial-chart-patterns--atr (v period)
  "Average true range of bar vector V over its last PERIOD bars."
  (let* ((n (length v)) (start (max 1 (- n period))) (sum 0.0) (k 0))
    (cl-loop for i from start below n
             for b = (aref v i) for pc = (plist-get (aref v (1- i)) :close)
             do (cl-incf sum (max (- (plist-get b :high) (plist-get b :low))
                                  (abs (- (plist-get b :high) pc))
                                  (abs (- (plist-get b :low) pc))))
             do (cl-incf k))
    (if (> k 0) (/ sum k)
      (- (plist-get (aref v 0) :high) (plist-get (aref v 0) :low)))))

(cl-defun financial-chart-support-resistance (bars &key left right tolerance atr
                                                   (max 4) (min-touches 1) window)
  "Support and resistance levels of BARS, strongest first.
Swing highs and lows (`financial-chart-swing-points' with LEFT and
RIGHT, default 3) within TOLERANCE of each other (a fraction of price,
default 0.01) or ATR times the 14-bar average true range join one
level.  A level touched by at least MIN-TOUCHES pivots (default 1) is
kept; the MAX strongest (default 4, nil for all) are returned, ranked
by touches then recency.  WINDOW limits the search to the last WINDOW
bars.  Each level is a plist (:price :touches :kind :first :last):
:price the mean of its pivots, :kind `support' when below the last
close else `resistance', :first and :last the bar indices of its first
and last touch (in BARS)."
  (let* ((v (vconcat bars)) (n (length v))
         (start (if window (max 0 (- n window)) 0))
         (_ (when window (financial-chart-patterns--check-count "window" window 2)))
         (_ (when tolerance (unless (and (numberp tolerance) (> tolerance 0))
                              (financial-chart-patterns--fail "tolerance is a fraction above 0, got %S" tolerance))))
         (_ (when atr (unless (and (numberp atr) (> atr 0))
                        (financial-chart-patterns--fail "atr is a multiple above 0, got %S" atr))))
         (_ (financial-chart-patterns--check-count "min-touches" min-touches))
         (_ (when max (financial-chart-patterns--check-count "max" max)))
         (pivots (mapcar (lambda (p) (cons (+ start (car p)) (cdr p)))
                         (financial-chart-swing-points (append (seq-subseq v start) nil) left right)))
         (last-close (plist-get (aref v (1- n)) :close))
         (atr-band (and atr (* atr (financial-chart-patterns--atr v 14))))
         (width (lambda (price) (or atr-band (* (or tolerance 0.01) price))))
         clusters)
    ;; Greedy clustering in price order: a pivot joins the cluster whose
    ;; running mean it is within the band of.
    (dolist (p (sort (copy-sequence pivots) (lambda (a b) (< (nth 2 a) (nth 2 b)))))
      (let ((c (car clusters)))
        (if (and c (<= (abs (- (nth 2 p) (/ (plist-get c :sum) (length (plist-get c :members)))))
                       (funcall width (nth 2 p))))
            (setcar clusters (list :sum (+ (plist-get c :sum) (nth 2 p))
                                   :members (cons p (plist-get c :members))))
          (push (list :sum (nth 2 p) :members (list p)) clusters))))
    (let* ((levels
            (cl-loop for c in clusters
                     for members = (plist-get c :members)
                     for touches = (length members)
                     for price = (/ (plist-get c :sum) touches)
                     when (>= touches min-touches)
                     collect (list :price price :touches touches
                                   :kind (if (< price last-close) 'support 'resistance)
                                   :first (apply #'min (mapcar #'car members))
                                   :last (apply #'max (mapcar #'car members)))))
           (ranked (sort levels (lambda (a b)
                                  (or (> (plist-get a :touches) (plist-get b :touches))
                                      (and (= (plist-get a :touches) (plist-get b :touches))
                                           (> (plist-get a :last) (plist-get b :last))))))))
      (if (and max (> (length ranked) max)) (seq-take ranked max) ranked))))

;;; Anchored VWAP

(defun financial-chart-anchored-vwap (bars anchor)
  "VWAP of BARS from bar index ANCHOR on: one value per bar, nil before it.
Typical price (high+low+close)/3 weighted by volume; a bar without
volume repeats the last value."
  (financial-chart-patterns--check-count "anchor" anchor 0)
  (let ((pv 0.0) (vol 0.0) last)
    (cl-loop for b in bars for i from 0
             collect (when (>= i anchor)
                       (let ((v (plist-get b :volume)))
                         (when (and (numberp v) (> v 0))
                           (cl-incf pv (* v (/ (+ (plist-get b :high) (plist-get b :low) (plist-get b :close)) 3.0)))
                           (cl-incf vol v)
                           (setq last (/ pv vol)))
                         last)))))

;;; Candlestick patterns

(defconst financial-chart-candle-pattern-names
  '(("hammer" "H" bullish) ("inverted-hammer" "IH" bullish)
    ("hanging-man" "HM" bearish) ("shooting-star" "SS" bearish)
    ("doji" "D" neutral) ("dragonfly-doji" "DD" bullish) ("gravestone-doji" "GD" bearish)
    ("bullish-engulfing" "BuE" bullish) ("bearish-engulfing" "BeE" bearish)
    ("bullish-harami" "BuH" bullish) ("bearish-harami" "BeH" bearish)
    ("piercing-line" "PL" bullish) ("dark-cloud-cover" "DCC" bearish)
    ("morning-star" "MS" bullish) ("evening-star" "ES" bearish)
    ("three-white-soldiers" "3WS" bullish) ("three-black-crows" "3BC" bearish)
    ("rising-three-methods" "R3M" bullish) ("falling-three-methods" "F3M" bearish)
    ("tweezer-bottom" "TB" bullish) ("tweezer-top" "TT" bearish)
    ("bullish-marubozu" "BuM" bullish) ("bearish-marubozu" "BeM" bearish))
  "Detected candlestick patterns as (NAME SHORT-LABEL DIRECTION).")

(defvar financial-chart-candle-trend-bars 5
  "Bars back that decide the trend into a bar (hammer vs hanging man).")

(defun financial-chart-candle-patterns (bars &optional names)
  "Candlestick patterns in BARS as (INDEX NAME DIRECTION), oldest first.
NAME is from `financial-chart-candle-pattern-names', DIRECTION
`bullish', `bearish' or `neutral'; INDEX is the pattern's last bar.
NAMES, a list of names, keeps only those.  A bar may carry several
patterns.  Reversal patterns (all but doji, marubozu, three soldiers
or crows and the three methods) need the trend they reverse: the close
before the pattern's first bar against the close
`financial-chart-candle-trend-bars' earlier, down for a bullish one, up
for a bearish one; with too little history they are not reported."
  (when names
    (dolist (name names)
      (unless (assoc name financial-chart-candle-pattern-names)
        (financial-chart-patterns--fail "Unknown candlestick pattern %S; patterns: %s" name
                                        (mapconcat #'car financial-chart-candle-pattern-names ", ")))))
  (let* ((v (vconcat bars)) (n (length v)) found)
    (cl-labels ((o (i) (plist-get (aref v i) :open))
                (c (i) (plist-get (aref v i) :close))
                (h (i) (plist-get (aref v i) :high))
                (l (i) (plist-get (aref v i) :low))
                (range (i) (- (h i) (l i)))
                (body (i) (abs (- (c i) (o i))))
                (top (i) (max (o i) (c i)))
                (bot (i) (min (o i) (c i)))
                (upper (i) (- (h i) (top i)))
                (lower (i) (- (bot i) (l i)))
                (up-p (i) (> (c i) (o i)))
                (down-p (i) (< (c i) (o i)))
                (long-p (i) (and (> (range i) 0) (>= (body i) (* 0.5 (range i)))))
                (doji-p (i) (and (> (range i) 0) (<= (body i) (* 0.1 (range i)))))
                (trend (i) (let ((k financial-chart-candle-trend-bars))
                             (when (>= (- i 1 k) 0)
                               (cond ((< (c (1- i)) (c (- i 1 k))) 'down)
                                     ((> (c (1- i)) (c (- i 1 k))) 'up)))))
                (add (i name) (push (list i name (nth 2 (assoc name financial-chart-candle-pattern-names)))
                                    found)))
      (dotimes (i n)
        (let ((r (range i)) (tr (trend i)))
          (when (> r 0)
            ;; One bar.
            (cond ((doji-p i)
                   (cond ((and (<= (upper i) (* 0.1 r)) (>= (lower i) (* 0.6 r))) (add i "dragonfly-doji"))
                         ((and (<= (lower i) (* 0.1 r)) (>= (upper i) (* 0.6 r))) (add i "gravestone-doji"))
                         (t (add i "doji"))))
                  ((and (>= (lower i) (* 2 (body i))) (<= (upper i) (* 0.15 r)))
                   (pcase tr ('down (add i "hammer")) ('up (add i "hanging-man"))))
                  ((and (>= (upper i) (* 2 (body i))) (<= (lower i) (* 0.15 r)))
                   (pcase tr ('down (add i "inverted-hammer")) ('up (add i "shooting-star"))))
                  ((>= (body i) (* 0.95 r))
                   (add i (if (up-p i) "bullish-marubozu" "bearish-marubozu"))))
            ;; Two bars.
            (when (>= i 1)
              (let ((p (1- i)))
                (when (and (eq (trend p) 'down) (down-p p) (up-p i) (<= (o i) (c p)) (>= (c i) (o p))
                           (> (body i) (body p)))
                  (add i "bullish-engulfing"))
                (when (and (eq (trend p) 'up) (up-p p) (down-p i) (>= (o i) (c p)) (<= (c i) (o p))
                           (> (body i) (body p)))
                  (add i "bearish-engulfing"))
                (when (and (long-p p) (<= (top i) (top p)) (>= (bot i) (bot p))
                           (< (body i) (* 0.5 (body p))) (> (body i) 0))
                  (cond ((and (eq (trend p) 'down) (down-p p) (up-p i)) (add i "bullish-harami"))
                        ((and (eq (trend p) 'up) (up-p p) (down-p i)) (add i "bearish-harami"))))
                (when (and (eq (trend p) 'down) (down-p p) (long-p p) (up-p i) (< (o i) (c p))
                           (> (c i) (/ (+ (o p) (c p)) 2.0)) (< (c i) (o p)))
                  (add i "piercing-line"))
                (when (and (eq (trend p) 'up) (up-p p) (long-p p) (down-p i) (> (o i) (c p))
                           (< (c i) (/ (+ (o p) (c p)) 2.0)) (> (c i) (o p)))
                  (add i "dark-cloud-cover"))
                (let ((tol (* 0.05 (/ (+ r (range p)) 2.0))))
                  (when (and (eq tr 'down) (down-p p) (up-p i) (<= (abs (- (l i) (l p))) tol))
                    (add i "tweezer-bottom"))
                  (when (and (eq tr 'up) (up-p p) (down-p i) (<= (abs (- (h i) (h p))) tol))
                    (add i "tweezer-top")))))
            ;; Three bars.
            (when (>= i 2)
              (let ((a (- i 2)) (b (1- i)))
                (when (and (eq (trend a) 'down) (down-p a) (long-p a)
                           (<= (body b) (* 0.3 (body a))) (< (top b) (c a))
                           (up-p i) (> (c i) (/ (+ (o a) (c a)) 2.0)))
                  (add i "morning-star"))
                (when (and (eq (trend a) 'up) (up-p a) (long-p a)
                           (<= (body b) (* 0.3 (body a))) (> (bot b) (c a))
                           (down-p i) (< (c i) (/ (+ (o a) (c a)) 2.0)))
                  (add i "evening-star"))
                (when (and (cl-every #'up-p (list a b i)) (cl-every #'long-p (list a b i))
                           (> (c b) (c a)) (> (c i) (c b))
                           (<= (bot a) (o b) (top a)) (<= (bot b) (o i) (top b)))
                  (add i "three-white-soldiers"))
                (when (and (cl-every #'down-p (list a b i)) (cl-every #'long-p (list a b i))
                           (< (c b) (c a)) (< (c i) (c b))
                           (<= (bot a) (o b) (top a)) (<= (bot b) (o i) (top b)))
                  (add i "three-black-crows"))))
            ;; Five bars.
            (when (>= i 4)
              (let* ((a (- i 4)) (mid (list (- i 3) (- i 2) (1- i))))
                (when (and (up-p a) (long-p a) (up-p i) (long-p i) (> (c i) (c a))
                           (cl-every (lambda (k) (and (< (body k) (body a)) (<= (h k) (h a)) (>= (l k) (l a))))
                                     mid)
                           (cl-some #'down-p mid))
                  (add i "rising-three-methods"))
                (when (and (down-p a) (long-p a) (down-p i) (long-p i) (< (c i) (c a))
                           (cl-every (lambda (k) (and (< (body k) (body a)) (<= (h k) (h a)) (>= (l k) (l a))))
                                     mid)
                           (cl-some #'up-p mid))
                  (add i "falling-three-methods"))))))))
    (let ((all (nreverse found)))
      (if names (cl-remove-if-not (lambda (f) (member (nth 1 f) names)) all) all))))

(provide 'financial-chart-patterns)
;;; financial-chart-patterns.el ends here
