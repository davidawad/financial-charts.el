;;; financial-chart-detectors-swings.el --- swing, level, Fibonacci and trend-line detectors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The structural detectors under the drawing tools (fc-gty.1, fc-gty.3):
;;
;;   swing.pivot        swing highs and lows, confirmed RIGHT bars later
;;   swing.zigzag       alternating pivots at a percent (or ATR) reversal
;;   swing.abc          each new three-pivot leg A-B-C of the zigzag
;;   fib.retracement    the high-low swing of a window, each time it changes
;;   fib.extension      extension levels of each zigzag A-B-C
;;   level.auto-sr      swing pivots clustered into support/resistance levels
;;   trend.auto-lines   lines through consecutive swing lows and highs
;;
;; Every one runs forward over the bars and emits an event only once
;; the bars up to its confirmed_index settle it, so cutting the bars
;; at any bar k keeps exactly the events confirmed by k.  Prices enter
;; evidence and strength only as ratios to an average range, so scaling
;; the prices changes no event (and shifting them none, except where a
;; percent threshold is the definition: the zigzag family).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-detector-api)

(defsubst financial-chart-swing--h (v i) (plist-get (aref v i) :high))
(defsubst financial-chart-swing--l (v i) (plist-get (aref v i) :low))
(defsubst financial-chart-swing--c (v i) (plist-get (aref v i) :close))

(defun financial-chart-swing--range (v from to)
  "Mean high-low range of bars FROM..TO of V (clamped), at least a tiny positive."
  (let* ((from (max 0 from)) (to (min (1- (length v)) to))
         (n (max 1 (1+ (- to from))))
         (r (/ (cl-loop for j from from to to sum (- (financial-chart-swing--h v j) (financial-chart-swing--l v j)))
               (float n))))
    (if (> r 0) r 1e-12)))

(defun financial-chart-swing--atr (v i &optional period)
  "Mean true range of the PERIOD (14) bars ending at bar I of V."
  (let* ((period (or period 14)) (from (max 0 (- i period -1))))
    (/ (cl-loop for j from from to i
                for h = (financial-chart-swing--h v j) for l = (financial-chart-swing--l v j)
                sum (if (zerop j) (- h l)
                      (let ((pc (financial-chart-swing--c v (1- j))))
                        (max (- h l) (abs (- h pc)) (abs (- l pc))))))
       (float (1+ (- i from))))))

(defun financial-chart-swing--clamp (x)
  "X clamped to [0, 1]."
  (max 0.0 (min 1.0 (float x))))

;;; swing.pivot

(defun financial-chart-swing--pivots (v left right)
  "Raw swing pivot events of V with LEFT and RIGHT bars."
  (let ((n (length v)) events)
    (cl-loop
     for i from left below (- n right)
     for unit = (financial-chart-swing--range v (- i left) (1- i))
     do (let ((hi (financial-chart-swing--h v i)) (lo (financial-chart-swing--l v i)))
          (when (and (cl-loop for j from (- i left) below i always (> hi (financial-chart-swing--h v j)))
                     (cl-loop for j from (1+ i) to (+ i right) always (>= hi (financial-chart-swing--h v j))))
            (let ((pl (/ (- hi (cl-loop for j from (- i left) below i maximize (financial-chart-swing--h v j))) unit))
                  (pr (/ (- hi (cl-loop for j from (1+ i) to (+ i right) maximize (financial-chart-swing--h v j))) unit)))
              (push (list :index i :confirmed (+ i right) :direction 'bearish
                          :strength (financial-chart-swing--clamp (min pl pr))
                          :geometry (list :at i :y hi)
                          :evidence (list :side "high" :prominence_left pl :prominence_right pr))
                    events)))
          (when (and (cl-loop for j from (- i left) below i always (< lo (financial-chart-swing--l v j)))
                     (cl-loop for j from (1+ i) to (+ i right) always (<= lo (financial-chart-swing--l v j))))
            (let ((pl (/ (- (cl-loop for j from (- i left) below i minimize (financial-chart-swing--l v j)) lo) unit))
                  (pr (/ (- (cl-loop for j from (1+ i) to (+ i right) minimize (financial-chart-swing--l v j)) lo) unit)))
              (push (list :index i :confirmed (+ i right) :direction 'bullish
                          :strength (financial-chart-swing--clamp (min pl pr))
                          :geometry (list :at i :y lo)
                          :evidence (list :side "low" :prominence_left pl :prominence_right pr))
                    events)))))
    (nreverse events)))

(defconst financial-chart-swing--pivot-params
  '((:name left :type integer :default 3 :min 1 :doc "bars before a pivot it must exceed")
    (:name right :type integer :default 3 :min 1 :doc "bars after it that confirm it"))
  "Parameters of the pivot-based detectors.")

(financial-chart-register-detector
 'swing.pivot
 (lambda (v p) (financial-chart-swing--pivots v (plist-get p :left) (plist-get p :right)))
 :kind 'swing :direction 'both :params financial-chart-swing--pivot-params
 :lag 'right :lookback 3 :needs '(high low)
 :definition "Swing high/low (pivot): a high above the LEFT highs before it and not below the RIGHT highs after it, a low mirrored, known RIGHT bars later.  TradingView Pine ta.pivothigh / ta.pivotlow (leftbars, rightbars); the local extrema of Lo, Mamaysky and Wang (2000), \"Foundations of Technical Analysis\", Journal of Finance 55(4).  Strength: the smaller prominence over its neighbours in average ranges."
 :invariance '(scale shift)
 :mark '(:type "marker" :label "P")
 :example "examples/indicators/auto-trendlines.json"
 :doc "swing highs and lows")

;;; swing.zigzag

(defun financial-chart-swing--zigzag (v deviation atr)
  "Confirmed zigzag pivots of V as raw events, oldest first.
A reversal of DEVIATION percent from the running extreme (or ATR times
the 14-bar average true range when ATR is set) confirms it."
  (let* ((n (length v)) (d (/ deviation 100.0)) dir (ext-i 0) (ext 0.0) (hi-i 0) (lo-i 0)
         (last-price nil) events)
    (cl-labels ((h (i) (financial-chart-swing--h v i))
                (l (i) (financial-chart-swing--l v i))
                (up-by (base i) (if atr (+ base (* atr (financial-chart-swing--atr v i))) (* base (+ 1 d))))
                (down-by (base i) (if atr (- base (* atr (financial-chart-swing--atr v i))) (* base (- 1 d))))
                (threshold (base i) (if atr (* atr (financial-chart-swing--atr v i)) (* base d)))
                (emit (i kind price c)
                  (let ((leg (if last-price (/ (abs (- price last-price)) (threshold price c)) 1.0)))
                    (push (list :index i :confirmed c :direction (if (eq kind 'high) 'bearish 'bullish)
                                :strength (financial-chart-swing--clamp (/ leg 3))
                                :geometry (list :at i :y price)
                                :evidence (list :side (symbol-name kind) :leg leg))
                          events)
                    (setq last-price price))))
      (cl-loop
       for i from 1 below n
       do (pcase dir
            ('nil
             (when (> (h i) (h hi-i)) (setq hi-i i))
             (when (< (l i) (l lo-i)) (setq lo-i i))
             (cond ((and (< lo-i hi-i) (>= (h hi-i) (up-by (l lo-i) i)))
                    (emit lo-i 'low (l lo-i) i)
                    (setq dir 'up ext-i hi-i ext (h hi-i)))
                   ((and (< hi-i lo-i) (<= (l lo-i) (down-by (h hi-i) i)))
                    (emit hi-i 'high (h hi-i) i)
                    (setq dir 'down ext-i lo-i ext (l lo-i)))))
            ('up
             (cond ((> (h i) ext) (setq ext-i i ext (h i)))
                   ((<= (l i) (down-by ext i))
                    (emit ext-i 'high ext i)
                    (setq dir 'down ext-i i ext (l i)))))
            ('down
             (cond ((< (l i) ext) (setq ext-i i ext (l i)))
                   ((>= (h i) (up-by ext i))
                    (emit ext-i 'low ext i)
                    (setq dir 'up ext-i i ext (h i))))))))
    (nreverse events)))

(defconst financial-chart-swing--zigzag-params
  '((:name deviation :type number :default 5 :min 0.01 :doc "percent reversal that confirms a pivot")
    (:name atr :type number :default nil :min 0.01 :doc "if set, the reversal in multiples of the 14-bar ATR instead"))
  "Parameters of the zigzag-based detectors.")

(defun financial-chart-swing--zigzag-of (v p)
  "Zigzag of V with parameters P."
  (financial-chart-swing--zigzag v (plist-get p :deviation) (plist-get p :atr)))

(financial-chart-register-detector
 'swing.zigzag #'financial-chart-swing--zigzag-of
 :kind 'swing :direction 'both :params financial-chart-swing--zigzag-params
 :lag 'variable :lookback 1 :needs '(high low close)
 :definition "Zig Zag (TradingView built-in Zig Zag; Merrill, A. 1977, \"Filtered Waves\"): pivots alternate high and low, each confirmed on the bar price reverses DEVIATION percent from it (or ATR times the 14-bar average true range).  The last, still running extreme is not an event.  Percent thresholds are scale- but not shift-invariant.  Strength: the leg to it in thresholds, over 3."
 :invariance '(scale)
 :mark '(:type "line")
 :example "examples/indicators/zigzag.json"
 :doc "alternating confirmed pivots")

;;; swing.abc and fib.extension

(defun financial-chart-swing--abc (v p &optional ratios)
  "Each three consecutive zigzag pivots A, B, C of V (params P) as raw events.
With RATIOS, add extension levels C + r(B - A)."
  (let ((pivots (financial-chart-swing--zigzag-of v p)))
    (cl-loop
     for (a b c) on pivots while c
     collect
     (let* ((pa (cons (plist-get a :index) (plist-get (plist-get a :geometry) :y)))
            (pb (cons (plist-get b :index) (plist-get (plist-get b :geometry) :y)))
            (pc (cons (plist-get c :index) (plist-get (plist-get c :geometry) :y)))
            (move (- (cdr pb) (cdr pa)))
            (retrace (if (zerop move) 0.0 (/ (- (cdr pb) (cdr pc)) move))))
       (list :index (car pc) :confirmed (plist-get c :confirmed)
             :direction (if (> move 0) 'bullish 'bearish)
             :strength (financial-chart-swing--clamp (- 1 (/ (abs (- retrace 0.618)) 0.618)))
             :geometry (if ratios
                           (list :levels (mapcar (lambda (r) (cons r (+ (cdr pc) (* r move)))) ratios)
                                 :from pa :to pb :anchor pc)
                         (list :points (list pa pb pc)))
             :evidence (list :retrace retrace))))))

(financial-chart-register-detector
 'swing.abc (lambda (v p) (financial-chart-swing--abc v p))
 :kind 'swing :direction 'both :params financial-chart-swing--zigzag-params
 :lag 'variable :lookback 1 :needs '(high low close)
 :definition "A-B-C leg: three consecutive confirmed Zig Zag pivots (TradingView Zig Zag), the input of Andrews' pitchfork (Mikula, P. 2000, \"The Best Trendline Methods of Alan Andrews\"), the measured move (Bulkowski, T. 2005, \"Encyclopedia of Chart Patterns\", 2nd ed.) and the Fibonacci extension.  Confirmed with C.  Strength: closeness of the B-C retracement of A-B to 0.618."
 :invariance '(scale)
 :mark '(:type "line")
 :example "examples/indicators/pitchfork.json"
 :doc "three-pivot legs of the zigzag")

(defconst financial-chart-fibonacci-extension-ratios [0 0.618 1 1.272 1.618 2 2.618]
  "Default Fibonacci extension ratios.")

(financial-chart-register-detector
 'fib.extension
 (lambda (v p) (financial-chart-swing--abc v p (append (plist-get p :levels) nil)))
 :kind 'fib :direction 'both
 :params (append financial-chart-swing--zigzag-params
                 '((:name levels :type numbers :default [0 0.618 1 1.272 1.618 2 2.618]
                          :doc "extension ratios of the A-B move, projected from C")))
 :lag 'variable :lookback 1 :needs '(high low close)
 :definition "Fibonacci extension (TradingView \"Trend-Based Fib Extension\" drawing tool): levels C + r x (B - A) for the last confirmed Zig Zag pivots A, B, C.  Confirmed with C."
 :invariance '(scale)
 :mark '(:type "levels")
 :example "examples/indicators/fib-extension.json"
 :doc "extension levels of each A-B-C leg")

;;; fib.retracement

(defconst financial-chart-fibonacci-ratios [0 0.236 0.382 0.5 0.618 0.786 1]
  "Default Fibonacci retracement ratios.")

(defun financial-chart-swing--retracements (v window ratios)
  "A raw event each time the high-low swing of the last WINDOW bars of V changes.
WINDOW nil means every bar so far.  The swing runs from whichever of
the highest high and lowest low (first occurrence) comes first to the
other; levels are TO - r(TO - FROM) for RATIOS."
  (let ((n (length v)) (hi 0) (lo 0) prev events)
    (cl-loop
     for i from 0 below n
     for start = (if window (max 0 (- i window -1)) 0)
     do (progn
          ;; Keep the first-occurring extremes of bars START..I.
          (if (< hi start)
              (setq hi (cl-loop with best = start for j from start to i
                                when (> (financial-chart-swing--h v j) (financial-chart-swing--h v best)) do (setq best j)
                                finally return best))
            (when (> (financial-chart-swing--h v i) (financial-chart-swing--h v hi)) (setq hi i)))
          (if (< lo start)
              (setq lo (cl-loop with best = start for j from start to i
                                when (< (financial-chart-swing--l v j) (financial-chart-swing--l v best)) do (setq best j)
                                finally return best))
            (when (< (financial-chart-swing--l v i) (financial-chart-swing--l v lo)) (setq lo i)))
          (unless (equal prev (cons hi lo))
            (setq prev (cons hi lo))
            (let* ((high (cons hi (financial-chart-swing--h v hi))) (low (cons lo (financial-chart-swing--l v lo)))
                   (from (if (<= lo hi) low high)) (to (if (<= lo hi) high low))
                   (swing (/ (- (cdr high) (cdr low)) (financial-chart-swing--range v start i))))
              (push (list :index i :confirmed i :direction (if (<= lo hi) 'bullish 'bearish)
                          :strength (financial-chart-swing--clamp (/ swing 10))
                          :geometry (list :levels (mapcar (lambda (r) (cons r (- (cdr to) (* r (- (cdr to) (cdr from))))))
                                                          ratios)
                                          :from from :to to)
                          :evidence (list :swing swing))
                    events)))))
    (nreverse events)))

(financial-chart-register-detector
 'fib.retracement
 (lambda (v p) (financial-chart-swing--retracements v (plist-get p :window) (append (plist-get p :levels) nil)))
 :kind 'fib :direction 'both
 :params '((:name window :type integer :default nil :min 2 :doc "bars of the swing; null: every bar so far")
           (:name levels :type numbers :default [0 0.236 0.382 0.5 0.618 0.786 1] :doc "retracement ratios"))
 :lag 0 :lookback 0 :needs '(high low)
 :definition "Fibonacci retracement (TradingView \"Fib Retracement\" drawing tool, auto-anchored): levels TO - r x (TO - FROM) of the swing between the highest high and the lowest low of the last WINDOW bars, from whichever came first.  An event each bar the swing changes; the latest is the swing as of the last bar.  Strength: the swing in average ranges, over 10."
 :invariance '(scale shift)
 :mark '(:type "levels")
 :example "examples/indicators/fibonacci.json"
 :doc "the window's high-low swing and its retracement levels")

;;; level.auto-sr

(defun financial-chart-level-touches (v y band from to)
  "Visits of bars FROM..TO of V to the level Y within BAND: runs of
consecutive bars whose range comes within BAND of Y count once each."
  (let ((visits 0) (inside nil))
    (cl-loop for i from (max 0 from) to (min to (1- (length v)))
             for touching = (and (<= (financial-chart-swing--l v i) (+ y band))
                                 (>= (financial-chart-swing--h v i) (- y band)))
             do (when (and touching (not inside)) (cl-incf visits))
             do (setq inside touching))
    visits))

(defun financial-chart-level-band (v p y i)
  "The auto-sr band of level Y at bar I of V with params P."
  (if (plist-get p :tolerance) (* (plist-get p :tolerance) (abs y))
    (* (plist-get p :atr) (financial-chart-swing--atr v i))))

(defun financial-chart-swing--auto-sr (v p)
  "Raw support/resistance events of V with params P: one per pivot touch."
  (let* ((pivots (sort (financial-chart-swing--pivots v (plist-get p :left) (plist-get p :right))
                       (lambda (a b) (or (< (plist-get a :confirmed) (plist-get b :confirmed))
                                         (and (= (plist-get a :confirmed) (plist-get b :confirmed))
                                              (< (plist-get a :index) (plist-get b :index)))))))
         clusters events)
    ;; A cluster is (ID SUM COUNT FIRST MIN MAX).
    (dolist (pv pivots)
      (let* ((y (plist-get (plist-get pv :geometry) :y)) (c (plist-get pv :confirmed))
             (band (financial-chart-level-band v p y c))
             (best (car (sort (seq-filter (lambda (k) (<= (abs (- y (/ (nth 1 k) (nth 2 k)))) band)) clusters)
                              (lambda (a b) (< (abs (- y (/ (nth 1 a) (nth 2 a)))) (abs (- y (/ (nth 1 b) (nth 2 b))))))))))
        (if best
            (setf (nth 1 best) (+ (nth 1 best) y) (nth 2 best) (1+ (nth 2 best))
                  (nth 4 best) (min (nth 4 best) y) (nth 5 best) (max (nth 5 best) y))
          (setq best (list (length clusters) y 1 (plist-get pv :index) y y))
          (setq clusters (append clusters (list best))))
        (let* ((pivots (nth 2 best)) (y (/ (nth 1 best) pivots))
               (touches (financial-chart-level-touches v y band (nth 3 best) c)))
          (push (list :index (plist-get pv :index) :confirmed c :direction 'neutral
                      :strength (financial-chart-swing--clamp (/ touches 5.0))
                      :geometry (list :y y :from (nth 3 best) :to (plist-get pv :index))
                      :evidence (list :cluster (nth 0 best) :pivots pivots :touches touches
                                      :side (plist-get (plist-get pv :evidence) :side)
                                      :spread (if (> band 0) (/ (- (nth 5 best) (nth 4 best)) band) 0.0)))
                events))))
    (nreverse events)))

(financial-chart-register-detector
 'level.auto-sr #'financial-chart-swing--auto-sr
 :kind 'level :direction 'neutral
 :params (append financial-chart-swing--pivot-params
                 '((:name atr :type number :default 0.5 :min 0.01 :doc "cluster band in multiples of the 14-bar ATR at the touch")
                   (:name tolerance :type number :default nil :min 0.0001 :doc "if set, the band as a fraction of price instead (not shift-invariant)")))
 :lag 'right :lookback 3 :needs '(high low close)
 :definition "Automatic support and resistance: swing pivots (TradingView Pine ta.pivothigh/ta.pivotlow) joined, in the order they are confirmed, to the nearest level whose mean is within the band (ATR x the 14-bar average true range, or TOLERANCE x price); Osler, C. (2000), \"Support for Resistance: Technical Analysis and Intraday Exchange Rates\", FRBNY Economic Policy Review 6(2), on why levels at repeated turning points matter.  An event per pivot joining a level carries the level's mean, its pivots and its touches so far: visits of price to within the band since the level's first pivot, a run of bars counting once (evidence cluster, pivots, touches, spread); the newest event of each cluster is the level as of then.  Strength: touches over 5."
 :invariance '(scale shift)
 :mark '(:type "level")
 :example "examples/indicators/support-resistance.json"
 :doc "clustered pivot levels, one event per touch")

;;; trend.auto-lines

(defun financial-chart-swing--trend-lines (v p)
  "Raw trend-line events of V (params P).
A line through each pair of consecutive swing lows or swing highs."
  (let ((pivots (financial-chart-swing--pivots v (plist-get p :left) (plist-get p :right)))
        (last (list (cons "low" nil) (cons "high" nil))) events)
    (dolist (pv pivots)
      (let* ((side (plist-get (plist-get pv :evidence) :side))
             (prev (cdr (assoc side last)))
             (pt (cons (plist-get pv :index) (plist-get (plist-get pv :geometry) :y))))
        (when prev
          (let* ((ppt (cons (plist-get prev :index) (plist-get (plist-get prev :geometry) :y)))
                 (slope (/ (/ (- (cdr pt) (cdr ppt)) (float (- (car pt) (car ppt))))
                           (financial-chart-swing--range v (car ppt) (car pt)))))
            (push (list :index (car pt) :confirmed (plist-get pv :confirmed)
                        :direction (cond ((> slope 0) 'bullish) ((< slope 0) 'bearish) (t 'neutral))
                        :strength (financial-chart-swing--clamp (* 4 (abs slope)))
                        :geometry (list :points (list ppt pt))
                        :evidence (list :side (if (equal side "low") "support" "resistance") :slope slope))
                  events)))
        (setcdr (assoc side last) pv)))
    (nreverse events)))

(financial-chart-register-detector
 'trend.auto-lines #'financial-chart-swing--trend-lines
 :kind 'trend :direction 'both :params financial-chart-swing--pivot-params
 :lag 'right :lookback 3 :needs '(high low)
 :definition "Automatic trend lines (Edwards, R. and Magee, J. 1948, \"Technical Analysis of Stock Trends\", ch. 14; TradingView auto trendlines): a support line through each two consecutive swing lows and a resistance line through each two consecutive swing highs (Pine ta.pivotlow/ta.pivothigh), confirmed with the second pivot.  Evidence slope is in average ranges per bar; strength 4 x |slope|, clamped."
 :invariance '(scale shift)
 :mark '(:type "line")
 :example "examples/indicators/auto-trendlines.json"
 :doc "support and resistance lines through consecutive pivots")

(provide 'financial-chart-detectors-swings)
;;; financial-chart-detectors-swings.el ends here
