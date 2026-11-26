;;; financial-chart-eas-markups.el --- automatic levels, candle patterns and drawing tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; More annotation types for composed charts (fc-gty), drawn like the
;; ones of `financial-chart-eas-annotations' and found by the pure
;; detectors of `financial-chart-patterns':
;;
;;   support-resistance  swing pivots clustered into levels, the
;;                       strongest "max" drawn with their touch count
;;                       ("left", "right", "tolerance", "atr",
;;                       "touches", "window", "bands")
;;   patterns            candlestick patterns marked over/under their
;;                       bar ("only", "labels": short|name|none)
;;   zigzag              alternating pivots at a "deviation" % move
;;   auto-trendlines     lines through the last two swing lows and highs
;;   fib-extension       extension levels of a move "from" -> "to"
;;                       projected from "then" (default: the last three
;;                       zigzag pivots)
;;   fib-timezones       vertical lines at Fibonacci bar counts from "at"
;;   channel             a parallel channel: "from"/"to" and "through"
;;                       or "offset", else a regression channel
;;   pitchfork           Andrews' pitchfork on "points" (3 of [AT, Y])
;;   anchored-vwap       VWAP from the bar "at" (or "anchor": low|high)
;;   measure             a price range box with change, % and bars
;;   measured-move       the leg "from" -> "to" repeated from "then"
;;
;; A bad one signals `financial-chart-invalid-chart' with :code and the
;; JSON :path, like every other annotation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-patterns)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-styles)
(require 'financial-chart-eas-shift)
(require 'financial-chart-eas-annotations)

(defconst financial-chart-fibonacci-extension-ratios [0 0.618 1 1.272 1.618 2 2.618]
  "Default Fibonacci extension ratios.")

(defconst financial-chart-fibonacci-time-zones [0 1 2 3 5 8 13 21 34 55 89 144 233]
  "Bar counts of the Fibonacci time zones.")

;;; Arguments

(defun financial-chart-markup--key (key)
  "KEY's JSON name."
  (substring (symbol-name key) 1))

(defun financial-chart-markup--int (a key path default &optional minimum)
  "A's KEY as an integer of at least MINIMUM (1), else DEFAULT; PATH locates A."
  (let ((v (financial-chart-series-get a key)))
    (cond ((null v) default)
          ((and (integerp v) (>= v (or minimum 1))) v)
          (t (financial-chart-series-fail (format "%s/%s" path (financial-chart-markup--key key))
                                          "INVALID_ANNOTATION" "%s is an integer >= %d, got %S"
                                          (financial-chart-markup--key key) (or minimum 1) v)))))

(defun financial-chart-markup--positive (a key path default)
  "A's KEY as a number above 0, else DEFAULT; PATH locates A."
  (let ((v (financial-chart-series-get a key)))
    (cond ((null v) default)
          ((and (numberp v) (> v 0)) v)
          (t (financial-chart-series-fail (format "%s/%s" path (financial-chart-markup--key key))
                                          "INVALID_ANNOTATION" "%s is a number above 0, got %S"
                                          (financial-chart-markup--key key) v)))))

(defun financial-chart-markup--choice (a key path choices)
  "A's KEY, one of CHOICES (the first by default); PATH locates A."
  (let ((v (or (financial-chart-series-get a key) (car choices))))
    (unless (member v choices)
      (financial-chart-series-fail (format "%s/%s" path (financial-chart-markup--key key)) "INVALID_ANNOTATION"
                                   "%s is %s, got %S" (financial-chart-markup--key key)
                                   (string-join choices ", ") v))
    v))

(defun financial-chart-markup--ratios (a path default)
  "A's \"levels\" ratios, else DEFAULT; PATH locates A."
  (let ((ratios (or (financial-chart-series-get a :levels) default)))
    (unless (and (or (vectorp ratios) (consp ratios)) (seq-every-p #'numberp ratios))
      (financial-chart-series-fail (concat path "/levels") "INVALID_ANNOTATION"
                                   "levels are ratios such as 1.618, got %S" ratios))
    (append ratios nil)))

(defmacro financial-chart-markup--detect (path &rest body)
  "BODY, a detector call; a bad argument is INVALID_ANNOTATION at PATH."
  (declare (indent 1))
  `(condition-case err (progn ,@body)
     (financial-chart-invalid-indicator
      (financial-chart-series-fail ,path "INVALID_ANNOTATION" "%s" (cadr err)))))

(defun financial-chart-markup--colour (a)
  "A's \"color\", else the annotation colour."
  (or (financial-chart-series-get a :color) financial-chart-annotation-color))

;;; Geometry

(defun financial-chart-markup--bar-x (ctx i)
  "The x position of bar I in CTX."
  (aref (plist-get ctx :xs) i))

(defun financial-chart-markup--end (ctx)
  "The chart's last x position in CTX (past the bars when a series is shifted)."
  (let ((xs (financial-chart-shift-xs ctx))) (aref xs (1- (length xs)))))

(defun financial-chart-markup--pivot-point (ctx pivot)
  "PIVOT (INDEX KIND PRICE) as (X . Y) in CTX."
  (cons (financial-chart-markup--bar-x ctx (car pivot)) (nth 2 pivot)))

(defun financial-chart-markup--points (ctx a key path n auto)
  "A's KEY, N points [AT, Y], as (X . Y)s, else AUTO's N in CTX.
AUTO is a function of no arguments returning N (X . Y)s or nil;
PATH locates A."
  (let ((given (financial-chart-series-get a key)))
    (if given
        (let ((points (append given nil)))
          (unless (= (length points) n)
            (financial-chart-series-fail (format "%s/%s" path (financial-chart-markup--key key))
                                         "INVALID_ANNOTATION" "%s is %d points [AT, Y], got %S"
                                         (financial-chart-markup--key key) n given))
          (seq-map-indexed (lambda (p i)
                             (financial-chart-annotation--point
                              ctx p (format "%s/%s/%d" path (financial-chart-markup--key key) i)))
                           points))
      (or (funcall auto)
          (financial-chart-series-fail (format "%s/%s" path (financial-chart-markup--key key))
                                       "INVALID_ANNOTATION"
                                       "Found too few pivots for %s; lower \"deviation\" or give \"%s\""
                                       (financial-chart-series-get a :type)
                                       (financial-chart-markup--key key))))))

(defun financial-chart-markup--last-pivots (ctx a path n)
  "The last N confirmed zigzag pivots of CTX's bars as (X . Y)s, or nil.
A gives the \"deviation\"; PATH locates A."
  (let ((pivots (financial-chart-markup--detect (concat path "/deviation")
                  (financial-chart-zigzag (plist-get ctx :bars)
                                          (financial-chart-markup--positive a :deviation path 5)))))
    ;; The last pivot is the running extreme, not yet confirmed.
    (let ((confirmed (butlast pivots)))
      (when (>= (length confirmed) n)
        (mapcar (lambda (p) (financial-chart-markup--pivot-point ctx p)) (last confirmed n))))))

(defun financial-chart-markup--line (p1 p2 x)
  "The y at X on the line through points P1 and P2, (X . Y) each."
  (if (= (car p1) (car p2)) (cdr p2)
    (+ (cdr p1) (* (/ (float (- (cdr p2) (cdr p1))) (- (car p2) (car p1))) (- x (car p1))))))

(defun financial-chart-markup--segment-row (x1 y1 x2 y2)
  "A rule row from (X1, Y1) to (X2, Y2)."
  (list :time x1 :time2 x2 :y y1 :y2 y2))

(defun financial-chart-markup--segments (ctx name rows colour dash width)
  "A rule layer NAME drawing segment ROWS in COLOUR, DASH and WIDTH."
  (financial-chart-annotation--rule ctx name rows colour dash width
                                    :x "time" :x2 "time2" :y "y" :y2 "y2"))

(defun financial-chart-markup--labels (ctx name rows colour &rest mark)
  "A text layer NAME of ROWS ({time, y, label}) in COLOUR; nil without ROWS."
  (when rows
    (list (apply #'financial-chart-annotation--text ctx name rows colour mark))))

(defun financial-chart-markup--level-layers (ctx a name start levels colour)
  "Horizontal rules NAME from START to the chart's end at LEVELS, labelled.
LEVELS are (RATIO . PRICE); A gives dash and width."
  (let* ((end (financial-chart-markup--end ctx))
         (rows (mapcar (lambda (l) (list :time start :time2 end :y (cdr l)
                                         :label (format "%s %.2f" (car l) (cdr l))))
                       levels)))
    (list (financial-chart-annotation--rule ctx name rows colour (financial-chart-series-get a :dash)
                                            (or (financial-chart-series-get a :width) 1)
                                            :x "time" :x2 "time2" :y "y")
          (financial-chart-annotation--text ctx (concat name "-labels")
                                            (mapcar (lambda (r) (plist-put (copy-sequence r) :time end)) rows)
                                            colour :align "right" :baseline "bottom" :dy -1))))

(defun financial-chart-markup--format-ratio (r)
  "Ratio R as a percent label."
  (format "%s%%" (/ (round (* 1000 r)) 10.0)))

;;; Support and resistance

(defun financial-chart-markup--support-resistance (ctx a name path)
  "Layers of the support-resistance annotation A named NAME at PATH in CTX."
  (let* ((tolerance (financial-chart-markup--positive a :tolerance path nil))
         (atr (financial-chart-markup--positive a :atr path nil))
         (_ (when (and tolerance atr)
              (financial-chart-series-fail (concat path "/atr") "INVALID_ANNOTATION"
                                           "Give \"tolerance\" or \"atr\", not both")))
         (levels (financial-chart-markup--detect path
                   (financial-chart-support-resistance
                    (plist-get ctx :bars)
                    :left (financial-chart-markup--int a :left path 3)
                    :right (financial-chart-markup--int a :right path nil)
                    :tolerance tolerance :atr atr
                    :max (financial-chart-markup--int a :max path 4)
                    :min-touches (financial-chart-markup--int a :touches path 1)
                    :window (financial-chart-markup--int a :window path nil 2))))
         (end (financial-chart-markup--end ctx))
         (band (financial-chart-series-get a :bands))
         (colour-of (lambda (l) (or (financial-chart-series-get a :color)
                                    (plist-get ctx (if (eq (plist-get l :kind) 'support) :up :down)))))
         (row (lambda (l) (list :time (financial-chart-markup--bar-x ctx (plist-get l :first)) :time2 end
                                :y (plist-get l :price)
                                :label (format "%s %.2f x%d" (if (eq (plist-get l :kind) 'support) "S" "R")
                                               (plist-get l :price) (plist-get l :touches)))))
         (width (or (financial-chart-series-get a :tolerance) 0.01)))
    (cl-loop for kind in '(support resistance)
             for group = (seq-filter (lambda (l) (eq (plist-get l :kind) kind)) levels)
             for colour = (and group (funcall colour-of (car group)))
             for layer = (format "%s-%s" name kind)
             when group
             append (append
                     (when band
                       (list (list :name (concat layer "-bands")
                                   :data (list :values
                                               (vconcat (mapcar (lambda (l)
                                                                  (let ((r (funcall row l)) (h (* 0.5 width (plist-get l :price))))
                                                                    (list :time (plist-get r :time) :time2 end
                                                                          :y (- (plist-get r :y) h) :y2 (+ (plist-get r :y) h))))
                                                                group)))
                                   :mark (list :type "rect" :color colour :opacity 0.12)
                                   :encoding (financial-chart-annotation--encoding
                                              ctx '(:x "time" :x2 "time2" :y "y" :y2 "y2")))))
                     (list (financial-chart-annotation--rule ctx layer (mapcar row group) colour
                                                             (or (financial-chart-series-get a :dash) [6 3])
                                                             (or (financial-chart-series-get a :width) 1.5)
                                                             :x "time" :x2 "time2" :y "y"
                                                             :tooltip [(:field "label" :type "nominal")]))
                     (financial-chart-markup--labels
                      ctx (concat layer "-labels")
                      (mapcar (lambda (l) (plist-put (funcall row l) :time end)) group)
                      colour :align "right" :baseline "bottom" :dy -2)))))

;;; Candlestick patterns

(defun financial-chart-markup--patterns (ctx a name path)
  "Layers of the patterns annotation A named NAME at PATH in CTX."
  (let* ((only (financial-chart-series-get a :only))
         (only (cond ((null only) nil)
                     ((stringp only) (list only))
                     ((and (or (vectorp only) (consp only)) (seq-every-p #'stringp only)) (append only nil))
                     (t (financial-chart-series-fail (concat path "/only") "INVALID_ANNOTATION"
                                                     "only is an array of pattern names, got %S" only))))
         (_ (seq-do-indexed
             (lambda (p i)
               (unless (assoc p financial-chart-candle-pattern-names)
                 (financial-chart-series-fail (format "%s/only/%d" path i) "INVALID_ANNOTATION"
                                              "Unknown candlestick pattern %S; patterns: %s" p
                                              (mapconcat #'car financial-chart-candle-pattern-names ", "))))
             only))
         (labels (financial-chart-markup--choice a :labels path '("short" "name" "none")))
         (found (financial-chart-candle-patterns (plist-get ctx :bars) only))
         (bars (vconcat (plist-get ctx :bars)))
         (range (financial-chart-annotation--range ctx))
         (pad (* 0.04 (- (cdr range) (car range))))
         (stack (make-hash-table :test #'equal))
         (rows (mapcar
                (lambda (f)
                  (pcase-let* ((`(,i ,pattern ,direction) f)
                               (below (eq direction 'bullish))
                               (k (cl-incf (gethash (cons i below) stack -1)))
                               (bar (aref bars i)))
                    (list :direction direction
                          :time (financial-chart-markup--bar-x ctx i)
                          :y (if below (- (plist-get bar :low) (* pad (1+ k)))
                               (+ (plist-get bar :high) (* pad (1+ k))))
                          :name pattern
                          :label (pcase labels
                                   ("name" pattern)
                                   (_ (nth 1 (assoc pattern financial-chart-candle-pattern-names)))))))
                found)))
    (cl-loop for (direction shape key) in '((bullish "triangle-up" :up) (bearish "triangle-down" :down)
                                            (neutral "diamond" nil))
             for group = (mapcar (lambda (r) (cl-loop for (k v) on r by #'cddr
                                                      unless (eq k :direction) append (list k v)))
                                 (seq-filter (lambda (r) (eq (plist-get r :direction) direction)) rows))
             for colour = (or (financial-chart-series-get a :color)
                              (if key (plist-get ctx key) financial-chart-annotation-color))
             for layer = (format "%s-%s" name direction)
             when group
             append (append
                     (list (list :name layer :data (list :values (vconcat group))
                                 :mark (list :type "point" :shape shape :filled t :size 40
                                             :color colour :opacity 1)
                                 :encoding (list :x (financial-chart-annotation--x-enc ctx)
                                                 :y (financial-chart-styles-y "y")
                                                 :tooltip [(:field "name" :type "nominal")])))
                     (unless (equal labels "none")
                       (financial-chart-markup--labels
                        ctx (concat layer "-labels") group colour
                        :baseline (if (eq direction 'bullish) "top" "bottom")
                        :dy (if (eq direction 'bullish) 7 -7)))))))

;;; Zigzag and automatic trend lines

(defun financial-chart-markup--zigzag (ctx a name path)
  "Layers of the zigzag annotation A named NAME at PATH in CTX."
  (let* ((pivots (financial-chart-markup--detect (concat path "/deviation")
                   (financial-chart-zigzag (plist-get ctx :bars)
                                           (financial-chart-markup--positive a :deviation path 5))))
         (colour (financial-chart-markup--colour a))
         (points (mapcar (lambda (p) (financial-chart-markup--pivot-point ctx p)) pivots))
         (rows (cl-loop for (p q) on points while q
                        collect (financial-chart-markup--segment-row (car p) (cdr p) (car q) (cdr q))))
         ;; HH/HL/LH/LL against the previous pivot of the same kind.
         (labels (when (financial-chart-series-get a :labels)
                   (cl-loop for p in pivots for i from 0
                            for prev = (cl-find (nth 1 p) (seq-take pivots i) :key #'cadr :from-end t)
                            when prev
                            collect (list :time (financial-chart-markup--bar-x ctx (car p)) :y (nth 2 p)
                                          :label (concat (if (> (nth 2 p) (nth 2 prev)) "H" "L")
                                                         (if (eq (nth 1 p) 'high) "H" "L")))))))
    (when rows
      (append
       (list (financial-chart-markup--segments ctx name rows colour (financial-chart-series-get a :dash)
                                               (or (financial-chart-series-get a :width) 1.5)))
       (financial-chart-markup--labels ctx (concat name "-labels") labels colour
                                       :baseline "bottom" :dy -4)))))

(defun financial-chart-markup--extend (ctx p1 p2 extend)
  "The segment P1-P2 ((X . Y) each) extended per EXTEND in CTX, as a row."
  (let* ((xs (financial-chart-shift-xs ctx))
         (x1 (if (member extend '("left" "both")) (aref xs 0) (car p1)))
         (x2 (if (member extend '("right" "both")) (aref xs (1- (length xs))) (car p2))))
    (financial-chart-markup--segment-row x1 (financial-chart-markup--line p1 p2 x1)
                                         x2 (financial-chart-markup--line p1 p2 x2))))

(defun financial-chart-markup--auto-trendlines (ctx a name path)
  "Layers of the auto-trendlines annotation A named NAME at PATH in CTX."
  (let* ((kind (financial-chart-markup--choice a :kind path '("both" "support" "resistance")))
         (extend (financial-chart-markup--choice a :extend path '("right" "none" "left" "both")))
         (swings (financial-chart-markup--detect path
                   (financial-chart-swing-points (plist-get ctx :bars)
                                                 (financial-chart-markup--int a :left path 3)
                                                 (financial-chart-markup--int a :right path nil))))
         (last-two (lambda (k) (last (seq-filter (lambda (s) (eq (nth 1 s) k)) swings) 2))))
    (cl-loop for (k which key) in '((low "support" :up) (high "resistance" :down))
             for pair = (funcall last-two k)
             for colour = (or (financial-chart-series-get a :color) (plist-get ctx key))
             for layer = (format "%s-%s" name which)
             when (and (member kind (list "both" which)) (= (length pair) 2))
             append (let* ((p1 (financial-chart-markup--pivot-point ctx (car pair)))
                           (p2 (financial-chart-markup--pivot-point ctx (cadr pair)))
                           (row (financial-chart-markup--extend ctx p1 p2 extend)))
                      (list (financial-chart-markup--segments ctx layer (list row) colour
                                                              (financial-chart-series-get a :dash)
                                                              (or (financial-chart-series-get a :width) 1.5))
                            (list :name (concat layer "-pivots")
                                  :data (list :values (vector (list :time (car p1) :y (cdr p1))
                                                              (list :time (car p2) :y (cdr p2))))
                                  :mark (list :type "point" :shape "circle" :filled t :size 20 :color colour)
                                  :encoding (list :x (financial-chart-annotation--x-enc ctx)
                                                  :y (financial-chart-styles-y "y"))))))))

;;; Fibonacci extension and time zones

(defun financial-chart-markup--fib-extension (ctx a name path)
  "Layers of the fib-extension annotation A named NAME at PATH in CTX."
  (pcase-let* ((auto (lambda () (financial-chart-markup--last-pivots ctx a path 3)))
               (`(,from ,to ,then)
                (if (or (financial-chart-series-get a :from) (financial-chart-series-get a :to)
                        (financial-chart-series-get a :then))
                    (mapcar (lambda (key)
                              (unless (financial-chart-series-get a key)
                                (financial-chart-series-fail (format "%s/%s" path (financial-chart-markup--key key))
                                                             "INVALID_ANNOTATION"
                                                             "fib-extension needs \"from\", \"to\" and \"then\" (or none)"))
                              (financial-chart-annotation--point
                               ctx (financial-chart-series-get a key)
                               (format "%s/%s" path (financial-chart-markup--key key))))
                            '(:from :to :then))
                  (financial-chart-markup--points ctx a :points path 3 auto)))
               (ratios (financial-chart-markup--ratios a path financial-chart-fibonacci-extension-ratios))
               (move (- (cdr to) (cdr from)))
               (levels (mapcar (lambda (r) (cons (financial-chart-markup--format-ratio r) (+ (cdr then) (* r move))))
                               ratios))
               (colour (financial-chart-markup--colour a)))
    (append
     (list (financial-chart-markup--segments
            ctx (concat name "-legs")
            (list (financial-chart-markup--segment-row (car from) (cdr from) (car to) (cdr to))
                  (financial-chart-markup--segment-row (car to) (cdr to) (car then) (cdr then)))
            colour [3 3] 1))
     (financial-chart-markup--level-layers ctx a name (car then) levels colour))))

(defun financial-chart-markup--fib-timezones (ctx a name path)
  "Layers of the fib-timezones annotation A named NAME at PATH in CTX."
  (let* ((at (financial-chart-series-get a :at))
         (start (if at (financial-chart-annotation--bar ctx at (concat path "/at"))
                  (let ((pivots (financial-chart-zigzag (plist-get ctx :bars)
                                                        (financial-chart-markup--positive a :deviation path 5))))
                    (if pivots (car (car pivots)) 0))))
         (scale (financial-chart-markup--int a :scale path 1))
         (xs (financial-chart-shift-xs ctx))
         (colour (financial-chart-markup--colour a))
         (rows (cl-loop for k across financial-chart-fibonacci-time-zones
                        for i = (+ start (* scale k))
                        when (< i (length xs))
                        collect (list :time (aref xs i) :label (format "%d" k)))))
    (append
     (list (financial-chart-annotation--rule ctx name rows colour (or (financial-chart-series-get a :dash) [2 3])
                                             (financial-chart-series-get a :width)
                                             :x "time" :tooltip [(:field "label" :type "nominal")]))
     (financial-chart-markup--labels ctx (concat name "-labels") rows colour
                                     :align "left" :baseline "top" :dx 2))))

;;; Channels and pitchforks

(defun financial-chart-markup--regression (ctx window)
  "The least-squares line of CTX's closes over the last WINDOW bars.
Return (P1 P2 OFFSET): two points on it and the largest distance of a
high or low from it."
  (let* ((bars (vconcat (plist-get ctx :bars))) (n (length bars))
         (start (if window (max 0 (- n window)) 0))
         (is (number-sequence start (1- n)))
         (m (float (length is)))
         (xm (/ (apply #'+ is) m))
         (ym (/ (apply #'+ (mapcar (lambda (i) (plist-get (aref bars i) :close)) is)) m))
         (sxx (apply #'+ (mapcar (lambda (i) (expt (- i xm) 2)) is)))
         (slope (if (zerop sxx) 0
                  (/ (apply #'+ (mapcar (lambda (i) (* (- i xm) (- (plist-get (aref bars i) :close) ym))) is))
                     sxx)))
         (at (lambda (i) (+ ym (* slope (- i xm)))))
         (offset (apply #'max (mapcar (lambda (i)
                                        (max (- (plist-get (aref bars i) :high) (funcall at i))
                                             (- (funcall at i) (plist-get (aref bars i) :low))))
                                      is))))
    (list (cons (financial-chart-markup--bar-x ctx start) (funcall at start))
          (cons (financial-chart-markup--bar-x ctx (1- n)) (funcall at (1- n)))
          offset)))

(defun financial-chart-markup--channel (ctx a name path)
  "Layers of the channel annotation A named NAME at PATH in CTX."
  (pcase-let* ((given (or (financial-chart-series-get a :from) (financial-chart-series-get a :to)))
               (`(,p1 ,p2 ,offset)
                (if (not given)
                    (pcase-let ((`(,p1 ,p2 ,off) (financial-chart-markup--regression
                                                  ctx (financial-chart-markup--int a :window path nil 2))))
                      ;; The regression line is the middle; offset each way.
                      (list (cons (car p1) (- (cdr p1) off)) (cons (car p2) (- (cdr p2) off)) (* 2 off)))
                  (let* ((p1 (financial-chart-annotation--point ctx (financial-chart-series-get a :from)
                                                                (concat path "/from")))
                         (p2 (financial-chart-annotation--point ctx (financial-chart-series-get a :to)
                                                                (concat path "/to")))
                         (through (financial-chart-series-get a :through)))
                    (list p1 p2
                          (cond (through
                                 (let ((p (financial-chart-annotation--point ctx through (concat path "/through"))))
                                   (- (cdr p) (financial-chart-markup--line p1 p2 (car p)))))
                                ((numberp (financial-chart-series-get a :offset))
                                 (financial-chart-series-get a :offset))
                                (t (financial-chart-series-fail
                                    (concat path "/through") "INVALID_ANNOTATION"
                                    "A channel with \"from\"/\"to\" needs \"through\": [AT, Y] or \"offset\"")))))))
               (extend (financial-chart-markup--choice a :extend path '("right" "none" "left" "both")))
               (base (financial-chart-markup--extend ctx p1 p2 extend))
               (shift (lambda (row d) (list :time (plist-get row :time) :time2 (plist-get row :time2)
                                            :y (+ (plist-get row :y) d) :y2 (+ (plist-get row :y2) d))))
               (colour (financial-chart-markup--colour a))
               (label (financial-chart-series-get a :label)))
    (append
     (list (financial-chart-markup--segments ctx name (list base (funcall shift base offset)) colour
                                             (financial-chart-series-get a :dash)
                                             (or (financial-chart-series-get a :width) 1.5))
           (financial-chart-markup--segments ctx (concat name "-mid") (list (funcall shift base (/ offset 2.0)))
                                             colour [4 3] 1))
     (when label
       (financial-chart-markup--labels ctx (concat name "-label")
                                       (list (list :time (plist-get base :time2)
                                                   :y (+ (plist-get base :y2) offset) :label label))
                                       colour :align "right" :baseline "bottom" :dy -2)))))

(defun financial-chart-markup--pitchfork (ctx a name path)
  "Layers of the pitchfork annotation A named NAME at PATH in CTX."
  (pcase-let* ((`(,p1 ,p2 ,p3) (financial-chart-markup--points
                                ctx a :points path 3 (lambda () (financial-chart-markup--last-pivots ctx a path 3))))
               (mid (cons (/ (+ (car p2) (car p3)) 2.0) (/ (+ (cdr p2) (cdr p3)) 2.0)))
               (end (financial-chart-markup--end ctx))
               (slope-at (lambda (p x) (+ (cdr p) (- (financial-chart-markup--line p1 mid x)
                                                     (financial-chart-markup--line p1 mid (car p))))))
               (colour (financial-chart-markup--colour a)))
    (list (financial-chart-markup--segments
           ctx name
           (list (financial-chart-markup--segment-row (car p1) (cdr p1) end (financial-chart-markup--line p1 mid end))
                 (financial-chart-markup--segment-row (car p2) (cdr p2) end (funcall slope-at p2 end))
                 (financial-chart-markup--segment-row (car p3) (cdr p3) end (funcall slope-at p3 end)))
           colour (financial-chart-series-get a :dash) (or (financial-chart-series-get a :width) 1.5))
          (financial-chart-markup--segments
           ctx (concat name "-handle")
           (list (financial-chart-markup--segment-row (car p2) (cdr p2) (car p3) (cdr p3)))
           colour [3 3] 1))))

;;; Anchored VWAP

(defun financial-chart-markup--anchored-vwap (ctx a name path)
  "Layers of the anchored-vwap annotation A named NAME at PATH in CTX."
  (unless (plist-get ctx :volume)
    (financial-chart-series-fail path "NO_VOLUME" "anchored-vwap needs bars with \"volume\""))
  (let* ((bars (plist-get ctx :bars))
         (at (financial-chart-series-get a :at))
         (anchor (financial-chart-markup--choice a :anchor path '("low" "high" "first")))
         (window (financial-chart-markup--int a :window path nil 2))
         (n (length bars))
         (start (if window (max 0 (- n window)) 0))
         (index (cond (at (financial-chart-annotation--bar ctx at (concat path "/at")))
                      ((equal anchor "first") start)
                      (t (let ((key (if (equal anchor "low") :low :high))
                               (better (if (equal anchor "low") #'< #'>)))
                           (cl-loop with best = start for i from start below n
                                    when (funcall better (plist-get (nth i bars) key)
                                                  (plist-get (nth best bars) key))
                                    do (setq best i)
                                    finally return best)))))
         (values (financial-chart-anchored-vwap bars index))
         (colour (or (financial-chart-series-get a :color) "#ff9800"))
         (label (or (financial-chart-series-get a :label) "AVWAP"))
         (rows (cl-loop for v in values for i from 0
                        when v collect (list :time (financial-chart-markup--bar-x ctx i) :y v :label label))))
    (append
     (list (list :name name :data (list :values (vconcat rows))
                 :mark (financial-chart-styles-line-mark "line" colour (or (financial-chart-series-get a :width) 1.5)
                                                         (financial-chart-series-get a :dash))
                 :encoding (list :x (financial-chart-annotation--x-enc ctx) :y (financial-chart-styles-y "y")
                                 :tooltip [(:field "label" :type "nominal")
                                           (:field "y" :type "quantitative" :format ".2f")])))
     (when rows
       (financial-chart-markup--labels ctx (concat name "-label") (last rows) colour
                                       :align "left" :dx 3)))))

;;; Measures

(defun financial-chart-markup--change-label (ctx from to)
  "The change from FROM to TO ((X . Y) each) as text: price, %, bars."
  (let* ((d (- (cdr to) (cdr from)))
         (bars (abs (- (or (seq-position (financial-chart-shift-xs ctx) (car to) #'=) 0)
                       (or (seq-position (financial-chart-shift-xs ctx) (car from) #'=) 0)))))
    (format "%+.2f (%+.1f%%) %d bars" d (if (zerop (cdr from)) 0 (* 100.0 (/ d (cdr from)))) bars)))

(defun financial-chart-markup--measure (ctx a name path)
  "Layers of the measure annotation A named NAME at PATH in CTX."
  (let* ((from (financial-chart-annotation--point ctx (financial-chart-series-get a :from) (concat path "/from")))
         (to (financial-chart-annotation--point ctx (financial-chart-series-get a :to) (concat path "/to")))
         (colour (or (financial-chart-series-get a :color) (plist-get ctx (if (>= (cdr to) (cdr from)) :up :down))))
         (label (or (financial-chart-series-get a :label) (financial-chart-markup--change-label ctx from to))))
    (list (list :name name
                :data (list :values (vector (list :time (min (car from) (car to)) :time2 (max (car from) (car to))
                                                  :y (min (cdr from) (cdr to)) :y2 (max (cdr from) (cdr to)))))
                :mark (list :type "rect" :color colour :opacity (or (financial-chart-series-get a :opacity) 0.15))
                :encoding (financial-chart-annotation--encoding ctx '(:x "time" :x2 "time2" :y "y" :y2 "y2")))
          (financial-chart-markup--segments
           ctx (concat name "-arrow")
           (list (financial-chart-markup--segment-row (car from) (cdr from) (car to) (cdr to)))
           colour [3 3] 1)
          (financial-chart-annotation--text ctx (concat name "-label")
                                            (list (list :time (/ (+ (car from) (car to)) 2.0)
                                                        :y (max (cdr from) (cdr to)) :label label))
                                            colour :align "center" :baseline "bottom" :dy -2))))

(defun financial-chart-markup--measured-move (ctx a name path)
  "Layers of the measured-move annotation A named NAME at PATH in CTX."
  (pcase-let* ((`(,from ,to ,then)
                (if (financial-chart-series-get a :from)
                    (mapcar (lambda (key)
                              (financial-chart-annotation--point
                               ctx (financial-chart-series-get a key)
                               (format "%s/%s" path (financial-chart-markup--key key))))
                            '(:from :to :then))
                  (financial-chart-markup--points ctx a :points path 3
                                                  (lambda () (financial-chart-markup--last-pivots ctx a path 3)))))
               (target (+ (cdr then) (- (cdr to) (cdr from))))
               (tx (+ (car then) (- (car to) (car from))))
               (end (financial-chart-markup--end ctx))
               (tx (min tx end))
               (colour (financial-chart-markup--colour a)))
    (list (financial-chart-markup--segments
           ctx (concat name "-legs")
           (list (financial-chart-markup--segment-row (car from) (cdr from) (car to) (cdr to))
                 (financial-chart-markup--segment-row (car to) (cdr to) (car then) (cdr then)))
           colour nil 1.5)
          (financial-chart-markup--segments
           ctx name (list (financial-chart-markup--segment-row (car then) (cdr then) tx target))
           colour [4 2] 1.5)
          (financial-chart-annotation--rule ctx (concat name "-target")
                                            (list (list :time tx :time2 end :y target))
                                            colour [2 2] 1 :x "time" :x2 "time2" :y "y")
          (financial-chart-annotation--text ctx (concat name "-label")
                                            (list (list :time end :y target
                                                        :label (or (financial-chart-series-get a :label)
                                                                   (format "target %.2f" target))))
                                            colour :align "right" :baseline "bottom" :dy -2))))

;;; Registration

(defconst financial-chart-markup-types
  '(("support-resistance" . financial-chart-markup--support-resistance)
    ("patterns" . financial-chart-markup--patterns)
    ("zigzag" . financial-chart-markup--zigzag)
    ("auto-trendlines" . financial-chart-markup--auto-trendlines)
    ("fib-extension" . financial-chart-markup--fib-extension)
    ("fib-timezones" . financial-chart-markup--fib-timezones)
    ("channel" . financial-chart-markup--channel)
    ("pitchfork" . financial-chart-markup--pitchfork)
    ("anchored-vwap" . financial-chart-markup--anchored-vwap)
    ("measure" . financial-chart-markup--measure)
    ("measured-move" . financial-chart-markup--measured-move))
  "The markup annotation types of this file and their drawing functions.")

(pcase-dolist (`(,type . ,fn) financial-chart-markup-types)
  (financial-chart-annotation-register type fn))

(provide 'financial-chart-eas-markups)
;;; financial-chart-eas-markups.el ends here
