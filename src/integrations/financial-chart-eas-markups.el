;;; financial-chart-eas-markups.el --- automatic levels, candle patterns and drawing tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; More annotation types for composed charts (fc-gty), drawn like the
;; ones of `financial-chart-eas-annotations'.  The automatic ones are
;; views of detector events (docs/design/markups.md: detection is data,
;; drawing a view of it); `financial-chart-detect' returns the same
;; events as data:
;;
;;   support-resistance  level.auto-sr: the strongest "max" levels as of
;;                       the last bar with their touch count ("left",
;;                       "right", "atr", "tolerance", "touches",
;;                       "window", "bands")
;;   patterns            pattern.*: candlestick patterns over/under their
;;                       bar ("only", "trend", "since", "labels":
;;                       short|name|none)
;;   zigzag              swing.zigzag: confirmed pivots at a
;;                       "deviation" % (or "atr") reversal, and the
;;                       running leg dashed
;;   auto-trendlines     trend.auto-lines: the newest support and
;;                       resistance lines
;;   fib-extension       fib.extension of the last zigzag A-B-C, or of
;;                       "from" -> "to" projected from "then"
;;   fib-timezones       vertical lines at Fibonacci bar counts from "at"
;;                       (default: the first zigzag pivot)
;;   pitchfork           Andrews' pitchfork on "points", else the last
;;                       swing.abc leg
;;   measured-move       the leg "from" -> "to" repeated from "then",
;;                       else from the last swing.abc leg
;;
;; and drawing tools computed as of the last bar: channel (parallel or
;; regression), anchored-vwap (VWAP from "at" or "anchor": low|high)
;; and measure (a price range box).
;;
;; A bad one signals `financial-chart-invalid-chart' with :code and the
;; JSON :path, like every other annotation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-patterns)
(require 'financial-chart-detector-api)
(require 'financial-chart-detectors-candles)
(require 'financial-chart-detectors-swings)
(require 'financial-chart-detectors-tools)
(require 'financial-chart-evaluate)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-styles)
(require 'financial-chart-eas-shift)
(require 'financial-chart-eas-annotations)
(require 'financial-chart-eas-labels)
(require 'financial-chart-eas-presets)
(require 'eas)

(declare-function financial-chart-compose-read "financial-chart-eas-compose" (chart))
(declare-function financial-chart-compose--bars "financial-chart-eas-compose" (chart))

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
  "The points of the latest swing.abc leg of CTX's bars as (X . Y)s, or nil.
A gives \"deviation\" and \"atr\"; PATH locates A.  N must be 3."
  (cl-assert (= n 3))
  (when-let* ((e (car (last (financial-chart-markup--events ctx a path "swing.abc" '(:deviation :atr))))))
    (let ((table (financial-chart-markup--index-table ctx)))
      (mapcar (lambda (p) (financial-chart-markup--event-point ctx table p))
              (plist-get (plist-get e :geometry) :points)))))

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
  "A text layer NAME of ROWS ({time, y, label}) in COLOUR; nil without ROWS.
Its labels may move (an automatic markup's labels go around the others);
pin them with `financial-chart-labels-pin'."
  (when rows
    (list (financial-chart-labels--unpin (apply #'financial-chart-annotation--text ctx name rows colour mark)))))

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
          (financial-chart-labels-pin
           (financial-chart-annotation--text ctx (concat name "-labels")
                                             (mapcar (lambda (r) (plist-put (copy-sequence r) :time end)) rows)
                                             colour :align "right" :baseline "bottom" :dy -1)))))

(defun financial-chart-markup--format-ratio (r)
  "Ratio R as a percent label."
  (format "%s%%" (/ (round (* 1000 r)) 10.0)))

;;; Support and resistance

(defun financial-chart-markup--support-resistance (ctx a name path)
  "Layers of the support-resistance annotation A named NAME at PATH in CTX.
A view of level.auto-sr: each level's newest event, the MAX with the
most touches (at least \"touches\"), most recent first, kept only when
touched in the last \"window\" bars; support or resistance by the last
close."
  (let* ((events (financial-chart-markup--events ctx a path "level.auto-sr" '(:left :right :atr :tolerance)))
         (max (financial-chart-markup--int a :max path 4))
         (min-touches (financial-chart-markup--int a :touches path 1))
         (window (financial-chart-markup--int a :window path nil 2))
         (bars (vconcat (plist-get ctx :bars))) (n (length bars))
         (last-close (plist-get (aref bars (1- n)) :close))
         (latest (make-hash-table))
         (_ (dolist (e events) (puthash (plist-get (plist-get e :evidence) :cluster) e latest)))
         (table (financial-chart-markup--index-table ctx))
         ;; A level's touches as of the last bar, not as of its newest pivot.
         (params (financial-chart-detector-params "level.auto-sr"
                                                  (cl-loop for k in '(:left :right :atr :tolerance)
                                                           when (plist-member a k) append (list k (plist-get a k)))))
         (touches-now (make-hash-table :test #'eq))
         (_ (dolist (e (hash-table-values latest))
              (let ((y (plist-get (plist-get e :geometry) :y)))
                (puthash e (financial-chart-level-touches
                            bars y (financial-chart-level-band bars params y (1- n))
                            (gethash (plist-get (plist-get e :geometry) :from) table) (1- n))
                         touches-now))))
         (levels (seq-filter (lambda (e) (and (>= (gethash e touches-now) min-touches)
                                              (or (null window) (>= (plist-get e :index) (- n window)))))
                             (hash-table-values latest)))
         (levels (seq-take (sort levels (lambda (x y)
                                          (let ((tx (gethash x touches-now)) (ty (gethash y touches-now)))
                                            (or (> tx ty) (and (= tx ty) (> (plist-get x :index) (plist-get y :index)))))))
                           max))
         (end (financial-chart-markup--end ctx))
         (band (lambda (e) (let ((g (plist-get e :geometry)))
                             (* 0.5 (or (financial-chart-series-get a :tolerance) 0.01) (abs (plist-get g :y))))))
         (kind (lambda (e) (if (< (plist-get (plist-get e :geometry) :y) last-close) 'support 'resistance)))
         (row (lambda (e) (let ((y (plist-get (plist-get e :geometry) :y)))
                            (list :time (financial-chart-markup--bar-x
                                         ctx (gethash (plist-get (plist-get e :geometry) :from) table))
                                  :time2 end :y y :id (plist-get e :id)
                                  :label (format "%s %.2f x%d" (if (eq (funcall kind e) 'support) "S" "R") y
                                                 (gethash e touches-now)))))))
    (cl-loop for k in '(support resistance)
             for group = (seq-filter (lambda (e) (eq (funcall kind e) k)) levels)
             for colour = (or (financial-chart-series-get a :color) (plist-get ctx (if (eq k 'support) :up :down)))
             for layer = (format "%s-%s" name k)
             when group
             append (append
                     (when (financial-chart-series-get a :bands)
                       (list (list :name (concat layer "-bands")
                                   :data (list :values
                                               (vconcat (mapcar (lambda (e)
                                                                  (let ((r (funcall row e)) (h (funcall band e)))
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
                                                             :tooltip [(:field "label" :type "nominal")
                                                                       (:field "id" :type "nominal")]))
                     ;; Labelled at the level's first touch, not the right edge, where
                     ;; Fibonacci and level labels would sit on each other.
                     (mapcar #'financial-chart-labels-pin
                             (financial-chart-markup--labels
                              ctx (concat layer "-labels") (mapcar row group)
                              colour :align "left" :baseline "bottom" :dx 2 :dy -2))))))

;;; Candlestick patterns

(defun financial-chart-markup--patterns (ctx a name path)
  "Layers of the patterns annotation A named NAME at PATH in CTX.
A view of the pattern.* detectors (\"only\" names some, \"trend\" is
passed to each that takes it, \"since\" keeps the last N bars); a plain
doji on a bar with a dragonfly or gravestone doji is left out."
  (let* ((only (financial-chart-series-get a :only))
         (only (cond ((null only) nil)
                     ((stringp only) (list only))
                     ((and (or (vectorp only) (consp only)) (seq-every-p #'stringp only)) (append only nil))
                     (t (financial-chart-series-fail (concat path "/only") "INVALID_ANNOTATION"
                                                     "only is an array of pattern names, got %S" only))))
         (all (financial-chart-candle-pattern-list))
         (_ (seq-do-indexed
             (lambda (p i)
               (unless (member p all)
                 (financial-chart-series-fail (format "%s/only/%d" path i) "INVALID_ANNOTATION"
                                              "Unknown candlestick pattern %S; patterns: %s" p (string-join all ", "))))
             only))
         (labels (financial-chart-markup--choice a :labels path '("short" "name" "none")))
         (show-noise (financial-chart-series-get a :show_noise))
         (since (financial-chart-markup--int a :since path nil))
         (trend (financial-chart-series-get a :trend))
         (_ (when (and trend (not (and (integerp trend) (>= trend 0))))
              (financial-chart-series-fail (concat path "/trend") "INVALID_ANNOTATION"
                                           "trend is a bar count >= 0, got %S" trend)))
         (bars (vconcat (plist-get ctx :bars)))
         (n (length bars))
         (found (cl-loop for p in (or only all)
                         for detector = (concat "pattern." p)
                         append (financial-chart-detect
                                 detector bars
                                 (and trend (plist-get (financial-chart-detector detector) :params)
                                      (list :trend trend)))))
         (dojis (mapcar (lambda (e) (plist-get e :index))
                        (seq-filter (lambda (e) (member (plist-get e :detector)
                                                        '("pattern.dragonfly-doji" "pattern.gravestone-doji")))
                                    found)))
         (found (seq-remove (lambda (e) (and (equal (plist-get e :detector) "pattern.doji")
                                             (memq (plist-get e :index) dojis)))
                            found))
         (found (if since (seq-filter (lambda (e) (>= (plist-get e :index) (- n since))) found) found))
         (found (sort found (lambda (x y) (< (plist-get x :index) (plist-get y :index)))))
         (range (financial-chart-annotation--range ctx))
         (pad (* 0.04 (- (cdr range) (car range))))
         (stack (make-hash-table :test #'equal))
         (rows (mapcar
                (lambda (e)
                  (let* ((i (plist-get e :index))
                         (direction (intern (plist-get e :direction)))
                         (pattern (string-remove-prefix "pattern." (plist-get e :detector)))
                         (below (eq direction 'bullish))
                         (k (cl-incf (gethash (cons i below) stack -1)))
                         (bar (aref bars i)))
                    (list :direction direction
                          :time (financial-chart-markup--bar-x ctx i)
                          :y (if below (- (plist-get bar :low) (* pad (1+ k)))
                               (+ (plist-get bar :high) (* pad (1+ k))))
                          :name pattern :id (plist-get e :id)
                          :verdict (or (plist-get (financial-chart-evidence-verdict (plist-get e :detector)) :status)
                                       "untested")
                          :strength (plist-get e :strength)
                          :label (pcase labels
                                   ("name" pattern)
                                   (_ (plist-get (plist-get (financial-chart-detector (plist-get e :detector)) :mark)
                                                 :label))))))
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
                                                 ;; A detector whose evidence says noise is drawn faded.
                                                 :opacity (if show-noise '(:value 1)
                                                            '(:condition (:test "datum.verdict === 'noise'" :value 0.3)
                                                                         :value 1))
                                                 :tooltip [(:field "name" :type "nominal")
                                                           (:field "strength" :type "quantitative" :format ".2f")
                                                           (:field "id" :type "nominal")])))
                     (unless (equal labels "none")
                       (financial-chart-markup--labels
                        ctx (concat layer "-labels") group colour
                        :baseline (if (eq direction 'bullish) "top" "bottom")
                        :dy (if (eq direction 'bullish) 7 -7)))))))

;;; Zigzag and automatic trend lines

(defun financial-chart-markup--zigzag (ctx a name path)
  "Layers of the zigzag annotation A named NAME at PATH in CTX.
A view of swing.zigzag: its confirmed pivots joined, and a dashed leg
from the last one to the running extreme since."
  (let* ((events (financial-chart-markup--events ctx a path "swing.zigzag" '(:deviation :atr)))
         (bars (vconcat (plist-get ctx :bars))) (n (length bars))
         (colour (financial-chart-markup--colour a))
         (pivots (mapcar (lambda (e) (list (plist-get e :index) (plist-get (plist-get e :evidence) :side)
                                           (plist-get (plist-get e :geometry) :y)))
                         events))
         (points (mapcar (lambda (p) (cons (financial-chart-markup--bar-x ctx (car p)) (nth 2 p))) pivots))
         (rows (cl-loop for (p q) on points while q
                        collect (financial-chart-markup--segment-row (car p) (cdr p) (car q) (cdr q))))
         (running (when-let* ((last (car (last pivots))) ((< (car last) (1- n))))
                    (let* ((high (equal (nth 1 last) "low"))
                           (key (if high :high :low))
                           (best (cl-loop with b = (1+ (car last)) for i from (1+ (car last)) below n
                                          when (funcall (if high #'> #'<) (plist-get (aref bars i) key)
                                                        (plist-get (aref bars b) key))
                                          do (setq b i) finally return b)))
                      (financial-chart-markup--segment-row
                       (financial-chart-markup--bar-x ctx (car last)) (nth 2 last)
                       (financial-chart-markup--bar-x ctx best) (plist-get (aref bars best) key)))))
         (labels (when (financial-chart-series-get a :labels)
                   (cl-loop for p in pivots for i from 0
                            for prev = (cl-find (nth 1 p) (seq-take pivots i) :key #'cadr :test #'equal :from-end t)
                            when prev
                            collect (list :time (financial-chart-markup--bar-x ctx (car p)) :y (nth 2 p)
                                          :label (concat (if (> (nth 2 p) (nth 2 prev)) "H" "L")
                                                         (if (equal (nth 1 p) "high") "H" "L")))))))
    (append
     (when rows
       (list (financial-chart-markup--segments ctx name rows colour (financial-chart-series-get a :dash)
                                               (or (financial-chart-series-get a :width) 1.5))))
     (when running
       (list (financial-chart-markup--segments ctx (concat name "-running") (list running) colour [3 3] 1)))
     (financial-chart-markup--labels ctx (concat name "-labels") labels colour
                                     :baseline "bottom" :dy -4))))

(defun financial-chart-markup--extend (ctx p1 p2 extend)
  "The segment P1-P2 ((X . Y) each) extended per EXTEND in CTX, as a row."
  (let* ((xs (financial-chart-shift-xs ctx))
         (x1 (if (member extend '("left" "both")) (aref xs 0) (car p1)))
         (x2 (if (member extend '("right" "both")) (aref xs (1- (length xs))) (car p2))))
    (financial-chart-markup--segment-row x1 (financial-chart-markup--line p1 p2 x1)
                                         x2 (financial-chart-markup--line p1 p2 x2))))

(defun financial-chart-markup--auto-trendlines (ctx a name path)
  "Layers of the auto-trendlines annotation A named NAME at PATH in CTX.
A view of trend.auto-lines: the newest support and resistance line."
  (let* ((kind (financial-chart-markup--choice a :kind path '("both" "support" "resistance")))
         (extend (financial-chart-markup--choice a :extend path '("right" "none" "left" "both")))
         (events (financial-chart-markup--events ctx a path "trend.auto-lines" '(:left :right)))
         (table (financial-chart-markup--index-table ctx)))
    (cl-loop for (which key) in '(("support" :up) ("resistance" :down))
             for e = (car (last (seq-filter (lambda (e) (equal (plist-get (plist-get e :evidence) :side) which))
                                            events)))
             for colour = (or (financial-chart-series-get a :color) (plist-get ctx key))
             for layer = (format "%s-%s" name which)
             when (and e (member kind (list "both" which)))
             append (pcase-let* ((`(,p1 ,p2) (mapcar (lambda (p) (financial-chart-markup--event-point ctx table p))
                                                     (plist-get (plist-get e :geometry) :points)))
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
                  (let ((first (car (financial-chart-markup--events ctx a path "swing.zigzag" '(:deviation :atr)))))
                    (if first (plist-get first :index) 0))))
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

(defun financial-chart-markup--channel (ctx a name path)
  "Layers of the channel annotation A named NAME at PATH in CTX."
  (pcase-let* ((given (or (financial-chart-series-get a :from) (financial-chart-series-get a :to)))
               (`(,p1 ,p2 ,offset)
                (if (not given)
                    ;; The newest channel.regression event: the channel as of the last bar.
                    (let* ((e (car (last (financial-chart-markup--events ctx a path "channel.regression" '(:window)))))
                           (table (financial-chart-markup--index-table ctx))
                           (pts (mapcar (lambda (p) (financial-chart-markup--event-point ctx table p))
                                        (plist-get (plist-get e :geometry) :points))))
                      (unless e
                        (financial-chart-series-fail (concat path "/window") "INVALID_ANNOTATION"
                                                     "Too few bars for a regression channel"))
                      (list (nth 0 pts) (nth 1 pts) (- (cdr (nth 2 pts)) (cdr (nth 0 pts)))))
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

(defun financial-chart-markup--events (ctx a path detector keys)
  "DETECTOR's events over CTX's bars with A's KEYS as its parameters.
A bad parameter is INVALID_ANNOTATION at PATH/KEY."
  (let ((params (cl-loop for k in keys
                         when (plist-member a k) append (list k (plist-get a k)))))
    (condition-case err
        (financial-chart-detect detector (plist-get ctx :bars) params)
      (financial-chart-invalid-detection
       (financial-chart-series-fail (concat path (string-remove-prefix "/params" (or (plist-get (cddr err) :path) "")))
                                    "INVALID_ANNOTATION" "%s" (cadr err))))))

(defun financial-chart-markup--index-table (ctx)
  "A hash from event times of CTX's bars to bar indices."
  (let ((v (vconcat (plist-get ctx :bars))) (table (make-hash-table :test #'equal)))
    (dotimes (i (length v)) (puthash (financial-chart-detector--time v i) i table))
    table))

(defun financial-chart-markup--event-point (ctx table point)
  "Event POINT [TIME Y] as (X . Y) in CTX; TABLE maps times to bars."
  (cons (financial-chart-markup--bar-x ctx (gethash (aref point 0) table)) (aref point 1)))

(defconst financial-chart-markup-detectors
  '(("support-resistance" "level.auto-sr" (:left :right :atr :tolerance))
    ("zigzag" "swing.zigzag" (:deviation :atr))
    ("auto-trendlines" "trend.auto-lines" (:left :right))
    ("fib-extension" "fib.extension" (:deviation :atr :levels) :from)
    ("pitchfork" "swing.abc" (:deviation :atr) :points)
    ("measured-move" "swing.abc" (:deviation :atr) :from)
    ("fibonacci" "fib.retracement" (:window :levels) :from)
    ("channel" "channel.regression" (:window) :from)
    ("anchored-vwap" "vwap.anchored" (:anchor :window) :at))
  "Annotation types drawn from a detector: (TYPE DETECTOR PARAM-KEYS [MANUAL-KEY]).
With MANUAL-KEY present the annotation is hand-placed instead.")

(defun financial-chart-markup--constant-event (bars a path)
  "Hand-placed annotation A at PATH as a constant event over BARS, or nil."
  (let* ((v (vconcat bars))
         (index-of (lambda (at)
                     (let ((x (and at (or (stringp at) (numberp at)) (eas-time-parse at))))
                       (cond ((and (integerp at) (null (plist-get (aref v 0) :time))) (min at (1- (length v))))
                             (x (cl-loop with best = 0 for i from 0 below (length v)
                                         when (<= (abs (- (plist-get (aref v i) :time) x))
                                                  (abs (- (plist-get (aref v best) :time) x)))
                                         do (setq best i)
                                         finally return best))))))
         (point (lambda (p) (let ((p (append p nil))) (cons (funcall index-of (car p)) (cadr p)))))
         (type (financial-chart-series-get a :type))
         (at (financial-chart-series-get a :at))
         (raw (pcase type
                ("level" (let ((from (or (funcall index-of (financial-chart-series-get a :from)) 0)))
                           (list :index from :geometry (list :y (financial-chart-series-get a :y) :from from
                                                             :to (or (funcall index-of (financial-chart-series-get a :to))
                                                                     (1- (length v)))))))
                ((or "trendline" "box" "measure")
                 (let ((p1 (funcall point (financial-chart-series-get a :from)))
                       (p2 (funcall point (financial-chart-series-get a :to))))
                   (list :index (car p1) :geometry (list :points (list p1 p2)))))
                ((or "buy" "sell" "event" "text")
                 (let ((i (funcall index-of (if (or (vectorp at) (consp at)) (elt at 0) at))))
                   (list :index i :geometry (list :at i :y (or (financial-chart-series-get a :y)
                                                               (plist-get (aref v i) :close)))))))))
    (when (and raw (plist-get raw :index))
      (let ((i (plist-get raw :index)))
        (list :id (format "constant.%s@%s" type (string-remove-prefix "/" path))
              :detector (concat "constant." type) :params nil
              :index i :time (financial-chart-detector--time v i)
              :confirmed_index i :confirmed_time (financial-chart-detector--time v i)
              :direction (pcase type ("buy" "bullish") ("sell" "bearish") (_ "neutral"))
              :strength 1.0
              :geometry (financial-chart-detector--geometry v (plist-get raw :geometry))
              :evidence (append
                         (list :label (or (financial-chart-series-get a :label) :null))
                         (when (equal type "measure")
                           (let ((pts (plist-get (plist-get raw :geometry) :points)))
                             (list :change_pct (* 100.0 (/ (- (cdr (cadr pts)) (cdr (car pts))) (cdr (car pts))))
                                   :bars (- (car (cadr pts)) (car (car pts))))))))))))

(defun financial-chart-chart-events (chart)
  "Every event a chart description's annotations show, as event/v1 data.
Detector-backed annotations (`financial-chart-markup-detectors' and
patterns) give their detector's events; hand-placed ones (level,
trendline, box, buy, sell, event, text, measure) give constant events.
CHART is a plist, JSON text or file; presets are expanded first."
  (let* ((chart (financial-chart-presets-apply (financial-chart-compose-read chart)))
         (bars (car (financial-chart-compose--bars chart)))
         (panes (cons (cons "/price" (financial-chart-series-get chart :price))
                      (seq-map-indexed (lambda (p i) (cons (format "/panes/%d" i) p))
                                       (append (financial-chart-series-get chart :panes) nil)))))
    (cl-loop
     for (ppath . pane) in panes
     append (cl-loop
             for a in (append (financial-chart-series-get pane :annotations) nil) for i from 0
             for path = (format "%s/annotations/%d" ppath i)
             for type = (financial-chart-series-get a :type)
             for spec = (assoc type financial-chart-markup-detectors)
             append
             (cond
              ((equal type "patterns")
               (let ((only (append (financial-chart-series-get a :only) nil)))
                 (cl-loop for p in (or only (financial-chart-candle-pattern-list))
                          append (financial-chart-detect (concat "pattern." p) bars
                                                         (when (financial-chart-series-get a :trend)
                                                           (list :trend (financial-chart-series-get a :trend)))))))
              ((and spec (not (and (nth 3 spec) (financial-chart-series-get a (nth 3 spec)))))
               (financial-chart-detect (nth 1 spec) bars
                                       (cl-loop for k in (nth 2 spec) when (plist-member a k)
                                                append (list k (plist-get a k)))))
              (t (delq nil (list (financial-chart-markup--constant-event bars a path)))))))))

(defun financial-chart-detect-main ()
  "Batch entry: detect from the command line and print the chart/v1 answer.
  --detector NAME --bars FILE [--params JSON] [--since N] [--limit N]
  --chart FILE   every event the chart's annotations show
FILE is a JSON array of bars or a chart with \"bars\".  Exit 1 on failure."
  (let ((args command-line-args-left) detector bars-file chart params since limit)
    (setq command-line-args-left nil)
    (while args
      (pcase (pop args)
        ("--" nil)
        ("--detector" (setq detector (pop args)))
        ("--bars" (setq bars-file (pop args)))
        ("--chart" (setq chart (pop args)))
        ("--params" (setq params (eas-json-parse (pop args))))
        ("--since" (let ((s (pop args))) (setq since (if (string-match-p "\\`[0-9]+\\'" s) (string-to-number s) s))))
        ("--limit" (setq limit (string-to-number (pop args))))))
    (let ((answer
           (condition-case err
               (if chart
                   (let ((events (financial-chart-chart-events chart)))
                     (list :contract "chart/v1" :ok t :data (list :events (vconcat events))
                           :cost (list :events (length events))))
                 (let* ((data (eas-json-read-file bars-file))
                        (bars (if (financial-chart-presets--object-p data) (plist-get data :bars) data))
                        (bars (car (financial-chart-compose--bars (list :bars bars)))))
                   (financial-chart-detect-verb detector bars params since limit)))
             (error (list :contract "chart/v1" :ok :false
                          :reason (or (plist-get (cddr err) :code) (symbol-name (car err)))
                          :message (if (stringp (cadr err)) (cadr err) (error-message-string err))
                          :evidence (list :path (or (plist-get (cddr err) :path) :null)))))))
      (princ (eas-json-encode answer))
      (terpri)
      (unless (eq (plist-get answer :ok) t) (kill-emacs 1)))))

(defun financial-chart-check-chart (chart)
  "Validate CHART (a plist, JSON text or file) before computing anything.
Return the chart/v1 envelope: the bars' id and problems
\(`financial-chart-check-bars'), presets that do not expand, unknown
annotation types and bad detector parameters, each with its JSON path."
  (let (problems bars expanded)
    (condition-case err
        (let ((c (financial-chart-compose-read chart)))
          (setq bars (append (financial-chart-series-get c :bars) nil))
          (dolist (p (financial-chart-check-bars bars))
            (push (list :path (format "/bars/%d/%s" (plist-get p :index) (plist-get p :field))
                        :code "INVALID_BAR" :problem (plist-get p :problem))
                  problems))
          (setq expanded (financial-chart-presets-apply c)))
      (financial-chart-invalid-chart
       (push (list :path (plist-get (cddr err) :path) :code (plist-get (cddr err) :code) :problem (cadr err))
             problems)))
    (when expanded
      (cl-loop
       for (ppath . pane) in (cons (cons "/price" (financial-chart-series-get expanded :price))
                                   (seq-map-indexed (lambda (p i) (cons (format "/panes/%d" i) p))
                                                    (append (financial-chart-series-get expanded :panes) nil)))
       do (cl-loop
           for a in (append (financial-chart-series-get pane :annotations) nil) for i from 0
           for path = (format "%s/annotations/%d" ppath i)
           for type = (financial-chart-series-get a :type)
           for spec = (assoc type financial-chart-markup-detectors)
           do (cond
               ((not (member type financial-chart-annotation-types))
                (push (list :path (concat path "/type") :code "UNKNOWN_ANNOTATION"
                            :problem (format "Unknown annotation type %S" type))
                      problems))
               ((and spec (not (and (nth 3 spec) (financial-chart-series-get a (nth 3 spec)))))
                (condition-case err
                    (financial-chart-detector-params
                     (nth 1 spec) (cl-loop for k in (nth 2 spec) when (plist-member a k) append (list k (plist-get a k))))
                  (financial-chart-invalid-detection
                   (push (list :path (concat path (string-remove-prefix "/params" (plist-get (cddr err) :path)))
                               :code "INVALID_ANNOTATION" :problem (cadr err))
                         problems))))))))
    (setq problems (nreverse problems))
    (append (list :contract "chart/v1" :ok (if problems :false t))
            (when problems (list :reason (plist-get (car problems) :code)
                                 :evidence (list :path (plist-get (car problems) :path))))
            (list :data (list :bars-id (if bars (financial-chart-bars-id bars) :null) :n (length bars)
                              :problems (vconcat problems))
                  :next (if problems ["(financial-chart-expand CHART)"]
                          ["(financial-chart-compose CHART)" "(financial-chart-summarize CHART)"])))))

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
