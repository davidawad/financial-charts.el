;;; financial-chart-patterns.el --- swings, levels and candlestick patterns as plain lists -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Short answers over bar/v1 bars (plists with :open :high :low :close,
;; oldest first), read off the detectors' events (fc-gty; the events
;; themselves, with confirmation, strength and evidence, come from
;; `financial-chart-detect'):
;;
;;   `financial-chart-swing-points'       swing.pivot as (INDEX KIND PRICE)
;;   `financial-chart-zigzag'             swing.zigzag as (INDEX KIND PRICE)
;;   `financial-chart-support-resistance' level.auto-sr as of the last bar
;;   `financial-chart-candle-patterns'    pattern.* as (INDEX NAME DIRECTION)
;;   `financial-chart-anchored-vwap'      VWAP from an anchor bar (a series)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-core)
(require 'financial-chart-indicator-api)
(require 'financial-chart-detector-api)
(require 'financial-chart-detectors-candles)
(require 'financial-chart-detectors-swings)

(defun financial-chart-patterns--detect (name bars params)
  "Events of detector NAME over BARS with PARAMS.
A bad parameter signals `financial-chart-invalid-indicator'."
  (condition-case err
      (financial-chart-detect name bars params)
    (financial-chart-invalid-detection
     (signal 'financial-chart-invalid-indicator (list (cadr err))))))

(defun financial-chart-patterns--point (e)
  "Pivot event E as (INDEX KIND PRICE)."
  (list (plist-get e :index) (intern (plist-get (plist-get e :evidence) :side))
        (plist-get (plist-get e :geometry) :y)))

(defun financial-chart-swing-points (bars &optional left right)
  "Swing highs and lows of BARS as (INDEX KIND PRICE), by confirmation.
See the detector swing.pivot: a high above the LEFT (3) highs before
it and not below the RIGHT (default LEFT) after it.  KIND is `high' or
`low'."
  (let ((left (or left 3)))
    (mapcar #'financial-chart-patterns--point
            (financial-chart-patterns--detect "swing.pivot" bars (list :left left :right (or right left))))))

(defun financial-chart-zigzag (bars &optional deviation)
  "Confirmed zigzag pivots of BARS as (INDEX KIND PRICE), alternating.
See the detector swing.zigzag; DEVIATION percent defaults to 5."
  (mapcar #'financial-chart-patterns--point
          (financial-chart-patterns--detect "swing.zigzag" bars (when deviation (list :deviation deviation)))))

(cl-defun financial-chart-support-resistance (bars &key left right tolerance atr (max 4) (min-touches 1))
  "Support and resistance levels of BARS as of the last bar, strongest first.
The newest level.auto-sr event of each level (LEFT, RIGHT, TOLERANCE,
ATR as the detector's parameters) touched at least MIN-TOUCHES times,
ranked by touches then recency, MAX of them (nil for all).  Each is
\(:price :touches :kind :first :last :id): :kind `support' below the
last close, else `resistance'; :first and :last the bar indices of its
first and latest touch."
  (let* ((events (financial-chart-patterns--detect
                  "level.auto-sr" bars
                  (append (when left (list :left left)) (when (or right left) (list :right (or right left)))
                          (when tolerance (list :tolerance tolerance)) (when atr (list :atr atr)))))
         (v (vconcat bars))
         (last-close (plist-get (aref v (1- (length v))) :close))
         (latest (make-hash-table))
         (first (make-hash-table)))
    (dolist (e events)
      (let ((k (plist-get (plist-get e :evidence) :cluster)))
        (puthash k e latest)
        (unless (gethash k first) (puthash k (plist-get e :index) first))))
    (let* ((levels (cl-loop for k being the hash-keys of latest using (hash-values e)
                            for y = (plist-get (plist-get e :geometry) :y)
                            for touches = (plist-get (plist-get e :evidence) :touches)
                            when (>= touches min-touches)
                            collect (list :price y :touches touches
                                          :kind (if (< y last-close) 'support 'resistance)
                                          :first (gethash k first) :last (plist-get e :index)
                                          :id (plist-get e :id))))
           (ranked (sort levels (lambda (a b) (or (> (plist-get a :touches) (plist-get b :touches))
                                                  (and (= (plist-get a :touches) (plist-get b :touches))
                                                       (> (plist-get a :last) (plist-get b :last))))))))
      (if (and max (> (length ranked) max)) (seq-take ranked max) ranked))))

(defun financial-chart-candle-pattern-list ()
  "Names of the candlestick pattern detectors, without \"pattern.\"."
  (mapcar (lambda (n) (string-remove-prefix "pattern." n))
          (seq-filter (lambda (n) (string-prefix-p "pattern." n)) (financial-chart-detector-names))))

(defun financial-chart-candle-pattern-names ()
  "The candlestick patterns as (NAME SHORT-LABEL DIRECTION)."
  (mapcar (lambda (name)
            (let ((e (financial-chart-detector (concat "pattern." name))))
              (list name (plist-get (plist-get e :mark) :label) (plist-get e :direction))))
          (financial-chart-candle-pattern-list)))

(defun financial-chart-candle-patterns (bars &optional names params)
  "Candlestick patterns in BARS as (INDEX NAME DIRECTION), oldest first.
NAMES, a list of pattern names (`financial-chart-candle-pattern-list'),
keeps only those; PARAMS (such as (:trend 0)) go to each detector that
takes them.  DIRECTION is `bullish', `bearish' or `neutral'."
  (dolist (name names)
    (unless (member name (financial-chart-candle-pattern-list))
      (signal 'financial-chart-invalid-indicator
              (list (format "Unknown candlestick pattern %S; patterns: %s" name
                            (string-join (financial-chart-candle-pattern-list) ", "))))))
  (sort (cl-loop for name in (or names (financial-chart-candle-pattern-list))
                 for detector = (concat "pattern." name)
                 append (mapcar (lambda (e) (list (plist-get e :index) name (intern (plist-get e :direction))))
                                (financial-chart-patterns--detect
                                 detector bars
                                 (and (plist-get (financial-chart-detector detector) :params) params))))
        (lambda (a b) (< (car a) (car b)))))

;;; Anchored VWAP

(defun financial-chart-anchored-vwap (bars anchor)
  "VWAP of BARS from bar index ANCHOR on: one value per bar, nil before it.
Typical price (high+low+close)/3 weighted by volume; a bar without
volume repeats the last value (Shannon, B. 2023, \"Maximum Trading
Gains with Anchored VWAP\")."
  (unless (and (integerp anchor) (>= anchor 0))
    (signal 'financial-chart-invalid-indicator (list (format "anchor must be a bar index, got %S" anchor))))
  (let ((pv 0.0) (vol 0.0) last)
    (cl-loop for b in (append bars nil) for i from 0
             collect (when (>= i anchor)
                       (let ((v (plist-get b :volume)))
                         (when (and (numberp v) (> v 0))
                           (cl-incf pv (* v (/ (+ (plist-get b :high) (plist-get b :low) (plist-get b :close)) 3.0)))
                           (cl-incf vol v)
                           (setq last (/ pv vol)))
                         last)))))

(provide 'financial-chart-patterns)
;;; financial-chart-patterns.el ends here
