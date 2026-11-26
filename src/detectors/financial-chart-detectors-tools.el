;;; financial-chart-detectors-tools.el --- regression channel and anchored VWAP detectors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The drawing tools that are computed from the bars, as detectors
;; (fc-gty.14):
;;
;;   channel.regression  the least-squares channel of the last WINDOW
;;                       closes as of each bar (the newest event is the
;;                       channel as of the last bar)
;;   vwap.anchored       closes crossing the VWAP anchored at the lowest
;;                       low (or highest high, or first bar) so far
;;
;; The measure tool is hand-placed; `financial-chart-chart-events' gives
;; it as a constant event with its change, percent and bars as evidence.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-detector-api)
(require 'financial-chart-detectors-swings)

(defsubst financial-chart-tools--f (v i k) (plist-get (aref v i) k))

(defun financial-chart-tools--channel (v start end)
  "The least-squares channel of V's closes over bars START..END.
Return (LOW-START LOW-END HIGH-START HIGH-END SLOPE): the line through
the closes moved down and up by the largest distance of a low or high
from it, at START and END, and its slope per bar."
  (let* ((m (float (1+ (- end start))))
         (xm (/ (cl-loop for i from start to end sum i) m))
         (ym (/ (cl-loop for i from start to end sum (financial-chart-tools--f v i :close)) m))
         (sxx (cl-loop for i from start to end sum (expt (- i xm) 2)))
         (slope (if (zerop sxx) 0.0
                  (/ (cl-loop for i from start to end
                              sum (* (- i xm) (- (financial-chart-tools--f v i :close) ym)))
                     sxx)))
         (at (lambda (i) (+ ym (* slope (- i xm)))))
         (off (cl-loop for i from start to end
                       maximize (max (- (financial-chart-tools--f v i :high) (funcall at i))
                                     (- (funcall at i) (financial-chart-tools--f v i :low))))))
    (list (- (funcall at start) off) (- (funcall at end) off)
          (+ (funcall at start) off) (+ (funcall at end) off) slope)))

(defun financial-chart-tools--channels (v p)
  "Raw channel.regression events of V with params P: one per bar from the
first full window (every bar so far when WINDOW is null, from bar 1)."
  (let ((window (plist-get p :window)) events)
    (cl-loop
     for i from (if window (1- window) 1) below (length v)
     for start = (if window (- i window -1) 0)
     do (pcase-let* ((`(,l0 ,l1 ,h0 ,h1 ,slope) (financial-chart-tools--channel v start i))
                     (unit (financial-chart-swing--range v start i))
                     (width (/ (- h1 l1) unit))
                     (pos (if (> h1 l1) (/ (- (financial-chart-tools--f v i :close) l1) (- h1 l1)) 0.5)))
          (push (list :index i :confirmed i
                      :direction (cond ((> slope 0) 'bullish) ((< slope 0) 'bearish) (t 'neutral))
                      :strength (financial-chart-swing--clamp (* 10 (/ (abs slope) unit)))
                      :geometry (list :points (list (cons start l0) (cons i l1) (cons start h0) (cons i h1)))
                      :evidence (list :slope (/ slope unit) :width width :position pos))
                events)))
    (nreverse events)))

(financial-chart-register-detector
 'channel.regression #'financial-chart-tools--channels
 :kind 'trend :direction 'both
 :params '((:name window :type integer :default nil :min 2 :doc "bars of the channel; null: every bar so far"))
 :lag 0 :lookback 1 :needs '(high low close)
 :definition "Linear regression channel (TradingView \"Regression Trend\" drawing tool; Raff, G. 1991, \"Trading the Regression Channel\", Technical Analysis of Stocks & Commodities 9(1)): the least-squares line of the last WINDOW closes, moved up and down by the largest distance of a high or low from it.  An event per bar; geometry points are the lower line's two ends then the upper's.  Evidence: slope and width in average ranges, the close's position in the channel (0 lower, 1 upper).  Strength: 10 x |slope| in ranges per bar, clamped."
 :invariance '(scale shift)
 :mark '(:type "channel")
 :example "examples/indicators/channel.json"
 :doc "the regression channel as of each bar")

(defun financial-chart-tools--anchored-crosses (v p)
  "Raw vwap.anchored events of V with params P: closes crossing the VWAP
anchored at the current anchor."
  (let* ((anchor-kind (plist-get p :anchor)) (window (plist-get p :window))
         (anchor nil) (pv 0.0) (vol 0.0) (prev-side nil) events)
    (cl-loop
     for i from 0 below (length v)
     for start = (if window (max 0 (- i window -1)) 0)
     do (let ((best (pcase anchor-kind
                      ("first" start)
                      (_ (let ((key (if (equal anchor-kind "high") :high :low))
                               (better (if (equal anchor-kind "high") #'> #'<)))
                           (if (and anchor (>= anchor start)
                                    (not (funcall better (financial-chart-tools--f v i key)
                                                  (financial-chart-tools--f v anchor key))))
                               anchor
                             (cl-loop with b = start for j from start to i
                                      when (funcall better (financial-chart-tools--f v j key)
                                                    (financial-chart-tools--f v b key))
                                      do (setq b j) finally return b)))))))
          (unless (eql best anchor)
            ;; A new anchor restarts the VWAP from it.
            (setq anchor best pv 0.0 vol 0.0 prev-side nil)
            (cl-loop for j from anchor below i
                     for w = (or (financial-chart-tools--f v j :volume) 0)
                     do (cl-incf pv (* w (/ (+ (financial-chart-tools--f v j :high) (financial-chart-tools--f v j :low)
                                               (financial-chart-tools--f v j :close))
                                            3.0)))
                     do (cl-incf vol w)))
          (let ((w (or (financial-chart-tools--f v i :volume) 0)))
            (cl-incf pv (* w (/ (+ (financial-chart-tools--f v i :high) (financial-chart-tools--f v i :low)
                                   (financial-chart-tools--f v i :close))
                                3.0)))
            (cl-incf vol w))
          (when (> vol 0)
            (let* ((vwap (/ pv vol)) (c (financial-chart-tools--f v i :close))
                   (side (cond ((> c vwap) 'above) ((< c vwap) 'below))))
              (when (and side prev-side (not (eq side prev-side)))
                (let ((unit (financial-chart-swing--range v (max 0 (- i 9)) i)))
                  (push (list :index i :confirmed i :direction (if (eq side 'above) 'bullish 'bearish)
                              :strength (financial-chart-swing--clamp (/ (abs (- c vwap)) unit))
                              :geometry (list :at i :y vwap)
                              :evidence (list :distance (/ (- c vwap) unit) :anchor_bars_ago (- i anchor)))
                        events)))
              (when side (setq prev-side side))))))
    (nreverse events)))

(financial-chart-register-detector
 'vwap.anchored #'financial-chart-tools--anchored-crosses
 :kind 'trend :direction 'both
 :params '((:name anchor :type choice :choices ("low" "high" "first") :default "low"
                  :doc "anchor at the lowest low, the highest high or the first bar of the window so far")
           (:name window :type integer :default nil :min 2 :doc "bars searched for the anchor; null: every bar so far"))
 :lag 0 :lookback 1 :needs '(high low close volume)
 :definition "Anchored VWAP cross (Shannon, B. 2023, \"Maximum Trading Gains with Anchored VWAP\"; TradingView \"Anchored VWAP\" drawing tool): the volume-weighted typical price from the anchor bar, re-anchored when a lower low (or higher high) appears; an event on each close that crosses it.  Bullish crossing up.  Evidence: the close's distance from the VWAP in average ranges and the anchor's age in bars.  Strength: that distance, clamped."
 :invariance '(scale shift)
 :mark '(:type "marker")
 :example "examples/indicators/anchored-vwap.json"
 :doc "closes crossing the anchored VWAP")

(provide 'financial-chart-detectors-tools)
;;; financial-chart-detectors-tools.el ends here
