;;; financial-chart-eas-trading-time.el --- a trading-time x axis for composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Trading charts leave out the time nobody traded: no gap between
;; Friday's candle and Monday's, none overnight between intraday bars,
;; none for a holiday.  On a trading-time axis (fc-gbo.5, the default of
;; a composed chart whose bars have times) each bar sits at its ordinal,
;; 0 to N-1, one slot apart, and the axis still reads as dates: ticks
;; fall on the bars that open a new minute block, hour, day, week,
;; month, quarter or year, whichever is the finest unit giving at most
;; `financial-chart-trading-max-ticks' of them, labelled with that date
;; (a coarser change, such as a new year, labels the coarser unit).
;;
;; "x": "calendar" (or {"scale": "calendar"}) keeps calendar time:
;; epoch ms on a temporal axis, gaps and all.  {"scale": "trading",
;; "ticks": N} caps the ticks at N.
;;
;; Dates given elsewhere (annotations) map to a bar: the bar at that
;; time, else the nearest one (an event on a weekend or holiday lands
;; on the closest session).  Forward shifts extend the ordinals by N
;; slots, labelled with the weekdays (or bar spacing) after the last
;; bar.
;;
;; Pure: times in, ordinals, ticks and axis plists out.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-shift)

(defvar financial-chart-trading-max-ticks 8
  "Most date ticks a trading-time axis draws unless the chart says.")

(defconst financial-chart-trading-scales '("trading" "calendar")
  "The x scales of a composed chart with bar times.")

;;; The chart's choice

(defun financial-chart-trading-options (chart)
  "CHART's \"x\" as (SCALE . MAX-TICKS); signal at /x when invalid.
\"x\" is \"trading\", \"calendar\" or {\"scale\", \"ticks\"}; SCALE
defaults to trading and MAX-TICKS to `financial-chart-trading-max-ticks'."
  (let* ((x (financial-chart-series-get chart :x))
         (object (and (listp x) (keywordp (car x))))
         (scale (cond ((null x) "trading") ((stringp x) x)
                      (object (or (financial-chart-series-get x :scale) "trading"))))
         (ticks (or (and object (financial-chart-series-get x :ticks))
                    financial-chart-trading-max-ticks)))
    (unless (member scale financial-chart-trading-scales)
      (financial-chart-series-fail (if object "/x/scale" "/x") "INVALID_X"
                                   "x is \"trading\" (bar slots, no gaps) or \"calendar\" (dates to scale), or {\"scale\", \"ticks\"}; got %S"
                                   (or scale x)))
    (unless (and (integerp ticks) (> ticks 0))
      (financial-chart-series-fail "/x/ticks" "INVALID_X" "x ticks is a positive count, got %S" ticks))
    (cons scale ticks)))

;;; Ticks

(defun financial-chart-trading--decode (ms)
  "Epoch MS decoded in UTC."
  (decode-time (floor ms 1000) t))

(defun financial-chart-trading--day (ms)
  "Days from the epoch to epoch MS (UTC)."
  (floor ms 86400000))

(defun financial-chart-trading--month (ms)
  "Months from year 0 to epoch MS (UTC)."
  (let ((d (financial-chart-trading--decode ms)))
    (+ (* 12 (decoded-time-year d)) (1- (decoded-time-month d)))))

(defconst financial-chart-trading-units
  ;; (NAME SPAN-MS KEY): KEY maps epoch ms to the unit it falls in.
  `(("5 minutes" 300000 ,(lambda (ms) (floor ms 300000)))
    ("15 minutes" 900000 ,(lambda (ms) (floor ms 900000)))
    ("30 minutes" 1800000 ,(lambda (ms) (floor ms 1800000)))
    ("hour" 3600000 ,(lambda (ms) (floor ms 3600000)))
    ("day" 86400000 financial-chart-trading--day)
    ;; Weeks start on Monday; the epoch was a Thursday.
    ("week" ,(* 7 86400000) ,(lambda (ms) (floor (+ (financial-chart-trading--day ms) 3) 7)))
    ("month" ,(* 28 86400000) financial-chart-trading--month)
    ("quarter" ,(* 90 86400000) ,(lambda (ms) (floor (financial-chart-trading--month ms) 3)))
    ,@(mapcar (lambda (n)
                (list (if (= n 1) "year" (format "%d years" n)) (* n 365 86400000)
                      (lambda (ms) (floor (decoded-time-year (financial-chart-trading--decode ms)) n))))
              '(1 2 5 10 20 50 100)))
  "Tick units of a trading-time axis, finest first.")

(defun financial-chart-trading--boundaries (times key)
  "Indices of TIMES (a vector) whose KEY differs from the previous one's."
  (cl-loop for i from 1 below (length times)
           unless (equal (funcall key (aref times i)) (funcall key (aref times (1- i))))
           collect i))

(defun financial-chart-trading--median-gap (times)
  "The median gap between consecutive TIMES (a vector), or 0."
  (let ((gaps (sort (cl-loop for i from 1 below (length times)
                             collect (- (aref times i) (aref times (1- i))))
                    #'<)))
    (if gaps (nth (/ (length gaps) 2) gaps) 0)))

(defun financial-chart-trading--label (ms prev unit)
  "The tick label of epoch MS on UNIT ticks; PREV is the bar before it, or nil."
  (let* ((d (financial-chart-trading--decode ms))
         (new-year (and prev (/= (decoded-time-year d)
                                 (decoded-time-year (financial-chart-trading--decode prev)))))
         (new-day (or (null prev)
                      (/= (financial-chart-trading--day ms) (financial-chart-trading--day prev))))
         (year (format "%d" (decoded-time-year d)))
         (month (format-time-string "%b" (floor ms 1000) t))
         (day (format "%s %d" month (decoded-time-day d))))
    (pcase unit
      ((or "5 minutes" "15 minutes" "30 minutes" "hour")
       (if new-day day (format-time-string "%H:%M" (floor ms 1000) t)))
      ((or "day" "week") (if new-year year day))
      ((or "month" "quarter") (if new-year year month))
      (_ year))))

(defun financial-chart-trading-ticks (times &optional max)
  "Date ticks of a trading-time axis over TIMES (epoch ms, a vector).
Return ((INDEX . LABEL) ...): the bars opening a new unit of the finest
`financial-chart-trading-units' entry no finer than the bar spacing
that gives at most MAX (default `financial-chart-trading-max-ticks')
ticks.  With no unit boundary among TIMES the first bar is the tick."
  (let* ((max (or max financial-chart-trading-max-ticks))
         (gap (financial-chart-trading--median-gap times))
         (pick (or (cl-loop for (name span key) in financial-chart-trading-units
                            for ticks = (and (>= span gap) (financial-chart-trading--boundaries times key))
                            when (and ticks (<= (length ticks) max)) return (cons name ticks))
                   (let ((coarsest (car (last financial-chart-trading-units))))
                     (cons (car coarsest)
                           (seq-take (financial-chart-trading--boundaries times (nth 2 coarsest)) max))))))
    (if (and (null (cdr pick)) (> (length times) 0))
        (list (cons 0 (financial-chart-trading--label (aref times 0) nil "day")))
      (mapcar (lambda (i) (cons i (financial-chart-trading--label
                                   (aref times i) (aref times (1- i)) (car pick))))
              (cdr pick)))))

(defun financial-chart-trading-axis (ticks)
  "Axis properties drawing TICKS ((INDEX . LABEL) ...) on bar ordinals."
  (list :values (vconcat (mapcar #'car ticks))
        :labelExpr (concat (mapconcat (lambda (tk) (format "datum.value == %d ? '%s' : "
                                                           (car tk) (cdr tk)))
                                      ticks "")
                           "''")))

;;; The context

(defun financial-chart-trading-context (ctx)
  "CTX with its trading-time axis, when it has bar :times.
Adds :times-ext (the times of every slot, the forward-shift slots
continuing the bar spacing), :x-axis (date ticks) and :x-scale (one
half slot of padding either side).  Without :times, CTX as is."
  (if-let* ((times (plist-get ctx :times)))
      (let* ((slots (length (financial-chart-shift-xs ctx)))
             (times-ext (vconcat times (financial-chart-shift-future-xs
                                        times "temporal" (- slots (length times))))))
        (append (list :times-ext times-ext
                      :x-axis (financial-chart-trading-axis
                               (financial-chart-trading-ticks times-ext (plist-get ctx :max-ticks)))
                      :x-scale (list :domain (vector -0.5 (- slots 0.5)) :nice :false :zero :false))
                ctx))
    ctx))

;;; Dates to bars

(defun financial-chart-trading-index (times ms)
  "The index of the bar of TIMES (a sorted vector) at or nearest epoch MS.
A tie goes to the later bar.  Return nil when MS lies outside TIMES."
  (let ((n (length times)))
    (when (and (> n 0) (<= (aref times 0) ms) (<= ms (aref times (1- n))))
      (let ((lo 0) (hi (1- n)))
        ;; The first bar at or after MS.
        (while (< lo hi)
          (let ((mid (/ (+ lo hi) 2)))
            (if (< (aref times mid) ms) (setq lo (1+ mid)) (setq hi mid))))
        (if (and (> lo 0) (< (- ms (aref times (1- lo))) (- (aref times lo) ms)))
            (1- lo)
          lo)))))

(provide 'financial-chart-eas-trading-time)
;;; financial-chart-eas-trading-time.el ends here
