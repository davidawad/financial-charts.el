;;; financial-chart-eas-shift.el --- shifted series and bars past the last one -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A series of a composed chart may be shifted: "shift": 26 draws each
;; value 26 bars after the bar it was computed on (Ichimoku's senkou
;; spans, a displaced moving average), -26 draws it 26 bars earlier
;; (chikou).  A forward shift runs past the last bar, so the chart grows
;; future x positions: the bar spacing continued (weekdays only when the
;; bars are daily and skip weekends).  Shifted values beyond the last
;; bar have no row of their own; their layer carries its own data.
;;
;; Pure: series and x positions in, series and x positions out.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun financial-chart-shift-values (values shift length)
  "VALUES (a vector) moved SHIFT places later in a vector of LENGTH.
Positions with no source value are nil."
  (let ((out (make-vector length nil)))
    (cl-loop for v across values for i from 0
             for j = (+ i shift)
             when (and v (>= j 0) (< j length)) do (aset out j v))
    out))

(defun financial-chart-shift-extent (series)
  "Bars past the last bar that SERIES (finished series plists) need."
  (cl-reduce #'max series :key (lambda (s) (max 0 (or (plist-get s :shift) 0))) :initial-value 0))

(defun financial-chart-shift-series (series n)
  "SERIES with each :shift applied over N bars.
Every series' :values grows to N plus `financial-chart-shift-extent';
a series with a value past bar N is marked :extended."
  (let ((length (+ n (financial-chart-shift-extent series))))
    (mapcar (lambda (s)
              (let* ((values (financial-chart-shift-values
                              (plist-get s :values) (or (plist-get s :shift) 0) length))
                     (extended (cl-loop for i from n below length thereis (aref values i))))
                (append (list :values values :extended extended) s)))
            series)))

(defun financial-chart-shift--median-gap (xs)
  "The median gap between consecutive XS (a vector), or 1."
  (let ((gaps (sort (cl-loop for i from 1 below (length xs) collect (- (aref xs i) (aref xs (1- i)))) #'<)))
    (if gaps (nth (/ (length gaps) 2) gaps) 1)))

(defun financial-chart-shift--weekend-p (ms)
  "Non-nil when epoch MS falls on a Saturday or Sunday (UTC)."
  (memq (decoded-time-weekday (decode-time (floor ms 1000) t)) '(0 6)))

(defun financial-chart-shift-future-xs (xs x-type count)
  "COUNT x positions after XS (a vector) on an X-TYPE axis.
They continue the median bar spacing.  Daily temporal bars that never
fall on a weekend continue on weekdays only."
  (when (> count 0)
    (let* ((gap (financial-chart-shift--median-gap xs))
           (last (if (> (length xs) 0) (aref xs (1- (length xs))) -1))
           (weekdays (and (equal x-type "temporal") (= gap 86400000)
                          (not (seq-some #'financial-chart-shift--weekend-p xs))))
           out)
      (while (< (length out) count)
        (setq last (+ last gap))
        (unless (and weekdays (financial-chart-shift--weekend-p last))
          (push last out)))
      (nreverse out))))

(defun financial-chart-shift-context (ctx series)
  "CTX with :xs-ext, its x positions grown as far as SERIES draw.
SERIES come from `financial-chart-shift-series'; only values past the
last bar count, so a span too short to reach past it adds nothing."
  (let* ((xs (plist-get ctx :xs))
         (n (length xs))
         (count (cl-loop for s in series
                         maximize (let ((values (plist-get s :values)))
                                    (or (cl-loop for i from (1- (length values)) downto n
                                                 when (aref values i) return (1+ (- i n)))
                                        0)))))
    (append (list :xs-ext (vconcat xs (financial-chart-shift-future-xs xs (plist-get ctx :x-type) (or count 0))))
            ctx)))

(defun financial-chart-shift-xs (ctx)
  "CTX's x positions, grown past the last bar when a series is shifted."
  (or (plist-get ctx :xs-ext) (plist-get ctx :xs)))

(defun financial-chart-shift-layer-data (ctx s)
  "The own :data of series S in CTX when it runs past the last bar, else nil."
  (when (plist-get s :extended)
    (let ((field (intern (concat ":" (plist-get s :column)))))
      (list :data (list :values (vconcat (cl-loop for x across (financial-chart-shift-xs ctx)
                                                  for v across (plist-get s :values)
                                                  when v collect (list :time x field v))))))))

(provide 'financial-chart-eas-shift)
;;; financial-chart-eas-shift.el ends here
