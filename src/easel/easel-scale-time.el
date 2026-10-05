;;; easel-scale-time.el --- UTC time ticks and Vega's multi-format labels -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A port of d3-time's tick interval choice (utc variants) and
;; vega-format's timeMultiFormat, so time axes tick where Vega's do.
;; Weekday and month names are English (C locale), as in Vega.

;;; Code:

(require 'easel-time)

(defconst easel-scale-time--intervals
  `((second 1 1000) (second 5 5000) (second 15 15000) (second 30 30000)
    (minute 1 60000) (minute 5 300000) (minute 15 900000) (minute 30 1800000)
    (hour 1 3600000) (hour 3 10800000) (hour 6 21600000) (hour 12 43200000)
    (day 1 86400000) (day 2 172800000)
    (week 1 604800000)
    (month 1 2592000000) (month 3 7776000000)
    (year 1 31536000000))
  "d3-time tickIntervals: (UNIT STEP APPROX-DURATION-MS).")

(defun easel-scale-time--floor (unit ms)
  "Floor epoch MS to the start of UNIT (UTC)."
  (let ((f (easel-time-fields ms)))
    (pcase unit
      ('millisecond ms)
      ('second (* 1000 (floor ms 1000)))
      ('minute (* 60000 (floor ms 60000)))
      ('hour (* 3600000 (floor ms 3600000)))
      ('day (* 86400000 (floor ms 86400000)))
      ('week (- (* 86400000 (floor ms 86400000)) (* 86400000 (plist-get f :weekday))))
      ('month (easel-time-ms (plist-get f :year) (plist-get f :month) 1))
      ('year (easel-time-ms (plist-get f :year) 1 1)))))

(defun easel-scale-time--offset (unit ms)
  "Advance MS by one UNIT."
  (let ((f (easel-time-fields ms)))
    (pcase unit
      ('millisecond (1+ ms)) ('second (+ ms 1000)) ('minute (+ ms 60000))
      ('hour (+ ms 3600000)) ('day (+ ms 86400000)) ('week (+ ms 604800000))
      ('month (easel-time-ms (plist-get f :year) (1+ (plist-get f :month)) (plist-get f :day)
                             (plist-get f :hours) (plist-get f :minutes)))
      ('year (easel-time-ms (1+ (plist-get f :year)) (plist-get f :month) (plist-get f :day))))))

(defun easel-scale-time--field (unit ms)
  "The field d3's interval.every(step) filters on for UNIT at MS."
  (let ((f (easel-time-fields ms)))
    (pcase unit
      ('millisecond ms) ('second (plist-get f :seconds)) ('minute (plist-get f :minutes))
      ('hour (plist-get f :hours)) ('day (1- (plist-get f :day))) ('week 0)
      ('month (1- (plist-get f :month))) ('year (plist-get f :year)))))

(defun easel-scale-time--interval (start stop count)
  "Return (UNIT . STEP), d3's tickInterval for START..STOP and COUNT."
  (let* ((target (/ (abs (- stop start)) (float count)))
         (i (or (seq-position easel-scale-time--intervals target
                              (lambda (iv tg) (> (nth 2 iv) tg)))
                (length easel-scale-time--intervals))))
    (cond
     ((= i (length easel-scale-time--intervals))
      (cons 'year (max 1 (round (easel-scale-tick-increment-years start stop count)))))
     ((= i 0) (cons 'millisecond (max 1 (round (/ (- stop start) (float count))))))
     (t (let* ((prev (nth (1- i) easel-scale-time--intervals))
               (next (nth i easel-scale-time--intervals))
               (pick (if (< (/ target (nth 2 prev)) (/ (nth 2 next) target)) prev next)))
          (cons (nth 0 pick) (nth 1 pick)))))))

(declare-function easel-scale-tick-increment "easel-scale" (start stop count))

(defun easel-scale-tick-increment-years (start stop count)
  "d3.tickStep over START..STOP measured in years, for year intervals."
  (let ((inc (easel-scale-tick-increment (/ start 31536000000.0) (/ stop 31536000000.0) count)))
    (if (< inc 0) (/ 1.0 (- inc)) inc)))

(defun easel-scale-time-ticks (start stop count)
  "Return UTC tick times (epoch ms) between START and STOP, about COUNT."
  (let* ((reverse (< stop start))
         (lo (min start stop)) (hi (max start stop))
         (interval (easel-scale-time--interval lo hi (max 1 count)))
         (unit (car interval)) (step (cdr interval))
         (tick (easel-scale-time--floor unit lo))
         ticks)
    (when (< tick lo) (setq tick (easel-scale-time--offset unit tick)))
    (while (<= tick hi)
      (when (zerop (mod (easel-scale-time--field unit tick) step))
        (push tick ticks))
      (setq tick (easel-scale-time--offset unit tick)))
    (if reverse ticks (nreverse ticks))))

(defun easel-scale-time-multi-format (ms)
  "Format epoch MS the way Vega's default time axis does."
  (let* ((f (easel-time-fields ms))
         (system-time-locale "C")
         (fmt (cond
               ((> (plist-get f :milliseconds) 0) ".%3N")
               ((> (plist-get f :seconds) 0) ":%S")
               ((> (plist-get f :minutes) 0) "%I:%M")
               ((> (plist-get f :hours) 0) "%I %p")
               ((> (plist-get f :day) 1) (if (zerop (plist-get f :weekday)) "%b %d" "%a %d"))
               ((> (plist-get f :month) 1) "%B")
               (t "%Y"))))
    (easel-time-format ms fmt)))

(provide 'easel-scale-time)
;;; easel-scale-time.el ends here
