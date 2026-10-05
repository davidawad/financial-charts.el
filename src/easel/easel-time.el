;;; easel-time.el --- dates as UTC epoch milliseconds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Temporal values are epoch milliseconds.  Calendar fields, formats
;; and zone-less date-time strings follow `easel-time-zone', which is
;; nil (UTC) by default so output is identical on every machine.  Bound
;; to a zone it gives Vega's local-time semantics in that zone: a
;; date-time without an offset is local, a date-only string is UTC
;; midnight (as JavaScript parses it), and fields, timeUnits, "time"
;; scale ticks and labels are local.  The conformance oracle binds it to
;; the zone bin/chart's references were built in.

;;; Code:

(require 'easel-core)

(defvar easel-time-zone nil
  "Zone for local time, or nil for UTC.
A `decode-time' ZONE such as \"America/Chicago\".")

(defconst easel-time--iso-regexp
  (concat "\\`\\([0-9]\\{4\\}\\)\\(?:[-/]\\([0-9]\\{1,2\\}\\)"
          "\\(?:[-/]\\([0-9]\\{1,2\\}\\)"
          "\\(?:[T ]\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)"
          "\\(?::\\([0-9]\\{2\\}\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?"
          "\\(Z\\|[+-][0-9]\\{2\\}:?[0-9]\\{2\\}\\)?\\)?\\)?\\)?\\'")
  "ISO-8601-ish date or date-time: YYYY[-MM[-DD[THH:MM[:SS[.fff]][zone]]]].")

(defun easel-time-days-from-civil (year month day)
  "Days since 1970-01-01 for the proleptic Gregorian YEAR MONTH DAY."
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (mp (mod (+ month 9) 12))
         (doy (+ (/ (+ (* 153 mp) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun easel-time-civil-from-days (days)
  "Return (YEAR MONTH DAY) for DAYS since 1970-01-01."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (/ (- doe (/ doe 1460) (- (/ doe 36524)) (/ doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (/ yoe 4)) (/ yoe 100))))
         (mp (/ (+ (* 5 doy) 2) 153))
         (day (1+ (- doy (/ (+ (* 153 mp) 2) 5))))
         (month (if (< mp 10) (+ mp 3) (- mp 9)))
         (year (+ yoe (* era 400) (if (<= month 2) 1 0))))
    (list year month day)))

(defun easel-time-string-p (value)
  "Non-nil when VALUE is a string `easel-time-parse' reads as a date."
  (and (stringp value) (string-match-p easel-time--iso-regexp value)
       (> (length value) 4)))

(defvar easel-time--parse-cache (make-hash-table :test 'equal)
  "Date string -> epoch ms (or :none).  A crosshair tests every row's
date against the selection on each move (fc-qx1.2), so parse once.")

(defconst easel-time--parse-cache-limit 200000
  "Entries kept before `easel-time--parse-cache' is emptied.")

(defun easel-time-parse (value)
  "Return VALUE as UTC epoch milliseconds, or nil when it is not a date.
Numbers are already epoch milliseconds."
  (cond
   ((numberp value) value)
   ((stringp value)
    (let ((hit (gethash value easel-time--parse-cache)))
      (if hit (and (not (eq hit :none)) hit)
        (when (>= (hash-table-count easel-time--parse-cache) easel-time--parse-cache-limit)
          (clrhash easel-time--parse-cache))
        (let ((ms (easel-time--parse-string value)))
          (puthash value (or ms :none) easel-time--parse-cache)
          ms))))))

(defun easel-time--parse-string (value)
  "Parse string VALUE as `easel-time-parse' does, uncached."
  (cond
   ((string-match easel-time--iso-regexp value)
    (let* ((num (lambda (n default)
                  (if (match-string n value) (string-to-number (match-string n value)) default)))
           (year (funcall num 1 1970)) (month (funcall num 2 1)) (day (funcall num 3 1))
           (hour (funcall num 4 0)) (minute (funcall num 5 0)) (sec (funcall num 6 0))
           (frac (match-string 7 value))
           (zone (match-string 8 value))
           (ms (if frac (round (* 1000 (string-to-number (concat "0." frac)))) 0))
           (local (and easel-time-zone (match-string 4 value) (null zone)))
           (offset (if (and zone (not (equal zone "Z")))
                       (let ((sign (if (eq (aref zone 0) ?-) -1 1))
                             (digits (replace-regexp-in-string ":" "" (substring zone 1))))
                         (* sign (+ (* 60 (string-to-number (substring digits 0 2)))
                                    (string-to-number (substring digits 2)))))
                     0)))
      (if local (easel-time-ms year month day hour minute sec ms)
        (+ (* 1000 (+ (* 86400 (easel-time-days-from-civil year month day))
                      (* 3600 hour) (* 60 (- minute offset)) sec))
           ms))))))

(defun easel-time-fields (ms)
  "Return the calendar fields of epoch MS in `easel-time-zone' as a plist.
Keys are :year :month (1-12) :day :hours :minutes :seconds
:milliseconds and :weekday (0 is Sunday)."
  (if easel-time-zone
      (let* ((ms (floor ms)) (s (floor ms 1000)) (d (decode-time s easel-time-zone)))
        (list :year (decoded-time-year d) :month (decoded-time-month d) :day (decoded-time-day d)
              :hours (decoded-time-hour d) :minutes (decoded-time-minute d) :seconds (decoded-time-second d)
              :milliseconds (- ms (* 1000 s)) :weekday (decoded-time-weekday d)))
    (easel-time--utc-fields ms)))

(defun easel-time--utc-fields (ms)
  "The UTC calendar fields of epoch MS (see `easel-time-fields')."
  (let* ((ms (floor ms))
         (days (floor ms 86400000))
         (rem (- ms (* days 86400000)))
         (civil (easel-time-civil-from-days days)))
    (list :year (nth 0 civil) :month (nth 1 civil) :day (nth 2 civil)
          :hours (/ rem 3600000) :minutes (% (/ rem 60000) 60)
          :seconds (% (/ rem 1000) 60) :milliseconds (% rem 1000)
          :weekday (mod (+ 4 days) 7))))

(defun easel-time-ms (year &optional month day hours minutes seconds milliseconds)
  "Return epoch milliseconds for calendar fields in `easel-time-zone'.
MONTH (1-12), DAY and the clock fields may overflow; they are normalized."
  (if easel-time-zone
      (+ (* 1000 (time-convert (encode-time (list (or seconds 0) (or minutes 0) (or hours 0) (or day 1)
                                                  (or month 1) year nil -1 easel-time-zone))
                               'integer))
         (or milliseconds 0))
    (easel-time--utc-ms year month day hours minutes seconds milliseconds)))

(defun easel-time--utc-ms (year &optional month day hours minutes seconds milliseconds)
  "UTC epoch milliseconds for calendar fields YEAR MONTH DAY HOURS
MINUTES SECONDS MILLISECONDS (see `easel-time-ms')."
  (let* ((month (or month 1))
         (y (+ year (floor (1- month) 12)))
         (m (1+ (mod (1- month) 12))))
    (+ (* 86400000 (+ (easel-time-days-from-civil y m 1) (1- (or day 1))))
       (* 3600000 (or hours 0)) (* 60000 (or minutes 0))
       (* 1000 (or seconds 0)) (or milliseconds 0))))

(defun easel-time-format (ms format-string)
  "Format epoch MS in `easel-time-zone' with `format-time-string' FORMAT-STRING."
  (format-time-string format-string (seconds-to-time (/ ms 1000.0)) (or easel-time-zone t)))

(defun easel-time-iso (ms)
  "Return MS as an ISO date (YYYY-MM-DD) or date-time string."
  (if (zerop (mod (floor ms) 86400000))
      (easel-time-format ms "%Y-%m-%d")
    (easel-time-format ms "%Y-%m-%dT%H:%M:%SZ")))

(provide 'easel-time)
;;; easel-time.el ends here
