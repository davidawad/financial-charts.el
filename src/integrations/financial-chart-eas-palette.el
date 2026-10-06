;;; financial-chart-eas-palette.el --- stable series colours for composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The palette of `financial-chart-compose' (fc-gbo.2).  Each indicator
;; has a home colour, so SMA is the same blue in every chart and every
;; pane; a second, different series of the same indicator (SMA 20 then
;; SMA 50) takes the next palette colour nobody in the chart uses yet.
;; Up and down colours (candles, volume, baseline fills) are separate.
;; The assignment is a pure function of the chart's series keys in
;; order, so the same chart always gets the same colours.

;;; Code:

(require 'cl-lib)

(defgroup financial-chart-palette nil
  "Colours of composed financial charts."
  :group 'financial-chart)

(defcustom financial-chart-palette
  '("#2962ff" "#ff9800" "#9c27b0" "#00897b" "#e91e63" "#795548"
    "#00bcd4" "#827717" "#3f51b5" "#f4511e" "#607d8b" "#8bc34a")
  "Series colours of composed charts, in assignment order.
No entry should be close to `financial-chart-palette-up' or
`financial-chart-palette-down', which mean rising and falling."
  :type '(repeat string))

(defcustom financial-chart-palette-up "#26a69a"
  "Colour of rising bars, candles and the above-baseline fill."
  :type 'string)

(defcustom financial-chart-palette-down "#ef5350"
  "Colour of falling bars, candles and the below-baseline fill."
  :type 'string)

(defcustom financial-chart-palette-price "#455a64"
  "Colour of the price line, step and area styles."
  :type 'string)

(defcustom financial-chart-palette-homes
  '(("sma" . 0) ("ema" . 1) ("rsi" . 2) ("macd" . 0) ("macd-signal" . 1)
    ("macd-histogram" . 10) ("bollinger-bands" . 3) ("vwap" . 4) ("wma" . 5)
    ("stochastic" . 6) ("atr" . 7) ("keltner-channels" . 8) ("donchian-channels" . 9)
    ("obv" . 6) ("cci" . 5) ("dmi" . 8) ("aroon" . 9) ("parabolic-sar" . 4))
  "Home palette index of each indicator NAME (a string).
An indicator not listed hashes its name onto the palette, so it still
keeps one colour across charts."
  :type '(alist :key-type string :value-type integer))

(defun financial-chart-palette-home (name)
  "Home index into `financial-chart-palette' of indicator NAME (a string)."
  (mod (or (cdr (assoc name financial-chart-palette-homes))
           (cl-reduce (lambda (acc c) (mod (+ (* acc 31) c) 65521)) name :initial-value 7))
       (length financial-chart-palette)))

(defun financial-chart-palette-assign (entries)
  "Colours for ENTRIES, a list of (KEY . NAME) in chart order.
KEY identifies one series (an indicator with its parameters and
output); NAME is the indicator's name.  Equal KEYs share a colour,
wherever they appear.  The first KEY of a NAME takes NAME's home
colour; another KEY of the same NAME takes the next palette colour,
from the home onward, that no KEY in ENTRIES holds yet.  Return an
alist (KEY . COLOUR) in first-seen order."
  (let ((n (length financial-chart-palette))
        taken result)
    (dolist (entry (cl-remove-duplicates entries :key #'car :test #'equal :from-end t))
      (let* ((home (financial-chart-palette-home (cdr entry)))
             (index (or (cl-loop for step below n
                                 for i = (mod (+ home step) n)
                                 unless (memq i taken) return i)
                        home)))
        (push index taken)
        (push (cons (car entry) (nth index financial-chart-palette)) result)))
    (nreverse result)))

(provide 'financial-chart-eas-palette)
;;; financial-chart-eas-palette.el ends here
