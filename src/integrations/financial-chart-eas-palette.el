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
wherever they appear.  The first KEY of each NAME takes NAME's home
colour.  Another KEY of the same NAME (SMA 50 after SMA 20) takes the
next palette colour after the home that is neither held nor the home
of another NAME in ENTRIES, so a repeat never takes an indicator's own
colour.  Return an alist (KEY . COLOUR) in first-seen order."
  (let* ((n (length financial-chart-palette))
         (keys (cl-remove-duplicates entries :key #'car :test #'equal :from-end t))
         (homes (mapcar (lambda (e) (financial-chart-palette-home (cdr e))) keys))
         (index (make-vector (length keys) nil))
         seen)
    ;; First of each name: its home.
    (cl-loop for entry in keys for home in homes for i from 0
             unless (or (member (cdr entry) seen) (cl-find home index))
             do (aset index i home)
             do (push (cdr entry) seen))
    ;; Repeats: the next free colour, preferring no one's home.
    (cl-loop for home in homes for i from 0
             unless (aref index i)
             do (aset index i
                      (or (cl-loop for step from 1 to n
                                   for j = (mod (+ home step) n)
                                   unless (or (cl-find j index) (memq j homes)) return j)
                          (cl-loop for step from 1 to n
                                   for j = (mod (+ home step) n)
                                   unless (cl-find j index) return j)
                          home)))
    (cl-loop for entry in keys for i across index
             collect (cons (car entry) (nth i financial-chart-palette)))))

(provide 'financial-chart-eas-palette)
;;; financial-chart-eas-palette.el ends here
