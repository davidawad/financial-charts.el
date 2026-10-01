;;; financial-chart-core.el --- Shared configuration, data prep and series helpers for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The backend-neutral base every financial-chart module builds on: the
;; customization group, the candle defcustoms, scale conversion, bar
;; windowing and row/glyph geometry.

;;; Code:

(require 'cl-lib)

(define-error 'financial-chart-error "financial-chart error")

(defgroup financial-chart nil
  "OHLC candlestick chart rendering."
  :group 'tools)

;; -----------------------------------------------------------------------
;; Size
;; -----------------------------------------------------------------------

(defcustom financial-chart-height 20
  "Number of character rows the price panel uses."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-max-bars 80
  "Maximum number of bars to render; older bars are trimmed.
A nil or non-positive value disables windowing entirely (render every
bar given, however wide that makes the chart)."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'financial-chart)

(defcustom financial-chart-candle-width 1
  "Character columns each candle's body occupies."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-candle-gap 1
  "Character columns of blank space between adjacent candles."
  :type 'integer
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Colors
;; -----------------------------------------------------------------------

(defcustom financial-chart-up-face 'success
  "Face used for up candles (close >= open)."
  :type 'face
  :group 'financial-chart)

(defcustom financial-chart-down-face 'error
  "Face used for down candles (close < open)."
  :type 'face
  :group 'financial-chart)

(defcustom financial-chart-wick-face nil
  "Face used for wick-only rows, or nil to reuse the candle's own
up/down face."
  :type '(choice (const :tag "Same as candle body" nil) face)
  :group 'financial-chart)

(defcustom financial-chart-axis-face nil
  "Face used for price/volume/date axis labels, or nil for the default face."
  :type '(choice (const :tag "Default face" nil) face)
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Glyphs
;; -----------------------------------------------------------------------

(defcustom financial-chart-glyph-full-block ?█
  "Glyph for a row a candle's body fully spans."
  :type 'character
  :group 'financial-chart)

(defcustom financial-chart-glyph-upper-half ?▀
  "Glyph for a row a candle's body only partially spans, upper half."
  :type 'character
  :group 'financial-chart)

(defcustom financial-chart-glyph-lower-half ?▄
  "Glyph for a row a candle's body only partially spans, lower half."
  :type 'character
  :group 'financial-chart)

(defcustom financial-chart-glyph-wick ?│
  "Glyph for a row only a candle's wick (not body) passes through."
  :type 'character
  :group 'financial-chart)

(defcustom financial-chart-glyph-empty ?\s
  "Glyph for a row neither a candle's body nor wick reaches."
  :type 'character
  :group 'financial-chart)

(defcustom financial-chart-glyph-indicator ?•
  "Default glyph for an overlay indicator with no `:glyph' of its own."
  :type 'character
  :group 'financial-chart)

(defcustom financial-chart-glyph-volume-bar financial-chart-glyph-full-block
  "Glyph for a fully-covered volume-pane row.
Partial rows reuse `financial-chart-glyph-upper-half'/`-lower-half'."
  :type 'character
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Price scale + axis
;; -----------------------------------------------------------------------

(defcustom financial-chart-scale 'linear
  "Price-axis scale: `linear' or `log'."
  :type '(choice (const linear) (const log))
  :group 'financial-chart)

(defcustom financial-chart-axis-label-count 3
  "Number of evenly-spaced price labels on the Y-axis (minimum 2:
bottom and top)."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-axis-format "%7.2f "
  "Format string for one price-axis label."
  :type 'string
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Volume pane
;; -----------------------------------------------------------------------

(defcustom financial-chart-show-volume t
  "Whether to render a volume pane below the price panel.
Only takes effect when at least one bar carries a non-nil :volume."
  :type 'boolean
  :group 'financial-chart)

(defcustom financial-chart-volume-height 5
  "Number of character rows the volume pane uses."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-volume-up-face nil
  "Face for up-candle volume bars, or nil to reuse `financial-chart-up-face'."
  :type '(choice (const :tag "Same as financial-chart-up-face" nil) face)
  :group 'financial-chart)

(defcustom financial-chart-volume-down-face nil
  "Face for down-candle volume bars, or nil to reuse `financial-chart-down-face'."
  :type '(choice (const :tag "Same as financial-chart-down-face" nil) face)
  :group 'financial-chart)

(defcustom financial-chart-volume-axis-label-count 2
  "Number of evenly-spaced labels on the volume pane's Y-axis."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-volume-axis-format "%7.0f "
  "Format string for one volume-axis label."
  :type 'string
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; X-axis
;; -----------------------------------------------------------------------

(defcustom financial-chart-show-x-axis t
  "Whether to render an X-axis date/time line below the chart.
Only takes effect when at least one bar carries a non-nil :time."
  :type 'boolean
  :group 'financial-chart)

(defcustom financial-chart-x-axis-label-count 4
  "Number of evenly-spaced date/time labels on the X-axis."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-x-axis-format "%m/%d"
  "`format-time-string' format for one X-axis label."
  :type 'string
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Overlay indicators
;; -----------------------------------------------------------------------

(defcustom financial-chart-indicators nil
  "List of overlay indicator specs, each a plist:
`:fn' (required) -- a function of one argument, the (already-windowed)
bars list, returning a list of the same length where each element is
either a number (the indicator's value for that bar, on the same price
scale as the candles) or nil (no value yet, e.g. a warm-up period).
`:face' -- face for this indicator's glyph (default `default').
`:glyph' -- character for this indicator's glyph (default
`financial-chart-glyph-indicator').

When more than one indicator's value lands in the same cell, the last
matching spec in this list wins."
  :type '(repeat plist)
  :group 'financial-chart)


;; -----------------------------------------------------------------------
;; Scale-space helpers (linear passthrough, or log)
;; -----------------------------------------------------------------------

(defun financial-chart--to-scale (value)
  "Convert VALUE from price-space to `financial-chart-scale' scale-space."
  (if (eq financial-chart-scale 'log) (log (max value 1e-9)) value))

(defun financial-chart--from-scale (value)
  "Convert VALUE from `financial-chart-scale' scale-space back to price-space."
  (if (eq financial-chart-scale 'log) (exp value) value))

;; -----------------------------------------------------------------------
;; Bar windowing
;; -----------------------------------------------------------------------

(defun financial-chart--window-bars (bars)
  "Trim BARS to the most recent `financial-chart-max-bars', if set."
  (if (and financial-chart-max-bars
           (> financial-chart-max-bars 0)
           (> (length bars) financial-chart-max-bars))
      (last bars financial-chart-max-bars)
    bars))

;; -----------------------------------------------------------------------
;; Row/label geometry
;; -----------------------------------------------------------------------

(defun financial-chart--bars-range (bars)
  "Return (LOW . HIGH) in scale-space spanning every bar's wick in BARS."
  (cons
   (financial-chart--to-scale
    (apply #'min (mapcar (lambda (b) (plist-get b :low)) bars)))
   (financial-chart--to-scale
    (apply #'max (mapcar (lambda (b) (plist-get b :high)) bars)))))

(defun financial-chart--row-bounds (min max height row)
  "Return (LOW . HIGH) scale-space bounds for character ROW (0 = bottom row)."
  (let ((unit (/ (- max min) (float height))))
    (cons (+ min (* row unit)) (+ min (* (1+ row) unit)))))

(defun financial-chart--axis-label-rows (height count)
  "Return COUNT row indices (0=bottom), evenly spaced across [0,HEIGHT)."
  (if (<= count 1)
      (list 0)
    (let ((step (/ (float (1- height)) (1- count))))
      (delete-dups (mapcar (lambda (i) (round (* i step)))
                           (number-sequence 0 (1- count)))))))

;; -----------------------------------------------------------------------
;; Glyph selection
;; -----------------------------------------------------------------------

(defun financial-chart--glyph (row-low row-high body-low body-high wick-low wick-high)
  "Return (TYPE . CHAR) for one row given its price bounds.
TYPE is `body', `wick', or `empty'. ROW-LOW/ROW-HIGH bound this text
row; BODY-LOW/BODY-HIGH bound the filled region (a candle body, or a
volume bar's [0,volume] span); WICK-LOW/WICK-HIGH bound the full
high/low range (equal to BODY-LOW/BODY-HIGH when there is no separate
wick, e.g. volume bars)."
  (let* ((row-mid (/ (+ row-low row-high) 2.0))
         (overlap-low (max body-low row-low))
         (overlap-high (min body-high row-high)))
    (cond
     ((>= overlap-low overlap-high)
      (if (and (<= wick-low row-high) (>= wick-high row-low))
          (cons 'wick financial-chart-glyph-wick)
        (cons 'empty financial-chart-glyph-empty)))
     ((and (<= overlap-low row-low) (>= overlap-high row-high))
      (cons 'body financial-chart-glyph-full-block))
     (t
      (cons 'body
            (if (>= (/ (+ overlap-low overlap-high) 2.0) row-mid)
                financial-chart-glyph-upper-half
              financial-chart-glyph-lower-half))))))

(defun financial-chart--candle-face (bar)
  "Return the face BAR's candle should render in."
  (if (>= (plist-get bar :close) (plist-get bar :open))
      financial-chart-up-face
    financial-chart-down-face))

(defun financial-chart--cell-string (char width center-only)
  "Return a WIDTH-wide string of CHAR.
When CENTER-ONLY and WIDTH > 1, pad with spaces on both sides so a
single CHAR sits in the middle column (used for wick rows, so a
multi-column-wide candle's wick still renders as a thin center line
rather than a solid block)."
  (if (or (not center-only) (<= width 1))
      (make-string width char)
    (let* ((left (/ (1- width) 2))
           (right (- width 1 left)))
      (concat (make-string left ?\s) (string char) (make-string right ?\s)))))

(provide 'financial-chart-core)
;;; financial-chart-core.el ends here
