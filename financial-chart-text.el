;;; financial-chart-text.el --- Unicode text renderers for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Text candlesticks: price panel, volume panel, X-axis, and the
;; `financial-chart-render'/`financial-chart-view' entry points.

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-indicators)

;; -----------------------------------------------------------------------
;; Price panel
;; -----------------------------------------------------------------------

(defun financial-chart--axis-label (min max height row)
  "Return a right-aligned price-axis label string for ROW, or blank padding."
  (if (memq row (financial-chart--axis-label-rows
                 height financial-chart-axis-label-count))
      (let* ((bounds (financial-chart--row-bounds min max height row))
             (value (if (= row (1- height)) (cdr bounds) (car bounds)))
             (text (format financial-chart-axis-format
                          (financial-chart--from-scale value))))
        (if financial-chart-axis-face
            (propertize text 'face financial-chart-axis-face)
          text))
    (make-string (length (format financial-chart-axis-format 0.0)) ?\s)))

(defun financial-chart--candle-cell (row-low row-high bar)
  "Return (TEXT . FACE) for BAR's candle at the row bound by ROW-LOW/ROW-HIGH."
  (let* ((open (financial-chart--to-scale (plist-get bar :open)))
         (close (financial-chart--to-scale (plist-get bar :close)))
         (low (financial-chart--to-scale (plist-get bar :low)))
         (high (financial-chart--to-scale (plist-get bar :high)))
         (glyph (financial-chart--glyph
                 row-low row-high (min open close) (max open close) low high))
         (type (car glyph))
         (char (cdr glyph))
         (face
          (if (eq type 'wick)
              (or financial-chart-wick-face (financial-chart--candle-face bar))
            (financial-chart--candle-face bar))))
    (cons
     (financial-chart--cell-string char financial-chart-candle-width
                                   (eq type 'wick))
     (unless (eq type 'empty) face))))

(defun financial-chart--render-price-panel (bars min max height indicator-series)
  "Render the price panel (candles + Y-axis) as a string."
  (let ((n (length bars))
        (gap (make-string financial-chart-candle-gap ?\s)))
    (mapconcat
     (lambda (row)
       (let ((bounds (financial-chart--row-bounds min max height row)))
         (concat
          (financial-chart--axis-label min max height row)
          (mapconcat
           (lambda (idx)
             (let* ((bar (nth idx bars))
                    (base (financial-chart--candle-cell
                           (car bounds) (cdr bounds) bar))
                    (overlay
                     (financial-chart--indicator-overlay
                      (car bounds) (cdr bounds) idx indicator-series))
                    (cell (or overlay base)))
               (propertize (car cell) 'face (cdr cell))))
           (number-sequence 0 (1- n))
           gap))))
     (number-sequence (1- height) 0 -1)
     "\n")))

;; -----------------------------------------------------------------------
;; Volume panel
;; -----------------------------------------------------------------------

(defun financial-chart--volume-axis-label (max-vol height row)
  "Return a right-aligned volume-axis label string for ROW, or blank padding."
  (if (memq row (financial-chart--axis-label-rows
                 height financial-chart-volume-axis-label-count))
      (let* ((value (if (= row (1- height)) max-vol
                       (* row (/ max-vol (float height)))))
             (text (format financial-chart-volume-axis-format value)))
        (if financial-chart-axis-face
            (propertize text 'face financial-chart-axis-face)
          text))
    (make-string (length (format financial-chart-volume-axis-format 0.0)) ?\s)))

(defun financial-chart--render-volume-panel (bars)
  "Render the volume panel as a string, or nil when no bar has :volume."
  (when (cl-some (lambda (b) (plist-get b :volume)) bars)
    (let* ((height financial-chart-volume-height)
           (n (length bars))
           (gap (make-string financial-chart-candle-gap ?\s))
           (volumes (mapcar (lambda (b) (float (or (plist-get b :volume) 0))) bars))
           (max-vol (max 1.0 (apply #'max volumes))))
      (mapconcat
       (lambda (row)
         (let ((row-low (* row (/ max-vol height)))
               (row-high (* (1+ row) (/ max-vol height))))
           (concat
            (financial-chart--volume-axis-label max-vol height row)
            (mapconcat
             (lambda (idx)
               (let* ((bar (nth idx bars))
                      (vol (float (or (plist-get bar :volume) 0)))
                      (glyph (financial-chart--glyph
                              row-low row-high 0.0 vol 0.0 vol))
                      (up (if (>= (plist-get bar :close) (plist-get bar :open))
                              t
                            nil))
                      (face
                       (if up
                           (or financial-chart-volume-up-face
                               financial-chart-up-face)
                         (or financial-chart-volume-down-face
                             financial-chart-down-face))))
                 (propertize
                  (financial-chart--cell-string
                   (cdr glyph) financial-chart-candle-width nil)
                  'face (unless (eq (car glyph) 'empty) face))))
             (number-sequence 0 (1- n))
             gap))))
       (number-sequence (1- height) 0 -1)
       "\n"))))

;; -----------------------------------------------------------------------
;; X-axis
;; -----------------------------------------------------------------------

(defun financial-chart--render-x-axis (bars)
  "Render the X-axis date/time line as a string, or nil when no bar has :time."
  (when (cl-some (lambda (b) (plist-get b :time)) bars)
    (let* ((label-width (length (format financial-chart-axis-format 0.0)))
           (cell-width (+ financial-chart-candle-width financial-chart-candle-gap))
           (n (length bars))
           (line-length (max label-width (+ label-width (* n cell-width))))
           (line (make-string line-length ?\s))
           (rows
            (financial-chart--axis-label-rows n financial-chart-x-axis-label-count)))
      (dolist (idx rows)
        (let ((time (plist-get (nth idx bars) :time)))
          (when time
            (let* ((label
                    (format-time-string financial-chart-x-axis-format
                                        (/ time 1000.0)))
                   (start (+ label-width (* idx cell-width)))
                   (end (min line-length (+ start (length label)))))
              (when (< start line-length)
                (store-substring line start (substring label 0 (- end start))))))))
      line)))

;; -----------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------

;;;###autoload
(defun financial-chart-render (bars &optional height)
  "Render BARS as a candlestick chart string, oldest bar first.
BARS is a list of (:open :high :low :close &optional :volume :time)
plists; only :open/:high/:low/:close are required. HEIGHT overrides
`financial-chart-height' for the price panel specifically; every other
aspect of rendering (bar-count window, candle width/gap, colors,
glyphs, scale, axis label counts/formats, the volume panel, the
X-axis, and overlay indicators) is controlled by the corresponding
`financial-chart-*' custom variable -- `let'-bind one for a one-off
override rather than passing it positionally."
  (unless bars
    (user-error "financial-chart-render: no bars to render"))
  (let* ((bars (financial-chart--window-bars bars))
         (height (or height financial-chart-height))
         (range (financial-chart--bars-range bars))
         (min (car range))
         (max (if (= (car range) (cdr range)) (+ (cdr range) 0.0001) (cdr range)))
         (indicator-series (financial-chart--compute-indicator-series bars))
         (price (financial-chart--render-price-panel bars min max height
                                                      indicator-series))
         (volume (and financial-chart-show-volume
                      (financial-chart--render-volume-panel bars)))
         (x-axis (and financial-chart-show-x-axis
                      (financial-chart--render-x-axis bars))))
    (mapconcat #'identity (delq nil (list price volume x-axis)) "\n")))

;;;###autoload
(defun financial-chart-view (bars &optional title height)
  "Pop a *financial-chart* buffer rendering BARS as candlesticks.
TITLE, if given, is inserted as a header line. HEIGHT overrides
`financial-chart-height'; see `financial-chart-render' for how every
other aspect of rendering is configured."
  (let ((buffer (get-buffer-create "*financial-chart*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when title
          (insert title "\n\n"))
        (insert (financial-chart-render bars height) "\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(provide 'financial-chart-text)
;;; financial-chart-text.el ends here
