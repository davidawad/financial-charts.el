;;; financial-chart-text.el --- Unicode text renderers for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Text candlesticks: price panel, volume panel, X-axis, and the
;; `financial-chart-render'/`financial-chart-view' entry points.

;;; Code:

(require 'financial-chart-series)
(require 'subr-x)
(require 'financial-chart-core)
(require 'financial-chart-indicators)

(defun financial-chart-text--palette-face (face)
  "Map semantic FACE roles to colorblind-safe faces when selected."
  (if (not (eq financial-chart-color-palette 'colorblind-safe))
      face
    (cond
     ((memq face (list 'financial-chart-up 'success financial-chart-up-face))
      'financial-chart-colorblind-up)
     ((memq face (list 'financial-chart-down 'error financial-chart-down-face))
      'financial-chart-colorblind-down)
     ((and (listp face) (not (keywordp (car-safe face))))
      (mapcar #'financial-chart-text--palette-face face))
     (t face))))

(defun financial-chart-text--apply-palette (text)
  "Apply the selected semantic palette to face properties in TEXT."
  (when (and (stringp text) (eq financial-chart-color-palette 'colorblind-safe))
    (let ((position 0))
      (while (< position (length text))
        (let* ((next (or (next-single-property-change position 'face text)
                         (length text)))
               (face (get-text-property position 'face text))
               (mapped (financial-chart-text--palette-face face)))
          (unless (eq face mapped)
            (put-text-property position next 'face mapped text))
          (setq position next)))))
  text)

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
;; Oscillator panel
;; -----------------------------------------------------------------------

(defun financial-chart--oscillator-row (value height)
  "Map oscillator VALUE in [0,100] to a bottom-based row in HEIGHT."
  (round (* (1- height)
            (/ (max 0.0 (min 100.0 (float value))) 100.0))))

(defun financial-chart--oscillator-axis-label (height row)
  "Return the oscillator's fixed-scale label for ROW, or blank padding."
  (let* ((label-width (length (format financial-chart-axis-format 0.0)))
         (guide-70 (financial-chart--oscillator-row 70 height))
         (guide-30 (financial-chart--oscillator-row 30 height))
         (value (cond ((= row (1- height)) 100)
                      ((= row guide-70) 70)
                      ((= row 0) 0)
                      ((= row guide-30) 30))))
    (if value
        (let* ((text (format (format "%%%dd " (max 1 (1- label-width))) value)))
          (if financial-chart-axis-face
              (propertize text 'face financial-chart-axis-face)
            text))
      (make-string label-width ?\s))))

(defun financial-chart--oscillator-overlay (row index series-list height)
  "Return (TEXT . FACE) for the last oscillator at ROW and bar INDEX."
  (let (result)
    (dolist (spec series-list)
      (let ((value (nth index (plist-get spec :series))))
        (when (and (numberp value)
                   (= row (financial-chart--oscillator-row value height)))
          (setq result
                (cons (financial-chart--cell-string
                       (or (plist-get spec :glyph)
                           financial-chart-glyph-indicator)
                       financial-chart-candle-width nil)
                      (or (plist-get spec :face) 'default))))))
    result))

(defun financial-chart--render-oscillator-panel (bars series-list)
  "Render the 0-100 oscillator panel, including 30/70 guide rows."
  (let* ((height financial-chart-oscillator-height)
         (n (length bars))
         (gap (make-string financial-chart-candle-gap ?\s))
         (guide-face (or financial-chart-axis-face 'shadow))
         (guide-70 (financial-chart--oscillator-row 70 height))
         (guide-30 (financial-chart--oscillator-row 30 height)))
    (mapconcat
     (lambda (row)
       (concat
        (financial-chart--oscillator-axis-label height row)
        (mapconcat
         (lambda (idx)
           (let ((cell (financial-chart--oscillator-overlay
                        row idx series-list height)))
             (if cell
                 (propertize (car cell) 'face (cdr cell))
               (propertize
                (financial-chart--cell-string
                 (if (memq row (list guide-70 guide-30)) ?─ ?\s)
                 financial-chart-candle-width nil)
                'face (and (memq row (list guide-70 guide-30)) guide-face)))))
         (number-sequence 0 (1- n)) gap)))
     (number-sequence (1- height) 0 -1)
     "\n")))

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
override rather than passing it positionally. Configured
`financial-chart-oscillators' render in a separate fixed 0-100 panel
between prices and volume."
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
         (oscillator-series (financial-chart--compute-oscillator-series bars))
         (oscillator (and financial-chart-oscillators
                          (financial-chart--render-oscillator-panel
                           bars oscillator-series)))
         (volume (and financial-chart-show-volume
                      (financial-chart--render-volume-panel bars)))
         (x-axis (and financial-chart-show-x-axis
                      (financial-chart--render-x-axis bars))))
    (mapconcat #'identity (delq nil (list price oscillator volume x-axis)) "\n")))

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

;; -----------------------------------------------------------------------
;; Generic chart kinds (area, line, sparkline, payoff, bars, depth)
;; -----------------------------------------------------------------------

(defun financial-chart-text--label (text label-width face)
  "TEXT right-aligned in LABEL-WIDTH columns plus a space, in FACE."
  (propertize (format (format "%%%ds " label-width) text) 'face face))

(defun financial-chart-text--footer (values n unit label-width accent-face dim-face)
  "The \"last / range / N pts\" footer line for column VALUES over N points."
  (let ((r (financial-chart-range values)))
    (concat
     (financial-chart-text--label "" label-width dim-face)
     (propertize (format "last %s%s" (financial-chart-fmt (car (last values))) unit)
                 'face accent-face)
     (propertize (format "   range %s–%s%s   %d pts"
                         (financial-chart-fmt (car r)) (financial-chart-fmt (cdr r))
                         unit n)
                 'face dim-face))))

(defun financial-chart-text--series-x-axis (series width face &optional offset)
  "Date labels for SERIES below WIDTH plot columns, or nil without epoch X.
OFFSET is the number of leading columns before the plot begins."
  (let ((ticks (financial-chart-series-x-axis-labels series width)))
    (when ticks
      (let* ((offset (or offset 0))
             (line-width
              (max (+ offset width)
                   (cl-loop for (position label) in ticks
                            maximize (+ offset
                                        (round (* position (max 0 (1- width))))
                                        (length label)))))
             (line (make-string line-width ?\s)))
        (dolist (tick ticks)
          (let ((start (+ offset
                          (round (* (car tick) (max 0 (1- width)))))))
            (store-substring line start (cadr tick))))
        (propertize line 'face face)))))

;; --- area ---------------------------------------------------------------------

(cl-defun financial-chart-text-area
    (series &key (width 60) (height financial-chart-plot-height) (unit "") (label-width 6)
            (scale 'linear)
            (up-face 'financial-chart-up) (down-face 'financial-chart-down)
            (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
            (footer t) &allow-other-keys)
  "Render SERIES as an eighth-block area chart string.
WIDTH/HEIGHT are the plot size in columns/rows (labels excluded); UNIT
suffixes the axis and footer numbers.  :SCALE is `linear' or `log'; log
requires every Y value to be positive.  Epoch-millisecond X coordinates
add date labels below the plot.  The fill is UP-FACE when the series ends
at or above where it started, else DOWN-FACE.  FOOTER nil omits the
trailing last/range/points line.  Returns nil for no data."
  (when-let* ((values (financial-chart-series-values series)))
    (financial-chart-series-validate-scale series scale)
    (let* ((raw-cols (financial-chart-series-resample series width))
           (range-values (if (financial-chart-series-x-aware-p series) values raw-cols))
           (cols (mapcar (lambda (value)
                           (financial-chart-series-scale-value value scale))
                         raw-cols))
           (range (financial-chart-range
                   (mapcar (lambda (value)
                             (financial-chart-series-scale-value value scale))
                           range-values)))
           (lo (car range))
           (hi (cdr range))
           (display-lo (financial-chart-series-unscale-value lo scale))
           (display-hi (financial-chart-series-unscale-value hi scale))
           (span (financial-chart-series-scale-span lo hi scale))
           (cells (* height 8))
           (levels (mapcar (lambda (v) (max 1 (round (* cells (/ (- v lo) span)))))
                           cols))
           (face (financial-chart-direction-face raw-cols up-face down-face))
           (x-axis (financial-chart-text--series-x-axis
                    series (length cols) dim-face (1+ label-width)))
           (rows
            (cl-loop
             for row from (1- height) downto 0
             for floor-cells = (* row 8)
             collect
             (concat
              (financial-chart-text--label (cond ((= row (1- height)) (concat (financial-chart-fmt display-hi) unit))
                                     ((= row 0) (concat (financial-chart-fmt display-lo) unit))
                                     (t ""))
                               label-width dim-face)
              (propertize
               (mapconcat (lambda (lvl)
                            (string (aref financial-chart-blocks
                                          (max 0 (min 8 (- lvl floor-cells))))))
                          levels "")
               'face face)
              "\n"))))
      (let ((chart (apply #'concat rows))
            (footer-text
             (when footer
               (financial-chart-text--footer range-values (length values) unit label-width
                                             accent-face dim-face))))
        (concat chart
                (when x-axis (concat x-axis (when footer-text "\n\n")))
                (when (and footer-text (not x-axis)) "\n")
                footer-text)))))

;; --- braille line ---------------------------------------------------------------

(defconst financial-chart-text--braille-bits [[#x01 #x08] [#x02 #x10] [#x04 #x20] [#x40 #x80]]
  "Braille dot bit for [ROW-IN-CELL][COL-IN-CELL], row 0 at the top.")

(cl-defun financial-chart-text-line
    (series &key (width 60) (height financial-chart-plot-height) (unit "") (label-width 6)
            (scale 'linear)
            (up-face 'financial-chart-up) (down-face 'financial-chart-down)
            (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
            (footer t) &allow-other-keys)
  "Render SERIES as a braille line chart string (2x4 dots per cell).
Twice the horizontal and four times the vertical resolution of a block
chart, for terminals whose font carries the braille block.  Keywords as
in `financial-chart-text-area'.  :SCALE is `linear' or `log'; log
requires positive Y values.  Returns nil for no data."
  (when-let* ((values (financial-chart-series-values series)))
    (financial-chart-series-validate-scale series scale)
    (let* ((raw-pts (financial-chart-series-resample series (* 2 width)))
           (range-values (if (financial-chart-series-x-aware-p series) values raw-pts))
           (pts (mapcar (lambda (value)
                          (financial-chart-series-scale-value value scale))
                        raw-pts))
           (range (financial-chart-range
                   (mapcar (lambda (value)
                             (financial-chart-series-scale-value value scale))
                           range-values)))
           (lo (car range))
           (span (financial-chart-series-scale-span lo (cdr range) scale))
           (display-lo (financial-chart-series-unscale-value lo scale))
           (display-hi (financial-chart-series-unscale-value (cdr range) scale))
           (dots (* height 4))
           (ys (mapcar (lambda (v) (- dots 1 (round (* (1- dots) (/ (- v lo) span))))) pts))
           (ncols (/ (1+ (length pts)) 2))
           (grid (make-vector (* height ncols) 0))
           (face (financial-chart-direction-face raw-pts up-face down-face))
           (x-axis (financial-chart-text--series-x-axis
                    series ncols dim-face (1+ label-width))))
      (cl-flet ((dot (x y)
                  (let ((cell (+ (* (/ y 4) ncols) (/ x 2))))
                    (aset grid cell (logior (aref grid cell)
                                            (aref (aref financial-chart-text--braille-bits (% y 4))
                                                  (% x 2)))))))
        (cl-loop for (y0 y1) on ys
                 for x from 0
                 do (dot x y0)
                 when y1 do (cl-loop for y from (min y0 y1) to (max y0 y1)
                                     do (dot (if (< y (/ (+ y0 y1) 2.0)) x (1+ x)) y))))
      (concat
       (mapconcat
        (lambda (row)
          (concat
           (financial-chart-text--label (cond ((= row 0) (concat (financial-chart-fmt display-hi) unit))
                                  ((= row (1- height)) (concat (financial-chart-fmt display-lo) unit))
                                  (t ""))
                            label-width dim-face)
           (propertize
            (apply #'string (cl-loop for c from 0 below ncols
                                     collect (+ #x2800 (aref grid (+ (* row ncols) c)))))
            'face face)
           "\n"))
        (number-sequence 0 (1- height)) "")
       (when x-axis (concat x-axis "\n"))
       (when footer
         (concat "\n" (financial-chart-text--footer range-values (length values) unit label-width
                                        accent-face dim-face)))))))

;; --- sparkline ------------------------------------------------------------------

(cl-defun financial-chart-text-sparkline
    (series &key width face (up-face 'financial-chart-up) (down-face 'financial-chart-down)
            &allow-other-keys)
  "One-row sparkline of SERIES resampled to WIDTH columns.
WIDTH defaults to one column per value.  Epoch-millisecond X coordinates
add date labels below it.  FACE overrides the up/down direction colouring.
Returns \"\" for no data."
  (let ((values (financial-chart-series-values series)))
    (if (null values)
        ""
      (let* ((sample-width (or width (length values)))
             (cols (financial-chart-series-resample series sample-width))
             (range (financial-chart-range cols))
             (span (- (cdr range) (car range)))
             (sparkline
              (propertize
               (mapconcat (lambda (v)
                            (string (aref financial-chart-blocks
                                          (if (zerop span) 4
                                            (1+ (round (* 7 (/ (- v (car range)) span))))))))
                          cols "")
               'face (or face (financial-chart-direction-face cols up-face down-face))))
             (x-axis (financial-chart-text--series-x-axis series (length cols)
                                                         'financial-chart-dim)))
        (if x-axis (concat sparkline "\n" x-axis) sparkline)))))

;; --- payoff ---------------------------------------------------------------------

(defun financial-chart-text--signed-glyph (a b c0)
  "Glyph for the region [A,B] (eighths) within the cell starting at C0."
  (let ((overlap (max 0 (- (min b (+ c0 8)) (max a c0)))))
    (cond
     ((zerop overlap) nil)
     ((<= a c0) (aref financial-chart-blocks overlap))
     ((>= b (+ c0 8)) (cond ((>= overlap 6) ?█) ((>= overlap 3) ?▀) (t ?▔)))
     (t (aref financial-chart-blocks (max 1 overlap))))))

(defun financial-chart-text--zero-split (lo hi height)
  "(NEG-ROWS . PER-ROW): rows below zero and value per row for LO..HI.
Zero always falls on a row boundary and both signs share one scale,
chosen as the smallest per-row value that fits LO and HI in HEIGHT rows."
  (cond
   ((>= lo 0) (cons 0 (/ (max 0.001 hi) (float height))))
   ((<= hi 0) (cons height (/ (max 0.001 (- lo)) (float height))))
   (t (let (best)
        (dolist (rn (number-sequence 1 (1- height)) best)
          (let ((s (max (/ (- lo) (float rn)) (/ hi (float (- height rn))))))
            (when (or (null best) (< s (cdr best)))
              (setq best (cons rn s)))))))))

(cl-defun financial-chart-text-payoff
    (payoff &key (width 60) (height financial-chart-plot-height) (unit "$") (label-width 7)
            (up-face 'financial-chart-up) (down-face 'financial-chart-down)
            (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
            (footer t) &allow-other-keys)
  "Render PAYOFF ((PRICE PNL) ...) as a zero-anchored P/L diagram string.
Profit fills up from the zero line in UP-FACE, loss down in DOWN-FACE;
the zero row carries a dim baseline.  The footer lists breakevens,
max gain/loss and the price span.  Returns nil for no data."
  (when-let* ((values (financial-chart-series-values payoff)))
    (pcase-let* ((xs (delq nil (financial-chart-series-xs payoff)))
                 (cols (financial-chart-interpolate payoff width))
                 (`(,lo . ,hi) (financial-chart-payoff-range payoff))
                 (`(,neg-rows . ,per-row) (financial-chart-text--zero-split lo hi height))
                 (z (* 8 neg-rows))
                 (zero-row (min (1- height) neg-rows))
                 (levels (mapcar (lambda (v) (+ z (round (* 8 (/ v per-row))))) cols)))
      (concat
       (mapconcat
        (lambda (row)
          (let ((c0 (* row 8)))
            (concat
             (financial-chart-text--label (cond ((= row (1- height)) (financial-chart-fmt-money hi unit))
                                    ((= row 0) (financial-chart-fmt-money lo unit))
                                    ((= row zero-row) "0")
                                    (t ""))
                              label-width dim-face)
             (mapconcat
              (lambda (lvl)
                (let ((g (financial-chart-text--signed-glyph (min z lvl) (max z lvl) c0)))
                  (cond (g (propertize (string g) 'face (if (>= lvl z) up-face down-face)))
                        ((= row zero-row) (propertize "─" 'face dim-face))
                        (t " "))))
              levels "")
             "\n")))
        (number-sequence (1- height) 0 -1) "")
       (when xs
         (let ((left (format "%s%s" unit (financial-chart-fmt (car xs))))
               (right (format "%s%s" unit (financial-chart-fmt (car (last xs))))))
           (concat (financial-chart-text--label "" label-width dim-face)
                   (propertize (concat left
                                       (make-string (max 1 (- (length cols) (length left)
                                                              (length right)))
                                                    ?\s)
                                       right)
                               'face dim-face)
                   "\n")))
       (when footer
         (let ((bes (financial-chart-payoff-breakevens payoff))
               (all (financial-chart-range values)))
           (concat
            "\n" (financial-chart-text--label "" label-width dim-face)
            (propertize (if bes
                            (format "breakeven %s"
                                    (mapconcat (lambda (b) (concat unit (financial-chart-fmt b)))
                                               bes ", "))
                          "no breakeven")
                        'face accent-face)
            (propertize (format "   max %s / %s   %d pts"
                                (financial-chart-fmt-money (cdr all) unit)
                                (financial-chart-fmt-money (car all) unit)
                                (length values))
                        'face dim-face))))))))

;; --- diverging bars ---------------------------------------------------------------

(cl-defun financial-chart-text-bars
    (bars &key (width 60) label-width (unit "")
          (up-face 'financial-chart-up) (down-face 'financial-chart-down) (dim-face 'financial-chart-dim)
          &allow-other-keys)
  "Render BARS ((LABEL . VALUE) ...) as diverging horizontal bars.
Negative values grow left of a centre axis in DOWN-FACE, positive values
right in UP-FACE, each scaled to the largest magnitude.  WIDTH is the
whole line: label, bars and value.  Returns nil for no data."
  (when bars
    (let* ((lw (or label-width
                   (apply #'max (mapcar (lambda (b) (string-width (format "%s" (car b))))
                                        bars))))
           (half (max 1 (/ (- width lw 12) 2)))
           (peak (max 1e-9 (apply #'max (mapcar (lambda (b) (abs (cdr b))) bars)))))
      (mapconcat
       (lambda (b)
         (let* ((v (cdr b))
                (n (if (zerop v) 0 (max 1 (round (* half (/ (abs v) (float peak)))))))
                (face (if (< v 0) down-face up-face)))
           (concat
            (propertize (format (format "%%-%ds " lw) (car b)) 'face dim-face)
            (make-string (if (< v 0) (- half n) half) ?\s)
            (if (< v 0) (propertize (make-string n ?█) 'face face) "")
            (propertize "│" 'face dim-face)
            (if (< v 0) "" (propertize (make-string n ?█) 'face face))
            (make-string (if (< v 0) half (- half n)) ?\s)
            " "
            (propertize (financial-chart-fmt-money v unit) 'face face)
            "\n")))
       bars ""))))

;; --- OHLC ---------------------------------------------------------------------------

(cl-defun financial-chart-text-ohlc
    (bars &key (height financial-chart-plot-height) &allow-other-keys)
  "Render OHLC BARS as text candlesticks with a HEIGHT-row price panel.
The `ohlc' kind's adapter over `financial-chart-render'."
  (financial-chart-render bars height))

;; --- order-book depth -----------------------------------------------------------------

(defun financial-chart-depth-bar (size max-size cols &optional face)
  "A block bar for depth SIZE against MAX-SIZE over COLS columns, in FACE.
Square-root scaled so one whale level does not flatten every other bar
to a single block; never shorter than one block."
  (propertize (make-string (max 1 (round (* cols (sqrt (/ (float size) (float max-size))))))
                           ?█)
              'face face))

(provide 'financial-chart-text)
;;; financial-chart-text.el ends here
