;;; financial-chart.el --- OHLC candlestick charts, rendered in-buffer -*- lexical-binding: t; -*-

;; Author: David Awad
;; Keywords: comm, tools, finance

;;; Commentary:

;; Pure-Elisp candlestick chart renderer: takes a list of OHLC bars and
;; renders them as a unicode candlestick chart (with an optional volume
;; pane and X-axis) directly in an Emacs buffer -- no gnuplot, no
;; image/PNG generation, no external process.
;;
;; Every visual/behavioral knob is a `defcustom' -- height, bar-count
;; window, candle width/gap, colors, glyph characters, linear/log price
;; scale, axis label counts/formats, volume pane, X-axis, and overlay
;; indicators. Nothing about the chart's appearance is hardcoded into
;; the rendering functions; a caller who wants a one-off override
;; `let'-binds the relevant variable(s) around a `financial-chart-render'
;; call rather than passing a long positional argument list -- standard
;; Emacs idiom for widely-configurable rendering code.
;;
;; Data-source agnostic: `financial-chart-render'/`financial-chart-view'
;; take a plain list of (:open :high :low :close &optional :volume :time)
;; plists, oldest first. :volume feeds the optional volume pane; :time
;; (epoch milliseconds) feeds the optional X-axis; both are omittable,
;; and the chart degrades gracefully (no volume pane without :volume
;; data present in ANY bar; no X-axis without :time data).
;;
;; The Schwab bridge at the bottom is soft-wired via `fboundp' so this
;; file never hard-requires schwab-broker.el -- callers who never load
;; it get a renderer that still works against any other bar source
;; (Alpaca, a CSV import, synthetic data for testing, ...).

;;; Code:

(require 'cl-lib)

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
;; Schwab bridge defaults
;; -----------------------------------------------------------------------

(defcustom financial-chart-schwab-default-period-type "month"
  "Default `:period-type' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'string
  :group 'financial-chart)

(defcustom financial-chart-schwab-default-period 1
  "Default `:period' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-schwab-default-frequency-type "daily"
  "Default `:frequency-type' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'string
  :group 'financial-chart)

(defcustom financial-chart-schwab-default-frequency 1
  "Default `:frequency' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'integer
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

;; -----------------------------------------------------------------------
;; Indicator overlays
;; -----------------------------------------------------------------------

(defun financial-chart--compute-indicator-series (bars)
  "Evaluate every `financial-chart-indicators' spec's `:fn' over BARS."
  (mapcar
   (lambda (spec)
     (list :glyph (or (plist-get spec :glyph) financial-chart-glyph-indicator)
           :face (or (plist-get spec :face) 'default)
           :series (funcall (plist-get spec :fn) bars)))
   financial-chart-indicators))

(defun financial-chart--indicator-overlay (row-low row-high index series-list)
  "Return (TEXT . FACE), the last SERIES-LIST entry landing in this row
at bar INDEX, or nil when none do."
  (let (result)
    (dolist (spec series-list)
      (let ((value (nth index (plist-get spec :series))))
        (when (and value
                   (let ((scaled (financial-chart--to-scale value)))
                     (and (<= scaled row-high) (>= scaled row-low))))
          (setq result
                (cons
                 (financial-chart--cell-string
                  (plist-get spec :glyph) financial-chart-candle-width t)
                 (plist-get spec :face))))))
    result))

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

;; -----------------------------------------------------------------------
;; Schwab bridge -- soft-wired, only usable once schwab-broker.el is loaded
;; (schwab-broker-price-history-sync's real return shape, confirmed against
;; its own test fixture: {symbol, empty, candles: [{open,high,low,close,
;; volume,datetime}, ...]}, datetime in epoch milliseconds).
;; -----------------------------------------------------------------------

(defun financial-chart--schwab-candle->bar (candle)
  "Map one Schwab /pricehistory CANDLE alist to a financial-chart bar plist."
  (list
   :open (alist-get 'open candle)
   :high (alist-get 'high candle)
   :low (alist-get 'low candle)
   :close (alist-get 'close candle)
   :volume (alist-get 'volume candle)
   :time (alist-get 'datetime candle)))

(defun financial-chart--merge-schwab-defaults (keys)
  "Fill in `financial-chart-schwab-default-*' for any key KEYS omits.
KEYS is a flat keyword plist, as accepted by
`schwab-broker-price-history-sync'; an explicitly-supplied key is never
overridden."
  (let ((result (copy-sequence keys)))
    (dolist (default
             (list (cons :period-type financial-chart-schwab-default-period-type)
                   (cons :period financial-chart-schwab-default-period)
                   (cons :frequency-type
                         financial-chart-schwab-default-frequency-type)
                   (cons :frequency financial-chart-schwab-default-frequency)))
      (unless (plist-member result (car default))
        (setq result (plist-put result (car default) (cdr default)))))
    result))

;;;###autoload
(defun financial-chart-schwab-view (symbol &rest keys)
  "Fetch SYMBOL's price history via schwab-broker and view as candlesticks.
KEYS is passed through to `schwab-broker-price-history-sync' verbatim,
overriding `financial-chart-schwab-default-period-type'/`-period'/
`-frequency-type'/`-frequency' for any key it supplies -- e.g.
:period-type \"day\" :period 5 :frequency-type \"minute\" :frequency 1
for 5 days of 1-minute bars, leaving other defaults untouched."
  (interactive (list (read-string "Symbol: ")))
  (unless (fboundp 'schwab-broker-price-history-sync)
    (user-error
     "schwab-broker not loaded -- financial-chart-schwab-view needs it"))
  (let* ((keys (financial-chart--merge-schwab-defaults keys))
         (history (apply #'schwab-broker-price-history-sync symbol keys))
         (candles (append (alist-get 'candles history) nil))
         (bars
          (mapcar #'financial-chart--schwab-candle->bar candles)))
    (financial-chart-view bars
                          (format "%s"
                                  (or (alist-get 'symbol history)
                                      (upcase symbol))))))

(provide 'financial-chart)
;;; financial-chart.el ends here
