;;; financial-chart.el --- OHLC candlestick charts, rendered in-buffer -*- lexical-binding: t; -*-

;; Author: David Awad
;; Keywords: comm, tools, finance

;;; Commentary:

;; Pure-Elisp candlestick chart renderer, two output forms:
;;
;; 1. `financial-chart-render'/`-view' -- a unicode candlestick chart
;;    (with an optional volume pane and X-axis) directly in an Emacs
;;    buffer. No gnuplot, no external process.
;; 2. `financial-chart-render-svg'/`-export-svg' -- a real vector SVG
;;    candlestick chart (actual rectangles/lines, not glyphs), built
;;    with Emacs's own `svg.el' -- still no external process.
;;    `-export-png' rasterizes that SVG to a PNG file via whichever of
;;    rsvg-convert/ImageMagick is available -- the one place in this
;;    file that shells out, because rasterizing vector graphics is not
;;    something Elisp can do on its own.
;;
;; Both renderers share the same configuration surface and the same
;; data-prep code (bar windowing, scale conversion, price range) so
;; they can never drift out of sync with each other.
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
;; Built-in indicator functions -- ready-made `:fn' values for
;; `financial-chart-indicators'.
;;
;; SMA/EMA/VWAP are on the same price scale as the candles (dollars, or
;; whatever unit :open/:high/:low/:close are in) and overlay directly:
;;
;;   (setq financial-chart-indicators
;;         (list (list :fn #'financial-chart-sma :face 'font-lock-keyword-face)))
;;
;; RSI is a 0-100 oscillator, NOT on the price scale -- see its own
;; docstring below before reaching for it as an overlay.
;; -----------------------------------------------------------------------

(defun financial-chart-sma (bars &optional window field)
  "Simple moving average of BARS' FIELD (default :close) over WINDOW
bars (default 20). Returns a list the same length as BARS: nil for the
first WINDOW-1 entries (not enough data yet), then the trailing mean."
  (let* ((window (or window 20))
         (field (or field :close))
         (values (mapcar (lambda (b) (plist-get b field)) bars))
         (n (length values)))
    (cl-loop for i from 0 below n
             collect
             (if (< i (1- window))
                 nil
               (/ (apply #'+ (cl-subseq values (- i window -1) (1+ i)))
                  (float window))))))

(defun financial-chart-ema (bars &optional window field)
  "Exponential moving average of BARS' FIELD (default :close) over
WINDOW bars (default 20), seeded with a simple average of the first
WINDOW values. Returns a list the same length as BARS: nil for the
first WINDOW-1 entries."
  (let* ((window (or window 20))
         (field (or field :close))
         (values (mapcar (lambda (b) (plist-get b field)) bars))
         (n (length values))
         (alpha (/ 2.0 (1+ window)))
         (result (make-list n nil))
         (prev nil))
    (cl-loop
     for i from 0 below n
     do
     (cond
      ((< i (1- window)) nil)
      ((= i (1- window))
       (setq prev (/ (apply #'+ (cl-subseq values 0 window)) (float window)))
       (setf (nth i result) prev))
      (t
       (setq prev (+ (* alpha (nth i values)) (* (- 1 alpha) prev)))
       (setf (nth i result) prev))))
    result))

(defun financial-chart-rsi (bars &optional period field)
  "Simple-average RSI of BARS' FIELD (default :close) over PERIOD bars
\(default 14). Returns a list the same length as BARS, values in
[0,100]; nil for the first bar (no prior value to diff against) and
for any bar before PERIOD changes have accumulated.

RSI is NOT on the same scale as price (0-100, vs. actual price levels)
-- do not pass this directly as a `financial-chart-indicators' :fn; it
will render invisible or nonsensical overlaid on the price panel's own
price-based Y-axis. Use it for a table/memo (see investment-memo.el's
option snapshot for the pattern), or build a separate oscillator
sub-panel with its own 0-100 scale, analogous to the volume panel."
  (let* ((period (or period 14))
         (field (or field :close))
         (values (mapcar (lambda (b) (plist-get b field)) bars))
         (n (length values))
         (changes
          (cl-loop for i from 1 below n
                   collect (- (nth i values) (nth (1- i) values)))))
    (cons
     nil
     (cl-loop
      for i from 0 below (length changes)
      collect
      (if (< i (1- period))
          nil
        (let* ((window (cl-subseq changes (- i period -1) (1+ i)))
               (gains (cl-loop for c in window when (> c 0) sum c))
               (losses (cl-loop for c in window when (< c 0) sum (- c)))
               (avg-gain (/ gains (float period)))
               (avg-loss (/ losses (float period))))
          (if (zerop avg-loss)
              100.0
            (- 100.0 (/ 100.0 (1+ (/ avg-gain avg-loss)))))))))))

(defun financial-chart-vwap (bars)
  "Cumulative volume-weighted average price over BARS, using typical
price ((high+low+close)/3) per bar.

VWAP conventionally resets every session -- pass one day's worth of
intraday bars for a real session VWAP, not a multi-day history, unless
you deliberately want a running VWAP across the whole window. Returns a
list the same length as BARS; nil for any bar with no :volume."
  (let ((cum-pv 0.0) (cum-vol 0.0))
    (mapcar
     (lambda (b)
       (let ((vol (plist-get b :volume)))
         (if (not vol)
             nil
           (let ((typical
                  (/ (+ (plist-get b :high) (plist-get b :low)
                       (plist-get b :close))
                     3.0)))
             (setq cum-pv (+ cum-pv (* typical vol)))
             (setq cum-vol (+ cum-vol vol))
             (if (zerop cum-vol) nil (/ cum-pv cum-vol))))))
     bars)))

;; -----------------------------------------------------------------------
;; Indicator cohorts -- named, reusable indicator sets (L4 of the
;; financial data abstraction tower, dot-financial-abstraction-tower-s15we.4)
;;
;; A cohort is DATA: a `financial-chart-indicator-cohorts' entry names a
;; reusable set of members, each either a built-in overlay fn or a
;; core-resource catalog recipe id. Adding a cohort or a member is a data
;; edit -- no new code. `financial-chart-resolve-cohort' turns a cohort
;; into concrete `financial-chart-indicators' :fn specs (pure, no I/O);
;; `financial-chart-describe-cohort' explains every member's disposition
;; (resolved / needs-oscillator-panel / unresolvable) with provenance and,
;; for catalog members, a live probe of the .3 indicator catalog.
;; -----------------------------------------------------------------------

;; Soft dependency on the L3 core-resource Emacs bridge (personal config
;; core-resources.el, dot-financial-abstraction-tower-s15we.3): called only
;; under `fboundp' in `financial-chart--probe-catalog-member', never
;; hard-required, so this file stays a standalone package.
(declare-function david-core-resource-get "core-resources"
                  (kind id &optional scope))

(define-error 'financial-chart-unresolvable-cohort
  "financial-chart: cohort member cannot be resolved" 'error)

(defcustom financial-chart-indicator-cohorts
  '((trend-following
     :doc "Price-overlay trend set: SMA20, SMA50, session VWAP."
     :members ((:fn financial-chart-sma :args (20) :face font-lock-keyword-face)
               (:fn financial-chart-sma :args (50) :face font-lock-type-face)
               (:fn financial-chart-vwap :face font-lock-constant-face)))
    (mean-reversion
     :doc "SMA20 price overlay plus catalog RSI-14 (oscillator, sub-panel only)."
     :members ((:fn financial-chart-sma :args (20) :face font-lock-keyword-face)
               (:indicator "finance.market.rsi-14" :params (:period 14))))
    (momentum
     :doc "Catalog RSI-14 only -- all-oscillator; resolves to an empty overlay \
set until a 0-100 sub-panel exists (visible via describe-cohort)."
     :members ((:indicator "finance.market.rsi-14" :params (:period 14)))))
  "Named indicator cohorts for `financial-chart-indicators' overlays.
Each entry is (COHORT-NAME :doc DOC :members (MEMBER...)).  A MEMBER is
either a built-in spec (:fn FN :args ARGS :face F :glyph G) -- FN a
`financial-chart-*' indicator applied to bars plus ARGS -- or a
catalog-backed spec (:indicator RECIPE-ID :params PLIST :face F :glyph G)
resolved through `financial-chart-recipe-evaluators'.  Adding a cohort or
a member is a pure data edit; resolve with `financial-chart-resolve-cohort'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'financial-chart)

(defcustom financial-chart-recipe-evaluators
  '(("finance.market.sma" :fn financial-chart-sma :arg-keys (:window))
    ("finance.market.rsi-14" :fn financial-chart-rsi :arg-keys (:period)
     :oscillator t))
  "Translation table from a core-resource indicator RECIPE-ID to a local
built-in evaluator.  Each entry is (RECIPE-ID :fn FN :arg-keys (KEY...)
[:oscillator BOOL]).  `financial-chart-resolve-cohort' maps a catalog
member's :params through :arg-keys into FN's trailing arguments;
:oscillator flags a 0-100 sub-panel indicator (excluded from the price
overlay).  A recipe id absent from this table is UNRESOLVABLE -- never
silently dropped.  Extending coverage is a data edit: add one entry.
`resource indicator get' returns a recipe DAG, not an elisp function, so
this explicit id-keyed table is the honest boundary (the transform chain
is not reachable through the front door), not a general recipe evaluator."
  :type '(alist :key-type string :value-type plist)
  :group 'financial-chart)

(defconst financial-chart--oscillator-fns '(financial-chart-rsi)
  "Built-in indicator fns producing a 0-100 oscillator, NOT a price-scale
overlay.  Members using these are flagged `needs-oscillator-panel' and
excluded from `financial-chart-resolve-cohort' until a sub-panel exists --
see `financial-chart-rsi's own docstring.")

(defun financial-chart--cohort (name)
  "Return cohort NAME's plist (:doc/:members) from
`financial-chart-indicator-cohorts', or nil when NAME is undefined."
  (cdr (assq name financial-chart-indicator-cohorts)))

(defun financial-chart--cohort-overlay-spec (member fn args)
  "Build a concrete `financial-chart-indicators' spec: FN curried over
ARGS into a one-argument overlay fn, carrying MEMBER's :face/:glyph."
  (let ((face (plist-get member :face))
        (glyph (plist-get member :glyph)))
    (append
     (list :fn (lambda (bars) (apply fn bars args)))
     (when face (list :face face))
     (when glyph (list :glyph glyph)))))

(defun financial-chart--resolve-member (member)
  "Classify MEMBER, returning (STATUS . DETAIL).  STATUS is `resolved'
\(DETAIL a concrete overlay spec), `needs-oscillator-panel' (DETAIL a
reason string; the member is a valid oscillator excluded from the price
overlay), or `unresolvable' (DETAIL a reason string naming the fix).
Pure: performs no I/O."
  (cond
   ((plist-member member :fn)
    (let ((fn (plist-get member :fn))
          (args (plist-get member :args)))
      (cond
       ((not (fboundp fn))
        (cons 'unresolvable
              (format "built-in fn `%s' is undefined -- load financial-chart.el \
or fix the cohort member" fn)))
       ((memq fn financial-chart--oscillator-fns)
        (cons 'needs-oscillator-panel
              (format "`%s' is a 0-100 oscillator; needs a sub-panel, excluded \
from the price overlay" fn)))
       (t (cons 'resolved (financial-chart--cohort-overlay-spec member fn args))))))
   ((plist-member member :indicator)
    (let* ((id (plist-get member :indicator))
           (params (plist-get member :params))
           (evaluator (cdr (assoc id financial-chart-recipe-evaluators))))
      (cond
       ((null evaluator)
        (cons 'unresolvable
              (format "no local evaluator for indicator `%s' -- add an entry to \
`financial-chart-recipe-evaluators' or drop the member" id)))
       ((plist-get evaluator :oscillator)
        (cons 'needs-oscillator-panel
              (format "indicator `%s' is a 0-100 oscillator; needs a sub-panel, \
excluded from the price overlay" id)))
       (t
        (let* ((fn (plist-get evaluator :fn))
               (arg-keys (plist-get evaluator :arg-keys))
               (args (mapcar (lambda (k) (plist-get params k)) arg-keys)))
          (if (fboundp fn)
              (cons 'resolved (financial-chart--cohort-overlay-spec member fn args))
            (cons 'unresolvable
                  (format "evaluator for `%s' maps to undefined fn `%s'" id fn))))))))
   (t (cons 'unresolvable
            (format "member is neither an :fn built-in nor an :indicator catalog \
spec: %S" member)))))

(defun financial-chart-resolve-cohort (name)
  "Resolve cohort NAME to a list of concrete `financial-chart-indicators'
specs -- the price-overlay-safe members only.  Oscillator members are
EXCLUDED (their price-panel overlay would be nonsensical; see
`financial-chart-describe-cohort' for the full per-member disposition).
Signal `financial-chart-unresolvable-cohort' -- its message naming the
offending member and the fix -- if any member cannot be resolved at all.
Pure: performs no I/O, so builtin-only cohorts resolve with the .3
catalog bridge absent."
  (let ((cohort (financial-chart--cohort name)))
    (unless cohort
      (signal 'financial-chart-unresolvable-cohort
              (list (format "no cohort named `%s' in \
`financial-chart-indicator-cohorts'" name))))
    (let (specs)
      (dolist (member (plist-get cohort :members))
        (let ((res (financial-chart--resolve-member member)))
          (pcase (car res)
            ('resolved (push (cdr res) specs))
            ('needs-oscillator-panel nil)
            ('unresolvable
             (signal 'financial-chart-unresolvable-cohort
                     (list (format "cohort `%s': %s" name (cdr res))))))))
      (nreverse specs))))

(defun financial-chart-list-cohorts ()
  "Return a summary of every cohort in `financial-chart-indicator-cohorts':
one (NAME :doc DOC :members N :resolvable R :excluded E :unresolvable U)
per cohort.  Pure: uses the static oscillator/evaluator classification,
not the live catalog."
  (mapcar
   (lambda (entry)
     (let* ((name (car entry))
            (plist (cdr entry))
            (members (plist-get plist :members))
            (r 0) (e 0) (u 0))
       (dolist (m members)
         (pcase (car (financial-chart--resolve-member m))
           ('resolved (setq r (1+ r)))
           ('needs-oscillator-panel (setq e (1+ e)))
           ('unresolvable (setq u (1+ u)))))
       (list name :doc (plist-get plist :doc)
             :members (length members) :resolvable r :excluded e
             :unresolvable u)))
   financial-chart-indicator-cohorts))

(defun financial-chart--catalog-value-oscillator-p (value)
  "Non-nil when a catalog record's VALUE alist describes a bounded
oscillator (RSI-style 0-100) rather than a price-scale series.  Probes
`bounds'/`unit' (Law 5: overlay-safety is probed from the live record,
not asserted from a hardcoded symbol list)."
  (or (and (alist-get 'bounds value) t)
      (and (member (alist-get 'unit value) '("1" "index" "score" "percent")) t)))

(defun financial-chart--probe-catalog-member (id)
  "Probe the live indicator catalog for ID through the core-resource
bridge, returning (:catalog-live LIVE :probed-oscillator OSC
:catalog-detail STR).  Soft: when `david-core-resource-get' is unbound
\(the .3 bridge is not loaded) LIVE/OSC are `:unknown' and callers fall
back to the static `financial-chart-recipe-evaluators' classification
\(Law 5 soft-fail; builtin-only cohorts never reach here)."
  (if (not (fboundp 'david-core-resource-get))
      (list :catalog-live :unknown :probed-oscillator :unknown
            :catalog-detail "core-resource indicator bridge not loaded; \
using static classification")
    (let ((record (ignore-errors (david-core-resource-get "indicator" id))))
      (if (not record)
          (list :catalog-live :false :probed-oscillator :unknown
                :catalog-detail (format "indicator `%s' not found in the live \
catalog" id))
        (let* ((attributes (alist-get 'attributes record))
               (value (alist-get 'value attributes)))
          (list :catalog-live t
                :probed-oscillator
                (and (financial-chart--catalog-value-oscillator-p value) t)
                :catalog-detail
                (format "live catalog: unit=%s scale=%s bounds=%s"
                        (alist-get 'unit value)
                        (alist-get 'scale value)
                        (alist-get 'bounds value))))))))

(defun financial-chart--describe-member (member)
  "Return a disposition plist for MEMBER: (:member M :kind KIND :status
STATUS :detail DETAIL [catalog probe keys]).  Catalog members carry a
live probe from `financial-chart--probe-catalog-member'."
  (let* ((res (financial-chart--resolve-member member))
         (detail (if (eq (car res) 'resolved)
                     "resolves to a price-panel overlay"
                   (cdr res))))
    (cond
     ((plist-member member :fn)
      (list :member member :kind 'builtin :status (car res) :detail detail))
     ((plist-member member :indicator)
      (append
       (list :member member :kind 'catalog :status (car res) :detail detail)
       (financial-chart--probe-catalog-member (plist-get member :indicator))))
     (t (list :member member :kind 'unknown :status (car res) :detail detail)))))

(defun financial-chart-describe-cohort (name)
  "Describe cohort NAME: provenance plus each member's disposition.
Returns (:name NAME :doc DOC :provenance PROV :members (MDESC...)); each
MDESC is a `financial-chart--describe-member' plist.  For catalog members
this MAY call `david-core-resource-get' (the only I/O in this layer,
read-only, soft -- an absent bridge downgrades to static classification)
to confirm live catalog validity and PROBE overlay-safety from the
record's value.  Signals `financial-chart-unresolvable-cohort' when NAME
is undefined."
  (let ((cohort (financial-chart--cohort name)))
    (unless cohort
      (signal 'financial-chart-unresolvable-cohort
              (list (format "no cohort named `%s'" name))))
    (list :name name
          :doc (plist-get cohort :doc)
          :provenance
          (format "defcustom `financial-chart-indicator-cohorts' (%s)"
                  (or (ignore-errors
                        (symbol-file 'financial-chart-indicator-cohorts 'defvar))
                      "financial-chart.el"))
          :members
          (mapcar #'financial-chart--describe-member
                  (plist-get cohort :members)))))

(defun financial-chart-cohort-doctor-checks ()
  "Doctor probe for the cohort layer (L4), consumed by the tower doctor
\(dot-financial-abstraction-tower-s15we.7).  Returns one plist per cohort:
\(:layer \"L4\" :name NAME :status pass|fail :detail D :remediation R).
PASS iff the cohort resolves without a `financial-chart-unresolvable-cohort'
error (oscillator exclusions are expected, not failures)."
  (mapcar
   (lambda (entry)
     (let ((name (car entry)))
       (condition-case err
           (let ((specs (financial-chart-resolve-cohort name)))
             (list :layer "L4" :name (format "cohort:%s" name) :status 'pass
                   :detail (format "resolves to %d price-overlay spec(s)"
                                   (length specs))
                   :remediation ""))
         (financial-chart-unresolvable-cohort
          (list :layer "L4" :name (format "cohort:%s" name) :status 'fail
                :detail (error-message-string err)
                :remediation "fix the cohort member or add a \
`financial-chart-recipe-evaluators' entry")))))
   financial-chart-indicator-cohorts))

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
;; SVG rendering -- a real vector chart, sharing this file's data-prep
;; code (windowing, scale conversion, price range, indicator series) and
;; configuration surface with the text renderer above, so they can never
;; drift out of sync with each other or need separate customization.
;; -----------------------------------------------------------------------

(require 'svg)

(defcustom financial-chart-svg-candle-width 6
  "Pixel width of each candle's body in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-candle-gap 3
  "Pixel gap between adjacent candles in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-wick-width 1
  "Pixel stroke width of a candle's wick line in the SVG renderer."
  :type 'number
  :group 'financial-chart)

(defcustom financial-chart-svg-price-height 400
  "Pixel height of the price panel in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-volume-height 100
  "Pixel height of the volume panel in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-left 55
  "Left margin (pixels) reserved for price/volume axis labels."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-right 20
  "Right margin (pixels) in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-top 40
  "Top margin (pixels) reserved for the title in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-bottom 30
  "Bottom margin (pixels) reserved for X-axis labels in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-font-size 12
  "Font size (pixels) for all text in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-font-family
  "DejaVu Sans Mono, Menlo, Consolas, monospace"
  "CSS font-family value for all text in the SVG renderer.
This package's own default is a sensible, widely-available open-source
monospace stack (DejaVu Sans Mono, with common platform fallbacks and a
generic `monospace' as the last resort) -- it doesn't assume any one
specific font is installed on whatever machine ends up rasterizing the
SVG. Set this to your own preferred font machine-wide (e.g. `(setq
financial-chart-svg-font-family \"Hack\")'), or pass FONT-FAMILY to
`financial-chart-render-svg'/`-export-svg'/`-export-png' to override it
for one call."
  :type 'string
  :group 'financial-chart)

(defcustom financial-chart-svg-background nil
  "Background color for the SVG renderer, or nil to use the current
`default' face's background (theme-aware)."
  :type '(choice (const :tag "Theme background" nil) color)
  :group 'financial-chart)

(defcustom financial-chart-svg-text-color nil
  "Text color for the SVG renderer, or nil to use `financial-chart-axis-face'
\(or the `default' face, if that's also nil) -- theme-aware either way."
  :type '(choice (const :tag "Theme-derived" nil) color)
  :group 'financial-chart)

(defcustom financial-chart-export-directory "~/Desktop"
  "Default directory the `financial-chart-schwab-export-*' commands
suggest/save into."
  :type 'directory
  :group 'financial-chart)

(defcustom financial-chart-png-converter nil
  "How `financial-chart-export-png' rasterizes SVG to PNG: nil
auto-detects the first available of `rsvg-convert'/`convert'/`magick'
via `executable-find'; a symbol names one of those explicitly; a
function is called as (FN SVG-FILE PNG-FILE) and does the conversion
itself (e.g. to shell out to some other tool, or convert in-process)."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const rsvg-convert) (const convert) (const magick)
                 function)
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-foreground "#333333"
  "Foreground used by the SVG renderer when a face's color can't be
resolved to a real color -- happens in a themeless session (e.g.
`emacs -Q --batch'), where Emacs returns its literal \"unspecified\"
placeholder instead of an actual color. Never used when a real theme
color is available."
  :type 'color
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-background "#ffffff"
  "Background fallback -- see `financial-chart-svg-fallback-foreground'."
  :type 'color
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-up-color "#2e7d32"
  "Foreground fallback specifically for `financial-chart-up-face' -- see
`financial-chart-svg-fallback-foreground'."
  :type 'color
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-down-color "#c62828"
  "Foreground fallback specifically for `financial-chart-down-face' -- see
`financial-chart-svg-fallback-foreground'."
  :type 'color
  :group 'financial-chart)

(defun financial-chart--color-unspecified-p (value)
  "Return non-nil when VALUE is Emacs's \"no real color\" placeholder
\(nil, the symbol `unspecified', or the strings \"unspecified-fg\"/
\"unspecified-bg\" -- `face-attribute' returns one of these forms
depending on the attribute and Emacs version when a face has no theme
color set, e.g. in a themeless `emacs -Q --batch' session)."
  (or (null value) (eq value 'unspecified)
      (member value '("unspecified-fg" "unspecified-bg"))))

(defun financial-chart--face-color (face &optional attr fallback)
  "Return FACE's resolved ATTR (default :foreground) as a color string.
Falls back to the `default' face's ATTR when FACE is nil or has none
of its own, and finally to FALLBACK (default
`financial-chart-svg-fallback-foreground'/`-background', by ATTR) when
even that is unresolvable -- always returns a real, renderable color,
regardless of whether a theme is loaded."
  (let* ((attr (or attr :foreground))
         (value
          (let ((v (and face (face-attribute face attr nil t))))
            (if (financial-chart--color-unspecified-p v)
                (face-attribute 'default attr nil t)
              v))))
    (if (financial-chart--color-unspecified-p value)
        (or fallback
            (if (eq attr :background)
                financial-chart-svg-fallback-background
              financial-chart-svg-fallback-foreground))
      (format "%s" value))))

(defun financial-chart--axis-label-values (min max count)
  "Return COUNT scale-space values evenly spaced from MIN to MAX inclusive."
  (if (<= count 1)
      (list min)
    (let ((step (/ (- max min) (float (1- count)))))
      (mapcar (lambda (i) (+ min (* i step))) (number-sequence 0 (1- count))))))

(defun financial-chart--svg-x (index)
  "Pixel X of bar INDEX's left edge in the SVG renderer."
  (+ financial-chart-svg-margin-left
     (* index (+ financial-chart-svg-candle-width financial-chart-svg-candle-gap))))

(defun financial-chart--svg-y (value min max panel-y panel-height)
  "Map scale-space VALUE in [MIN,MAX] to a pixel Y within a panel
spanning [PANEL-Y, PANEL-Y+PANEL-HEIGHT), Y growing downward."
  ;; (float ...) on the numerator is load-bearing: MIN/MAX/VALUE are
  ;; frequently plain integers (any bar data using whole-number prices),
  ;; and Elisp's `/' truncates on all-integer operands -- (/ 6 33) is 0,
  ;; not 0.18 -- which collapsed every candle but the topmost to the
  ;; panel's bottom pixel until this was caught by testing with integer
  ;; OHLC values.
  (+ panel-y (* panel-height (- 1.0 (/ (float (- value min)) (- max min))))))

(defun financial-chart--svg-price-panel (svg bars min max panel-y panel-h
                                             text-color indicator-series)
  "Draw the price panel (axis labels, gridlines, candles, indicator
overlays) into SVG."
  (dolist (value (financial-chart--axis-label-values
                  min max financial-chart-axis-label-count))
    (let ((y (financial-chart--svg-y value min max panel-y panel-h)))
      (svg-text svg (string-trim (format financial-chart-axis-format
                                        (financial-chart--from-scale value)))
               :x 5 :y (+ y 4) :fill text-color
               :font-family financial-chart-svg-font-family
               :font-size financial-chart-svg-font-size)))
  (cl-loop
   for i from 0
   for bar in bars
   do
   (let* ((x (financial-chart--svg-x i))
          (cx (+ x (/ financial-chart-svg-candle-width 2.0)))
          (open (financial-chart--to-scale (plist-get bar :open)))
          (close (financial-chart--to-scale (plist-get bar :close)))
          (low (financial-chart--to-scale (plist-get bar :low)))
          (high (financial-chart--to-scale (plist-get bar :high)))
          (up (>= (plist-get bar :close) (plist-get bar :open)))
          (color
           (financial-chart--face-color
            (if up financial-chart-up-face financial-chart-down-face)
            :foreground
            (if up financial-chart-svg-fallback-up-color
              financial-chart-svg-fallback-down-color)))
          (wick-color
           (if financial-chart-wick-face
               (financial-chart--face-color financial-chart-wick-face)
             color))
          (y-open (financial-chart--svg-y open min max panel-y panel-h))
          (y-close (financial-chart--svg-y close min max panel-y panel-h))
          (y-high (financial-chart--svg-y high min max panel-y panel-h))
          (y-low (financial-chart--svg-y low min max panel-y panel-h))
          (body-top (min y-open y-close))
          (body-h (max 1.0 (abs (- y-close y-open)))))
     (svg-line svg cx y-high cx y-low
              :stroke wick-color :stroke-width financial-chart-svg-wick-width)
     (svg-rectangle svg x body-top financial-chart-svg-candle-width body-h
                    :fill color)))
  (dolist (spec indicator-series)
    (let ((points
           (cl-loop
            for i from 0
            for value in (plist-get spec :series)
            when value
            collect
            (cons
             (+ (financial-chart--svg-x i) (/ financial-chart-svg-candle-width 2.0))
             (financial-chart--svg-y
              (financial-chart--to-scale value) min max panel-y panel-h)))))
      (when (>= (length points) 2)
        (svg-polyline svg points
                      :stroke (financial-chart--face-color (plist-get spec :face))
                      :fill "none" :stroke-width 1.5)))))

(defun financial-chart--svg-volume-panel (svg bars panel-y panel-h text-color)
  "Draw the volume panel (axis labels, bars) into SVG."
  (let* ((volumes (mapcar (lambda (b) (float (or (plist-get b :volume) 0))) bars))
         (max-vol (max 1.0 (apply #'max volumes))))
    (dolist (value (financial-chart--axis-label-values
                    0.0 max-vol financial-chart-volume-axis-label-count))
      (let ((y (financial-chart--svg-y value 0.0 max-vol panel-y panel-h)))
        (svg-text svg (string-trim (format financial-chart-volume-axis-format value))
                 :x 5 :y (+ y 4) :fill text-color
                 :font-size financial-chart-svg-font-size
                 :font-family financial-chart-svg-font-family)))
    (cl-loop
     for i from 0
     for bar in bars
     do
     (let* ((x (financial-chart--svg-x i))
            (vol (float (or (plist-get bar :volume) 0)))
            (up (>= (plist-get bar :close) (plist-get bar :open)))
            (color
             (financial-chart--face-color
              (if up
                  (or financial-chart-volume-up-face financial-chart-up-face)
                (or financial-chart-volume-down-face financial-chart-down-face))
              :foreground
              (if up financial-chart-svg-fallback-up-color
                financial-chart-svg-fallback-down-color)))
            (y (financial-chart--svg-y vol 0.0 max-vol panel-y panel-h))
            (h (max 1.0 (- (+ panel-y panel-h) y))))
       (svg-rectangle svg x y financial-chart-svg-candle-width h :fill color)))))

(defun financial-chart--svg-x-axis (svg bars axis-y text-color)
  "Draw evenly-spaced date/time labels into SVG below the chart."
  (let* ((n (length bars))
         (rows
          (financial-chart--axis-label-rows n financial-chart-x-axis-label-count)))
    (dolist (i rows)
      (let ((time (plist-get (nth i bars) :time)))
        (when time
          (svg-text svg (format-time-string financial-chart-x-axis-format
                                            (/ time 1000.0))
                   :x (financial-chart--svg-x i) :y (+ axis-y 15)
                   :fill text-color :font-size financial-chart-svg-font-size
                   :font-family financial-chart-svg-font-family))))))

;;;###autoload
(defun financial-chart-render-svg (bars &optional title font-family)
  "Render BARS as a real vector SVG candlestick chart, returned as an
XML string. Shares `financial-chart-render''s configuration surface
\(bar windowing, colors, scale, volume panel, X-axis, indicators) plus
its own `financial-chart-svg-*' size/margin/color knobs. FONT-FAMILY
overrides `financial-chart-svg-font-family' for this call only."
  (unless bars
    (user-error "financial-chart-render-svg: no bars to render"))
  (let* ((financial-chart-svg-font-family
          (or font-family financial-chart-svg-font-family))
         (bars (financial-chart--window-bars bars))
         (n (length bars))
         (range (financial-chart--bars-range bars))
         (min (car range))
         (max (if (= (car range) (cdr range)) (+ (cdr range) 0.0001) (cdr range)))
         (show-volume
          (and financial-chart-show-volume
               (cl-some (lambda (b) (plist-get b :volume)) bars)))
         (show-x-axis
          (and financial-chart-show-x-axis
               (cl-some (lambda (b) (plist-get b :time)) bars)))
         (plot-width
          (+ financial-chart-svg-margin-left financial-chart-svg-margin-right
             (* n (+ financial-chart-svg-candle-width financial-chart-svg-candle-gap))))
         (price-y financial-chart-svg-margin-top)
         (price-h financial-chart-svg-price-height)
         (volume-y (+ price-y price-h 10))
         (volume-h (if show-volume financial-chart-svg-volume-height 0))
         (xaxis-y (+ volume-y volume-h (if show-volume 10 0)))
         (total-height
          (+ xaxis-y (if show-x-axis financial-chart-svg-margin-bottom 10)))
         (bg (or financial-chart-svg-background
                (financial-chart--face-color 'default :background)))
         (text-color
          (or financial-chart-svg-text-color
              (and financial-chart-axis-face
                   (financial-chart--face-color financial-chart-axis-face))
              (financial-chart--face-color 'default :foreground)))
         (indicator-series (financial-chart--compute-indicator-series bars))
         (svg (svg-create plot-width total-height)))
    (svg-rectangle svg 0 0 plot-width total-height :fill bg)
    (when title
      (svg-text svg title :x financial-chart-svg-margin-left :y 20
               :fill text-color
               :font-size (+ 2 financial-chart-svg-font-size)
               :font-family financial-chart-svg-font-family
               :font-weight "bold"))
    (financial-chart--svg-price-panel
     svg bars min max price-y price-h text-color indicator-series)
    (when show-volume
      (financial-chart--svg-volume-panel svg bars volume-y volume-h text-color))
    (when show-x-axis
      (financial-chart--svg-x-axis svg bars xaxis-y text-color))
    (with-temp-buffer
      (svg-print svg)
      (buffer-string))))

;;;###autoload
(defun financial-chart-export-svg (bars file &optional title font-family)
  "Write BARS as an SVG candlestick chart to FILE. Returns FILE.
FONT-FAMILY overrides `financial-chart-svg-font-family' for this call."
  (with-temp-file file
    (insert (financial-chart-render-svg bars title font-family)))
  file)

(defun financial-chart--resolve-png-converter ()
  "Return the converter `financial-chart-export-png' should use."
  (or financial-chart-png-converter
      (cl-find-if (lambda (name) (executable-find (symbol-name name)))
                  '(rsvg-convert convert magick))
      (user-error
       "No SVG->PNG converter found (looked for rsvg-convert/convert/magick) -- install one (e.g. brew install librsvg) or set financial-chart-png-converter")))

(defun financial-chart--run-png-converter (converter svg-file png-file width height)
  "Invoke external CONVERTER to rasterize SVG-FILE to PNG-FILE."
  (let ((args
         (pcase converter
           ('rsvg-convert
            (append (list "-o" png-file)
                    (when width (list "-w" (number-to-string width)))
                    (when height (list "-h" (number-to-string height)))
                    (list svg-file)))
           ((or 'convert 'magick)
            (list svg-file png-file)))))
    (let ((status (apply #'call-process (symbol-name converter) nil nil nil args)))
      (unless (zerop status)
        (user-error "financial-chart-export-png: %s exited %s" converter status)))))

;;;###autoload
(defun financial-chart-export-png (bars file &optional title width height
                                        font-family)
  "Write BARS as a PNG candlestick chart to FILE.
Renders to SVG first (`financial-chart-export-svg') then rasterizes via
`financial-chart-png-converter' -- the one place this file shells out
to an external process, because rasterizing vector graphics isn't
something Elisp can do on its own. WIDTH/HEIGHT (pixels) are passed to
the converter when it supports them (currently: rsvg-convert).
FONT-FAMILY overrides `financial-chart-svg-font-family' for this call.
Returns FILE."
  (let ((svg-file (make-temp-file "financial-chart" nil ".svg"))
        (converter (financial-chart--resolve-png-converter)))
    (unwind-protect
        (progn
          (financial-chart-export-svg bars svg-file title font-family)
          (if (functionp converter)
              (funcall converter svg-file file)
            (financial-chart--run-png-converter
             converter svg-file file width height)))
      (delete-file svg-file))
    file))

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

(defun financial-chart--schwab-bars-and-title (symbol keys)
  "Fetch SYMBOL's price history via schwab-broker.
Returns (BARS . TITLE). Signals a clear `user-error' if schwab-broker
isn't loaded. KEYS is merged with `financial-chart--merge-schwab-defaults'."
  (unless (fboundp 'schwab-broker-price-history-sync)
    (user-error
     "schwab-broker not loaded -- this command needs it"))
  (let* ((keys (financial-chart--merge-schwab-defaults keys))
         (history (apply #'schwab-broker-price-history-sync symbol keys))
         (candles (append (alist-get 'candles history) nil))
         (bars (mapcar #'financial-chart--schwab-candle->bar candles)))
    (cons bars (format "%s" (or (alist-get 'symbol history) (upcase symbol))))))

;;;###autoload
(defun financial-chart-schwab-view (symbol &rest keys)
  "Fetch SYMBOL's price history via schwab-broker and view as candlesticks.
KEYS is passed through to `schwab-broker-price-history-sync' verbatim,
overriding `financial-chart-schwab-default-period-type'/`-period'/
`-frequency-type'/`-frequency' for any key it supplies -- e.g.
:period-type \"day\" :period 5 :frequency-type \"minute\" :frequency 1
for 5 days of 1-minute bars, leaving other defaults untouched."
  (interactive (list (read-string "Symbol: ")))
  (let ((bars-and-title (financial-chart--schwab-bars-and-title symbol keys)))
    (financial-chart-view (car bars-and-title) (cdr bars-and-title))))

;;;###autoload
(defun financial-chart-schwab-export-svg (symbol file &rest keys)
  "Fetch SYMBOL's price history via schwab-broker, export as SVG to FILE.
KEYS is as in `financial-chart-schwab-view'. Interactively, FILE
defaults to SYMBOL-chart.svg under `financial-chart-export-directory'."
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (expand-file-name (format "%s-chart.svg" (upcase symbol))
                             financial-chart-export-directory))))
  (let ((bars-and-title (financial-chart--schwab-bars-and-title symbol keys)))
    (financial-chart-export-svg (car bars-and-title) file (cdr bars-and-title))
    (message "financial-chart: wrote %s" file)
    file))

;;;###autoload
(defun financial-chart-schwab-export-png (symbol file &rest keys)
  "Fetch SYMBOL's price history via schwab-broker, export as PNG to FILE.
KEYS is as in `financial-chart-schwab-view'. Interactively, FILE
defaults to SYMBOL-chart.png under `financial-chart-export-directory'."
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (expand-file-name (format "%s-chart.png" (upcase symbol))
                             financial-chart-export-directory))))
  (let ((bars-and-title (financial-chart--schwab-bars-and-title symbol keys)))
    (financial-chart-export-png (car bars-and-title) file (cdr bars-and-title))
    (message "financial-chart: wrote %s" file)
    file))

(provide 'financial-chart)
;;; financial-chart.el ends here
