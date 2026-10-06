;;; financial-chart-plot.el --- One entry point for every chart kind, drawn by eas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `financial-chart-plot' KIND DATA &rest PROPS returns a chart as a string
;; (propertized text in a terminal, an SVG document in a GUI);
;; `-plot-insert' puts it at point and `-plot-view' shows it in a
;; `financial-chart-plot-mode' buffer.  KIND is a key of
;; `financial-chart-kinds'; each kind names a data shape and the eas
;; template that draws it.  DATA is validated against the shape, lowered
;; to the template's bindings and drawn by eas.  The candlestick entry
;; points (`financial-chart-render', `-render-svg', `-view', `-export-svg',
;; `-export-png') are the `ohlc' kind with the candle defcustoms.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-core)
(require 'financial-chart-series)
(require 'financial-chart-validate)
(require 'financial-chart-indicators)

;; The adapters and transforms the kinds' templates use are registered
;; by financial-chart-eas, which needs every shape and so loads after
;; the kind modules (financial-chart.el loads it).

(defcustom financial-chart-backend 'auto
  "Rendering backend: `text', `svg', or `auto'.
`auto' uses SVG images when the selected frame can display them, else
text -- so the same call looks right in a GUI and a terminal."
  :type '(choice (const auto) (const text) (const svg))
  :group 'financial-chart)

(defcustom financial-chart-empty-text "no data"
  "Text `financial-chart-plot-insert' shows when a chart has no data."
  :type 'string
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Shapes and kinds -- registries, so the whole surface is enumerable.
;; A kind names a data shape and the eas template that draws it; adding
;; a kind is `financial-chart-register-kind', no dispatch code changes.
;; -----------------------------------------------------------------------

(defconst financial-chart--example-price-changes
  [0.36 -0.18 0.12 -0.31 0.48 -0.09 0.22 -0.42 0.29 0.07 -0.16 0.34]
  "Small deterministic daily changes used to build realistic chart examples.")

(defun financial-chart--example-series (start &optional count changes)
  "Build COUNT deterministic price points beginning near START.
CHANGES is a vector of repeating daily moves, defaulting to
`financial-chart--example-price-changes'."
  (let ((price start)
        (count (or count 48))
        (changes (or changes financial-chart--example-price-changes)))
    (cl-loop for index from 0 below count
             do (setq price (+ price (aref changes (% index (length changes)))))
             collect (list (1+ index)
                           (/ (float (round (* price 100))) 100.0)))))

(defun financial-chart--example-payoff ()
  "Build a deterministic 21-point long-straddle payoff example."
  (cl-loop for price from 80 to 120 by 2
           collect (list price (- (abs (- price 100)) 8))))

(defun financial-chart--example-ohlc ()
  "Build 48 deterministic OHLCV bars with daily timestamps."
  (let ((points (financial-chart--example-series 100.0))
        (open 99.8)
        bars)
    (cl-loop for point in points
             for close = (cadr point)
             for index from 0
             for spread = (aref [0.22 0.31 0.18 0.27 0.36 0.2] (% index 6))
             do (progn
                  (push (list :open open
                              :high (+ (max open close) spread)
                              :low (- (min open close) spread)
                              :close close
                              :volume (+ 9000 (* 375 (% (* index 7) 24)))
                              :time (+ 1700000000000 (* index 86400000)))
                        bars)
                  (setq open close)))
    (nreverse bars)))

(defvar financial-chart-shapes
  '((series
     :doc "Numbers, (X Y) lists or (X . Y) conses, oldest first.  A nil Y is skipped."
     :example ((1 40.0) (2 45.0) (3 50.0) (4 42.0))
     :validator financial-chart--validate-series)
    (payoff
     :doc "(PRICE PNL) pairs sorted by ascending price; both numeric."
     :example ((90 50) (95 0) (100 -100) (105 0) (110 50))
     :validator financial-chart--validate-payoff)
    (labeled
     :doc "(LABEL . VALUE) conses, e.g. P/L per position; VALUE numeric."
     :example (("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950))
     :validator financial-chart--validate-labeled)
    (ohlc
     :doc "bar/v1 plists (:open :high :low :close [:volume] [:time]), oldest
first: high >= max(open, close) >= min(open, close) >= low, :volume
non-negative, :time epoch milliseconds, on every bar or none, strictly
increasing."
     :example ((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
               (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000))
     :validator financial-chart--validate-ohlc))
  "Data shapes chart kinds accept: (SHAPE :doc :example :validator
\[:values FN] [:from-json FN] [:to-json FN]).
:values (DATA PROPS -> numbers) feeds explain and SVG provenance;
:from-json (parsed JSON DATA -> Lisp DATA) is for callers that parse JSON.
Both are optional, so a module adding a shape never edits this file.")

(setf (plist-get (alist-get 'series financial-chart-shapes) :example)
      (financial-chart--example-series 100.0)
      (plist-get (alist-get 'payoff financial-chart-shapes) :example)
      (financial-chart--example-payoff)
      (plist-get (alist-get 'labeled financial-chart-shapes) :example)
      '(("AAPL" . 1200) ("VTI" . 8000) ("QQQ" . 2650) ("TLT" . -720)
        ("GLD" . 430) ("MSFT" . 980) ("NVDA" . 1540) ("CASH" . 2100))
      (plist-get (alist-get 'ohlc financial-chart-shapes) :example)
      (financial-chart--example-ohlc))

(defvar financial-chart-kinds
  '((area :shape series :template "area" :adapter "series"
          :bindings financial-chart--series-bindings :check financial-chart--check-scale
          :doc "Area chart of a price or value history.")
    (line :shape series :template "series-line" :adapter "series"
          :bindings financial-chart--series-bindings :check financial-chart--check-scale
          :doc "Line chart of a price or value history.")
    (sparkline :shape series :template "sparkline" :adapter "series"
               :bindings financial-chart--series-bindings
               :doc "Compact sparkline for tables and mode lines.")
    (payoff :shape payoff :template "payoff" :adapter "payoff"
            :doc "Zero-anchored P/L-vs-price diagram with breakevens.")
    (bars :shape labeled :template "diverging-bars" :adapter "labeled"
          :doc "Diverging horizontal bars, e.g. P/L per position.")
    (ohlc :shape ohlc :template "ohlc" :slot :bars :bindings financial-chart--ohlc-bindings
          :doc "Candlesticks with optional volume pane, indicator overlays and oscillators."))
  "Chart kinds: (KIND :shape SHAPE :template NAME [:slot SLOT]
\[:adapter ADAPTER] [:props ((PROP . SLOT) ...)] [:bindings FN] [:check FN]
:doc STRING).  DATA is validated by SHAPE, lowered by eas adapter
ADAPTER (else passed as is) into template slot SLOT (default :data);
each PROP given becomes SLOT.  :bindings (FN DATA PROPS) returns more
bindings; :check (FN DATA PROPS) validates props that change how DATA
is read.")

(defun financial-chart-register-kind (kind &rest spec)
  "Register (or replace) chart KIND with SPEC.
SPEC is (:shape SHAPE :template NAME :doc DOC [:slot :adapter :props
:bindings :check]) as in `financial-chart-kinds'.  :shape must name an
entry of `financial-chart-shapes' and :template an eas template."
  (unless (assq (plist-get spec :shape) financial-chart-shapes)
    (signal 'financial-chart-error
            (list (format "unknown shape %S; known: %S" (plist-get spec :shape)
                          (mapcar #'car financial-chart-shapes))
                  :code "unknown_shape")))
  (unless (stringp (plist-get spec :template))
    (signal 'financial-chart-error
            (list (format "kind %S needs :template, the eas template that draws it" kind)
                  :code "missing_template")))
  (setf (alist-get kind financial-chart-kinds) spec)
  kind)

(defun financial-chart--kind (kind)
  "KIND's registry plist, or signal `financial-chart-unknown-kind'."
  (or (alist-get kind financial-chart-kinds)
      (signal 'financial-chart-unknown-kind
              (list (format "%S is not a chart kind; use one of %s (see `financial-chart-list-kinds')"
                            kind (mapconcat #'symbol-name (mapcar #'car financial-chart-kinds) ", "))
                    :code "unknown_kind" :kind kind))))

;; -- validation --

(defun financial-chart--check-scale (data props)
  "Validate PROPS' :scale against series DATA."
  (financial-chart--validate-scale data (plist-get props :scale)))

;;;###autoload
(defun financial-chart-validate (kind data &rest props)
  "Return t when DATA fits KIND's shape, else signal a typed error.
`financial-chart-invalid-data' carries :code, the offending element's
:index and :field (`financial-chart-error-data' reads them).  Empty DATA
is valid: it renders as \"no data\".  A kind whose registry entry has
:check (a function of DATA and PROPS) also validates the props that
change how DATA is read, e.g. `multi''s :normalize."
  (let* ((entry (financial-chart--kind kind))
         (shape (plist-get entry :shape)))
    (when data
      (funcall (plist-get (alist-get shape financial-chart-shapes) :validator) data)
      (when-let* ((check (plist-get entry :check)))
        (funcall check data props)))
    t))

;;;###autoload
(defun financial-chart-check (kind data &rest props)
  "Like `financial-chart-validate', but answer as data, never signal.
Returns t, or (:code :index :field :message) for the first problem."
  (condition-case err (apply #'financial-chart-validate kind data props)
    (financial-chart-error (financial-chart-error-data err))))

;; -- backend --

(defun financial-chart-plot--plist-drop (plist &rest keys)
  "PLIST without KEYS."
  (cl-loop for (k v) on plist by #'cddr
           unless (memq k keys) append (list k v)))

(defun financial-chart--backend-decision (backend)
  "(CONCRETE-BACKEND . REASON) for BACKEND (nil = `financial-chart-backend')."
  (let ((requested (or backend financial-chart-backend)))
    (pcase requested
      ('svg '(svg . "requested svg"))
      ('text '(text . "requested text"))
      (_ (if (and (display-images-p) (image-type-available-p 'svg))
             '(svg . "auto: this frame displays SVG images")
           '(text . "auto: this frame cannot display SVG images"))))))

(defun financial-chart-plot--resolve-backend (backend)
  "Concrete backend (`text' or `svg') for BACKEND.
BACKEND nil means `financial-chart-backend'."
  (car (financial-chart--backend-decision backend)))

(defun financial-chart--renderer-args (backend props)
  "The props `financial-chart-eas-render' receives on BACKEND for caller PROPS.
SVG takes :width and :height in pixels (:pixel-width/:pixel-height,
default 600x240); text takes :width columns and :height rows."
  (if (eq backend 'svg)
      (append (list :width (or (plist-get props :pixel-width) 600)
                    :height (or (plist-get props :pixel-height) 240))
              (financial-chart-plot--plist-drop props :backend :width :height
                                                :pixel-width :pixel-height))
    (financial-chart-plot--plist-drop props :backend)))

;; -- eas bindings --

(defun financial-chart-eas--json-value (value)
  "VALUE as a slot value: symbols become strings."
  (if (and (symbolp value) value (not (keywordp value)) (not (eq value t)))
      (symbol-name value)
    value))

(defun financial-chart--temporal-p (data)
  "Non-nil when every X in series DATA is epoch milliseconds."
  (let ((xs (delq nil (mapcar #'financial-chart-series--point-x (append data nil)))))
    (and xs (cl-every (lambda (x) (and (numberp x) (> x 1e11))) xs))))

(defun financial-chart--series-bindings (data props)
  "Series slots for DATA: x_type, y_title from :unit, scale from :scale."
  (append (when (financial-chart--temporal-p data) (list :x_type "temporal"))
          (let ((unit (plist-get props :unit)))
            (when (and (stringp unit) (not (string-empty-p unit))) (list :y_title unit)))
          (when (eq (plist-get props :scale) 'log) (list :scale "log"))))

(defun financial-chart--overlay-items (bars specs)
  "SPECS (`financial-chart-indicators' entries) as precomputed template items."
  (let (seen)
    (cl-loop for series in (financial-chart--compute-series bars specs)
             for i from 1
             for label = (plist-get series :label)
             ;; Each overlay is its own colour, so a repeated label gets its index.
             do (when (member label seen) (setq label (format "%s %d" label i)))
             (push label seen)
             collect (list :transform "values"
                           :values (vconcat (plist-get series :series))
                           :as label))))

(defun financial-chart--ohlc-bindings (bars _props)
  "ohlc slots: the windowed bars, volume pane, overlays and oscillators."
  (let ((bars (financial-chart--window-bars bars)))
    (list :bars bars
          :volume (if (and financial-chart-show-volume (cl-some (lambda (b) (plist-get b :volume)) bars))
                      t :false)
          :indicators (vconcat (financial-chart--overlay-items bars financial-chart-indicators))
          :oscillators (vconcat (financial-chart--overlay-items bars financial-chart-oscillators)))))

(defun financial-chart-eas-bindings (kind data &optional props)
  "Bindings for KIND's eas template that draw DATA with PROPS."
  (let* ((entry (financial-chart--kind kind))
         (adapter (plist-get entry :adapter))
         (slot (or (plist-get entry :slot) :data))
         (extra (when-let* ((fn (plist-get entry :bindings))) (funcall fn data props)))
         (bindings (list slot (if adapter (eas-data-rows (eas-data-from adapter data)) data))))
    (when (stringp (plist-get props :title))
      (setq bindings (plist-put bindings :title (plist-get props :title))))
    (dolist (pair (plist-get entry :props))
      (when-let* ((value (plist-get props (car pair))))
        (setq bindings (plist-put bindings (cdr pair) (financial-chart-eas--json-value value)))))
    (cl-loop for (key value) on extra by #'cddr
             do (setq bindings (plist-put bindings key value)))
    bindings))

(defun financial-chart-eas-resolve (kind data &optional props)
  "The pure Vega-Lite spec KIND's template resolves to for DATA and PROPS.
PROPS' :font, when a string, becomes the spec's config.font."
  (let ((spec (eas-resolve (plist-get (financial-chart--kind kind) :template)
                           (financial-chart-eas-bindings kind data props)))
        (font (plist-get props :font)))
    (if (stringp font)
        (plist-put spec :config (plist-put (copy-sequence (plist-get spec :config)) :font font))
      spec)))

(defun financial-chart-eas-scene (kind data backend &optional props)
  "KIND's scene for DATA on BACKEND (`text' or `svg') sized by PROPS.
Text takes :width columns and :height rows; svg :width and :height
pixels (what `financial-chart--renderer-args' hands over)."
  (let ((w (plist-get props :width)) (h (plist-get props :height)))
    (eas-compile (financial-chart-eas-resolve kind data props)
                 :target backend
                 :size (if (eq backend 'text)
                           (list :cols (or w 60) :rows (or h financial-chart-plot-height))
                         (and w h (cons w h))))))

(defun financial-chart-eas-render (kind data backend &rest props)
  "KIND drawn from DATA by its eas template on BACKEND, as a string.
Text keeps eas's text properties (help-echo, datum); svg is a document.
nil for empty DATA (\"\" for a sparkline)."
  (if (or (null data) (and (vectorp data) (zerop (length data))))
      (if (eq kind 'sparkline) "" nil)
    (let ((scene (financial-chart-eas-scene kind data backend props)))
      (if (eq backend 'text) (eas-text-render scene) (eas-svg-render scene)))))

;; -- explain: the pure plan --

(defun financial-chart--data-summary (shape data &optional props)
  "Point count and value range of DATA in SHAPE, for explain and provenance.
A shape whose `financial-chart-shapes' entry has :values (a function of
DATA and PROPS returning the plotted numbers) is summarized from those;
the built-in shapes are handled here."
  (let* ((values-fn (plist-get (alist-get shape financial-chart-shapes) :values))
         (ys (if values-fn
                 (funcall values-fn data props)
               (pcase shape
                 ('labeled (mapcar #'cdr data))
                 ('ohlc (append (delq nil (mapcar (lambda (b) (plist-get b :low)) data))
                                (delq nil (mapcar (lambda (b) (plist-get b :high)) data))))
                 (_ (financial-chart-series-values data)))))
         (points (if values-fn (length ys) (length data))))
    (append (list :points points)
            (when ys (list :min (apply #'min ys) :max (apply #'max ys))))))

;;;###autoload
(defun financial-chart-explain (kind data &rest props)
  "Return the plan `financial-chart-plot' would follow, without rendering.
A plist: :kind :shape :template :valid (t, or the error as
`financial-chart-error-data') :backend and :backend-reason :renderer
:args :points :min :max.  (apply RENDERER KIND DATA BACKEND ARGS) draws
exactly what `financial-chart-plot' returns before its SVG provenance.
Pure: no I/O, never signals for bad DATA."
  (let* ((entry (financial-chart--kind kind))
         (shape (plist-get entry :shape))
         (decision (financial-chart--backend-decision (plist-get props :backend)))
         (valid (apply #'financial-chart-check kind data props)))
    (append
     (list :kind kind :shape shape :template (plist-get entry :template) :valid valid
           :backend (car decision) :backend-reason (cdr decision)
           :renderer 'financial-chart-eas-render
           :args (financial-chart--renderer-args (car decision) props))
     (when (eq valid t) (financial-chart--data-summary shape data props)))))

;; -- render --

(defun financial-chart--svg-provenance (svg kind data props)
  "SVG with a <title> and <desc> naming KIND, the data and PROPS' :title.
An agent reading the file alone knows what it shows."
  (let* ((summary (financial-chart--data-summary
                   (plist-get (financial-chart--kind kind) :shape) data props))
         (title (replace-regexp-in-string
                 "[[:cntrl:]]" ""
                 (format "%s" (or (plist-get props :title)
                                   (format "%s chart" kind)))
                 t t))
         (desc (format "financial-chart %s: %d points%s" kind (plist-get summary :points)
                       (if (plist-get summary :min)
                           (format ", range %s to %s"
                                   (financial-chart-fmt (plist-get summary :min))
                                   (financial-chart-fmt (plist-get summary :max)))
                         "")))
         (end (and (string-prefix-p "<svg" svg) (string-search ">" svg))))
    (if (not end)
        svg
      (concat (substring svg 0 (1+ end))
              "<title>" (xml-escape-string title) "</title>"
              "<desc>" (xml-escape-string desc) "</desc>"
              (substring svg (1+ end))))))

;;;###autoload
(defun financial-chart-plot (kind data &rest props)
  "Render DATA as a KIND chart and return it as a string.
KIND is a key of `financial-chart-kinds' (`financial-chart-list-kinds');
DATA must fit its shape (`financial-chart-validate').  KIND's eas
template draws it.  PROPS: :backend (text, svg or auto -- see
`financial-chart-backend'), :width/:height (text columns/rows),
:pixel-width/:pixel-height (SVG), :unit, :title, :font (SVG font
family), :scale `linear' or `log' for area/line series, and the
kind's own props such as `multi''s :normalize.  With `svg' the string
is an SVG document carrying a <title>/<desc> provenance block; with
`text' it is propertized text.  Returns nil when DATA is empty
\(sparkline: \"\").  `financial-chart-explain' shows the plan first."
  (financial-chart--kind kind)
  (let ((backend (financial-chart-plot--resolve-backend (plist-get props :backend))))
    (apply #'financial-chart-validate kind data props)
    (let ((out (apply #'financial-chart-eas-render kind data backend
                      (financial-chart--renderer-args backend props))))
      (if (and out (eq backend 'svg))
          (financial-chart--svg-provenance out kind data props)
        out))))

;;;###autoload
(defun financial-chart-plot-spec (spec)
  "Render chart SPEC, a plist (:kind KIND :data DATA . PROPS).
A chart as plain data, convenient to build from parsed JSON."
  (apply #'financial-chart-plot (plist-get spec :kind) (plist-get spec :data)
         (financial-chart-plot--plist-drop spec :kind :data)))

;; -- enumerate --

;;;###autoload
(defun financial-chart-list-kinds ()
  "Every chart kind as (KIND :shape SHAPE :template NAME :doc DOC), registry order."
  (mapcar (lambda (e) (list (car e) :shape (plist-get (cdr e) :shape)
                            :template (plist-get (cdr e) :template)
                            :doc (plist-get (cdr e) :doc)))
          financial-chart-kinds))

;;;###autoload
(defun financial-chart-describe-kind (kind)
  "KIND's full description: shape doc, example data, its eas template,
and whether that template is registered right now."
  (let* ((entry (financial-chart--kind kind))
         (shape (alist-get (plist-get entry :shape) financial-chart-shapes)))
    (list :kind kind :doc (plist-get entry :doc)
          :shape (plist-get entry :shape) :shape-doc (plist-get shape :doc)
          :example (plist-get shape :example)
          :template (plist-get entry :template)
          :template-defined (and (member (plist-get entry :template) (eas-template-names)) t))))

;;;###autoload
(defun financial-chart-sparkline (series &rest props)
  "Compact text sparkline string for SERIES, drawn by eas's sparkline.
PROPS: :width columns (default 20) and :height rows (default 2)."
  (financial-chart-plot 'sparkline series :backend 'text
                        :width (or (plist-get props :width) 20)
                        :height (or (plist-get props :height) 2)))

;;;###autoload
(defun financial-chart-plot-insert (kind data &rest props)
  "Insert DATA as a KIND chart at point: an SVG image or text.
PROPS as in `financial-chart-plot'.
Inserts `financial-chart-empty-text' for no data.  When SVG is requested
but this Emacs cannot display SVG images, inserts the text chart with a
one-line note instead of failing."
  (let* ((requested (financial-chart-plot--resolve-backend (plist-get props :backend)))
         (backend (if (and (eq requested 'svg) (not (image-type-available-p 'svg)))
                      'text
                    requested))
         (out (apply #'financial-chart-plot kind data :backend backend props)))
    (unless (eq backend requested)
      (insert (propertize "(this Emacs cannot display SVG; showing text)\n"
                          'face 'financial-chart-dim)))
    (cond
     ((or (null out) (equal out "")) (insert (propertize financial-chart-empty-text 'face 'financial-chart-dim)))
     ((eq backend 'svg) (insert-image (create-image out 'svg t :ascent 'center) "[chart]"))
     (t (insert out)))))

;; --- the *financial-chart* buffer ----------------------------------------------------

(defvar-local financial-chart-plot--spec nil
  "(KIND DATA PROPS) of the chart shown in this `financial-chart-plot-mode' buffer.")

(defvar-local financial-chart-plot--refresh-timer nil
  "Buffer-local repeating timer for live plot refresh.")

(defvar-local financial-chart-plot--refresh-enabled nil
  "Non-nil when live refresh is enabled for this plot buffer.")

(defvar-local financial-chart-plot--zoom-window nil
  "Visible series index range as a cons (START . END), or nil for all data.")

(defvar-local financial-chart-plot--base-index 0
  "Source index of the first visible series point.")

(defvar-local financial-chart-plot--last-inspected-point nil
  "Last tooltip shown in the echo area, to avoid redundant messages.")

(defun financial-chart-plot--stop-refresh ()
  "Cancel this buffer's live refresh timer."
  (when (timerp financial-chart-plot--refresh-timer)
    (cancel-timer financial-chart-plot--refresh-timer))
  (setq financial-chart-plot--refresh-timer nil
        financial-chart-plot--refresh-enabled nil))

(defun financial-chart-plot--timer-refresh (buffer)
  "Refresh BUFFER if it is live, visible, and still has refresh enabled."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when financial-chart-plot--refresh-enabled
        (financial-chart-plot-refresh-data)))))

(defun financial-chart-plot--start-refresh ()
  "Start this buffer's configured repeating refresh timer."
  (let* ((props (nth 2 financial-chart-plot--spec))
         (refresh-fn (plist-get props :refresh-fn))
         (interval (plist-get props :refresh-interval)))
    (when (and (functionp refresh-fn) (numberp interval) (> interval 0))
      (setq financial-chart-plot--refresh-timer
            (run-at-time interval interval #'financial-chart-plot--timer-refresh
                         (current-buffer))
            financial-chart-plot--refresh-enabled t))))

(defvar financial-chart-plot-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'financial-chart-plot-refresh)
    (define-key m (kbd "t") #'financial-chart-plot-toggle-backend)
    (define-key m (kbd "r") #'financial-chart-plot-toggle-refresh)
    (define-key m (kbd "+") #'financial-chart-plot-zoom-in)
    (define-key m (kbd "-") #'financial-chart-plot-zoom-out)
    (define-key m (kbd "0") #'financial-chart-plot-zoom-reset)
    m)
  "Keymap for `financial-chart-plot-mode'.")

(define-derived-mode financial-chart-plot-mode special-mode "Finchart"
  "Major mode for a buffer showing one financial-chart chart.
\\<financial-chart-plot-mode-map>\\[financial-chart-plot-refresh] re-renders to the window size; \
\\[financial-chart-plot-toggle-backend] flips text/SVG; \\
\\[financial-chart-plot-toggle-refresh] toggles live refresh; \\
\\[financial-chart-plot-zoom-in]/\\[financial-chart-plot-zoom-out] zoom the series around point or latest data; \\
\\[financial-chart-plot-zoom-reset] shows all data.  Point on a text chart shows that datum."
  (add-hook 'post-command-hook #'financial-chart-plot--inspect-point nil t)
  (add-hook 'kill-buffer-hook #'financial-chart-plot--stop-refresh nil t))

(defun financial-chart-plot--fit-props (props)
  "PROPS with :width/:height filled in from the selected window if absent."
  (setq props (financial-chart-plot--plist-drop props :refresh-fn :refresh-interval))
  (let* ((win (get-buffer-window (current-buffer) t))
         (cols (if win (window-body-width win) 80))
         (rows (if win (window-body-height win) 24)))
    (append props
            (unless (plist-member props :width)
              (list :width (or financial-chart-plot-width (max 20 (- cols 10)))))
            (unless (plist-member props :height)
              (list :height (max 6 (min 20 (- rows 6)))))
            (unless (plist-member props :pixel-width)
              (list :pixel-width (if win (window-body-width win t) 600))))))

(defun financial-chart-plot--visible-window (length)
  "Return this buffer's clamped visible series range within LENGTH."
  (if (<= length 0)
      (cons 0 0)
    (let* ((window (or financial-chart-plot--zoom-window (cons 0 length)))
           (start (max 0 (min (car window) (1- length))))
           (end (max (1+ start) (min (cdr window) length))))
      (cons start end))))

(defun financial-chart-plot--visible-data (kind data)
  "Return KIND's current visible data slice and its starting index."
  (if (eq (plist-get (financial-chart--kind kind) :shape) 'series)
      (let* ((items (append data nil))
             (window (financial-chart-plot--visible-window (length items))))
        (when financial-chart-plot--zoom-window
          (setq financial-chart-plot--zoom-window window))
        (cons (cl-subseq items (car window) (cdr window)) (car window)))
    (cons data 0)))

(defun financial-chart-plot--series-view-p ()
  "Return non-nil when the current plot is a registered series kind."
  (and financial-chart-plot--spec
       (eq (plist-get (financial-chart--kind (car financial-chart-plot--spec)) :shape)
           'series)))

(defun financial-chart-plot--index-at-point ()
  "Source index of the series point under point, or nil.
eas marks each text cell with `eas-datum', the index of its row in the
mark; the series adapter keeps the visible slice's order (dropping nil
Ys), so that row is the slice's Nth plotted point."
  (when-let* ((datum (and (< (point) (point-max)) (get-text-property (point) 'eas-datum)))
              ((integerp datum)))
    (let* ((visible (car (financial-chart-plot--visible-data
                          (car financial-chart-plot--spec) (nth 1 financial-chart-plot--spec))))
           (plotted (cl-loop for p in visible for i from 0
                             when (financial-chart-series--point-y p) collect i)))
      (when-let* ((position (nth datum plotted)))
        (+ financial-chart-plot--base-index position)))))

(defun financial-chart-plot--inspect-point ()
  "Show the tooltip of the datum at point in the echo area."
  (let ((tip (and (< (point) (point-max)) (get-text-property (point) 'help-echo))))
    (unless (equal tip financial-chart-plot--last-inspected-point)
      (setq financial-chart-plot--last-inspected-point tip)
      (if (stringp tip) (message "%s" tip) (message nil)))))

(defun financial-chart-plot--render ()
  "Render this plot buffer's current data and visible series slice."
  (pcase-let* ((`(,kind ,data ,props) financial-chart-plot--spec)
               (`(,visible-data . ,base-index) (financial-chart-plot--visible-data kind data))
               (render-props (financial-chart-plot--fit-props props))
               (requested (financial-chart-plot--resolve-backend (plist-get props :backend)))
               (backend (if (and (eq requested 'svg) (not (image-type-available-p 'svg)))
                            'text requested))
               (out (apply #'financial-chart-plot kind visible-data :backend backend render-props)))
    (when financial-chart-plot--last-inspected-point
      (message nil))
    (setq financial-chart-plot--last-inspected-point nil
          financial-chart-plot--base-index base-index)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when-let* ((title (plist-get props :title)))
        (insert (propertize title 'face 'bold) "\n\n"))
      (when (and (not (eq backend requested)) (eq backend 'text))
        (insert (propertize "(this Emacs cannot display SVG; showing text)\n"
                            'face 'financial-chart-dim)))
      (cond
       ((or (null out) (equal out ""))
        (insert (propertize financial-chart-empty-text 'face 'financial-chart-dim)))
       ((eq backend 'svg)
        (insert-image (create-image out 'svg t :ascent 'center) "[chart]"))
       (t (insert out)))
      (goto-char (point-min)))))

(defun financial-chart-plot-refresh ()
  "Re-render this buffer's chart to fit its window."
  (interactive)
  (financial-chart-plot--render))

(defun financial-chart-plot-refresh-data ()
  "Call this plot's `:refresh-fn' once, store its DATA, and redraw.
Returns the fresh data.  Call this directly in tests instead of waiting
for a real timer."
  (interactive)
  (let* ((kind (car financial-chart-plot--spec))
         (props (nth 2 financial-chart-plot--spec))
         (refresh-fn (plist-get props :refresh-fn)))
    (when (functionp refresh-fn)
      (let ((data (funcall refresh-fn)))
        (apply #'financial-chart-validate kind data props)
        (setf (nth 1 financial-chart-plot--spec) data)
        (financial-chart-plot--render)
        data))))

(defun financial-chart-plot-toggle-refresh ()
  "Toggle this plot buffer's configured live refresh timer."
  (interactive)
  (if financial-chart-plot--refresh-enabled
      (progn
        (financial-chart-plot--stop-refresh)
        (message "Live refresh stopped"))
    (if (and (functionp (plist-get (nth 2 financial-chart-plot--spec) :refresh-fn))
             (numberp (plist-get (nth 2 financial-chart-plot--spec) :refresh-interval))
             (> (plist-get (nth 2 financial-chart-plot--spec) :refresh-interval) 0))
        (progn
          (financial-chart-plot--start-refresh)
          (message "Live refresh started"))
      (message "No live refresh function and positive interval configured"))))

(defun financial-chart-plot--zoom (direction)
  "Change the visible series range in DIRECTION (-1 zoom in, 1 zoom out)."
  (if (not (financial-chart-plot--series-view-p))
      (message "Zoom is available for series charts")
    (let* ((data (append (nth 1 financial-chart-plot--spec) nil))
           (length (length data))
           (window (financial-chart-plot--visible-window length))
           (start (car window))
           (end (cdr window))
           (span (- end start))
           (anchor (or (financial-chart-plot--index-at-point) (1- end)))
           (new-span (if (< direction 0)
                         (max 1 (floor (* span 0.8)))
                       (min length (max (1+ span) (ceiling (* span 1.25)))))))
      (setq anchor (max 0 (min anchor (1- length)))
            start (max 0 (- anchor (/ (1- new-span) 2)))
            end (+ start new-span))
      (when (> end length)
        (setq end length start (max 0 (- end new-span))))
      (setq financial-chart-plot--zoom-window (cons start end))
      (financial-chart-plot--render))))

(defun financial-chart-plot-zoom-in ()
  "Narrow the visible series range around point or the latest data."
  (interactive)
  (financial-chart-plot--zoom -1))

(defun financial-chart-plot-zoom-out ()
  "Widen the visible series range around point or the latest data."
  (interactive)
  (financial-chart-plot--zoom 1))

(defun financial-chart-plot-zoom-reset ()
  "Show the full series in this plot buffer."
  (interactive)
  (if (financial-chart-plot--series-view-p)
      (progn
        (setq financial-chart-plot--zoom-window nil)
        (financial-chart-plot--render))
    (message "Zoom is available for series charts")))

(defun financial-chart-plot-toggle-backend ()
  "Flip this buffer's chart between text and SVG."
  (interactive)
  (let* ((props (nth 2 financial-chart-plot--spec))
         (now (financial-chart-plot--resolve-backend (plist-get props :backend))))
    (setf (nth 2 financial-chart-plot--spec)
          (plist-put (copy-sequence props) :backend (if (eq now 'svg) 'text 'svg)))
    (financial-chart-plot-refresh)))

;;;###autoload
(defun financial-chart-plot-view (kind data &rest props)
  "Show DATA as a KIND chart in a `financial-chart-plot-mode' buffer.
Return the buffer.  PROPS as in `financial-chart-plot', plus :buffer
\(name, default \"*financial-chart*\"), :refresh-fn (function returning
fresh DATA), and :refresh-interval (positive timer interval in seconds).
A configured timer starts automatically and refreshes while visible."
  (let ((refresh-fn (plist-get props :refresh-fn))
        (refresh-interval (plist-get props :refresh-interval)))
    (when (or refresh-fn refresh-interval)
      (unless (and (functionp refresh-fn) (numberp refresh-interval) (> refresh-interval 0))
        (signal 'financial-chart-error
                (list ":refresh-fn and a positive :refresh-interval must be supplied together"
                      :code "invalid_refresh"))))
    (let ((buf (get-buffer-create (or (plist-get props :buffer) "*financial-chart*"))))
      (with-current-buffer buf
        (financial-chart-plot--stop-refresh)
        (financial-chart-plot-mode)
        (setq financial-chart-plot--spec
              (list kind data (financial-chart-plot--plist-drop props :buffer)))
        (setq financial-chart-plot--zoom-window nil
              financial-chart-plot--last-inspected-point nil))
      (unless noninteractive (pop-to-buffer buf))
      (with-current-buffer buf
        (financial-chart-plot-refresh)
        (financial-chart-plot--start-refresh))
      buf)))

;; --- candlesticks -----------------------------------------------------------------

(defun financial-chart--require-bars (bars caller)
  "Signal unless BARS is non-empty; CALLER names the entry point."
  (unless bars
    (financial-chart--invalid nil nil "no_data" "%s: no bars to render; pass bar/v1 plists" caller)))

;;;###autoload
(defun financial-chart-render (bars &optional height width)
  "Render BARS as a text candlestick chart string, oldest bar first.
BARS is a list of bar/v1 plists (:open :high :low :close [:volume]
\[:time]).  HEIGHT (rows, default `financial-chart-height') and WIDTH
\(columns, default `financial-chart-plot-width' or 80) size it.  The
bar window, volume pane, overlays and oscillators follow
`financial-chart-max-bars', `financial-chart-show-volume',
`financial-chart-indicators' and `financial-chart-oscillators'."
  (financial-chart--require-bars bars "financial-chart-render")
  (financial-chart-plot 'ohlc bars :backend 'text
                        :height (or height financial-chart-height)
                        :width (or width financial-chart-plot-width 80)))

;;;###autoload
(defun financial-chart-render-svg (bars &optional title font-family)
  "Render BARS as an SVG candlestick chart, returned as an XML string.
TITLE heads the chart; FONT-FAMILY sets the SVG font for this call.
Configured like `financial-chart-render'."
  (financial-chart--require-bars bars "financial-chart-render-svg")
  (apply #'financial-chart-plot 'ohlc bars :backend 'svg
         (append (when title (list :title title))
                 (when font-family (list :font font-family)))))

;;;###autoload
(defun financial-chart-view (bars &optional title height)
  "Show BARS as candlesticks in a `financial-chart-plot-mode' buffer.
TITLE, if given, heads the chart; HEIGHT overrides `financial-chart-height'."
  (financial-chart--require-bars bars "financial-chart-view")
  (apply #'financial-chart-plot-view 'ohlc bars
         :height (or height financial-chart-height)
         (when title (list :title title))))

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

;;;###autoload
(defun financial-chart-export-svg (bars file &optional title font-family)
  "Write BARS as an SVG candlestick chart to FILE.  Returns FILE.
TITLE and FONT-FAMILY are as in `financial-chart-render-svg'."
  (let ((svg (financial-chart-render-svg bars title font-family)))
    (with-temp-file file (insert svg)))
  file)

(defun financial-chart--resolve-png-converter ()
  "Return the converter `financial-chart-export-png' should use."
  (or financial-chart-png-converter
      (cl-find-if (lambda (name) (executable-find (symbol-name name)))
                  '(rsvg-convert convert magick))
      (signal 'financial-chart-error
              (list "No SVG->PNG converter found (looked for rsvg-convert/convert/magick); install one (e.g. librsvg) or set financial-chart-png-converter"
                    :code "no_png_converter"))))

(defun financial-chart--run-png-converter (converter svg-file png-file width height)
  "Invoke external CONVERTER to rasterize SVG-FILE to PNG-FILE.
WIDTH and HEIGHT (pixels) reach rsvg-convert only."
  (let* ((args
          (pcase converter
            ('rsvg-convert
             (append (list "-o" png-file)
                     (when width (list "-w" (number-to-string width)))
                     (when height (list "-h" (number-to-string height)))
                     (list svg-file)))
            ((or 'convert 'magick)
             (list svg-file png-file))))
         (status (apply #'call-process (symbol-name converter) nil nil nil args)))
    (unless (zerop status)
      (signal 'financial-chart-error
              (list (format "financial-chart-export-png: %s exited %s" converter status)
                    :code "png_converter_failed")))))

;;;###autoload
(defun financial-chart-export-png (bars file &optional title width height font-family)
  "Write BARS as a PNG candlestick chart to FILE.  Returns FILE.
Renders to SVG (`financial-chart-export-svg') then rasterizes via
`financial-chart-png-converter', the one external process this package
runs.  WIDTH/HEIGHT (pixels) reach converters that take them
\(rsvg-convert); TITLE and FONT-FAMILY are as in `-render-svg'."
  (let ((svg-file (make-temp-file "financial-chart" nil ".svg"))
        (converter (financial-chart--resolve-png-converter)))
    (unwind-protect
        (progn
          (financial-chart-export-svg bars svg-file title font-family)
          (if (functionp converter)
              (funcall converter svg-file file)
            (financial-chart--run-png-converter converter svg-file file width height)))
      (delete-file svg-file))
    file))

;;;###autoload
(defun financial-chart-demo ()
  "Show every financial-chart kind over built-in sample data."
  (interactive)
  (let ((series (cl-loop for i from 0 below 120
                         collect (list i (+ 100 (* 8 (sin (/ i 9.0))) (* 0.05 i)))))
        (payoff (cl-loop for p from 80 to 120
                         collect (list p (- (* 100 (max 0 (- p 100))) 250))))
        (buf (get-buffer-create "*financial-chart demo*")))
    (with-current-buffer buf
      (financial-chart-plot-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (spec `(("area" area ,series :unit "$")
                        ("line" line ,series :unit "$" :backend text)
                        ("payoff (long 100 call, $2.50)" payoff ,payoff)
                        ("bars" bars (("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950)))))
          (insert (propertize (car spec) 'face 'bold) "\n")
          (apply #'financial-chart-plot-insert (nth 1 spec) (nth 2 spec) :height 8 (nthcdr 3 spec))
          (insert "\n\n"))
        (insert "sparkline\n" (financial-chart-sparkline series :width 40) "\n")
        (goto-char (point-min))))
    (unless noninteractive (pop-to-buffer buf))
    buf))

;; --- health ---------------------------------------------------------------------

(defun financial-chart--check (name ok detail &optional remediation)
  "One eager doctor row: (:name NAME :status pass|fail :detail :remediation)."
  (list :name name :status (if ok 'pass 'fail) :detail detail
        :remediation (unless ok remediation)))

(defun financial-chart-plot-doctor-checks ()
  "Eager doctor rows: every registered kind renders its example in both
backends, and whether this frame can show SVG inline.  No I/O."
  (append
   (mapcar
    (lambda (entry)
      (let* ((kind (car entry))
             (example (plist-get (alist-get (plist-get (cdr entry) :shape)
                                            financial-chart-shapes)
                                 :example))
             (err (condition-case e
                      (progn
                        (financial-chart-plot kind example :backend 'text :width 40 :height 8)
                        (financial-chart-plot kind example :backend 'svg)
                        nil)
                    (error (error-message-string e)))))
        (financial-chart--check
         (format "kind %s renders" kind) (null err)
         (or err (format "example renders as text and svg through eas template %s"
                         (plist-get (cdr entry) :template)))
         (format "(financial-chart-describe-kind '%s) and check its template" kind))))
    financial-chart-kinds)
   (list (list :name "inline svg display"
               :status (if (image-type-available-p 'svg) 'pass 'skip)
               :detail (if (image-type-available-p 'svg)
                           "svg images available; auto backend draws SVG in GUI frames"
                         "no svg image support: auto backend draws text")
               :remediation nil))))

(provide 'financial-chart-plot)
;;; financial-chart-plot.el ends here
