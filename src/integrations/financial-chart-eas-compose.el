;;; financial-chart-eas-compose.el --- declarative financial charts compiled to eas specs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The composition DSL (fc-gbo.2).  A chart is a JSON object (or the
;; same plist) the caller supplies; `financial-chart-compose' compiles
;; it to one plain eas (Vega-Lite) spec with the data inline:
;;
;;   {"title": "TSM daily",
;;    "bars": [{"time": "2026-08-20", "open": .., "high": .., "low": ..,
;;              "close": .., "volume": ..}, ...],
;;    "colors": {"up": "#26a69a", "down": "#ef5350"},
;;    "price": {"style": "candles",
;;              "series": [{"indicator": "sma", "params": [20]},
;;                         {"indicator": "sma", "params": [50], "dash": [4, 2]}],
;;              "fills": [{"between": ["sma-20", "sma-50"],
;;                         "above": "#26a69a", "below": "#ef5350"}]},
;;    "panes": [{"volume": true},
;;              {"series": ["rsi"], "rules": [30, 70],
;;               "fills": [{"between": ["rsi", 70], "color": "#ef5350"}]}]}
;;
;; The price pane draws a price style (`financial-chart-styles') with
;; overlays; each entry of "panes" is one more pane under it.  Every
;; pane shares the x axis, one crosshair and one zoom.  Series, fills
;; and colours are `financial-chart-eas-series' and
;; `financial-chart-eas-palette'.  A bad description signals
;; `financial-chart-invalid-chart' with :code and the JSON :path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-plot)
(require 'financial-chart-eas-palette)
(require 'financial-chart-eas-styles)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-shift)
(require 'financial-chart-overlay-indicators)

(defvar financial-chart-compose-pane-height 70
  "Default height of a pane under the price pane.")

(defvar financial-chart-compose-price-height 200
  "Default height of the price pane.")

;;; Bars

(defun financial-chart-compose--bars (chart)
  "CHART's bars as a validated list of plists, and their x positions.
Return (BARS XS X-TYPE): XS are epoch ms when every bar has a time,
else bar indices."
  (let ((bars (append (financial-chart-series-get chart :bars) nil)))
    (unless bars
      (financial-chart-series-fail "/bars" "NO_BARS"
                                   "A chart needs \"bars\": [{open, high, low, close[, volume, time]}, ...]"))
    (condition-case err
        (financial-chart-validate 'ohlc bars)
      (financial-chart-invalid-data
       (let* ((index (plist-get (cddr err) :index))
              ;; A bad or missing time is the DSL's own INVALID_TIME.
              (time (equal (format "%s" (plist-get (cddr err) :field)) "time")))
         (signal 'financial-chart-invalid-chart
                 (list (cadr err) :code (if time "INVALID_TIME" "INVALID_BAR") :index index
                       :path (format "/bars/%s%s" (or index "") (if time "/time" "")))))))
    (let* ((bars (mapcar (lambda (b) (if (eq (plist-get b :volume) :null)
                                         (plist-put (copy-sequence b) :volume nil)
                                       b))
                         bars))
           (times (mapcar (lambda (b) (financial-chart-series-get b :time)) bars)))
      (cond
       ((cl-every #'null times)
        (list bars (number-sequence 0 (1- (length bars))) "quantitative"))
       (t (list bars
                (cl-loop for time in times for i from 0
                         collect (or (and time (eas-time-parse time))
                                     (financial-chart-series-fail
                                      (format "/bars/%d/time" i) "INVALID_TIME"
                                      "Bar %d time %S is not epoch ms or an ISO date; give every bar one, or none"
                                      i time)))
                "temporal"))))))

(defun financial-chart-compose--rows (bars xs columns)
  "One row per bar of BARS at XS with every (FIELD . VECTOR) of COLUMNS."
  (vconcat
   (cl-loop for bar in bars for x in xs for i from 0
            collect (append (list :time x)
                            (cl-loop for key in '(:open :high :low :close :volume)
                                     append (list key (or (plist-get bar key) :null)))
                            (cl-loop for (field . values) in columns
                                     append (list (intern (concat ":" field))
                                                  (or (aref values i) :null)))))))

;;; Layers

(defun financial-chart-compose--y-domain (layers domain)
  "LAYERS with DOMAIN (a [LO HI] vector) on every quantitative y scale."
  (if (not domain) layers
    (mapcar (lambda (layer)
              (let* ((enc (plist-get layer :encoding)) (y (plist-get enc :y)))
                (if (not (equal (plist-get y :type) "quantitative")) layer
                  (plist-put (copy-sequence layer) :encoding
                             (plist-put (copy-sequence enc) :y
                                        (plist-put (copy-sequence y) :scale
                                                   (list :domain (vconcat domain))))))))
            layers)))

(defun financial-chart-compose--series-layer (ctx s legend)
  "A layer drawing series S of CTX; LEGEND is the pane's (LABELS . COLOURS)."
  (let* ((field (plist-get s :column))
         (style (plist-get s :style))
         (two-tone (and (equal style "histogram") (or (plist-get s :above) (plist-get s :below))))
         (colour (if two-tone
                     (list :condition (list :test (format "datum.%s >= 0" field)
                                            :value (or (plist-get s :above) (plist-get ctx :up)))
                           :value (or (plist-get s :below) (plist-get ctx :down)))
                   (list :datum (plist-get s :label) :type "nominal" :title :null
                         :scale (list :domain (vconcat (car legend)) :range (vconcat (cdr legend))))))
         (mark (pcase style
                 ("histogram" (list :type "bar" :opacity 0.8))
                 ("dots" (list :type "point" :filled t :size 12))
                 ("area" (list :type "area" :opacity 0.3))
                 (_ (financial-chart-styles-line-mark
                     "line" nil (or (plist-get s :width) 1.5) (plist-get s :dash)
                     :interpolate (if (equal style "step") "step-after" "linear"))))))
    (append
     (list :name (format "series-%s" (plist-get s :id)))
     (financial-chart-shift-layer-data ctx s)
     (list :transform (vector (list :filter (format "isValid(datum.%s)" field)))
           :mark (cl-loop for (k v) on mark by #'cddr unless (null v) append (list k v))
           :encoding (list :x (financial-chart-styles-x ctx)
                           :y (financial-chart-styles-y field)
                           :color colour)))))

(defun financial-chart-compose--rule (rule)
  "A reference rule layer of RULE: a number, or {y, color, dash, width}."
  (let ((rule (if (numberp rule) (list :y rule) rule)))
    (financial-chart-styles-rule-layer
     (financial-chart-series-get rule :y)
     (or (financial-chart-series-get rule :color) "gray")
     (or (financial-chart-series-get rule :dash) [2 2])
     (financial-chart-series-get rule :width))))

(defun financial-chart-compose--hit (ctx name series)
  "The invisible layer NAME the crosshair snaps to, with the readout of SERIES."
  (list :name name
        :mark (list :type "rule" :opacity 0)
        :encoding
        (list :x (financial-chart-styles-x ctx)
              :tooltip
              (vconcat
               (list (list :field "time" :type (plist-get ctx :x-type) :title "date"))
               (mapcar (lambda (f) (list :field f :type "quantitative"))
                       (if (plist-get ctx :volume) '("open" "high" "low" "close" "volume")
                         '("open" "high" "low" "close")))
               (mapcar (lambda (s) (list :field (plist-get s :column) :type "quantitative"
                                         :format ".2f" :title (plist-get s :label)))
                       series)))))

(defun financial-chart-compose--crosshair-rule (ctx)
  "The rule drawn at the crosshair's bar in CTX."
  (list :transform (vector (list :filter (list :param "crosshair" :empty :false)))
        :mark (list :type "rule" :color "gray" :strokeDash [2 2])
        :encoding (list :x (financial-chart-styles-x ctx))))

;;; Panes

(defun financial-chart-compose--pane (ctx pane spec)
  "PANE's vconcat entry in CTX.
SPEC is (:name :height :base BASE-LAYERS :series SERIES :fills FILLS
:title TITLE :domain DOMAIN)."
  (let* ((series (plist-get spec :series))
         (legend-series (cl-remove-if (lambda (s) (and (equal (plist-get s :style) "histogram")
                                                       (or (plist-get s :above) (plist-get s :below))))
                                      series))
         (legend (cons (mapcar (lambda (s) (plist-get s :label)) legend-series)
                       (mapcar (lambda (s) (plist-get s :color)) legend-series)))
         (hit (format "%s-hit" (plist-get spec :name)))
         (fills (cl-loop for fill in (plist-get spec :fills) for i from 0
                         append (financial-chart-styles-fill-layers
                                 ctx (financial-chart-styles-fill-rows
                                      (financial-chart-shift-xs ctx) (plist-get fill :a) (plist-get fill :b))
                                 (if (or (plist-get fill :color) (plist-get fill :above)
                                         (plist-get fill :below))
                                     fill
                                   (plist-put (copy-sequence fill) :color "#90a4ae"))
                                 (format "%s-fill-%d" (plist-get spec :name) i))))
         (rules (mapcar #'financial-chart-compose--rule
                        (append (financial-chart-series-get pane :rules) nil)))
         (layers (append fills (plist-get spec :base) rules
                         (mapcar (lambda (s) (financial-chart-compose--series-layer ctx s legend))
                                 series)))
         (layers (financial-chart-compose--y-domain layers (plist-get spec :domain))))
    (when-let* ((title (plist-get spec :title)))
      (setq layers (cons (let ((first (car layers)))
                           (plist-put (copy-sequence first) :encoding
                                      (plist-put (copy-sequence (plist-get first :encoding)) :y
                                                 (plist-put (copy-sequence
                                                             (plist-get (plist-get first :encoding) :y))
                                                            :title title))))
                         (cdr layers))))
    (list :name (plist-get spec :name)
          :width (plist-get ctx :width) :height (plist-get spec :height)
          :layer (vconcat layers
                          (list (financial-chart-compose--hit ctx hit series)
                                (financial-chart-compose--crosshair-rule ctx))))))

(defun financial-chart-compose--volume-layer (ctx)
  "The volume bars of CTX, coloured like their candle."
  (list :name "volume" :mark (list :type "bar" :opacity 0.6)
        :encoding (list :x (financial-chart-styles-x ctx)
                        :y (list :field "volume" :type "quantitative" :axis '(:tickCount 2))
                        :color (financial-chart-styles-up-down ctx "open" "close"))))

(defun financial-chart-compose--pane-domain (pane series)
  "PANE's y domain: its \"domain\", else the bounds all its SERIES share."
  (or (financial-chart-series-get pane :domain)
      (let ((bounds (delete-dups (mapcar (lambda (s) (plist-get s :bounds)) series))))
        (when (and series (= (length bounds) 1) (car bounds))
          (vector (car (car bounds)) (cdr (car bounds)))))))

;;; The chart

(defconst financial-chart-compose-examples-directory
  (expand-file-name "../../examples/compose"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Example chart descriptions, one per price style (STYLE.json).")

(defun financial-chart-compose-read (chart)
  "CHART as a parsed description: a plist, a JSON string or a JSON file."
  (cond ((and (stringp chart) (string-match-p "\\`[[:space:]]*{" chart)) (eas-json-parse chart))
        ((stringp chart)
         (unless (file-readable-p chart)
           (financial-chart-series-fail "" "NOT_FOUND" "No chart file %s" chart))
         (eas-json-read-file chart))
        ((and (listp chart) (keywordp (car chart))) chart)
        (t (financial-chart-series-fail "" "INVALID_CHART"
                                        "A chart is an object (plist), JSON text or a JSON file, got %S"
                                        chart))))

(defun financial-chart-compose--pane-series (pane bars path)
  "The resolved series of PANE (at PATH) over BARS."
  (cl-loop for item in (append (financial-chart-series-get pane :series) nil) for i from 0
           append (financial-chart-series-resolve item bars (format "%s/series/%d" path i))))

(defun financial-chart-compose--context (chart bars xs x-type)
  "The layer-building context of CHART over BARS at XS on an X-TYPE axis."
  (let ((colors (financial-chart-series-get chart :colors)))
    (list :bars bars :xs (vconcat xs) :x-type x-type
          :up (or (financial-chart-series-get colors :up) financial-chart-palette-up)
          :down (or (financial-chart-series-get colors :down) financial-chart-palette-down)
          :price-color (or (financial-chart-series-get colors :price) financial-chart-palette-price)
          :closes (vconcat (mapcar (lambda (b) (plist-get b :close)) bars))
          :volume (cl-some (lambda (b) (numberp (plist-get b :volume))) bars)
          :width (or (financial-chart-series-get chart :width) "container"))))

(defun financial-chart-compose--pane-context (ctx p n)
  "CTX for pane P (-1 for price) of N panes under the price pane.
Only the bottom pane labels and titles the shared x axis."
  (append (if (= p (1- n)) (list :x-title "date") (list :x-hidden t)) ctx))

(defun financial-chart-compose--check-panes (panes)
  "Signal unless PANES, the \"panes\" value, is an array of objects."
  (seq-do-indexed
   (lambda (pane i)
     (unless (and (listp pane) (or (null pane) (keywordp (car pane))))
       (financial-chart-series-fail (format "/panes/%d" i) "INVALID_PANE"
                                    "A pane is an object with \"series\", \"volume\", \"fills\", \"rules\", got %S"
                                    pane))
     (unless (cl-some (lambda (key) (financial-chart-series-get pane key))
                      '(:series :volume :fills :rules))
       (financial-chart-series-fail (format "/panes/%d" i) "EMPTY_PANE"
                                    "Pane %d draws nothing; give it \"series\", \"volume\": true, \"fills\" or \"rules\""
                                    i)))
   panes))

(defun financial-chart-compose (chart)
  "Compile CHART, a financial chart description, to a plain eas spec.
CHART is a plist or JSON (see `financial-chart-compose-read'); the
commentary of this file and README.md describe its keys.  The spec
carries the bars and every derived series inline, so it renders with
eas alone (`eas-compile', `bin/eas render SPEC.json').  Signal
`financial-chart-invalid-chart' with :code and :path on a bad CHART."
  (pcase-let* ((chart (financial-chart-compose-read chart))
               (`(,bars ,xs ,x-type) (financial-chart-compose--bars chart))
               (ctx (financial-chart-compose--context chart bars xs x-type))
               (price (or (financial-chart-series-get chart :price) '(:style "candles")))
               (panes (append (financial-chart-series-get chart :panes) nil))
               (_ (financial-chart-compose--check-panes panes))
               (n (length panes))
               (style (financial-chart-styles-price (financial-chart-compose--pane-context ctx -1 n)
                                                    price))
               (all (financial-chart-series-finish
                     (cl-loop for pane in (cons price panes) for p from -1
                              append (mapcar (lambda (s) (append (list :pane p) s))
                                             (financial-chart-compose--pane-series
                                              pane bars (if (< p 0) "/price" (format "/panes/%d" p)))))))
               (all (financial-chart-shift-series all (length bars)))
               (ctx (financial-chart-shift-context ctx all))
               (columns (append (plist-get style :columns)
                                (cl-remove-duplicates
                                 (mapcar (lambda (s) (cons (plist-get s :column) (plist-get s :values))) all)
                                 :key #'car :test #'equal)))
               (entries
                (cl-loop for pane in (cons price panes) for p from -1
                         for path = (if (< p 0) "/price" (format "/panes/%d" p))
                         for series = (cl-remove-if-not (lambda (s) (eql (plist-get s :pane) p)) all)
                         for volume = (and (>= p 0) (financial-chart-series-get pane :volume))
                         for pctx = (financial-chart-compose--pane-context ctx p n)
                         do (when (and volume (not (plist-get ctx :volume)))
                              (financial-chart-series-fail (concat path "/volume") "NO_VOLUME"
                                                           "A volume pane needs bars with \"volume\""))
                         collect
                         (financial-chart-compose--pane
                          pctx pane
                          (list :name (or (financial-chart-series-get pane :id)
                                          (cond ((< p 0) "price") (volume "volume")
                                                (t (format "pane-%d" (1+ p)))))
                                :height (or (financial-chart-series-get pane :height)
                                            (if (< p 0) financial-chart-compose-price-height
                                              financial-chart-compose-pane-height))
                                :base (cond ((< p 0) (plist-get style :layers))
                                            (volume (list (financial-chart-compose--volume-layer pctx))))
                                :title (or (financial-chart-series-get pane :title)
                                           (cond ((< p 0) "price") (volume "volume")
                                                 (series (mapconcat (lambda (s) (plist-get s :label))
                                                                    series ", "))))
                                :domain (financial-chart-compose--pane-domain pane series)
                                :series series
                                :fills (cl-loop for fill in (append (financial-chart-series-get pane :fills) nil)
                                                for i from 0
                                                collect (financial-chart-series-fill
                                                         fill all bars (format "%s/fills/%d" path i)))))))
               (hits (mapcar (lambda (e) (format "%s-hit" (plist-get e :name))) entries))
               (title (financial-chart-series-get chart :title)))
    (append
     (list :$schema "https://vega.github.io/schema/vega-lite/v6.json")
     (when title (list :title title))
     (list :description (or (financial-chart-series-get chart :description)
                            (format "%s price%s of %d bars%s."
                                    (or (financial-chart-series-get price :style) "candles")
                                    (if all (format " with %d series" (length all)) "")
                                    (length bars)
                                    (if panes (format " and %d pane%s below" n (if (> n 1) "s" "")) "")))
           :data (list :values (financial-chart-compose--rows bars xs columns))
           :resolve '(:scale (:x "shared" :y "independent" :color "independent")))
     (unless (and (plist-member chart :crosshair)
                  (not (financial-chart-series-get chart :crosshair)))
       (list :params (financial-chart-compose--params hits)))
     (list :vconcat (vconcat entries)))))

(defun financial-chart-compose--params (hits)
  "The shared crosshair and zoom params over the HITS layers."
  (vector (list :name "crosshair"
                :select '(:type "point" :on "pointermove" :nearest t :encodings ["x"]
                                :clear "pointerleave")
                :views (vconcat hits))
          (list :name "zoom" :select '(:type "interval" :encodings ["x"]) :bind "scales"
                :views (vconcat hits))))

(cl-defun financial-chart-compose-render (chart &key (backend 'text) width height)
  "CHART (see `financial-chart-compose') drawn by eas on BACKEND.
BACKEND is `text' (WIDTH columns, HEIGHT rows; default 80x24) or `svg'
\(WIDTH by HEIGHT pixels, else eas's size).  Return a string; text keeps
eas's help-echo and datum properties."
  (let* ((spec (financial-chart-compose chart))
         (scene (eas-compile spec :target backend
                             :size (if (eq backend 'text)
                                       (list :cols (or width 80) :rows (or height 24))
                                     (and width height (cons width height))))))
    (if (eq backend 'text) (eas-text-render scene) (eas-svg-render scene))))

(defun financial-chart-compose-example (&optional style)
  "The example description for price STYLE (default candles), parsed."
  (let ((file (expand-file-name (format "%s.json" (or style "candles"))
                                financial-chart-compose-examples-directory)))
    (unless (file-readable-p file)
      (financial-chart-series-fail "/price/style" "UNKNOWN_STYLE" "No example for style %s; styles: %s"
                                   style (mapconcat #'car financial-chart-styles ", ")))
    (eas-json-read-file file)))

(defun financial-chart-compose-describe ()
  "The composition DSL as JSON-ready data: styles, series, fills, panes."
  (list :contract "financial-chart/compose/v1"
        :entry-points '(:compile "financial-chart-compose" :render "financial-chart-compose-render"
                        :example "financial-chart-compose-example"
                        :shell "financial-chart-compose-main")
        :styles (vconcat (mapcar (lambda (s) (list :name (car s) :doc (cdr s))) financial-chart-styles))
        :chart '(:bars "bar/v1 rows {time?, open, high, low, close, volume?}, oldest first"
                 :title "string" :description "string" :width "pixels, default container"
                 :colors "{up, down, price}" :crosshair "boolean, default true"
                 :price "{style, field, color, width, dash, baseline, above, below, height, series, fills, rules}"
                 :panes "[{series, fills, rules, volume, title, domain, height, id}]")
        :series '(:forms ["\"sma\"" "{indicator, params, output}" "{values, label}" "{field}"]
                  :keys "id label color width dash style above below"
                  :ids "indicator-params (sma-20); each output of a multi-output indicator, or one picked by output, adds .OUTPUT (macd-12-26-9.macd-signal)")
        :series-styles (vconcat financial-chart-series-styles)
        :fills "{between: [A, B], color} or {between: [A, B], above, below, opacity}; A and B are series ids, labels, bar fields or numbers"
        :rules "a number or {y, color, dash, width}"
        :palette (list :series (vconcat financial-chart-palette) :up financial-chart-palette-up
                       :down financial-chart-palette-down)
        :indicators (vconcat (mapcar (lambda (e) (symbol-name (car e)))
                                     (reverse financial-chart-indicator-registry)))))

(defun financial-chart-compose-failure (err)
  "ERR (a condition) as {ok: false, reason, message, path?, index?}."
  (let ((props (and (stringp (cadr err)) (cddr err))))
    (append (list :ok :false
                  :reason (or (plist-get props :code) (symbol-name (car err)))
                  :message (if (stringp (cadr err)) (cadr err) (error-message-string err)))
            (when-let* ((path (plist-get props :path))) (list :path path))
            (when-let* ((index (plist-get props :index))) (list :index index)))))

(defun financial-chart-compose-main ()
  "Batch entry: compile the chart file in `command-line-args-left'.
Prints the eas spec as JSON, or with --backend text|svg the drawing
\(--cols/--rows, --width/--height).  On failure it prints
`financial-chart-compose-failure' as JSON on stderr and exits 1."
  (let ((args command-line-args-left) file backend width height)
    (setq command-line-args-left nil)
    (while args
      (pcase (pop args)
        ("--" nil)
        ("--backend" (setq backend (intern (pop args))))
        ((or "--cols" "--width") (setq width (string-to-number (pop args))))
        ((or "--rows" "--height") (setq height (string-to-number (pop args))))
        (arg (setq file arg))))
    (condition-case err
        (princ (if backend
                   (substring-no-properties
                    (financial-chart-compose-render file :backend backend :width width :height height))
                 (eas-json-pretty (financial-chart-compose file))))
      (error (message "%s" (eas-json-encode (financial-chart-compose-failure err)))
             (kill-emacs 1)))
    (terpri)))

(provide 'financial-chart-eas-compose)
;;; financial-chart-eas-compose.el ends here
