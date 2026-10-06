;;; financial-chart-eas-styles.el --- price styles and fills as eas layers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The layer builders behind `financial-chart-compose' (fc-gbo.2):
;;
;;   price styles  candles, hollow, heikin-ashi, ohlc, line, step, area
;;                 and baseline, each a list of plain Vega-Lite layers
;;                 over the chart's rows (plus derived columns, such as
;;                 Heikin-Ashi's, computed here from the supplied bars)
;;   fills         the area between two series, one colour or two: the
;;                 crossings are interpolated into the rows, so a fill
;;                 that is one colour where A is above B and another
;;                 where it is below switches exactly where they cross
;;
;; Everything here is pure: bars in, plists out.  The rows carry the x
;; position as a number (epoch ms, or the bar index without times).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-core)

(define-error 'financial-chart-invalid-chart
  "financial-chart: invalid chart description" 'financial-chart-error)

(defconst financial-chart-styles
  '(("candles" . "Japanese candlesticks: a low-high wick and an open-close body, filled, up/down coloured")
    ("hollow" . "hollow candlesticks: rising bodies outlined, falling bodies filled")
    ("heikin-ashi" . "Heikin-Ashi candles, averaged from the supplied bars")
    ("ohlc" . "OHLC bars: a low-high rule with an open tick left and a close tick right")
    ("line" . "a line through each bar's close (or FIELD)")
    ("step" . "a step line: each close held until the next bar")
    ("area" . "a mountain: the close line with the area under it filled")
    ("baseline" . "the close against a BASELINE level, filled ABOVE/BELOW colours either side"))
  "Price styles of `financial-chart-compose' with what each draws.")

;;; Rows and encodings

(defun financial-chart-styles-x (ctx)
  "The x encoding of CTX's rows.
CTX's :x-title titles the axis; :x-hidden drops its labels (a pane
above the bottom one)."
  (append (list :field "time" :type (plist-get ctx :x-type)
                :title (or (plist-get ctx :x-title) :null))
          (when (plist-get ctx :x-hidden) (list :axis '(:labels :false)))))

(defun financial-chart-styles-y (field &optional title)
  "A quantitative y encoding of FIELD with an unzeroed scale and TITLE."
  (list :field field :type "quantitative" :scale '(:zero :false)
        :title (or title :null)))

(defun financial-chart-styles-up-down (ctx open close)
  "A colour encoding: CTX's up colour when CLOSE >= OPEN, else down."
  (list :condition (list :test (format "datum.%s >= datum.%s" close open)
                         :value (plist-get ctx :up))
        :value (plist-get ctx :down)))

(defun financial-chart-styles-line-mark (type color width dash &rest more)
  "A TYPE mark plist with COLOR, stroke WIDTH, DASH and MORE properties."
  (append (list :type type :color color)
          (when width (list :strokeWidth width))
          (when (and dash (> (length dash) 0)) (list :strokeDash (vconcat dash)))
          more))

;;; Heikin-Ashi

(defun financial-chart-styles-heikin-ashi (bars)
  "Heikin-Ashi columns of BARS: an alist (FIELD . VECTOR).
ha_close is the bar's OHLC mean; ha_open the mean of the previous
Heikin-Ashi open and close (the first bar's open and close); ha_high
and ha_low extend the high and low to the Heikin-Ashi body."
  (let (opens closes highs lows prev-open prev-close)
    (dolist (bar bars)
      (let* ((o (plist-get bar :open)) (h (plist-get bar :high))
             (l (plist-get bar :low)) (c (plist-get bar :close))
             (ha-close (/ (+ o h l c) 4.0))
             (ha-open (if prev-open (/ (+ prev-open prev-close) 2.0) (/ (+ o c) 2.0))))
        (push ha-open opens) (push ha-close closes)
        (push (max h ha-open ha-close) highs) (push (min l ha-open ha-close) lows)
        (setq prev-open ha-open prev-close ha-close)))
    (list (cons "ha_open" (vconcat (nreverse opens)))
          (cons "ha_high" (vconcat (nreverse highs)))
          (cons "ha_low" (vconcat (nreverse lows)))
          (cons "ha_close" (vconcat (nreverse closes))))))

;;; Candles and bars

(defun financial-chart-styles--candles (ctx o h l c &optional hollow)
  "Candle layers of CTX over fields O H L C; HOLLOW outlines rising bodies."
  (let ((x (financial-chart-styles-x ctx))
        (colour (financial-chart-styles-up-down ctx o c))
        (up (format "datum.%s >= datum.%s" c o)))
    (append
     (list (list :name "wicks" :mark "rule"
                 :encoding (list :x x :y (financial-chart-styles-y l "price")
                                 :y2 (list :field h) :color colour)))
     (if hollow
         (list (list :name "bodies-up" :transform (vector (list :filter up))
                     :mark (list :type "bar" :fill "white" :stroke (plist-get ctx :up)
                                 :strokeWidth 1)
                     :encoding (list :x x :y (financial-chart-styles-y o) :y2 (list :field c)))
               (list :name "bodies-down" :transform (vector (list :filter (format "!(%s)" up)))
                     :mark (list :type "bar" :color (plist-get ctx :down))
                     :encoding (list :x x :y (financial-chart-styles-y o) :y2 (list :field c))))
       (list (list :name "bodies" :mark "bar"
                   :encoding (list :x x :y (financial-chart-styles-y o) :y2 (list :field c)
                                   :color colour)))))))

(defun financial-chart-styles--tick-width (xs)
  "Half the median gap between consecutive XS, times 0.8: an OHLC tick."
  (let ((gaps (sort (cl-loop for (a b) on (append xs nil) while b collect (- b a)) #'<)))
    (* 0.4 (if gaps (nth (/ (length gaps) 2) gaps) 1))))

(defun financial-chart-styles--ohlc (ctx)
  "OHLC bar layers of CTX: a low-high rule, open tick left, close tick right."
  (let* ((x (financial-chart-styles-x ctx))
         (colour (financial-chart-styles-up-down ctx "open" "close"))
         (w (financial-chart-styles--tick-width (plist-get ctx :xs))))
    (list (list :name "ranges" :mark "rule"
                :encoding (list :x x :y (financial-chart-styles-y "low" "price")
                                :y2 (list :field "high") :color colour))
          (list :name "opens" :transform (vector (list :calculate (format "datum.time - %s" w)
                                                       :as "tick_left"))
                :mark "rule"
                :encoding (list :x (list :field "tick_left" :type (plist-get ctx :x-type))
                                :x2 (list :field "time")
                                :y (financial-chart-styles-y "open") :color colour))
          (list :name "closes" :transform (vector (list :calculate (format "datum.time + %s" w)
                                                        :as "tick_right"))
                :mark "rule"
                :encoding (list :x x :x2 (list :field "tick_right")
                                :y (financial-chart-styles-y "close") :color colour)))))

;;; Lines

(defun financial-chart-styles--price-line (ctx price type &rest more)
  "A TYPE (line or area) layer of PRICE's field in CTX with MORE mark props."
  (let ((field (or (plist-get price :field) "close")))
    (list :name (format "price-%s" type)
          :mark (apply #'financial-chart-styles-line-mark type
                       (or (plist-get price :color) (plist-get ctx :price-color))
                       (plist-get price :width) (plist-get price :dash) more)
          :encoding (list :x (financial-chart-styles-x ctx)
                          :y (financial-chart-styles-y field "price")))))

(defun financial-chart-styles-rule-layer (y color dash width &optional name)
  "A horizontal rule at Y drawn in COLOR, DASH and WIDTH, named NAME."
  (append (when name (list :name name))
          (list :data (list :values (vector (list :level y)))
                :mark (financial-chart-styles-line-mark "rule" color width dash)
                :encoding (list :y (list :field "level" :type "quantitative")))))

(defun financial-chart-styles-price (ctx price)
  "PRICE's style as (:columns ALIST :layers LIST) over CTX's rows.
CTX carries :bars :xs :x-type :up :down :price-color and :closes.
PRICE is the price pane: :style, :field, :color, :width, :dash,
:baseline, :above, :below."
  (let ((style (or (plist-get price :style) "candles")))
    (pcase style
      ("candles" (list :layers (financial-chart-styles--candles ctx "open" "high" "low" "close")))
      ("hollow" (list :layers (financial-chart-styles--candles ctx "open" "high" "low" "close" t)))
      ("heikin-ashi"
       (list :columns (financial-chart-styles-heikin-ashi (plist-get ctx :bars))
             :layers (financial-chart-styles--candles ctx "ha_open" "ha_high" "ha_low" "ha_close")))
      ("ohlc" (list :layers (financial-chart-styles--ohlc ctx)))
      ("line" (list :layers (list (financial-chart-styles--price-line ctx price "line"))))
      ("step" (list :layers (list (financial-chart-styles--price-line
                                   ctx price "line" :interpolate "step-after"))))
      ("area" (list :layers (list (financial-chart-styles--price-line ctx price "area" :opacity 0.25)
                                  (financial-chart-styles--price-line ctx price "line"))))
      ("baseline" (financial-chart-styles--baseline ctx price))
      (_ (signal 'financial-chart-invalid-chart
                 (list (format "Unknown price style %S; styles: %s" style
                               (mapconcat #'car financial-chart-styles ", "))
                       :code "UNKNOWN_STYLE" :path "/price/style" :field "style"))))))

(defun financial-chart-styles--baseline (ctx price)
  "Baseline style of PRICE in CTX: fills either side of a level, then the line."
  (let* ((field (or (plist-get price :field) "close"))
         (values (plist-get ctx :closes))
         (level (or (plist-get price :baseline)
                    (seq-find #'numberp values)))
         (fill (list :above (or (plist-get price :above) (plist-get ctx :up))
                     :below (or (plist-get price :below) (plist-get ctx :down))
                     :opacity 0.3)))
    (list :layers
          (append (financial-chart-styles-fill-layers
                   ctx (financial-chart-styles-fill-rows
                        (plist-get ctx :xs) values (make-vector (length values) level))
                   fill "baseline")
                  (list (financial-chart-styles-rule-layer level "gray" [3 3] 1 "baseline")
                        (financial-chart-styles--price-line
                         ctx (append (list :field field) price) "line"))))))

;;; Fills between two series

(defun financial-chart-styles--cross (x0 a0 b0 x1 a1 b1)
  "The row where A crosses B between (X0 A0 B0) and (X1 A1 B1), or nil."
  (let ((d0 (- a0 b0)) (d1 (- a1 b1)))
    (when (< (* d0 d1) 0)
      (let* ((f (/ (float d0) (- d0 d1)))
             (x (+ x0 (* f (- x1 x0))))
             (y (+ a0 (* f (- a1 a0)))))
        (list :time x :a y :b y :lo y)))))

(defun financial-chart-styles-fill-rows (xs as bs)
  "Rows {time, a, b, lo, seg} for the fill between series AS and BS.
XS, AS and BS are aligned sequences; a nil (or :null) in either series
starts a new segment SEG.  Where A and B cross between two bars a row
at the interpolated crossing is inserted, so a two-colour fill switches
exactly there.  lo is min(a, b)."
  (let ((seg -1) prev rows)
    (cl-loop for x across (vconcat xs) for a across (vconcat as) for b across (vconcat bs)
             do (if (not (and (numberp a) (numberp b)))
                    (setq prev nil)
                  (unless prev (setq seg (1+ seg)))
                  (when-let* ((cross (and prev (apply #'financial-chart-styles--cross
                                                      (append prev (list x a b))))))
                    (push (append cross (list :seg seg)) rows))
                  (push (list :time x :a a :b b :lo (min a b) :seg seg) rows)
                  (setq prev (list x a b))))
    (nreverse rows)))

(defun financial-chart-styles-fill-layers (ctx rows fill &optional name)
  "Area layers shading ROWS (from `financial-chart-styles-fill-rows') in CTX.
FILL has :color (one colour) or :above and :below (A above B, A below
B), and :opacity (default 0.2).  NAME prefixes the layer names."
  (let* ((name (or name "fill"))
         (opacity (or (plist-get fill :opacity) 0.2))
         (x (financial-chart-styles-x ctx))
         (data (list :values (vconcat rows)))
         (detail (when (> (cl-reduce #'max rows :key (lambda (r) (plist-get r :seg))
                                     :initial-value 0)
                          0)
                   (list :detail (list :field "seg" :type "nominal"))))
         (area (lambda (suffix color y y2)
                 (list :name (format "%s-%s" name suffix) :data data
                       :mark (list :type "area" :color color :opacity opacity)
                       :encoding (append (list :x x :y (financial-chart-styles-y y)
                                               :y2 (list :field y2))
                                         detail)))))
    (when rows
      (if (plist-get fill :color)
          (list (funcall area "band" (plist-get fill :color) "a" "b"))
        (list (funcall area "above" (or (plist-get fill :above) (plist-get ctx :up)) "a" "lo")
              (funcall area "below" (or (plist-get fill :below) (plist-get ctx :down)) "b" "lo"))))))

(provide 'financial-chart-eas-styles)
;;; financial-chart-eas-styles.el ends here
