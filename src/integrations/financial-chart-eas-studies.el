;;; financial-chart-eas-studies.el --- the indicator catalog of composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Studies (fc-gbo.3): one word in a chart description that stands for
;; the series, fills, reference rules and zones a trader expects of an
;; indicator.  "bollinger" is the three bands with the channel shaded;
;; "rsi" is a pane with RSI, 70/30 rules, the zone between them and the
;; overbought/oversold excursions filled.  Each study is a function of
;; its context (`financial-chart-catalog-expand' builds it) returning a
;; pane fragment in the composition DSL itself:
;;
;;   (:series [...] :fills [...] :rules [...] :title T :domain D :volume V)
;;
;; so a study adds nothing the DSL cannot say by hand.  Overlays go in
;; the price pane's "studies"; oscillators are one pane each.  Colours
;; come from `financial-chart-palette' and the chart's up/down colours.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-indicator-api)
(require 'financial-chart-eas-palette)
(require 'financial-chart-overlay-indicators)

(defcustom financial-chart-studies-ribbon-colors '("#2962ff" "#e91e63")
  "First and last colour of a moving-average ribbon, fastest first."
  :type '(list string string)
  :group 'financial-chart-palette)

(defconst financial-chart-studies-rule-color "#9e9e9e"
  "Colour of a study's reference levels.")

(defun financial-chart-studies--colour (i)
  "Palette colour I."
  (nth (mod i (length financial-chart-palette)) financial-chart-palette))

(defun financial-chart-studies--mix (from to f)
  "The colour F of the way from hex FROM to hex TO."
  (cl-flet ((rgb (hex) (mapcar (lambda (i) (string-to-number (substring hex i (+ i 2)) 16)) '(1 3 5))))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (a b) (round (+ a (* f (- b a))))) (rgb from) (rgb to)))))

(defun financial-chart-studies--line (ctx output suffix &rest props)
  "A series item of CTX's indicator OUTPUT with id ID.SUFFIX and PROPS."
  (append (list :indicator (symbol-name (plist-get ctx :indicator))
                :params (vconcat (plist-get ctx :params)))
          (when output (list :output output))
          (list :id (if suffix (concat (plist-get ctx :id) "." suffix) (plist-get ctx :id)))
          props))

(defun financial-chart-studies--id (ctx suffix)
  "The id of CTX's series SUFFIX."
  (if suffix (concat (plist-get ctx :id) "." suffix) (plist-get ctx :id)))

;;; Overlays

(defun financial-chart-studies--channel (ctx outputs colour)
  "Upper, middle and lower OUTPUTS of CTX in COLOUR, the channel shaded."
  (pcase-let ((`(,upper ,middle ,lower) outputs))
    (list :series (list (financial-chart-studies--line ctx upper "upper" :color colour :width 1)
                        (financial-chart-studies--line ctx middle "middle" :color colour :width 1
                                                       :dash [4 2])
                        (financial-chart-studies--line ctx lower "lower" :color colour :width 1))
          :fills (list (list :between (vector (financial-chart-studies--id ctx "upper")
                                              (financial-chart-studies--id ctx "lower"))
                             :color colour :opacity 0.08)))))

(defun financial-chart-studies-ichimoku (ctx)
  "Ichimoku in CTX: five lines, the cloud coloured by which span leads."
  (let ((up (plist-get ctx :up)) (down (plist-get ctx :down))
        ;; Explicit, so supplied "values" for the spans shift too.
        (shift (or (nth 3 (plist-get ctx :params)) 26)))
    (list :series (list (financial-chart-studies--line ctx "ichimoku-tenkan" "tenkan"
                                                       :color (financial-chart-studies--colour 0) :width 1.2)
                        (financial-chart-studies--line ctx "ichimoku-kijun" "kijun"
                                                       :color (financial-chart-studies--colour 2) :width 1.2)
                        (financial-chart-studies--line ctx "ichimoku-chikou" "chikou"
                                                       :color (financial-chart-studies--colour 7) :width 1
                                                       :shift (- shift))
                        (financial-chart-studies--line ctx "ichimoku-senkou-a" "senkou-a" :color up :width 1
                                                       :shift shift)
                        (financial-chart-studies--line ctx "ichimoku-senkou-b" "senkou-b" :color down :width 1
                                                       :shift shift))
          :fills (list (list :between (vector (financial-chart-studies--id ctx "senkou-a")
                                              (financial-chart-studies--id ctx "senkou-b"))
                             :above up :below down :opacity 0.15)))))

(defun financial-chart-studies-vwap-bands (ctx)
  "VWAP in CTX with each band pair, the inner band shaded most."
  (let* ((colour (financial-chart-studies--colour 4))
         (ms (or (plist-get ctx :params) '(1 2)))
         (tags (mapcar (lambda (m) (format "%s" m)) ms)))
    (list :series (cons (financial-chart-studies--line ctx "vwap" "vwap" :color colour :width 2)
                        (cl-loop for tag in tags
                                 append (list (financial-chart-studies--line
                                               ctx (concat "vwap-upper-" tag) (concat "upper-" tag)
                                               :color colour :width 1 :dash [2 2])
                                              (financial-chart-studies--line
                                               ctx (concat "vwap-lower-" tag) (concat "lower-" tag)
                                               :color colour :width 1 :dash [2 2]))))
          :fills (cons (list :between (vector (financial-chart-studies--id ctx (concat "upper-" (car tags)))
                                              (financial-chart-studies--id ctx (concat "lower-" (car tags))))
                             :color colour :opacity 0.1)
                       ;; Each further band pair shades the gap to the one inside it.
                       (cl-loop for (inner outer) on tags while outer
                                append (cl-loop for side in '("upper-" "lower-")
                                                collect (list :between
                                                              (vector (financial-chart-studies--id ctx (concat side outer))
                                                                      (financial-chart-studies--id ctx (concat side inner)))
                                                              :color colour :opacity 0.05)))))))

(defun financial-chart-studies-pivots (ctx)
  "Pivot points in CTX: P grey, resistances down-coloured, supports up."
  (list :series
        (cl-loop for (level colour dash) in `(("pp" ,(financial-chart-studies--colour 10) nil)
                                              ("r1" ,(plist-get ctx :down) nil)
                                              ("r2" ,(plist-get ctx :down) [4 2])
                                              ("r3" ,(plist-get ctx :down) [1 2])
                                              ("s1" ,(plist-get ctx :up) nil)
                                              ("s2" ,(plist-get ctx :up) [4 2])
                                              ("s3" ,(plist-get ctx :up) [1 2]))
                 collect (append (financial-chart-studies--line ctx (concat "pivot-" level) level
                                                                :color colour :width 1 :style "step")
                                 (when dash (list :dash dash))))))

(defun financial-chart-studies-supertrend (ctx)
  "SuperTrend in CTX: the up and down legs, the gap to price shaded."
  (let ((up (plist-get ctx :up)) (down (plist-get ctx :down)))
    (list :series (list (financial-chart-studies--line ctx "supertrend-up" "up" :color up :width 2)
                        (financial-chart-studies--line ctx "supertrend-down" "down" :color down :width 2))
          :fills (list (list :between (vector "close" (financial-chart-studies--id ctx "up"))
                             :color up :opacity 0.1)
                       (list :between (vector "close" (financial-chart-studies--id ctx "down"))
                             :color down :opacity 0.1)))))

(defun financial-chart-studies-psar (ctx)
  "Parabolic SAR dots in CTX, up-coloured under price and down-coloured over it."
  (let* ((bars (plist-get ctx :bars))
         (sar (apply #'financial-chart-parabolic-sar bars (plist-get ctx :params)))
         (side (lambda (below)
                 (vconcat (cl-mapcar (lambda (v b) (if (and v (eq below (< v (plist-get b :close)))) v :null))
                                     sar bars)))))
    (list :series (list (list :values (funcall side t) :label "SAR below" :id (financial-chart-studies--id ctx "below")
                              :style "dots" :color (plist-get ctx :up))
                        (list :values (funcall side nil) :label "SAR above" :id (financial-chart-studies--id ctx "above")
                              :style "dots" :color (plist-get ctx :down))))))

(defun financial-chart-studies-ribbon (ctx)
  "A moving-average ribbon in CTX: one line per period, fast to slow."
  (pcase-let* ((params (plist-get ctx :params))
               (kind (if (stringp (car params)) (car params) "ema"))
               (periods (or (if (stringp (car params)) (cdr params) params) '(10 20 30 40 50 60)))
               (`(,from ,to) financial-chart-studies-ribbon-colors)
               (n (length periods)))
    (list :series (cl-loop for p in periods for i from 0
                           collect (list :indicator kind :params (vector p)
                                         :id (financial-chart-studies--id ctx (format "%s" p))
                                         :color (financial-chart-studies--mix from to (if (> n 1) (/ i (float (1- n))) 0))
                                         :width 1.2)))))

;;; Oscillators

(defun financial-chart-studies--bands (ctx line hi lo mid colour)
  "Rules at HI and LO (and MID) of CTX, the zone between, LINE's excursions."
  (let ((line (financial-chart-studies--id ctx line)))
    (list :rules (append (list (list :y hi :color financial-chart-studies-rule-color :dash [4 2])
                               (list :y lo :color financial-chart-studies-rule-color :dash [4 2]))
                         (when mid (list (list :y mid :color financial-chart-studies-rule-color :dash [1 3]))))
          :fills (list (list :between (vector hi lo) :color colour :opacity 0.06)
                       (list :between (vector line hi) :above (plist-get ctx :up) :below "none" :opacity 0.35)
                       (list :between (vector line lo) :above "none" :below (plist-get ctx :down) :opacity 0.35)))))

(defun financial-chart-studies--levels (ctx hi lo)
  "CTX's \"levels\" as (HI LO), defaulting to HI and LO."
  (or (plist-get ctx :levels) (list hi lo)))

(defun financial-chart-studies--oscillator (output hi lo &optional mid)
  "A study of one OUTPUT line between levels HI and LO (and MID)."
  (lambda (ctx)
    (pcase-let ((`(,hi ,lo) (financial-chart-studies--levels ctx hi lo))
                (colour (financial-chart-palette-home (symbol-name (plist-get ctx :indicator)))))
      (append (list :series (list (financial-chart-studies--line ctx output nil)))
              (financial-chart-studies--bands ctx nil hi lo mid (financial-chart-studies--colour colour))))))

(defun financial-chart-studies-stochastic (ctx)
  "Slow stochastic in CTX: %K and dashed %D between 80 and 20."
  (pcase-let ((`(,hi ,lo) (financial-chart-studies--levels ctx 80 20)))
    (append (list :series (list (financial-chart-studies--line ctx "stochastic-k" "k")
                                (financial-chart-studies--line ctx "stochastic-d" "d" :dash [4 2]
                                                               :color (financial-chart-studies--colour 1))))
            (financial-chart-studies--bands ctx "k" hi lo nil (financial-chart-studies--colour 6)))))

(defun financial-chart-studies-macd (ctx)
  "MACD in CTX: the line, its signal and an up/down histogram on zero."
  (list :series (list (financial-chart-studies--line ctx "macd-histogram" "histogram"
                                                     :above (plist-get ctx :up) :below (plist-get ctx :down))
                      (financial-chart-studies--line ctx "macd" "macd")
                      (financial-chart-studies--line ctx "macd-signal" "signal"))
        :rules (list (list :y 0 :color financial-chart-studies-rule-color :dash [1 3]))))

(defun financial-chart-studies-adx (ctx)
  "ADX in CTX with +DI up-coloured, -DI down-coloured and a 25 trend line."
  (list :series (list (financial-chart-studies--line ctx "adx" "adx" :width 2)
                      (financial-chart-studies--line ctx "plus-di" "plus-di" :color (plist-get ctx :up) :width 1)
                      (financial-chart-studies--line ctx "minus-di" "minus-di" :color (plist-get ctx :down) :width 1))
        :rules (list (list :y (car (or (plist-get ctx :levels) '(25)))
                           :color financial-chart-studies-rule-color :dash [4 2]))))

(defun financial-chart-studies-aroon (ctx)
  "Aroon up (up colour) and down (down colour) between 70 and 30."
  (pcase-let ((`(,hi ,lo) (financial-chart-studies--levels ctx 70 30)))
    (list :series (list (financial-chart-studies--line ctx "aroon-up" "up" :color (plist-get ctx :up))
                        (financial-chart-studies--line ctx "aroon-down" "down" :color (plist-get ctx :down)))
          :rules (cl-loop for (y dash) in `((,hi [4 2]) (,lo [4 2]) (50 [1 3]))
                          collect (list :y y :color financial-chart-studies-rule-color :dash dash)))))

(defun financial-chart-studies--zero-line (output &optional shade)
  "A study of OUTPUT around a zero rule; SHADE fills it up/down from zero."
  (lambda (ctx)
    (append (list :series (list (financial-chart-studies--line ctx output nil))
                  :rules (list (list :y 0 :color financial-chart-studies-rule-color :dash [1 3])))
            (when shade
              (list :fills (list (list :between (vector (plist-get ctx :id) 0) :opacity 0.25
                                       :above (plist-get ctx :up) :below (plist-get ctx :down))))))))

(defun financial-chart-studies--plain (output)
  "A study drawing OUTPUT alone."
  (lambda (ctx) (list :series (list (financial-chart-studies--line ctx output nil)))))

(defconst financial-chart-studies
  `(("ichimoku" price ichimoku financial-chart-studies-ichimoku
     "Tenkan, kijun, chikou and senkou A/B spans 26 bars ahead, the cloud coloured by which leads")
    ("bollinger" price bollinger-bands
     ,(lambda (ctx) (financial-chart-studies--channel
                     ctx '("bollinger-upper" "bollinger-middle" "bollinger-lower")
                     (financial-chart-studies--colour 3)))
     "Bollinger Bands (20, 2): the bands with the channel shaded")
    ("keltner" price keltner-channels
     ,(lambda (ctx) (financial-chart-studies--channel
                     ctx '("keltner-upper" "keltner-middle" "keltner-lower")
                     (financial-chart-studies--colour 8)))
     "Keltner Channels (EMA 20, ATR 10 x2) with the channel shaded")
    ("donchian" price donchian-channels
     ,(lambda (ctx) (financial-chart-studies--channel
                     ctx '("donchian-upper" "donchian-middle" "donchian-lower")
                     (financial-chart-studies--colour 9)))
     "Donchian Channels (20): highest high, lowest low and midpoint, shaded")
    ("envelopes" price envelopes
     ,(lambda (ctx) (financial-chart-studies--channel
                     ctx '("envelope-upper" "envelope-middle" "envelope-lower")
                     (financial-chart-studies--colour 6)))
     "SMA 20 envelopes 2.5% either side, shaded")
    ("vwap-bands" price vwap-bands financial-chart-studies-vwap-bands
     "VWAP with 1 and 2 deviation bands (params: the multipliers)")
    ("pivots" price pivot-points financial-chart-studies-pivots
     "Pivot points (params: classic|fibonacci|woodie|camarilla, day|week|month|year|auto|N)")
    ("supertrend" price supertrend financial-chart-studies-supertrend
     "SuperTrend (10, 3): the trailing stop in the trend's colour, the gap to the close shaded")
    ("psar" price parabolic-sar financial-chart-studies-psar
     "Parabolic SAR (0.02, 0.2) dots, up-coloured under price and down-coloured over it")
    ("ma-ribbon" price nil financial-chart-studies-ribbon
     "Moving-average ribbon (params: [kind] periods..., default ema 10..60), a colour ramp")
    ("rsi" pane rsi ,(financial-chart-studies--oscillator nil 70 30 50)
     "RSI (14) with 70/30 rules, the zone between and the excursions filled (levels: [hi, lo])")
    ("stochastic" pane stochastic financial-chart-studies-stochastic
     "Slow stochastic %K and %D with 80/20 rules and zone (levels: [hi, lo])")
    ("macd" pane macd financial-chart-studies-macd
     "MACD (12, 26, 9): line, signal and up/down-coloured histogram on a zero rule")
    ("adx" pane dmi financial-chart-studies-adx
     "ADX with +DI and -DI and a 25 trend-strength rule (levels: [y])")
    ("cci" pane cci ,(financial-chart-studies--oscillator nil 100 -100 0)
     "CCI (20) with +/-100 rules, zone and excursions (levels: [hi, lo])")
    ("williams-r" pane williams-r ,(financial-chart-studies--oscillator nil -20 -80)
     "Williams %R (14) with -20/-80 rules and zone (levels: [hi, lo])")
    ("mfi" pane money-flow-index ,(financial-chart-studies--oscillator nil 80 20)
     "Money Flow Index (14) with 80/20 rules and zone (levels: [hi, lo])")
    ("ultimate-oscillator" pane ultimate-oscillator ,(financial-chart-studies--oscillator nil 70 30)
     "Ultimate Oscillator with 70/30 rules and zone (levels: [hi, lo])")
    ("aroon" pane aroon financial-chart-studies-aroon
     "Aroon up and down (25) with 70/30 rules (levels: [hi, lo])")
    ("obv" pane obv ,(financial-chart-studies--plain nil) "On-balance volume")
    ("atr" pane atr ,(financial-chart-studies--plain nil) "Average true range (14)")
    ("roc" pane roc ,(financial-chart-studies--zero-line nil) "Rate of change around zero")
    ("momentum" pane momentum ,(financial-chart-studies--zero-line nil) "Momentum around zero")
    ("cmf" pane chaikin-money-flow ,(financial-chart-studies--zero-line nil t)
     "Chaikin money flow, shaded up above zero and down below")
    ("volume" pane nil ,(lambda (_ctx) (list :volume t)) "Volume bars coloured like their candle"))
  "The study catalog: (NAME PLACE INDICATOR FUNCTION DOC).
PLACE is `price' (an overlay in the price pane's \"studies\") or
`pane' (a pane of its own).  INDICATOR names the registered indicator
the study evaluates, if any; FUNCTION maps the study context to a pane
fragment.")

(provide 'financial-chart-eas-studies)
;;; financial-chart-eas-studies.el ends here
