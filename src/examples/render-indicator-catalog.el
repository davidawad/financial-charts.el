;;; render-indicator-catalog.el --- Regenerate the indicator catalog examples -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Writes examples/indicators/NAME.json: one composed chart per study
;; of `financial-chart-studies' and one per kind of annotation, over the
;; same 100 synthetic daily bars (a seeded random walk, so the files
;; never change by accident).  Run from the repository root:
;;
;;   emacs -Q --batch -L ../eas.el/src -L src -L src/core -L src/indicators \
;;     -L src/renderers -L src/charts -L src/integrations \
;;     -l src/examples/render-indicator-catalog.el

;;; Code:

(require 'cl-lib)
(require 'financial-chart)

(defun render-indicator-catalog--bars (n)
  "N synthetic weekday bars from 2026-02-02, a seeded random walk."
  (let ((seed 20261007) (close 100.0) (day (eas-time-ms 2026 2 2)) bars)
    (cl-flet ((rand () (setq seed (mod (+ (* seed 1103515245) 12345) 2147483648))
                (/ seed 2147483648.0)))
      (dotimes (i n)
        (let* ((drift (* 1.1 (sin (/ i 11.0))))
               (open close)
               (next (/ (fround (* (+ open drift (* 2.6 (- (rand) 0.5))) 100)) 100))
               (high (+ (max open next) (* 1.3 (rand))))
               (low (- (min open next) (* 1.3 (rand)))))
          (push (list :time (format-time-string "%Y-%m-%d" (/ day 1000) t)
                      :open open :high (/ (fround (* high 100)) 100)
                      :low (/ (fround (* low 100)) 100) :close next
                      :volume (+ 700000 (round (* 1100000 (rand)))))
                bars)
          (setq day (+ day (* 86400000 (if (= (mod i 5) 4) 3 1))))
          (setq close next)))
      (nreverse bars))))

(defconst render-indicator-catalog--charts
  '(("ichimoku" "Ichimoku cloud: spans 26 bars ahead, chikou 26 back" (:studies ["ichimoku"]) [])
    ("bollinger" "Bollinger Bands (20, 2) with the channel shaded" (:studies ["bollinger"]) [(:study "volume" :height 50)])
    ("keltner" "Keltner Channels (EMA 20, ATR 10 x2)" (:studies ["keltner"]) [])
    ("donchian" "Donchian Channels (20)" (:studies ["donchian"]) [])
    ("envelopes" "SMA 20 envelopes at 3%" (:studies [(:study "envelopes" :params [20 3])]) [])
    ("vwap-bands" "Cumulative VWAP with 1 and 2 deviation bands" (:studies ["vwap-bands"]) [(:study "volume" :height 50)])
    ("pivots" "Classic weekly pivot points" (:studies [(:study "pivots" :params ["classic" "week"])]) [])
    ("supertrend" "SuperTrend (10, 3)" (:studies ["supertrend"]) [])
    ("psar" "Parabolic SAR dots either side of price" (:studies ["psar"]) [])
    ("ma-ribbon" "EMA ribbon 10 to 60" (:style "line" :studies ["ma-ribbon"]) [])
    ("rsi" "RSI 14 with 70/30 levels, zone and excursions" nil [(:study "rsi" :height 110)])
    ("stochastic" "Slow stochastic with 80/20 levels" nil [(:study "stochastic" :height 110)])
    ("macd" "MACD 12/26/9 with an up/down histogram" nil [(:study "macd" :height 110)])
    ("adx" "ADX with +DI and -DI" nil [(:study "adx" :height 110)])
    ("cci" "CCI 20 with +/-100 levels" nil [(:study "cci" :height 110)])
    ("williams-r" "Williams %R 14" nil [(:study "williams-r" :height 110)])
    ("mfi" "Money Flow Index 14" nil [(:study "mfi" :height 110)])
    ("ultimate-oscillator" "Ultimate Oscillator" nil [(:study "ultimate-oscillator" :height 110)])
    ("aroon" "Aroon up and down (25)" nil [(:study "aroon" :height 110)])
    ("obv" "On-balance volume" nil [(:study "volume" :height 50) (:study "obv" :height 90)])
    ("atr" "Average true range (14)" nil [(:study "atr" :height 90)])
    ("roc" "Rate of change (12) around zero" nil [(:study "roc" :params [12] :height 90)])
    ("momentum" "Momentum (10) around zero" nil [(:study "momentum" :params [10] :height 90)])
    ("cmf" "Chaikin money flow (20)" nil [(:study "cmf" :height 90)])
    ("volume" "Volume bars coloured like their candles" nil [(:study "volume" :height 70)])
    ("markers" "Sell and buy where SMA 10 crosses SMA 30"
     (:series [(:indicator "sma" :params [10]) (:indicator "sma" :params [30] :dash [4 2])]
      :annotations [(:type "sell" :at "2026-04-08" :label "sell")
                    (:type "buy" :at ["2026-05-26"] :label "buy")])
     [])
    ("annotations" "Levels, a trend line, an event, a box and a note"
     (:annotations [(:type "level" :y 127.3 :label "resistance" :color "#ef5350" :from "2026-03-09" :to "2026-04-17")
                    (:type "level" :y 106.2 :label "support" :color "#26a69a" :from "2026-04-27")
                    (:type "trendline" :from ["2026-05-11" 106.2] :to ["2026-06-01" 117.7] :extend "right"
                     :label "trend")
                    (:type "event" :at "2026-04-16" :label "earnings")
                    (:type "box" :from ["2026-05-04" 106] :to ["2026-05-15" 108.6] :label "base")
                    (:type "text" :at "2026-02-23" :y 118 :label "breakout")])
     [(:study "rsi" :height 80 :annotations [(:type "event" :at "2026-04-16")])])
    ("fibonacci" "Fibonacci retracement of the largest swing"
     (:annotations [(:type "fibonacci")])
     []))
  "(NAME TITLE PRICE PANES) for each example; PRICE adds to candles.")

(let* ((root (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))
       (dir (expand-file-name "examples/indicators" root))
       (bars (vconcat (render-indicator-catalog--bars 100))))
  (make-directory dir t)
  (pcase-dolist (`(,name ,title ,price ,panes) render-indicator-catalog--charts)
    (let ((chart (list :title title :bars bars
                       :price (append (unless (plist-get price :style) (list :style "candles")) price)
                       :panes panes)))
      (financial-chart-compose chart)
      (with-temp-file (expand-file-name (concat name ".json") dir)
        (insert (eas-json-pretty chart) "\n"))
      (message "wrote examples/indicators/%s.json" name))))

;;; render-indicator-catalog.el ends here
