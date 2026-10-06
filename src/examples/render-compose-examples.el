;;; render-compose-examples.el --- Regenerate the composed-chart examples -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Writes examples/compose/STYLE.json, one chart description per price
;; style of `financial-chart-compose', over the same 80 synthetic daily
;; bars (a seeded random walk, so the files never change by accident).
;; Run from the repository root:
;;
;;   emacs -Q --batch -L ../eas.el/src -L src -L src/core -L src/indicators \
;;     -L src/renderers -L src/charts -L src/integrations \
;;     -l src/examples/render-compose-examples.el

;;; Code:

(require 'cl-lib)
(require 'financial-chart)

(defun render-compose-examples--bars (n)
  "N synthetic weekday bars from 2026-03-02, a seeded random walk."
  (let ((seed 20261006) (close 100.0) (day (eas-time-ms 2026 3 2)) bars)
    (cl-flet ((rand () (setq seed (mod (+ (* seed 1103515245) 12345) 2147483648))
                (/ seed 2147483648.0)))
      (dotimes (i n)
        (let* ((drift (* 0.9 (sin (/ i 9.0))))
               (open close)
               (next (/ (fround (* (+ open drift (* 2.4 (- (rand) 0.5))) 100)) 100))
               (high (+ (max open next) (* 1.2 (rand))))
               (low (- (min open next) (* 1.2 (rand)))))
          (push (list :time (format-time-string "%Y-%m-%d" (/ day 1000) t)
                      :open open :high (/ (fround (* high 100)) 100)
                      :low (/ (fround (* low 100)) 100) :close next
                      :volume (+ 800000 (round (* 900000 (rand)))))
                bars)
          (setq day (+ day (* 86400000 (if (= (mod i 5) 4) 3 1))))
          (setq close next)))
      (nreverse bars))))

(defconst render-compose-examples--charts
  '(("candles" "Candlesticks, SMA 10/30 cross fill, volume, RSI and MACD"
     (:style "candles"
      :series [(:indicator "sma" :params [10]) (:indicator "sma" :params [30] :dash [4 2])]
      :fills [(:between ["sma-10" "sma-30"] :above "#26a69a" :below "#ef5350" :opacity 0.25)])
     [(:volume t :height 50)
      (:series [(:indicator "rsi" :params [14])] :rules [30 70]
       :fills [(:between ["rsi-14" 70] :color "#ef5350" :opacity 0.3)])
      (:series [(:indicator "macd" :output "macd") (:indicator "macd" :output "macd-signal")
                (:indicator "macd" :output "macd-histogram" :above "#26a69a" :below "#ef5350")]
       :rules [0])])
    ("hollow" "Hollow candles inside shaded Bollinger Bands"
     (:style "hollow"
      :series [(:indicator "bollinger-bands" :params [20 2] :id "bb" :width 1)
               (:indicator "ema" :params [9] :width 2)]
      :fills [(:between ["bb.bollinger-upper" "bb.bollinger-lower"] :color "#00897b" :opacity 0.12)])
     [])
    ("heikin-ashi" "Heikin-Ashi candles with EMA 20 and volume"
     (:style "heikin-ashi" :series [(:indicator "ema" :params [20])])
     [(:volume t)])
    ("ohlc" "OHLC bars with SMA 20 and a slow stochastic"
     (:style "ohlc" :series [(:indicator "sma" :params [20])])
     [(:series [(:indicator "stochastic")] :rules [20 80] :domain [0 100])])
    ("line" "Close line with a precomputed series and VWAP"
     (:style "line" :color "#263238" :width 2
      :series [(:indicator "vwap" :dash [2 2]) (:field "high" :label "high" :color "#90a4ae" :width 1)])
     [])
    ("step" "Step line of closes with Parabolic SAR dots"
     (:style "step" :series [(:indicator "parabolic-sar")])
     [])
    ("area" "Mountain of closes with EMA 20 and EMA 50"
     (:style "area" :series [(:indicator "ema" :params [20]) (:indicator "ema" :params [50])])
     [(:volume t :height 50)])
    ("baseline" "Closes against a 100 baseline, RSI underneath"
     (:style "baseline" :baseline 100)
     [(:series ["rsi"] :rules [(:y 50 :color "#607d8b" :dash [1 1])]
       :fills [(:between ["rsi" 50] :above "#26a69a" :below "#ef5350" :opacity 0.3)])]))
  "(STYLE TITLE PRICE PANES) for each example.")

(let* ((root (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))
       (dir (expand-file-name "examples/compose" root))
       (bars (render-compose-examples--bars 80)))
  (make-directory dir t)
  (pcase-dolist (`(,style ,title ,price ,panes) render-compose-examples--charts)
    (let ((chart (list :title title :bars (vconcat bars) :price price :panes panes)))
      (when (equal style "line")
        ;; A series computed elsewhere: one value per bar, null for none.
        (setf (plist-get chart :price)
              (plist-put (copy-sequence price) :series
                         (vconcat (plist-get price :series)
                                  (list (list :values (vconcat (cl-loop for b in bars for i from 0
                                                                        collect (if (< i 5) :null
                                                                                  (+ (plist-get b :close) 1.5))))
                                              :label "model" :id "model" :color "#e91e63"))))))
      (financial-chart-compose chart)
      (with-temp-file (expand-file-name (concat style ".json") dir)
        (insert (eas-json-pretty chart) "\n"))
      (message "wrote examples/compose/%s.json" style))))

;;; render-compose-examples.el ends here
