;;; render-indicator-examples.el --- Regenerate indicator showcase PNGs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name "../.." here))
       (csv-file (expand-file-name "examples/tsmc-daily.csv" root))
       (output-dir (expand-file-name "docs/images/indicators" root)))
  (add-to-list 'load-path (expand-file-name "src" root))
  (require 'financial-chart)
  (require 'financial-chart-svg)
  (let* ((lines (with-temp-buffer
                  (insert-file-contents csv-file)
                  (split-string (string-trim (buffer-string)) "\n" t)))
         (bars (mapcar
                (lambda (line)
                  (pcase-let* ((`(,date ,open ,high ,low ,close ,volume)
                                (split-string line "," t))
                               (`(,month ,day ,year)
                                (mapcar #'string-to-number (split-string date "/")))
                               (time (* 1000 (float-time
                                              (encode-time 0 0 12 day month year)))))
                    (list :open (string-to-number open)
                          :high (string-to-number high)
                          :low (string-to-number low)
                          :close (string-to-number close)
                          :volume (string-to-number volume)
                          :time time)))
                (cdr lines))))
    (make-directory output-dir t)
    (let ((financial-chart-show-volume t)
          (financial-chart-show-x-axis t)
          (financial-chart-svg-price-height 430)
          (financial-chart-svg-oscillator-height 150))
      (cl-labels
          ((save-candle (file title overlays oscillators &optional bands)
             (let ((financial-chart-indicators overlays)
                   (financial-chart-oscillators oscillators)
                   (financial-chart-indicator-bands bands))
               (financial-chart-export-png
                bars (expand-file-name file output-dir) title 1280 760)))
           (save-plot (file title chart-spec)
             (let* ((svg (financial-chart-plot-spec
                          (append chart-spec
                                  (list :backend 'svg :title title
                                        :pixel-width 1280 :pixel-height 760))))
                    (svg-file (make-temp-file "financial-chart-indicator" nil ".svg"))
                    (png-file (expand-file-name file output-dir))
                    (converter (financial-chart--resolve-png-converter)))
               (unwind-protect
                   (progn
                     (with-temp-file svg-file (insert svg))
                     (if (functionp converter)
                         (funcall converter svg-file png-file)
                       (financial-chart--run-png-converter
                        converter svg-file png-file 1280 760)))
                 (when (file-exists-p svg-file) (delete-file svg-file)))))
           (series (name label values)
             (financial-chart-normalize-indicator-series
              (list :name name :label label
                    :values (or (plist-get values :values) values)
                    :timestamps (or (plist-get values :timestamps)
                                    (mapcar (lambda (bar) (plist-get bar :time))
                                            bars)))))
           (multi-data (outputs)
             (mapcar (lambda (output)
                       (cons (plist-get output :label)
                             (financial-chart-indicator-series-data output)))
                     outputs)))
        (save-candle
         "trend-overlays.png" "TSMC — SMA and EMA"
         (list (list :fn (lambda (data) (financial-chart-sma data 8))
                     :label "SMA 8" :face 'font-lock-keyword-face)
               (list :fn (lambda (data) (financial-chart-ema data 5))
                     :label "EMA 5" :face 'font-lock-function-name-face))
         nil nil)
        (save-candle
         "bollinger-bands.png" "TSMC — Bollinger Bands"
         (list (list :fn (lambda (data)
                           (plist-get (nth 2 (financial-chart-bollinger-bands data 10 2))
                                      :values))
                     :label "Lower Band" :face 'font-lock-constant-face
                     :color "#c45b6a")
               (list :fn (lambda (data)
                           (plist-get (nth 1 (financial-chart-bollinger-bands data 10 2))
                                      :values))
                     :label "Middle Band" :face 'font-lock-keyword-face
                     :color "#6b7280")
               (list :fn (lambda (data)
                           (plist-get (nth 0 (financial-chart-bollinger-bands data 10 2))
                                      :values))
                     :label "Upper Band" :face 'font-lock-warning-face
                     :color "#4c9f70"))
         nil
         (list (financial-chart-bollinger-band-spec
                10 2 "#4c9f70" "#c45b6a" 0.16)))
        (save-candle
         "oscillators.png" "TSMC — RSI and Stochastic"
         nil
         (list (list :fn (lambda (data) (financial-chart-rsi data 8))
                     :label "RSI 8" :face 'font-lock-keyword-face)
               (list :fn (lambda (data)
                           (plist-get (car (financial-chart-stochastic data 8 3))
                                      :values))
                     :label "Stoch %K" :face 'font-lock-warning-face)))
        (save-candle
         "trend-strength.png" "TSMC — ADX and Aroon"
         nil
         (list (list :fn (lambda (data)
                           (plist-get (nth 2 (financial-chart-dmi data 5))
                                      :values))
                     :label "ADX 5" :face 'font-lock-keyword-face)
               (list :fn (lambda (data)
                           (plist-get (car (financial-chart-aroon data 8))
                                      :values))
                     :label "Aroon Up" :face 'font-lock-warning-face)))
        (save-plot
         "macd.png" "TSMC — MACD"
         (list :kind 'multi :data
               (multi-data (financial-chart-indicator-evaluate 'macd bars 5 10 4))))
        (save-plot
         "atr.png" "TSMC — Average True Range"
         (list :kind 'line
               :data (financial-chart-indicator-series-data
                      (series 'atr "ATR 8"
                              (financial-chart-indicator-evaluate 'atr bars 8)))))
        (save-plot
         "money-flow.png" "TSMC — Chaikin Money Flow"
         (list :kind 'line
               :data (financial-chart-indicator-series-data
                      (series 'cmf "CMF 10"
                              (financial-chart-indicator-evaluate
                               'chaikin-money-flow bars 10)))))
        (save-plot
         "on-balance-volume.png" "TSMC — On-Balance Volume"
         (list :kind 'line
               :data (financial-chart-indicator-series-data
                      (series 'obv "OBV"
                              (financial-chart-indicator-evaluate 'obv bars)))))
        (princ (format "Wrote 8 indicator charts from %d TSMC daily bars to %s\n"
                       (length bars) output-dir))))))
;;; render-indicator-examples.el ends here
