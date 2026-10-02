;;; render-tsmc-chart.el --- Regenerate README TSMC chart -*- lexical-binding: t; -*-

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name "../.." here)))
  (add-to-list 'load-path (expand-file-name "src" root))
  (require 'financial-chart)
  (require 'financial-chart-svg)
  (require 'subr-x)
  (let* ((csv-file (expand-file-name "examples/tsmc-daily.csv" root))
         (output-file (expand-file-name "images/tsmc-candlestick.png" root))
         (lines (with-temp-buffer
                  (insert-file-contents csv-file)
                  (split-string (string-trim (buffer-string)) "\n" t)))
         (bars
          (mapcar
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
    (make-directory (file-name-directory output-file) t)
    (let ((financial-chart-show-volume t)
          (financial-chart-show-x-axis t)
          (financial-chart-svg-width 1280)
          (financial-chart-svg-height 760))
      (financial-chart-export-png
       bars output-file "TSMC (NYSE: TSM) — Daily candles" 1280 760))
    (princ (format "Wrote %s from %d daily bars\n" output-file (length bars)))))

;;; render-tsmc-chart.el ends here
