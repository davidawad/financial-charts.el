;;; financial-chart-volatility-test.el --- Volatility indicator tests -*- lexical-binding: t; -*-

(require 'ert)
(defvar financial-chart-volatility-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-volatility-test--dir))
(require 'financial-chart-volatility)

(defconst financial-chart-volatility-test--bars
  '((:time 1000 :open 10 :high 11 :low 9 :close 10 :volume 100)
    (:time 2000 :open 12 :high 13 :low 11 :close 12 :volume 110)
    (:time 3000 :open 14 :high 15 :low 13 :close 14 :volume 120)
    (:time 4000 :open 16 :high 17 :low 15 :close 16 :volume 130)))

(ert-deftest financial-chart-volatility-atr-uses-wilder-smoothing ()
  (should (equal (financial-chart-atr financial-chart-volatility-test--bars 2)
                 '(nil 2.5 2.75 2.875))))

(ert-deftest financial-chart-volatility-bollinger-bands-are-bar-aligned ()
  (let ((bands (financial-chart-bollinger-bands
                financial-chart-volatility-test--bars 2 1)))
    (should (equal (plist-get (nth 0 bands) :values)
                   '(nil 12.0 14.0 16.0)))
    (should (equal (plist-get (nth 1 bands) :values)
                   '(nil 11.0 13.0 15.0)))
    (should (equal (plist-get (nth 2 bands) :values)
                   '(nil 10.0 12.0 14.0)))))

(ert-deftest financial-chart-volatility-keltner-uses-ema-and-atr ()
  (let ((channels (financial-chart-keltner-channels
                   financial-chart-volatility-test--bars 2 2 2)))
    (should (equal (plist-get (nth 0 channels) :values)
                   '(nil 16.0 18.5 20.75)))
    (should (equal (plist-get (nth 1 channels) :values)
                   '(nil 11.0 13.0 15.0)))
    (should (equal (plist-get (nth 2 channels) :values)
                   '(nil 6.0 7.5 9.25)))))

(ert-deftest financial-chart-volatility-donchian-tracks-rolling-extremes ()
  (let ((channels (financial-chart-donchian-channels
                   financial-chart-volatility-test--bars 2)))
    (should (equal (plist-get (nth 0 channels) :values)
                   '(nil 13 15 17)))
    (should (equal (plist-get (nth 1 channels) :values)
                   '(nil 11.0 13.0 15.0)))
    (should (equal (plist-get (nth 2 channels) :values)
                   '(nil 9 11 13)))))

(ert-deftest financial-chart-volatility-missing-data-restarts-warmup ()
  (let* ((bars (list (nth 0 financial-chart-volatility-test--bars)
                     (plist-put (copy-sequence
                                 (nth 1 financial-chart-volatility-test--bars))
                                :close nil)
                     (nth 2 financial-chart-volatility-test--bars)
                     (nth 3 financial-chart-volatility-test--bars)))
         (atr (financial-chart-atr bars 2))
         (bands (financial-chart-bollinger-bands bars 2)))
    (should (equal atr '(nil 2.5 nil nil)))
    (should (equal (plist-get (nth 0 bands) :values)
                   '(nil nil nil 17.0)))))

(ert-deftest financial-chart-volatility-builtins-register-with-common-api ()
  (let ((outputs (financial-chart-indicator-evaluate
                  'bollinger-bands financial-chart-volatility-test--bars 2)))
    (should (equal (mapcar (lambda (series) (plist-get series :name)) outputs)
                   '(bollinger-upper bollinger-middle bollinger-lower)))
    (should (equal (plist-get (car outputs) :timestamps)
                   '(1000 2000 3000 4000)))))

(provide 'financial-chart-volatility-test)
;;; financial-chart-volatility-test.el ends here
