;;; financial-chart-indicator-api-test.el --- Provider-neutral indicator API tests -*- lexical-binding: t; -*-

(require 'ert)
(defvar financial-chart-indicator-api-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-indicator-api-test--dir))
(require 'financial-chart)

(ert-deftest financial-chart-indicator-api-normalizes-provider-series ()
  (let ((series
         (financial-chart-normalize-indicator-series
          '(:name "vendor.band.upper"
            :label "Upper band" :values [nil 12.5 13]
            :timestamps [1000 2000 3000] :unit :price
            :panel :overlay :source :vendor))))
    (should (eq (plist-get series :schema) 'indicator-series/v1))
    (should (equal (plist-get series :values) '(nil 12.5 13)))
    (should (equal (financial-chart-indicator-series-data series)
                   '((1000 nil) (2000 12.5) (3000 13))))
    (should (eq (plist-get series :source) :vendor))))

(ert-deftest financial-chart-indicator-api-rejects-misaligned-timestamps ()
  (should-error
   (financial-chart-normalize-indicator-series
    '(:name bad :values (1 2) :timestamps (1000)))
   :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-indicator-api-rejects-nonnumeric-values ()
  (should-error
   (financial-chart-normalize-indicator-series
    '(:name bad :values (1 "2")))
   :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-indicator-api-evaluates-provider-neutral-builtins ()
  (let* ((bars '((:close 10 :time 1000)
                 (:close 12 :time 2000)
                 (:close 14 :time 3000)))
         (series (financial-chart-indicator-evaluate 'sma bars 2)))
    (should (equal (plist-get series :values) '(nil 11.0 13.0)))
    (should (equal (plist-get series :timestamps) '(1000 2000 3000)))
    (should (eq (plist-get series :panel) :overlay))
    (should (equal (plist-get series :params) '(2)))))

(ert-deftest financial-chart-indicator-api-builds-renderable-spec ()
  (let ((spec (financial-chart-indicator-chart-spec
               '(:name rsi :label "RSI" :values (nil 45 60) :unit :percent))))
    (should (eq (plist-get spec :kind) 'line))
    (should (equal (plist-get spec :title) "RSI"))
    (should (equal (plist-get spec :data) '((0 . nil) (1 . 45) (2 . 60))))
    (should (stringp (financial-chart-plot-spec spec)))))

(ert-deftest financial-chart-indicator-api-series-colors-reach-renderers ()
  (let* ((single (financial-chart-indicator-chart-spec
                  '(:name rsi :values (40 50) :color "#123456")))
         (single-svg (financial-chart-plot-spec
                      (plist-put single :backend 'svg)))
         (multi (financial-chart-indicator-chart-spec
                 '((:name fast :label "Fast" :values (1 2) :color "#123456")
                   (:name slow :label "Slow" :values (2 3) :color "#abcdef"))))
         (multi-svg (financial-chart-plot-spec
                     (plist-put multi :backend 'svg))))
    (should (string-match-p "stroke=\"#123456\"" single-svg))
    (should (string-match-p "stroke=\"#123456\"" multi-svg))
    (should (string-match-p "stroke=\"#abcdef\"" multi-svg))))

(ert-deftest financial-chart-indicator-api-plots-multiple-output-series ()
  (let* ((series (financial-chart-indicator-evaluate
                  'macd
                  '((:close 10) (:close 11) (:close 12) (:close 13))
                  1 2 1))
         (spec (financial-chart-indicator-chart-spec series "MACD")))
    (should (= (length series) 3))
    (should (eq (plist-get spec :kind) 'multi))
    (should (= (length (plist-get spec :data)) 3))
    (should (stringp (financial-chart-plot-spec spec)))))

(provide 'financial-chart-indicator-api-test)
;;; financial-chart-indicator-api-test.el ends here
