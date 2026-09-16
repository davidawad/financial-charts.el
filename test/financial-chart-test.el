;;; financial-chart-test.el --- rendering + Schwab bridge contract -*- lexical-binding: t; -*-

(require 'ert)
(require 'financial-chart)

(ert-deftest financial-chart-glyph-full-body-row ()
  (should (eq (financial-chart--glyph 0.0 1.0 -1.0 2.0 -2.0 3.0) ?█)))

(ert-deftest financial-chart-glyph-upper-half-body-row ()
  ;; body occupies [0.6,1.4], row spans [0,1] -- overlap [0.6,1.0],
  ;; midpoint 0.8 >= row-mid 0.5 -> upper glyph.
  (should (eq (financial-chart--glyph 0.0 1.0 0.6 1.4 0.6 1.4) ?▀)))

(ert-deftest financial-chart-glyph-lower-half-body-row ()
  ;; body occupies [-0.4,0.4], row spans [0,1] -- overlap [0,0.4],
  ;; midpoint 0.2 < row-mid 0.5 -> lower glyph.
  (should (eq (financial-chart--glyph 0.0 1.0 -0.4 0.4 -0.4 0.4) ?▄)))

(ert-deftest financial-chart-glyph-wick-only-row ()
  (should (eq (financial-chart--glyph 5.0 6.0 0.0 1.0 -1.0 10.0) ?│)))

(ert-deftest financial-chart-glyph-empty-row ()
  (should (eq (financial-chart--glyph 50.0 51.0 0.0 1.0 -1.0 10.0) ?\s)))

(ert-deftest financial-chart-candle-face-up-vs-down ()
  (let ((financial-chart-up-face 'success)
        (financial-chart-down-face 'error))
    (should
     (eq (financial-chart--candle-face '(:open 10 :high 12 :low 9 :close 11))
         'success))
    (should
     (eq (financial-chart--candle-face '(:open 11 :high 12 :low 9 :close 10))
         'error))
    ;; doji (close == open) counts as up
    (should
     (eq (financial-chart--candle-face '(:open 10 :high 12 :low 9 :close 10))
         'success))))

(ert-deftest financial-chart-render-errors-on-no-bars ()
  (should-error (financial-chart-render nil) :type 'user-error))

(ert-deftest financial-chart-render-has-height-rows-and-one-column-per-bar ()
  (let* ((bars
          '((:open 10 :high 12 :low 9 :close 11)
            (:open 11 :high 13 :low 10 :close 9)
            (:open 9 :high 10 :low 8 :close 9.5)))
         (rendered (financial-chart-render bars 5))
         (lines (split-string rendered "\n")))
    (should (= (length lines) 5))
    (dolist (line lines)
      ;; strip the fixed-width axis label prefix (8 chars) before counting
      ;; one glyph per bar
      (should (= (length (substring-no-properties line 8)) (length bars))))))

(ert-deftest financial-chart-render-tolerates-zero-range-bars ()
  (let ((bars (list '(:open 100 :high 100 :low 100 :close 100))))
    (should (stringp (financial-chart-render bars 3)))))

(ert-deftest financial-chart-schwab-candle-maps-reference-shaped-fixture ()
  ;; Same field names Schwab's real /pricehistory endpoint returns
  ;; (schwab-broker-test-price-history-parses-reference-shaped-fixture,
  ;; schwab-broker.el's own test suite).
  (let* ((candle
          '((open . 189.5) (high . 190.2) (low . 189.1) (close . 190.0)
            (volume . 1234567) (datetime . 1757790000000)))
         (bar (financial-chart--schwab-candle->bar candle)))
    (should (= (plist-get bar :open) 189.5))
    (should (= (plist-get bar :high) 190.2))
    (should (= (plist-get bar :low) 189.1))
    (should (= (plist-get bar :close) 190.0))
    (should (= (plist-get bar :volume) 1234567))
    (should (= (plist-get bar :time) 1757790000000))))

(ert-deftest financial-chart-schwab-view-errors-without-schwab-broker-loaded ()
  ;; schwab-broker.el is never required by this test file, so
  ;; schwab-broker-price-history-sync genuinely isn't fboundp here.
  (should-not (fboundp 'schwab-broker-price-history-sync))
  (should-error (financial-chart-schwab-view "AAPL") :type 'user-error))

(provide 'financial-chart-test)
;;; financial-chart-test.el ends here
