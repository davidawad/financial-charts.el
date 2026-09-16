;;; financial-chart-test.el --- rendering + Schwab bridge contract -*- lexical-binding: t; -*-

(require 'ert)
(require 'financial-chart)

(defmacro financial-chart-test--with-defaults (&rest body)
  "Run BODY with every financial-chart-* defcustom reset to its
standard value, so tests don't leak customizations across each other."
  `(let ((financial-chart-height 20)
         (financial-chart-max-bars 80)
         (financial-chart-candle-width 1)
         (financial-chart-candle-gap 1)
         (financial-chart-up-face 'success)
         (financial-chart-down-face 'error)
         (financial-chart-wick-face nil)
         (financial-chart-axis-face nil)
         (financial-chart-glyph-full-block ?█)
         (financial-chart-glyph-upper-half ?▀)
         (financial-chart-glyph-lower-half ?▄)
         (financial-chart-glyph-wick ?│)
         (financial-chart-glyph-empty ?\s)
         (financial-chart-glyph-indicator ?•)
         (financial-chart-glyph-volume-bar ?█)
         (financial-chart-scale 'linear)
         (financial-chart-axis-label-count 3)
         (financial-chart-axis-format "%7.2f ")
         (financial-chart-show-volume t)
         (financial-chart-volume-height 5)
         (financial-chart-volume-up-face nil)
         (financial-chart-volume-down-face nil)
         (financial-chart-volume-axis-label-count 2)
         (financial-chart-volume-axis-format "%7.0f ")
         (financial-chart-show-x-axis t)
         (financial-chart-x-axis-label-count 4)
         (financial-chart-x-axis-format "%m/%d")
         (financial-chart-indicators nil))
     ,@body))

(defun financial-chart-test--bars (n)
  "Return N synthetic bars with distinct OHLCV/time values."
  (cl-loop for i from 0 below n
           collect (list :open (+ 100 i) :high (+ 105 i) :low (+ 95 i)
                         :close (+ 100 (if (cl-evenp i) 2 -2) i)
                         :volume (* 1000 (1+ i))
                         :time (* i 86400000))))

;; -- glyph selection --

(ert-deftest financial-chart-glyph-full-body-row ()
  (financial-chart-test--with-defaults
   (should (equal (financial-chart--glyph 0.0 1.0 -1.0 2.0 -2.0 3.0)
                  (cons 'body ?█)))))

(ert-deftest financial-chart-glyph-upper-half-body-row ()
  (financial-chart-test--with-defaults
   (should (equal (financial-chart--glyph 0.0 1.0 0.6 1.4 0.6 1.4)
                  (cons 'body ?▀)))))

(ert-deftest financial-chart-glyph-lower-half-body-row ()
  (financial-chart-test--with-defaults
   (should (equal (financial-chart--glyph 0.0 1.0 -0.4 0.4 -0.4 0.4)
                  (cons 'body ?▄)))))

(ert-deftest financial-chart-glyph-wick-only-row ()
  (financial-chart-test--with-defaults
   (should (equal (financial-chart--glyph 5.0 6.0 0.0 1.0 -1.0 10.0)
                  (cons 'wick ?│)))))

(ert-deftest financial-chart-glyph-empty-row ()
  (financial-chart-test--with-defaults
   (should (equal (financial-chart--glyph 50.0 51.0 0.0 1.0 -1.0 10.0)
                  (cons 'empty ?\s)))))

(ert-deftest financial-chart-glyph-respects-custom-characters ()
  (financial-chart-test--with-defaults
   (let ((financial-chart-glyph-full-block ?#)
         (financial-chart-glyph-wick ?|))
     (should (equal (financial-chart--glyph 0.0 1.0 -1.0 2.0 -2.0 3.0)
                    (cons 'body ?#)))
     (should (equal (financial-chart--glyph 5.0 6.0 0.0 1.0 -1.0 10.0)
                    (cons 'wick ?|))))))

;; -- candle face --

(ert-deftest financial-chart-candle-face-up-vs-down ()
  (financial-chart-test--with-defaults
   (should
    (eq (financial-chart--candle-face '(:open 10 :high 12 :low 9 :close 11))
        'success))
   (should
    (eq (financial-chart--candle-face '(:open 11 :high 12 :low 9 :close 10))
        'error))
   (should
    (eq (financial-chart--candle-face '(:open 10 :high 12 :low 9 :close 10))
        'success))))

;; -- render basics / errors --

(ert-deftest financial-chart-render-errors-on-no-bars ()
  (financial-chart-test--with-defaults
   (should-error (financial-chart-render nil) :type 'user-error)))

(ert-deftest financial-chart-render-tolerates-zero-range-bars ()
  (financial-chart-test--with-defaults
   (let ((bars (list '(:open 100 :high 100 :low 100 :close 100))))
     (should (stringp (financial-chart-render bars 3))))))

;; -- bar-count windowing --

(ert-deftest financial-chart-max-bars-trims-to-most-recent ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-max-bars 3)
          (financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 10))
          (rendered (financial-chart-render bars 5))
          (line (car (split-string rendered "\n"))))
     ;; 3 kept bars * (1 width + 1 gap) - 1 trailing gap = 5 glyph chars,
     ;; plus the axis label prefix.
     (should (= (length (substring-no-properties line 8)) 5)))))

(ert-deftest financial-chart-max-bars-nil-disables-windowing ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-max-bars nil)
          (financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 10))
          (rendered (financial-chart-render bars 5))
          (line (car (split-string rendered "\n"))))
     ;; 10 bars * 2 cols - 1 trailing gap = 19
     (should (= (length (substring-no-properties line 8)) 19)))))

;; -- candle width / gap --

(ert-deftest financial-chart-candle-width-expands-each-column ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-candle-width 3)
          (financial-chart-candle-gap 0)
          (financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 2))
          (rendered (financial-chart-render bars 5))
          (line (car (split-string rendered "\n"))))
     (should (= (length (substring-no-properties line 8)) 6)))))

(ert-deftest financial-chart-candle-gap-adds-space-between-columns ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-candle-width 1)
          (financial-chart-candle-gap 2)
          (financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 3))
          (rendered (financial-chart-render bars 5))
          (line (car (split-string rendered "\n"))))
     ;; 3 candles (1 wide) + 2 gaps (2 wide) between them = 3 + 4 = 7
     (should (= (length (substring-no-properties line 8)) 7)))))

;; -- log scale --

(ert-deftest financial-chart-log-scale-does-not-error-and-differs-from-linear ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars
           (list '(:open 10 :high 12 :low 9 :close 11)
                 '(:open 100 :high 120 :low 90 :close 110)
                 '(:open 1000 :high 1200 :low 900 :close 1100)))
          (linear (let ((financial-chart-scale 'linear))
                    (financial-chart-render bars 10)))
          (logged (let ((financial-chart-scale 'log))
                    (financial-chart-render bars 10))))
     (should (stringp logged))
     (should-not (equal linear logged)))))

;; -- faces --

(ert-deftest financial-chart-wick-face-override ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-wick-face 'shadow)
          (cell (financial-chart--candle-cell
                 5.0 6.0 '(:open 0 :high 10 :low -1 :close 1))))
     (should (eq (cdr cell) 'shadow)))))

(ert-deftest financial-chart-wick-face-nil-falls-back-to-candle-face ()
  (financial-chart-test--with-defaults
   (let ((cell (financial-chart--candle-cell
                5.0 6.0 '(:open 0 :high 10 :low -1 :close 1))))
     (should (eq (cdr cell) 'success)))))

;; -- axis label count --

(ert-deftest financial-chart-axis-label-rows-honors-count ()
  (financial-chart-test--with-defaults
   (should (= (length (financial-chart--axis-label-rows 20 3)) 3))
   (should (= (length (financial-chart--axis-label-rows 20 5)) 5))
   (should (member 0 (financial-chart--axis-label-rows 20 5)))
   (should (member 19 (financial-chart--axis-label-rows 20 5)))))

;; -- volume panel --

(ert-deftest financial-chart-volume-panel-present-when-volume-data-exists ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 5))
          (rendered (financial-chart-render bars 5)))
     ;; price panel (5 rows) + volume panel (default height 5) = 10 lines
     (should (= (length (split-string rendered "\n")) 10)))))

(ert-deftest financial-chart-volume-panel-absent-without-volume-data ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-x-axis nil)
          (bars (list '(:open 1 :high 2 :low 0 :close 1)
                      '(:open 1 :high 2 :low 0 :close 1)))
          (rendered (financial-chart-render bars 5)))
     (should (= (length (split-string rendered "\n")) 5)))))

(ert-deftest financial-chart-show-volume-nil-suppresses-panel-even-with-data ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 5))
          (rendered (financial-chart-render bars 5)))
     (should (= (length (split-string rendered "\n")) 5)))))

;; -- X-axis --

(ert-deftest financial-chart-x-axis-present-when-time-data-exists ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (bars (financial-chart-test--bars 5))
          (rendered (financial-chart-render bars 5))
          (lines (split-string rendered "\n")))
     (should (= (length lines) 6))
     ;; epoch 0 -> some date label should appear somewhere on the axis line
     (should (> (length (string-trim (car (last lines)))) 0)))))

(ert-deftest financial-chart-x-axis-absent-without-time-data ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (bars (list '(:open 1 :high 2 :low 0 :close 1)
                      '(:open 1 :high 2 :low 0 :close 1)))
          (rendered (financial-chart-render bars 5)))
     (should (= (length (split-string rendered "\n")) 5)))))

(ert-deftest financial-chart-show-x-axis-nil-suppresses-axis-even-with-data ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 5))
          (rendered (financial-chart-render bars 5)))
     (should (= (length (split-string rendered "\n")) 5)))))

;; -- indicator overlays --

(ert-deftest financial-chart-indicator-overlay-appears-at-its-value-row ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (financial-chart-candle-width 1)
          (financial-chart-candle-gap 0)
          (bars (list '(:open 0 :high 10 :low 0 :close 0)))
          ;; constant series pinned to the exact middle of the range
          (financial-chart-indicators
           (list (list :fn (lambda (_bars) '(5.0)) :glyph ?X :face 'bold)))
          (rendered (financial-chart-render bars 10)))
     (should (string-match-p "X" rendered)))))

(ert-deftest financial-chart-no-indicators-means-no-overlay-glyph ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (list '(:open 0 :high 10 :low 0 :close 0)))
          (rendered (financial-chart-render bars 10)))
     (should-not (string-match-p "X" rendered)))))

;; -- Schwab bridge --

(ert-deftest financial-chart-schwab-candle-maps-reference-shaped-fixture ()
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
  (should-not (fboundp 'schwab-broker-price-history-sync))
  (should-error (financial-chart-schwab-view "AAPL") :type 'user-error))

(ert-deftest financial-chart-merge-schwab-defaults-fills-missing-keys-only ()
  (financial-chart-test--with-defaults
   (let ((merged (financial-chart--merge-schwab-defaults
                  (list :period-type "day" :period 5))))
     ;; explicit keys survive unchanged
     (should (equal (plist-get merged :period-type) "day"))
     (should (equal (plist-get merged :period) 5))
     ;; missing keys filled from defaults
     (should (equal (plist-get merged :frequency-type)
                    financial-chart-schwab-default-frequency-type))
     (should (equal (plist-get merged :frequency)
                    financial-chart-schwab-default-frequency)))))

(ert-deftest financial-chart-merge-schwab-defaults-on-empty-keys ()
  (financial-chart-test--with-defaults
   (let ((merged (financial-chart--merge-schwab-defaults nil)))
     (should (equal (plist-get merged :period-type)
                    financial-chart-schwab-default-period-type))
     (should (equal (plist-get merged :period)
                    financial-chart-schwab-default-period))
     (should (equal (plist-get merged :frequency-type)
                    financial-chart-schwab-default-frequency-type))
     (should (equal (plist-get merged :frequency)
                    financial-chart-schwab-default-frequency)))))

(provide 'financial-chart-test)
;;; financial-chart-test.el ends here
