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
         (financial-chart-indicators nil)
         (financial-chart-svg-candle-width 6)
         (financial-chart-svg-candle-gap 3)
         (financial-chart-svg-wick-width 1)
         (financial-chart-svg-price-height 400)
         (financial-chart-svg-volume-height 100)
         (financial-chart-svg-margin-left 55)
         (financial-chart-svg-margin-right 20)
         (financial-chart-svg-margin-top 40)
         (financial-chart-svg-margin-bottom 30)
         (financial-chart-svg-font-size 12)
         (financial-chart-svg-font-family
          "DejaVu Sans Mono, Menlo, Consolas, monospace")
         (financial-chart-svg-background nil)
         (financial-chart-svg-text-color nil)
         (financial-chart-export-directory "~/Desktop")
         (financial-chart-png-converter nil))
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

;; -- provider-agnostic bridge (L2, all data via market-data.el) --

(defvar financial-chart-test--md-bars-called nil
  "Set non-nil by the mock `market-data-bars' when it is invoked.")
(defvar financial-chart-test--md-bars-provider nil
  "Records the :provider the mock `market-data-bars' received.")

(defmacro financial-chart-test--with-mock-market-data (&rest body)
  "Run BODY with `market-data-explain'/`-bars'/`-capabilities' mocked.
The mock `market-data-explain' echoes the requested (or default)
provider + normalized params with ZERO I/O; the mock `market-data-bars'
returns two synthetic bar/v1 plists and records its call + received
:provider.  Nothing here touches a network or a broker."
  `(let ((financial-chart-test--md-bars-called nil)
         (financial-chart-test--md-bars-provider nil))
     (cl-letf (((symbol-function 'market-data-explain)
                (lambda (symbol &rest keys)
                  (list :symbol (upcase symbol)
                        :provider (or (plist-get keys :provider) 'schwab)
                        :why 'only-loaded
                        :want-fields (plist-get keys :fields)
                        :params
                        (list :period-type (or (plist-get keys :period-type) "month")
                              :period (or (plist-get keys :period) 1)
                              :frequency-type (or (plist-get keys :frequency-type) "daily")
                              :frequency (or (plist-get keys :frequency) 1)))))
               ((symbol-function 'market-data-bars)
                (lambda (_symbol &rest keys)
                  (setq financial-chart-test--md-bars-called t
                        financial-chart-test--md-bars-provider (plist-get keys :provider))
                  (list (list :open 100 :high 105 :low 99 :close 103 :volume 1000 :time 0)
                        (list :open 103 :high 107 :low 102 :close 106 :volume 1200
                              :time 86400000))))
               ((symbol-function 'market-data-capabilities)
                (lambda ()
                  '((schwab :loaded t :authed t :native-fields nil :priority 20)))))
       ,@body)))

(ert-deftest financial-chart-view-symbol-renders-with-provenance-title ()
  (financial-chart-test--with-defaults
   (financial-chart-test--with-mock-market-data
    (financial-chart-view-symbol "aapl")
    (with-current-buffer "*financial-chart*"
      (let ((s (buffer-string)))
        ;; Law 7 title block: symbol · provider · period/frequency · bars · fetched-at
        (should (string-match-p "AAPL" s))
        (should (string-match-p "schwab" s))
        (should (string-match-p "month" s))
        (should (string-match-p "daily" s))
        (should (string-match-p "2 bars" s))
        (should (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" s)))))))

(ert-deftest financial-chart-view-symbol-threads-provider-override ()
  (financial-chart-test--with-defaults
   (financial-chart-test--with-mock-market-data
    (financial-chart-view-symbol "aapl" :provider 'alpaca)
    ;; :provider override threads end-to-end into the fetch
    (should (eq financial-chart-test--md-bars-provider 'alpaca))
    (with-current-buffer "*financial-chart*"
      (should (string-match-p "alpaca" (buffer-string)))))))

(ert-deftest financial-chart-explain-symbol-is-zero-io ()
  (financial-chart-test--with-defaults
   (financial-chart-test--with-mock-market-data
    (let ((plan (financial-chart-explain-symbol "aapl" :provider 'schwab)))
      ;; Law 3: explain performs NO fetch (mock bars asserted uncalled)
      (should-not financial-chart-test--md-bars-called)
      (should (eq (plist-get plan :provider) 'schwab))
      ;; merged render config present + reflects the effective defcustom
      (should (plist-member plan :render))
      (should (= (plist-get (plist-get plan :render) :height)
                 financial-chart-height))))))

(ert-deftest financial-chart-view-symbol-errors-without-market-data ()
  ;; market-data (L1) not loaded -> typed user-error naming the fix (Law 4)
  (should-not (fboundp 'market-data-bars))
  (should-error (financial-chart-view-symbol "AAPL") :type 'user-error))

(ert-deftest financial-chart-schwab-view-is-obsolete-alias-forwarding-schwab ()
  ;; deprecation, not deletion: still fbound, obsolete-marked, forwards :provider 'schwab
  (should (fboundp 'financial-chart-schwab-view))
  (should (get 'financial-chart-schwab-view 'byte-obsolete-info))
  (financial-chart-test--with-defaults
   (financial-chart-test--with-mock-market-data
    (financial-chart-schwab-view "aapl")
    (should (eq financial-chart-test--md-bars-provider 'schwab))
    (with-current-buffer "*financial-chart*"
      (should (string-match-p "schwab" (buffer-string)))))))

;; -- SVG rendering --

(ert-deftest financial-chart-render-svg-errors-on-no-bars ()
  (financial-chart-test--with-defaults
   (should-error (financial-chart-render-svg nil) :type 'user-error)))

(ert-deftest financial-chart-render-svg-produces-well-formed-xml ()
  (financial-chart-test--with-defaults
   (let* ((bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars "TEST")))
     (should (string-prefix-p "<svg" (string-trim svg)))
     (should (string-suffix-p "</svg>" (string-trim svg)))
     (should (string-match-p "<rect" svg))
     (should (string-match-p "<text" svg))
     (should (string-match-p "TEST" svg)))))

(ert-deftest financial-chart-render-svg-uses-default-font-family-on-every-text ()
  (financial-chart-test--with-defaults
   (let* ((bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars "TEST")))
     (should (string-match-p "font-family=\"DejaVu Sans Mono" svg))
     ;; every <text> element carries a font-family, none left unstyled
     (should
      (cl-every
       (lambda (chunk) (string-match-p "font-family=" chunk))
       (cdr (split-string svg "<text")))))))

(ert-deftest financial-chart-render-svg-font-family-param-overrides-defcustom ()
  (financial-chart-test--with-defaults
   (let* ((bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars "TEST" "Hack")))
     (should (string-match-p "font-family=\"Hack\"" svg))
     (should-not (string-match-p "DejaVu" svg))
     ;; overriding via the param must not leak into the defcustom itself
     (should (equal financial-chart-svg-font-family
                    "DejaVu Sans Mono, Menlo, Consolas, monospace")))))

(ert-deftest financial-chart-render-svg-font-family-defcustom-override ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-svg-font-family "Hack")
          (bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars "TEST")))
     (should (string-match-p "font-family=\"Hack\"" svg)))))

;; -- regression: integer-division truncation in the pixel-Y mapping --
;;
;; `financial-chart-test--bars' (used by nearly every test above) already
;; generates plain-integer OHLC values, yet none of those tests caught
;; this: they all assert on SVG *structure* (rect/text counts, declared
;; width, a font-family attribute) rather than the actual pixel geometry,
;; so a bug that collapsed every candle but the topmost to the panel's
;; bottom pixel produced a well-formed, plausible-looking SVG with the
;; right element counts and passed every one of them. These two tests
;; check real Y-coordinates specifically to close that gap.

(ert-deftest financial-chart-svg-y-uses-float-division-for-integer-bounds ()
  (financial-chart-test--with-defaults
   (should (= (financial-chart--svg-y 97 97 130 40 400) 440.0))
   (should (= (financial-chart--svg-y 130 97 130 40 400) 40.0))
   ;; (/ 6 33) truncates to 0 under plain integer division, which used to
   ;; collapse this to 440.0 (the panel bottom) instead of ~367.27
   (let ((y (financial-chart--svg-y 103 97 130 40 400)))
     (should (< (abs (- y 367.27)) 0.1)))))

(ert-deftest financial-chart-render-svg-integer-prices-produce-distinct-wick-heights ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (bars (financial-chart-test--bars 10))
          (svg (financial-chart-render-svg bars))
          (y1-values
           (delq nil
                 (mapcar
                  (lambda (chunk)
                    (when (string-match "y1=\"\\([0-9.]+\\)\"" chunk)
                      (string-to-number (match-string 1 chunk))))
                  (split-string svg "<line ")))))
     ;; a real price ladder produces mostly-distinct wick heights; the
     ;; integer-division bug collapsed all but one to the same value
     (should (> (length (delete-dups y1-values)) 5)))))

(ert-deftest financial-chart-render-svg-includes-volume-and-x-axis-when-present ()
  (financial-chart-test--with-defaults
   (let* ((bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars)))
     ;; 5 bars * 2 candles-worth of rects (body) + volume bars = at least 10
     (should (>= (cl-count-if (lambda (_) t) (split-string svg "<rect")) 10))
     ;; a date-formatted label like "01/01" should appear on the X-axis
     (should (string-match-p "[0-9][0-9]/[0-9][0-9]" svg)))))

(ert-deftest financial-chart-render-svg-omits-volume-without-volume-data ()
  (financial-chart-test--with-defaults
   (let* ((bars (list '(:open 1 :high 2 :low 0 :close 1)
                      '(:open 1 :high 2 :low 0 :close 1)))
          (with-volume (financial-chart-render-svg (financial-chart-test--bars 5)))
          (without-volume (financial-chart-render-svg bars)))
     (should (string-match-p "[0-9][0-9]/[0-9][0-9]" with-volume))
     ;; fewer rects: no volume-panel bars for a 2-bar chart with no :volume
     (should (< (length without-volume) (length with-volume))))))

(ert-deftest financial-chart-render-svg-respects-custom-margins-and-size ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-svg-margin-left 100)
          (financial-chart-svg-candle-width 20)
          (bars (financial-chart-test--bars 3))
          (svg (financial-chart-render-svg bars))
          (expected-width
           (+ financial-chart-svg-margin-left financial-chart-svg-margin-right
              (* 3 (+ financial-chart-svg-candle-width
                     financial-chart-svg-candle-gap)))))
     (should
      (string-match-p (format "width=\"%d\"" expected-width) svg)))))

(ert-deftest financial-chart-render-svg-draws-indicator-polyline ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-indicators
           (list (list :fn (lambda (bars) (make-list (length bars) 100.0))
                       :face 'success)))
          (bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars)))
     (should (string-match-p "<polyline" svg)))))

;; -- SVG export --

(ert-deftest financial-chart-export-svg-writes-file ()
  (financial-chart-test--with-defaults
   (let ((file (make-temp-file "financial-chart-test" nil ".svg")))
     (unwind-protect
         (progn
           (financial-chart-export-svg (financial-chart-test--bars 5) file "AAPL")
           (should (file-exists-p file))
           (with-temp-buffer
             (insert-file-contents file)
             (should (string-match-p "AAPL" (buffer-string)))
             (should (string-match-p "<svg" (buffer-string)))))
       (delete-file file)))))

(ert-deftest financial-chart-export-svg-font-family-param ()
  (financial-chart-test--with-defaults
   (let ((file (make-temp-file "financial-chart-test" nil ".svg")))
     (unwind-protect
         (progn
           (financial-chart-export-svg
            (financial-chart-test--bars 5) file "AAPL" "Hack")
           (with-temp-buffer
             (insert-file-contents file)
             (should (string-match-p "font-family=\"Hack\"" (buffer-string)))))
       (delete-file file)))))

;; -- PNG export (converter mocked -- no dependency on a real rsvg-convert/
;; ImageMagick install being present on the test machine) --

(ert-deftest financial-chart-export-png-invokes-function-converter ()
  (financial-chart-test--with-defaults
   (let* ((calls nil)
          (svg-existed-at-call-time nil)
          (financial-chart-png-converter
           (lambda (svg-file png-file)
             (push (cons svg-file png-file) calls)
             (setq svg-existed-at-call-time (file-exists-p svg-file))
             (with-temp-file png-file (insert "fake-png-bytes"))))
          (out-file (make-temp-file "financial-chart-test" nil ".png")))
     (unwind-protect
         (progn
           (financial-chart-export-png (financial-chart-test--bars 3) out-file "AAPL")
           (should (= (length calls) 1))
           ;; the intermediate .svg existed at call time (it's deleted by
           ;; export-png's own unwind-protect right after this returns)
           (should svg-existed-at-call-time)
           (should (equal (cdar calls) out-file))
           (with-temp-buffer
             (insert-file-contents out-file)
             (should (equal (buffer-string) "fake-png-bytes"))))
       (delete-file out-file)))))

(ert-deftest financial-chart-export-png-cleans-up-intermediate-svg ()
  (financial-chart-test--with-defaults
   (let* ((captured-svg-file nil)
          (financial-chart-png-converter
           (lambda (svg-file png-file)
             (setq captured-svg-file svg-file)
             (with-temp-file png-file (insert "x"))))
          (out-file (make-temp-file "financial-chart-test" nil ".png")))
     (unwind-protect
         (progn
           (financial-chart-export-png (financial-chart-test--bars 3) out-file)
           (should-not (file-exists-p captured-svg-file)))
       (delete-file out-file)))))

(ert-deftest financial-chart-resolve-png-converter-errors-when-none-found ()
  (financial-chart-test--with-defaults
   (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
     (should-error (financial-chart--resolve-png-converter) :type 'user-error))))

(ert-deftest financial-chart-resolve-png-converter-auto-detect-uses-real-executable-find ()
  ;; Regression test: `executable-find' takes a STRING, and the auto-detect
  ;; list is a list of SYMBOLS -- a prior version passed symbols straight
  ;; through and errored with "wrong-type-argument stringp" the moment a
  ;; converter actually needed to be found (every mocked test above hid
  ;; this, since none of them exercised the real `executable-find').
  ;; Deliberately does NOT mock `executable-find', so this only proves
  ;; anything on a machine with at least one of rsvg-convert/convert/magick
  ;; installed -- skip rather than false-negative if none are present.
  (financial-chart-test--with-defaults
   (if (cl-some #'executable-find '("rsvg-convert" "convert" "magick"))
       (should (memq (financial-chart--resolve-png-converter)
                     '(rsvg-convert convert magick)))
     (ert-skip "no rsvg-convert/convert/magick on this machine"))))

(ert-deftest financial-chart-resolve-png-converter-prefers-explicit-setting ()
  (financial-chart-test--with-defaults
   (let ((financial-chart-png-converter 'magick))
     (should (eq (financial-chart--resolve-png-converter) 'magick)))))

;; -- provider-agnostic SVG/PNG export + doctor hook --

(ert-deftest financial-chart-export-symbol-svg-errors-without-market-data ()
  (should-not (fboundp 'market-data-bars))
  (should-error (financial-chart-export-symbol-svg "AAPL" "/tmp/x.svg")
                :type 'user-error))

(ert-deftest financial-chart-export-symbol-svg-writes-file-with-provenance-title ()
  (financial-chart-test--with-defaults
   (financial-chart-test--with-mock-market-data
    (let ((file (make-temp-file "financial-chart-test" nil ".svg")))
      (unwind-protect
          (progn
            (financial-chart-export-symbol-svg "aapl" file)
            (should financial-chart-test--md-bars-called)
            (should (file-exists-p file))
            (with-temp-buffer
              (insert-file-contents file)
              (let ((s (buffer-string)))
                (should (string-match-p "AAPL" s))
                ;; provenance title stamped into the SVG
                (should (string-match-p "schwab" s)))))
        (delete-file file))))))

(ert-deftest financial-chart-schwab-export-fns-are-obsolete-aliases ()
  ;; deprecation, not deletion: both export wrappers still fbound + obsolete-marked
  (should (fboundp 'financial-chart-schwab-export-svg))
  (should (get 'financial-chart-schwab-export-svg 'byte-obsolete-info))
  (should (fboundp 'financial-chart-schwab-export-png))
  (should (get 'financial-chart-schwab-export-png 'byte-obsolete-info)))

(ert-deftest financial-chart-doctor-checks-shape-and-loadable ()
  (financial-chart-test--with-defaults
   (let ((checks (financial-chart-doctor-checks)))
     ;; every entry is (LABEL . CHECK-FN)
     (should (cl-every (lambda (c) (and (stringp (car c)) (functionp (cdr c))))
                       checks))
     ;; package-loadable check passes (financial-chart is required here)
     (let ((res (funcall (cdr (assoc "financial-chart package loadable" checks)))))
       (should (plist-get res :ok)))
     ;; with a provider available (mocked), the bridge check passes
     (financial-chart-test--with-mock-market-data
      (let ((res (funcall
                  (cdr (assoc "financial-chart bridge resolves a provider via market-data"
                              checks)))))
        (should (plist-get res :ok))))
     ;; each check returns a plist carrying :ok and :detail
     (dolist (c checks)
       (let ((res (funcall (cdr c))))
         (should (plist-member res :ok))
         (should (stringp (plist-get res :detail))))))))

;; -- built-in indicator functions --

(defun financial-chart-test--close-bars (closes)
  "Minimal bars with only :close set, for SMA/EMA/RSI tests."
  (mapcar (lambda (c) (list :close c)) closes))

(ert-deftest financial-chart-sma-nil-during-warmup-then-trailing-mean ()
  (let ((sma (financial-chart-sma
              (financial-chart-test--close-bars '(1 2 3 4 5)) 3)))
    (should (equal (nth 0 sma) nil))
    (should (equal (nth 1 sma) nil))
    (should (= (nth 2 sma) 2.0))
    (should (= (nth 3 sma) 3.0))
    (should (= (nth 4 sma) 4.0))))

(ert-deftest financial-chart-sma-respects-custom-field ()
  (let* ((bars (list '(:close 1 :open 10) '(:close 2 :open 20) '(:close 3 :open 30)))
         (sma (financial-chart-sma bars 2 :open)))
    (should (equal (nth 0 sma) nil))
    (should (= (nth 1 sma) 15.0))
    (should (= (nth 2 sma) 25.0))))

(ert-deftest financial-chart-ema-seeds-with-sma-then-smooths ()
  (let ((ema (financial-chart-ema
              (financial-chart-test--close-bars '(1 2 3 4 5)) 3)))
    (should (equal (nth 0 ema) nil))
    (should (equal (nth 1 ema) nil))
    (should (= (nth 2 ema) 2.0))
    (should (= (nth 3 ema) 3.0))
    (should (= (nth 4 ema) 4.0))))

(ert-deftest financial-chart-rsi-matches-hand-computed-example ()
  ;; closes 10,11,12,11,13 -> changes +1,+1,-1,+2 ; period 3
  (let ((rsi (financial-chart-rsi
              (financial-chart-test--close-bars '(10 11 12 11 13)) 3)))
    (should (equal (nth 0 rsi) nil))
    (should (equal (nth 1 rsi) nil))
    (should (equal (nth 2 rsi) nil))
    ;; window [+1,+1,-1]: avg-gain=2/3, avg-loss=1/3, RS=2 -> RSI=66.667
    (should (< (abs (- (nth 3 rsi) 66.6667)) 0.01))
    ;; window [+1,-1,+2]: avg-gain=1.0, avg-loss=1/3, RS=3 -> RSI=75.0
    (should (< (abs (- (nth 4 rsi) 75.0)) 0.01))))

(ert-deftest financial-chart-rsi-is-100-when-no-losses-in-window ()
  (let ((rsi (financial-chart-rsi
              (financial-chart-test--close-bars '(10 11 12 13)) 2)))
    (should (= (nth 3 rsi) 100.0))))

(ert-deftest financial-chart-rsi-stays-within-0-100-bounds ()
  (let ((rsi (financial-chart-rsi
              (financial-chart-test--close-bars
               '(100 95 110 88 120 80 130 70 140 60 150))
              3)))
    (dolist (v rsi)
      (when v
        (should (>= v 0.0))
        (should (<= v 100.0))))))

(ert-deftest financial-chart-vwap-matches-hand-computed-example ()
  (let* ((bars (list '(:high 10 :low 8 :close 9 :volume 100)
                     '(:high 12 :low 10 :close 11 :volume 200)))
         (vwap (financial-chart-vwap bars)))
    ;; bar1: typical=(10+8+9)/3=9, cum_pv=900, cum_vol=100 -> 9.0
    (should (< (abs (- (nth 0 vwap) 9.0)) 0.001))
    ;; bar2: typical=11, cum_pv=900+2200=3100, cum_vol=300 -> 10.3333
    (should (< (abs (- (nth 1 vwap) 10.3333)) 0.001))))

(ert-deftest financial-chart-vwap-nil-for-bars-without-volume ()
  (let* ((bars (list '(:high 10 :low 8 :close 9)
                     '(:high 12 :low 10 :close 11 :volume 200)))
         (vwap (financial-chart-vwap bars)))
    (should (equal (nth 0 vwap) nil))
    (should (numberp (nth 1 vwap)))))

(ert-deftest financial-chart-sma-usable-directly-as-indicator-overlay ()
  ;; Confirms the documented usage pattern actually works end to end.
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-show-x-axis nil)
          (financial-chart-indicators
           (list (list :fn (lambda (bars) (financial-chart-sma bars 3))
                       :face 'font-lock-keyword-face)))
          (bars (financial-chart-test--bars 10))
          (svg (financial-chart-render-svg bars)))
     (should (string-match-p "<polyline" svg)))))

;; -- indicator cohorts (L4) --

(defconst financial-chart-test--rsi-record
  '((ref . ((kind . "indicator") (id . "finance.market.rsi-14")))
    (name . "Observed 14-period relative strength index")
    (revision . "1.0.0")
    (attributes
     . ((description . "RSI")
        (value . ((type . "array") (unit . "1") (nullable . t)
                  (item_type . "number") (scale . "linear")
                  (bounds . ((min . 0) (max . 100))))))))
  "Mock `david-core-resource-get' record for a bounded (oscillator) indicator.")

(defconst financial-chart-test--sma-record
  '((ref . ((kind . "indicator") (id . "finance.market.sma")))
    (name . "Observed close simple moving average")
    (revision . "1.0.0")
    (attributes
     . ((description . "SMA")
        (value . ((type . "array") (unit . "usd/share") (nullable . t)
                  (item_type . "number") (scale . "linear"))))))
  "Mock `david-core-resource-get' record for a price-scale indicator.")

(ert-deftest financial-chart-cohort-builtin-only-resolves-without-catalog ()
  "A builtin-only cohort resolves purely, never touching the .3 bridge."
  (let ((called nil))
    (cl-letf (((symbol-function 'david-core-resource-get)
               (lambda (&rest _) (setq called t) nil)))
      (let ((specs (financial-chart-resolve-cohort 'trend-following)))
        (should (= (length specs) 3))
        (should (cl-every (lambda (s) (functionp (plist-get s :fn))) specs))
        (should-not called)))))

(ert-deftest financial-chart-cohort-builtin-args-thread-into-closure ()
  "A builtin member's :args curry into the resolved one-arg overlay fn."
  (let* ((financial-chart-indicator-cohorts
          '((c :doc "d" :members ((:fn financial-chart-sma :args (5))))))
         (spec (car (financial-chart-resolve-cohort 'c)))
         (bars (financial-chart-test--bars 10)))
    (should (equal (funcall (plist-get spec :fn) bars)
                   (financial-chart-sma bars 5)))))

(ert-deftest financial-chart-cohort-catalog-member-resolves-with-param ()
  "A catalog member threads its :params through the evaluator table into
the builtin, purely (no bridge call needed for resolve)."
  (let ((financial-chart-indicator-cohorts
         '((cat :doc "catalog sma"
                :members ((:indicator "finance.market.sma" :params (:window 30)))))))
    (let* ((specs (financial-chart-resolve-cohort 'cat))
           (fn (plist-get (car specs) :fn))
           (bars (financial-chart-test--bars 40))
           (series (funcall fn bars)))
      (should (= (length specs) 1))
      (should (= (length series) 40))
      (should-not (nth 28 series))
      (should (numberp (nth 29 series)))
      (should (equal series (financial-chart-sma bars 30))))))

(ert-deftest financial-chart-cohort-unresolvable-member-signals-typed-error ()
  "A catalog member with no evaluator entry signals the typed error,
naming the member and the fix."
  (let ((financial-chart-indicator-cohorts
         '((bad :doc "no evaluator"
                :members ((:indicator "finance.market.bar-return" :params nil))))))
    (let ((err (should-error (financial-chart-resolve-cohort 'bad)
                             :type 'financial-chart-unresolvable-cohort)))
      (should (string-match-p "finance.market.bar-return" (cadr err)))
      (should (string-match-p "financial-chart-recipe-evaluators" (cadr err))))))

(ert-deftest financial-chart-cohort-unknown-name-signals ()
  "Resolving an undefined cohort name signals the typed error."
  (should-error (financial-chart-resolve-cohort 'does-not-exist)
                :type 'financial-chart-unresolvable-cohort))

(ert-deftest financial-chart-cohort-all-oscillator-resolves-empty ()
  "An all-oscillator cohort resolves to an empty overlay set (excluded,
not an error)."
  (should (null (financial-chart-resolve-cohort 'momentum))))

(ert-deftest financial-chart-cohort-describe-flags-oscillator ()
  "Describe flags an oscillator member `needs-oscillator-panel'."
  (let* ((desc (financial-chart-describe-cohort 'mean-reversion))
         (members (plist-get desc :members))
         (rsi (cl-find-if
               (lambda (m) (equal (plist-get (plist-get m :member) :indicator)
                                  "finance.market.rsi-14"))
               members)))
    (should rsi)
    (should (eq (plist-get rsi :status) 'needs-oscillator-panel))))

(ert-deftest financial-chart-cohort-overlay-safety-probed-from-value ()
  "Overlay-safety is derived from the record's `attributes.value' (bounds
/ unit), not a hardcoded symbol list; describe surfaces the live probe."
  (should (financial-chart--catalog-value-oscillator-p
           (alist-get 'value (alist-get 'attributes
                                        financial-chart-test--rsi-record))))
  (should-not (financial-chart--catalog-value-oscillator-p
               (alist-get 'value (alist-get 'attributes
                                            financial-chart-test--sma-record))))
  (let ((financial-chart-indicator-cohorts
         '((c :doc "d"
              :members ((:indicator "finance.market.rsi-14" :params (:period 14)))))))
    (cl-letf (((symbol-function 'david-core-resource-get)
               (lambda (&rest _) financial-chart-test--rsi-record)))
      (let* ((desc (financial-chart-describe-cohort 'c))
             (m (car (plist-get desc :members))))
        (should (eq (plist-get m :catalog-live) t))
        (should (eq (plist-get m :probed-oscillator) t))))))

(ert-deftest financial-chart-cohort-describe-soft-fails-without-bridge ()
  "Describe of a catalog member downgrades to :unknown when the .3 bridge
is absent (fboundp soft-fail), never erroring."
  (let ((financial-chart-indicator-cohorts
         '((c :doc "d"
              :members ((:indicator "finance.market.sma" :params (:window 20)))))))
    (cl-letf (((symbol-function 'david-core-resource-get) nil))
      (fmakunbound 'david-core-resource-get)
      (let* ((desc (financial-chart-describe-cohort 'c))
             (m (car (plist-get desc :members))))
        (should (eq (plist-get m :catalog-live) :unknown))))))

(ert-deftest financial-chart-cohort-list-summarizes ()
  "`financial-chart-list-cohorts' returns a pure per-cohort summary."
  (let ((rows (financial-chart-list-cohorts)))
    (let ((tf (assq 'trend-following rows)))
      (should tf)
      (should (= (plist-get (cdr tf) :members) 3))
      (should (= (plist-get (cdr tf) :resolvable) 3))
      (should (= (plist-get (cdr tf) :excluded) 0)))
    (let ((mr (cdr (assq 'mean-reversion rows))))
      (should (= (plist-get mr :resolvable) 1))
      (should (= (plist-get mr :excluded) 1)))))

(ert-deftest financial-chart-cohort-doctor-checks-pass-and-fail ()
  "The L4 doctor hook passes for resolvable cohorts and fails (with a
remediation) for a broken one."
  (let ((checks (financial-chart-cohort-doctor-checks)))
    (should (cl-every (lambda (c) (eq (plist-get c :status) 'pass)) checks))
    (should (cl-every (lambda (c) (equal (plist-get c :layer) "L4")) checks)))
  (let ((financial-chart-indicator-cohorts
         '((broken :doc "d" :members ((:indicator "nope.no.eval" :params nil))))))
    (let ((c (car (financial-chart-cohort-doctor-checks))))
      (should (eq (plist-get c :status) 'fail))
      (should (string-match-p "nope.no.eval" (plist-get c :detail)))
      (should (> (length (plist-get c :remediation)) 0)))))

(provide 'financial-chart-test)
;;; financial-chart-test.el ends here
