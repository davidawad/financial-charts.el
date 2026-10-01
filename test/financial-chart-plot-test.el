;;; financial-chart-plot-test.el --- ERT tests for financial-chart -*- lexical-binding: t; -*-

;; Pure data -> chart tests: no network, no windows, no broker.  Rendered
;; charts are compared against golden fixtures in test/fixtures/ (text
;; without properties, SVG as the serialized document); faces are
;; asserted separately.  Regenerate the goldens after an intended visual
;; change with FINANCIAL_CHART_UPDATE_GOLDEN=1 and review the fixture diff.

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-plot-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-plot-test--dir))
(require 'financial-chart)

(defconst financial-chart-plot-test--fixtures (expand-file-name "fixtures" financial-chart-plot-test--dir))

(defun financial-chart-plot-test--golden (name actual)
  "Compare ACTUAL (a string, properties ignored) with fixture NAME."
  (let ((file (expand-file-name name financial-chart-plot-test--fixtures))
        (text (substring-no-properties actual)))
    (when (getenv "FINANCIAL_CHART_UPDATE_GOLDEN")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file)))
    (should (file-exists-p file))
    (let ((expected (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (buffer-string))))
      (should (equal (financial-chart-plot-test--normalize name text)
                     (financial-chart-plot-test--normalize name expected))))))

(defun financial-chart-plot-test--normalize (name s)
  "S, with whitespace around SVG tags dropped when NAME is an .svg fixture.
svg.el's printer puts whitespace between elements in some Emacs builds
and not others; text fixtures stay byte-exact."
  (if (string-suffix-p ".svg" name)
      (replace-regexp-in-string "[ \t\n]*\\(<\\|>\\)[ \t\n]*" "\\1" s)
    s))

(defconst financial-chart-plot-test--series
  '((1 40.0) (2 45.0) (3 50.0) (4 42.0) (5 47.5) (6 39.0) (7 41.0))
  "The tradeboard chart fixture shape: (TS VALUE) pairs.")

(defconst financial-chart-plot-test--wave
  (cl-loop for i from 0 below 200 collect (list i (+ 50 (* 10 (sin (/ i 7.0))))))
  "A longer series that forces resampling.")

(defconst financial-chart-plot-test--payoff
  (cl-loop for p from 90 to 110 collect (list p (- (* 10 (max 0 (- p 100))) 25)))
  "Long call, strike 100, premium 25 (per 10 contracts-ish).")

(defconst financial-chart-plot-test--straddle
  '((90 50) (95 0) (100 -100) (105 0) (110 50)))

(defconst financial-chart-plot-test--bars '(("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950)))

(defmacro financial-chart-plot-test--svg-env (&rest body)
  "Run BODY with a fixed palette and font so SVG output is deterministic."
  `(let ((financial-chart-svg-palette financial-chart-svg--fallback-palette)
         (financial-chart-svg-font-family "monospace")
         (financial-chart-svg-font-size 11))
     ,@body))

;; --- core -----------------------------------------------------------------------

(ert-deftest financial-chart-plot-test-series-values-accepts-every-shape ()
  (should (equal (financial-chart-series-values '(1 2 3)) '(1 2 3)))
  (should (equal (financial-chart-series-values '((a 1) (b 2))) '(1 2)))
  (should (equal (financial-chart-series-values '((a . 1) (b . 2))) '(1 2)))
  (should (equal (financial-chart-series-values [4 5]) '(4 5)))
  (should (equal (financial-chart-series-values '((a 1) (b nil) (c 3))) '(1 3)))
  (should (equal (financial-chart-series-xs '((a 1) (b nil) (c 3))) '(a c))))

(ert-deftest financial-chart-plot-test-resample-averages ()
  ;; ported from tradeboard's chart-columns tests: same semantics
  (should (equal (financial-chart-resample '(10.0 20.0 30.0 40.0) 2) '(15.0 35.0)))
  (should (equal (financial-chart-resample '(10.0 20.0) 80) '(10.0 20.0)))
  (should (null (financial-chart-resample nil 10))))

(ert-deftest financial-chart-plot-test-interpolate-adds-resolution-only ()
  (should (equal (financial-chart-interpolate '((0 0) (10 10)) 3) '(0.0 5.0 10.0)))
  (should (equal (financial-chart-interpolate '((0 0) (5 10) (10 0)) 5) '(0.0 5.0 10.0 5.0 0.0)))
  ;; at least WIDTH points (or no numeric X): plain resampling
  (should (equal (financial-chart-interpolate '((0 1) (1 2) (2 3) (3 4)) 2) '(1.5 3.5)))
  (should (equal (financial-chart-interpolate '(1 2) 4) '(1.0 2.0))))

(ert-deftest financial-chart-plot-test-fmt ()
  (should (equal (financial-chart-fmt 89.3333) "89.3"))
  (should (equal (financial-chart-fmt 89.0) "89"))
  (should (equal (financial-chart-fmt 0.25) "0.2"))
  (should (equal (financial-chart-fmt-money 75 "$") "+$75"))
  (should (equal (financial-chart-fmt-money -25.5 "$") "-$25.5"))
  (should (equal (financial-chart-fmt-money 0) "0")))

(ert-deftest financial-chart-plot-test-range-and-direction ()
  (should (equal (financial-chart-range '(3 1 2)) '(1 . 3)))
  (should (equal (financial-chart-range '(3 1 2) t) '(0 . 3)))
  (should (eq (financial-chart-direction-face '(1 2) 'up 'down) 'up))
  (should (eq (financial-chart-direction-face '(2 1) 'up 'down) 'down)))

(ert-deftest financial-chart-plot-test-breakevens ()
  (should (equal (financial-chart-payoff-breakevens financial-chart-plot-test--payoff) '(102.5)))
  (should (equal (financial-chart-payoff-breakevens financial-chart-plot-test--straddle) '(95 105)))
  (should (equal (financial-chart-payoff-breakevens '((1 -5) (2 5))) '(1.5)))
  (should (null (financial-chart-payoff-breakevens '((1 1) (2 2))))))

(ert-deftest financial-chart-plot-test-ohlc-closes ()
  (should (equal (financial-chart-ohlc-closes '((:open 1 :high 2 :low 0 :close 1.5 :time 10)))
                 '((10 1.5)))))

;; --- text backend -----------------------------------------------------------------

(ert-deftest financial-chart-plot-test-area-golden ()
  (financial-chart-plot-test--golden "area.txt"
                         (financial-chart-text-area financial-chart-plot-test--series :width 20 :height 8 :unit "c"))
  (financial-chart-plot-test--golden "area-resampled.txt"
                         (financial-chart-text-area financial-chart-plot-test--wave :width 40 :height 6)))

(ert-deftest financial-chart-plot-test-area-footer-and-empty ()
  (let ((s (financial-chart-text-area financial-chart-plot-test--series :width 20 :height 4 :unit "¢")))
    (should (string-match-p "last 41¢   range 39–50¢   7 pts\\'" s)))
  (should-not (string-match-p "pts" (financial-chart-text-area '(1 2) :width 4 :height 2 :footer nil)))
  (should (null (financial-chart-text-area nil))))

(ert-deftest financial-chart-plot-test-area-faces-are-caller-supplied ()
  (let* ((s (financial-chart-text-area '(1 2 3) :width 3 :height 2
                                :up-face 'my-up :down-face 'my-down
                                :dim-face 'my-dim :accent-face 'my-accent))
         (block (string-match "█" s)))
    (should (eq (get-text-property 0 'face s) 'my-dim))
    (should (eq (get-text-property block 'face s) 'my-up))
    (should (eq (get-text-property (string-match "last" s) 'face s) 'my-accent)))
  (let ((s (financial-chart-text-area '(3 2 1) :width 3 :height 2)))
    (should (eq (get-text-property (string-match "█" s) 'face s) 'financial-chart-down))))

(ert-deftest financial-chart-plot-test-line-golden ()
  (financial-chart-plot-test--golden "line.txt"
                         (financial-chart-text-line financial-chart-plot-test--wave :width 40 :height 5)))

(ert-deftest financial-chart-plot-test-line-uses-braille ()
  (let ((s (financial-chart-text-line '(1 2 3 4) :width 2 :height 1 :footer nil)))
    (should (cl-every (lambda (c) (or (<= #x2800 c #x28ff) (memq c '(?\s ?\n ?. ?0 ?1 ?2 ?3 ?4))))
                      s))))

(ert-deftest financial-chart-plot-test-sparkline ()
  (should (equal (substring-no-properties (financial-chart-sparkline '(1 2 3 2 5 4 8))) "▁▂▃▂▅▄█"))
  (should (equal (substring-no-properties (financial-chart-sparkline '(5 5 5))) "▄▄▄"))
  (should (equal (financial-chart-sparkline nil) ""))
  (should (= 4 (length (financial-chart-sparkline financial-chart-plot-test--wave :width 4))))
  (should (eq (get-text-property 0 'face (financial-chart-sparkline '(3 1))) 'financial-chart-down)))

(ert-deftest financial-chart-plot-test-payoff-golden ()
  (financial-chart-plot-test--golden "payoff.txt"
                         (financial-chart-text-payoff financial-chart-plot-test--payoff :width 21 :height 6))
  (financial-chart-plot-test--golden "payoff-straddle.txt"
                         (financial-chart-text-payoff financial-chart-plot-test--straddle :width 21 :height 6)))

(ert-deftest financial-chart-plot-test-payoff-zero-split-shares-one-scale ()
  ;; -25..75 in 6 rows: 2 loss rows at 18.75/row fits both signs best
  (should (equal (financial-chart-text--zero-split -25 75 6) '(2 . 18.75)))
  (should (equal (car (financial-chart-text--zero-split 5 10 4)) 0))
  (should (equal (car (financial-chart-text--zero-split -10 -5 4)) 4)))

(ert-deftest financial-chart-plot-test-payoff-faces-by-sign ()
  (let ((s (financial-chart-text-payoff financial-chart-plot-test--straddle :width 5 :height 4 :footer nil)))
    (should (memq 'financial-chart-up
                  (cl-loop for i below (length s) collect (get-text-property i 'face s))))
    (should (memq 'financial-chart-down
                  (cl-loop for i below (length s) collect (get-text-property i 'face s))))))

(ert-deftest financial-chart-plot-test-bars-golden ()
  (financial-chart-plot-test--golden "bars.txt" (financial-chart-text-bars financial-chart-plot-test--bars :width 40 :unit "$")))

(ert-deftest financial-chart-plot-test-depth-bar-sqrt-scale ()
  (should (equal (financial-chart-depth-bar 20 20 16) (make-string 16 ?█)))
  (should (= 8 (length (financial-chart-depth-bar 5 20 16))))
  (should (= 1 (length (financial-chart-depth-bar 0 20 16))))
  (should (eq (get-text-property 0 'face (financial-chart-depth-bar 1 1 4 'x)) 'x)))

(ert-deftest financial-chart-plot-test-ohlc-delegates-to-candles ()
  (let ((bars '((:open 1 :high 2 :low 0.5 :close 1.5 :time 1)
                (:open 1.5 :high 3 :low 1 :close 2.5 :time 2))))
    (cl-letf (((symbol-function 'financial-chart-render)
               (lambda (b h) (format "CANDLES %d %d" (length b) h))))
      (should (equal (financial-chart-text-ohlc bars :height 7) "CANDLES 2 7")))))

;; --- SVG backend ------------------------------------------------------------------------

(ert-deftest financial-chart-plot-test-svg-golden ()
  (financial-chart-plot-test--svg-env
   (financial-chart-plot-test--golden "area.svg" (financial-chart-svg-area financial-chart-plot-test--series :width 300 :height 120
                                                        :unit "c" :title "area"))
   (financial-chart-plot-test--golden "payoff.svg" (financial-chart-svg-payoff financial-chart-plot-test--straddle
                                                            :width 300 :height 140))
   (financial-chart-plot-test--golden "bars.svg" (financial-chart-svg-bars financial-chart-plot-test--bars :width 300 :unit "$"))))

(ert-deftest financial-chart-plot-test-svg-is-well-formed ()
  (financial-chart-plot-test--svg-env
   (dolist (svg (list (financial-chart-svg-area financial-chart-plot-test--wave)
                      (financial-chart-svg-payoff financial-chart-plot-test--payoff)
                      (financial-chart-svg-bars financial-chart-plot-test--bars)))
     (should (string-prefix-p "<svg " svg))
     (when (fboundp 'libxml-parse-xml-region)
       (with-temp-buffer
         (insert svg)
         (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max))))))))))

(ert-deftest financial-chart-plot-test-svg-palette-fallback-in-batch ()
  (let ((financial-chart-svg-palette nil))
    (unless (display-graphic-p)
      (should (equal (financial-chart-svg--color 'up) "#2e7d32")))
    (let ((financial-chart-svg-palette '((up . "#123456"))))
      (should (equal (financial-chart-svg--color 'up) "#123456")))))

(ert-deftest financial-chart-plot-test-svg-ohlc-delegates-when-loaded ()
  (cl-letf (((symbol-function 'financial-chart-render-svg)
             (lambda (bars title) (format "<svg>%d %s</svg>" (length bars) title))))
    (should (equal (financial-chart-svg-ohlc '((:close 1)) :title "T") "<svg>1 T</svg>"))))

;; --- dispatch / buffer ----------------------------------------------------------------

(ert-deftest financial-chart-plot-test-render-dispatches-by-backend ()
  (should (string-match-p "last" (financial-chart-plot 'area '(1 2 3) :backend 'text :width 3 :height 2)))
  (financial-chart-plot-test--svg-env
   (should (string-prefix-p "<svg width=\"320\""
                            (financial-chart-plot 'area '(1 2 3) :backend 'svg :pixel-width 320))))
  (should-error (financial-chart-plot 'pie '(1 2)) :type 'financial-chart-unknown-kind))

(ert-deftest financial-chart-plot-test-insert-auto-is-text-without-images ()
  (unless (display-images-p)
    (with-temp-buffer
      (financial-chart-plot-insert 'area '(1 2 3) :width 3 :height 2)
      (should (string-match-p "last 3" (buffer-string))))
    (with-temp-buffer
      (financial-chart-plot-insert 'area nil)
      (should (equal (buffer-string) "no data")))))

(ert-deftest financial-chart-plot-test-insert-svg-falls-back-without-svg-support ()
  "An Emacs built without SVG images gets the text chart plus a note."
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil)))
    (with-temp-buffer
      (financial-chart-plot-insert 'area '(1 2 3) :backend 'svg :width 3 :height 2)
      (should (string-prefix-p "(this Emacs cannot display SVG" (buffer-string)))
      (should (string-match-p "last 3" (buffer-string))))))

(ert-deftest financial-chart-plot-test-svg-golden-ignores-tag-whitespace ()
  (should (equal (financial-chart-plot-test--normalize "x.svg" "<svg> <rect></rect>\n <text> a</text></svg>")
                 (financial-chart-plot-test--normalize "x.svg" "<svg><rect></rect><text>a</text></svg>")))
  (should-not (equal (financial-chart-plot-test--normalize "x.txt" " a")
                     (financial-chart-plot-test--normalize "x.txt" "a"))))

(ert-deftest financial-chart-plot-test-view-and-toggle ()
  (let ((buf (financial-chart-plot-view 'payoff financial-chart-plot-test--straddle
                            :title "straddle" :buffer "*financial-chart-test*" :width 10 :height 4
                            :backend 'text)))
    (unwind-protect
        (with-current-buffer buf
          (should (derived-mode-p 'financial-chart-plot-mode))
          (should (string-prefix-p "straddle\n\n" (buffer-string)))
          (should (string-match-p "breakeven \\$95, \\$105" (buffer-string)))
          (financial-chart-plot-toggle-backend)
          (should (eq (plist-get (nth 2 financial-chart-plot--spec) :backend) 'svg)))
      (kill-buffer buf))))

(ert-deftest financial-chart-plot-test-demo-renders-every-kind ()
  (let ((buf (financial-chart-demo)))
    (unwind-protect
        (with-current-buffer buf
          (dolist (k '("area" "line" "payoff" "bars" "sparkline"))
            (should (string-match-p k (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest financial-chart-plot-test-doctor-checks-pass ()
  (dolist (c (financial-chart-plot-doctor-checks))
    (should (stringp (plist-get c :name)))
    (should (memq (plist-get c :status) '(pass skip)))))

(provide 'financial-chart-plot-test)
;;; financial-chart-plot-test.el ends here
