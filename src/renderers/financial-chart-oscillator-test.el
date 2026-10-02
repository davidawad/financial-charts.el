;;; financial-chart-oscillator-test.el --- Oscillator panel tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'xml)
(defvar financial-chart-oscillator-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-oscillator-test--dir))
(require 'financial-chart)

(defconst financial-chart-oscillator-test--fixtures
  (expand-file-name "../../test/fixtures" financial-chart-oscillator-test--dir))

(defun financial-chart-oscillator-test--golden (name actual)
  "Compare ACTUAL, with text properties removed, against fixture NAME."
  (let ((file (expand-file-name name financial-chart-oscillator-test--fixtures))
        (text (substring-no-properties actual)))
    (when (getenv "FINANCIAL_CHART_UPDATE_GOLDEN")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file)))
    (should (file-exists-p file))
    (let ((expected (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (buffer-string))))
      (should (equal text expected)))))

(defmacro financial-chart-oscillator-test--with-defaults (&rest body)
  "Run BODY with deterministic small text and SVG chart settings."
  `(let ((financial-chart-max-bars nil)
         (financial-chart-height 4)
         (financial-chart-oscillator-height 5)
         (financial-chart-candle-width 1)
         (financial-chart-candle-gap 1)
         (financial-chart-axis-format "%7.2f ")
         (financial-chart-axis-face nil)
         (financial-chart-show-volume t)
         (financial-chart-volume-height 2)
         (financial-chart-volume-axis-label-count 2)
         (financial-chart-show-x-axis nil)
         (financial-chart-indicators nil)
         (financial-chart-oscillators nil)
         (financial-chart-svg-candle-width 6)
         (financial-chart-svg-candle-gap 3)
         (financial-chart-svg-price-height 100)
         (financial-chart-svg-volume-height 30)
         (financial-chart-svg-oscillator-height 50)
         (financial-chart-svg-margin-left 55)
         (financial-chart-svg-margin-right 20)
         (financial-chart-svg-margin-top 20)
         (financial-chart-svg-margin-bottom 10)
         (financial-chart-svg-font-size 12)
         (financial-chart-svg-font-family "monospace")
         (financial-chart-svg-palette financial-chart-svg--fallback-palette))
     ,@body))

(defconst financial-chart-oscillator-test--bars
  '((:open 98 :high 100 :low 96 :close 99 :volume 10)
    (:open 99 :high 101 :low 97 :close 100 :volume 20)
    (:open 100 :high 102 :low 98 :close 101 :volume 30)
    (:open 101 :high 103 :low 99 :close 102 :volume 40)))

(defun financial-chart-oscillator-test--fixed-series (_bars)
  '(10 30 70 90))

(defun financial-chart-oscillator-test--svg-height (svg)
  "Return the root SVG HEIGHT attribute."
  (should (string-match "<svg[^>]*height=\"\\([0-9]+\\)\"" svg))
  (string-to-number (match-string 1 svg)))

(ert-deftest financial-chart-oscillator-text-golden-and-panel-order ()
  (financial-chart-oscillator-test--with-defaults
   (let* ((financial-chart-oscillators
           '((:fn financial-chart-oscillator-test--fixed-series
              :face bold :glyph ?R)))
          (rendered (financial-chart-render
                     financial-chart-oscillator-test--bars))
          (lines (split-string rendered "\n")))
     (financial-chart-oscillator-test--golden "oscillator.txt" rendered)
     (should (= (length lines) 11))
     (should (string-match-p "100" (nth 4 lines)))
     (should (string-match-p "70" (nth 5 lines)))
     (should (string-match-p "30" (nth 7 lines)))
     (should (string-match-p "0" (nth 8 lines)))
     (should (string-match-p "R" (mapconcat #'identity lines "\n"))))))

(ert-deftest financial-chart-oscillator-text-without-panel-golden ()
  (financial-chart-oscillator-test--with-defaults
   (let ((financial-chart-show-volume nil))
     (financial-chart-oscillator-test--golden
      "oscillator-disabled.txt"
      (financial-chart-render financial-chart-oscillator-test--bars)))))

(ert-deftest financial-chart-oscillator-text-height-only-applies-when-configured ()
  (financial-chart-oscillator-test--with-defaults
   (let ((without (financial-chart-render financial-chart-oscillator-test--bars)))
     (let ((financial-chart-oscillator-height 8))
       (should (equal without
                      (financial-chart-render
                       financial-chart-oscillator-test--bars)))))))

(ert-deftest financial-chart-oscillator-svg-guides-series-and-xml ()
  (financial-chart-oscillator-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-oscillators
           '((:fn financial-chart-oscillator-test--fixed-series :face bold)))
          (svg (financial-chart-render-svg
                financial-chart-oscillator-test--bars "RSI")))
     (with-temp-buffer
       (insert svg)
       (should (libxml-parse-xml-region (point-min) (point-max))))
     (should (string-match-p "stroke-dasharray=\"3 3\"" svg))
     (should (= (length (split-string svg "<polyline")) 2))
     (should (string-match-p ">70</text>" svg))
     (should (string-match-p ">30</text>" svg)))))

(ert-deftest financial-chart-oscillator-svg-height-is-conditional ()
  (financial-chart-oscillator-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (without (financial-chart-render-svg
                    financial-chart-oscillator-test--bars))
          (same-with-different-height
           (let ((financial-chart-svg-oscillator-height 300))
             (financial-chart-render-svg
              financial-chart-oscillator-test--bars)))
          (with
           (let ((financial-chart-oscillators
                  '((:fn financial-chart-oscillator-test--fixed-series)))
                 (financial-chart-svg-oscillator-height 50))
             (financial-chart-render-svg
              financial-chart-oscillator-test--bars))))
     (should (equal without same-with-different-height))
     (should (= (- (financial-chart-oscillator-test--svg-height with)
                   (financial-chart-oscillator-test--svg-height without))
                (+ financial-chart-svg-oscillator-height 10))))))

(ert-deftest financial-chart-oscillator-cohort-resolution-and-description ()
  (let* ((specs (financial-chart-resolve-cohort 'momentum))
         (description (financial-chart-describe-cohort 'momentum))
         (member (car (plist-get description :members)))
         (summary (cdr (assq 'momentum (financial-chart-list-cohorts))))
         (values (funcall (plist-get (car specs) :fn)
                          financial-chart-oscillator-test--bars)))
    (should (= (length specs) 1))
    (should (eq (plist-get (car specs) :panel) 'oscillator))
    (should (eq (plist-get member :status) 'resolved))
    (should (eq (plist-get member :panel) 'oscillator))
    (should (string-match-p "oscillator" (plist-get member :detail)))
    (should (= (plist-get summary :resolvable) 1))
    (should (= (plist-get summary :oscillators) 1))
    (should (cl-every (lambda (value) (or (null value) (<= 0 value 100))) values))))

(ert-deftest financial-chart-oscillator-builtin-rsi-cohort-routes-panel ()
  (let* ((financial-chart-indicator-cohorts
          '((builtin-rsi :doc "rsi"
                         :members ((:fn financial-chart-rsi :args (2))))))
         (spec (car (financial-chart-resolve-cohort 'builtin-rsi))))
    (should (eq (plist-get spec :panel) 'oscillator))
    (should (= (length (funcall (plist-get spec :fn)
                                financial-chart-oscillator-test--bars))
               (length financial-chart-oscillator-test--bars)))))

(ert-deftest financial-chart-oscillator-preset-renders-cohort-in-panel ()
  (let ((financial-chart-presets '((oscillator-only :doc "test" :cohort momentum)))
        captured)
    (cl-letf (((symbol-function 'financial-chart--require-market-data) #'ignore)
              ((symbol-function 'market-data-explain)
               (lambda (&rest _) '(:provider mock)))
              ((symbol-function 'market-data-bars)
               (lambda (&rest _) financial-chart-oscillator-test--bars))
              ((symbol-function 'financial-chart--symbol-title)
               (lambda (&rest _) "mock")))
      (financial-chart--preset-render
       'oscillator-only "TEST" nil
       (lambda (_bars _title)
         (setq captured (list financial-chart-indicators
                              financial-chart-oscillators)))))
    (should (null (car captured)))
    (should (= (length (cadr captured)) 1))
    (should (eq (plist-get (car (cadr captured)) :panel) 'oscillator))))

;;; financial-chart-oscillator-test.el ends here
