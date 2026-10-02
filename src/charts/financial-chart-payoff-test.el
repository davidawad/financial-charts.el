;;; financial-chart-payoff-test.el --- payoff renderer tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart)

(defconst financial-chart-payoff-test--payoff
  '((90 50) (95 0) (100 -100) (105 0) (110 50)))

(defconst financial-chart-payoff-test--curves
  '(("T+0" . ((90 30) (100 -20) (110 30)))
    ("T+15" . ((90 20) (100 -5) (110 40)))
    ("T+30" . ((90 10) (100 10) (110 20)))))

(defconst financial-chart-payoff-test--fixtures
  (expand-file-name "../../test/fixtures" (file-name-directory (or load-file-name buffer-file-name))))

(defun financial-chart-payoff-test--golden (name actual)
  "Compare ACTUAL text with fixture NAME, optionally updating it."
  (let ((file (expand-file-name name financial-chart-payoff-test--fixtures))
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

(ert-deftest financial-chart-payoff-test-extreme-labels-use-source-data ()
  (let ((text (financial-chart-plot 'payoff financial-chart-payoff-test--payoff
                                    :backend 'text)))
    (should (string-match-p (regexp-quote "+$50") text))
    (should (string-match-p (regexp-quote "-$100") text))
    (should-not (string-match-p (regexp-quote "-$96.6") text)))
  (let ((svg (financial-chart-plot 'payoff financial-chart-payoff-test--payoff
                                   :backend 'svg)))
    (should (string-match-p (regexp-quote ">+$50</text>") svg))
    (should (string-match-p (regexp-quote ">-$100</text>") svg))))

(ert-deftest financial-chart-payoff-test-curves-text-golden-and-faces ()
  (let* ((text (financial-chart-plot 'payoff-curves
                                     financial-chart-payoff-test--curves
                                     :backend 'text :width 24 :height 8))
         (faces nil)
         (start 0))
    (while (setq start (string-match "●" text start))
      (push (get-text-property start 'face text) faces)
      (setq start (1+ start)))
    (should (equal (nreverse faces)
                   '(financial-chart-up financial-chart-down financial-chart-accent)))
    (should (string-match-p "breakeven \\$96, \\$104" text))
    (financial-chart-payoff-test--golden "payoff-curves.txt" text)))

(ert-deftest financial-chart-payoff-test-curves-reject-different-price-grids ()
  (let ((err (should-error
              (financial-chart-validate
               'payoff-curves
               '(("T+0" . ((90 1) (100 2)))
                 ("T+15" . ((90 1) (105 2)))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 1))
    (should (equal (plist-get (cddr err) :code) "invalid_data"))))

(ert-deftest financial-chart-payoff-test-empty-inner-payoffs-render-nothing ()
  (let ((curves '(("T+0" . nil))))
    (should-not (financial-chart-plot 'payoff-curves curves :backend 'text))
    (should-not (financial-chart-plot 'payoff-curves curves :backend 'svg))))

(ert-deftest financial-chart-payoff-test-curves-svg-polylines-and-xml ()
  (let ((svg (financial-chart-plot 'payoff-curves
                                  financial-chart-payoff-test--curves
                                  :backend 'svg :title "T+ curves")))
    (should (= (with-temp-buffer
                 (insert svg)
                 (how-many "<polyline" (point-min) (point-max)))
               3))
    (dolist (label '("T+0" "T+15" "T+30"))
      (should (string-match-p (regexp-quote label) svg)))
    (dolist (color '("#2e7d32" "#c62828" "#1565c0"))
      (should (string-match-p (regexp-quote color) svg)))
    (when (fboundp 'libxml-parse-xml-region)
      (with-temp-buffer
        (insert svg)
        (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max)))))))))

(provide 'financial-chart-payoff-test)
;;; financial-chart-payoff-test.el ends here
