;;; financial-chart-returns-test.el --- Returns chart tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-returns-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-returns-test--dir))
(require 'financial-chart)

(defconst financial-chart-returns-test--series
  '(("Jan" 100) ("Feb" 120) ("Mar" 90) ("Apr" 105) ("May" 80) ("Jun" 110)))

(defun financial-chart-returns-test--golden (name actual)
  "Compare ACTUAL with the text fixture NAME, ignoring properties."
  (let ((file (expand-file-name (concat "fixtures/" name)
                                financial-chart-returns-test--dir))
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

(ert-deftest financial-chart-returns-drawdowns-preserve-coordinates ()
  (let ((drawdowns (financial-chart-drawdowns financial-chart-returns-test--series)))
    (should (= (length drawdowns) 6))
    (should (equal (car (nth 4 drawdowns)) "May"))
    (should (= (cdr (nth 0 drawdowns)) 0))
    (should (= (cdr (nth 2 drawdowns)) -0.25))
    (should (= (cdr (nth 4 drawdowns)) (- (/ 1.0 3))))))

(ert-deftest financial-chart-returns-drawdowns-use-source-indexes ()
  (should (equal (financial-chart-drawdowns '(100 80 90))
                 '((0 . 0.0) (1 . -0.2) (2 . -0.1))))
  (should (equal (financial-chart-drawdowns '((a 100) (b nil) (c 80)))
                 '((a . 0.0) (c . -0.2))))
  (let ((err (should-error (financial-chart-drawdowns '(0 1))
                           :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 0))))

(ert-deftest financial-chart-returns-formats-numeric-time-coordinates ()
  (should (equal (financial-chart-returns--location
                  '(:x 1700000000000 :index 2))
                 "2023-11-14"))
  (should (equal (financial-chart-returns--location '(:x 4 :index 3))
                 "index 4")))

(ert-deftest financial-chart-returns-calculates-consecutive-simple-returns ()
  (let ((returns (financial-chart-returns financial-chart-returns-test--series)))
    (should (= (length returns) 5))
    (should (= (nth 0 returns) 0.2))
    (should (= (nth 1 returns) -0.25))
    (should (= (nth 2 returns) (/ 1.0 6)))
    (should (= (nth 3 returns) (- (/ 25.0 105))))
    (should (= (nth 4 returns) 0.375)))
  (should (equal (financial-chart-returns '((a 100) (b nil) (c 125)))
                 '(0.25)))
  (should-not (financial-chart-returns '(100)))
  (let ((err (should-error (financial-chart-returns '(5 0 2))
                           :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 2))))

(ert-deftest financial-chart-returns-histogram-bins-cover-range-and-counts ()
  (let* ((bins (financial-chart-histogram-bins '(-1 -0.5 0.1 0.6 1.0) 4)))
    (should (= (length bins) 4))
    (should (= (caar bins) -1))
    (should (= (cadr (car (last bins))) 1))
    (should (equal (mapcar #'caddr bins) '(1 1 1 2)))
    (should (= (apply #'+ (mapcar #'caddr bins)) 5)))
  (should (equal (mapcar #'caddr (financial-chart-histogram-bins '(0.1 0.1 0.1) 4))
                 '(0 0 3 0)))
  (should (= (length (financial-chart-histogram-bins '(1 2)))
             financial-chart-returns-default-bins))
  (should-not (financial-chart-histogram-bins nil))
  (should-error (financial-chart-histogram-bins '(1 2) 0)
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-returns-text-drawdown-golden ()
  (financial-chart-returns-test--golden
   "drawdown.txt"
   (financial-chart-plot 'drawdown financial-chart-returns-test--series
                         :backend 'text :width 24 :height 6)))

(ert-deftest financial-chart-returns-text-histogram-golden ()
  (financial-chart-returns-test--golden
   "histogram.txt"
   (financial-chart-plot 'histogram financial-chart-returns-test--series
                         :backend 'text :width 36 :height 6 :bins 8)))

(ert-deftest financial-chart-returns-drawdown-has-zero-at-the-top ()
  (let* ((chart (financial-chart-text-drawdown '(100 120 90 105) :width 12 :height 5))
         (first-row (car (split-string chart "\n"))))
    (should (string-prefix-p "     0% " first-row))
    (should (string-match-p "max drawdown -25% at index 2" chart)))
  (let* ((chart (financial-chart-text-drawdown '(10 11 12) :width 8 :height 4))
         (first-row (car (split-string chart "\n"))))
    (should (string-match-p "⠉" first-row))))

(ert-deftest financial-chart-returns-registered-and-doctor-ready ()
  (dolist (kind '(drawdown histogram))
    (let ((description (financial-chart-describe-kind kind)))
      (should (plist-get description :renderers-defined))
      (should (eq t (financial-chart-validate kind
                                              (plist-get description :example))))))
  (should (stringp (financial-chart-plot 'drawdown '(100 90) :backend 'text)))
  (should (string-match-p "kind drawdown renders" (format "%S" (financial-chart-doctor-checks))))
  (should (string-match-p "kind histogram renders" (format "%S" (financial-chart-doctor-checks)))))

(ert-deftest financial-chart-returns-svg-parses-and-has-summary ()
  (dolist (kind '(drawdown histogram))
    (let ((svg (financial-chart-plot kind financial-chart-returns-test--series
                                     :backend 'svg :pixel-width 500 :pixel-height 260
                                     :bins 8)))
      (should (string-prefix-p "<svg " svg))
      (should (string-match-p
               (if (eq kind 'drawdown) "max drawdown" "mean") svg))
      (when (fboundp 'libxml-parse-xml-region)
        (with-temp-buffer
          (insert svg)
          (should (eq 'svg (car (libxml-parse-xml-region
                                 (point-min) (point-max))))))))))

(provide 'financial-chart-returns-test)
;;; financial-chart-returns-test.el ends here
