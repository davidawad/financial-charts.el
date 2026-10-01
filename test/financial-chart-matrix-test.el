;;; financial-chart-matrix-test.el --- Matrix and volume-profile ERT tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-matrix-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-matrix-test--dir))
(require 'financial-chart)

(defconst financial-chart-matrix-test--fixtures
  (expand-file-name "fixtures" financial-chart-matrix-test--dir))

(defconst financial-chart-matrix-test--data
  '(:labels ("SPY" "QQQ" "TLT")
    :rows ((1.0 0.82 -0.12)
           (0.82 1.0 -0.08)
           (-0.12 -0.08 1.0))))

(defconst financial-chart-matrix-test--bars
  '((:open 100 :high 101 :low 99 :close 100 :volume 100)
    (:open 101 :high 104 :low 100 :close 103 :volume 300)
    (:open 103 :high 105 :low 102 :close 104 :volume 200)))

(defun financial-chart-matrix-test--golden (name actual)
  "Compare ACTUAL, without text properties, to fixture NAME."
  (let ((file (expand-file-name name financial-chart-matrix-test--fixtures))
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

(defun financial-chart-matrix-test--xml (svg)
  "Parse SVG when this Emacs has libxml support."
  (when (fboundp 'libxml-parse-xml-region)
    (with-temp-buffer
      (insert svg)
      (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max))))))))

(defun financial-chart-matrix-test--count (regexp text)
  "Count occurrences of REGEXP in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (count-matches regexp)))

(defun financial-chart-matrix-test--rects-fit (svg width height)
  "Assert each SVG rectangle in SVG fits within WIDTH x HEIGHT."
  (with-temp-buffer
    (insert svg)
    (goto-char (point-min))
    (while (re-search-forward "<rect\\([^>]*\\)>" nil t)
      (let ((attrs (match-string 1)))
        (cl-labels ((attr (name)
                          (if (string-match (concat name "=\"\\([0-9.]+\\)\"") attrs)
                              (string-to-number (match-string 1 attrs))
                            0)))
          (should (<= (+ (attr "x") (attr "width")) (+ width 0.01)))
          (should (<= (+ (attr "y") (attr "height")) (+ height 0.01))))))))

(ert-deftest financial-chart-matrix-test-registers-kinds-and-shape ()
  (should (eq (plist-get (financial-chart-describe-kind 'heatmap) :shape) 'matrix))
  (should (eq (plist-get (financial-chart-describe-kind 'volume-profile) :shape) 'ohlc))
  (should (financial-chart-validate 'heatmap financial-chart-matrix-test--data))
  (should (financial-chart-validate 'volume-profile financial-chart-matrix-test--bars)))

(ert-deftest financial-chart-matrix-test-validates-square-numeric-data ()
  (should (financial-chart-validate
           'heatmap '(:labels ["A" "B"] :rows [[1 0.4] [0.4 1]])))
  (let ((err (should-error
              (financial-chart-validate
               'heatmap '(:labels ("A" "B") :rows ((1 0) (0))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 1)))
  (should-error
   (financial-chart-validate 'heatmap '(:labels ("A") :rows (("high"))))
   :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-matrix-test-vector-rows-render-svg-cells ()
  (let* ((labels ["A" "B"])
         (list-data '(:labels ("A" "B") :rows ((1 0.2) (0.2 1))))
         (vector-data (list :labels labels :rows [[1 0.2] [0.2 1]]))
         (list-svg (financial-chart-plot 'heatmap list-data :backend 'svg))
         (vector-svg (financial-chart-plot 'heatmap vector-data :backend 'svg)))
    (should (= (financial-chart-matrix-test--count "<rect" list-svg)
               (financial-chart-matrix-test--count "<rect" vector-svg)))))

(ert-deftest financial-chart-matrix-test-rectangular-matrix-has-axis-labels ()
  (let* ((data '(:labels ("Asset A" "Asset B")
                 :column-labels ("Bear" "Base" "Bull")
                 :rows ((-0.7 0.1 0.8) (-0.3 0.2 0.6))))
         (text (financial-chart-plot 'heatmap data :backend 'text))
         (svg (financial-chart-plot 'heatmap data :backend 'svg)))
    (should (financial-chart-validate 'heatmap data))
    (should (string-match-p "Asset A" text))
    (should (string-match-p "Bull" text))
    (should (string-match-p "Asset B" svg))
    (should (string-match-p "Bear" svg))
    (should (= (financial-chart-matrix-test--count "<rect" svg) 55))))

(ert-deftest financial-chart-matrix-test-explains-cell-range-and-count ()
  (let ((summary (financial-chart-explain 'heatmap financial-chart-matrix-test--data
                                          :backend 'text)))
    (should (eq (plist-get summary :shape) 'matrix))
    (should (= (plist-get summary :points) 9))
    (should (= (plist-get summary :min) -0.12))
    (should (= (plist-get summary :max) 1.0)))
  (should (equal (financial-chart-matrix--range '(-1 0 0.5 1)) '(-1.0 . 1.0)))
  (should (equal (financial-chart-matrix--range '(4 6)) '(4 . 6))))

(ert-deftest financial-chart-matrix-test-volume-profile-preserves-total-volume ()
  (let* ((profile (financial-chart-matrix--volume-data
                   financial-chart-matrix-test--bars 6))
         (total (apply #'+ (plist-get profile :volumes))))
    (should (< (abs (- total 600.0)) 0.0001))
    (should (= (plist-get profile :poc) 3))))

(ert-deftest financial-chart-matrix-test-text-heatmap-golden ()
  (financial-chart-matrix-test--golden
   "heatmap.txt"
   (financial-chart-plot 'heatmap financial-chart-matrix-test--data :backend 'text)))

(ert-deftest financial-chart-matrix-test-text-volume-profile-golden ()
  (financial-chart-matrix-test--golden
   "volume-profile.txt"
   (financial-chart-plot 'volume-profile financial-chart-matrix-test--bars
                         :backend 'text :bins 6 :width 12 :unit "$")))

(ert-deftest financial-chart-matrix-test-heatmap-faces-show-sign ()
  (let ((chart (financial-chart-plot 'heatmap financial-chart-matrix-test--data
                                     :backend 'text)))
    (should (string-match "▓" chart))
    (should (eq (get-text-property (match-beginning 0) 'face chart)
                'financial-chart-up))
    (should (string-match "░" chart))
    (should (eq (get-text-property (match-beginning 0) 'face chart)
                'financial-chart-down))))

(ert-deftest financial-chart-matrix-test-volume-profile-marks-poc-and-close ()
  (let* ((chart (financial-chart-plot 'volume-profile financial-chart-matrix-test--bars
                                      :backend 'text :bins 6 :width 12))
         (p (string-match "P" chart))
         (c (string-match "C" chart)))
    (should p)
    (should c)
    (should (eq (get-text-property p 'face chart) 'financial-chart-accent))
    (should (eq (get-text-property c 'face chart) 'financial-chart-down))
    (should (string-match "◀ POC = point of control" chart))))

(ert-deftest financial-chart-matrix-test-volume-profile-rejects-invalid-bin-count ()
  (should-error
   (financial-chart-plot 'volume-profile financial-chart-matrix-test--bars
                         :backend 'text :bins 0)
   :type 'financial-chart-error)
  (should-error
   (financial-chart-plot 'volume-profile financial-chart-matrix-test--bars
                         :backend 'text :bins 1000000000)
   :type 'financial-chart-error))

(ert-deftest financial-chart-matrix-test-volume-profile-rejects-negative-volume ()
  (should-error
   (financial-chart-validate
    'volume-profile '((:open 1 :high 2 :low 0 :close 1 :volume -1)))
   :type 'financial-chart-invalid-data)
  (let ((err (should-error
              (financial-chart-plot
               'volume-profile
               '((:open 1 :high 2 :low 0 :close 1 :volume -1))
               :backend 'text)
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 0))))

(ert-deftest financial-chart-matrix-test-labels-and-titles-drop-controls ()
  (let* ((escape (string 27))
         (bell (string 7))
         (unsafe (concat "A" escape "]52;c;clipboard" bell))
         (data (list :labels (list "SAFE" unsafe) :rows '((1 0) (0 1))))
         (text (financial-chart-plot 'heatmap data :backend 'text :title unsafe))
         (svg (financial-chart-plot 'heatmap data :backend 'svg :title unsafe))
         (profile-text (financial-chart-plot 'volume-profile
                                             financial-chart-matrix-test--bars
                                             :backend 'text :title unsafe :unit unsafe))
         (profile-svg (financial-chart-plot 'volume-profile
                                            financial-chart-matrix-test--bars
                                            :backend 'svg :title unsafe :unit unsafe)))
    (dolist (rendered (list text svg profile-text profile-svg))
      (should-not (string-match-p (regexp-quote escape) rendered))
      (should-not (string-match-p (regexp-quote bell) rendered)))
    (financial-chart-matrix-test--xml svg)
    (financial-chart-matrix-test--xml profile-svg)))

(ert-deftest financial-chart-matrix-test-volume-profile-without-volume-has-no-poc ()
  (let* ((bars '((:open 1 :high 2 :low 0 :close 1)))
         (text (financial-chart-plot 'volume-profile bars :backend 'text :bins 3))
         (svg (financial-chart-plot 'volume-profile bars :backend 'svg :bins 3)))
    (should-not (string-match-p "^P " text))
    (should (string-match-p "POC unavailable: no positive volume" text))
    (should-not (string-match-p ">POC</text>" svg))
    (should (string-match-p ">POC unavailable</text>" svg))))

(ert-deftest financial-chart-matrix-test-heatmap-svg-colors-and-parses ()
  (let ((svg (financial-chart-plot 'heatmap financial-chart-matrix-test--data
                                  :backend 'svg :title "Correlations")))
    (should (string-match-p "#2166ac" svg))
    (should (string-match-p "#b2182b" svg))
    (should (string-match-p "Correlations" svg))
    (financial-chart-matrix-test--xml svg)))

(ert-deftest financial-chart-matrix-test-large-heatmap-grid-fits-svg ()
  (let* ((labels (make-list 30 "x"))
         (row (make-list 30 1))
         (data (list :labels labels :rows (make-list 30 row)))
         (svg (financial-chart-plot 'heatmap data :backend 'svg)))
    (financial-chart-matrix-test--xml svg)
    (financial-chart-matrix-test--rects-fit svg 600 240)))

(ert-deftest financial-chart-matrix-test-volume-profile-svg-markers-and-parses ()
  (let ((svg (financial-chart-plot 'volume-profile financial-chart-matrix-test--bars
                                  :backend 'svg :bins 6 :unit "$")))
    (should (string-match-p "POC" svg))
    (should (string-match-p "Close \$104" svg))
    (should (string-match-p "OHLCV estimate: volume is spread uniformly" svg))
    (financial-chart-matrix-test--xml svg)))

(ert-deftest financial-chart-matrix-test-doctor-renders-examples ()
  (dolist (kind '(heatmap volume-profile))
    (let ((row (cl-find-if
                (lambda (check)
                  (equal (plist-get check :name)
                         (format "kind %s renders" kind)))
                (financial-chart-doctor-checks))))
      (should row)
      (should (eq (plist-get row :status) 'pass)))))

(provide 'financial-chart-matrix-test)
;;; financial-chart-matrix-test.el ends here
