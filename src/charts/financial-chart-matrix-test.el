;;; financial-chart-matrix-test.el --- Matrix and volume-profile ERT tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-matrix-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-matrix-test--dir))
(require 'financial-chart)

(defconst financial-chart-matrix-test--data
  '(:labels ("SPY" "QQQ" "TLT")
    :rows ((1.0 0.82 -0.12)
           (0.82 1.0 -0.08)
           (-0.12 -0.08 1.0))))

(defconst financial-chart-matrix-test--bars
  '((:open 100 :high 101 :low 99 :close 100 :volume 100)
    (:open 101 :high 104 :low 100 :close 103 :volume 300)
    (:open 103 :high 105 :low 102 :close 104 :volume 200)))

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

(ert-deftest financial-chart-matrix-test-explains-cell-range-and-count ()
  (let ((summary (financial-chart-explain 'heatmap financial-chart-matrix-test--data
                                          :backend 'text)))
    (should (eq (plist-get summary :shape) 'matrix))
    (should (= (plist-get summary :points) 9))
    (should (= (plist-get summary :min) -0.12))
    (should (= (plist-get summary :max) 1.0))))

(ert-deftest financial-chart-matrix-test-volume-profile-preserves-total-volume ()
  (let* ((profile (financial-chart-volume-profile
                   financial-chart-matrix-test--bars 6))
         (total (apply #'+ (plist-get profile :volumes))))
    (should (< (abs (- total 600.0)) 0.0001))
    (should (= (plist-get profile :poc) 3)))
  (should-not (plist-get (financial-chart-volume-profile '((:open 1 :high 2 :low 0 :close 1)) 3) :poc)))

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
