;;; financial-chart-series-x-test.el --- X-aware series rendering tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'financial-chart-batch)
(defvar financial-chart-series-x-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-series-x-test--dir))
(require 'financial-chart)

(defconst financial-chart-series-x-test--fixtures
  (expand-file-name "fixtures" financial-chart-series-x-test--dir))

(defconst financial-chart-series-x-test--irregular
  '((0 10.0) (1 40.0) (9 20.0) (10 30.0)))

(defconst financial-chart-series-x-test--time
  '((1704110400000 10.0) (1704196800000 40.0)
    (1704542400000 20.0) (1704628800000 30.0)))

(defun financial-chart-series-x-test--golden (name actual)
  "Compare ACTUAL, without text properties, with fixture NAME."
  (let ((file (expand-file-name name financial-chart-series-x-test--fixtures))
        (text (substring-no-properties actual)))
    (when (getenv "FINANCIAL_CHART_UPDATE_GOLDEN")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file)))
    (should (file-exists-p file))
    (let ((coding-system-for-read 'utf-8-unix))
      (should (equal text
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))))))

(ert-deftest financial-chart-series-x-resamples-irregular-x-coordinates ()
  (let ((samples (financial-chart-series-resample
                  '((0 0.0) (1 10.0) (9 0.0)) 10)))
    (should (= (length samples) 10))
    (should (= (nth 0 samples) 0.0))
    (should (= (nth 1 samples) 10.0))
    (should (= (nth 5 samples) 5.0))
    (should (= (nth 9 samples) 0.0))))

(ert-deftest financial-chart-series-x-preserves-index-sampling-without-x ()
  (should (equal (financial-chart-series-resample '(1 2 3) 10)
                 (financial-chart-resample '(1 2 3) 10))))

(ert-deftest financial-chart-series-x-interpolates-regular-x-coordinates ()
  (should (equal (financial-chart-series-resample
                  '((0 0.0) (1 10.0) (2 0.0)) 2)
                 '(0.0 0.0))))

(ert-deftest financial-chart-series-x-interpolates-descending-coordinates ()
  (should (equal (financial-chart-series-resample
                  '((10 0.0) (5 10.0) (0 0.0)) 3)
                 '(0.0 10.0 0.0))))

(ert-deftest financial-chart-series-x-adds-two-to-four-date-labels ()
  (let ((labels (financial-chart-series-x-axis-labels
                 financial-chart-series-x-test--time 30)))
    (should (<= 2 (length labels) 4))
    (should (equal (mapcar #'cadr labels) '("01/01" "01/03" "01/05" "01/07")))
    (should (equal (mapcar #'cadr
                           (financial-chart-series-x-axis-labels
                            (reverse financial-chart-series-x-test--time) 30))
                   '("01/07" "01/05" "01/03" "01/01")))
    (should-not (financial-chart-series-x-axis-labels '((1 2) (2 3)) 30))))

(ert-deftest financial-chart-series-x-text-axis-aligns-with-plot-gutter ()
  (let ((axis (financial-chart-text--series-x-axis
               financial-chart-series-x-test--time 30 'financial-chart-dim 7)))
    (should (string-prefix-p (make-string 7 ?\s) axis))
    (should (string-match-p "       01/01" axis))))

(ert-deftest financial-chart-series-x-aware-text-goldens ()
  (financial-chart-series-x-test--golden
   "area-x-aware.txt"
   (financial-chart-text-area financial-chart-series-x-test--irregular
                              :width 30 :height 5))
  (financial-chart-series-x-test--golden
   "line-x-aware.txt"
   (financial-chart-text-line financial-chart-series-x-test--irregular
                              :width 30 :height 5))
  (financial-chart-series-x-test--golden
   "sparkline-x-aware.txt"
   (financial-chart-text-sparkline financial-chart-series-x-test--irregular
                                  :width 30)))

(ert-deftest financial-chart-series-x-time-axis-text-goldens ()
  (financial-chart-series-x-test--golden
   "area-time-axis.txt"
   (financial-chart-text-area financial-chart-series-x-test--time
                              :width 30 :height 5))
  (financial-chart-series-x-test--golden
   "line-time-axis.txt"
   (financial-chart-text-line financial-chart-series-x-test--time
                              :width 30 :height 5))
  (financial-chart-series-x-test--golden
   "sparkline-time-axis.txt"
   (financial-chart-text-sparkline financial-chart-series-x-test--time
                                  :width 30)))

(ert-deftest financial-chart-series-x-log-scale-renders-positive-values ()
  (let ((series '(1.0 10.0 100.0)))
    (should-not (equal (substring-no-properties
                        (financial-chart-text-area series :width 20 :height 5))
                       (substring-no-properties
                        (financial-chart-text-area series :width 20 :height 5
                                                   :scale 'log))))
    (should (stringp (financial-chart-text-line series :width 20 :height 5
                                                :scale 'log)))
    (dolist (kind '(area line))
      (should (string-prefix-p
               "<svg "
               (financial-chart-plot kind series :backend 'svg :scale 'log))))))

(ert-deftest financial-chart-series-x-log-scale-preserves-narrow-ranges ()
  (let ((svg (financial-chart-svg-area '(100.0 100.01)
                                       :width 300 :height 120 :scale 'log)))
    (should (string-match-p "290\\.0 10\\.0" svg))))

(ert-deftest financial-chart-series-x-log-scale-rejects-nonpositive-values ()
  (dolist (kind '(area line))
    (dolist (backend '(text svg))
      (let ((caught nil))
        (condition-case err
            (financial-chart-plot kind '((1 2.0) (2 0.0) (3 4.0))
                                  :backend backend :scale 'log)
          (financial-chart-invalid-data (setq caught err)))
        (should caught)
        (should (equal (plist-get (cddr caught) :code) "invalid_data"))
        (should (= (plist-get (cddr caught) :index) 1))))))

(ert-deftest financial-chart-series-x-json-scale-is-a-symbol ()
  (let ((spec (financial-chart-batch-spec
               '((kind . "area") (data 1.0 10.0 100.0)
                 (backend . "text") (scale . "log")))))
    (should (eq (plist-get spec :scale) 'log))
    (should (stringp (apply #'financial-chart-plot
                            (list (plist-get spec :kind) (plist-get spec :data)
                                  :backend 'text :scale (plist-get spec :scale)))))))

(ert-deftest financial-chart-series-x-svg-time-axis-is-well-formed ()
  (let ((svg (financial-chart-plot 'line financial-chart-series-x-test--time
                                  :backend 'svg :scale 'log
                                  :pixel-width 480 :pixel-height 220)))
    (should (string-match-p "01/01" svg))
    (should (string-match-p "01/07" svg))
    (should (string-match-p "text-anchor=\"end\"[^>]*>01/07" svg))
    (when (fboundp 'libxml-parse-xml-region)
      (with-temp-buffer
        (insert svg)
        (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max)))))))))

(ert-deftest financial-chart-series-x-svg-reduces-ticks-at-narrow-widths ()
  (let ((svg (financial-chart-svg-area financial-chart-series-x-test--time
                                       :width 160 :height 180)))
    (should (string-match-p ">01/01</text>" svg))
    (should (string-match-p ">01/07</text>" svg))
    (should-not (string-match-p ">01/04</text>" svg))))

(provide 'financial-chart-series-x-test)
;;; financial-chart-series-x-test.el ends here
