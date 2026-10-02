;;; financial-chart-multi-test.el --- Tests for multi-series charts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-multi-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-multi-test--dir))
(require 'financial-chart)
(require 'financial-chart-batch)

(defconst financial-chart-multi-test--data
  '(("AAPL" . ((1 100) (2 104) (3 102) (4 110)))
    ("SPY" . ((1 50) (2 51) (3 51.5) (4 53))))
  "Two series with different starting values, for normalization fixtures.")

(defconst financial-chart-multi-test--fixtures
  (expand-file-name "../../test/fixtures" financial-chart-multi-test--dir))

(defun financial-chart-multi-test--golden (name actual)
  "Compare ACTUAL, ignoring text properties, with fixture NAME."
  (let ((file (expand-file-name name financial-chart-multi-test--fixtures))
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

(ert-deftest financial-chart-multi-test-registry-and-explain ()
  (let ((plan (financial-chart-explain 'multi financial-chart-multi-test--data
                                       :backend 'svg :normalize 100 :unit "%")))
    (should (eq (plist-get plan :shape) 'multi-series))
    (should (eq (plist-get plan :renderer) 'financial-chart-svg-multi))
    (should (eq (plist-get plan :valid) t))
    (should (= (plist-get plan :points) 8))
    (should (= (plist-get plan :min) 100))
    (should (< (abs (- (plist-get plan :max) 110)) 1e-8))))

(ert-deftest financial-chart-multi-test-validator-indexes-bad-series ()
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-validate
                    'multi '(("AAPL" . (1 2)) ("BROKEN" . (1 "bad"))))
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)
    (should (equal (plist-get (nthcdr 2 err) :index) 1))))

(ert-deftest financial-chart-multi-test-normalization-and-zero-base ()
  (let ((prepared (financial-chart-multi--prepare financial-chart-multi-test--data 100)))
    (should (= (car (cdr (car prepared))) 100))
    (should (< (abs (- (car (last (cdr (car prepared)))) 110)) 1e-8))
    (should (= (car (cdr (cadr prepared))) 100))
    (should (< (abs (- (car (last (cdr (cadr prepared)))) 106)) 1e-8)))
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-multi--prepare
                    '(("A" . (1 2)) ("B" . (0 1))) 100)
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)
    (should (equal (plist-get (nthcdr 2 err) :index) 1))))

(ert-deftest financial-chart-multi-test-explain-rejects-zero-normalization-base ()
  (let ((plan (financial-chart-explain 'multi '(("X" . (0 10)))
                                       :backend 'text :normalize 100)))
    (should (string-match-p "starts at zero" (plist-get plan :valid)))
    (should-not (plist-member plan :min)))
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-plot 'multi '(("X" . (0 10)))
                                         :backend 'text :normalize 100)
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)))

(ert-deftest financial-chart-multi-test-rejects-invalid-normalize-type ()
  (let ((err (condition-case condition
                 (progn
                   (financial-chart-plot 'multi financial-chart-multi-test--data
                                         :backend 'text :normalize "100")
                   nil)
               (financial-chart-invalid-data condition))))
    (should err)
    (should (string-match-p "must be nil or a number" (cadr err)))))

(ert-deftest financial-chart-multi-test-text-legend-uses-latest-value ()
  (let ((chart (financial-chart-text-multi
                (list (cons "S" (number-sequence 1 100)))
                :width 2 :height 3)))
    (should (string-match-p "S 100" chart))))

(ert-deftest financial-chart-multi-test-batch-nested-series-data ()
  (let* ((json "{\"kind\":\"multi\",\"data\":[[\"AAPL\",[[1,100],[2,102]]]],\"backend\":\"text\"}")
         (parsed (json-parse-string json :object-type 'alist :array-type 'list))
         (spec (financial-chart-batch-spec parsed))
         (chart (financial-chart-plot-spec spec)))
    (should (equal (plist-get spec :data)
                   '(("AAPL" . ((1 100) (2 102))))))
    (should (string-match-p "AAPL 102" chart))))

(ert-deftest financial-chart-multi-test-batch-example-round-trip ()
  (let* ((json (json-encode (financial-chart-batch--example 'multi)))
         (parsed (json-parse-string json :object-type 'alist :array-type 'list))
         (spec (financial-chart-batch-spec parsed))
         (chart (financial-chart-plot-spec spec)))
    (should (string-match-p "AAPL" chart))
    (should (string-match-p "SPY" chart))))

(ert-deftest financial-chart-multi-test-text-golden-and-faces ()
  (let* ((chart (financial-chart-plot 'multi financial-chart-multi-test--data
                                      :backend 'text :width 24 :height 6
                                      :normalize 100 :unit "%"))
         (aapl (string-match "AAPL" chart))
         (spy (string-match "SPY" chart)))
    (financial-chart-multi-test--golden "multi.txt" chart)
    (should (eq (get-text-property aapl 'face chart) 'financial-chart-up))
    (should (eq (get-text-property spy 'face chart) 'financial-chart-down))))

(ert-deftest financial-chart-multi-test-svg-lines-colors-and-xml ()
  (let ((svg (financial-chart-plot 'multi financial-chart-multi-test--data
                                   :backend 'svg :pixel-width 420 :pixel-height 220
                                   :normalize 100 :unit "%" :title "Benchmark")))
    (with-temp-buffer
      (insert svg)
      (should (= 2 (how-many "<polyline" (point-min) (point-max)))))
    (should (string-match-p "#2e7d32" svg))
    (should (string-match-p "#c62828" svg))
    (should (string-match-p "AAPL 110%" svg))
    (should (string-match-p "SPY 106%" svg))
    (should (string-match-p "range 100 to 110" svg))
    (when (fboundp 'libxml-parse-xml-region)
      (with-temp-buffer
        (insert svg)
        (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max)))))))))

(ert-deftest financial-chart-multi-test-text-label-control-characters ()
  (let* ((label (concat "A" (string 27) "]52;c;Y2xpcA==" (string 7)))
         (chart (financial-chart-text-multi
                 (list (cons label '((1 1) (2 2)))))))
    (should (string-match-p "A ]52;c;Y2xpcA== " chart))
    (should-not (string-match-p (string 27) chart))
    (should-not (string-match-p (string 7) chart))))

(ert-deftest financial-chart-multi-test-svg-singletons-use-shared-scale-and-distinct-colors ()
  (let* ((data (mapcar (lambda (index)
                         (cons (format "S%d" index) (list (1+ index))))
                       (number-sequence 0 4)))
         (svg (financial-chart-svg-multi data))
         (start 0)
         y-positions
         colors)
    (while (string-match "<polyline points=\"\\([^\"]+\\)\"[^>]*stroke=\"\\([^\"]+\\)\""
                         svg start)
      (let* ((end (match-end 0))
             (points (match-string 1 svg))
             (color (match-string 2 svg))
             (first-point (car (split-string points ",")))
             (y (string-to-number (cadr (split-string first-point " ")))))
        (push y y-positions)
        (push color colors)
        (setq start end)))
    (setq y-positions (nreverse y-positions)
          colors (nreverse colors))
    (should (= (length y-positions) 5))
    (should (> (nth 0 y-positions) (nth 1 y-positions)))
    (should (> (nth 1 y-positions) (nth 2 y-positions)))
    (should (= (length colors) (length (delete-dups (copy-sequence colors)))))))

(provide 'financial-chart-multi-test)
;;; financial-chart-multi-test.el ends here
