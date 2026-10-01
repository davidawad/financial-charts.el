;;; financial-chart-visual-qa-test.el --- Visual QA regressions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defconst financial-chart-visual-qa-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-visual-qa-test--dir))
(require 'financial-chart)

(defconst financial-chart-visual-qa-test--fixtures
  (expand-file-name "fixtures" financial-chart-visual-qa-test--dir))

(defun financial-chart-visual-qa-test--golden (name actual)
  "Compare ACTUAL, without text properties, to fixture NAME."
  (let ((file (expand-file-name name financial-chart-visual-qa-test--fixtures))
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

(defun financial-chart-visual-qa-test--attribute (attributes name)
  "Return SVG ATTRIBUTE named NAME from ATTRIBUTES."
  (when (string-match (concat "\\b" (regexp-quote name)
                              "=\"\\([^\"]+\\)\"")
                      attributes)
    (match-string 1 attributes)))

(defun financial-chart-visual-qa-test--text-nodes (svg)
  "Return SVG text nodes as (TEXT X Y ANCHOR FONT-SIZE) lists."
  (let ((position 0)
        nodes)
    (while (string-match "<text\\([^>]*\\)>\\([^<]*\\)</text>" svg position)
      (let* ((attributes (match-string 1 svg))
             (text (match-string 2 svg))
             (end (match-end 0))
             (x (financial-chart-visual-qa-test--attribute attributes "x"))
             (y (financial-chart-visual-qa-test--attribute attributes "y"))
             (anchor (financial-chart-visual-qa-test--attribute attributes "text-anchor"))
             (font-size (financial-chart-visual-qa-test--attribute attributes "font-size")))
        (when (and x y anchor font-size)
          (push (list text (string-to-number x) (string-to-number y) anchor
                      (string-to-number font-size))
                nodes))
        (setq position end)))
    (nreverse nodes)))

(defun financial-chart-visual-qa-test--svg-width (svg)
  "Canvas width of SVG."
  (when (string-match "<svg\\([^>]*\\)>" svg)
    (string-to-number
     (financial-chart-visual-qa-test--attribute (match-string 1 svg) "width"))))

(defun financial-chart-visual-qa-test--interval (node)
  "Text interval for SVG text NODE."
  (financial-chart-svg--label-interval (nth 1 node) (car node) (nth 3 node)))

(ert-deftest financial-chart-visual-qa-examples-have-visual-density ()
  (dolist (entry financial-chart-shapes)
    (let* ((shape (car entry))
           (example (plist-get (cdr entry) :example)))
      (pcase shape
        ('series (should (<= 30 (length example) 60)))
        ('ohlc (should (<= 30 (length example) 60)))
        ('payoff (should (>= (length example) 20)))
        ('labeled (should (>= (length example) 8)))
        ('multi-series
         (should (>= (length example) 2))
         (dolist (series example)
           (should (<= 30 (length (cdr series)) 60))))
        ('payoff-curves
         (should (= (length example) 3))
         (dolist (curve example)
           (should (= (length (cdr curve)) 21))))
        ('matrix
         (should (= (length (plist-get example :labels)) 5))
         (should (= (length (plist-get example :rows)) 5)))
        ('order-book
         (should (= (length (plist-get example :bids)) 8))
         (should (= (length (plist-get example :asks)) 8))))))
  (dolist (kind (mapcar #'car financial-chart-kinds))
    (let* ((shape (plist-get (financial-chart--kind kind) :shape))
           (example (plist-get (alist-get shape financial-chart-shapes) :example)))
      (should (financial-chart-validate kind example)))))

(ert-deftest financial-chart-visual-qa-nice-axis-ticks-preserve-step-precision ()
  (should (equal (financial-chart--axis-label-values 99.49375 101.00625 5)
                 '(99.5 100.0 100.5 101.0)))
  (should (equal (financial-chart--axis-tick-label
                  100.5 99.49375 101.00625 5 "$")
                 "$100.5"))
  (let* ((book (plist-get (alist-get 'order-book financial-chart-shapes) :example))
         (svg (financial-chart-plot 'depth book :backend 'svg))
         (summary (cl-find-if
                   (lambda (node) (string-prefix-p "mid " (car node)))
                   (financial-chart-visual-qa-test--text-nodes svg))))
    (should (string-match-p (regexp-quote "$100.5") svg))
    (should (string-match-p (regexp-quote "$101.0") svg))
    (should-not (string-match-p (regexp-quote "$100.9375") svg))
    (should summary)
    (should (>= (- (nth 2 summary) (nth 4 summary)) 0))))

(ert-deftest financial-chart-visual-qa-heatmap-column-labels-fit-canvas ()
  (let* ((data (plist-get (alist-get 'matrix financial-chart-shapes) :example))
         (svg (financial-chart-plot 'heatmap data :backend 'svg))
         (labels (cl-remove-if-not
                  (lambda (node)
                    (and (member (car node) '("SPY" "QQQ" "TLT" "GLD" "USO"))
                         (< (nth 2 node) 30)))
                  (financial-chart-visual-qa-test--text-nodes svg))))
    (should (= (length labels) 5))
    (dolist (label labels)
      (should (>= (- (nth 2 label) (nth 4 label)) 0))
      (should (<= (nth 1 label) 600)))))

(ert-deftest financial-chart-visual-qa-ohcl-minimum-width-and-date-label-spacing ()
  (let* ((bars '((:open 100 :high 103 :low 99 :close 102 :volume 12000
                  :time 1700000000000)
                 (:open 102 :high 104 :low 101 :close 101.5 :volume 9500
                  :time 1700086400000)))
         (svg (financial-chart-plot 'ohlc bars :backend 'svg))
         (dates (cl-remove-if-not
                 (lambda (node)
                   (string-match-p "\\`[0-9][0-9]/[0-9][0-9]\\'" (car node)))
                 (financial-chart-visual-qa-test--text-nodes svg))))
    (should (>= (financial-chart-visual-qa-test--svg-width svg) 550))
    (should (= (length dates) 2))
    (let ((first (financial-chart-visual-qa-test--interval (car dates)))
          (last (financial-chart-visual-qa-test--interval (cadr dates))))
      (should (<= (+ (cdr first) 4) (car last))))))

(ert-deftest financial-chart-visual-qa-text-ohlc-final-date-fits-axis ()
  (let* ((bars (plist-get (alist-get 'ohlc financial-chart-shapes) :example))
         (last-time (plist-get (car (last bars)) :time))
         (last-label (format-time-string financial-chart-x-axis-format
                                         (/ last-time 1000.0)))
         (axis (financial-chart--render-x-axis bars)))
    (should (string-match-p (regexp-quote last-label) axis))))

(ert-deftest financial-chart-visual-qa-x-tick-labels-drop-duplicates-and-collisions ()
  (should (equal
           (financial-chart-svg--nonoverlapping-label-indices
            '((55 "duplicate" "start") (60 "duplicate" "middle")
              (580 "right edge" "end"))
            55 580)
           '(0 2)))
  (let* ((series (plist-get (alist-get 'series financial-chart-shapes) :example))
         (svg (financial-chart-plot 'histogram series :backend 'svg))
         (frame (financial-chart-svg--frame 600 240 nil))
         (labels-y (+ (nth 1 frame) (max 1 (- (nth 3 frame) 42)) 17))
         (labels (cl-remove-if-not
                  (lambda (node) (= (nth 2 node) labels-y))
                  (financial-chart-visual-qa-test--text-nodes svg)))
         (texts (mapcar #'car labels)))
    (should (= (length texts) (length (delete-dups (copy-sequence texts)))))
    (cl-loop for first in labels
             for last in (cdr labels)
             do (should (<= (+ (cdr (financial-chart-visual-qa-test--interval first)) 4)
                            (car (financial-chart-visual-qa-test--interval last)))))))

(ert-deftest financial-chart-visual-qa-volume-profile-svg-labels-and-solid-bins ()
  (let* ((bars (plist-get (alist-get 'ohlc financial-chart-shapes) :example))
         (svg (financial-chart-plot 'volume-profile bars :backend 'svg
                                    :title "Volume profile" :unit "$"))
         (nodes (financial-chart-visual-qa-test--text-nodes svg))
         (title (assoc "Volume profile" nodes))
         (subtitle (cl-find-if (lambda (node)
                                 (string-prefix-p "OHLCV estimate:" (car node)))
                               nodes))
         (close (cl-find-if (lambda (node) (string-prefix-p "Close $" (car node)))
                            nodes))
         (close-right (+ (nth 1 close) (* (string-width (car close))
                                          (nth 4 close) 0.62) 2)))
    (should title)
    (should subtitle)
    (should (< (nth 2 title) (nth 2 subtitle)))
    (should close)
    (should (<= close-right 600))
    (should (= (length (financial-chart-visual-qa-test--svg-bin-rows svg)) 24))
    (let ((rows (financial-chart-visual-qa-test--svg-bin-rows svg)))
      (cl-loop for first in rows
               for next in (cdr rows)
               do (should (< (abs (- (+ (nth 0 first) (nth 1 first))
                                     (nth 0 next)))
                             0.001))))))

(ert-deftest financial-chart-visual-qa-volume-profile-text-markers-golden ()
  (let* ((bars (plist-get (alist-get 'ohlc financial-chart-shapes) :example))
         (chart (financial-chart-plot 'volume-profile bars :backend 'text
                                      :unit "$" :width 24)))
    (should (string-match-p "◀ POC" chart))
    (should (string-match-p "◀ close" chart))
    (should-not (string-match-p "[PC]\\$[0-9]" chart))
    (financial-chart-visual-qa-test--golden "visual-qa-volume-profile.txt" chart)))

(defun financial-chart-visual-qa-test--svg-bin-rows (svg)
  "Extract volume-profile bin rectangles as (Y HEIGHT) pairs."
  (let ((position 0)
        rows)
    (while (string-match "<rect\\([^>]*\\)>" svg position)
      (let* ((attributes (match-string 1 svg))
             (end (match-end 0))
             (fill (financial-chart-visual-qa-test--attribute attributes "fill"))
             (y (financial-chart-visual-qa-test--attribute attributes "y"))
             (height (financial-chart-visual-qa-test--attribute attributes "height")))
        (when (and (member fill '("#2e7d32" "#7b3294")) y height
                   (< (string-to-number y) 200)
                   (< (string-to-number height) 10))
          (push (list (string-to-number y) (string-to-number height)) rows))
        (setq position end)))
    (nreverse rows)))

(provide 'financial-chart-visual-qa-test)

;;; financial-chart-visual-qa-test.el ends here
