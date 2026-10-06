;;; financial-chart-depth-test.el --- ERT tests for depth charts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'financial-chart)

(defconst financial-chart-depth-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst financial-chart-depth-test--fixtures
  (expand-file-name "../../test/fixtures" financial-chart-depth-test--dir))
(defconst financial-chart-depth-test--book
  '(:bids ((99.5 4.0) (100.0 2.0) (99.0 6.0))
    :asks ((101.0 3.0) (100.5 1.0) (101.5 5.0)))
  "A deliberately unsorted book to check best-price ordering.")

(defun financial-chart-depth-test--golden (name actual)
  "Compare ACTUAL, without text properties, with fixture NAME."
  (let ((file (expand-file-name name financial-chart-depth-test--fixtures))
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

(defmacro financial-chart-depth-test--svg-env (&rest body)
  "Run BODY with a deterministic SVG palette and font."
  `(let ((financial-chart-svg-palette financial-chart-svg--fallback-palette)
         (financial-chart-svg-font-family "monospace")
         (financial-chart-svg-font-size 11))
     ,@body))

(ert-deftest financial-chart-depth-test-kind-is-registered ()
  (let ((description (financial-chart-describe-kind 'depth)))
    (should (eq (plist-get description :shape) 'order-book))
    (should (plist-get description :renderers-defined))
    (should (financial-chart-validate 'depth (plist-get description :example))))
  (should (member "depth"
                  (mapcar (lambda (entry) (plist-get entry :kind))
                          (append (plist-get (financial-chart-describe) :kinds) nil))))
  (should (plist-get (financial-chart--data-summary 'order-book
                                                     financial-chart-depth-test--book)
                     :points))
  (should (= 6 (plist-get (financial-chart--data-summary
                           'order-book financial-chart-depth-test--book)
                          :points))))

(ert-deftest financial-chart-depth-test-validates-levels-and-locates-errors ()
  (should (financial-chart-validate 'depth financial-chart-depth-test--book))
  (let ((err (should-error
              (financial-chart-validate
               'depth '(:bids ((100 1)) :asks ((101 1) (102 "large"))))
              :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "invalid_data"))
    (should (= (plist-get (cddr err) :index) 1)))
  (let ((err (should-error
              (financial-chart-validate
               'depth '(:bids ((100 1)) :asks ((101))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 0)))
  (should-error (financial-chart-validate 'depth '(:bids nil))
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-depth-test-rejects-crossed-book-with-index ()
  (let ((err (should-error
              (financial-chart-validate
               'depth '(:bids ((99 1) (101 1)) :asks ((100 1))))
              :type 'financial-chart-invalid-data)))
    (should (= (plist-get (cddr err) :index) 1))
    (should (string-match-p "best bid 101 exceeds best ask 100"
                            (cadr err))))
  (should (financial-chart-validate
           'depth '(:bids ((100 1)) :asks ((100 2))))))

(ert-deftest financial-chart-depth-test-text-ladder-golden ()
  (financial-chart-depth-test--golden
   "depth-ladder.txt"
   (financial-chart-plot 'depth financial-chart-depth-test--book
                         :backend 'text :width 50 :height 4 :unit "$")))

(ert-deftest financial-chart-depth-test-text-cumulative-golden ()
  (financial-chart-depth-test--golden
   "depth-cumulative.txt"
   (financial-chart-plot 'depth financial-chart-depth-test--book
                         :backend 'text :style 'cumulative
                         :width 50 :height 4 :unit "$")))

(ert-deftest financial-chart-depth-test-text-faces-follow-book-side ()
  (let* ((chart (financial-chart-plot 'depth financial-chart-depth-test--book
                                      :backend 'text :height 4
                                      :up-face 'my-bid :down-face 'my-ask))
         (ask-glyph (string-match "█" chart))
         (bid-label (string-match "BIDS" chart))
         (bid-glyph (string-match "█" chart bid-label)))
    (should (eq (get-text-property ask-glyph 'face chart) 'my-ask))
    (should (eq (get-text-property bid-glyph 'face chart) 'my-bid)))
  (let ((chart (financial-chart-text-depth financial-chart-depth-test--book
                                           :style 'cumulative
                                           :up-face 'my-bid :down-face 'my-ask)))
    (should (eq (get-text-property (string-match "█" chart) 'face chart) 'my-ask))
    (should (eq (get-text-property
                 (string-match "█" chart (string-match "BID CUMULATIVE" chart))
                 'face chart)
                'my-bid))))

(ert-deftest financial-chart-depth-test-text-spread-and-sort-order ()
  (let* ((chart (substring-no-properties
                 (financial-chart-text-depth financial-chart-depth-test--book
                                             :height 4 :unit "$")))
         (worse-ask (string-match "101" chart))
         (best-ask (string-match "100.5" chart))
         (spread (string-match "spread \\$0.5 (0.50%)" chart))
         (best-bid (string-match "100" chart (string-match "BIDS" chart))))
    (should (< worse-ask best-ask))
    (should (< best-ask spread))
    (should (< spread best-bid))))

(ert-deftest financial-chart-depth-test-one-sided-and-empty-books ()
  (let ((one-sided '(:bids ((100 2)) :asks nil))
        (empty '(:bids nil :asks nil)))
    (let ((text (financial-chart-text-depth one-sided))
          (svg (financial-chart-svg-depth one-sided)))
      (should (string-match-p "best bid 100 | mid n/a | spread n/a" text))
      (should (string-match-p "best bid \$100 | mid n/a | spread n/a" svg)))
    (should-not (financial-chart-text-depth empty))
    (should-not (financial-chart-svg-depth empty))))

(ert-deftest financial-chart-depth-test-unit-drops-terminal-controls ()
  (let* ((unit (concat (string 27) "]52;c;Y2xpcGJvYXJk" (string 7)))
         (price (financial-chart-depth--price 100 unit)))
    (should-not (string-match-p (regexp-quote (string 27)) price))
    (should-not (string-match-p (regexp-quote (string 7)) price))))

(ert-deftest financial-chart-depth-test-explain-reports-price-range ()
  (let ((plan (financial-chart-explain 'depth financial-chart-depth-test--book
                                       :backend 'svg)))
    (should (eq (plist-get plan :shape) 'order-book))
    (should (= (plist-get plan :points) 6))
    (should (= (plist-get plan :min) 99.0))
    (should (= (plist-get plan :max) 101.5))))

(ert-deftest financial-chart-depth-test-svg-is-well-formed-and-labeled ()
  (financial-chart-depth-test--svg-env
   (let ((svg (financial-chart-plot 'depth financial-chart-depth-test--book
                                    :backend 'svg :pixel-width 500 :pixel-height 260
                                    :title "Market depth" :unit "$")))
     (should (string-match-p "<title>Market depth</title>" svg))
     (should (string-match-p "depth: 6 points, range 99 to 101.5" svg))
     (should (string-match-p "mid \\$100.25 | spread \\$0.5 (0.50%)" svg))
     (should (string-match-p "fill=\"#2e7d32\"" svg))
     (should (string-match-p "fill=\"#c62828\"" svg))
     (when (fboundp 'libxml-parse-xml-region)
       (with-temp-buffer
         (insert svg)
         (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max))))))))))

(ert-deftest financial-chart-depth-test-requires-both-book-sides ()
  (should-error (financial-chart-validate 'depth '(:bids ((100 2))))
                :type 'financial-chart-invalid-data))

(provide 'financial-chart-depth-test)
;;; financial-chart-depth-test.el ends here
