;;; financial-chart-svg-unify-test.el --- Shared SVG style tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'financial-chart)

(defun financial-chart-svg-unify-test--has-face-p (string face)
  "Whether STRING has at least one character with FACE."
  (cl-loop for index below (length string)
           thereis (eq (get-text-property index 'face string) face)))

(defun financial-chart-svg-unify-test--xml-p (svg)
  "Whether SVG parses as an XML document when libxml is available."
  (or (not (fboundp 'libxml-parse-xml-region))
      (with-temp-buffer
        (insert svg)
        (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max)))))))

(ert-deftest financial-chart-svg-unify-line-and-sparkline-use-unfilled-polyline ()
  (dolist (kind '(line sparkline))
    (let ((svg (financial-chart-plot kind '(10 12 9 14)
                                     :backend 'svg
                                     :pixel-width 320 :pixel-height 160)))
      (should (string-match-p "width=\"320\" height=\"160\"" svg))
      (should (string-match-p "<polyline[^>]*fill=\"none\"" svg))
      (should-not (string-match-p "<polygon" svg))
      (should (string-match-p "stroke-opacity=\"0.55\"" svg))
      (should (string-match-p ">1</text>" svg))
      (should (string-match-p ">4</text>" svg))
      (should (string-match-p "<title>Point 1: 10</title>" svg))
      (should (string-match-p "<title>Point 4: 14</title>" svg))
      (should (financial-chart-svg-unify-test--xml-p svg)))))

(ert-deftest financial-chart-svg-unify-safe-palette-covers-text-and-svg ()
  (let ((up-text (financial-chart-plot 'line '(1 2 3) :backend 'text
                                       :palette 'colorblind-safe :width 3 :height 2))
        (down-text (financial-chart-plot 'line '(3 2 1) :backend 'text
                                         :palette 'colorblind-safe :width 3 :height 2))
        (up-svg (financial-chart-plot 'line '(1 2 3) :backend 'svg
                                      :palette 'colorblind-safe))
        (down-svg (financial-chart-plot 'line '(3 2 1) :backend 'svg
                                        :palette 'colorblind-safe)))
    (should (financial-chart-svg-unify-test--has-face-p
             up-text 'financial-chart-colorblind-up))
    (should (financial-chart-svg-unify-test--has-face-p
             down-text 'financial-chart-colorblind-down))
    (should (string-match-p "stroke=\"#0072B2\"" up-svg))
    (should (string-match-p "stroke=\"#D55E00\"" down-svg)))
  (let ((financial-chart-color-palette 'colorblind-safe))
    (should (equal (financial-chart-svg--color 'up) "#0072B2"))
    (should (equal (financial-chart-svg--color 'down) "#D55E00"))))

(ert-deftest financial-chart-svg-unify-all-kinds-use-shared-canvas-and-fonts ()
  (let ((financial-chart-svg-font-family "svg-unify-test-font"))
    (dolist (kind (mapcar #'car financial-chart-kinds))
      (let* ((shape (plist-get (financial-chart--kind kind) :shape))
             (example (plist-get (alist-get shape financial-chart-shapes) :example))
             (svg (financial-chart-plot kind example :backend 'svg :title "style check")))
        (should (string-prefix-p "<svg " svg))
        (should (string-match-p "svg-unify-test-font" svg))
        (should (financial-chart-svg-unify-test--xml-p svg))
        (with-temp-buffer
          (insert svg)
          (should (> (how-many "<title>" (point-min) (point-max)) 1))
          (when (eq kind 'multi)
            (should (search-backward "AAPL" nil t))
            (should (search-forward "SPY" nil t))))))))

(ert-deftest financial-chart-svg-unify-multiple-series-have-legend-and-point-titles ()
  (let ((svg (financial-chart-plot
              'payoff-curves
              '(("T+0" . ((90 2) (100 -1) (110 2)))
                ("T+15" . ((90 1) (100 0) (110 3))))
              :backend 'svg)))
    (should (string-match-p (regexp-quote ">T+0</text>") svg))
    (should (string-match-p (regexp-quote ">T+15</text>") svg))
    (should (string-match-p (regexp-quote "<title>T+0: price 90, P/L +$2</title>") svg))
    (should (string-match-p (regexp-quote "<title>T+15: price 110, P/L +$3</title>") svg))))

(ert-deftest financial-chart-svg-unify-indicator-colors-are-configurable ()
  (let ((financial-chart-svg-series-colors '("#123456" "#abcdef")))
    (should (string-match-p
             "stroke=\"#123456\""
             (financial-chart-plot 'line '(1 2 3) :backend 'svg)))
    (should (string-match-p
             "stroke=\"#abcdef\""
             (financial-chart-plot
              'multi '(("A" . (1 2 3)) ("B" . (2 3 4))) :backend 'svg))))
  (let* ((financial-chart-indicators
          (list (list :fn (lambda (bars) (mapcar (lambda (bar)
                                                    (plist-get bar :close))
                                                  bars))
                      :label "close" :color "#123456")))
         (svg (financial-chart-render-svg
               '((:open 1 :high 3 :low 0 :close 2)
                 (:open 2 :high 4 :low 1 :close 3)))))
    (should (string-match-p "stroke=\"#123456\"" svg))))

(ert-deftest financial-chart-svg-unify-bollinger-close-regions-use-configurable-fills ()
  (let* ((financial-chart-indicator-bands
          (list (financial-chart-bollinger-band-spec
                 2 1 "#aa1122" "#2233bb" 0.25)))
         (financial-chart-show-volume nil)
         (financial-chart-show-x-axis nil)
         (svg (financial-chart-render-svg
               '((:open 10 :high 11 :low 9 :close 10)
                 (:open 12 :high 13 :low 11 :close 12)
                 (:open 14 :high 15 :low 13 :close 14)
                 (:open 16 :high 17 :low 15 :close 16)))))
    (should (string-match-p "fill-opacity=\"0.25\" fill=\"#aa1122\"" svg))
    (should (string-match-p "fill-opacity=\"0.25\" fill=\"#2233bb\"" svg))
    (should (= 4 (1- (length (split-string svg "<polygon" t)))))))

(provide 'financial-chart-svg-unify-test)

;;; financial-chart-svg-unify-test.el ends here
