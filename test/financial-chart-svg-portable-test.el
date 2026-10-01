;;; financial-chart-svg-portable-test.el --- SVG output is build-independent -*- lexical-binding: t; -*-

;; Some Emacs builds' `svg-print' put whitespace between elements and
;; around text; others don't.  Serialized SVG must be identical either way.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'financial-chart)

(defun financial-chart-svg-portable-test--spaced-print (orig dom)
  "Call ORIG `svg-print' on DOM, then add whitespace like some builds do."
  (let ((start (point)))
    (funcall orig dom)
    (save-excursion
      (goto-char start)
      (while (search-forward "><" nil t)
        (replace-match ">\n  <" t t)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward ">\\([^<\n]\\)" nil t)
        (replace-match "> \\1" t)))))

(ert-deftest financial-chart-svg-portable-test-whitespace-printing-build ()
  (let ((svg (svg-create 10 10)))
    (svg-text svg "70" :x 1)
    (let* ((plain (financial-chart-svg--string svg))
           (orig (symbol-function 'svg-print))
           (spaced (cl-letf (((symbol-function 'svg-print)
                              (lambda (dom)
                                (financial-chart-svg-portable-test--spaced-print orig dom))))
                     (financial-chart-svg--string svg))))
      (should (string-match-p ">70</text>" plain))
      (should (equal spaced plain)))))

(ert-deftest financial-chart-svg-portable-test-candles-use-the-serializer ()
  (let* ((calls 0)
         (orig (symbol-function 'financial-chart-svg--string)))
    (cl-letf (((symbol-function 'financial-chart-svg--string)
               (lambda (svg) (cl-incf calls) (funcall orig svg))))
      (financial-chart-render-svg '((:open 1 :high 2 :low 0.5 :close 1.5)
                                    (:open 1.5 :high 3 :low 1 :close 2.5)))
      (should (>= calls 1)))))

(provide 'financial-chart-svg-portable-test)
;;; financial-chart-svg-portable-test.el ends here
