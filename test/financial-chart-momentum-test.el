;;; financial-chart-momentum-test.el --- Momentum indicator tests -*- lexical-binding: t; -*-

(require 'ert)
(defvar financial-chart-momentum-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-momentum-test--dir))
(require 'financial-chart-momentum)

(defconst financial-chart-momentum-test--bars
  '((:close 10 :high 11 :low 9)
    (:close 12 :high 13 :low 11)
    (:close 15 :high 16 :low 14)
    (:close 18 :high 19 :low 17)
    (:close 20 :high 21 :low 19)))

(ert-deftest financial-chart-momentum-calculates-bar-aligned-differences ()
  (should (equal (financial-chart-momentum financial-chart-momentum-test--bars 2)
                 '(nil nil 5 6 5)))
  (should (equal (financial-chart-momentum
                  '((:close 0) (:close 2) (:close 4)) 1)
                 '(nil 2 2))))

(ert-deftest financial-chart-momentum-roc-handles-zero-and-missing-reference ()
  (should (equal (financial-chart-rate-of-change
                  '((:close 0) (:close 5) (:close 10) (:close 20)) 2)
                 '(nil nil nil 300.0)))
  (should (equal (financial-chart-rate-of-change
                  '((:close 10) (:close nil) (:close 12)) 1)
                 '(nil nil nil))))

(ert-deftest financial-chart-momentum-cci-uses-typical-price-and-warmup ()
  (let ((bars '((:high 12 :low 8 :close 10)
                (:high 14 :low 10 :close 12)
                (:high 16 :low 12 :close 14))))
    (should (equal (financial-chart-commodity-channel-index bars 2)
                   '(nil 66.66666666666667 66.66666666666667))))
  (should (equal (financial-chart-commodity-channel-index
                  '((:high 1 :low 1 :close 1) (:high 1 :low 1 :close 1)) 2)
                 '(nil 0.0))))

(ert-deftest financial-chart-momentum-missing-data-recovers-after-window ()
  (should (equal (financial-chart-momentum
                  '((:close 1) (:close nil) (:close 3) (:close 4)) 1)
                 '(nil nil nil 1))))

(ert-deftest financial-chart-momentum-macd-registers-three-aligned-outputs ()
  (let* ((bars (cl-loop for n from 1 to 12
                        collect (list :close (+ 10 (* n n)))))
         (result (financial-chart-indicator-evaluate 'macd bars 2 4 2)))
    (should (equal (mapcar (lambda (series) (plist-get series :name)) result)
                   '(macd macd-signal macd-histogram)))
    (should (equal (mapcar (lambda (series) (plist-get series :label)) result)
                   '("MACD" "MACD Signal" "MACD Histogram")))
    (should (cl-every (lambda (series)
                        (= (length (plist-get series :values)) (length bars)))
                      result))
    (should (cl-every #'null (seq-take (plist-get (nth 0 result) :values) 3)))
    (should (cl-every #'null (seq-take (plist-get (nth 1 result) :values) 4)))
    (should (numberp (car (last (plist-get (nth 2 result) :values)))))))

(ert-deftest financial-chart-momentum-macd-rejects-reversed-periods ()
  (should-error (financial-chart-macd '((:close 1)) 26 12 9)
                :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-momentum-indicators-register-with-provider-neutral-api ()
  (let ((names (mapcar (lambda (entry) (plist-get entry :name))
                       (financial-chart-list-indicators))))
    (dolist (name '(momentum roc cci macd))
      (should (memq name names)))))

(provide 'financial-chart-momentum-test)
;;; financial-chart-momentum-test.el ends here
