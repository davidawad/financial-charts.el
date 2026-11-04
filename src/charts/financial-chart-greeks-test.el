;;; financial-chart-greeks-test.el --- Tests for Greeks-by-strike -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart)

(defconst financial-chart-greeks-test--rows
  '((:expiry "2026-12-18" :strike 95 :delta 1250 :gamma -320 :vega 85 :theta -42)
    (:expiry "2026-12-18" :strike 100 :delta 2100 :gamma 540 :vega 160 :theta -88)
    (:expiry "2027-01-15" :strike 100 :delta 740 :gamma 190 :vega 230 :theta -31)))

(ert-deftest financial-chart-greeks-registers-and-renders-by-expiry ()
  (should (eq (plist-get (financial-chart--kind 'greeks-by-strike) :shape) 'greeks-by-strike))
  (should (equal (financial-chart-validate 'greeks-by-strike financial-chart-greeks-test--rows) t))
  (let ((text (financial-chart-plot 'greeks-by-strike financial-chart-greeks-test--rows
                                    :backend 'text :width 70 :height 20))
        (svg (financial-chart-plot 'greeks-by-strike financial-chart-greeks-test--rows
                                   :backend 'svg)))
    (should (stringp text))
    (should (string-prefix-p "<svg" svg)))
  (should (eas-template-get "greeks-by-strike")))

(ert-deftest financial-chart-greeks-template-parity ()
  (let ((parity (financial-chart-eas-parity 'greeks-by-strike)))
    (should (plist-get parity :pass))
    (should (= (length (plist-get parity :checks)) 3))))

(ert-deftest financial-chart-greeks-checks-bad-row-and-field ()
  (let ((result (financial-chart-check
                 'greeks-by-strike
                 '((:expiry "2026-12-18" :strike 95 :delta 1 :gamma 2 :vega 3 :theta 4)
                   (:expiry "2027-01-15" :strike 0 :delta 1 :gamma 2 :vega 3 :theta 4)))))
    (should (equal (plist-get result :code) "not_positive"))
    (should (= (plist-get result :index) 1))
    (should (equal (plist-get result :field) "strike")))
  (should (equal (financial-chart-check 'greeks-by-strike
                                        '((:expiry "2026-12-18" :strike 95 :delta 1 :gamma 2 :vega 3)))
                 '(:code "missing_field" :index 0 :field "theta"
                   :message "element 0 (theta): required field :theta is missing"))))

(ert-deftest financial-chart-greeks-json-round-trip ()
  (let* ((shape (alist-get 'greeks-by-strike financial-chart-shapes))
         (data (plist-get shape :example)))
    (should (equal data (funcall (plist-get shape :from-json)
                                 (funcall (plist-get shape :to-json) data))))))

(provide 'financial-chart-greeks-test)
;;; financial-chart-greeks-test.el ends here
