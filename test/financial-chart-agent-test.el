;;; financial-chart-agent-test.el --- discover / validate / explain / describe -*- lexical-binding: t; -*-

;; The agent surface: every claim it makes about the package is checked
;; here against what the renderers actually do.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'financial-chart)

(ert-deftest financial-chart-agent-test-list-kinds-matches-registry ()
  (let ((kinds (mapcar #'car (financial-chart-list-kinds))))
    (should (equal kinds '(volume-profile heatmap area line sparkline payoff bars ohlc)))
    (dolist (k kinds)
      (should (plist-get (cdr (assq k (financial-chart-list-kinds))) :doc)))))

(ert-deftest financial-chart-agent-test-describe-kind-example-is-valid ()
  (dolist (k (mapcar #'car financial-chart-kinds))
    (let ((d (financial-chart-describe-kind k)))
      (should (plist-get d :renderers-defined))
      (should (eq t (financial-chart-validate k (plist-get d :example)))))))

(ert-deftest financial-chart-agent-test-unknown-kind-names-the-fix ()
  (let ((err (should-error (financial-chart-plot 'pie '(1 2))
                           :type 'financial-chart-unknown-kind)))
    (should (string-match-p "area, line" (cadr err)))
    (should (equal (plist-get (cddr err) :code) "unknown_kind"))))

(ert-deftest financial-chart-agent-test-validate-locates-bad-element ()
  (let ((err (should-error (financial-chart-validate 'area '(1 2 "x" 4))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :index) 2))
    (should (equal (plist-get (cddr err) :code) "invalid_data")))
  (let ((err (should-error (financial-chart-validate 'payoff '((90 1) (110 2) (100 3)))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :index) 2))
    (should (string-match-p "ascend" (cadr err))))
  (should-error (financial-chart-validate 'bars '(("A" . 1) ("B" . "x")))
                :type 'financial-chart-invalid-data)
  (should (eq t (financial-chart-validate 'area nil))))

(ert-deftest financial-chart-agent-test-validate-ohlc-without-market-data ()
  (cl-letf (((symbol-function 'market-data-validate-bars) nil))
    (fmakunbound 'market-data-validate-bars)
    (let ((err (should-error (financial-chart-validate 'ohlc '((:open 1 :high 2 :low 0 :close 1)
                                                               (:open 1 :high 2 :low 0)))
                             :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :index) 1)))))

(ert-deftest financial-chart-agent-test-plot-rejects-bad-data-before-rendering ()
  (should-error (financial-chart-plot 'area '(1 nil-ish "bad") :backend 'text)
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-agent-test-explain-is-the-plan-plot-follows ()
  (let ((plan (financial-chart-explain 'area '(1 5 3) :backend 'text :width 10)))
    (should (eq (plist-get plan :valid) t))
    (should (eq (plist-get plan :backend) 'text))
    (should (eq (plist-get plan :renderer) 'financial-chart-text-area))
    (should (equal (plist-get plan :args) '(:width 10)))
    (should (equal (plist-get plan :points) 3))
    (should (equal (plist-get plan :min) 1))
    (should (equal (plist-get plan :max) 5))
    ;; the plan's renderer + args reproduce plot's output exactly
    (should (equal (apply (plist-get plan :renderer) '(1 5 3) (plist-get plan :args))
                   (financial-chart-plot 'area '(1 5 3) :backend 'text :width 10))))
  (let ((plan (financial-chart-explain 'bars '(("A" . "x")) :backend 'svg)))
    (should (stringp (plist-get plan :valid)))
    (should (eq (plist-get plan :renderer) 'financial-chart-svg-bars))
    (should (equal (plist-get (plist-get plan :args) :width) 600))))

(ert-deftest financial-chart-agent-test-svg-carries-provenance ()
  (let ((svg (financial-chart-plot 'area '((1 40) (2 50)) :backend 'svg :title "AAPL & co")))
    (should (string-match-p "<title>AAPL &amp; co</title>" svg))
    (should (string-match-p "<desc>financial-chart area: 2 points, range 40 to 50</desc>" svg))
    (when (fboundp 'libxml-parse-xml-region)
      (with-temp-buffer
        (insert svg)
        (should (eq 'svg (car (libxml-parse-xml-region (point-min) (point-max)))))))))

(ert-deftest financial-chart-agent-test-plot-spec ()
  (should (equal (financial-chart-plot-spec '(:kind sparkline :data (1 2 3)))
                 (financial-chart-sparkline '(1 2 3)))))

(ert-deftest financial-chart-agent-test-register-kind ()
  (let ((financial-chart-kinds (copy-tree financial-chart-kinds)))
    (financial-chart-register-kind 'echo :shape 'series
                                   :text (lambda (d &rest _) (format "%S" d))
                                   :svg (lambda (_d &rest _) "<svg></svg>")
                                   :doc "test")
    (should (equal (financial-chart-plot 'echo '(1 2) :backend 'text) "(1 2)"))
    (should-error (financial-chart-register-kind 'bad :shape 'nope) :type 'financial-chart-error)))

(ert-deftest financial-chart-agent-test-describe-round-trips-json ()
  (let* ((d (financial-chart-describe))
         (back (json-parse-string (json-encode d) :object-type 'plist)))
    (should (equal (plist-get back :package) "financial-chart"))
    (should (vectorp (plist-get back :kinds)))
    (should (= (length (plist-get back :kinds)) (length financial-chart-kinds)))
    ;; every advertised entry point exists
    (dolist (g financial-chart-entry-points)
      (dolist (f (cdr g))
        (should (or (fboundp f) (boundp f)))))))

(ert-deftest financial-chart-agent-test-doctor-buffer ()
  (let ((rows (financial-chart-doctor)))
    (should rows)
    (with-current-buffer "*financial-chart doctor*"
      (should (string-match-p "kind area renders" (buffer-string))))))

(provide 'financial-chart-agent-test)
;;; financial-chart-agent-test.el ends here
