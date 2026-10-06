;;; financial-chart-templates-test.el --- financial kinds as eas templates -*- lexical-binding: t; -*-

;;; Commentary:

;; Every financial-chart kind is drawn by an eas template that plots
;; the numbers financial-chart computes (parity), `financial-chart-plot'
;; draws with it, and the templates' text renderings are goldens
;; (EAS_UPDATE_GOLDEN=1 rewrites them).

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defun financial-chart-templates-test--example (kind)
  "The example data of KIND's shape."
  (plist-get (alist-get (plist-get (financial-chart--kind kind) :shape) financial-chart-shapes)
             :example))

(defun financial-chart-templates-test--failures (parity)
  "PARITY's failing checks."
  (seq-remove (lambda (c) (plist-get c :pass)) (plist-get parity :checks)))

(ert-deftest financial-chart-templates-cover-every-kind ()
  (dolist (kind (mapcar #'car financial-chart-kinds))
    (should (eas-template-get (plist-get (financial-chart--kind kind) :template)))
    (should (alist-get kind financial-chart-eas-parity-checks))))

(ert-deftest financial-chart-templates-are-at-parity ()
  (dolist (kind (mapcar #'car financial-chart-eas-parity-checks))
    (let ((parity (financial-chart-eas-parity kind)))
      (should (equal (list kind (financial-chart-templates-test--failures parity)) (list kind nil)))
      ;; Every kind checks its numbers, not just that it draws.
      (should (> (length (plist-get parity :checks)) 2))))
  (dolist (case '((multi :normalize 100) (histogram :bins 8) (volume-profile :bins 12)
                  (area :scale log) (line :unit "USD")))
    (let ((parity (apply #'financial-chart-eas-parity (car case) nil (cdr case))))
      (should (equal (list case (financial-chart-templates-test--failures parity)) (list case nil))))))

(ert-deftest financial-chart-templates-ohlc-parity-with-overlays-and-panes ()
  (let* ((financial-chart-indicators (list (list :fn (lambda (b) (financial-chart-sma b 5)) :label "SMA 5")
                                           (list :fn (lambda (b) (financial-chart-ema b 10)))
                                           (list :fn (lambda (b) (financial-chart-ema b 20)))))
         (financial-chart-oscillators (list (list :fn (lambda (b) (financial-chart-rsi b 14)) :label "RSI")))
         (parity (financial-chart-eas-parity 'ohlc))
         (bindings (financial-chart-eas-bindings 'ohlc (financial-chart-templates-test--example 'ohlc))))
    (should-not (financial-chart-templates-test--failures parity))
    (should (equal (mapcar (lambda (c) (plist-get c :check)) (plist-get parity :checks))
                   '("native" "renders" "candles open" "candles high" "candles low" "candles close"
                     "overlay 1" "overlay 2" "overlay 3")))
    ;; Repeated labels get their index, so each overlay keeps its own colour.
    (should (equal (mapcar (lambda (i) (plist-get i :as)) (plist-get bindings :indicators))
                   '("SMA 5" "Indicator" "Indicator 3")))
    (should (eq (plist-get bindings :volume) t))
    (let ((views (mapcar (lambda (v) (plist-get v :id))
                         (plist-get (financial-chart-eas-scene 'ohlc (plist-get bindings :bars) 'svg) :views))))
      (should (equal (length views) 3))
      (should (member "volume" views)))))

(ert-deftest financial-chart-templates-ohlc-indicator-slot-by-name ()
  (let* ((bars (financial-chart-templates-test--example 'ohlc))
         (resolved (eas-resolve "ohlc" (list :bars bars
                                             :indicators [(:name "sma" :params [5] :as "sma5")
                                                          (:name "bollinger-bands" :output "bollinger-upper"
								 :as "upper")]
                                             :oscillators ["rsi"])))
         (row (aref (plist-get (plist-get resolved :data) :values) 30))
         (expected (financial-chart-indicator-evaluate 'sma bars 5)))
    (should (equal (plist-get row :sma5) (nth 30 (plist-get expected :values))))
    (should (numberp (plist-get row :upper)))
    (should (numberp (plist-get row :rsi)))
    (should (= (length (plist-get resolved :vconcat)) 2))
    (should-not (eas-spec-unsupported resolved))
    (eas-test-should-code "INVALID_INPUT"
			  (eas-resolve "ohlc" (list :bars bars :indicators [(:name "bollinger-bands" :output "nope")])))))

(ert-deftest financial-chart-templates-transforms ()
  (let ((rows [(:a 1) (:a 2)]))
    (should (equal (eas-transform-run [(:x-eas:transform "values" :values [5 nil] :as "v")] rows)
                   [(:a 1 :v 5) (:a 2 :v :null)]))
    (eas-test-should-code "INVALID_INPUT"
			  (eas-transform-run [(:x-eas:transform "values" :values [5] :as "v")] rows)))
  (let* ((bars (financial-chart-templates-test--example 'ohlc))
         (rows (eas-transform-run [(:x-eas:transform "volume-profile" :bins 6)]
                                  (eas-data-rows (eas-data-from "bar/v1" bars))))
         (profile (financial-chart-volume-profile bars 6)))
    (should (= (length rows) 6))
    (should (equal (mapcar (lambda (r) (plist-get r :volume)) rows) (plist-get profile :volumes)))
    (should (= (cl-count t rows :key (lambda (r) (plist-get r :poc))) 1))
    (eas-test-should-code "INVALID_INPUT"
			  (eas-transform-run [(:x-eas:transform "volume-profile" :bins 0)]
					     (eas-data-rows (eas-data-from "bar/v1" bars))))))

(ert-deftest financial-chart-templates-plain-ones-need-no-financial-transforms ()
  ;; eas.el's own templates are pure Vega-Lite over rows and render
  ;; every example without financial-chart; financial-chart's own
  ;; templates/ need its transforms.
  (let ((eas-transforms (seq-remove (lambda (e) (member (car e) '("indicator" "values" "volume-profile")))
                                    eas-transforms)))
    (dolist (name (eas-template-names))
      (let ((template (eas-template-get name)))
        (unless (string-prefix-p financial-chart-eas-templates-root (plist-get template :path))
          (should (eas-resolve name (eas-template-example name))))))))

(ert-deftest financial-chart-templates-plot-draws-with-the-template ()
  (let* ((data (financial-chart-templates-test--example 'payoff))
         (text (financial-chart-plot 'payoff data :backend 'text :width 40 :height 8))
         (plan (financial-chart-explain 'payoff data :backend 'text :width 40 :height 8)))
    (should (equal text (financial-chart-eas-render 'payoff data 'text :width 40 :height 8)))
    (should (equal text (apply (plist-get plan :renderer) 'payoff data 'text (plist-get plan :args))))
    (should (text-property-not-all 0 (length text) 'eas-datum nil text))
    (should (equal (plist-get plan :template) "payoff"))
    (should (string-match-p "<title>payoff chart</title>"
                            (financial-chart-plot 'payoff data :backend 'svg)))
    (should (equal (financial-chart-plot 'sparkline nil :backend 'text) ""))
    (should-not (financial-chart-plot 'area nil :backend 'text))))

(ert-deftest financial-chart-templates-parity-is-a-doctor-row ()
  (should (cl-every (lambda (row) (eq (plist-get row :status) 'pass))
                    (financial-chart-eas-parity-doctor-checks)))
  (let ((financial-chart-eas-parity-checks (copy-tree financial-chart-eas-parity-checks)))
    (setf (alist-get 'heatmap financial-chart-eas-parity-checks)
          (lambda (_data _props _scene) (list (list "cells" '(1) '(2)))))
    (should-not (plist-get (financial-chart-eas-parity 'heatmap) :pass))
    (should (eq 'fail (plist-get (cl-find "kind heatmap template parity"
                                          (financial-chart-eas-parity-doctor-checks)
                                          :key (lambda (r) (plist-get r :name)) :test #'equal)
                                 :status)))))

(ert-deftest financial-chart-templates-text-goldens ()
  (dolist (kind (mapcar #'car financial-chart-eas-parity-checks))
    (financial-chart-test-golden (format "template-%s.txt" kind)
                     (substring-no-properties
                      (financial-chart-eas-render kind (financial-chart-templates-test--example kind)
                                                  'text :width 64 :height 16)))))

(provide 'financial-chart-templates-test)
;;; financial-chart-templates-test.el ends here
