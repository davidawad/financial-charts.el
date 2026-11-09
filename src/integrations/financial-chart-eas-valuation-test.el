;;; financial-chart-eas-valuation-test.el --- valuation stadium on eas -*- lexical-binding: t; -*-

;;; Commentary:

;; The valuation-stadium template over valuation-rs's real
;; valuation.stadium/1 output (test/fixtures/valuation/): strict
;; validation, the football field's text goldens and SVG, the
;; diagnostic/skipped strip, and the drill-down tree, including the
;; click that opens it.  EAS_UPDATE_GOLDEN=1 rewrites the goldens.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'financial-chart-eas-valuation)
(require 'eas-agent)
(require 'eas-action)
(require 'eas-action-drill)

(defconst financial-chart-valuation-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "The package root.")

(defun financial-chart-valuation-test--fixture (name)
  "The stadium fixture NAME (stock, cds, re-multifamily)."
  (eas-json-read-file (expand-file-name (format "test/fixtures/valuation/%s.stadium.json" name)
                                        financial-chart-valuation-test--root)))

(defun financial-chart-valuation-test--text (stadium &rest bindings)
  "STADIUM's football field as plain text at 100 by 36."
  (substring-no-properties
   (eas-text-render (eas-compile (eas-resolve "valuation-stadium" (append (list :data stadium) bindings))
                                 :target 'text :size '(:cols 100 :rows 36)))))

(defun financial-chart-valuation-test--rows (stadium)
  "The lowered rows of STADIUM."
  (financial-chart-valuation-rows stadium))

(defun financial-chart-valuation-test--bands (rows)
  "ROWS' bands in order."
  (mapcar (lambda (r) (plist-get r :band)) rows))

(defun financial-chart-valuation-test--with-row (stadium index &rest changes)
  "STADIUM with row INDEX's CHANGES (plist) applied."
  (let ((copy (copy-tree stadium t)))
    (let ((row (aref (plist-get copy :rows) index)))
      (while changes (setq row (plist-put row (pop changes) (pop changes))))
      (aset (plist-get copy :rows) index row))
    copy))

(defun financial-chart-valuation-test--invalid (stadium)
  "The error data validating STADIUM signals, as (CODE INDEX FIELD)."
  (condition-case err (progn (financial-chart-valuation-validate stadium) nil)
    (financial-chart-invalid-data
     (let ((d (financial-chart-error-data err)))
       (list (plist-get d :code) (plist-get d :index) (plist-get d :field))))))

;;; Validation

(ert-deftest financial-chart-valuation-fixtures-validate ()
  (dolist (name '("stock" "cds" "re-multifamily"))
    (should-not (financial-chart-valuation-test--invalid (financial-chart-valuation-test--fixture name)))))

(ert-deftest financial-chart-valuation-accepts-the-envelope ()
  (let* ((stadium (financial-chart-valuation-test--fixture "cds"))
         (envelope (list :contract "valuation.stadium/1" :ok t :data stadium :next [])))
    (should (equal (financial-chart-valuation-rows envelope) (financial-chart-valuation-rows stadium)))))

(ert-deftest financial-chart-valuation-validation-is-strict ()
  (let ((stock (financial-chart-valuation-test--fixture "stock")))
    (should (equal (financial-chart-valuation-test--invalid (plist-put (copy-tree stock t) :contract "valuation.stadium/2"))
                   '("wrong_contract" nil "contract")))
    (should (equal (financial-chart-valuation-test--invalid '(:contract "valuation.stadium/1" :rows []))
                   '("missing_field" nil "class")))
    (should (equal (financial-chart-valuation-test--invalid "nope") '("not_a_plist" nil nil)))
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 0 :status "maybe"))
                   '("invalid_status" 0 "status")))
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 2 :approach "vibes"))
                   '("invalid_approach" 2 "approach")))
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 1 :role "side"))
                   '("invalid_role" 1 "role")))
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 1 :value "cheap"))
                   '("not_a_number" 1 "value")))
    ;; low <= value <= high.
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 0 :low 300.0))
                   '("range_order" 0 "low")))
    ;; One price axis, one unit.
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 3 :unit "usd"))
                   '("mixed_units" 3 "unit")))
    (should (equal (financial-chart-valuation-test--invalid
                    (financial-chart-valuation-test--with-row stock 0 :detail '(:lines [])))
                   '("invalid_detail" 0 "detail.value")))))

(ert-deftest financial-chart-valuation-adapter-signals-eas-errors ()
  (let ((bad (plist-put (copy-tree (financial-chart-valuation-test--fixture "cds") t) :contract "x")))
    (let ((err (eas-test-should-code "SHAPE_INVALID" (eas-resolve "valuation-stadium" (list :data bad)))))
      (should (equal (plist-get err :field) "contract"))
      (should (equal (plist-get err :shape) "valuation-stadium")))
    (let ((env (eas-agent "check" "valuation-stadium" :data (list :data bad))))
      (should (eq (plist-get env :ok) :false))
      (should (equal (plist-get env :reason) "SHAPE_INVALID")))))

;;; Rows

(ert-deftest financial-chart-valuation-bands ()
  (let* ((rows (financial-chart-valuation-test--rows (financial-chart-valuation-test--fixture "stock")))
         (bands (financial-chart-valuation-test--bands rows)))
    (should (equal (car bands) "header"))
    (should (equal (car (last bands)) "caption"))
    ;; Reference, weighted mean and median are markers, never bars.
    (should (= (cl-count "marker" bands :test #'equal) 3))
    (should (= (cl-count "value" bands :test #'equal) 8))
    (should (= (cl-count "skipped" bands :test #'equal) 1))
    ;; The implied-growth diagnostic is in the strip, not on the price axis.
    (should (= (cl-count "diagnostic" bands :test #'equal) 1))
    (let ((diag (cl-find "diagnostic" rows :key (lambda (r) (plist-get r :band)) :test #'equal)))
      (should (equal (plist-get diag :method) "eq.reverse_dcf"))
      (should-not (plist-get diag :low))
      (should (equal (plist-get diag :note) "2.04% rate; weights 1.0"))
      (should (equal (plist-get diag :unit_label) "rate"))
      (should (equal (plist-get diag :weights) 1.0)))
    (let ((comps (cl-find "eq.comps_ev_ebitda" rows :key (lambda (r) (plist-get r :method)) :test #'equal)))
      (should (equal (plist-get comps :unit_label) "USD per share"))
      (should (equal (plist-get comps :weights) '(:model 0.7 :confidence 0.3)))
      (should (equal (plist-get comps :weights_label) "weights model 0.7, confidence 0.3"))
      (should (string-match-p "USD per share" (plist-get comps :value_label))))
    (let ((skipped (cl-find "skipped" rows :key (lambda (r) (plist-get r :band)) :test #'equal)))
      (should (equal (plist-get skipped :note) "needs current_assets, total_liabilities; weights 1.0")))
    ;; Sorted by value, highest first; a method with no range is a point.
    (let ((values (seq-filter (lambda (r) (equal (plist-get r :band) "value")) rows)))
      (should (equal (mapcar (lambda (r) (plist-get r :rank_value)) values) '(0 3 2 1 7 6 4 5)))
      (let ((graham (cl-find "eq.graham_number" values :key (lambda (r) (plist-get r :method)) :test #'equal)))
        (should (= (plist-get graham :low) (plist-get graham :high)))
        (should (equal (plist-get graham :range_label) "point"))))))

(ert-deftest financial-chart-valuation-error-and-extra-skips-are-listed ()
  (let* ((stock (financial-chart-valuation-test--fixture "stock"))
         (stock (financial-chart-valuation-test--with-row
                 stock 0 :status "error" :reason "peer set empty" :value :null :low :null :high :null))
         (stock (financial-chart-valuation-test--with-row
                 stock 1 :status "skipped" :value :null :low :null :high :null
                 :missing ["eps (usd_per_share): trailing EPS"]))
         (rows (financial-chart-valuation-test--rows stock))
         (notes (mapcar (lambda (r) (cons (plist-get r :band) (plist-get r :note))) rows)))
    (should (member '("error" . "error: peer set empty; weights model 0.7, confidence 0.3") notes))
    (should (member '("skipped" . "needs eps; weights 1.0") notes))
    (should (member '("skipped" . "needs current_assets, total_liabilities; weights 1.0") notes))
    (let ((text (financial-chart-valuation-test--text stock)))
      (should (string-match-p "error: peer set empty" text))
      (should (string-match-p "needs eps" text)))))

(ert-deftest financial-chart-valuation-unit-and-weight-fields-fall-back-to-legacy ()
  (let* ((stock (financial-chart-valuation-test--fixture "stock"))
         (stock (financial-chart-valuation-test--with-row stock 0 :unit_label :null :weights :null))
         (row (cl-find "eq.comps_ev_ebitda" (financial-chart-valuation-test--rows stock)
                       :key (lambda (r) (plist-get r :method)) :test #'equal)))
    (should (equal (plist-get row :unit_label) "usd per share"))
    (should (equal (plist-get row :weights) 1.0))
    (should (equal (plist-get row :weights_label) "weights 1.0"))))

;;; Goldens and SVG

(ert-deftest financial-chart-valuation-text-goldens ()
  (financial-chart-test-golden "template-valuation-stadium.txt"
                               (financial-chart-valuation-test--text (financial-chart-valuation-test--fixture "stock")))
  (financial-chart-test-golden "template-valuation-stadium-grouped.txt"
                               (financial-chart-valuation-test--text (financial-chart-valuation-test--fixture "stock")
                                                                     :group t))
  (financial-chart-test-golden "template-valuation-stadium-cds.txt"
                               (financial-chart-valuation-test--text (financial-chart-valuation-test--fixture "cds")))
  (financial-chart-test-golden "template-valuation-stadium-re.txt"
                               (financial-chart-valuation-test--text
                                (financial-chart-valuation-test--fixture "re-multifamily"))))

(ert-deftest financial-chart-valuation-text-is-deterministic-and-complete ()
  (let* ((stock (financial-chart-valuation-test--fixture "stock"))
         (text (financial-chart-valuation-test--text stock)))
    (should (equal text (financial-chart-valuation-test--text stock)))
    (should (string-match-p "ACME Corp common (illustrative inputs)  (long_equity, default)" text))
    (should (string-match-p "8 of 10 methods ok; dispersion 57.5%" text))
    (should (string-match-p "market 140.00" text))
    (should (string-match-p "needs current_assets, total_liabilities" text))
    (should (string-match-p "2.04% rate" text))))

(ert-deftest financial-chart-valuation-groups-by-approach ()
  (let* ((stock (financial-chart-valuation-test--fixture "stock"))
         (labels '("Comparables: EV" "Comparables: trailing" "Graham" "Residual income"
                   "DCF: free cash flow to t" "Dividend discount model:"))
         (order (lambda (text)
                  (mapcar #'car (sort (mapcar (lambda (l) (cons l (string-match (regexp-quote l) text))) labels)
                                      (lambda (a b) (< (cdr a) (cdr b))))))))
    ;; Sorted by value, the market comps head the field; grouped, the income
    ;; methods (DCF, residual income, DDM) come first, then market, then heuristic.
    (should (equal (funcall order (financial-chart-valuation-test--text stock))
                   '("Comparables: EV" "DCF: free cash flow to t" "Comparables: trailing"
                     "Graham" "Residual income" "Dividend discount model:")))
    (should (equal (funcall order (financial-chart-valuation-test--text stock :group t))
                   '("DCF: free cash flow to t" "Residual income" "Dividend discount model:"
                     "Comparables: EV" "Comparables: trailing" "Graham")))))

(ert-deftest financial-chart-valuation-svg ()
  (dolist (name '("stock" "cds" "re-multifamily"))
    (let ((svg (eas-svg-render (eas-compile (eas-resolve "valuation-stadium"
                                                         (list :data (financial-chart-valuation-test--fixture name)))
                                            :target 'svg :size '(900 . 520)))))
      (should (string-prefix-p "<svg" svg))
      (should (string-match-p "income\\|arbitrage" svg))
      (should (string-match-p "dispersion" svg)))))

(ert-deftest financial-chart-valuation-example-and-check ()
  (let ((env (eas-agent "check" "valuation-stadium" :data (eas-template-example "valuation-stadium"))))
    (should (eq (plist-get env :ok) t))
    (should (eq (plist-get (plist-get env :data) :native) t))))

;;; Drill-down

(ert-deftest financial-chart-valuation-trace-is-traceable ()
  (let* ((stock (financial-chart-valuation-test--fixture "stock"))
         (tree (financial-chart-valuation-trace stock "eq.dcf_fcff")))
    (should (string-prefix-p "* DCF: free cash flow to the firm at WACC [eq.dcf_fcff] = 163.364" tree))
    ;; Assumptions carry their provenance.
    (should (string-match-p "^\\*\\*\\* risk_free_rate = 0.0484 (fred DGS10 as of 2026-11-05)$" tree))
    (should (string-match-p "^ *url: https://fred.stlouisfed.org/series/DGS10$" tree))
    (should (string-match-p "tax_rate = 0.21 (profile standard.tax_rate as of 2018-01-01)" tree))
    ;; A line shows its formula and the values it is from.
    (should (string-match-p "equity value / shares outstanding" tree))
    (should (string-match-p "from equity_value = 16,336,402,080, shares_outstanding = 100,000,000" tree))
    ;; Components nest: value -> wacc -> cost_of_equity -> lines.
    (should (string-match-p "^\\*\\*\\* wacc \\[coc.wacc\\] = 0.0919924$" tree))
    (should (string-match-p "^\\*\\*\\*\\*\\* cost_of_equity \\[coc.capm\\] = 0.0979$" tree))
    ;; A component line resolves a parent's assumption.
    (should (string-match-p "from risk_free_rate = 0.0484, equity_risk_premium = 0.045" tree))
    (financial-chart-test-golden "valuation-trace-dcf-fcff.txt" tree)))

(ert-deftest financial-chart-valuation-trace-errors ()
  (let ((stock (financial-chart-valuation-test--fixture "stock")))
    (should (equal (condition-case err (financial-chart-valuation-trace stock "eq.nope")
                     (financial-chart-invalid-data (plist-get (financial-chart-error-data err) :code)))
                   "unknown_method"))
    (should (equal (condition-case err (financial-chart-valuation-trace stock "eq.ncav")
                     (financial-chart-invalid-data (plist-get (financial-chart-error-data err) :code)))
                   "no_detail"))))

(ert-deftest financial-chart-valuation-drill-buffer ()
  (let ((buffer (financial-chart-valuation-drill (financial-chart-valuation-test--fixture "cds")
                                                 "cds.credit_triangle")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (buffer-name) "*valuation: cds.credit_triangle*"))
          (should (derived-mode-p 'outline-mode))
          (should buffer-read-only)
          (should (string-prefix-p "* Credit triangle" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest financial-chart-valuation-click-opens-the-drill-down ()
  (let ((view (eas-view-open "valuation-stadium"
                             :bindings (list :data (financial-chart-valuation-test--fixture "stock"))
                             :id "valuation-test")))
    (unwind-protect
        (progn
          ;; A click on a method's bar (the range, or its value tick) runs the action.
          (let* ((click (eas-action-run view (list :view "field" :mark "range" :datum 0
                                                   :row (list :method "eq.comps_pe"))))
                 (buffer (get-buffer "*valuation: eq.comps_pe*")))
            (should (equal (plist-get click :action) "valuation-drill"))
            (should (equal (plist-get click :result) "trace *valuation: eq.comps_pe*"))
            (should (string-match-p "median of peer P/E" (with-current-buffer buffer (buffer-string))))
            (kill-buffer buffer))
          ;; A marker is not a method: nothing is bound to it.
          (should-not (equal (plist-get (eas-action-run view (list :view "field" :mark "markers" :datum 0
                                                                   :row (list :band "marker")))
                                        :action)
                             "valuation-drill")))
      (eas-view-close view))))

(provide 'financial-chart-eas-valuation-test)
;;; financial-chart-eas-valuation-test.el ends here
