;;; financial-chart-3d-rates-test.el --- 3D rates and credit charts -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these with financial-chart-3d-test.el.  The
;; fixtures (test/fixtures/3d/rates/, the templates' examples) are
;; invest-metrics output, null inputs left out; the first tests check
;; them against the metrics' own rules, so the adapters are tested on
;; what the metrics emit.  Goldens and the gallery page are checked for
;; every variant, these included, by financial-chart-3d-test.el.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'financial-chart-3d-rates)
(require 'graphics-eas)
(financial-chart-3d--ensure)

(defun financial-chart-3d-rates-test--data (name)
  "The data of rates template NAME's example (test/fixtures/3d/rates/)."
  (append (plist-get (eas-json-read-file
                      (expand-file-name (format "test/fixtures/3d/rates/%s.data.json" name)
                                        financial-chart-test-root))
                     :data)
          nil))

(defun financial-chart-3d-rates-test--near (a b &optional eps)
  "Non-nil when A and B are within EPS (default 1e-9 relative)."
  (<= (abs (- a b)) (* (or eps 1e-9) (max 1.0 (abs a) (abs b)))))

(defun financial-chart-3d-rates-test--prim (prims id)
  "The primitive ID among PRIMS."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

;;; The fixtures are what the metrics emit

(ert-deftest financial-chart-3d-rates-fixtures-follow-the-metrics ()
  ;; fi.nss_fit: the mean rate; fi.curve_regime: -1, 0, 1 by the ends;
  ;; fi.forward_path: the last forward; fi.rolldown_carry: ytm * t bps.
  (dolist (e (financial-chart-3d-rates-test--data "yield-curve"))
    (let* ((nss (plist-get e :nss_fit))
           (rates (append (plist-get (plist-get nss :inputs) :curve_rates) nil)))
      (should (equal (plist-get nss :key) "nss_fit"))
      (should (financial-chart-3d-rates-test--near (plist-get nss :value) (/ (apply #'+ rates) (length rates))))
      (should (equal (cdr (assoc (round (plist-get (plist-get e :curve_regime) :value))
                                 financial-chart-3d-rates-regimes))
                     (financial-chart-3d-rates-regime rates)))
      (when-let* ((fwd (plist-get e :forward_path)))
        (should (= (plist-get fwd :value) (car (last (append (plist-get (plist-get fwd :inputs) :forward_rates) nil))))))
      (seq-doseq (c (plist-get e :rolldown_carry))
        (let ((in (plist-get c :inputs)))
          (should (financial-chart-3d-rates-test--near
                   (plist-get c :value) (* (plist-get in :ytm) (plist-get in :horizon_years) 10000)))))))
  ;; fi.spread_pctile: the share of the history at or below the spread.
  (dolist (mc (financial-chart-3d-rates-test--data "credit-spreads"))
    (let* ((in (plist-get mc :inputs)) (x (plist-get in :spread_bps))
           (h (append (plist-get in :spread_history_bps) nil)))
      (should (financial-chart-3d-rates-test--near
               (plist-get mc :value) (/ (* 100.0 (seq-count (lambda (v) (<= v x)) h)) (length h))))))
  ;; fi.krd: the sum of the key-rate durations; fi.dv01: price * duration / 1e4.
  (dolist (e (financial-chart-3d-rates-test--data "key-rate-durations"))
    (let ((krd (plist-get e :krd)) (dv01 (plist-get e :dv01)))
      (should (financial-chart-3d-rates-test--near
               (plist-get krd :value) (apply #'+ (append (plist-get (plist-get krd :inputs) :key_rate_shocks) nil))))
      (should (financial-chart-3d-rates-test--near
               (plist-get dv01 :value) (* (plist-get (plist-get dv01 :inputs) :price)
                                          (plist-get (plist-get dv01 :inputs) :duration) 1e-4)))))
  ;; credit.cds_isda_implied_hazard_pd: one node per quoted maturity, PD rising.
  (dolist (e (financial-chart-3d-rates-test--data "cds-term-structure"))
    (let ((curve (append (plist-get (plist-get e :output) :curve) nil)))
      (should (equal (mapcar (lambda (n) (plist-get n :maturity)) curve)
                     (mapcar (lambda (s) (plist-get s :maturity)) (plist-get (plist-get e :input) :spreads))))
      (should (equal (plist-get (plist-get e :output) :as_of) (plist-get (plist-get e :input) :as_of)))
      (cl-loop for (a b) on curve while b
               do (should (< (plist-get a :cumulative_default_probability)
                             (plist-get b :cumulative_default_probability)))))))

;;; Adapters

(ert-deftest financial-chart-3d-rates-curve-rows ()
  (let* ((history (financial-chart-3d-rates-test--data "yield-curve"))
         (rows (financial-chart-3d-rates-curve-rows history))
         (spot (seq-filter (lambda (r) (equal (plist-get r :kind) "spot")) rows))
         (forward (seq-filter (lambda (r) (equal (plist-get r :kind) "forward")) rows))
         (at (lambda (date tenor) (seq-find (lambda (r) (and (equal (plist-get r :date) date)
                                                             (= (plist-get r :tenor) tenor)))
                                            spot))))
    (should (= (length spot) (* 16 10)))
    (should (= (length forward) 7))
    (should (equal (plist-get (funcall at "2023-06-30" 0.25) :yield) 5.43))
    (should (equal (plist-get (funcall at "2023-06-30" 0.25) :tenor_label) "3M"))
    (should (equal (plist-get (funcall at "2023-06-30" 10.0) :tenor_label) "10Y"))
    ;; Inverted from 2022's last quarter to 2024's third, as the metric says.
    (should (equal (delete-dups (mapcar (lambda (r) (cons (plist-get r :date) (plist-get r :regime))) spot))
                   (mapcar (lambda (e) (cons (plist-get e :as_of)
                                             (if (member (plist-get e :as_of)
                                                         '("2022-12-30" "2023-03-31" "2023-06-30" "2023-09-29"
                                                           "2023-12-29" "2024-03-28" "2024-06-28" "2024-09-30"))
                                                 "inverted" "normal")))
                           history)))
    ;; Carry only where rolldown_carry was given: the last date.
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :date))
                                        (seq-filter (lambda (r) (numberp (plist-get r :carry_bps))) spot)))
                   '("2025-12-31")))
    (should (equal (plist-get (funcall at "2025-12-31" 10.0) :carry_bps) 103.0))
    ;; Flat rows pass through.
    (should (equal (financial-chart-3d-rates-curve-rows rows) rows))))

(ert-deftest financial-chart-3d-rates-curve-rows-read-a-verbatim-output ()
  ;; As `invest metric run fi.nss_fit' prints it: every input, nulls too.
  (let* ((output (eas-json-parse "{\"calculation\":\"fixed-income metric; rates are decimal unless the field is explicitly in bps\",\"inputs\":{\"annual_debt_service\":null,\"benchmark_spread_bps\":null,\"bid_ask_bps\":null,\"bill_days\":null,\"bill_discount_rate\":null,\"bill_price\":null,\"call_price\":null,\"call_years\":null,\"cash\":null,\"cashflows\":null,\"cds_spread_bps\":null,\"convexity\":null,\"coupon_rate\":null,\"current_price\":null,\"curve_rates\":[0.03,0.04],\"curve_years\":[1.0,2.0],\"default_probability\":null,\"duration\":null,\"ebitda\":null,\"face\":null,\"forward_rates\":null,\"forward_years\":null,\"frequency\":null,\"general_collateral_rate\":null,\"horizon_years\":null,\"inflation_rate\":null,\"interest_expense\":null,\"issue_price\":null,\"key_rate_shocks\":null,\"market_depth\":null,\"net_income\":null,\"nominal_yield\":null,\"par_yield\":null,\"price\":null,\"principal\":null,\"psa\":null,\"rating\":null,\"real_yield\":null,\"recovery_rate\":null,\"reinvest_rate\":null,\"repo_rate\":null,\"revenue\":null,\"spread_bps\":null,\"spread_history_bps\":null,\"tax_exempt\":null,\"tax_rate\":null,\"total_debt\":null,\"treasury_yield\":null,\"years\":null,\"years_held\":null,\"yield_pct\":null,\"ytm\":null},\"key\":\"nss_fit\",\"value\":0.035}"))
         (rows (financial-chart-3d-rates-curve-rows (vector (list :as_of "2026-10-16" :nss_fit output)))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :tenor) (plist-get r :yield) (plist-get r :regime)
                                             (plist-get r :kind) (plist-get r :carry_bps)))
                           rows)
                   '((1.0 3.0 "normal" "spot" :null) (2.0 4.0 "normal" "spot" :null)))))
  ;; The metric's own input, without nss_fit, reads the same.
  (should (equal (length (financial-chart-3d-rates-curve-rows
                          '((:date "2026-10-16" :curve_years [1 2 5] :curve_rates [0.05 0.04 0.03]))))
                 3))
  (should (equal (plist-get (car (financial-chart-3d-rates-curve-rows
                                  '((:date "2026-10-16" :curve_years [1 2 5] :curve_rates [0.05 0.04 0.03]))))
                            :regime)
                 "inverted"))
  (let ((err (should-error (financial-chart-3d-rates-curve-rows '((:nss_fit (:value 1)))) :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "missing_date")))
  (let ((err (should-error (financial-chart-3d-rates-curve-rows
                            '((:as_of "2026-01-02" :curve_years [1 2] :curve_rates [0.03])))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "bad_curve"))
    (should (equal (plist-get (cddr err) :index) 0))))

(ert-deftest financial-chart-3d-rates-spread-rows ()
  (let ((rows (financial-chart-3d-rates-spread-rows (financial-chart-3d-rates-test--data "credit-spreads"))))
    (should (= (length rows) (* 7 8)))
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :rating)) rows))
                   '("AAA" "AA" "A" "BBB" "BB" "B" "CCC")))
    (should (equal (seq-take (mapcar (lambda (r) (plist-get r :tenor_label)) rows) 8)
                   '("1Y" "2Y" "3Y" "5Y" "7Y" "10Y" "20Y" "30Y")))
    (let ((ccc1 (seq-find (lambda (r) (and (equal (plist-get r :rating) "CCC") (= (plist-get r :tenor) 1))) rows)))
      (should (equal (plist-get ccc1 :spread_bps) 820.0))
      (should (equal (plist-get ccc1 :notch) 4))
      ;; Distressed names are wider now than through their history.
      (should (= (plist-get ccc1 :pctile) 100.0))))
  ;; spread_suite's value wins over spread_pctile's input; pctile alone is null.
  (should (equal (financial-chart-3d-rates-spread-rows
                  '((:key "spread_pctile" :value 40.0 :inputs (:rating "A" :years 5 :spread_bps 70.0))
                    (:key "spread_suite" :value 72.5 :inputs (:rating "A" :years 5 :spread_bps 72.5))
                    (:key "spread_suite" :value 120.0 :inputs (:rating "BBB" :years 5))))
                 '((:rating "A" :notch 16 :tenor 5.0 :tenor_label "5Y" :spread_bps 72.5 :pctile 40.0)
                   (:rating "BBB" :notch 13 :tenor 5.0 :tenor_label "5Y" :spread_bps 120.0 :pctile :null))))
  (should (= (financial-chart-3d-rates-notch "bbb-") 12))
  (should (= (financial-chart-3d-rates-notch "NR") 0))
  (let ((err (should-error (financial-chart-3d-rates-spread-rows '((:key "oas" :value 1 :inputs (:rating "A" :years 5))))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "unknown_metric"))))

(ert-deftest financial-chart-3d-rates-cds-rows ()
  (should (= (- (financial-chart-3d-rates--days "2031-12-20") (financial-chart-3d-rates--days "2026-10-20")) 1887))
  (should (= (financial-chart-3d-rates--days "1970-01-01") 0))
  (should (= (financial-chart-3d-rates--days "2000-03-01") 11017))
  (let* ((data (financial-chart-3d-rates-test--data "cds-term-structure"))
         (rows (financial-chart-3d-rates-cds-rows data)))
    (should (= (length rows) 60))
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :tenor)) rows)) '(1.0 2.0 3.0 5.0 7.0 10.0)))
    ;; With the inputs, the value is the quoted par spread.
    (let ((peak (seq-find (lambda (r) (and (equal (plist-get r :as_of) "2026-04-20") (= (plist-get r :tenor) 5))) rows)))
      (should (equal (plist-get peak :spread_bp) 235.0))
      (should (equal (plist-get peak :value) 235.0))
      (should (> (plist-get peak :hazard_bps) 300))
      (should (< 0 (plist-get peak :pd_pct) 100)))
    (should (equal (mapcar (lambda (r) (plist-get r :value)) (financial-chart-3d-rates-cds-rows data "pd_pct"))
                   (mapcar (lambda (r) (plist-get r :pd_pct)) rows)))
    ;; Outputs alone: the hazard rate.
    (let ((alone (financial-chart-3d-rates-cds-rows (mapcar (lambda (e) (plist-get e :output)) data))))
      (should (equal (mapcar (lambda (r) (plist-get r :value)) alone)
                     (mapcar (lambda (r) (plist-get r :hazard_bps)) rows)))
      (should (eq (plist-get (car alone) :spread_bp) :null))))
  (should-error (financial-chart-3d-rates-cds-rows nil "nope") :type 'financial-chart-invalid-data)
  (let ((err (should-error (financial-chart-3d-rates-cds-rows '((:as_of "2026-01-02")))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "bad_cds"))))

(ert-deftest financial-chart-3d-rates-krd-rows ()
  (let ((rows (financial-chart-3d-rates-krd-rows (financial-chart-3d-rates-test--data "key-rate-durations"))))
    (should (= (length rows) (* 7 6)))
    (should (equal (seq-take (mapcar (lambda (r) (plist-get r :tenor_label)) rows) 6)
                   '("6M" "2Y" "5Y" "10Y" "20Y" "30Y")))
    (let ((short (seq-filter (lambda (r) (equal (plist-get r :holding) "Short 10Y futures")) rows)))
      (should (equal (plist-get (nth 3 short) :krd) -5.2))
      (should (equal (plist-get (car short) :duration) -6.45))
      (should (< (plist-get (car short) :dv01) 0))))
  ;; Tenors: the entry's, else the default key rates; a mismatch is an error.
  (should (equal (mapcar (lambda (r) (plist-get r :tenor))
                         (financial-chart-3d-rates-krd-rows
                          '((:holding "X" :tenors [1 3] :krd (:value 2.0 :inputs (:key_rate_shocks [0.5 1.5]))))))
                 '(1.0 3.0)))
  (should (equal (length (financial-chart-3d-rates-krd-rows
                          '((:holding "X" :krd (:value 2.0 :inputs (:key_rate_shocks [0 0 1 1 0 0]))))))
                 6))
  (let ((err (should-error (financial-chart-3d-rates-krd-rows
                            '((:holding "X" :krd (:value 2.0 :inputs (:key_rate_shocks [1 1])))))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (cddr err) :code) "tenor_mismatch"))))

(ert-deftest financial-chart-3d-rates-shapes-take-metric-json ()
  (dolist (case '(("invest/yield-curve" "yield-curve" 167) ("invest/credit-spreads" "credit-spreads" 56)
                  ("invest/cds-term" "cds-term-structure" 60) ("invest/krd" "key-rate-durations" 42)))
    (let ((data (eas-data-from (car case) (vconcat (financial-chart-3d-rates-test--data (cadr case))))))
      (should (= (length (plist-get data :rows)) (nth 2 case)))
      ;; data/v1 passes through.
      (should (eq (eas-data-from (car case) data) data))))
  (let ((failure (eas-data-check "invest/krd" [(:holding "X")])))
    (should (equal (plist-get failure :code) "SHAPE_INVALID"))
    (should (equal (plist-get failure :index) 0))
    (should (equal (plist-get failure :field) "krd"))))

;;; Templates: 2D and 3D

(ert-deftest financial-chart-3d-rates-templates-draw-in-2d ()
  (dolist (case '(("yield-curve" "5.4") ("credit-spreads" "820") ("cds-term-structure" "235")
                  ("key-rate-durations" "10.6")))
    (dolist (name (list (car case) (financial-chart-3d--name (car case))))
      (ert-info ((format "template %s" name))
        (let* ((resolved (eas-resolve name (eas-template-example name)))
               (text (substring-no-properties
                      (eas-text-render (eas-compile resolved :target 'text :size '(:cols 72 :rows 20))))))
          (should-not (eas-spec-unsupported resolved))
          ;; The cells carry their values.
          (should (string-search (cadr case) text)))))))

(ert-deftest financial-chart-3d-rates-are-in-the-gallery ()
  (dolist (name financial-chart-3d-rates-templates)
    (should (member name (financial-chart-3d-variants)))
    (should (member (plist-get (financial-chart-3d--describe name) :mark) (graphics-eas-all-marks)))))

(ert-deftest financial-chart-3d-rates-curves-replay-with-a-trail ()
  (dolist (name '("yield-curve" "cds-term-structure"))
    (ert-info (name)
      (let ((view (financial-chart-3d-open name :id (concat "fc3d-rates-test:" name))))
        (unwind-protect
            (let ((timeline (graphics-eas-timeline view)))
              (should (> (plist-get timeline :duration) 0))
              (should (plist-get timeline :loop))
              (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
              (let ((start (graphics-eas-frame view 60 24)))
                (financial-chart-3d-seek-end view)
                (should-not (equal start (graphics-eas-frame view 60 24)))))
          (financial-chart-3d--close view))))))

(ert-deftest financial-chart-3d-rates-yield-mark ()
  (let* ((rows (financial-chart-3d-rates-curve-rows (financial-chart-3d-rates-test--data "yield-curve")))
         (graphics '(:mark "fc-yield-curve3d" :encoding (:x (:field "tenor") :z (:field "date") :y (:field "yield")
                                                         :color (:field "regime"))))
         (dates (delete-dups (mapcar (lambda (r) (plist-get r :date)) rows)))
         (upto (lambda (n) (seq-filter (lambda (r) (member (plist-get r :date) (seq-take dates n))) rows)))
         ;; Five curves so far, scales over all sixteen.
         (prims (let ((graphics-eas-domain-rows rows))
                  (financial-chart-3d-rates--term-surface graphics (funcall upto 5))))
         (trail (financial-chart-3d-rates-test--prim prims "trail"))
         (front (financial-chart-3d-rates-test--prim prims "front")))
    ;; Four earlier curves of nine segments, coloured by regime; the newest bright.
    (should (= (length (plist-get trail :segments)) (* 4 9)))
    (should (equal (delete-dups (append (plist-get trail :colors) nil)) '("#26a69a" "#ef5350")))
    (should (= (length (plist-get front :segments)) 9))
    (should (equal (plist-get front :color) "#ffffff"))
    ;; The fifth curve (2023-03-31) sits at the fifth of sixteen depths.
    (should (= (aref (aref (plist-get front :segments) 0) 2)
               (nth 4 (financial-chart-3d-rates--spread 16 4.5))))
    ;; No forward curve until the last date, then one beside it.
    (should (= (length (plist-get (financial-chart-3d-rates-test--prim prims "forward") :segments)) 0))
    (let ((all (financial-chart-3d-rates--term-surface graphics rows)))
      (should (= (length (plist-get (financial-chart-3d-rates-test--prim all "forward") :segments)) 6)))
    ;; fill draws the lit surface, its cells coloured by regime too.
    (let ((mesh (financial-chart-3d-rates-test--prim
                 (financial-chart-3d-rates--term-surface (append graphics '(:fill t)) rows) "surface")))
      (should (equal (plist-get mesh :type) "mesh"))
      (should (= (length (plist-get mesh :triangles)) (* 2 9 15)))
      (should (member "#ef5350" (append (plist-get mesh :colors) nil))))))

(ert-deftest financial-chart-3d-rates-credit-and-krd-marks ()
  (let* ((rows (financial-chart-3d-rates-spread-rows (financial-chart-3d-rates-test--data "credit-spreads")))
         (prims (financial-chart-3d-rates--credit-mark '(:mark "fc-credit3d") rows))
         (curves (financial-chart-3d-rates-test--prim prims "curves"))
         (front-z (aref (aref (plist-get curves :segments) 0) 2)))
    ;; Seven ratings of seven segments, AAA in front, coloured by percentile.
    (should (= (length (plist-get curves :segments)) 49))
    (should (> front-z 0))
    (should (> (length (delete-dups (append (plist-get curves :colors) nil))) 20))
    (should (= (length (plist-get (financial-chart-3d-rates-test--prim prims "ribs") :segments)) (* 8 6)))
    ;; Log height: CCC's 820 is not ten times A's 82.
    (let ((linear (financial-chart-3d-rates--credit-mark '(:mark "fc-credit3d" :scale "linear") rows)))
      (should-not (equal (plist-get (financial-chart-3d-rates-test--prim linear "curves") :segments)
                         (plist-get curves :segments)))))
  (let* ((rows (financial-chart-3d-rates-krd-rows (financial-chart-3d-rates-test--data "key-rate-durations")))
         (bars (financial-chart-3d-rates-test--prim (financial-chart-3d-rates--krd-mark '(:mark "fc-krd3d") rows) "bars"))
         (boxes (append (plist-get bars :boxes) nil)))
    ;; A box per non-zero key-rate duration; the short futures hang below the floor.
    (should (= (length boxes) (seq-count (lambda (r) (/= (plist-get r :krd) 0)) rows)))
    (should (= (seq-count (lambda (b) (< (aref b 1) 0)) boxes) 3))
    (should (= (length (delete-dups (append (plist-get bars :colors) nil))) 7))
    ;; total adds each holding's whole duration.
    (should (= (length (plist-get (financial-chart-3d-rates-test--prim
                                   (financial-chart-3d-rates--krd-mark '(:mark "fc-krd3d" :total t) rows) "bars")
                                  :boxes))
               (+ (length boxes) 7)))))

(ert-deftest financial-chart-3d-rates-show-falls-back-to-2d ()
  (let ((financial-chart-3d-graphics-feature 'financial-chart-3d-rates-test-no-such-feature)
        (data (vconcat (financial-chart-3d-rates-test--data "key-rate-durations"))))
    (let ((view (financial-chart-3d-rates-show "key-rate-durations" data :id "fc3d-rates-test-2d")))
      (unwind-protect
          (progn (should (eas-view-p view))
                 (should-not (graphics-eas-get view))
                 (should (= (length (plist-get (eas-view-data view) :rows)) 42)))
        (when-let* ((buffer (get-buffer (format "*eas %s*" (eas-view-id view))))) (kill-buffer buffer))
        (eas-view-close view))))
  (let ((view (financial-chart-3d-rates-show "key-rate-durations"
                                             (vconcat (financial-chart-3d-rates-test--data "key-rate-durations"))
                                             :id "fc3d-rates-test-3d")))
    (unwind-protect (should (graphics-eas-get view))
      (financial-chart-3d--close view)))
  (should-error (financial-chart-3d-rates-show "nope" nil) :type 'financial-chart-invalid-data))

(provide 'financial-chart-3d-rates-test)
;;; financial-chart-3d-rates-test.el ends here
