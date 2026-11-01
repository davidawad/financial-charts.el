;;; financial-chart-3d-rates.el --- 3D rates and credit charts via graphics.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Four rates and credit templates, each with a 2D template
;; (templates/financial/) and its 3D variant (templates3d/), fed the
;; JSON the invest-metrics metrics emit (`invest metric run ID'); this
;; package never fetches it.  docs/3d-rates.md has each data contract.
;;
;;   yield-curve          fi.nss_fit (+ fi.curve_regime, fi.forward_path,
;;                        fi.rolldown_carry) per date: a yield surface over
;;                        tenor and date, coloured by curve regime, that
;;                        replays one curve per frame and leaves a trail
;;   credit-spreads       fi.spread_suite / fi.spread_pctile per rating
;;                        and tenor: a spread surface coloured by the
;;                        spread's percentile in its own history
;;   cds-term-structure   credit.cds_isda_implied_hazard_pd per date (with
;;                        its input's par spreads): the term structure as
;;                        a surface over tenor and date, replayed
;;   key-rate-durations   fi.krd (+ fi.dv01) per holding: 3D bars of key
;;                        rate duration, tenor across, holding back
;;
;; Each template's data slot has an eas data shape (registered here,
;; "invest/yield-curve", ...) that lowers the metric JSON to flat rows,
;; so 2D and 3D draw the same rows; rows already flat pass through.
;; The shapes and adapters
;; (`financial-chart-3d-rates-curve-rows', ...) need no graphics.el; the
;; marks are registered with `graphics-eas-define-mark' when graphics.el
;; loads:
;;
;;   fc-yield-curve3d  a surface of curves over tenor and date, rows
;;                     coloured by regime (or by level), each curve
;;                     traced, the newest bright, a forward curve beside
;;   fc-cds-term3d     the same surface of curves, coloured by level
;;   fc-credit3d       a spread surface over tenor and rating (log
;;                     height), cells coloured by percentile
;;   fc-krd3d          signed bars over tenor and holding, a total
;;                     duration column at the end
;;
;;   (financial-chart-3d-open "yield-curve" :bindings '(:data HISTORY) :show t)
;;   (financial-chart-3d-rates-show "credit-spreads" METRICS) ; 2D without graphics.el

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-3d)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))

;;; Small helpers

(defun financial-chart-3d-rates--invalid (message code index field)
  "Signal `financial-chart-invalid-data': MESSAGE, CODE at INDEX's FIELD."
  (signal 'financial-chart-invalid-data
          (list (concat "financial-chart-3d-rates: " message) :code code :index index :field field)))

(defun financial-chart-3d-rates--get (object &rest keys)
  "OBJECT's value at the path KEYS (keywords), nil for null or missing."
  (let ((v object))
    (dolist (k keys)
      (setq v (and (keywordp (car-safe v)) (financial-chart-3d--value v k))))
    v))

(defun financial-chart-3d-rates--list (v)
  "V (a vector or list) as a list; nil otherwise."
  (cond ((vectorp v) (append v nil))
        ((and (listp v) (not (keywordp (car-safe v)))) v)))

(defun financial-chart-3d-rates--round (v digits)
  "V rounded to DIGITS decimals, a float."
  (let ((m (expt 10.0 digits))) (/ (fround (* v m)) m)))

(defun financial-chart-3d-rates-tenor-label (years)
  "A tenor of YEARS as traders write it: 3M, 6M, 1Y, 2.5Y, 30Y."
  (let ((years (float years)))
    (if (< years 1)
        (format "%dM" (round (* years 12)))
      (if (= years (fround years)) (format "%dY" (round years)) (format "%gY" years)))))

(defun financial-chart-3d-rates--days (date)
  "Days since 1970-01-01 of ISO DATE (\"YYYY-MM-DD\"), or nil."
  (when (and (stringp date) (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date))
    ;; Days from civil (Howard Hinnant's algorithm).
    (let* ((y (string-to-number (match-string 1 date)))
           (m (string-to-number (match-string 2 date)))
           (d (string-to-number (match-string 3 date)))
           (y (if (<= m 2) (1- y) y))
           (era (floor y 400))
           (yoe (- y (* era 400)))
           (doy (+ (/ (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (1- d)))
           (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
      (+ (* era 146097) doe -719468))))

(defun financial-chart-3d-rates--flat-p (rows keys)
  "Non-nil when every row of ROWS already carries every one of KEYS, as atoms.
An entry stamped with some of KEYS but holding a metric output (a
plist) under another is not flat (fc-jyr: {holding, tenor, krd: output})."
  (and rows (seq-every-p (lambda (r) (seq-every-p (lambda (k) (and (plist-member r k)
                                                                   (atom (plist-get r k))
                                                                   (not (vectorp (plist-get r k)))))
                                                  keys))
                         rows)))

;;; Yield curves: fi.nss_fit per date

(defconst financial-chart-3d-rates-regimes
  '((-1 . "inverted") (0 . "flat") (1 . "normal"))
  "fi.curve_regime values and their names.")

(defun financial-chart-3d-rates-regime (rates)
  "The regime name of curve RATES (short tenor first), as fi.curve_regime decides.
\"normal\" when the last rate is above the first, \"inverted\" below it,
else \"flat\" (1e-8 apart or fewer than two)."
  (let ((first (car rates)) (last (car (last rates))))
    (cond ((< (length rates) 2) "flat")
          ((> last (+ first 1e-8)) "normal")
          ((< last (- first 1e-8)) "inverted")
          (t "flat"))))

(defun financial-chart-3d-rates--curve (entry)
  "The (YEARS . RATES) of history ENTRY: its nss_fit's inputs, else its own."
  (let ((source (if (financial-chart-3d-rates--get entry :nss_fit :inputs :curve_rates)
                    (financial-chart-3d-rates--get entry :nss_fit :inputs)
                  entry)))
    (cons (financial-chart-3d-rates--list (financial-chart-3d--value source :curve_years))
          (financial-chart-3d-rates--list (financial-chart-3d--value source :curve_rates)))))

(defun financial-chart-3d-rates-curve-rows (history)
  "Flat yield-curve rows of HISTORY, the fi.nss_fit outputs by date.
HISTORY is a sequence of entries {as_of, nss_fit, curve_regime?,
forward_path?, rolldown_carry?}, oldest first: nss_fit is the metric's
output, whose inputs carry curve_years and curve_rates (decimals);
curve_regime's value (-1, 0, 1) names the regime, else it is decided
from the curve as the metric decides it; forward_path's inputs
forward_years and forward_rates are a forward curve of that date;
rolldown_carry is a list of outputs whose inputs.years is a tenor and
value its carry in bps.  An entry may carry curve_years and
curve_rates itself instead of nss_fit, and `date' for as_of.
Rows {date, tenor, tenor_label, yield (percent), regime, kind
\(\"spot\" or \"forward\"), carry_bps}; rows already flat (with date,
tenor and yield) pass through."
  (let ((entries (financial-chart-3d--rows history)))
    (if (financial-chart-3d-rates--flat-p entries '(:date :tenor :yield)) entries
      (cl-loop
       for e in entries for i from 0
       append
       (let* ((date (or (financial-chart-3d--value e :as_of) (financial-chart-3d--value e :date)))
              (curve (financial-chart-3d-rates--curve e))
              (years (car curve)) (rates (cdr curve))
              (code (financial-chart-3d-rates--get e :curve_regime :value))
              (regime (or (and (numberp code) (cdr (assoc (round code) financial-chart-3d-rates-regimes)))
                          (financial-chart-3d-rates-regime rates)))
              (carry (delq nil (mapcar (lambda (mc)
                                         (let ((y (financial-chart-3d-rates--get mc :inputs :years))
                                               (v (financial-chart-3d--value mc :value)))
                                           (and (numberp y) (numberp v) (cons (float y) v))))
                                       (financial-chart-3d-rates--list (financial-chart-3d--value e :rolldown_carry)))))
              (fy (financial-chart-3d-rates--list (financial-chart-3d-rates--get e :forward_path :inputs :forward_years)))
              (fr (financial-chart-3d-rates--list (financial-chart-3d-rates--get e :forward_path :inputs :forward_rates))))
         (unless date
           (financial-chart-3d-rates--invalid "each curve needs its date (as_of)" "missing_date" i "as_of"))
         (unless (and years (= (length years) (length rates))
                      (seq-every-p #'numberp years) (seq-every-p #'numberp rates))
           (financial-chart-3d-rates--invalid
            "each curve needs nss_fit.inputs.curve_years and curve_rates, numbers of one length"
            "bad_curve" i "nss_fit"))
         (append
          (cl-loop for y in years for r in rates
                   collect (list :date date :tenor (float y)
                                 :tenor_label (financial-chart-3d-rates-tenor-label y)
                                 :yield (financial-chart-3d-rates--round (* 100 r) 4)
                                 :regime regime :kind "spot"
                                 :carry_bps (let ((c (cdr (assoc (float y) carry))))
                                              (if c (financial-chart-3d-rates--round c 2) :null))))
          (cl-loop for y in fy for r in fr
                   when (and (numberp y) (numberp r))
                   collect (list :date date :tenor (float y)
                                 :tenor_label (financial-chart-3d-rates-tenor-label y)
                                 :yield (financial-chart-3d-rates--round (* 100 r) 4)
                                 :regime regime :kind "forward" :carry_bps :null))))))))

;;; Credit spreads: fi.spread_suite / fi.spread_pctile by rating and tenor

(defconst financial-chart-3d-rates-ratings
  ["D" "C" "CC" "CCC-" "CCC" "CCC+" "B-" "B" "B+" "BB-" "BB" "BB+" "BBB-" "BBB"
   "BBB+" "A-" "A" "A+" "AA-" "AA" "AA+" "AAA"]
  "Ratings by notch, D (0) to AAA (21), as fi.rating_notch ranks them.")

(defun financial-chart-3d-rates-notch (rating)
  "The notch of RATING (0 for D or an unknown rating), as fi.rating_notch."
  (or (and (stringp rating)
           (seq-position financial-chart-3d-rates-ratings (upcase (string-trim rating))))
      0))

(defun financial-chart-3d-rates-spread-rows (metrics)
  "Flat credit-spread rows of METRICS: fi.spread_suite, fi.spread_pctile outputs.
Each output's inputs carry rating and years (the tenor); spread_suite's
value is the spread in bps, spread_pctile's the percentile (0 to 100)
of inputs.spread_bps in inputs.spread_history_bps.  Outputs of one
rating and tenor make one row {rating, notch, tenor, tenor_label,
spread_bps, pctile}, best rating first, then by tenor; a spread from
spread_suite wins over spread_pctile's input.  Rows already flat (with
rating, tenor and spread_bps) pass through."
  (let ((outputs (financial-chart-3d--rows metrics)))
    (if (financial-chart-3d-rates--flat-p outputs '(:rating :tenor :spread_bps)) outputs
      (let ((cells (make-hash-table :test #'equal)) order)
        (cl-loop
         for mc in outputs for i from 0
         do (let* ((key (financial-chart-3d--value mc :key))
                   (rating (financial-chart-3d-rates--get mc :inputs :rating))
                   (years (financial-chart-3d-rates--get mc :inputs :years))
                   (value (financial-chart-3d--value mc :value))
                   (cell-key (cons rating (and (numberp years) (float years))))
                   (cell (gethash cell-key cells)))
              (unless (member key '("spread_suite" "spread_pctile"))
                (financial-chart-3d-rates--invalid
                 (format "output %d is %S, not spread_suite or spread_pctile" i key) "unknown_metric" i "key"))
              (unless (and (stringp rating) (numberp years))
                (financial-chart-3d-rates--invalid
                 "each spread output needs inputs.rating and inputs.years" "missing_cell" i "inputs"))
              (unless cell (push cell-key order))
              (when (numberp value)
                (if (equal key "spread_suite")
                    (setq cell (plist-put cell :suite value))
                  (setq cell (plist-put cell :pctile value))
                  (setq cell (plist-put cell :input (financial-chart-3d-rates--get mc :inputs :spread_bps)))))
              (puthash cell-key cell cells)))
        (let ((rows (delq nil
                          (mapcar (lambda (k)
                                    (let* ((cell (gethash k cells))
                                           (spread (or (plist-get cell :suite) (plist-get cell :input))))
                                      (when (numberp spread)
                                        (list :rating (car k) :notch (financial-chart-3d-rates-notch (car k))
                                              :tenor (cdr k) :tenor_label (financial-chart-3d-rates-tenor-label (cdr k))
                                              :spread_bps (financial-chart-3d-rates--round spread 2)
                                              :pctile (let ((p (plist-get cell :pctile)))
                                                        (if (numberp p) (financial-chart-3d-rates--round p 2) :null))))))
                                  (nreverse order)))))
          (sort rows (lambda (a b) (if (/= (plist-get a :notch) (plist-get b :notch))
                                       (> (plist-get a :notch) (plist-get b :notch))
                                     (< (plist-get a :tenor) (plist-get b :tenor))))))))))

;;; CDS term structure: credit.cds_isda_implied_hazard_pd by date

(defconst financial-chart-3d-rates-cds-measures
  '("value" "spread_bp" "hazard_bps" "pd_pct" "hazard_rate" "survival"
    "cumulative_pd" "forward_hazard")
  "The fields a CDS chart draws, the cds-term-structure template's measure enum.
The names are shared by the adapter and 2D/3D templates; \"auto\"
resolves to a concrete field present on all applicable rows.")

(defun financial-chart-3d-rates-cds-measure (rows measure)
  "The field of `financial-chart-3d-rates-cds-measures' MEASURE draws in ROWS.
MEASURE nil or \"auto\" is the par spread (spread_bp) when every row has
one, else the hazard rate (hazard_bps), or value for rows already flat
without either; any other measure is itself.
Signals `financial-chart-invalid-data' (bad_measure) for one not in
the set."
  (cond ((member measure '(nil "auto"))
         (cond ((and rows (seq-every-p (lambda (r) (numberp (plist-get r :spread_bp))) rows)) "spread_bp")
               ((seq-every-p (lambda (r) (numberp (plist-get r :hazard_bps))) rows) "hazard_bps")
               ((seq-every-p (lambda (r) (numberp (plist-get r :hazard_rate))) rows) "hazard_rate")
               (t "value")))
        ((member measure financial-chart-3d-rates-cds-measures) measure)
        (t (financial-chart-3d-rates--invalid
            (format "measure %S is not one of auto, %s" measure
                    (string-join financial-chart-3d-rates-cds-measures ", "))
            "bad_measure" nil "measure"))))

(defun financial-chart-3d-rates-cds-rows (entries &optional measure)
  "Flat CDS term-structure rows of ENTRIES, ISDA hazard and PD outputs.
Each entry is the metric's output {reference_entity, as_of, curve:
[{maturity, hazard_rate, cumulative_default_probability}]}, or
{input, output} with the metric's input too, whose spreads give each
maturity's par spread.  Rows {as_of, tenor (whole years to maturity),
tenor_label, maturity, spread_bp, hazard_bps, pd_pct, value}, by date
then tenor.  MEASURE (`financial-chart-3d-rates-cds-measure') picks
value; nil, \"auto\" or \"value\" (the default) is the par spread when
every row has one, else the hazard rate.  Rows already flat (with
as_of, tenor and value) pass through."
  (let ((entries (financial-chart-3d--rows entries)))
    (financial-chart-3d-rates-cds-measure nil measure)
    (if (financial-chart-3d-rates--flat-p entries '(:as_of :tenor :value)) entries
      (let* ((rows
              (cl-loop
               for e in entries for i from 0
               append
               (let* ((output (or (financial-chart-3d--value e :output) e))
                      (input (financial-chart-3d--value e :input))
                      (as-of (financial-chart-3d--value output :as_of))
                      (start (financial-chart-3d-rates--days as-of))
                      (spreads (mapcar (lambda (s) (cons (financial-chart-3d--value s :maturity)
                                                         (financial-chart-3d--value s :spread_bp)))
                                       (financial-chart-3d-rates--list (financial-chart-3d--value input :spreads))))
                      (curve (financial-chart-3d-rates--list (financial-chart-3d--value output :curve))))
                 (unless (and start curve)
                   (financial-chart-3d-rates--invalid
                    "each CDS output needs as_of (YYYY-MM-DD) and its curve" "bad_cds" i "output"))
                 (cl-loop for node in curve for j from 0
                          collect (let* ((maturity (financial-chart-3d--value node :maturity))
                                         (end (financial-chart-3d-rates--days maturity))
                                         (h (financial-chart-3d--value node :hazard_rate))
                                         (pd (financial-chart-3d--value node :cumulative_default_probability))
                                         (s (cdr (assoc maturity spreads))))
                                    (unless (and end (numberp h) (numberp pd))
                                      (financial-chart-3d-rates--invalid
                                       (format "curve node %d of entry %d needs maturity, hazard_rate and cumulative_default_probability" j i)
                                       "bad_node" i "curve"))
                                    (let ((tenor (float (max 1 (round (/ (- end start) 365.25))))))
                                      (append
                                       (cl-loop for k in '(:reference_entity :source)
                                                for v = (financial-chart-3d--value output k)
                                                when (stringp v) append (list k v))
                                       (list :as_of as-of :tenor tenor
                                             :tenor_label (financial-chart-3d-rates-tenor-label tenor)
                                             :maturity maturity
                                             :spread_bp (if (numberp s) (float s) :null)
                                             :hazard_bps (financial-chart-3d-rates--round (* 10000 h) 2)
                                             :pd_pct (financial-chart-3d-rates--round (* 100 pd) 3)))))))))
             (field (financial-chart-3d--key
                     (financial-chart-3d-rates-cds-measure rows (if (equal measure "value") nil measure)))))
        (mapcar (lambda (r)
                  (append r (list :hazard_rate (/ (plist-get r :hazard_bps) 10000.0)
                                  :survival (- 1.0 (/ (plist-get r :pd_pct) 100.0))
                                  :cumulative_pd (/ (plist-get r :pd_pct) 100.0)
                                  :forward_hazard :null
                                  :value (let ((v (plist-get r field))) (if (numberp v) v :null)))))
                rows)))))

(defun financial-chart-3d-rates-cds-term-rows (output &optional measure)
  "Adapt credit.cds_term_structure OUTPUT to rows for the existing template.
Top-level metadata is copied onto each row for complete element tooltips."
  (let* ((tenors (financial-chart-3d-rates--list (financial-chart-3d--value output :tenors)))
         (buckets (financial-chart-3d-rates--list (financial-chart-3d--value output :buckets)))
         (context (cl-loop for (k v) on output by #'cddr
                           unless (memq k '(:tenors :buckets)) append (list k v)))
         rows)
    (unless (and (stringp (financial-chart-3d--value output :as_of))
                 (or tenors (equal (financial-chart-3d--value output :shape) "insufficient")))
      (financial-chart-3d-rates--invalid "cds_term_structure needs as_of and tenors (or insufficient shape)"
                                         "bad_cds_term_structure" 0 "tenors"))
    (dolist (node tenors)
      (let* ((years (financial-chart-3d--value node :tenor_years))
             (row (append (copy-sequence context) (copy-sequence node)
                          (list :row_kind "tenor" :tenor years
                                :tenor_label (and (numberp years) (financial-chart-3d-rates-tenor-label years))
                                :spread_bp (let ((s (financial-chart-3d--value node :par_spread_bp)))
                                             (if (numberp s) s :null))
                                :hazard_bps (let ((h (financial-chart-3d--value node :hazard_rate)))
                                              (if (numberp h) (* h 10000) :null))
                                :pd_pct (let ((pd (financial-chart-3d--value node :cumulative_pd)))
                                          (if (numberp pd) (* pd 100) :null)
                                          )
                                :value (let ((s (financial-chart-3d--value node :par_spread_bp))
                                             (h (financial-chart-3d--value node :hazard_rate)))
                                         (or (and (numberp s) s) (and (numberp h) (* h 10000)) :null))))))
        (unless (and (numberp years) (numberp (financial-chart-3d--value node :hazard_rate))
                     (numberp (financial-chart-3d--value node :survival))
                     (numberp (financial-chart-3d--value node :cumulative_pd)))
          (financial-chart-3d-rates--invalid "tenor needs tenor_years, hazard_rate, survival and cumulative_pd"
                                             "bad_tenor" 0 "tenors"))
        (push row rows)))
    (dolist (bucket buckets)
      (let* ((h (financial-chart-3d--value bucket :forward_hazard))
             (pd (financial-chart-3d--value bucket :conditional_pd))
             (row (append (copy-sequence context) (copy-sequence bucket)
                          (list :row_kind "bucket" :tenor (financial-chart-3d--value bucket :from_years)
                                :tenor_label (financial-chart-3d--value bucket :label)
                                :par_spread_bp :null :spread_bp :null :hazard_rate :null :survival :null
                                :cumulative_pd :null :hazard_bps :null :pd_pct :null
                                :value (if (numberp h) (* h 10000) :null)))))
        (unless (and (numberp h) (numberp pd)
                     (numberp (financial-chart-3d--value bucket :from_years))
                     (numberp (financial-chart-3d--value bucket :to_years)))
          (financial-chart-3d-rates--invalid "bucket needs from_years, to_years, forward_hazard and conditional_pd"
                                             "bad_bucket" 0 "buckets"))
        (push row rows)))
    (setq rows (nreverse rows))
    (let ((field (financial-chart-3d--key (financial-chart-3d-rates-cds-measure rows measure))))
      (mapcar (lambda (row)
                (let ((v (plist-get row field)))
                  (plist-put row :value (if (numberp v) v :null)))) rows))))

(defconst financial-chart-3d-rates-cds-ladder-measures
  '("model_yield" "model_price" "cds_spread_bp" "expected_loss_per_100" "dv01_usd"))

(defun financial-chart-3d-rates-cds-bond-rows (output &optional measure)
  "Rows from credit.cds_implied_ladder OUTPUT, retaining its full bond detail."
  (let* ((source (if (and (not (keywordp (car-safe output))) (= (length output) 1)
                          (keywordp (car-safe (aref (vconcat output) 0)))
                          (plist-member (aref (vconcat output) 0) :rows))
                     (aref (vconcat output) 0) output))
         (raw (and (keywordp (car-safe source)) (plist-member source :rows)
                   (or (plist-member source :as_of) (plist-member source :basis))))
         (flat (financial-chart-3d--rows source))
         (rows (if raw (financial-chart-3d-rates--list (financial-chart-3d--value source :rows)) flat))
         (summary (financial-chart-3d--value source :summary))
         (context (and raw (list :as_of (financial-chart-3d--value source :as_of)
                                 :basis (financial-chart-3d--value source :basis)
                                 :recovery (financial-chart-3d--value source :recovery)
                                 :discount_rate (financial-chart-3d--value source :discount_rate)
                                 :summary summary))))
    (unless rows (financial-chart-3d-rates--invalid "CDS implied ladder needs rows"
                                                    "bad_cds_ladder" 0 "rows"))
    (unless (or (member measure '(nil "auto"))
                (member measure financial-chart-3d-rates-cds-ladder-measures))
      (financial-chart-3d-rates--invalid (format "unknown CDS ladder measure %S" measure)
                                         "bad_measure" nil "measure"))
    (mapcar (lambda (row)
              (unless (and (stringp (financial-chart-3d--value row :name))
                           (numberp (financial-chart-3d--value row :years)))
                (financial-chart-3d-rates--invalid "bond row needs name and years"
                                                   "bad_bond" nil "rows"))
              (append (copy-sequence context) (copy-sequence row))) rows)))

;;; Key-rate durations: fi.krd (+ fi.dv01) by holding

(defconst financial-chart-3d-rates-key-rates '(0.5 2 5 10 20 30)
  "Key-rate tenors (years) assumed when a fi.krd output does not name them.")

(defun financial-chart-3d-rates-krd-rows (entries &optional tenors)
  "Flat key-rate duration rows of ENTRIES, one per holding and key rate.
Each entry is {holding, krd, dv01?}: krd the fi.krd output (its value
the duration, inputs.key_rate_shocks the duration at each key rate),
dv01 the fi.dv01 output.  The key rates are the entry's tenors, else
krd's inputs.curve_years when as many, else TENORS (default
`financial-chart-3d-rates-key-rates').  Rows {holding, tenor,
tenor_label, krd, duration, dv01}, in ENTRIES' order.  Rows already
flat (with holding, tenor and krd) pass through."
  (let ((entries (financial-chart-3d--rows entries)))
    (if (financial-chart-3d-rates--flat-p entries '(:holding :tenor :krd)) entries
      (cl-loop
       for e in entries for i from 0
       append
       (let* ((holding (financial-chart-3d--value e :holding))
              (shocks (financial-chart-3d-rates--list (financial-chart-3d-rates--get e :krd :inputs :key_rate_shocks)))
              (named (or (financial-chart-3d-rates--list (financial-chart-3d--value e :tenors))
                         (financial-chart-3d-rates--list (financial-chart-3d-rates--get e :krd :inputs :curve_years))))
              (keys (if (= (length named) (length shocks)) named
                      (or tenors financial-chart-3d-rates-key-rates)))
              (duration (financial-chart-3d-rates--get e :krd :value))
              (dv01 (financial-chart-3d-rates--get e :dv01 :value)))
         (unless (stringp holding)
           (financial-chart-3d-rates--invalid "each entry needs its holding's name" "missing_holding" i "holding"))
         (unless (and shocks (seq-every-p #'numberp shocks))
           (financial-chart-3d-rates--invalid "each entry needs krd.inputs.key_rate_shocks, numbers"
                                              "missing_krd" i "krd"))
         (unless (= (length keys) (length shocks))
           (financial-chart-3d-rates--invalid
            (format "%d key-rate durations but %d key rates; pass tenors" (length shocks) (length keys))
            "tenor_mismatch" i "tenors"))
         (cl-loop for y in keys for k in shocks
                  collect (list :holding holding :tenor (float y)
                                :tenor_label (financial-chart-3d-rates-tenor-label y)
                                :krd (float k)
                                :duration (financial-chart-3d-rates--round
                                           (if (numberp duration) duration (apply #'+ shocks)) 4)
                                :dv01 (if (numberp dv01) (financial-chart-3d-rates--round dv01 6) :null))))))))

;;; eas data shapes: metric JSON -> rows, for 2D and 3D alike

(defun financial-chart-3d-rates--shape (shape doc adapter example)
  "Register eas data SHAPE (DOC) whose rows are ADAPTER's of the caller's value.
EXAMPLE is a value it accepts.  A `financial-chart-invalid-data' from
ADAPTER becomes eas's SHAPE_INVALID, naming the entry and field."
  (eas-register-adapter
   shape :doc doc :example example
   :convert (lambda (value)
              (if (and (keywordp (car-safe value)) (plist-get value :schema) (plist-get value :rows))
                  value
                (condition-case err
                    (eas-data-make (vconcat (funcall adapter value)))
                  (financial-chart-invalid-data
                   (eas-shape-invalid (cadr err) (plist-get (cddr err) :index) (plist-get (cddr err) :field)
                                      :reason (plist-get (cddr err) :code))))))))

(financial-chart-3d-rates--shape
 "invest/yield-curve"
 "fi.nss_fit outputs by date, {as_of, nss_fit, curve_regime?, forward_path?, rolldown_carry?}, as rows {date, tenor, yield, regime, kind} (docs/3d-rates.md)."
 #'financial-chart-3d-rates-curve-rows
 [(:as_of "2026-01-30" :nss_fit (:key "nss_fit" :value 0.04 :inputs (:curve_years [2 10] :curve_rates [0.035 0.045])))])

(financial-chart-3d-rates--shape
 "invest/credit-spreads"
 "fi.spread_suite / fi.spread_pctile outputs (inputs rating, years) as rows {rating, tenor, spread_bps, pctile} (docs/3d-rates.md)."
 #'financial-chart-3d-rates-spread-rows
 [(:key "spread_suite" :value 95.0 :inputs (:rating "A" :years 10 :spread_bps 95.0))])

(financial-chart-3d-rates--shape
 "invest/cds-term"
 "credit.cds_isda_implied_hazard_pd outputs (or {input, output}) by date as rows {as_of, tenor, spread_bp, hazard_bps, pd_pct, value} (docs/3d-rates.md)."
 (lambda (value)
   (let ((first (car (financial-chart-3d--rows value))))
     (cond ((and (keywordp (car-safe value)) (plist-member value :tenors))
            (financial-chart-3d-rates-cds-term-rows value))
           ((and first (plist-member first :tenors))
            (financial-chart-3d-rates-cds-term-rows first))
           (t (financial-chart-3d-rates-cds-rows value)))))
 [(:as_of "2026-10-16" :curve [(:maturity "2031-12-20" :hazard_rate 0.02 :cumulative_default_probability 0.09)])])

(financial-chart-3d-rates--shape
 "invest/cds-bond-ladder"
 "bond_corp.cds_implied_ladder output rows with full bond valuation context (docs/3d-rates.md)."
 #'financial-chart-3d-rates-cds-bond-rows
 [(:as_of "2026-10-16" :basis "cds_implied_model, not traded" :rows [(:name "NVDA 2030" :years 4.0 :model_yield 0.05)]
   :summary (:count 1))])

(financial-chart-3d-rates--shape
 "invest/krd"
 "{holding, krd, dv01?} with fi.krd and fi.dv01 outputs as rows {holding, tenor, krd, duration, dv01} (docs/3d-rates.md)."
 #'financial-chart-3d-rates-krd-rows
 [(:holding "UST 10Y" :krd (:key "krd" :value 8.0 :inputs (:key_rate_shocks [0.1 0.4 7.5] :curve_years [2 5 10])))])

;;; 3D: geometry

(defconst financial-chart-3d-rates-regime-colors
  '(("normal" . "#26a69a") ("flat" . "#eeca3b") ("inverted" . "#ef5350"))
  "Regime colours: upward sloping teal, flat amber, inverted red.")

(defun financial-chart-3d-rates--hex (rgb)
  "RGB, three numbers 0-255, as \"#rrggbb\"."
  (apply #'format "#%02x%02x%02x" (mapcar (lambda (c) (max 0 (min 255 (round c)))) rgb)))

(defun financial-chart-3d-rates--rgb (hex)
  "HEX \"#rrggbb\" as a list of three numbers."
  (list (string-to-number (substring hex 1 3) 16) (string-to-number (substring hex 3 5) 16)
        (string-to-number (substring hex 5 7) 16)))

(defun financial-chart-3d-rates--ramp (colors u)
  "The colour at U (0 to 1) along the hex COLORS, evenly spaced."
  (let* ((n (1- (length colors)))
         (u (max 0.0 (min 1.0 (float u))))
         (i (min (1- n) (floor (* u n))))
         (f (- (* u n) i))
         (a (financial-chart-3d-rates--rgb (nth i colors)))
         (b (financial-chart-3d-rates--rgb (nth (1+ i) colors))))
    (financial-chart-3d-rates--hex (cl-mapcar (lambda (x y) (+ x (* f (- y x)))) a b))))

(defconst financial-chart-3d-rates-level-colors '("#2c7bb6" "#abd9e9" "#fdae61" "#d7191c")
  "Low to high: blue, pale blue, orange, red.")

(defun financial-chart-3d-rates--spread (n half)
  "N positions evenly from -HALF to HALF (one at 0)."
  (if (> n 1)
      (cl-loop for i from 0 below n collect (financial-chart-3d--r3 (- (* 2 half (/ i (float (1- n)))) half)))
    (list 0.0)))

(defun financial-chart-3d-rates--mesh (id xs zs heights colors)
  "A mesh ID over world XS by ZS, HEIGHTS a list of rows (one per z) of y.
COLORS is a function of (I J), the cell from column I and row J, to
its colour; both triangles of a cell take it.  A cell with a nil
corner is left out."
  (let ((nx (length xs)) vertices triangles tcolors (index 0) (ids (make-hash-table :test #'equal)))
    (cl-loop for z in zs for row in heights for j from 0
             do (cl-loop for x in xs for y in row for i from 0
                         when (numberp y)
                         do (push (vector x (financial-chart-3d--r3 y) z) vertices)
                         (puthash (cons i j) index ids)
                         (cl-incf index)))
    (dotimes (j (1- (length zs)))
      (dotimes (i (1- nx))
        (let ((a (gethash (cons i j) ids)) (b (gethash (cons (1+ i) j) ids))
              (c (gethash (cons i (1+ j)) ids)) (d (gethash (cons (1+ i) (1+ j)) ids)))
          (when (and a b c d)
            (let ((color (funcall colors i j)))
              (push (vector a b c) triangles) (push color tcolors)
              (push (vector b d c) triangles) (push color tcolors))))))
    (list :id id :type "mesh" :vertices (vconcat (nreverse vertices))
          :triangles (vconcat (nreverse triangles)) :colors (vconcat (nreverse tcolors)))))

(defun financial-chart-3d-rates--polyline (xs ys z)
  "Segments joining XS at heights YS (nil breaks the line), at depth Z."
  (cl-loop for (x0 x1) on xs for (y0 y1) on ys while x1
           when (and (numberp y0) (numberp y1))
           collect (vector x0 (financial-chart-3d--r3 y0) z x1 (financial-chart-3d--r3 y1) z)))

(defun financial-chart-3d-rates--floor-grid (xs zs color)
  "The floor's outline under XS by ZS, in COLOR."
  (let ((x0 (car xs)) (x1 (car (last xs))) (z0 (car zs)) (z1 (car (last zs))))
    (list :id "floor" :type "segments" :color color
          :segments (vector (vector x0 0 z0 x1 0 z0) (vector x1 0 z0 x1 0 z1)
                            (vector x1 0 z1 x0 0 z1) (vector x0 0 z1 x0 0 z0)))))

;;; 3D: curves over date (yield curves, CDS term structures)

(defun financial-chart-3d-rates--curve-tip (z a b yk)
  "Tooltip of the curve of date Z from row A to row B, YK the level drawn."
  (financial-chart-3d-tip
   (format "%s  %s to %s" z
           (or (financial-chart-3d--value a :tenor_label) (financial-chart-3d-num (financial-chart-3d--value a :tenor)))
           (or (financial-chart-3d--value b :tenor_label) (financial-chart-3d-num (financial-chart-3d--value b :tenor))))
   (format "%s %s to %s" (substring (symbol-name yk) 1)
           (financial-chart-3d-num (financial-chart-3d--value a yk) 3)
           (financial-chart-3d-num (financial-chart-3d--value b yk) 3))
   (let ((regime (financial-chart-3d--value b :regime)))
     (and regime (format "regime %s" regime)))))

(defun financial-chart-3d-rates--term-surface (graphics rows &optional tip)
  "Primitives of a surface of curves: GRAPHICS over flat ROWS.
Channels x (tenor, evenly spaced by rank), z (the curve's date, oldest
at the back) and y (the level, from 0 up); `color' a field naming each
curve's regime (colours `colors'), else the surface is coloured by
level.  Under a time encoding the scales span every row, the curves
so far are drawn and the newest is traced bright; each earlier curve
stays as a trail.  Rows whose `kind' is \"forward\" are drawn as a
line just in front of their date's curve, not in the surface.
Each segment of a curve has a tooltip: TIP of its date and the rows it
joins, and of the level field (default `financial-chart-3d-rates--curve-tip')."
  (let* ((tip (or tip #'financial-chart-3d-rates--curve-tip))
         (xk (financial-chart-3d--field graphics "x" "tenor"))
         (zk (financial-chart-3d--field graphics "z" "date"))
         (yk (financial-chart-3d--field graphics "y" "yield"))
         (ck (let ((spec (plist-get (financial-chart-3d--opt graphics "encoding") :color)))
               (and spec (financial-chart-3d--field graphics "color" nil))))
         (spot (lambda (r) (and (not (equal (financial-chart-3d--value r :kind) "forward"))
                                (not (equal (financial-chart-3d--value r :row_kind) "bucket")))))
         (dom (financial-chart-3d--domain rows))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk))
                                                     (seq-filter spot dom))))
         (numeric-axis (seq-some (lambda (r) (equal (financial-chart-3d--value r :row_kind) "tenor")) dom))
         (zs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom)))
         (ys (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r yk))) (and (numberp v) (float v)))) dom)))
         (lo (min 0.0 (if ys (apply #'min ys) 0.0)))
         (hi (if ys (apply #'max ys) 1.0))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (if (> hi lo) (/ height (- hi lo)) 1.0))
         (extent (financial-chart-3d--opt graphics "extent" [12 9]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (wx (if numeric-axis
                 (let ((hi-x (max 1e-9 (apply #'max xs))))
                   (mapcar (lambda (x) (- (* 2 w (/ x hi-x)) w)) xs))
               (financial-chart-3d-rates--spread (length xs) w)))
         (wz (financial-chart-3d-rates--spread (length zs) d))
         (z-of (let ((h (make-hash-table :test #'equal)))
                 (cl-loop for z in zs for p in wz do (puthash z p h)) h))
         (y-of (lambda (v) (* scale (- v lo))))
         (cells (make-hash-table :test #'equal))
         (cell-rows (make-hash-table :test #'equal))
         (forward (make-hash-table :test #'equal))
         (regime (make-hash-table :test #'equal))
         (colors (financial-chart-3d--opt graphics "colors"))
         (fill (eq (financial-chart-3d--opt graphics "fill" :false) t))
         (ribs (eq (financial-chart-3d--opt graphics "ribs" :false) t))
         present)
    (dolist (r rows)
      (let ((x (financial-chart-3d--value r xk)) (z (financial-chart-3d--value r zk))
            (y (financial-chart-3d--value r yk)))
        (when (and x z (numberp y))
          (if (funcall spot r)
              (progn (puthash (cons x z) (float y) cells)
                     (puthash (cons x z) r cell-rows)
                     (when ck (puthash z (financial-chart-3d--value r ck) regime))
                     (cl-pushnew z present :test #'equal))
            (push (cons x (float y)) (gethash z forward))))))
    (let* ((shown (seq-filter (lambda (z) (member z present)) zs))
           (newest (car (last shown)))
           (heights (lambda (z) (mapcar (lambda (x) (let ((v (gethash (cons x z) cells))) (and v (funcall y-of v)))) xs)))
           (curve-color (lambda (z)
                          (if ck
                              (let ((name (gethash z regime)))
                                (or (and colors name (financial-chart-3d--opt colors name))
                                    (cdr (assoc name financial-chart-3d-rates-regime-colors))
                                    "#8a94a6"))
                            (financial-chart-3d-rates--ramp
                             financial-chart-3d-rates-level-colors
                             (let ((vs (delq nil (mapcar (lambda (x) (gethash (cons x z) cells)) xs))))
                               (if (and vs (> hi lo)) (/ (- (/ (apply #'+ vs) (length vs)) lo) (- hi lo)) 0.5))))))
           (cell-color (lambda (i j)
                         (let ((z (nth j shown)))
                           (if ck (funcall curve-color z)
                             (let* ((vs (delq nil (list (gethash (cons (nth i xs) z) cells)
                                                        (gethash (cons (nth (1+ i) xs) z) cells)
                                                        (gethash (cons (nth i xs) (nth (1+ j) shown)) cells)
                                                        (gethash (cons (nth (1+ i) xs) (nth (1+ j) shown)) cells))))
                                    (m (if vs (/ (apply #'+ vs) (length vs)) lo)))
                               (financial-chart-3d-rates--ramp financial-chart-3d-rates-level-colors
                                                               (if (> hi lo) (/ (- m lo) (- hi lo)) 0.5)))))))
           ;; One per segment `financial-chart-3d-rates--polyline' draws.
           (tips (lambda (z) (cl-loop for (x0 x1) on xs while x1
                                      for a = (gethash (cons x0 z) cell-rows)
                                      for b = (gethash (cons x1 z) cell-rows)
                                      when (and a b) collect (funcall tip z a b yk))))
           (trail nil) (trail-colors nil) (trail-tips nil))
      (dolist (z shown)
        (unless (equal z newest)
          (dolist (s (financial-chart-3d-rates--polyline wx (funcall heights z) (gethash z z-of)))
            (push s trail)
            (push (funcall curve-color z) trail-colors))
          (setq trail-tips (append (reverse (funcall tips z)) trail-tips))))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (financial-chart-3d-rates--floor-grid wx wz "#3b4252")
       (if (and fill (>= (length xs) 2) (>= (length shown) 2))
           (financial-chart-3d-rates--mesh "surface" wx (mapcar (lambda (z) (gethash z z-of)) shown)
                                           (mapcar heights shown) cell-color)
         (list :id "surface" :type "segments" :segments []))
       (list :id "ribs" :type "segments" :color (financial-chart-3d--opt graphics "rib_color" "#5b6b86")
             :segments (if (and ribs (>= (length shown) 2))
                           (vconcat
                            (cl-loop for x in xs for xw in wx
                                     append (cl-loop for (z0 z1) on shown while z1
                                                     for y0 = (gethash (cons x z0) cells)
                                                     for y1 = (gethash (cons x z1) cells)
                                                     when (and y0 y1)
                                                     collect (vector xw (financial-chart-3d--r3 (funcall y-of y0)) (gethash z0 z-of)
                                                                     xw (financial-chart-3d--r3 (funcall y-of y1)) (gethash z1 z-of)))))
                         []))
       (list :id "trail" :type "segments" :segments (vconcat (nreverse trail))
             :colors (vconcat (nreverse trail-colors)) :tooltips (vconcat (nreverse trail-tips)))
       (list :id "front" :type "segments" :color "#ffffff"
             :segments (if newest
                           (vconcat (financial-chart-3d-rates--polyline wx (funcall heights newest)
                                                                        (gethash newest z-of)))
                         [])
             :tooltips (if newest (vconcat (funcall tips newest)) []))
       (list :id "forward" :type "segments"
             :color (financial-chart-3d--opt graphics "forward_color" "#ffb74d")
             :segments (let ((pts (and newest (sort (copy-sequence (gethash newest forward))
                                                    (lambda (a b) (< (car a) (car b)))))))
                         (if (and pts xs (numberp (car xs)))
                             ;; Forward tenors sit where the spot axis would put them,
                             ;; linear between its ranked tenors.
                             (let ((fx (mapcar (lambda (p) (financial-chart-3d-rates--interp xs wx (car p))) pts)))
                               (vconcat (financial-chart-3d-rates--polyline
                                         fx (mapcar (lambda (p) (funcall y-of (cdr p))) pts)
                                         (financial-chart-3d--r3 (+ (gethash newest z-of)
                                                                    (/ d (max 1 (length zs))))))))
                           [])))))))

(defun financial-chart-3d-rates--interp (xs ws x)
  "World position of X along sorted numbers XS placed at WS, linear between them."
  (cond ((<= x (car xs)) (car ws))
        ((>= x (car (last xs))) (car (last ws)))
        (t (cl-loop for (a b) on xs for (wa wb) on ws while b
                    when (<= a x b)
                    return (financial-chart-3d--r3 (+ wa (* (- wb wa) (/ (- x a) (float (- b a))))))))))

;;; 3D: credit spread surface

(defun financial-chart-3d-rates--credit-mark (graphics rows)
  "Primitives of the fc-credit3d mark: GRAPHICS over credit-spread ROWS.
A surface over tenor (x, evenly by rank) and rating (z, best in front),
height the spread (`scale' \"log\", the default, or \"linear\"), each
rating's curve traced; cells are coloured by their spread's
percentile in its history (green tight to red wide) when the rows
carry pctile, else by spread."
  (let* ((rows (financial-chart-3d-rates-spread-rows rows))
         (xk (financial-chart-3d--field graphics "x" "tenor"))
         (zk (financial-chart-3d--field graphics "z" "rating"))
         (yk (financial-chart-3d--field graphics "y" "spread_bps"))
         (ck (financial-chart-3d--field graphics "color" "pctile"))
         (log (equal (financial-chart-3d--opt graphics "scale" "log") "log"))
         (f (lambda (v) (if log (log (max v 1.0) 10) v)))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) rows)))
         (zs (let ((seen nil))
               (dolist (r (sort (copy-sequence rows)
                                (lambda (a b) (> (financial-chart-3d-rates-notch (financial-chart-3d--value a zk))
                                                 (financial-chart-3d-rates-notch (financial-chart-3d--value b zk))))))
                 (cl-pushnew (financial-chart-3d--value r zk) seen :test #'equal))
               (nreverse seen)))
         (vals (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r yk))) (and (numberp v) (funcall f v)))) rows)))
         (lo (if log (if vals (floor (apply #'min vals)) 0) 0.0))
         (hi (if vals (apply #'max vals) 1.0))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (if (> hi lo) (/ height (- hi lo)) 1.0))
         (extent (financial-chart-3d--opt graphics "extent" [12 8]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (wx (financial-chart-3d-rates--spread (length xs) w))
         ;; Best rating in front (largest z), worst at the back.
         (wz (reverse (financial-chart-3d-rates--spread (length zs) d)))
         (cells (make-hash-table :test #'equal))
         (pct (make-hash-table :test #'equal))
         (fill (eq (financial-chart-3d--opt graphics "fill" :false) t))
         (ribs (not (eq (financial-chart-3d--opt graphics "ribs" t) :false)))
         (cell-rows (make-hash-table :test #'equal))
         (any-pct nil))
    (dolist (r rows)
      (let ((x (financial-chart-3d--value r xk)) (z (financial-chart-3d--value r zk))
            (y (financial-chart-3d--value r yk)) (p (financial-chart-3d--value r ck)))
        (when (and x z (numberp y))
          (puthash (cons x z) (* scale (- (funcall f y) lo)) cells)
          (puthash (cons x z) r cell-rows)
          (when (numberp p) (setq any-pct t) (puthash (cons x z) p pct)))))
    (let* ((heights (mapcar (lambda (z) (mapcar (lambda (x) (gethash (cons x z) cells)) xs)) zs))
           (ramp '("#1a9850" "#a6d96a" "#fee08b" "#f46d43" "#d73027"))
           ;; The colour of the cells KEYS: their mean percentile, else spread.
           (color-of
            (lambda (keys)
              (let* ((vs (delq nil (mapcar (lambda (k) (gethash k (if any-pct pct cells))) keys)))
                     (m (if vs (/ (apply #'+ vs) (float (length vs))) 0)))
                (financial-chart-3d-rates--ramp ramp (if any-pct (/ m 100.0) (/ m height))))))
           (cell-color
            (lambda (i j)
              (funcall color-of (list (cons (nth i xs) (nth j zs)) (cons (nth (1+ i) xs) (nth j zs))
                                      (cons (nth i xs) (nth (1+ j) zs)) (cons (nth (1+ i) xs) (nth (1+ j) zs))))))
           ;; Each rating's curve, a segment per tenor step in its own colour.
           (lines (cl-loop for z in zs for zw in wz for row in heights
                           append (cl-loop for (x0 x1) on wx for (y0 y1) on row for i from 0 while x1
                                           when (and (numberp y0) (numberp y1))
                                           collect (list (vector x0 (financial-chart-3d--r3 y0) zw
                                                                 x1 (financial-chart-3d--r3 y1) zw)
                                                         (funcall color-of (list (cons (nth i xs) z)
                                                                                 (cons (nth (1+ i) xs) z)))
                                                         (let ((a (gethash (cons (nth i xs) z) cell-rows))
                                                               (b (gethash (cons (nth (1+ i) xs) z) cell-rows))
                                                               (label (lambda (r) (or (financial-chart-3d--value r :tenor_label)
                                                                                      (financial-chart-3d-num (financial-chart-3d--value r xk))))))
                                                           (financial-chart-3d-tip
                                                            (format "%s  %s to %s" z (funcall label a) (funcall label b))
                                                            (format "spread %s to %s bp" (financial-chart-3d-num (financial-chart-3d--value a yk) 1)
                                                                    (financial-chart-3d-num (financial-chart-3d--value b yk) 1))
                                                            (and (financial-chart-3d--value b ck)
                                                                 (format "percentile %s to %s" (financial-chart-3d-num (financial-chart-3d--value a ck) 0)
                                                                         (financial-chart-3d-num (financial-chart-3d--value b ck) 0)))))))))
           (ribs-segments (cl-loop for x in xs for xw in wx
                                   append (cl-loop for (z0 z1) on zs for (w0 w1) on wz while z1
                                                   for y0 = (gethash (cons x z0) cells)
                                                   for y1 = (gethash (cons x z1) cells)
                                                   when (and y0 y1)
                                                   collect (vector xw (financial-chart-3d--r3 y0) w0
                                                                   xw (financial-chart-3d--r3 y1) w1)))))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (financial-chart-3d-rates--floor-grid wx wz "#3b4252")
       (if (and fill (>= (length xs) 2) (>= (length zs) 2))
           (financial-chart-3d-rates--mesh "surface" wx wz heights cell-color)
         (list :id "surface" :type "segments" :segments []))
       (list :id "curves" :type "segments" :segments (vconcat (mapcar #'car lines))
             :colors (vconcat (mapcar #'cadr lines)) :tooltips (vconcat (mapcar #'caddr lines)))
       (list :id "ribs" :type "segments" :color (financial-chart-3d--opt graphics "rib_color" "#5b6b86")
             :segments (if ribs (vconcat ribs-segments) []))))))

;;; 3D: key-rate duration bars

(defun financial-chart-3d-rates--krd-mark (graphics rows)
  "Primitives of the fc-krd3d mark: GRAPHICS over key-rate duration ROWS.
A box per holding (z, in order, first at the back) and key rate (x,
by tenor) as tall as its duration, up for long and down for short
exposure, each holding its own colour; with `total' (default true) a
last column, set apart, is each holding's whole duration."
  (let* ((rows (financial-chart-3d-rates-krd-rows rows))
         (xk (financial-chart-3d--field graphics "x" "tenor"))
         (zk (financial-chart-3d--field graphics "z" "holding"))
         (yk (financial-chart-3d--field graphics "y" "krd"))
         (total (eq (financial-chart-3d--opt graphics "total" :false) t))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) rows)))
         (zs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zk)) rows)))
         (durations (mapcar (lambda (z) (cons z (apply #'+ (mapcar (lambda (r) (if (equal (financial-chart-3d--value r zk) z)
                                                                                     (or (financial-chart-3d--value r yk) 0) 0))
                                                                     rows))))
                            zs))
         (top (apply #'max 1e-9 (mapcar (lambda (r) (abs (or (financial-chart-3d--value r yk) 0))) rows)))
         (top (if total (max top (apply #'max 0 (mapcar (lambda (c) (abs (cdr c))) durations))) top))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (/ height top))
         (band (/ (float (financial-chart-3d--opt graphics "band" 0.6)) 2))
         (nx (+ (length xs) (if total 1.5 0)))
         (x0 (/ (1- nx) 2.0)) (z0 (/ (1- (length zs)) 2.0))
         (palette (financial-chart-3d--opt graphics "colors"
                                           ["#4c78a8" "#f58518" "#54a24b" "#e45756" "#72b7b2" "#eeca3b"
                                            "#b279a2" "#ff9da6" "#9d755d" "#bab0ac"]))
         (color-of (lambda (z) (elt palette (mod (seq-position zs z) (length palette)))))
         (box (lambda (x z v)
                (let ((y (financial-chart-3d--r3 (* scale v))))
                  (vector (financial-chart-3d--r3 (- x band)) (min 0 y) (financial-chart-3d--r3 (- z band))
                          (financial-chart-3d--r3 (+ x band)) (max 0 y) (financial-chart-3d--r3 (+ z band))))))
         boxes colors tips)
    (dolist (r rows)
      (let ((i (seq-position xs (financial-chart-3d--value r xk)))
            (j (seq-position zs (financial-chart-3d--value r zk)))
            (v (financial-chart-3d--value r yk)))
        (when (and i j (numberp v) (/= v 0))
          (push (funcall box (- i x0) (- j z0) v) boxes)
          (push (funcall color-of (financial-chart-3d--value r zk)) colors)
          (push (financial-chart-3d-tip
                 (format "%s  %s" (financial-chart-3d-num (financial-chart-3d--value r zk))
                         (or (financial-chart-3d--value r :tenor_label) (financial-chart-3d-num (financial-chart-3d--value r xk))))
                 (format "key-rate duration %s" (financial-chart-3d-num v 3))
                 (let ((dv01 (financial-chart-3d--value r :dv01)))
                   (and (numberp dv01) (format "DV01 %s" (financial-chart-3d-num dv01 2))))
                 (let ((dur (financial-chart-3d--value r :duration)))
                   (and (numberp dur) (format "holding duration %s" (financial-chart-3d-num dur 3)))))
                tips))))
    (when total
      (cl-loop for (z . dur) in durations for j from 0
               when (/= dur 0)
               do (push (funcall box (- (+ (length xs) 0.5) x0) (- j z0) dur) boxes)
               (push (financial-chart-3d-rates--ramp (list (funcall color-of z) "#ffffff") 0.45) colors)
               (push (financial-chart-3d-tip (format "%s  total" z)
                                             (format "duration %s (sum of key rates)" (financial-chart-3d-num dur 3)))
                     tips)))
    (let* ((left (- -0.5 x0)) (right (+ 0.5 x0)) (back (- -0.5 z0)) (front (+ 0.5 z0))
           (grid (append (cl-loop for i from 0 below (length xs)
                                  collect (vector (financial-chart-3d--r3 (- i x0)) 0 back
                                                  (financial-chart-3d--r3 (- i x0)) 0 front))
                         (cl-loop for j from 0 below (length zs)
                                  collect (vector left 0 (financial-chart-3d--r3 (- j z0))
                                                  right 0 (financial-chart-3d--r3 (- j z0)))))))
      (vector
       (financial-chart-3d--axes "axes" left right height back front)
       (list :id "grid" :type "segments" :color "#3b4252" :segments (vconcat grid))
       (list :id "bars" :type "boxes" :boxes (vconcat (nreverse boxes)) :colors (vconcat (nreverse colors))
             :tooltips (vconcat (nreverse tips)))))))

(defun financial-chart-3d-rates--pct (v &optional digits)
  "Decimal V as a percent, to DIGITS decimals (default 2); \"-\" for none."
  (if (numberp v) (format (format "%%.%df%%%%" (or digits 2)) (* 100 v)) "-"))

(defun financial-chart-3d-rates--bond-tip (row)
  "Tooltip of a CDS implied ladder bond ROW: never its run's summary."
  (let ((flags (delq nil (mapcar (lambda (k)
                                   (let ((v (financial-chart-3d--value row k)))
                                     (and v (format "%s %s" (if (eq k :spread_extrapolated) "spread" "treasury") v))))
                                 '(:spread_extrapolated :treasury_extrapolated)))))
    (financial-chart-3d-tip
     (format "%s  matures %s  coupon %s" (financial-chart-3d--value row :name)
             (financial-chart-3d-num (financial-chart-3d--value row :maturity))
             (financial-chart-3d-rates--pct (financial-chart-3d--value row :coupon)))
     (format "model price %s  yield %s  CDS %s bp"
             (financial-chart-3d-num (financial-chart-3d--value row :model_price) 3)
             (financial-chart-3d-rates--pct (financial-chart-3d--value row :model_yield) 3)
             (financial-chart-3d-num (financial-chart-3d--value row :cds_spread_bp) 1))
     (format "mod. duration %s  DV01 $%s  expected loss $%s"
             (financial-chart-3d-num (financial-chart-3d--value row :modified_duration) 3)
             (financial-chart-3d-num (financial-chart-3d--value row :dv01_usd) 0)
             (financial-chart-3d-num (financial-chart-3d--value row :expected_loss_usd) 0))
     (if flags (concat "extrapolated: " (string-join flags ", ")) "extrapolated: none"))))

(defun financial-chart-3d-rates--cds-source (row)
  "The source line of CDS ROW: where its numbers come from and its last print."
  (let ((source (or (financial-chart-3d--value row :source) (financial-chart-3d--value row :basis)))
        (date (financial-chart-3d--value row :last_trade_date))
        (trades (financial-chart-3d--value row :trades_30d)))
    (string-join (delq nil (list (and source (format "source %s" source))
                                 (and date (format "last print %s" date))
                                 (and (numberp trades) (format "%s trades/30d" trades))
                                 (and (eq (financial-chart-3d--value row :thin) t) "thin")))
                 ", ")))

(defun financial-chart-3d-rates--cds-node-tip (row)
  "Tooltip of a CDS tenor or forward bucket ROW."
  (if (equal (financial-chart-3d--value row :row_kind) "bucket")
      (financial-chart-3d-tip
       (format "%s  forward %s" (financial-chart-3d-num (financial-chart-3d--value row :as_of))
               (financial-chart-3d-num (financial-chart-3d--value row :label)))
       (format "forward hazard %s bp" (let ((h (financial-chart-3d--value row :forward_hazard)))
                                        (financial-chart-3d-num (and (numberp h) (* 10000 h)) 1)))
       (format "conditional PD %s" (financial-chart-3d-rates--pct (financial-chart-3d--value row :conditional_pd)))
       (string-join (delq nil (list (and (eq (financial-chart-3d--value row :thin) t) "thin")
                                    (and (eq (financial-chart-3d--value row :floored) t) "floored")))
                    ", "))
    (financial-chart-3d-tip
     (string-join (delq nil (list (financial-chart-3d--value row :reference_entity)
                                  (financial-chart-3d-num (financial-chart-3d--value row :as_of))
                                  (financial-chart-3d--value row :tenor_label)
                                  (let ((m (financial-chart-3d--value row :maturity))) (and m (format "(%s)" m)))))
                  "  ")
     (format "spread %s bp  hazard %s bp"
             (financial-chart-3d-num (financial-chart-3d--value row :spread_bp) 1)
             (financial-chart-3d-num (financial-chart-3d--value row :hazard_bps) 1))
     (format "PD %s%%" (financial-chart-3d-num (financial-chart-3d--value row :pd_pct)))
     (financial-chart-3d-rates--cds-source row))))

(defun financial-chart-3d-rates--cds-curve-tip (z a b _yk)
  "Tooltip of the CDS curve of date Z from tenor row A to tenor row B."
  (financial-chart-3d-tip
   (string-join (delq nil (list (financial-chart-3d--value b :reference_entity)
                                (format "%s  %s to %s" z (financial-chart-3d--value a :tenor_label)
                                        (financial-chart-3d--value b :tenor_label))))
                "  ")
   (format "spread %s to %s bp" (financial-chart-3d-num (financial-chart-3d--value a :spread_bp) 1)
           (financial-chart-3d-num (financial-chart-3d--value b :spread_bp) 1))
   (format "PD %s%% to %s%%" (financial-chart-3d-num (financial-chart-3d--value a :pd_pct))
           (financial-chart-3d-num (financial-chart-3d--value b :pd_pct)))
   (financial-chart-3d-rates--cds-source b)))

(defun financial-chart-3d-rates--cds-bond-mark (graphics rows)
  "CDS implied bond ladder bars, treasury ribbon and per-bond tooltips."
  (let* ((rows (financial-chart-3d-rates-cds-bond-rows rows))
         (xfield (financial-chart-3d--field graphics "x" "years"))
         (yfield (financial-chart-3d--field graphics "y" "model_yield"))
         (zfield (financial-chart-3d--field graphics "z" "name"))
         (colorfield (financial-chart-3d--field graphics "color" "spread_extrapolated"))
         (names (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zfield)) rows)))
         (maxx (max 1.0 (apply #'max (mapcar (lambda (r) (financial-chart-3d--value r xfield)) rows))))
         (maxy (max 1e-9 (apply #'max (mapcar (lambda (r) (abs (financial-chart-3d--value r yfield))) rows))))
         (height (float (financial-chart-3d--opt graphics "height" 8)))
         (extent (financial-chart-3d--opt graphics "extent" [14 10]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (half (/ (float (financial-chart-3d--opt graphics "band" 0.55)) 2))
         boxes colors tips ribbon)
    (dolist (row rows)
      (let* ((years (float (financial-chart-3d--value row xfield)))
             (z (- (* d (/ (or (seq-position names (financial-chart-3d--value row zfield)) 0)
                              (float (max 1 (1- (length names)))))) d))
             (value (float (financial-chart-3d--value row yfield)))
             (y (* height (/ value maxy)))
             (color (pcase (financial-chart-3d--value row colorfield)
                      ((or "below" "null" :null) "#e6ab02")
                      ("above" "#d95f59") (_ "#4c9f70"))))
        (push (vector (financial-chart-3d--r3 (- (- (* 2 w (/ years maxx)) w) half)) 0
                      (financial-chart-3d--r3 (- z half))
                      (financial-chart-3d--r3 (+ (- (* 2 w (/ years maxx)) w) half))
                      (financial-chart-3d--r3 y) (financial-chart-3d--r3 (+ z half))) boxes)
        (push color colors)
        (push (financial-chart-3d-rates--bond-tip row) tips)
        (let ((ty (financial-chart-3d--value row :treasury_yield)))
          (when (numberp ty)
            (push (cons (- (* 2 w (/ years maxx)) w) (* height (/ ty maxy))) ribbon)))))
    (let ((points (nreverse ribbon)))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (financial-chart-3d-rates--floor-grid (list (- w) w) (list (- d) d) "#3b4252")
       (list :id "bars" :type "boxes" :boxes (vconcat (nreverse boxes))
             :colors (vconcat (nreverse colors)) :tooltips (vconcat (nreverse tips)))
       (list :id "treasury-ribbon" :type "segments" :color "#9ecae9"
             :segments (vconcat (financial-chart-3d-rates--polyline
                                 (mapcar #'car points) (mapcar #'cdr points) (- d))))))))

;;; Registration

(defun financial-chart-3d-rates--term-replay (graphics rows)
  "The term surface replayed date by date (`financial-chart-3d--replay')."
  (financial-chart-3d--replay graphics rows nil #'financial-chart-3d-rates--term-surface))

(defun financial-chart-3d-rates--cds-term-geometry (graphics rows)
  "Existing term surface plus bucket slabs and dimmed, tooltipped tenor nodes.
Every segment of the curves has a tooltip too
\(`financial-chart-3d-rates--cds-curve-tip'), so a history of ISDA
outputs, which has no tenor nodes, hovers as well.  The nodes and slabs
sit on the surface's dates and tenors."
  (let* ((base (append (financial-chart-3d-rates--term-surface graphics rows
                                                               #'financial-chart-3d-rates--cds-curve-tip)
                       nil))
         (buckets (seq-filter (lambda (r) (equal (financial-chart-3d--value r :row_kind) "bucket")) rows))
         (tenors (seq-filter (lambda (r) (equal (financial-chart-3d--value r :row_kind) "tenor")) rows))
         (dom (financial-chart-3d--domain rows))
         (zk (financial-chart-3d--field graphics "z" "as_of"))
         (dates (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom)))
         (z-of (let ((h (make-hash-table :test #'equal)))
                 (cl-loop for date in dates
                          for z in (financial-chart-3d-rates--spread (length dates) (/ (float (elt (financial-chart-3d--opt graphics "extent" [12 12]) 1)) 2))
                          do (puthash date z h))
                 h))
         (max-tenor (apply #'max
                           (cons 1e-9
                                 (delq nil (mapcar (lambda (r) (and (equal (financial-chart-3d--value r :row_kind) "tenor")
                                                                    (financial-chart-3d--value r :tenor_years)))
                                                   dom)))))
         (extent (financial-chart-3d--opt graphics "extent" [12 12]))
         (w (/ (float (elt extent 0)) 2))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (maxy (apply #'max
                      (cons 1e-9 (mapcar (lambda (r) (abs (or (financial-chart-3d--value r :value) 0))) dom))))
         boxes colors tips nodes node-colors node-tips)
    (dolist (row buckets)
      (let* ((from (financial-chart-3d--value row :from_years))
             (to (financial-chart-3d--value row :to_years))
             (z (gethash (financial-chart-3d--value row zk) z-of 0.0))
             (value (or (financial-chart-3d--value row :forward_hazard)
                        (financial-chart-3d--value row :value) 0))
             (top (* height (/ value maxy)))
             (x0 (- (* 2 w (/ from max-tenor)) w))
             (x1 (- (* 2 w (/ to max-tenor)) w)))
        (push (vector (financial-chart-3d--r3 x0) 0 (financial-chart-3d--r3 (- z 0.18))
                      (financial-chart-3d--r3 x1) (financial-chart-3d--r3 top)
                      (financial-chart-3d--r3 (+ z 0.18))) boxes)
        (push (if (eq (financial-chart-3d--value row :thin) t) "#657080" "#69a9d3") colors)
        (push (financial-chart-3d-rates--cds-node-tip row) tips)))
    (dolist (row tenors)
      (let* ((tenor (financial-chart-3d--value row :tenor_years))
             (z (gethash (financial-chart-3d--value row zk) z-of 0.0))
             (value (or (financial-chart-3d--value row :value) 0))
             (x (- (* 2 w (/ tenor max-tenor)) w)) (y (* height (/ value maxy))))
        (push (vector (financial-chart-3d--r3 (- x 0.10)) (financial-chart-3d--r3 (- y 0.10)) (financial-chart-3d--r3 (- z 0.10))
                      (financial-chart-3d--r3 (+ x 0.10)) (financial-chart-3d--r3 (+ y 0.10)) (financial-chart-3d--r3 (+ z 0.10)))
              nodes)
        (push (if (eq (financial-chart-3d--value row :thin) t) "#596273" "#ffffff") node-colors)
        (push (financial-chart-3d-rates--cds-node-tip row) node-tips)))
    (vconcat base
             (list (list :id "forward-buckets" :type "boxes" :boxes (vconcat (nreverse boxes))
                         :colors (vconcat (nreverse colors)) :tooltips (vconcat (nreverse tips)))
                   (list :id "tenor-nodes" :type "boxes" :boxes (vconcat (nreverse nodes))
                         :colors (vconcat (nreverse node-colors)) :tooltips (vconcat (nreverse node-tips)))))))

(defun financial-chart-3d-rates--cds-term-replay (graphics rows)
  "CDS term replay including per-node tooltips and forward hazard buckets."
  (financial-chart-3d--replay graphics rows nil #'financial-chart-3d-rates--cds-term-geometry))

(defconst financial-chart-3d-rates-marks
  '(("fc-yield-curve3d" . financial-chart-3d-rates--term-replay)
    ("fc-cds-term3d" . financial-chart-3d-rates--cds-term-replay)
    ("fc-credit3d" . financial-chart-3d-rates--credit-mark)
    ("fc-krd3d" . financial-chart-3d-rates--krd-mark)
    ("fc-cds-bond-ladder3d" . financial-chart-3d-rates--cds-bond-mark))
  "The rates and credit 3D marks: (MARK . FUNCTION).")

(defun financial-chart-3d-rates--register-marks ()
  "Register `financial-chart-3d-rates-marks' with graphics.el."
  (dolist (m financial-chart-3d-rates-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; Now if graphics.el's marks are loaded, else when they load.
(with-eval-after-load 'graphics-eas-marks
  (financial-chart-3d-rates--register-marks))

;;; From invest metric outputs to bindings

(declare-function financial-chart-3d-metric-entries "financial-chart-3d" (output))
(declare-function financial-chart-3d-metric-stamp "financial-chart-3d" (entry args keys &optional default))

(defun financial-chart-3d-rates--entry (e key)
  "Entry E as a history entry: its fields but :output, the output at KEY."
  (append (cl-loop for (k v) on e by #'cddr unless (eq k :output) append (list k v))
          (list key (plist-get e :output))))

(defun financial-chart-3d-rates--dated (output args key)
  "OUTPUT's entries as history entries {as_of, KEY: output, ...}.
A single output is dated by ARGS's :as-of; other fields of an entry
\(curve_regime, forward_path, rolldown_carry beside an nss_fit) stay."
  (cl-loop for e in (financial-chart-3d-metric-entries output) for i from 0
           for date = (or (financial-chart-3d-metric-stamp e args '(:as_of :date :time))
                          (financial-chart--invalid i "as_of" "missing_date" "each curve needs its date (as_of)"))
           collect (append (list :as_of date) (financial-chart-3d-rates--entry e key))))

(defun financial-chart-3d-rates--from-nss-fit (_template output args)
  "yield-curve bindings from fi.nss_fit OUTPUT by date (entries {as_of, output})."
  (list :data (vconcat (financial-chart-3d-rates-curve-rows
                        (financial-chart-3d-rates--dated output args :nss_fit)))))

(defun financial-chart-3d-rates--from-curve-regime (_template output args)
  "yield-curve bindings from fi.curve_regime OUTPUT by date.
The output echoes the curve it classified (inputs.curve_years and
curve_rates); its value names the regime."
  (list :data (vconcat (financial-chart-3d-rates-curve-rows
                        (mapcar (lambda (e)
                                  (let ((inputs (financial-chart-3d-rates--get e :curve_regime :inputs)))
                                    (append e (list :curve_years (financial-chart-3d--value inputs :curve_years)
                                                    :curve_rates (financial-chart-3d--value inputs :curve_rates)))))
                                (financial-chart-3d-rates--dated output args :curve_regime))))))

(defun financial-chart-3d-rates--from-spreads (_template output _args)
  "credit-spreads bindings from fi.spread_suite / fi.spread_pctile OUTPUT.
OUTPUT is one output, a list of them (both metrics may mix), or entries
{output}; each output's inputs name its rating and tenor."
  (list :data (vconcat (financial-chart-3d-rates-spread-rows
                        (let ((entries (financial-chart-3d-metric-entries output)))
                          (if (and (= (length entries) 1)
                                   (not (keywordp (car-safe (plist-get (car entries) :output)))))
                              (plist-get (car entries) :output)
                            (mapcar (lambda (e) (plist-get e :output)) entries)))))))

(defun financial-chart-3d-rates--from-krd (_template output args)
  "key-rate-durations bindings from fi.krd OUTPUT, one entry per holding.
Each entry {holding, output, dv01?, tenors?}; a single output takes
ARGS's :holding."
  (list :data (vconcat (financial-chart-3d-rates-krd-rows
                        (cl-loop for e in (financial-chart-3d-metric-entries output)
                                 collect (append (list :holding (financial-chart-3d-metric-stamp e args '(:holding)))
                                                 (financial-chart-3d-rates--entry e :krd)))))))

(defun financial-chart-3d-rates--from-cds (_template output args)
  "cds-term-structure bindings from credit.cds_isda_implied_hazard_pd OUTPUT.
OUTPUT is one output, entries {input?, output} by date (the input
gives the par spreads), or a list of outputs.  ARGS's :measure picks
the field drawn: one of `financial-chart-3d-rates-cds-measures', or
\"auto\" (the default), which binds the concrete field it resolves to
\(`financial-chart-3d-rates-cds-measure'), so the template's measure
slot, in 2D and 3D, always names a field of the rows."
  (let* ((entries (if (and (sequencep output) (not (keywordp (car-safe output))))
                      (append output nil)
                    (list output)))
         (measure (plist-get args :measure))
         (rows (financial-chart-3d-rates-cds-rows entries measure)))
    (list :data (vconcat rows)
          :measure (if (equal measure "value") measure
                     (financial-chart-3d-rates-cds-measure rows measure)))))

(defun financial-chart-3d-rates--from-cds-term (_template output args)
  "cds-term-structure bindings from credit.cds_term_structure OUTPUT."
  ;; The metrics test lane represents every metric as a series of stamped
  ;; entries.  This metric returns a whole term structure in one output, so
  ;; unwrap its one-entry series before adapting it.
  (when (and (sequencep output) (not (keywordp (car-safe output)))
             (= (length output) 1)
             (financial-chart-3d--value (aref (vconcat output) 0) :output))
    (setq output (financial-chart-3d--value (aref (vconcat output) 0) :output)))
  (let* ((measure (or (plist-get args :measure) "auto"))
         (rows (financial-chart-3d-rates-cds-term-rows output measure))
         (slopes (financial-chart-3d--value output :slopes))
         (skipped (financial-chart-3d-rates--list (financial-chart-3d--value output :skipped)))
         (skipped-text (if skipped
                           (mapconcat (lambda (r)
                                        (format "%s (%s)"
                                                (financial-chart-3d-rates-tenor-label
                                                 (or (financial-chart-3d--value r :tenor_years) 0))
                                                (or (financial-chart-3d--value r :reason) "skipped")))
                                      skipped ", ")
                         "none"))
         (subtitle (format "Shape %s — slopes 2s5s %.1f bp, 5s10s %.1f bp — skipped %s"
                           (or (financial-chart-3d--value output :shape) "unknown")
                           (or (financial-chart-3d--value slopes :s2s5s_bp) 0.0)
                           (or (financial-chart-3d--value slopes :s5s10s_bp) 0.0) skipped-text))
         (resolved (if (member measure '(nil "auto"))
                       (financial-chart-3d-rates-cds-measure rows measure) measure)))
    (list :data (vconcat rows) :measure resolved :description subtitle)))

(defun financial-chart-3d-rates--from-cds-ladder (_template output args)
  "cds-bond-ladder bindings from credit.cds_implied_ladder OUTPUT."
  ;; The metrics test lane represents every metric as a series of stamped
  ;; entries.  This metric returns a whole ladder in one output, so unwrap
  ;; its one-entry series before adapting it.
  (when (and (sequencep output) (not (keywordp (car-safe output)))
             (= (length output) 1)
             (financial-chart-3d--value (aref (vconcat output) 0) :output))
    (setq output (financial-chart-3d--value (aref (vconcat output) 0) :output)))
  (let* ((measure (or (plist-get args :measure) "model_yield"))
         (rows (financial-chart-3d-rates-cds-bond-rows output measure))
         (s (financial-chart-3d--value output :summary))
         (summary (format "%d bonds | principal $%.0f | yield %.2f%% | duration %.2f | DV01 $%.0f | total expected loss $%.0f"
                          (or (financial-chart-3d--value s :count) 0)
                          (* 1.0 (or (financial-chart-3d--value s :total_principal) 0))
                          (* 100 (or (financial-chart-3d--value s :average_model_yield) 0))
                          (or (financial-chart-3d--value s :average_modified_duration) 0)
                          (or (financial-chart-3d--value s :total_dv01_usd) 0)
                          (or (financial-chart-3d--value s :total_expected_loss_usd) 0))))
    (list :data (vconcat rows) :measure measure
          :title "CDS implied bond ladder — model, not traded"
          :description summary)))

(defconst financial-chart-3d-rates-metric-adapters
  '(("fi.nss_fit" "yield-curve" financial-chart-3d-rates--from-nss-fit)
    ("fi.curve_regime" "yield-curve" financial-chart-3d-rates--from-curve-regime)
    ("fi.spread_suite" "credit-spreads" financial-chart-3d-rates--from-spreads)
    ("fi.spread_pctile" "credit-spreads" financial-chart-3d-rates--from-spreads)
    ("fi.krd" "key-rate-durations" financial-chart-3d-rates--from-krd)
    ("credit.cds_isda_implied_hazard_pd" "cds-term-structure" financial-chart-3d-rates--from-cds)
    ("credit.cds_term_structure" "cds-term-structure" financial-chart-3d-rates--from-cds-term)
    ("bond_corp.cds_implied_ladder" "cds-bond-ladder" financial-chart-3d-rates--from-cds-ladder))
  "The rates templates' metric adapters, (METRIC-ID TEMPLATE FUNCTION):
`financial-chart-3d-bindings-from-metric' calls them.")

;;; Showing one, in 3D or 2D

(defconst financial-chart-3d-rates-templates
  '("yield-curve" "credit-spreads" "cds-term-structure" "cds-bond-ladder" "key-rate-durations")
  "The rates and credit templates: 2D in templates/financial/, 3D in templates3d/.")

;;;###autoload
(cl-defun financial-chart-3d-rates-show (name data &key title id)
  "Show rates template NAME over DATA, the invest-metrics JSON it takes.
NAME is one of `financial-chart-3d-rates-templates'; DATA is parsed
JSON (eas's plists and vectors) as docs/3d-rates.md describes.  In 3D
when graphics.el is available, else the 2D template in an eas buffer.
TITLE titles it; ID names the view.  Returns the view."
  (unless (member name financial-chart-3d-rates-templates)
    (signal 'financial-chart-invalid-data
            (list (format "No rates template %S; templates: %s" name
                          (string-join financial-chart-3d-rates-templates ", "))
                  :code "unknown_template" :index nil :field "name")))
  (let ((bindings (append (list :data data) (and title (list :title title)))))
    (if (financial-chart-3d-available-p)
        (financial-chart-3d-open name :bindings bindings :id id :show t)
      (let ((view (eas-view-open name :bindings bindings :id (or id (concat "fc/" name)))))
        (eas-show view)
        view))))

(provide 'financial-chart-3d-rates)
;;; financial-chart-3d-rates.el ends here
