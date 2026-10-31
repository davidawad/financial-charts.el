;;; financial-chart-3d-metrics-test.el --- invest metric outputs to every template's bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; `financial-chart-3d-bindings-from-metric' turns what `invest metric
;; run ID' emits into a template's bindings.  These feed it real metric
;; output, test/fixtures/invest-metrics/runs.json (invest-metrics run
;; by scripts/invest-metrics-runs.py on the goldens' inputs and on a
;; series per metric), and check, for every pair
;; `financial-chart-3d-metric-templates' lists, that the bindings
;; validate and draw in 2D and draw a 3D frame that is not blank.  The
;; runs are checked against the golden cases beside them, so they are
;; the metrics' output, not a hand copy.  `make test-3d' runs these.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(require 'graphics-frame)
(financial-chart-3d--ensure)

(defconst financial-chart-3d-metrics-test-dir
  (expand-file-name "test/fixtures/invest-metrics" financial-chart-test-root)
  "The invest-metrics goldens and runs.")

(defvar financial-chart-3d-metrics-test--runs nil "runs.json, parsed once.")

(defun financial-chart-3d-metrics-test--runs ()
  "test/fixtures/invest-metrics/runs.json as eas parses it."
  (or financial-chart-3d-metrics-test--runs
      (setq financial-chart-3d-metrics-test--runs
            (eas-json-read-file (expand-file-name "runs.json" financial-chart-3d-metrics-test-dir)))))

(defun financial-chart-3d-metrics-test--series (metric)
  "The series of METRIC in runs.json: plists (:metric :name :args :entries)."
  (seq-filter (lambda (s) (equal (plist-get s :metric) metric))
              (append (plist-get (financial-chart-3d-metrics-test--runs) :series) nil)))

(defun financial-chart-3d-metrics-test--cases (metric)
  "The golden-input runs of METRIC in runs.json: plists (:metric :name :input :output)."
  (seq-filter (lambda (c) (equal (plist-get c :metric) metric))
              (append (plist-get (financial-chart-3d-metrics-test--runs) :cases) nil)))

(defun financial-chart-3d-metrics-test--bindings (template series &rest extra)
  "TEMPLATE's bindings from SERIES, a runs.json series, and EXTRA args."
  (let ((args (plist-get series :args)))
    (apply #'financial-chart-3d-bindings-from-metric template (plist-get series :metric)
           (plist-get series :entries)
           (append extra
                   (cl-loop for (k v) on args by #'cddr
                            append (list (intern (replace-regexp-in-string "_" "-" (symbol-name k)))
                                         (if (vectorp v) (append v nil) v)))))))

(defun financial-chart-3d-metrics-test--lit (view)
  "Lit cells of VIEW's 60x24 frame at the end of its replay."
  (financial-chart-3d-seek-end view)
  (let ((frame (graphics-eas-frame view 60 24)) (n 0))
    (dotimes (i (* 60 24) n)
      (when (> (car (graphics-frame-cell frame i)) 0) (cl-incf n)))))

(defun financial-chart-3d-metrics-test--draws (template bindings id)
  "Check BINDINGS validate and draw TEMPLATE in 2D, and in 3D not blank.
ID names the 3D view."
  (let ((spec (eas-resolve template bindings)))
    (should-not (eas-spec-check spec))
    (should-not (eas-spec-unsupported spec))
    (should (> (length (eas-text-render (eas-compile spec :target 'text :size '(:cols 60 :rows 14)))) 0)))
  (let ((view (financial-chart-3d-open template :bindings bindings :id id)))
    (unwind-protect (should (> (financial-chart-3d-metrics-test--lit view) 0))
      (financial-chart-3d--close view))))

;;; The runs are the metrics' output

(defun financial-chart-3d-metrics-test--pointer (object pointer)
  "OBJECT's value at JSON POINTER (\"/windows/0/window\"), or `:missing'."
  (let ((v object))
    (dolist (part (cdr (split-string pointer "/")) v)
      (setq v (cond ((eq v :missing) :missing)
                    ((vectorp v) (let ((i (string-to-number part))) (if (< i (length v)) (aref v i) :missing)))
                    ((keywordp (car-safe v)) (let ((k (intern (concat ":" part))))
                                               (if (plist-member v k) (plist-get v k) :missing)))
                    (t :missing))))))

(defun financial-chart-3d-metrics-test--close (a b tol)
  "Non-nil when A and B are equal, numbers within TOL."
  (cond ((and (numberp a) (numberp b)) (<= (abs (- a b)) (max tol (* 1e-9 (abs b)))))
        ((and (vectorp a) (vectorp b))
         (and (= (length a) (length b))
              (cl-every (lambda (x y) (financial-chart-3d-metrics-test--close x y tol)) a b)))
        ((and (keywordp (car-safe a)) (keywordp (car-safe b)))
         (and (= (length a) (length b))
              (cl-loop for (k v) on b by #'cddr
                       always (financial-chart-3d-metrics-test--close (plist-get a k) v tol))))
        (t (equal a b))))

(ert-deftest financial-chart-3d-metrics-runs-match-the-goldens ()
  ;; Every golden case of a metric the adapters take is in runs.json,
  ;; run on its input, and agrees with every pointer the golden pins.
  (let ((goldens (cl-loop for f in (directory-files financial-chart-3d-metrics-test-dir t "\\.json\\'")
                          unless (equal (file-name-nondirectory f) "runs.json")
                          append (append (plist-get (eas-json-read-file f) :cases) nil)))
        (metrics (mapcar #'car (financial-chart-3d-metric-templates)))
        (checked 0))
    (dolist (golden goldens)
      (when (and (member (plist-get golden :metric) metrics)
                 (keywordp (car-safe (plist-get golden :expect))))
        (let ((run (seq-find (lambda (c) (equal (plist-get c :name) (plist-get golden :name)))
                             (financial-chart-3d-metrics-test--cases (plist-get golden :metric))))
              (tol (or (plist-get golden :abs_tol) 1e-9)))
          (ert-info ((format "%s: %s" (plist-get golden :metric) (plist-get golden :name)))
            (should run)
            (should (equal (plist-get run :input) (plist-get golden :input)))
            (cl-loop for (k v) on (plist-get golden :expect) by #'cddr
                     do (cl-incf checked)
                     (should (financial-chart-3d-metrics-test--close
                              (financial-chart-3d-metrics-test--pointer (plist-get run :output) (substring (symbol-name k) 1))
                              v tol)))))))
    (should (> checked 50))))

;;; Every pair draws

(ert-deftest financial-chart-3d-metrics-cover-every-metric-template ()
  ;; Every template whose data comes from a metric has an adapter; the
  ;; rest take order books, bars or a metric whose shape does not fit.
  (let ((fed (delete-dups (apply #'append (mapcar #'cdr (financial-chart-3d-metric-templates)))))
        (not-from-a-metric '("depth-live" "ladder" "liquidity-heatmap" "session-profile"
                             "volume-profile" "revenue-surface")))
    (dolist (f (directory-files (expand-file-name "templates/financial" financial-chart-test-root) nil "\\.json\\'"))
      (let ((name (file-name-base f)))
        (ert-info (name)
          (should (if (member name not-from-a-metric) (not (member name fed)) (member name fed))))))
    (dolist (template fed) (should (member template (financial-chart-3d-variants))))))

(ert-deftest financial-chart-3d-metrics-every-pair-draws ()
  (pcase-dolist (`(,metric . ,templates) (financial-chart-3d-metric-templates))
    (dolist (template templates)
      (let ((series (financial-chart-3d-metrics-test--series metric))
            (cases (seq-filter (lambda (c) (keywordp (car-safe (plist-get c :output))))
                               (financial-chart-3d-metrics-test--cases metric))))
        (ert-info ((format "%s -> %s" metric template))
          ;; A metric without a series is a grid metric: its golden runs are the grids.
          (should (or series cases))
          (dolist (s series)
            (ert-info ((plist-get s :name))
              (financial-chart-3d-metrics-test--draws
               template (financial-chart-3d-metrics-test--bindings template s)
               (format "fc3d-metrics-test:%s:%s" metric template))))
          (unless series
            (dolist (c cases)
              (when (> (length (plist-get (plist-get c :output) :rows)) 2)
                (ert-info ((plist-get c :name))
                  (financial-chart-3d-metrics-test--draws
                   template (financial-chart-3d-bindings-from-metric template metric (plist-get c :output))
                   (format "fc3d-metrics-test:%s:%s" metric template)))))))))))

;;; Every value of every enum slot binds and draws (fc-1en)

(defconst financial-chart-3d-metrics-test-adapter-values
  '(("cds-term-structure" :measure "auto"))
  "Adapter option values beyond their slot's enum: (TEMPLATE KEY VALUE...).
The adapter resolves them to a value of the enum before binding.")

(defun financial-chart-3d-metrics-test--slot-values (template)
  "Args to try on TEMPLATE: nil (the defaults), then (KEY VALUE) for each
value of each enum slot and each of
`financial-chart-3d-metrics-test-adapter-values'."
  (let ((slots (plist-get (plist-get (eas-template-get template) :meta) :slots)))
    (cons nil
          (append
           (cl-loop for (k slot) on slots by #'cddr
                    append (mapcar (lambda (v) (list k v)) (append (plist-get slot :enum) nil)))
           (cl-loop for (name k . values) in financial-chart-3d-metrics-test-adapter-values
                    when (equal name template) append (mapcar (lambda (v) (list k v)) values))))))

(ert-deftest financial-chart-3d-metrics-every-enum-value-draws ()
  ;; For every pair and every value of every enum slot of its template
  ;; (and the adapters' own values, and none), the bindings resolve in
  ;; 2D and open a 3D frame that is not blank, carrying a measure the
  ;; slot takes.
  (pcase-dolist (`(,metric . ,templates) (financial-chart-3d-metric-templates))
    (dolist (template templates)
      (let* ((series (car (financial-chart-3d-metrics-test--series metric)))
             (case (seq-find (lambda (c) (> (length (plist-get (plist-get c :output) :rows)) 2))
                             (financial-chart-3d-metrics-test--cases metric)))
             (slots (plist-get (plist-get (eas-template-get template) :meta) :slots)))
        (dolist (extra (financial-chart-3d-metrics-test--slot-values template))
          (ert-info ((format "%s -> %s %S" metric template extra))
            (should (or series case))
            (let ((bindings (if series
                                (apply #'financial-chart-3d-metrics-test--bindings template series extra)
                              (apply #'financial-chart-3d-bindings-from-metric template metric
                                     (plist-get case :output) extra))))
              (cl-loop for (k v) on bindings by #'cddr
                       for enum = (plist-get (plist-get slots k) :enum)
                       when enum do (should (seq-contains-p enum v)))
              (financial-chart-3d-metrics-test--draws
               template bindings (format "fc3d-metrics-test-enum:%s:%s" metric template)))))))))

(ert-deftest financial-chart-3d-metrics-cds-measures-are-the-slot-enum ()
  ;; One measure set: the adapter's is the 2D and 3D templates' enum.
  (dolist (name '("cds-term-structure" "fc3d/cds-term-structure"))
    (should (equal (append (plist-get (plist-get (plist-get (plist-get (eas-template-get name) :meta) :slots)
                                                 :measure)
                                      :enum)
                           nil)
                   financial-chart-3d-rates-cds-measures)))
  ;; auto (or none) binds the field it resolves to: spreads when every
  ;; entry gives them, else the hazard rate.
  (let* ((entries (plist-get (car (financial-chart-3d-metrics-test--series "credit.cds_isda_implied_hazard_pd"))
                             :entries))
         (bare (mapcar (lambda (e) (plist-get e :output)) (append entries nil)))
         (measure (lambda (output &rest args)
                    (plist-get (apply #'financial-chart-3d-bindings-from-metric "cds-term-structure"
                                      "credit.cds_isda_implied_hazard_pd" output args)
                               :measure))))
    (should (equal (funcall measure entries) "spread_bp"))
    (should (equal (funcall measure entries :measure "auto") "spread_bp"))
    (should (equal (funcall measure bare :measure "auto") "hazard_bps"))
    (should (equal (funcall measure bare :measure "pd_pct") "pd_pct"))
    (should (equal (funcall measure bare :measure "value") "value"))
    (should (equal (plist-get (cddr (should-error (funcall measure bare :measure "spread")
                                                  :type 'financial-chart-invalid-data))
                              :code)
                   "bad_measure"))))

(ert-deftest financial-chart-3d-metrics-golden-outputs-adapt ()
  ;; One run's output (a golden case's), as an entry with its input and
  ;; stamps, adapts and validates in 2D.
  (let ((stamps '(:as-of "2026-03-31" :date "2026-03-31" :tenor 30 :expiry "2026-11-20" :horizon "1d"
                         :holding "Bond" :scenario "Shock" :strategy "Book" :symbol "XLK" :sector "Tech"
                         :time "2026-10-30T15:30:00"))
        (adapted 0))
    (pcase-dolist (`(,metric . ,templates) (financial-chart-3d-metric-templates))
      (dolist (c (financial-chart-3d-metrics-test--cases metric))
        (dolist (template templates)
          (ert-info ((format "%s -> %s: %s" metric template (plist-get c :name)))
            ;; opt.chain_stats's golden input has no open interest to draw.
            (if (equal metric "opt.chain_stats")
                (should (equal (plist-get (cddr (should-error (financial-chart-3d-bindings-from-metric
                                                               template metric
                                                               (list (list :output (plist-get c :output)
                                                                           :input (plist-get c :input)
                                                                           :expiry "2026-11-20"))
                                                               :spot 100)
                                                              :type 'financial-chart-invalid-data))
                                          :code)
                               "missing_open_interest"))
            (let* ((entry (append (list :output (plist-get c :output) :input (plist-get c :input))
                                  (cl-loop for (k v) on stamps by #'cddr
                                           append (list (intern (replace-regexp-in-string "-" "_" (symbol-name k))) v))))
                   ;; A ruin output echoes no inputs: its entry names the point.
                   (output (list (append entry (list :edge (plist-get (plist-get c :input) :edge)
                                                     :fraction (plist-get (plist-get c :input) :fraction)))))
                   (bindings (apply #'financial-chart-3d-bindings-from-metric template metric output
                                    (append (and (member template '("options-oi")) (list :spot 100))
                                            (and (equal template "dcf-sensitivity") (list :price 100))
                                            (cl-loop for (k v) on stamps by #'cddr
                                                     when (memq k '(:as-of :tenor :date :horizon :holding :scenario
                                                                           :strategy :symbol))
                                                     append (list k v)))))
                   (spec (eas-resolve template bindings)))
              (cl-incf adapted)
              (should (> (length (plist-get bindings :data)) 0))
              (should-not (eas-spec-check spec))))))))
    (should (> adapted 20))))

;;; What each adapter reads

(ert-deftest financial-chart-3d-metrics-rows-are-the-metrics-numbers ()
  (let ((b (financial-chart-3d-bindings-from-metric
            "risk-frontier" "port.frontier"
            (plist-get (car (financial-chart-3d-metrics-test--cases "port.frontier")) :output))))
    ;; Decimals become percent: vol 0.0894 is 8.94%.
    (should (equal (plist-get (aref (plist-get b :data) 0) :date) "2026-03-31"))
    (should (= (plist-get (aref (plist-get b :data) 0) :vol) 8.9443)))
  (let ((scaled (seq-find (lambda (c) (string-match-p "output_scale" (plist-get c :name)))
                          (financial-chart-3d-metrics-test--cases "port.frontier"))))
    (should (= (plist-get (aref (plist-get (financial-chart-3d-bindings-from-metric
                                            "risk-frontier" "port.frontier" (plist-get scaled :output)
                                            :output-scale 100 :as-of "2026-03-31")
                                           :data)
                                1)
                          :vol)
               20.0)))
  ;; port.stress: weight x scenario return, percent of NAV, summing to the value.
  (let* ((c (car (financial-chart-3d-metrics-test--cases "port.stress")))
         (rows (append (plist-get (financial-chart-3d-bindings-from-metric
                                   "risk-stress" "port.stress" (plist-get c :output) :scenario "Crash")
                                  :data)
                       nil)))
    (should (equal (mapcar (lambda (r) (plist-get r :asset)) rows) '("equity" "bonds")))
    (should (< (abs (- (apply #'+ (mapcar (lambda (r) (plist-get r :pnl)) rows))
                       (* 100 (plist-get (plist-get c :output) :value))))
               1e-9)))
  ;; stats.risk_contribution: shares in percent, assets named by :labels.
  (let ((rows (plist-get (financial-chart-3d-bindings-from-metric
                          "risk-contribution" "stats.risk_contribution"
                          (plist-get (car (financial-chart-3d-metrics-test--cases "stats.risk_contribution")) :output)
                          :labels '("Stocks" "Bonds") :as-of "2026-03-31")
                         :data)))
    (should (equal (mapcar (lambda (r) (list (plist-get r :asset) (plist-get r :contribution))) rows)
                   '(("Stocks" 33.33) ("Bonds" 66.67)))))
  ;; Kelly: grid rows as they are, ruin at the floor, the slots from the inputs.
  (let* ((outs (mapcar (lambda (c) (list :output (plist-get c :output) :edge 0.1 :fraction 1.0))
                       (seq-filter (lambda (c) (string-match-p "binary" (plist-get c :name)))
                                   (financial-chart-3d-metrics-test--cases "stats.kelly.growth"))))
         (b (financial-chart-3d-bindings-from-metric "kelly-surface" "stats.kelly.growth" outs)))
    (should (equal (plist-get b :model) "binary"))
    (should (equal (mapcar (lambda (r) (plist-get r :growth)) (plist-get b :data)) '(0.005 -1.0))))
  ;; eq.dcf --grid: the newest snapshot's price is the slot; nulls dropped.
  (let* ((s (car (financial-chart-3d-metrics-test--series "eq.dcf")))
         (b (financial-chart-3d-metrics-test--bindings "dcf-sensitivity" s)))
    (should (= (plist-get b :price) 141.2))
    (should (seq-every-p (lambda (r) (numberp (plist-get r :value_per_share))) (plist-get b :data))))
  ;; opt.gex: per-strike terms sum to the output; puts down.
  (let* ((s (car (financial-chart-3d-metrics-test--series "opt.gex")))
         (b (financial-chart-3d-metrics-test--bindings "options-gex" s))
         (total (apply #'+ (mapcar (lambda (e) (plist-get (plist-get e :output) :gex))
                                   (append (plist-get s :entries) nil)))))
    (should (= (plist-get b :spot) 586.15))
    (should (< (abs (- (* 1e6 (apply #'+ (mapcar (lambda (r) (+ (plist-get r :call_gex) (plist-get r :put_gex)))
                                                   (plist-get b :data))))
                       total))
               (* 1e-3 (abs total))))))

(ert-deftest financial-chart-3d-metrics-errors-name-the-entry ()
  (let ((code (lambda (thunk) (plist-get (cddr (should-error (funcall thunk) :type 'financial-chart-invalid-data))
                                         :code))))
    (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric "risk-stress" "opt.vrp" nil)))
                   "unknown_metric"))
    (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric "x" "no.such" nil)))
                   "unknown_metric"))
    ;; The options grids check the envelope is theirs.
    (let ((pop (plist-get (car (financial-chart-3d-metrics-test--cases "opt.pop_grid")) :output)))
      (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric
                                               "greeks-surface" "opt.greeks_grid" pop)))
                     "wrong_metric")))
    ;; options-oi needs the chain the metric ran on, and the spot.
    (let ((c (car (financial-chart-3d-metrics-test--cases "opt.max_pain"))))
      (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric
                                               "options-oi" "opt.max_pain"
                                               (list (list :expiry "2026-11-20" :output (plist-get c :output)))
                                               :spot 100)))
                     "missing_open_interest"))
      (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric
                                               "options-oi" "opt.max_pain"
                                               (list (list :expiry "2026-11-20" :output (plist-get c :output)
                                                           :input (plist-get c :input))))))
                     "missing_spot")))
    ;; opt.gex: an input that does not give the output's gex.
    (let ((c (car (financial-chart-3d-metrics-test--cases "opt.gex"))))
      (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric
                                               "options-gex" "opt.gex"
                                               (list (list :expiry "2026-11-20" :output (plist-get c :output)
                                                           :input (plist-put (copy-sequence (plist-get c :input))
                                                                             :gamma [0.02]))))))
                     "input_mismatch")))
    ;; Yield curves need their date.
    (should (equal (funcall code (lambda () (financial-chart-3d-bindings-from-metric
                                             "yield-curve" "fi.nss_fit"
                                             (plist-get (car (financial-chart-3d-metrics-test--cases "fi.nss_fit"))
                                                        :output))))
                   "missing_date"))))

(provide 'financial-chart-3d-metrics-test)
;;; financial-chart-3d-metrics-test.el ends here
