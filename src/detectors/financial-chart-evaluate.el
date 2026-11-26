;;; financial-chart-evaluate.el --- is a detector worth drawing: forward returns against chance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Layer E of docs/design/markups.md (fc-gty.9).  `financial-chart-evaluate'
;; runs detectors over a universe of bars sets and returns, and with a
;; directory writes, one evidence/v1 record per detector:
;;
;;   {"evidence/v1": "pattern.hammer", "params": {...}, "code_version": "...",
;;    "bars_ids": [...], "universe": ["TSM", ...], "range": [FIRST, LAST],
;;    "split": {"in_sample_end": T}, "n": 412, "n_oos": 130,
;;    "forward": {"1": {"mean", "baseline_mean", "hit", "p"}, "5": ..., "20": ...},
;;    "oos": {"5": {...}}, "costs_bps": 5, "baseline": "...",
;;    "reality_check_p": 0.31, "spa_p": 0.27, "random_entry_p": 0.4,
;;    "family_tests": {...}, "adjustment": "...", "reference": "...",
;;    "verdict": "noise"}
;;
;; The rules:
;; - An event is entered at the open of the bar after its
;;   confirmed_index, never at the event bar, and exited at the open H
;;   bars later.  A bullish event is long, a bearish one short; neutral
;;   events are not traded.  Costs: COSTS-BPS each way.
;; - The baseline is chance: DRAWS times, as many random entry bars in
;;   the same bars sets, with the same directions.  p is the share of
;;   draws whose mean return reaches the detector's.
;; - The split is fixed before the run: IN-SAMPLE-END (a fraction of each
;;   bars set, default 0.7, or a time); the verdict reads the
;;   out-of-sample events only.
;; - Detectors evaluated together are corrected for data snooping.
;;   Each detector's out-of-sample trades become a per-bar return series
;;   (the mean signed return of its open trades, net of costs; cash when
;;   flat), the same bars for all.  A stationary bootstrap (Politis, D.
;;   and Romano, J. 1994, "The Stationary Bootstrap", JASA 89; mean
;;   block MEAN-BLOCK bars) resamples those bars, and two tests read it:
;;   White's Reality Check (White, H. 2000, "A Reality Check for Data
;;   Snooping", Econometrica 68(5)) and Hansen's SPA with its consistent
;;   recentring (Hansen, P. 2005, "A Test for Superior Predictive
;;   Ability", JBES 23(4)).  Each record carries the family's p-values
;;   (family_tests) and its own against the family's max-statistic null
;;   (reality_check_p, spa_p: a single-step correction), plus
;;   random_entry_p, the random-entry max statistic over the family.
;; - verdict: "edge" when the out-of-sample excess over the baseline is
;;   positive, spa_p < 0.05 and random_entry_p < 0.05, with at least 30
;;   out-of-sample events; "untested" with fewer than 30; else "noise".
;;
;; Prices are used as given unless the caller passes ACTIONS (splits and
;; dividends per bars set): then `financial-chart-adjust-bars' back-adjusts
;; them first, and the record says so in "adjustment"; it names the bars
;; it read by their content hash.  Records are content-addressed files
;; under `financial-chart-evidence-directory' (owner data): each run
;; writes new files and never rewrites one, so runs never contend.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-core)
(require 'financial-chart-detector-api)

(defvar financial-chart-version)

(defcustom financial-chart-evidence-directory nil
  "Directory of evidence/v1 records (owner data), or nil for none."
  :type '(choice (const :tag "None" nil) directory)
  :group 'financial-chart)

(defvar financial-chart-evaluate-references
  '(("pattern." . "Bulkowski, T. 2008, \"Encyclopedia of Candlestick Charts\", Wiley (per-pattern statistics)")
    ("level." . "Osler, C. 2000, \"Support for Resistance\", FRBNY Economic Policy Review 6(2)")
    ("swing." . "Lo, A., Mamaysky, H. and Wang, J. 2000, \"Foundations of Technical Analysis\", Journal of Finance 55(4)"))
  "Published reference results, by detector name prefix.")

;;; Returns

(defun financial-chart-evaluate--entries (events bars horizon costs)
  "Signed forward returns of EVENTS over BARS at HORIZON, net of COSTS.
Each is (INDEX . RETURN): entry at the open after confirmed_index, exit
at the open HORIZON bars later; events too near the end are dropped."
  (let ((n (length bars)))
    (cl-loop for e in events
             for dir = (pcase (plist-get e :direction) ("bullish" 1) ("bearish" -1) (_ nil))
             for entry = (1+ (plist-get e :confirmed_index))
             for exit = (+ entry horizon)
             when (and dir (< exit n))
             collect (cons entry (- (* dir (financial-chart-evaluate--ret bars entry exit)) (* 2 costs))))))

(defun financial-chart-evaluate--ret (bars entry exit)
  "The return from the open of bar ENTRY to the open of bar EXIT of BARS."
  (let ((a (plist-get (aref bars entry) :open)) (b (plist-get (aref bars exit) :open)))
    (if (and (numberp a) (numberp b) (/= a 0)) (/ (- b a) (float a)) 0.0)))

(defun financial-chart-evaluate--mean (xs)
  "Mean of XS, 0 when empty."
  (if xs (/ (apply #'+ xs) (float (length xs))) 0.0))

(defun financial-chart-evaluate--sd (xs)
  "Sample standard deviation of XS (1e-12 when undefined)."
  (let ((n (length xs)))
    (if (< n 2) 1e-12
      (let ((m (financial-chart-evaluate--mean xs)))
        (max 1e-12 (sqrt (/ (cl-loop for x in xs sum (expt (- x m) 2)) (1- n))))))))

(defun financial-chart-evaluate--draw-means (sets dirs horizon costs draws rng)
  "DRAWS means of random entries: for each bars set of SETS (BARS . FROM . TO),
as many entries between FROM and TO as its DIRS (a list of signs per
set), at HORIZON net of COSTS.  RNG is a function of N giving 0..N-1."
  (cl-loop repeat draws
           collect (financial-chart-evaluate--mean
                    (cl-loop for (bars from . to) in sets for ds in dirs
                             append (cl-loop for d in ds
                                             for lo = (max 1 from) for hi = (- to horizon 1)
                                             when (> hi lo)
                                             collect (let ((entry (+ lo (funcall rng (- hi lo)))))
                                                       (- (* d (financial-chart-evaluate--ret bars entry (+ entry horizon)))
                                                          (* 2 costs))))))))

(defun financial-chart-evaluate--rng (seed)
  "A deterministic generator from SEED: a function of N returning 0..N-1."
  (let ((state (logand seed #xffffffff)))
    (lambda (n)
      (setq state (logand (+ (* state 1103515245) 12345) #x7fffffff))
      (% state n))))

(defun financial-chart-evaluate--code-version (name)
  "A stable hash of detector NAME's definition, parameters and package version."
  (let ((e (financial-chart-detector name)))
    (substring (secure-hash 'sha256 (format "%S" (list financial-chart-version
                                                       (plist-get e :definition) (plist-get e :params)
                                                       (plist-get e :lookback) (plist-get e :lag))))
               0 16)))

;;; The run

(defun financial-chart-evaluate--split-index (bars in-sample-end)
  "The first out-of-sample index of BARS for IN-SAMPLE-END (fraction or time)."
  (let ((n (length bars)))
    (cond ((floatp in-sample-end) (floor (* in-sample-end n)))
          ((stringp in-sample-end)
           (or (cl-loop for i from 0 below n
                        when (not (string< (format "%s" (financial-chart-detector--time bars i)) in-sample-end))
                        return i)
               n))
          (t (floor (* 0.7 n))))))

(cl-defun financial-chart-evaluate (detectors universe &key params (horizons '(1 5 20)) (costs-bps 5)
                                              (draws 1000) (in-sample-end 0.7) (seed 20261126)
                                              (mean-block 10) actions directory)
  "Evidence/v1 records of DETECTORS (names) over UNIVERSE, one per detector.
UNIVERSE is ((NAME . BARS) ...), each BARS bar/v1 oldest first.  PARAMS
are passed to every detector that takes them.  HORIZONS are bar counts;
COSTS-BPS is charged each way; DRAWS random-entry draws make the
baseline; IN-SAMPLE-END is a fraction of each bars set (0.7) or a time.
ACTIONS, ((NAME . CORPORATE-ACTIONS) ...), back-adjusts a bars set first
\(`financial-chart-adjust-bars').  MEAN-BLOCK is the stationary
bootstrap's mean block length in bars.  The verdict reads the
out-of-sample events at the middle horizon.  With
DIRECTORY (default `financial-chart-evidence-directory') each record is
also written there as a content-addressed file; return the records."
  (let* ((directory (or directory financial-chart-evidence-directory))
         (costs (/ costs-bps 10000.0))
         (sets (mapcar (lambda (u)
                         (let ((acts (cdr (assoc (car u) actions))))
                           (cons (car u) (vconcat (if acts (financial-chart-adjust-bars (cdr u) acts) (cdr u))))))
                       universe))
         (horizon (nth (/ (length horizons) 2) horizons))
         (rng (financial-chart-evaluate--rng seed))
         ;; Per detector: events per set, split.
         (runs
          (cl-loop
           for name in detectors
           collect
           (let* ((entry (financial-chart-detector name))
                  (p (and (plist-get entry :params)
                          (cl-loop for (k v) on params by #'cddr
                                   when (cl-find (intern (substring (symbol-name k) 1)) (plist-get entry :params)
                                                 :key (lambda (s) (plist-get s :name)))
                                   append (list k v))))
                  (per-set (mapcar (lambda (s) (list (car s) (cdr s) (financial-chart-detect name (cdr s) p)
                                                     (financial-chart-evaluate--split-index (cdr s) in-sample-end)))
                                   sets)))
             (list :name name :params p :sets per-set))))
         ;; Statistics per detector and horizon, and the out-of-sample null.
         (stats
          (cl-loop
           for run in runs
           collect
           (let ((forward nil) (oos nil) (oos-null nil) (n 0) (n-oos 0))
             (dolist (h horizons)
               (let* ((all (cl-loop for (_set bars events _split) in (plist-get run :sets)
                                    append (mapcar #'cdr (financial-chart-evaluate--entries events bars h costs))))
                      (out (cl-loop for (_set bars events split) in (plist-get run :sets)
                                    append (mapcar #'cdr (seq-filter (lambda (r) (>= (car r) split))
                                                                     (financial-chart-evaluate--entries events bars h costs)))))
                      (dirs-all (cl-loop for (_set _bars events _split) in (plist-get run :sets)
                                         collect (delq nil (mapcar (lambda (e) (pcase (plist-get e :direction)
                                                                                 ("bullish" 1) ("bearish" -1)))
                                                                   events))))
                      (dirs-oos (cl-loop for (_set _bars events split) in (plist-get run :sets)
                                         collect (delq nil (mapcar (lambda (e) (and (>= (1+ (plist-get e :confirmed_index)) split)
                                                                                    (pcase (plist-get e :direction)
                                                                                      ("bullish" 1) ("bearish" -1))))
                                                                   events))))
                      (null-all (financial-chart-evaluate--draw-means
                                 (mapcar (lambda (s) (cons (nth 1 s) (cons 0 (length (nth 1 s))))) (plist-get run :sets))
                                 dirs-all h costs draws rng))
                      (null-oos (financial-chart-evaluate--draw-means
                                 (mapcar (lambda (s) (cons (nth 1 s) (cons (nth 3 s) (length (nth 1 s))))) (plist-get run :sets))
                                 dirs-oos h costs draws rng))
                      (summary (lambda (xs null)
                                 (let ((m (financial-chart-evaluate--mean xs)))
                                   (list :n (length xs) :mean m
                                         :baseline_mean (financial-chart-evaluate--mean null)
                                         :hit (if xs (/ (cl-count-if (lambda (x) (> x 0)) xs) (float (length xs))) 0.0)
                                         :p (if xs (/ (cl-count-if (lambda (d) (>= d m)) null) (float (length null))) 1.0))))))
                 (push (cons (number-to-string h) (funcall summary all null-all)) forward)
                 (push (cons (number-to-string h) (funcall summary out null-oos)) oos)
                 (when (= h horizon)
                   (setq n (length all) n-oos (length out)
                         oos-null (list :mean (financial-chart-evaluate--mean out)
                                        :sd (financial-chart-evaluate--sd out) :n (length out)
                                        :null null-oos)))))
             (list :run run :forward (nreverse forward) :oos (nreverse oos) :n n :n-oos n-oos :null oos-null))))
         ;; The max statistic over the family, per draw.
         ;; Each detector's excess in units of its own null's spread; the
         ;; family is the detectors with at least 2 out-of-sample trades.
         (z (lambda (st m) (let ((nl (plist-get st :null)))
                             (/ (- m (financial-chart-evaluate--mean (plist-get nl :null)))
                                (financial-chart-evaluate--sd (plist-get nl :null))))))
         (family (seq-filter (lambda (st) (>= (plist-get (plist-get st :null) :n) 2)) stats))
         (observed (mapcar (lambda (st) (funcall z st (plist-get (plist-get st :null) :mean))) stats))
         (null-max (cl-loop for d from 0 below draws
                            collect (if family
                                        (apply #'max (mapcar (lambda (st) (funcall z st (nth d (plist-get (plist-get st :null) :null))))
                                                             family))
                                      0.0))))
    (let* ((periods (mapcar (lambda (st)
                              (apply #'vconcat
                                     (cl-loop for (_set bars events split) in (plist-get (plist-get st :run) :sets)
                                              collect (financial-chart-evaluate--period-series
                                                       events bars split horizon costs))))
                            stats))
           (len (length (car periods)))
           (boot (and (> len 1) (financial-chart-stationary-bootstrap-indices len mean-block draws rng)))
           (rc (and boot (financial-chart-reality-check periods boot)))
           (spa (and boot (financial-chart-spa periods boot))))
    (cl-loop
     for st in stats for v in observed for i from 0
     collect
     (let* ((run (plist-get st :run)) (name (plist-get run :name))
            (random-p (if (memq st family) (/ (cl-count-if (lambda (m) (>= m v)) null-max) (float draws)) 1.0))
            (rc-p (if rc (nth i (plist-get rc :model-p)) 1.0))
            (spa-p (if spa (nth i (plist-get spa :model-p)) 1.0))
            (mid (cdr (assoc (number-to-string horizon) (plist-get st :oos))))
            (verdict (cond ((< (plist-get st :n-oos) 30) "untested")
                           ((and (> (plist-get mid :mean) (plist-get mid :baseline_mean))
                                 (< spa-p 0.05) (< random-p 0.05))
                            "edge")
                           (t "noise")))
            (record
             (list :evidence/v1 name
                   :params (or (plist-get run :params) :null)
                   :code_version (financial-chart-evaluate--code-version name)
                   :bars_ids (vconcat (mapcar (lambda (s) (financial-chart-bars-id (nth 1 s))) (plist-get run :sets)))
                   :universe (vconcat (mapcar #'car (plist-get run :sets)))
                   :range (let ((s (car (plist-get run :sets))))
                            (vector (financial-chart-detector--time (nth 1 s) 0)
                                    (financial-chart-detector--time (nth 1 s) (1- (length (nth 1 s))))))
                   :split (list :in_sample_end (if (numberp in-sample-end) in-sample-end (format "%s" in-sample-end)))
                   :entry "open after confirmed_index; exit at the open H bars later"
                   :n (plist-get st :n) :n_oos (plist-get st :n-oos)
                   :forward (cl-loop for (h . s) in (plist-get st :forward) append (list (intern (concat ":" h)) s))
                   :oos (cl-loop for (h . s) in (plist-get st :oos) append (list (intern (concat ":" h)) s))
                   :costs_bps costs-bps
                   :baseline (format "random entry bars, same count and directions per bars set, %d draws" draws)
                   :reality_check_p rc-p
                   :spa_p spa-p
                   :random_entry_p random-p
                   :family (vconcat detectors)
                   :family_tests (list :reality_check_p (if rc (plist-get rc :p) :null)
                                       :spa_p (if spa (plist-get spa :p) :null)
                                       :bootstrap (format "stationary (Politis-Romano 1994), mean block %s bars, %d draws, benchmark cash"
                                                          mean-block draws))
                   :adjustment (let ((k (cl-loop for u in universe sum (length (cdr (assoc (car u) actions))))))
                                 (if (> k 0) (format "back-adjusted for %d corporate actions" k)
                                   "none: prices as given"))
                   :reference (or (cdr (seq-find (lambda (r) (string-prefix-p (car r) name))
                                                 financial-chart-evaluate-references))
                                  :null)
                   :created (format-time-string "%FT%TZ" nil t)
                   :verdict verdict)))
       (when directory (financial-chart-evidence-write record directory))
       record)))))

;;; Records

(defun financial-chart-evidence-write (record directory)
  "Write evidence RECORD under DIRECTORY as a new content-addressed file.
Return its path; an identical record is the same file."
  (make-directory directory t)
  (let* ((json (json-serialize record :null-object :null :false-object :false))
         (file (expand-file-name (format "%s-%s.json" (plist-get record :evidence/v1)
                                         (substring (secure-hash 'sha256 json) 0 16))
                                 directory)))
    (unless (file-exists-p file)
      (with-temp-file file (insert (decode-coding-string json 'utf-8) "\n")))
    file))

(defun financial-chart-evidence-records (name &optional directory)
  "Evidence records of detector NAME in DIRECTORY, newest first."
  (let ((directory (or directory financial-chart-evidence-directory)))
    (when (and directory (file-directory-p directory))
      (sort (mapcar (lambda (f) (with-temp-buffer
                                  (insert-file-contents f)
                                  (json-parse-buffer :object-type 'plist :array-type 'array
                                                     :null-object :null :false-object :false)))
                    (directory-files directory t (concat "\\`" (regexp-quote name) "-[0-9a-f]\\{16\\}\\.json\\'")))
            (lambda (a b) (string< (plist-get b :created) (plist-get a :created)))))))

(defun financial-chart-evidence-verdict (name &optional directory)
  "Detector NAME's newest evidence, or nil.
A plist (:status VERDICT :p P :n N :stale BOOL).
Stale when its code_version is not the detector's now, or it is older
than 90 days."
  (when-let* ((r (car (financial-chart-evidence-records name directory))))
    (let ((age (- (float-time) (float-time (date-to-time (plist-get r :created))))))
      (list :status (plist-get r :verdict) :p (plist-get r :reality_check_p) :n (plist-get r :n_oos)
            :stale (if (or (not (equal (plist-get r :code_version) (financial-chart-evaluate--code-version name)))
                           (> age (* 90 86400)))
                       t :false)))))

(defun financial-chart-evaluate-verb (detectors universe &rest options)
  "`financial-chart-evaluate' in the chart/v1 envelope: verdicts and files."
  (let ((start (float-time)))
    (condition-case err
        (let ((records (apply #'financial-chart-evaluate detectors universe options)))
          (list :contract "chart/v1" :ok t
                :data (vconcat (mapcar (lambda (r) (list :detector (plist-get r :evidence/v1) :verdict (plist-get r :verdict)
                                                         :n_oos (plist-get r :n_oos) :reality_check_p (plist-get r :reality_check_p)))
                                       records))
                :cost (list :ms (round (* 1000 (- (float-time) start))))
                :next ["(financial-chart-evidence-verdict NAME)"]))
      (financial-chart-invalid-detection
       (list :contract "chart/v1" :ok :false :reason (plist-get (cddr err) :code) :message (cadr err))))))

(defun financial-chart-evaluate--before-p (a b)
  "Non-nil when time A is before time B (both ISO strings or both epoch ms)."
  (cond ((and (numberp a) (numberp b)) (< a b))
        ((and (stringp a) (stringp b)) (string< a b))
        (t (signal 'financial-chart-invalid-detection
                   (list (format "Times %S and %S are not both ISO or both epoch ms" a b)
                         :code "INVALID_TIME" :path "/actions")))))

(defun financial-chart-adjust-bars (bars actions)
  "BARS (bar/v1, oldest first) back-adjusted for corporate ACTIONS.  Pure.
ACTIONS are plists (:time T :split R) -- R new shares per old share, 2
for a 2-for-1 -- or (:time T :dividend D) -- D per share, ex-dividend on
the bar at T (a bar's time, or its index when the bars have none).
Every bar before T is adjusted: a split divides its
prices by R and multiplies its volume by R; a dividend multiplies its
prices by 1 - D / C, C the close of the bar before T (the usual
total-return back-adjustment).  Bars at or after T are as given.
Return a new list of bars."
  (let* ((v (vconcat (mapcar #'copy-sequence (append bars nil))))
         (n (length v))
         (actions (sort (copy-sequence (append actions nil))
                        (lambda (a b) (financial-chart-evaluate--before-p (plist-get b :time) (plist-get a :time))))))
    ;; Newest first: each factor applies to the (already adjusted) bars before it.
    (dolist (a actions)
      (let* ((at (or (cl-loop for i from 0 below n
                              unless (financial-chart-evaluate--before-p (or (plist-get (aref v i) :time) i)
                                                                          (plist-get a :time))
                              return i)
                     n))
             (split (plist-get a :split)) (dividend (plist-get a :dividend))
             (price-factor (cond (split (/ 1.0 split))
                                 ((and dividend (> at 0))
                                  (- 1.0 (/ dividend (float (plist-get (aref v (1- at)) :close)))))
                                 (t 1.0)))
             (volume-factor (if split (float split) 1.0)))
        (unless (or split dividend)
          (signal 'financial-chart-invalid-detection
                  (list (format "Action %S is neither a split nor a dividend" a) :code "INVALID_ACTION" :path "/actions")))
        (dotimes (i at)
          (let ((b (aref v i)))
            (dolist (k '(:open :high :low :close))
              (when (numberp (plist-get b k)) (setq b (plist-put b k (* price-factor (plist-get b k))))))
            (when (numberp (plist-get b :volume)) (setq b (plist-put b :volume (* volume-factor (plist-get b :volume)))))
            (aset v i b)))))
    (append v nil)))

(defun financial-chart-stationary-bootstrap-indices (n mean-block draws rng)
  "DRAWS index lists of length N from the stationary bootstrap.
Politis, D. and Romano, J. 1994, \"The Stationary Bootstrap\", JASA 89:
each index follows the previous one (wrapping at N) with probability
1 - 1/MEAN-BLOCK and restarts at a uniform index otherwise.  RNG is a
function of K giving 0..K-1."
  (let ((p (/ 1.0 (max 1.0 mean-block))) (scale 1000000))
    (cl-loop repeat draws
             collect (let ((i (funcall rng n)))
                       (cons i (cl-loop repeat (1- n)
                                        collect (setq i (if (< (funcall rng scale) (* p scale))
                                                            (funcall rng n)
                                                          (% (1+ i) n)))))))))

(defun financial-chart-evaluate--means (f draws)
  "Mean of series F (a vector) under each index list of DRAWS."
  (mapcar (lambda (idx) (/ (cl-loop for i in idx sum (aref f i)) (float (length idx)))) draws))

(defun financial-chart-reality-check (series draws)
  "White's Reality Check p-value of each of SERIES against zero.
SERIES are vectors of one performance differential per period (the same
periods for all); DRAWS are bootstrap index lists
\(`financial-chart-stationary-bootstrap-indices').  White, H. 2000, \"A
Reality Check for Data Snooping\", Econometrica 68(5): V = max_k sqrt(n)
mean(f_k); each draw's V* = max_k sqrt(n) (mean(f*_k) - mean(f_k)); the
p-value is the share of draws with V* >= V.  Return (:p P :stat V
:model-p (P_k ...)), P_k the share of draws with V* >= sqrt(n) mean(f_k):
each model against the family's null, a single-step correction."
  (let* ((n (length (car series)))
         (means (mapcar (lambda (f) (financial-chart-evaluate--mean (append f nil))) series))
         (v (apply #'max (mapcar (lambda (m) (* (sqrt n) m)) means)))
         (boot (mapcar (lambda (f) (financial-chart-evaluate--means f draws)) series))
         (vstar (cl-loop for d from 0 below (length draws)
                         collect (apply #'max (cl-loop for b in boot for m in means
                                                       collect (* (sqrt n) (- (nth d b) m)))))))
    (list :p (/ (cl-count-if (lambda (x) (>= x v)) vstar) (float (length draws))) :stat v
          ;; Each model against the same max-statistic null (single step).
          :model-p (mapcar (lambda (m) (/ (cl-count-if (lambda (x) (>= x (* (sqrt n) m))) vstar)
                                          (float (length draws))))
                           means))))

(defun financial-chart-spa (series draws)
  "Hansen's SPA consistent p-value of SERIES against zero (see
`financial-chart-reality-check' for SERIES and DRAWS).  Hansen, P. 2005,
\"A Test for Superior Predictive Ability\", JBES 23(4): each model is
studentized by w_k, the bootstrap standard deviation of sqrt(n) mean(f*_k);
T = max(0, max_k sqrt(n) mean(f_k) / w_k); a draw's statistic recentres
by g(mean(f_k)) = mean(f_k) when mean(f_k) >= -sqrt(w_k^2 / n * 2 log log n),
else 0 (the consistent choice, which drops hopeless models), so
T* = max(0, max_k sqrt(n) (mean(f*_k) - g(mean(f_k))) / w_k).  The
p-value is the share of draws with T* >= T.  Return (:p P :stat T
:model-p (P_k ...)) as `financial-chart-reality-check'."
  (let* ((n (length (car series)))
         (loglog (max 0.0 (log (max 1.0 (log (max 1.0 (float n)))))))
         (means (mapcar (lambda (f) (financial-chart-evaluate--mean (append f nil))) series))
         (boot (mapcar (lambda (f) (financial-chart-evaluate--means f draws)) series))
         (omegas (cl-loop for b in boot
                          collect (let* ((xs (mapcar (lambda (m) (* (sqrt n) m)) b))
                                         (mu (financial-chart-evaluate--mean xs)))
                                    (max 1e-12 (sqrt (financial-chart-evaluate--mean
                                                      (mapcar (lambda (x) (expt (- x mu) 2)) xs)))))))
         (tstat (max 0.0 (apply #'max (cl-loop for m in means for w in omegas collect (/ (* (sqrt n) m) w)))))
         (g (cl-loop for m in means for w in omegas
                     collect (if (>= m (- (sqrt (* (/ (* w w) n) 2 loglog)))) m 0.0)))
         (tstar (cl-loop for d from 0 below (length draws)
                         collect (max 0.0 (apply #'max (cl-loop for b in boot for gk in g for w in omegas
                                                                collect (/ (* (sqrt n) (- (nth d b) gk)) w)))))))
    (list :p (/ (cl-count-if (lambda (x) (>= x tstat)) tstar) (float (length draws))) :stat tstat
          :model-p (cl-loop for m in means for w in omegas
                            collect (let ((tk (max 0.0 (/ (* (sqrt n) m) w))))
                                      (/ (cl-count-if (lambda (x) (>= x tk)) tstar) (float (length draws))))))))

(defun financial-chart-evaluate--period-series (events bars from horizon costs)
  "The per-bar return of trading EVENTS over BARS from bar FROM on, a vector.
Each event holds its direction from the open after confirmed_index for
HORIZON bars; a bar's value is the mean over open trades of their
signed open-to-open return, less COSTS on the bars a trade opens and
closes; 0 when flat (the benchmark is cash)."
  (let* ((n (length bars)) (len (max 0 (- n from 1)))
         (sum (make-vector len 0.0)) (count (make-vector len 0)) (cost (make-vector len 0.0)))
    (dolist (e events)
      (let ((dir (pcase (plist-get e :direction) ("bullish" 1) ("bearish" -1)))
            (entry (1+ (plist-get e :confirmed_index))))
        (when (and dir (>= entry from) (< (+ entry horizon) n))
          (cl-loop for tt from entry below (+ entry horizon)
                   for k = (- tt from)
                   do (cl-incf (aref sum k) (* dir (financial-chart-evaluate--ret bars tt (1+ tt))))
                   do (cl-incf (aref count k)))
          (cl-incf (aref cost (- entry from)) costs)
          (cl-incf (aref cost (- (+ entry horizon -1) from)) costs))))
    (cl-loop for k from 0 below len
             vconcat (list (if (zerop (aref count k)) 0.0
                             (/ (- (aref sum k) (aref cost k)) (aref count k)))))))

(provide 'financial-chart-evaluate)
;;; financial-chart-evaluate.el ends here
