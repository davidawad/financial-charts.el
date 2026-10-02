;;; financial-chart-indicators.el --- Indicator overlays, built-in indicators and named cohorts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overlay plumbing, the built-in SMA/EMA/RSI/VWAP functions, and
;; indicator cohorts (named, data-only indicator sets resolved against
;; built-ins or the core-resource indicator catalog).

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-indicator-api)

;; -----------------------------------------------------------------------
;; Indicator overlays
;; -----------------------------------------------------------------------

(defun financial-chart--compute-series (bars specs)
  "Evaluate every spec in SPECS over BARS, returning normalized series specs."
  (mapcar
   (lambda (spec)
     (list :label (or (plist-get spec :label)
                      (let ((fn (plist-get spec :fn)))
                        (if (symbolp fn) (symbol-name fn) "Indicator")))
           :glyph (or (plist-get spec :glyph) financial-chart-glyph-indicator)
           :face (or (plist-get spec :face) 'default)
           :color (plist-get spec :color)
           :series (funcall (plist-get spec :fn) bars)))
   specs))

(defun financial-chart--compute-indicator-series (bars)
  "Evaluate every `financial-chart-indicators' spec over BARS."
  (financial-chart--compute-series bars financial-chart-indicators))

(defun financial-chart--compute-oscillator-series (bars)
  "Evaluate every `financial-chart-oscillators' spec over BARS."
  (financial-chart--compute-series bars financial-chart-oscillators))

(defun financial-chart--compute-indicator-bands (bars)
  "Evaluate configured indicator bands over BARS."
  (mapcar
   (lambda (spec)
     (let ((upper (funcall (plist-get spec :upper-fn) bars))
           (lower (funcall (plist-get spec :lower-fn) bars)))
       (unless (and (= (length upper) (length bars))
                    (= (length lower) (length bars)))
         (signal 'financial-chart-error
                 (list "indicator band values must align with bars")))
       (list :upper upper :lower lower
             :upper-color (plist-get spec :upper-color)
             :lower-color (plist-get spec :lower-color)
             :opacity (plist-get spec :opacity))))
   financial-chart-indicator-bands))

(defun financial-chart--indicator-overlay (row-low row-high index series-list)
  "Return (TEXT . FACE), the last SERIES-LIST entry landing in this row
at bar INDEX, or nil when none do."
  (let (result)
    (dolist (spec series-list)
      (let ((value (nth index (plist-get spec :series))))
        (when (and value
                   (let ((scaled (financial-chart--to-scale value)))
                     (and (<= scaled row-high) (>= scaled row-low))))
          (setq result
                (cons
                 (financial-chart--cell-string
                  (plist-get spec :glyph) financial-chart-candle-width t)
                 (plist-get spec :face))))))
    result))

;; -----------------------------------------------------------------------
;; Built-in indicator functions -- ready-made `:fn' values for
;; `financial-chart-indicators'.
;;
;; SMA/EMA/VWAP are on the same price scale as the candles (dollars, or
;; whatever unit :open/:high/:low/:close are in) and overlay directly:
;;
;;   (setq financial-chart-indicators
;;         (list (list :fn #'financial-chart-sma :face 'font-lock-keyword-face)))
;;
;; RSI is a 0-100 oscillator, NOT on the price scale -- configure it in
;; `financial-chart-oscillators' or resolve a cohort that contains it.
;; -----------------------------------------------------------------------

(defun financial-chart-sma (bars &optional window field)
  "Simple moving average of BARS' FIELD (default :close) over WINDOW
bars (default 20). Returns a list the same length as BARS: nil for the
first WINDOW-1 entries (not enough data yet), then the trailing mean."
  (let* ((window (or window 20))
         (field (or field :close))
         (values (mapcar (lambda (b) (plist-get b field)) bars))
         (n (length values)))
    (cl-loop for i from 0 below n
             collect
             (if (< i (1- window))
                 nil
               (/ (apply #'+ (cl-subseq values (- i window -1) (1+ i)))
                  (float window))))))

(defun financial-chart-ema (bars &optional window field)
  "Exponential moving average of BARS' FIELD (default :close) over
WINDOW bars (default 20), seeded with a simple average of the first
WINDOW values. Returns a list the same length as BARS: nil for the
first WINDOW-1 entries."
  (let* ((window (or window 20))
         (field (or field :close))
         (values (mapcar (lambda (b) (plist-get b field)) bars))
         (n (length values))
         (alpha (/ 2.0 (1+ window)))
         (result (make-list n nil))
         (prev nil))
    (cl-loop
     for i from 0 below n
     do
     (cond
      ((< i (1- window)) nil)
      ((= i (1- window))
       (setq prev (/ (apply #'+ (cl-subseq values 0 window)) (float window)))
       (setf (nth i result) prev))
      (t
       (setq prev (+ (* alpha (nth i values)) (* (- 1 alpha) prev)))
       (setf (nth i result) prev))))
    result))

(defun financial-chart-rsi (bars &optional period field)
  "Simple-average RSI of BARS' FIELD (default :close) over PERIOD bars
\(default 14). Returns a list the same length as BARS, values in
[0,100]; nil for the first bar (no prior value to diff against) and
for any bar before PERIOD changes have accumulated. Use it as an
`:fn' in `financial-chart-oscillators', or as a cohort member, so it is
drawn in its own fixed 0-100 panel rather than on the price scale."
  (let* ((period (or period 14))
         (field (or field :close))
         (values (mapcar (lambda (b) (plist-get b field)) bars))
         (n (length values))
         (changes
          (cl-loop for i from 1 below n
                   collect (- (nth i values) (nth (1- i) values)))))
    (cons
     nil
     (cl-loop
      for i from 0 below (length changes)
      collect
      (if (< i (1- period))
          nil
        (let* ((window (cl-subseq changes (- i period -1) (1+ i)))
               (gains (cl-loop for c in window when (> c 0) sum c))
               (losses (cl-loop for c in window when (< c 0) sum (- c)))
               (avg-gain (/ gains (float period)))
               (avg-loss (/ losses (float period))))
          (if (zerop avg-loss)
              100.0
            (- 100.0 (/ 100.0 (1+ (/ avg-gain avg-loss)))))))))))

(defun financial-chart-vwap (bars)
  "Cumulative volume-weighted average price over BARS, using typical
price ((high+low+close)/3) per bar.

VWAP conventionally resets every session -- pass one day's worth of
intraday bars for a real session VWAP, not a multi-day history, unless
you deliberately want a running VWAP across the whole window. Returns a
list the same length as BARS; nil for any bar with no :volume."
  (let ((cum-pv 0.0) (cum-vol 0.0))
    (mapcar
     (lambda (b)
       (let ((vol (plist-get b :volume)))
         (if (not vol)
             nil
           (let ((typical
                  (/ (+ (plist-get b :high) (plist-get b :low)
                       (plist-get b :close))
                     3.0)))
             (setq cum-pv (+ cum-pv (* typical vol)))
             (setq cum-vol (+ cum-vol vol))
             (if (zerop cum-vol) nil (/ cum-pv cum-vol))))))
     bars)))

;; Provider-neutral registry entries for the existing calculators.  New
;; indicator modules register with this same API; broker adapters do not.
(financial-chart-register-indicator
 'sma #'financial-chart-sma
 :label "SMA" :unit :price :panel :overlay :scale :linear
 :description "Simple moving average.")
(financial-chart-register-indicator
 'ema #'financial-chart-ema
 :label "EMA" :unit :price :panel :overlay :scale :linear
 :description "Exponential moving average.")
(financial-chart-register-indicator
 'rsi #'financial-chart-rsi
 :label "RSI" :unit :percent :panel :oscillator :scale :bounded
 :bounds '(0 . 100)
 :description "Relative strength index (simple-average variant).")
(financial-chart-register-indicator
 'vwap #'financial-chart-vwap
 :label "VWAP" :unit :price :panel :overlay :scale :linear
 :description "Cumulative volume-weighted average price.")

;; -----------------------------------------------------------------------
;; Indicator cohorts -- named, reusable indicator sets
;;
;; A cohort is DATA: a `financial-chart-indicator-cohorts' entry names a
;; reusable set of members, each either a built-in overlay fn or a
;; catalog recipe id. Adding a cohort or a member is a data
;; edit -- no new code. `financial-chart-resolve-cohort' turns a cohort
;; into concrete price/oscillator specs (pure, no I/O);
;; `financial-chart-describe-cohort' explains where each member draws
;; (price / oscillator / unresolvable) with provenance and,
;; for catalog members, a probe of the configured indicator catalog.
;; -----------------------------------------------------------------------

(defcustom financial-chart-indicator-catalog-function nil
  "Function of one RECIPE-ID returning that indicator's catalog record,
or nil when there is no catalog.  The record is an alist whose
`attributes' -> `value' alist may carry `unit', `scale' and `bounds';
`financial-chart-describe-cohort' uses it to confirm a catalog member
exists and whether it is a 0-100 oscillator.  Without a catalog,
catalog members are classified from `financial-chart-recipe-evaluators'."
  :type '(choice (const :tag "No catalog" nil) function)
  :group 'financial-chart)

(define-error 'financial-chart-unresolvable-cohort
  "financial-chart: cohort member cannot be resolved" 'financial-chart-error)

(defcustom financial-chart-indicator-cohorts
  '((trend-following
     :doc "Price-overlay trend set: SMA20, SMA50, session VWAP."
     :members ((:fn financial-chart-sma :args (20) :face font-lock-keyword-face)
               (:fn financial-chart-sma :args (50) :face font-lock-type-face)
               (:fn financial-chart-vwap :face font-lock-constant-face)))
    (mean-reversion
     :doc "SMA20 price overlay plus catalog RSI-14 in the oscillator sub-panel."
     :members ((:fn financial-chart-sma :args (20) :face font-lock-keyword-face)
               (:indicator "finance.market.rsi-14" :params (:period 14))))
    (momentum
     :doc "Catalog RSI-14 only, drawn in the 0-100 oscillator sub-panel."
     :members ((:indicator "finance.market.rsi-14" :params (:period 14)))))
  "Named indicator cohorts for price and oscillator panels.
Each entry is (COHORT-NAME :doc DOC :members (MEMBER...)).  A MEMBER is
either a built-in spec (:fn FN :args ARGS :face F :glyph G) -- FN a
`financial-chart-*' indicator applied to bars plus ARGS -- or a
catalog-backed spec (:indicator RECIPE-ID :params PLIST :face F :glyph G)
resolved through `financial-chart-recipe-evaluators'.  Adding a cohort or
a member is a pure data edit; resolve with `financial-chart-resolve-cohort'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'financial-chart)

(defcustom financial-chart-recipe-evaluators
  '(("finance.market.sma" :fn financial-chart-sma :arg-keys (:window))
    ("finance.market.rsi-14" :fn financial-chart-rsi :arg-keys (:period)
     :oscillator t))
  "Translation table from a core-resource indicator RECIPE-ID to a local
built-in evaluator.  Each entry is (RECIPE-ID :fn FN :arg-keys (KEY...)
[:oscillator BOOL]).  `financial-chart-resolve-cohort' maps a catalog
member's :params through :arg-keys into FN's trailing arguments;
:oscillator flags a 0-100 sub-panel indicator.  A recipe id absent from
this table is UNRESOLVABLE -- never
silently dropped.  Extending coverage is a data edit: add one entry.
`resource indicator get' returns a recipe DAG, not an elisp function, so
this explicit id-keyed table is the honest boundary (the transform chain
is not reachable through the front door), not a general recipe evaluator."
  :type '(alist :key-type string :value-type plist)
  :group 'financial-chart)

(defconst financial-chart--oscillator-fns '(financial-chart-rsi)
  "Built-in indicator functions whose cohort specs use the oscillator panel.")

(defun financial-chart--cohort (name)
  "Return cohort NAME's plist (:doc/:members) from
`financial-chart-indicator-cohorts', or nil when NAME is undefined."
  (cdr (assq name financial-chart-indicator-cohorts)))

(defun financial-chart--cohort-overlay-spec (member fn args &optional oscillator)
  "Build a concrete panel spec: FN curried over ARGS into a one-argument
function, carrying MEMBER's :face/:glyph and marking OSCILLATOR specs."
  (let ((face (plist-get member :face))
        (glyph (plist-get member :glyph)))
    (append
     (list :fn (lambda (bars) (apply fn bars args)))
     (when oscillator (list :panel 'oscillator))
     (when face (list :face face))
     (when glyph (list :glyph glyph)))))

(defun financial-chart--resolve-member (member)
  "Classify MEMBER, returning (STATUS . DETAIL).  STATUS is `resolved'
\(DETAIL a concrete spec, tagged `:panel oscillator' when appropriate)
or `unresolvable' (DETAIL a reason string naming the fix). Pure: no I/O."
  (cond
   ((plist-member member :fn)
    (let ((fn (plist-get member :fn))
          (args (plist-get member :args)))
      (cond
       ((not (fboundp fn))
        (cons 'unresolvable
              (format "built-in fn `%s' is undefined -- load financial-chart.el \
or fix the cohort member" fn)))
       (t (cons 'resolved
                (financial-chart--cohort-overlay-spec
                 member fn args (memq fn financial-chart--oscillator-fns)))))))
   ((plist-member member :indicator)
    (let* ((id (plist-get member :indicator))
           (params (plist-get member :params))
           (evaluator (cdr (assoc id financial-chart-recipe-evaluators))))
      (cond
       ((null evaluator)
        (cons 'unresolvable
              (format "no local evaluator for indicator `%s' -- add an entry to \
`financial-chart-recipe-evaluators' or drop the member" id)))
       (t
        (let* ((fn (plist-get evaluator :fn))
               (arg-keys (plist-get evaluator :arg-keys))
               (oscillator (or (plist-get evaluator :oscillator)
                               (memq fn financial-chart--oscillator-fns)))
               (args (mapcar (lambda (k) (plist-get params k)) arg-keys)))
          (if (fboundp fn)
              (cons 'resolved
                    (financial-chart--cohort-overlay-spec
                     member fn args oscillator))
            (cons 'unresolvable
                  (format "evaluator for `%s' maps to undefined fn `%s'" id fn))))))))
   (t (cons 'unresolvable
            (format "member is neither an :fn built-in nor an :indicator catalog \
spec: %S" member)))))

(defun financial-chart-resolve-cohort (name)
  "Resolve cohort NAME to concrete specs for price and oscillator panels.
Oscillator specs carry `:panel oscillator'; ordinary specs omit :panel.
Signal `financial-chart-unresolvable-cohort' if any member cannot be
resolved. Pure: no I/O, so builtin cohorts work without a catalog."
  (let ((cohort (financial-chart--cohort name)))
    (unless cohort
      (signal 'financial-chart-unresolvable-cohort
              (list (format "no cohort named `%s' in \
`financial-chart-indicator-cohorts'" name))))
    (let (specs)
      (dolist (member (plist-get cohort :members))
        (let ((res (financial-chart--resolve-member member)))
          (pcase (car res)
            ('resolved (push (cdr res) specs))
            ('unresolvable
             (signal 'financial-chart-unresolvable-cohort
                     (list (format "cohort `%s': %s" name (cdr res))))))))
      (nreverse specs))))

(defun financial-chart-list-cohorts ()
  "Return a summary of every cohort in `financial-chart-indicator-cohorts':
one (NAME :doc DOC :members N :resolvable R :oscillators O :excluded E
:unresolvable U) per cohort.  OSCILLATORS are resolved members assigned to
the separate panel; EXCLUDED remains as a zero-valued compatibility field.
Pure: uses static classification, not the live catalog."
  (mapcar
   (lambda (entry)
     (let* ((name (car entry))
            (plist (cdr entry))
            (members (plist-get plist :members))
            (r 0) (o 0) (u 0))
       (dolist (m members)
         (let ((res (financial-chart--resolve-member m)))
           (pcase (car res)
             ('resolved
              (setq r (1+ r))
              (when (eq (plist-get (cdr res) :panel) 'oscillator)
                (setq o (1+ o))))
             ('unresolvable (setq u (1+ u))))))
       (list name :doc (plist-get plist :doc)
             :members (length members) :resolvable r :oscillators o :excluded 0
             :unresolvable u)))
   financial-chart-indicator-cohorts))

(defun financial-chart--catalog-value-oscillator-p (value)
  "Non-nil when a catalog record's VALUE alist describes a bounded
oscillator (RSI-style 0-100) rather than a price-scale series.  Probes
`bounds'/`unit', so overlay safety comes from the record itself."
  (or (and (alist-get 'bounds value) t)
      (and (member (alist-get 'unit value) '("1" "index" "score" "percent")) t)))

(defun financial-chart--probe-catalog-member (id)
  "Probe the indicator catalog for ID via
`financial-chart-indicator-catalog-function', returning (:catalog-live
LIVE :probed-oscillator OSC :catalog-detail STR).  With no catalog
function LIVE/OSC are `:unknown' and callers fall back to the static
`financial-chart-recipe-evaluators' classification."
  (if (not financial-chart-indicator-catalog-function)
      (list :catalog-live :unknown :probed-oscillator :unknown
            :catalog-detail "no indicator catalog configured; \
using static classification")
    (let ((record (ignore-errors
                    (funcall financial-chart-indicator-catalog-function id))))
      (if (not record)
          (list :catalog-live :false :probed-oscillator :unknown
                :catalog-detail (format "indicator `%s' not found in the live \
catalog" id))
        (let* ((attributes (alist-get 'attributes record))
               (value (alist-get 'value attributes)))
          (list :catalog-live t
                :probed-oscillator
                (and (financial-chart--catalog-value-oscillator-p value) t)
                :catalog-detail
                (format "live catalog: unit=%s scale=%s bounds=%s"
                        (alist-get 'unit value)
                        (alist-get 'scale value)
                        (alist-get 'bounds value))))))))

(defun financial-chart--describe-member (member)
  "Return a disposition plist for MEMBER: (:member M :kind KIND :status
STATUS :detail DETAIL :panel PANEL [catalog probe keys]).  Resolved
members name their price or oscillator panel. Catalog members carry a
live probe from `financial-chart--probe-catalog-member'."
  (let* ((res (financial-chart--resolve-member member))
         (panel (and (eq (car res) 'resolved)
                     (or (plist-get (cdr res) :panel) 'price)))
         (detail (if panel
                     (format "resolves to the %s panel"
                             (if (eq panel 'price) "price" "oscillator"))
                   (cdr res)))
         (description (list :member member :status (car res)
                            :detail detail :panel panel)))
    (cond
     ((plist-member member :fn)
      (plist-put description :kind 'builtin))
     ((plist-member member :indicator)
      (append
       (plist-put description :kind 'catalog)
       (financial-chart--probe-catalog-member (plist-get member :indicator))))
     (t (plist-put description :kind 'unknown)))))

(defun financial-chart-describe-cohort (name)
  "Describe cohort NAME: provenance plus each member's disposition.
Returns (:name NAME :doc DOC :provenance PROV :members (MDESC...)); each
MDESC names its :panel (`price' or `oscillator') when resolved. For
catalog members this MAY call
`financial-chart-indicator-catalog-function' (the only I/O in this layer,
read-only; with none set, static classification)
to confirm live catalog validity and PROBE overlay-safety from the
record's value.  Signals `financial-chart-unresolvable-cohort' when NAME
is undefined."
  (let ((cohort (financial-chart--cohort name)))
    (unless cohort
      (signal 'financial-chart-unresolvable-cohort
              (list (format "no cohort named `%s'" name))))
    (list :name name
          :doc (plist-get cohort :doc)
          :provenance
          (format "defcustom `financial-chart-indicator-cohorts' (%s)"
                  (or (ignore-errors
                        (symbol-file 'financial-chart-indicator-cohorts 'defvar))
                      "financial-chart.el"))
          :members
          (mapcar #'financial-chart--describe-member
                  (plist-get cohort :members)))))

(defun financial-chart-cohort-doctor-checks ()
  "Doctor rows for the cohorts, one per cohort:
\(:name NAME :status pass|fail :detail D :remediation R).
PASS iff the cohort resolves without a `financial-chart-unresolvable-cohort'
error."
  (mapcar
   (lambda (entry)
     (let ((name (car entry)))
       (condition-case err
           (let* ((specs (financial-chart-resolve-cohort name))
                  (oscillators
                   (cl-count 'oscillator specs
                             :key (lambda (spec) (plist-get spec :panel))))
                  (overlays (- (length specs) oscillators)))
             (list :name (format "cohort:%s" name) :status 'pass
                   :detail (format "resolves to %d price-overlay and %d oscillator spec(s)"
                                   overlays oscillators)
                   :remediation nil))
         (financial-chart-unresolvable-cohort
          (list :name (format "cohort:%s" name) :status 'fail
                :detail (error-message-string err)
                :remediation "fix the cohort member or add a \
`financial-chart-recipe-evaluators' entry")))))
   financial-chart-indicator-cohorts))

;; Built-in families are separate modules so additions stay isolated and
;; local.  All consume bar/v1 and register against the same output API.
(require 'financial-chart-trend-indicators)
(require 'financial-chart-momentum)
(require 'financial-chart-indicator-oscillators)
(require 'financial-chart-volatility)
(require 'financial-chart-trend-strength)
(require 'financial-chart-volume-indicators)

(provide 'financial-chart-indicators)
;;; financial-chart-indicators.el ends here
