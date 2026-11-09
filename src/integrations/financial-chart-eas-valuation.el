;;; financial-chart-eas-valuation.el --- valuation stadium (football field) on eas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Renders `valuation.stadium/1', the JSON valuation-rs emits for
;; `valuation stadium': every valuation method enabled for an asset
;; class run over one asset, one row per method.  This package never
;; runs a method or fetches an input; the caller supplies the JSON,
;; bare or inside valuation-rs's {contract, ok, data} envelope.
;;
;;   (eas-resolve "valuation-stadium" (list :data STADIUM))
;;   (financial-chart-eas-render-template ...)  ; or fc-eas render
;;
;; The football field (templates/financial/valuation-stadium.json) draws
;; one bar per ok `value' row (low..high, a tick at value, a point when
;; the method has no range), coloured by approach, with a rule at the
;; reference (market) price and the summary's weighted mean and median.
;; Rows with role `diagnostic' (yields, spreads, implied growth) never
;; sit on the price axis: they are listed in a strip beneath, with the
;; skipped rows (their missing inputs), errors (their reason) and
;; not-implemented methods.
;;
;; Selecting a bar runs the `valuation-drill' action, which opens that
;; row's traceable tree (`financial-chart-valuation-drill'): value, its
;; components, the lines with formula and `from', and the assumptions
;; with provenance.  An outline buffer fits better than a template: the
;; tree is prose (formulas, source notes) of unbounded depth, which
;; wants folding and search, not marks on an axis.
;;
;; The shape is validated strictly (`financial-chart-valuation-validate'),
;; signalling `financial-chart-invalid-data' with :code, :index (the
;; row) and :field.  Codes: not_a_plist, wrong_contract, missing_field,
;; invalid_status, invalid_approach, invalid_role, not_a_number,
;; range_order, mixed_units, invalid_detail.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'outline)
(require 'eas)
(require 'financial-chart-core)
(require 'financial-chart-validate)

(defconst financial-chart-valuation-contract "valuation.stadium/1"
  "The stadium contract this package renders.")

(defconst financial-chart-valuation-approaches
  '("income" "market" "cost" "arbitrage" "contingent_claim" "statistical" "heuristic")
  "Approaches, in legend order.")

(defconst financial-chart-valuation-statuses '("ok" "skipped" "error" "not_implemented"))
(defconst financial-chart-valuation-roles '("value" "diagnostic"))

;;; Access

(defun financial-chart-valuation--get (plist key)
  "PLIST's KEY, nil when absent or null."
  (let ((v (plist-get plist key)))
    (unless (memq v '(:null :false)) v)))

(defun financial-chart-valuation--items (value)
  "VALUE (vector or list) as a list."
  (cond ((vectorp value) (append value nil))
        ((memq value '(nil :null)) nil)
        (t value)))

(defun financial-chart-valuation--stadium (data)
  "The stadium inside DATA, which is bare or valuation-rs's envelope."
  (if (and (eas-object-p data) data (plist-get data :data) (null (plist-get data :rows)))
      (plist-get data :data)
    data))

;;; Validation

(defun financial-chart-valuation--number (value index field)
  "Signal not_a_number unless VALUE is finite."
  (unless (financial-chart--finite-p value)
    (financial-chart--invalid index field "not_a_number"
                              "%s must be a finite number, got %S" field value)))

(defun financial-chart-valuation--validate-detail (detail index)
  "Signal invalid_detail unless DETAIL is a Valuation tree."
  (unless (eas-object-p detail)
    (financial-chart--invalid index "detail" "invalid_detail" "detail must be an object"))
  (unless (numberp (plist-get detail :value))
    (financial-chart--invalid index "detail.value" "invalid_detail"
                              "a valuation needs a numeric value; give detail.value"))
  (dolist (field '(:lines :assumptions :components))
    (let ((v (plist-get detail field)))
      (unless (or (null v) (vectorp v) (listp v))
        (financial-chart--invalid index (format "detail.%s" (substring (symbol-name field) 1))
                                  "invalid_detail" "must be an array"))))
  (dolist (component (financial-chart-valuation--items (plist-get detail :components)))
    (financial-chart-valuation--validate-detail (plist-get component :valuation) index)))

(defun financial-chart-valuation-validate (data)
  "Signal `financial-chart-invalid-data' unless DATA is a valuation.stadium/1.
DATA is the stadium object or its {contract, ok, data} envelope.  Return
the stadium."
  (let ((stadium (financial-chart-valuation--stadium data)))
    (unless (and (consp stadium) (keywordp (car stadium)))
      (financial-chart--invalid nil nil "not_a_plist" "a stadium is a JSON object, got %S" stadium))
    (unless (equal (plist-get stadium :contract) financial-chart-valuation-contract)
      (financial-chart--invalid nil "contract" "wrong_contract"
                                "contract must be %S, got %S; run `valuation stadium`"
                                financial-chart-valuation-contract (plist-get stadium :contract)))
    (dolist (field '(:class :rows))
      (unless (plist-member stadium field)
        (financial-chart--invalid nil (substring (symbol-name field) 1) "missing_field"
                                  "a stadium needs %s" (substring (symbol-name field) 1))))
    (unless (or (vectorp (plist-get stadium :rows)) (listp (plist-get stadium :rows)))
      (financial-chart--invalid nil "rows" "not_a_list" "rows must be an array"))
    (let ((reference (financial-chart-valuation--get stadium :reference)))
      (when reference (financial-chart-valuation--number reference nil "reference")))
    (let ((index 0) unit)
      (dolist (row (financial-chart-valuation--items (plist-get stadium :rows)))
        (financial-chart-valuation--validate-row row index)
        (when (and (equal (plist-get row :status) "ok") (equal (or (plist-get row :role) "value") "value"))
          (let ((u (plist-get row :unit)))
            (when (and unit (not (equal unit u)))
              (financial-chart--invalid index "unit" "mixed_units"
                                        "value rows share one price axis but %s is %s, not %s; split the class by unit"
                                        (plist-get row :method) u unit))
            (setq unit u)))
        (setq index (1+ index))))
    stadium))

(defun financial-chart-valuation--validate-row (row index)
  "Signal unless ROW (number INDEX) is a stadium row."
  (unless (and (consp row) (keywordp (car row)))
    (financial-chart--invalid index nil "not_a_plist" "a row is a JSON object, got %S" row))
  (dolist (field '(:method :status :approach :unit))
    (unless (stringp (plist-get row field))
      (financial-chart--invalid index (substring (symbol-name field) 1) "missing_field"
                                "row needs a string %s" (substring (symbol-name field) 1))))
  (unless (member (plist-get row :status) financial-chart-valuation-statuses)
    (financial-chart--invalid index "status" "invalid_status" "status must be one of %s, got %S"
                              (string-join financial-chart-valuation-statuses "|") (plist-get row :status)))
  (unless (member (plist-get row :approach) financial-chart-valuation-approaches)
    (financial-chart--invalid index "approach" "invalid_approach" "approach must be one of %s, got %S"
                              (string-join financial-chart-valuation-approaches "|") (plist-get row :approach)))
  (let ((role (financial-chart-valuation--get row :role)))
    (unless (or (null role) (member role financial-chart-valuation-roles))
      (financial-chart--invalid index "role" "invalid_role" "role must be value or diagnostic, got %S" role)))
  (when (equal (plist-get row :status) "ok")
    (financial-chart-valuation--number (plist-get row :value) index "value")
    (let ((low (financial-chart-valuation--get row :low)) (high (financial-chart-valuation--get row :high))
          (value (plist-get row :value)))
      (when low (financial-chart-valuation--number low index "low"))
      (when high (financial-chart-valuation--number high index "high"))
      (when (and low high (or (> low value) (< high value)))
        (financial-chart--invalid index "low" "range_order"
                                  "%s: need low <= value <= high, got %s, %s, %s"
                                  (plist-get row :method) low value high))
      (when (and (null low) high)
        (financial-chart--invalid index "low" "range_order" "%s has high but no low" (plist-get row :method)))))
  (when-let* ((detail (financial-chart-valuation--get row :detail)))
    (financial-chart-valuation--validate-detail detail index)))

;;; Formatting

(defun financial-chart-valuation--format (value unit)
  "VALUE as text for UNIT: percent for rates, grouped for money, else 4 digits."
  (cond ((not (numberp value)) "-")
        ((member unit '("rate" "ratio_pct" "yield")) (format "%.2f%%" (* 100.0 value)))
        ((member unit '("usd" "eur" "gbp")) (financial-chart-valuation--group (format "%.0f" value)))
        ((string-match-p "per_100" unit) (format "%.3f" value))
        ((string-match-p "per_share\\|price" unit) (format "%.2f" value))
        ((<= (abs value) 0.0) "0")
        (t (format "%.4g" value))))

(defun financial-chart-valuation--group (digits)
  "DIGITS (an integer string, maybe negative) with thousands commas."
  (let ((sign (if (string-prefix-p "-" digits) "-" "")) (s (string-remove-prefix "-" digits)))
    (while (string-match "\\`\\([0-9]+\\)\\([0-9]\\{3\\}\\)" s)
      (setq s (replace-match "\\1,\\2" nil nil s)))
    (while (string-match "\\([0-9]+\\)\\([0-9]\\{3\\}\\)\\(,\\|\\'\\)" s)
      (setq s (replace-match "\\1,\\2\\3" nil nil s)))
    (concat sign s)))

(defun financial-chart-valuation--approach-label (approach)
  "APPROACH with underscores as spaces."
  (replace-regexp-in-string "_" " " approach))

(defun financial-chart-valuation--weights-text (weights)
  "WEIGHTS in a compact, readable form."
  (cond ((null weights) "")
        ((numberp weights) (format "%s" weights))
        ((stringp weights) weights)
        ((and (consp weights) (keywordp (car weights)))
         (string-join
          (cl-loop for (key value) on weights by #'cddr
                   collect (format "%s %s" (substring (symbol-name key) 1)
                                   (financial-chart-valuation--weights-text value)))
          ", "))
        ((or (vectorp weights) (listp weights))
         (string-join (mapcar #'financial-chart-valuation--weights-text
                              (financial-chart-valuation--items weights)) ", "))
        (t (format "%s" weights))))

(defun financial-chart-valuation--row-unit-label (row)
  "ROW's human-readable unit label, with legacy UNIT fallback."
  (or (financial-chart-valuation--get row :unit_label)
      (financial-chart-valuation--unit-label (plist-get row :unit))))

(defun financial-chart-valuation--row-weights (row)
  "ROW's displayed weights, preferring WEIGHTS over legacy WEIGHT."
  (let ((weights (or (financial-chart-valuation--get row :weights)
                     (financial-chart-valuation--get row :weight))))
    (when weights (format "weights %s" (financial-chart-valuation--weights-text weights)))))

(defun financial-chart-valuation--missing-note (row)
  "A skipped ROW's note: the inputs it needs."
  (let ((names (mapcar (lambda (m)
                         (if (eas-object-p m)
                             (plist-get m :name)
                           (car (split-string (format "%s" m) " ("))))
                       (financial-chart-valuation--items (plist-get row :missing)))))
    (cond (names (format "needs %s" (string-join names ", ")))
          ((financial-chart-valuation--get row :reason) (format "%s" (plist-get row :reason)))
          (t "skipped"))))

(defun financial-chart-valuation--note (row)
  "ROW's note for the strip beneath the field."
  (let* ((status (plist-get row :status)) (unit (plist-get row :unit))
         (note (cond ((equal status "skipped") (financial-chart-valuation--missing-note row))
                     ((equal status "error")
                      (format "error: %s" (or (financial-chart-valuation--get row :reason) "failed")))
                     ((equal status "not_implemented") "not implemented")
                     (t (format "%s %s" (financial-chart-valuation--format (plist-get row :value) unit)
                                (financial-chart-valuation--row-unit-label row)))))
         (weights (financial-chart-valuation--row-weights row)))
    (if weights (format "%s; %s" note weights) note)))

(defun financial-chart-valuation--unit-label (unit)
  "UNIT as a short label."
  (replace-regexp-in-string "_" " " unit))

(defun financial-chart-valuation--title (stadium)
  "The header text of STADIUM: asset, class and profile."
  (format "%s  (%s, %s)"
          (or (financial-chart-valuation--get stadium :asset) "valuation")
          (plist-get stadium :class)
          (or (financial-chart-valuation--get stadium :profile) "default")))

(defun financial-chart-valuation--caption (stadium)
  "The caption of STADIUM: counts, dispersion, mean and median."
  (let* ((summary (financial-chart-valuation--get stadium :summary))
         (value-unit (car (financial-chart-valuation--items (plist-get summary :units))))
         (fmt (lambda (v) (financial-chart-valuation--format v (or value-unit "")))))
    (if (null summary)
        "no summary"
      (string-join
       (delq nil
             (list (format "%d of %d methods ok" (or (plist-get summary :n_ok) 0)
                           (or (plist-get summary :n_methods) 0))
                   (and (financial-chart-valuation--get summary :dispersion)
                        (format "dispersion %.1f%%" (* 100.0 (plist-get summary :dispersion))))
                   (and (financial-chart-valuation--get summary :weighted_mean)
                        (format "weighted mean %s" (funcall fmt (plist-get summary :weighted_mean))))
                   (and (financial-chart-valuation--get summary :median)
                        (format "median %s" (funcall fmt (plist-get summary :median))))
                   (and (financial-chart-valuation--get stadium :reference)
                        (format "market %s" (funcall fmt (plist-get stadium :reference))))))
       "; "))))

;;; Rows

(defun financial-chart-valuation--row-name (row)
  "ROW's display name."
  (or (financial-chart-valuation--get row :name) (plist-get row :method)))

(defun financial-chart-valuation--rank (rows key)
  "ROWS (plists with :value and :approach) sorted by KEY, as a list."
  (sort (copy-sequence rows)
        (lambda (a b)
          (if (eq key :approach)
              (let ((pa (cl-position (plist-get a :approach) financial-chart-valuation-approaches :test #'equal))
                    (pb (cl-position (plist-get b :approach) financial-chart-valuation-approaches :test #'equal)))
                (if (= pa pb) (> (plist-get a :value) (plist-get b :value)) (< pa pb)))
            (> (plist-get a :value) (plist-get b :value))))))

(defun financial-chart-valuation-rows (data)
  "Lower stadium DATA (validated) to the rows the valuation-stadium template draws.
Every row has band, label, series, note and method.  Band `value' rows
add low, high, value, value_label, range_label, rank_value and
rank_approach; band `marker' rows (reference, weighted_mean, median) a
value; `diagnostic', `skipped', `error' and `not_implemented' rows
the strip's note; `header' and `caption' rows the title and the
dispersion line."
  (let* ((stadium (financial-chart-valuation-validate data))
         (summary (financial-chart-valuation--get stadium :summary))
         (all (financial-chart-valuation--items (plist-get stadium :rows)))
         (ok-value (seq-filter (lambda (r) (and (equal (plist-get r :status) "ok")
                                                (equal (or (financial-chart-valuation--get r :role) "value") "value")))
                               all))
         (unit (and ok-value (plist-get (car ok-value) :unit)))
         (by-value (financial-chart-valuation--rank ok-value :value))
         (by-approach (financial-chart-valuation--rank ok-value :approach))
         (rows nil))
    (dolist (row all)
      (let* ((status (plist-get row :status))
             (diagnostic (equal (financial-chart-valuation--get row :role) "diagnostic"))
             (band (cond ((not (equal status "ok")) status) (diagnostic "diagnostic") (t "value")))
             (value (financial-chart-valuation--get row :value))
             (low (or (financial-chart-valuation--get row :low) value))
             (high (or (financial-chart-valuation--get row :high) value))
             (unit-label (financial-chart-valuation--row-unit-label row))
             (weights (or (financial-chart-valuation--get row :weights)
                          (financial-chart-valuation--get row :weight)))
             (base (list :method (plist-get row :method) :label (financial-chart-valuation--row-name row)
                         :band band :series (plist-get row :approach)
                         :approach (financial-chart-valuation--approach-label (plist-get row :approach))
                         :unit (plist-get row :unit)
                         :unit_label unit-label :weights weights
                         :weights_label (and weights
                                             (format "weights %s"
                                                     (financial-chart-valuation--weights-text weights)))
                         :note (financial-chart-valuation--note row))))
        (push (if (equal band "value")
                  (append base
                          (list :value value :low low :high high
                                :value_label (format "%s %s" (financial-chart-valuation--format value unit) unit-label)
                                :range_label (if (= low high) "point"
                                               (format "%s to %s %s" (financial-chart-valuation--format low unit)
                                                       (financial-chart-valuation--format high unit) unit-label))
                                :rank_value (cl-position row by-value :test #'eq)
                                :rank_approach (cl-position row by-approach :test #'eq)))
                (append base (and value (list :value_label (format "%s %s"
                                                                    (financial-chart-valuation--format
                                                                     value (plist-get row :unit))
                                                                    unit-label)))))
              rows)))
    (setq rows (nreverse rows))
    (let ((markers
           (delq nil
                 (list (when-let* ((v (financial-chart-valuation--get stadium :reference)))
                         (list :band "marker" :series "market price" :label "market price" :value v))
                       (when-let* ((v (financial-chart-valuation--get summary :weighted_mean)))
                         (list :band "marker" :series "weighted mean" :label "weighted mean" :value v))
                       (when-let* ((v (financial-chart-valuation--get summary :median)))
                         (list :band "marker" :series "median" :label "median" :value v))))))
      (append (list (list :band "header" :label (financial-chart-valuation--title stadium) :series "header"
                          :note (financial-chart-valuation--title stadium)))
              (mapcar (lambda (m)
                        (append m (list :note (format "%s %s" (plist-get m :label)
                                                      (financial-chart-valuation--format (plist-get m :value) unit))
                                        :value_label (financial-chart-valuation--format (plist-get m :value) unit))))
                      markers)
              rows
              (list (list :band "caption" :label "caption" :series "caption"
                          :note (financial-chart-valuation--caption stadium)))))))

;;; Drill-down: the traceable tree

(defun financial-chart-valuation--source-text (source)
  "SOURCE (an assumption's provenance) as e.g. \"fred DGS10 as of 2026-11-06\"."
  (let ((kind (financial-chart-valuation--get source :kind))
        (ref (financial-chart-valuation--get source :ref))
        (as-of (financial-chart-valuation--get source :as_of)))
    (string-join (delq nil (list (and kind (format "%s" kind)) (and ref (format "%s" ref))
                                 (and as-of (format "as of %s" as-of))))
                 " ")))

(defun financial-chart-valuation--scalar (value unit)
  "VALUE (number, string or array) as short text for UNIT."
  (cond ((eq value :false) "false")
        ((eq value t) "true")
        ((eq value :null) "null")
        ((numberp value)
         (cond ((member unit '("rate" "yield")) (format "%.4g" value))
               ((integerp value) (financial-chart-valuation--group (format "%d" value)))
               ((>= (abs value) 1000) (financial-chart-valuation--group (format "%.0f" value)))
               (t (format "%.6g" value))))
        ((vectorp value)
         (format "[%s]" (string-join (mapcar (lambda (v) (financial-chart-valuation--scalar v unit))
                                             (seq-take value 6))
                                     ", ")
                 ))
        (t (format "%s" value))))

(defun financial-chart-valuation--lookup (valuation name)
  "NAME's value in VALUATION: a line, an assumption or a component."
  (or (cl-loop for line in (financial-chart-valuation--items (plist-get valuation :lines))
               when (equal (plist-get line :key) name) return (cons (plist-get line :value) nil))
      (cl-loop for a in (financial-chart-valuation--items (plist-get valuation :assumptions))
               when (equal (plist-get a :name) name)
               return (cons (plist-get a :value) (plist-get a :unit)))
      (cl-loop for c in (financial-chart-valuation--items (plist-get valuation :components))
               when (equal (plist-get c :key) name)
               return (cons (plist-get (plist-get c :valuation) :value) nil))))

(defun financial-chart-valuation--stars (depth)
  "An outline heading prefix for DEPTH (1 is a top heading)."
  (concat (make-string depth ?*) " "))

(defun financial-chart-valuation--lookup-scoped (scope name)
  "NAME's value in the first of SCOPE's valuations (innermost first) that has it."
  (cl-some (lambda (v) (financial-chart-valuation--lookup v name)) scope))

(defun financial-chart-valuation--tree-lines (valuation depth head &optional outer)
  "Outline lines for VALUATION at heading DEPTH, headed HEAD.
OUTER is the enclosing valuations, so a component's line can show the
value of a parent's assumption it is from.
Lines, assumptions and components are sub-headings; a line shows its
formula and the values of what it is `from'."
  (let* ((stars (lambda (n) (financial-chart-valuation--stars (+ depth n))))
         (indent (make-string (+ depth 2) ?\s))
         (out (list (format "%s%s = %s%s" (funcall stars 0) head
                            (financial-chart-valuation--scalar (plist-get valuation :value) "")
                            (if-let* ((low (financial-chart-valuation--get valuation :low))
                                      (high (financial-chart-valuation--get valuation :high)))
                                (format "  (range %s to %s)" (financial-chart-valuation--scalar low "")
                                        (financial-chart-valuation--scalar high ""))
                              ""))))
         (lines (financial-chart-valuation--items (plist-get valuation :lines)))
         (assumptions (financial-chart-valuation--items (plist-get valuation :assumptions)))
         (components (financial-chart-valuation--items (plist-get valuation :components))))
    (when lines
      (push (format "%slines" (funcall stars 1)) out)
      (dolist (line lines)
        (push (format "%s%s = %s" (funcall stars 2) (plist-get line :key)
                      (financial-chart-valuation--scalar (plist-get line :value) ""))
              out)
        (push (format "%s%s" indent (plist-get line :formula)) out)
        (when-let* ((from (financial-chart-valuation--items (plist-get line :from))))
          (push (format "%sfrom %s" indent
                        (string-join
                         (mapcar (lambda (name)
                                   (let ((hit (financial-chart-valuation--lookup-scoped (cons valuation outer) name)))
                                     (if hit
                                         (format "%s = %s" name
                                                 (financial-chart-valuation--scalar (car hit) (cdr hit)))
                                       name)))
                                 from)
                         ", "))
                out))))
    (when assumptions
      (push (format "%sassumptions" (funcall stars 1)) out)
      (dolist (a assumptions)
        (let ((source (financial-chart-valuation--source-text (plist-get a :source))))
          (push (format "%s%s = %s (%s)" (funcall stars 2) (plist-get a :name)
                        (financial-chart-valuation--scalar (plist-get a :value) (plist-get a :unit))
                        (if (string-empty-p source) "no source" source))
                out))
        (let ((src (plist-get a :source)))
          (dolist (field '((:url . "url") (:transform . "transform") (:note . "note")))
            (when-let* ((v (financial-chart-valuation--get src (car field))))
              (push (format "%s%s: %s" indent (cdr field) v) out))))))
    (when components
      (push (format "%scomponents" (funcall stars 1)) out)
      (dolist (c components)
        (setq out (append (reverse (financial-chart-valuation--tree-lines
                                    (plist-get c :valuation) (+ depth 2)
                                    (format "%s [%s]" (plist-get c :key) (plist-get c :method))
                                    (cons valuation outer)))
                          out))))
    (when-let* ((warnings (financial-chart-valuation--items (plist-get valuation :warnings))))
      (push (format "%swarnings" (funcall stars 1)) out)
      (dolist (w warnings) (push (format "%s%s" indent w) out)))
    (nreverse out)))

(defun financial-chart-valuation--find-row (stadium method)
  "STADIUM's row for METHOD, or signal not_found."
  (or (seq-find (lambda (r) (equal (plist-get r :method) method))
                (financial-chart-valuation--items (plist-get stadium :rows)))
      (financial-chart--invalid nil "method" "unknown_method"
                                "no row %S; methods: %s" method
                                (string-join (mapcar (lambda (r) (plist-get r :method))
                                                     (financial-chart-valuation--items
                                                      (plist-get stadium :rows)))
                                             ", "))))

(defun financial-chart-valuation-trace (data method)
  "The traceable tree of METHOD's row in stadium DATA, an outline as a string.
Value, then its lines (formula and the values it is from), assumptions
with provenance (`risk_free_rate = 0.0431 (fred DGS10 as of
2026-11-06)') and components, each a sub-tree.  Signals
`financial-chart-invalid-data' for an unknown METHOD, or a row with no
detail (skipped, errored)."
  (let* ((stadium (financial-chart-valuation-validate data))
         (row (financial-chart-valuation--find-row stadium method))
         (detail (financial-chart-valuation--get row :detail)))
    (unless (and detail (equal (plist-get row :status) "ok"))
      (financial-chart--invalid nil "detail" "no_detail" "%s has no detail to trace: %s" method
                                (financial-chart-valuation--note row)))
    (concat (string-join
             (financial-chart-valuation--tree-lines
              detail 1 (format "%s [%s]" (financial-chart-valuation--row-name row) method))
             "\n")
            "\n")))

(defvar-local financial-chart-valuation--method nil
  "The method a trace buffer shows.")

(define-derived-mode financial-chart-valuation-trace-mode outline-mode "Valuation-Trace"
  "Outline of one valuation method's traceable tree; TAB folds, q quits."
  (setq-local outline-regexp "\\*+ ")
  (setq buffer-read-only t))

(define-key financial-chart-valuation-trace-mode-map (kbd "q") #'quit-window)

(defun financial-chart-valuation-drill (data method &optional show)
  "Open METHOD's trace of stadium DATA in a buffer; display it when SHOW.
Return the buffer, named *valuation: METHOD*."
  (let ((text (financial-chart-valuation-trace data method))
        (buffer (get-buffer-create (format "*valuation: %s*" method))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (financial-chart-valuation-trace-mode)
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (setq-local financial-chart-valuation--method method)))
    (when show (pop-to-buffer buffer))
    buffer))

(defun financial-chart-valuation--drill-action (target view)
  "The `valuation-drill' action: open the trace of TARGET's method in VIEW."
  (let* ((row (plist-get target :row))
         (method (plist-get row :method))
         (stadium (financial-chart-valuation--stadium
                   (plist-get (eas-view-bindings view) :data))))
    (unless method
      (eas-signal "INVALID_INPUT" "The clicked datum is not a method; click a bar" :action "valuation-drill"))
    (let ((buffer (condition-case err
                      (financial-chart-valuation-drill stadium method (not noninteractive))
                    (financial-chart-error
                     (eas-signal "INVALID_INPUT" (format "%s" (cadr err)) :action "valuation-drill")))))
      (format "trace %s" (buffer-name buffer)))))

(eas-register-action
 "valuation-drill" :fn #'financial-chart-valuation--drill-action
 :doc "Open the clicked method's traceable valuation tree (value, components, lines, assumptions) in an outline buffer.")

;;; Adapter

(defun financial-chart-valuation--lower (data)
  "Adapter body: validate stadium DATA with eas errors and lower it to rows."
  (condition-case err
      (eas-data-make (vconcat (financial-chart-valuation-rows data)))
    (financial-chart-error
     (let ((props (cddr err)))
       (eas-signal "SHAPE_INVALID" (format "%s" (cadr err))
                   :index (or (plist-get props :index) :null) :field (or (plist-get props :field) :null)
                   :shape "valuation-stadium" :reason (plist-get props :code))))))

(eas-register-adapter
 "valuation-stadium"
 :doc "valuation.stadium/1 (valuation-rs `valuation stadium`) as rows {band, label, series, low, high, value, note, ...}."
 :convert #'financial-chart-valuation--lower)

(provide 'financial-chart-eas-valuation)
;;; financial-chart-eas-valuation.el ends here
