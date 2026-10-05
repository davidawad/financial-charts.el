;;; easel-transform.el --- the native Vega-Lite transforms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L1.  `easel-transform-run' applies a Vega-Lite transform array to
;; rows: filter, calculate, fold, timeUnit and bin here; aggregate,
;; joinaggregate and window in easel-transform-agg.el; domain
;; transforms through the registry.  Anything else is
;; UNSUPPORTED_FEATURE with the transform's JSON path.
;;
;; ENV is a plist of param values keyed by param name; expressions read
;; it, and `easel-transform-param-predicate' decides {"param": NAME}
;; filters (the runtime installs selection semantics there).

;;; Code:

(require 'easel-core)
(require 'easel-time)
(require 'easel-expr)
(require 'easel-transform-agg)
(require 'easel-transform-domain)

(defvar easel-transform-param-predicate
  (lambda (_param _row _env empty) empty)
  "Function (PARAM ROW ENV EMPTY) deciding a {\"param\": ...} predicate.
EMPTY is what an empty selection means.  The runtime replaces this
with Vega-Lite selection semantics.")

;;; filter

(defun easel-transform--field-value (pred row)
  "Value of PRED's field in ROW, truncated by PRED's timeUnit if any."
  (let ((value (plist-get row (easel-key (plist-get pred :field)))))
    (if-let* ((unit (plist-get pred :timeUnit)))
        (easel-time-unit-floor unit value)
      value)))

(defun easel-transform--pred-value (pred key)
  "PRED's comparison value under KEY, truncated by PRED's timeUnit if any."
  (let ((v (plist-get pred key)) (unit (plist-get pred :timeUnit)))
    (cond ((null unit) v)
          ((vectorp v) (vconcat (mapcar (lambda (x) (easel-time-unit-floor unit x)) v)))
          (t (easel-time-unit-floor unit v)))))

(defun easel-transform-predicate (pred row env)
  "Non-nil when ROW satisfies Vega-Lite predicate PRED under ENV."
  (cond
   ((stringp pred) (easel-expr-truthy (easel-expr-evaluate pred row env)))
   ((plist-get pred :and) (seq-every-p (lambda (p) (easel-transform-predicate p row env))
                                       (plist-get pred :and)))
   ((plist-get pred :or) (seq-some (lambda (p) (easel-transform-predicate p row env))
                                   (plist-get pred :or)))
   ((plist-member pred :not) (not (easel-transform-predicate (plist-get pred :not) row env)))
   ((plist-get pred :param)
    (funcall easel-transform-param-predicate (plist-get pred :param) row env
             (not (eq (plist-get pred :empty) :false))))
   ((plist-get pred :field)
    (let ((v (easel-transform--field-value pred row)))
      (cond
       ((plist-member pred :equal) (easel-expr--equal v (easel-transform--pred-value pred :equal)))
       ((plist-member pred :lt) (and (numberp v) (< v (easel-transform--pred-value pred :lt))))
       ((plist-member pred :lte) (and (numberp v) (<= v (easel-transform--pred-value pred :lte))))
       ((plist-member pred :gt) (and (numberp v) (> v (easel-transform--pred-value pred :gt))))
       ((plist-member pred :gte) (and (numberp v) (>= v (easel-transform--pred-value pred :gte))))
       ((plist-member pred :range)
        (let ((r (plist-get pred :range)))
          (and (numberp v) (<= (aref r 0) v (aref r 1)))))
       ((plist-member pred :oneOf) (seq-some (lambda (x) (easel-expr--equal v x)) (plist-get pred :oneOf)))
       ((plist-member pred :valid)
        (eq (not (memq v '(nil :null))) (easel-true-p (plist-get pred :valid))))
       (t (easel-signal "UNSUPPORTED_FEATURE" "Field predicate needs equal, lt, lte, gt, gte, range, oneOf or valid"
                        :feature "predicate/other")))))
   (t (easel-signal "INVALID_INPUT" "A filter is an expression string or a predicate object"))))

;;; timeUnit

(defconst easel-time-unit-parts
  '("year" "quarter" "month" "date" "day" "hours" "minutes" "seconds" "milliseconds")
  "Vega-Lite time unit components easel supports (week is not supported).")

(defun easel-time-unit-components (unit)
  "Return the component list of time UNIT such as \"yearmonthdate\"."
  (let ((rest (string-remove-prefix "utc" unit)) parts)
    (while (not (string-empty-p rest))
      (let ((part (seq-find (lambda (p) (string-prefix-p p rest))
                            '("year" "quarter" "month" "date" "day" "hours" "minutes"
                              "seconds" "milliseconds"))))
        (unless part
          (easel-signal "UNSUPPORTED_FEATURE" (format "Time unit %s is not supported" unit)
                        :feature (concat "timeUnit/" unit)))
        (push part parts)
        (setq rest (substring rest (length part)))))
    parts))

(defun easel-time-unit-floor (unit value)
  "Truncate date VALUE to time UNIT; return epoch ms or `:null'.
Absent components take Vega's defaults (year 2012, January, day 1)."
  (let ((ms (easel-time-parse value))
        (parts (easel-time-unit-components unit)))
    (if (null ms) :null
      (let* ((f (easel-time-fields ms))
             (has (lambda (p) (member p parts)))
             (month (cond ((funcall has "month") (plist-get f :month))
                          ((funcall has "quarter") (1+ (* 3 (/ (1- (plist-get f :month)) 3))))
                          (t 1))))
        (easel-time-ms (if (funcall has "year") (plist-get f :year) 2012)
                       month
                       (cond ((funcall has "date") (plist-get f :day))
                             ((funcall has "day") (1+ (plist-get f :weekday)))
                             (t 1))
                       (if (funcall has "hours") (plist-get f :hours) 0)
                       (if (funcall has "minutes") (plist-get f :minutes) 0)
                       (if (funcall has "seconds") (plist-get f :seconds) 0)
                       (if (funcall has "milliseconds") (plist-get f :milliseconds) 0))))))

;;; bin

(defun easel-bin-params (extent &optional opts)
  "Vega's bin parameters (:start :stop :step) for EXTENT and OPTS.
OPTS is a Vega-Lite bin object (:maxbins :step :nice :base :minstep)."
  (let* ((maxb (or (plist-get opts :maxbins) 10))
         (base (or (plist-get opts :base) 10))
         (logb (log base))
         (lo (float (aref extent 0))) (hi (float (aref extent 1)))
         (span (let ((s (- hi lo))) (if (zerop s) (if (zerop lo) 1.0 (abs lo)) s)))
         (minstep (or (plist-get opts :minstep) 0))
         (step (plist-get opts :step)))
    (unless step
      (let ((level (ceiling (/ (log maxb) logb))))
        (setq step (max minstep (expt (float base) (- (round (/ (log span) logb)) level))))
        (while (> (ceiling (/ span step)) maxb) (setq step (* step base)))
        (dolist (div '(5 2))
          (let ((v (/ step div)))
            (when (and (>= v minstep) (<= (/ span v) maxb)) (setq step v))))))
    (let* ((v (log step))
           (precision (if (>= v 0) 0 (1+ (truncate (/ (- v) logb)))))
           (eps (expt (float base) (- (1+ precision)))))
      (unless (eq (plist-get opts :nice) :false)
        (let ((floor-v (* (floor (+ (/ lo step) eps)) step)))
          (setq lo (if (< lo floor-v) (- floor-v step) floor-v)
                hi (* (ceiling (/ hi step)) step))))
      (list :start lo :stop (if (= hi lo) (+ lo step) hi) :step step))))

(defun easel-bin-value (params v)
  "Return the bin start for number V under bin PARAMS."
  (let ((start (plist-get params :start)) (stop (plist-get params :stop))
        (step (plist-get params :step)))
    (if (not (numberp v)) :null
      (let ((v (max start (min v (- stop step)))))
        (+ start (* step (floor (+ 1e-14 (/ (- v start) step)))))))))

(defun easel-transform-bin (tr rows)
  "Apply bin transform TR to ROWS."
  (let* ((field (easel-key (plist-get tr :field)))
         (opts (if (easel-object-p (plist-get tr :bin)) (plist-get tr :bin) nil))
         (as (plist-get tr :as))
         (as0 (easel-key (if (vectorp as) (aref as 0) as)))
         (as1 (easel-key (if (vectorp as) (aref as 1) (concat as "_end"))))
         (values (seq-filter #'numberp (seq-map (lambda (r) (plist-get r field)) rows)))
         (extent (or (plist-get opts :extent)
                     (if values (vector (apply #'min values) (apply #'max values)) [0 1])))
         (params (easel-bin-params extent opts)))
    (seq-map (lambda (row)
               (let ((b (easel-bin-value params (plist-get row field))))
                 (append row (list as0 b as1 (if (numberp b) (+ b (plist-get params :step)) :null)))))
             rows)))

;;; dispatch

(defun easel-transform-run (transforms rows &optional env path)
  "Apply the Vega-Lite TRANSFORMS array to ROWS and return new rows.
ENV is a plist of param values; PATH the array's JSON pointer."
  (let ((i -1) (rows (if (vectorp rows) rows (vconcat rows))))
    (seq-doseq (tr transforms)
      (setq i (1+ i))
      (let ((tpath (format "%s/%d" (or path "/transform") i)))
        (setq rows
              (vconcat
               (cond
                ((plist-get tr :x-easel:transform) (easel-transform-apply-domain tr rows tpath))
                ((plist-member tr :filter)
                 (seq-filter (lambda (row) (easel-transform-predicate (plist-get tr :filter) row env))
                             rows))
                ((plist-get tr :calculate)
                 (let ((as (easel-key (plist-get tr :as))) (expr (plist-get tr :calculate)))
                   (seq-map (lambda (row) (easel-plist-put row as (easel-expr-evaluate expr row env)))
                            rows)))
                ((plist-get tr :fold)
                 (let* ((as (or (plist-get tr :as) ["key" "value"]))
                        (k (easel-key (aref as 0))) (v (easel-key (aref as 1))))
                   (apply #'append
                          (seq-map (lambda (row)
                                     (seq-map (lambda (f) (append row (list k f v (plist-get row (easel-key f)))))
                                              (plist-get tr :fold)))
                                   rows))))
                ((plist-get tr :timeUnit)
                 (let ((field (easel-key (plist-get tr :field))) (as (easel-key (plist-get tr :as)))
                       (unit (plist-get tr :timeUnit)))
                   (seq-map (lambda (row) (easel-plist-put row as (easel-time-unit-floor
                                                                    unit (plist-get row field))))
                            rows)))
                ((plist-get tr :bin) (easel-transform-bin tr rows))
                ((plist-get tr :aggregate) (easel-transform-aggregate tr rows tpath))
                ((plist-get tr :joinaggregate) (easel-transform-joinaggregate tr rows tpath))
                ((plist-get tr :window) (easel-transform-window tr rows tpath))
                (t (easel-signal "UNSUPPORTED_FEATURE"
                                 (format "Transform %s is not in the native subset"
                                         (if (consp tr) (easel-key-name (car tr)) tr))
                                 :path tpath)))))))
    rows))

(provide 'easel-transform)
;;; easel-transform.el ends here
