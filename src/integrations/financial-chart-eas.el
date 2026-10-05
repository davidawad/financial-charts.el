;;; financial-chart-eas.el --- financial-chart's shapes and indicators on eas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The domain half of the eas engine for financial-chart.  It
;; registers, through eas's public registries only:
;;
;;   adapters    series payoff labeled matrix order-book payoff-curves
;;               multi-series, each validated by financial-chart's own
;;               shape validator and lowered to tidy rows
;;   transforms  "indicator", which evaluates any registered indicator
;;               through `financial-chart-indicator-evaluate' (no copy);
;;               "values", a precomputed column; and "volume-profile",
;;               which bins bars by price through
;;               `financial-chart-matrix--volume-data'
;;   templates   templates/financial/, whose templates need those
;;               transforms (templates/ holds the pure Vega-Lite ones)
;;
;; eas never refers to financial-chart; dependencies point this way.

;;; Code:

(require 'cl-lib)
(require 'eas)
(require 'financial-chart-core)
(require 'financial-chart-series)
(require 'financial-chart-indicator-api)
(require 'financial-chart-matrix)

(defvar financial-chart-shapes)

(defun financial-chart-eas--validate (shape data)
  "Validate DATA with SHAPE's financial-chart validator, failing as eas data."
  (let ((validator (plist-get (alist-get shape financial-chart-shapes) :validator)))
    (condition-case err
        (when (and data validator) (funcall validator data))
      (financial-chart-error
       (let ((props (cddr err)))
         (eas-signal "SHAPE_INVALID" (format "%s" (cadr err))
                       :index (plist-get props :index) :field (plist-get props :field)
                       :shape (symbol-name shape)))))))

(defun financial-chart-eas--series-rows (series &rest extra)
  "Lower SERIES to rows (:x X :y Y . EXTRA); bare numbers get their index as X."
  (let ((index -1) rows)
    (seq-doseq (point series)
      (setq index (1+ index))
      (when-let* ((y (financial-chart-series--point-y point)))
        (push (append extra (list :x (or (financial-chart-series--point-x point) index) :y y))
              rows)))
    (nreverse rows)))

(defmacro financial-chart-eas--adapter (shape doc &rest lower)
  "Register an eas adapter for SHAPE with DOC; LOWER uses `data'."
  (declare (indent 2))
  `(eas-register-adapter
    ,(symbol-name shape) :doc ,doc
    :example (plist-get (alist-get ',shape financial-chart-shapes) :example)
    :convert (lambda (data)
               (financial-chart-eas--validate ',shape data)
               (eas-data-make (vconcat (progn ,@lower))))))

(financial-chart-eas--adapter series
    "financial-chart series (numbers, (X Y) or (X . Y)) as rows {x, y}."
  (financial-chart-eas--series-rows data))

(financial-chart-eas--adapter payoff
    "financial-chart payoff (PRICE PNL) pairs as rows {price, pnl}."
  (mapcar (lambda (p) (list :price (car p) :pnl (cadr p))) data))

(financial-chart-eas--adapter labeled
    "financial-chart (LABEL . VALUE) conses as rows {label, value}."
  (mapcar (lambda (p) (list :label (format "%s" (car p)) :value (cdr p))) data))

(financial-chart-eas--adapter matrix
    "financial-chart matrix as long rows {row, column, value}."
  (let ((columns (or (plist-get data :column-labels) (plist-get data :labels))))
    (cl-loop for label in (plist-get data :labels)
             for row in (plist-get data :rows)
             append (cl-loop for column in columns for value in row
                             collect (list :row (format "%s" label)
                                           :column (format "%s" column) :value value)))))

(financial-chart-eas--adapter order-book
    "financial-chart order book as rows {side, price, size}."
  (append (mapcar (lambda (l) (list :side "bid" :price (car l) :size (cadr l)))
                  (plist-get data :bids))
          (mapcar (lambda (l) (list :side "ask" :price (car l) :size (cadr l)))
                  (plist-get data :asks))))

(financial-chart-eas--adapter payoff-curves
    "financial-chart labeled payoff curves as rows {curve, price, pnl}."
  (cl-loop for (label . curve) in data
           append (mapcar (lambda (p) (list :curve (format "%s" label) :price (car p) :pnl (cadr p)))
                          curve)))

(financial-chart-eas--adapter multi-series
    "financial-chart (LABEL . SERIES) entries as rows {series, x, y}."
  (cl-loop for (label . series) in data
           append (financial-chart-eas--series-rows series :series (format "%s" label))))

;;; indicator transform

(defun financial-chart-eas--pick-output (outputs output name)
  "The OUTPUTS entry named OUTPUT (else the first) of indicator NAME."
  (if (not (and output (not (equal output ""))))
      (car outputs)
    (or (cl-find output outputs :key (lambda (o) (format "%s" (plist-get o :name)))
                 :test #'equal)
        (eas-signal "INVALID_INPUT"
                    (format "Indicator %s has no output %s; outputs: %s" name output
                            (mapconcat (lambda (o) (format "%s" (plist-get o :name))) outputs ", "))
                    :transform "indicator" :indicator (symbol-name name) :field output))))

(defun financial-chart-eas--indicator (rows params)
  "Add indicator PARAMS's output columns to bar ROWS."
  (let* ((name (intern (plist-get params :name)))
         (as (or (plist-get params :as) (plist-get params :name)))
         (bars (append rows nil))
         (result (condition-case err
                     (apply #'financial-chart-indicator-evaluate name bars
                            (append (plist-get params :params) nil))
                   (financial-chart-error
                    (eas-signal "INVALID_INPUT" (format "Indicator %s: %s" name (cadr err))
                                  :transform "indicator" :indicator (plist-get params :name)))))
         (outputs (if (and (consp result) (keywordp (car result))) (list result) result))
         (outputs (if (eas-true-p (plist-get params :single))
                      (list (financial-chart-eas--pick-output outputs (plist-get params :output)
                                                              name))
                    outputs))
         (columns (mapcar (lambda (out)
                            (cons (eas-key (if (cdr outputs)
                                                 (format "%s_%s" as (plist-get out :name))
                                               as))
                                  (vconcat (plist-get out :values))))
                          outputs)))
    (seq-map-indexed
     (lambda (row i)
       (append row (cl-loop for (key . values) in columns
                            append (list key (or (aref values i) :null)))))
     rows)))

(eas-register-transform
 "indicator"
 :doc "Any financial-chart indicator over bar/v1 rows; multi-output indicators add AS_OUTPUT columns."
 :schema '(:name (:type "string" :required t :doc "indicator name, e.g. rsi (see describe)")
           :params (:type "array" :default [] :doc "positional indicator parameters")
           :as (:type "string" :doc "output column (default: the indicator name)")
           :single (:type "boolean" :doc "keep one output (OUTPUT, else the first) as column AS")
           :output (:type "string" :doc "with single: the output to keep, e.g. upper"))
 :fn #'financial-chart-eas--indicator)

;;; values transform

(defun financial-chart-eas--values (rows params)
  "Add PARAMS's :values, one per row, to ROWS as column :as."
  (let ((values (plist-get params :values))
        (key (eas-key (plist-get params :as))))
    (unless (= (length values) (length rows))
      (eas-signal "INVALID_INPUT"
                    (format "values has %d entries for %d rows; give one per row (null for none)"
                            (length values) (length rows))
                    :transform "values" :field (plist-get params :as)))
    (seq-map-indexed (lambda (row i) (append row (list key (or (aref values i) :null)))) rows)))

(eas-register-transform
 "values"
 :doc "A precomputed series as a new column, one value per row (null for none), e.g. an overlay computed elsewhere."
 :schema '(:values (:type "array" :required t :doc "one value per row")
           :as (:type "string" :required t :doc "the new column"))
 :fn #'financial-chart-eas--values)

;;; volume-profile transform

(defun financial-chart-eas--volume-profile (rows params)
  "Replace bar ROWS by one row per price level of PARAMS's :bins.
Each row is {bin, low, high, volume, poc, last_close}; poc marks the
level with the most volume."
  (let ((profile (condition-case err
                     (financial-chart-matrix--volume-data (append rows nil) (plist-get params :bins))
                   (financial-chart-error
                    (eas-signal "INVALID_INPUT" (format "volume-profile: %s" (cadr err))
                                  :transform "volume-profile" :field "bins")))))
    (when profile
      (cl-loop for volume in (plist-get profile :volumes)
               for i from 0
               for low = (+ (plist-get profile :low) (* i (plist-get profile :step)))
               collect (list :bin i :low low :high (+ low (plist-get profile :step))
                             :volume volume
                             :poc (if (eql i (plist-get profile :poc)) t :false)
                             :last_close (plist-get profile :last-close))))))

(eas-register-transform
 "volume-profile"
 :doc "OHLCV bar/v1 rows to one row per price level {bin, low, high, volume, poc, last_close}; each bar's volume spread over its low-high range."
 :schema '(:bins (:type "integer" :default 24 :doc "price levels"))
 :fn #'financial-chart-eas--volume-profile)

;;; templates

(defconst financial-chart-eas-template-directory
  (expand-file-name "../../templates/financial"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Templates that need financial-chart's domain transforms.")

(add-to-list 'eas-template-directories financial-chart-eas-template-directory t)
(eas-template-reload)

(provide 'financial-chart-eas)
;;; financial-chart-eas.el ends here
