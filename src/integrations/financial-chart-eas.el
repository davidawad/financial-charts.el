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
;;   transform   "indicator", which evaluates any registered indicator
;;               through `financial-chart-indicator-evaluate' (no copy)
;;
;; eas never refers to financial-chart; dependencies point this way.

;;; Code:

(require 'cl-lib)
(require 'eas)
(require 'financial-chart-core)
(require 'financial-chart-series)
(require 'financial-chart-indicator-api)

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
           :as (:type "string" :doc "output column (default: the indicator name)"))
 :fn #'financial-chart-eas--indicator)

(provide 'financial-chart-eas)
;;; financial-chart-eas.el ends here
