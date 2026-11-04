;;; financial-chart-scenario-heatmap.el --- Spot/IV scenario P&L grids -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;;; Commentary:

;; Spot shock by implied-volatility shock P&L grids, with Greeks shown
;; for each scenario cell.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)
(require 'financial-chart-validate)

(defconst financial-chart-scenario-heatmap--fields
  '(spot-shock iv-shock pnl delta gamma vega theta)
  "Required numeric fields in each scenario heatmap cell.")

(defun financial-chart-scenario-heatmap--validate (data)
  "Signal unless DATA is a list of unique spot/IV shock grid cells."
  (unless (and (proper-list-p data) data)
    (financial-chart--invalid nil "data" "not_a_list"
                              "scenario heatmap data must be a non-empty list of cell plists"))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for cell in data for index from 0
             do (unless (and (proper-list-p cell) (cl-evenp (length cell))
                             (keywordp (car cell)))
                  (financial-chart--invalid index "cell" "not_a_plist"
                                            "cell must be a plist with spot shock, IV shock, P/L and Greeks"))
             do (dolist (field financial-chart-scenario-heatmap--fields)
                  (let* ((key (intern (concat ":" (symbol-name field))))
                         (value (plist-get cell key)))
                    (unless (plist-member cell key)
                      (financial-chart--invalid index (symbol-name field) "missing_field"
                                                "required scenario field %s is missing" key))
                    (unless (financial-chart--finite-p value)
                      (financial-chart--invalid index (symbol-name field) "not_a_number"
                                                "%s must be a finite number, got %S" key value))))
             do (let ((coordinate (list (plist-get cell :spot-shock)
                                        (plist-get cell :iv-shock))))
                  (when (gethash coordinate seen)
                    (financial-chart--invalid index "spot-shock/iv-shock" "duplicate_cell"
                                              "each spot/IV shock coordinate must occur once"))
                  (puthash coordinate t seen)))))

(defun financial-chart-scenario-heatmap--values (data _props)
  "P/L values in scenario grid DATA."
  (mapcar (lambda (cell) (plist-get cell :pnl)) data))

(defconst financial-chart-scenario-heatmap--example
  '((:spot-shock -10 :iv-shock -20 :pnl -1450 :delta -72 :gamma 1.8 :vega 31 :theta -12)
    (:spot-shock -10 :iv-shock 0 :pnl -930 :delta -78 :gamma 2.0 :vega 34 :theta -13)
    (:spot-shock -10 :iv-shock 20 :pnl -280 :delta -85 :gamma 2.2 :vega 37 :theta -14)
    (:spot-shock 0 :iv-shock -20 :pnl -410 :delta 12 :gamma 1.5 :vega 29 :theta -11)
    (:spot-shock 0 :iv-shock 0 :pnl 0 :delta 18 :gamma 1.7 :vega 32 :theta -12)
    (:spot-shock 0 :iv-shock 20 :pnl 650 :delta 25 :gamma 1.9 :vega 35 :theta -13)
    (:spot-shock 10 :iv-shock -20 :pnl 790 :delta 98 :gamma 1.2 :vega 27 :theta -10)
    (:spot-shock 10 :iv-shock 0 :pnl 1320 :delta 104 :gamma 1.4 :vega 30 :theta -11)
    (:spot-shock 10 :iv-shock 20 :pnl 1980 :delta 111 :gamma 1.6 :vega 33 :theta -12))
  "Small complete scenario grid for describe and doctor.")

(add-to-list 'financial-chart-shapes
             `(scenario-heatmap
               :doc "Rows of (:spot-shock N :iv-shock N :pnl N :delta N :gamma N :vega N :theta N); shock values are percent changes from spot and implied volatility."
               :example ,financial-chart-scenario-heatmap--example
               :validator financial-chart-scenario-heatmap--validate
               :values financial-chart-scenario-heatmap--values))

(financial-chart-register-kind
 'scenario-heatmap :shape 'scenario-heatmap :template "scenario-heatmap"
 :doc "Portfolio P/L over spot and implied-volatility shocks, with a zero-centred diverging scale and per-cell Greeks.")

(provide 'financial-chart-scenario-heatmap)
;;; financial-chart-scenario-heatmap.el ends here
