;;; financial-chart.el --- Financial charts in Emacs: text in a terminal, SVG in a GUI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; Maintainer: David Awad <me@davidaw.ad>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: data, finance, tools
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software, to deal in it without restriction (MIT licence).

;;; Commentary:

;; Plain Lisp data in, chart out.  One call draws any chart kind --
;; candlesticks, area, braille line, sparkline, option payoff, diverging
;; P/L bars -- as propertized unicode text in a terminal frame or an SVG
;; image in a GUI frame.  No external process except optional PNG export.
;;
;; The central object is a CHART SPEC, a plist that round-trips JSON:
;;
;;   (:kind area :data ((1 40.0) (2 45.0) (3 50.0)) :unit "$" :backend text)
;;
;; and every entry point takes the same pieces:
;;
;;   discover   `financial-chart-list-kinds', `financial-chart-describe-kind',
;;              `financial-chart-describe' (the whole package as data)
;;   validate   `financial-chart-validate' -> t or a typed error with :index
;;   plan       `financial-chart-explain' -> backend + why, renderer, args,
;;              data summary; pure, never renders
;;   render     `financial-chart-plot' (string), `-plot-insert' (at point),
;;              `-plot-view' (buffer), `-plot-spec' (from a spec),
;;              `financial-chart-sparkline'
;;   candles    `financial-chart-render' / `-render-svg' / `-view', with
;;              volume, X-axis and indicator overlays; every knob a defcustom
;;   tickers    `financial-chart-view-symbol' and presets, through
;;              market-data.el when it is loaded (optional)
;;   health     `financial-chart-doctor' (M-x) / `financial-chart-doctor-checks'
;;
;; Non-Emacs callers use bin/financial-chart, which reads a spec as JSON.
;; Modules: -core (config), -series (shapes), -indicators (+ cohorts),
;; -text, -svg, -plot (kinds), -symbol (market-data bridge), -presets,
;; -batch (CLI).

;;; Code:

(require 'cl-lib)
(require 'financial-chart-core)
(require 'financial-chart-series)
(require 'financial-chart-indicators)
(require 'financial-chart-text)
(require 'financial-chart-svg)
(require 'financial-chart-symbol)
(require 'financial-chart-presets)
(require 'financial-chart-plot)

(defconst financial-chart-version "0.2.0"
  "Version of the financial-chart package.")

(defconst financial-chart-entry-points
  '((discover financial-chart-list-kinds financial-chart-describe-kind
              financial-chart-describe financial-chart-list-cohorts
              financial-chart-list-presets)
    (validate financial-chart-validate)
    (plan financial-chart-explain financial-chart-explain-symbol
          financial-chart-resolve-preset financial-chart-resolve-cohort)
    (render financial-chart-plot financial-chart-plot-spec financial-chart-plot-insert
            financial-chart-plot-view financial-chart-sparkline financial-chart-render
            financial-chart-render-svg financial-chart-view)
    (export financial-chart-export-svg financial-chart-export-png
            financial-chart-export-symbol-svg financial-chart-export-symbol-png)
    (tickers financial-chart-view-symbol financial-chart-view-preset)
    (extend financial-chart-register-kind financial-chart-indicator-cohorts
            financial-chart-presets financial-chart-recipe-evaluators)
    (health financial-chart-doctor financial-chart-doctor-checks))
  "Public entry points grouped by what a caller is doing.")

(defun financial-chart--vec (list)
  "LIST as a vector, so `json-encode' emits an array, never an object."
  (apply #'vector list))

;;;###autoload
(defun financial-chart-describe ()
  "The whole package as data: version, kinds, shapes, cohorts, presets,
entry points.  Lists are vectors, so the result round-trips `json-encode'."
  (list :package "financial-chart" :version financial-chart-version
        :kinds (financial-chart--vec
                (mapcar (lambda (k)
                          (list :kind (symbol-name (car k))
                                :shape (symbol-name (plist-get (cdr k) :shape))
                                :doc (plist-get (cdr k) :doc)))
                        (financial-chart-list-kinds)))
        :shapes (financial-chart--vec
                 (mapcar (lambda (s) (list :shape (symbol-name (car s))
                                           :doc (plist-get (cdr s) :doc)))
                         financial-chart-shapes))
        :cohorts (financial-chart--vec (mapcar (lambda (c) (symbol-name (car c)))
                                               financial-chart-indicator-cohorts))
        :presets (financial-chart--vec (mapcar (lambda (p) (symbol-name (car p)))
                                               financial-chart-presets))
        :market-data (and (fboundp 'market-data-bars) t)
        :entry-points (financial-chart--vec
                       (mapcar (lambda (g)
                                 (list :verb (symbol-name (car g))
                                       :functions (financial-chart--vec
                                                   (mapcar #'symbol-name (cdr g)))))
                               financial-chart-entry-points))))

;;;###autoload
(defun financial-chart-doctor-checks ()
  "Every package health row: (:name :status :detail :remediation), :status
pass, fail or skip.  Covers chart kinds, symbol charting and export,
cohorts and presets.  No network."
  (append (financial-chart-plot-doctor-checks)
          (financial-chart-symbol-doctor-checks)
          (financial-chart-cohort-doctor-checks)
          (financial-chart-preset-doctor-checks)))

;;;###autoload
(defun financial-chart-doctor ()
  "Show `financial-chart-doctor-checks' in a buffer; return the rows."
  (interactive)
  (let ((rows (financial-chart-doctor-checks)))
    (with-current-buffer (get-buffer-create "*financial-chart doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "financial-chart %s\n\n" financial-chart-version))
        (dolist (r rows)
          (insert (format "%-5s %s -- %s\n" (upcase (symbol-name (plist-get r :status)))
                          (plist-get r :name) (plist-get r :detail)))
          (when (plist-get r :remediation)
            (insert (format "      fix: %s\n" (plist-get r :remediation))))))
      (special-mode)
      (unless noninteractive (pop-to-buffer (current-buffer))))
    rows))

(provide 'financial-chart)
;;; financial-chart.el ends here
