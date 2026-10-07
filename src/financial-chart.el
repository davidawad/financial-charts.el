;;; financial-chart.el --- Financial charts: text in a terminal, SVG in a GUI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; Maintainer: David Awad <me@davidaw.ad>
;; Version: 0.4.3
;; Package-Requires: ((emacs "30.1") (eas "0.2.2"))
;; Keywords: data, finance, tools
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software, to deal in it without restriction (MIT licence).

;;; Commentary:

;; Plain Lisp data in, chart out.  financial-chart never fetches data:
;; it validates what its caller supplies and draws it through eas
;; templates -- candlesticks (with indicator overlays and oscillator
;; panes), area, line, sparkline, multi-series comparisons, option
;; payoff and T+n payoff curves, diverging P/L bars, drawdown, returns
;; histogram, order-book depth, heatmap, volume profile -- as
;; propertized text in a terminal frame or an SVG image in a GUI frame.
;;
;; The central object is a CHART SPEC, a plist that round-trips JSON:
;;
;;   (:kind area :data ((1 40.0) (2 45.0) (3 50.0)) :unit "$" :backend text)
;;
;; and every entry point takes the same pieces:
;;
;;   discover   `financial-chart-list-kinds', `financial-chart-describe-kind',
;;              `financial-chart-describe' (the whole package as data)
;;   validate   `financial-chart-validate' -> t or a typed error with
;;              :code :index :field; `financial-chart-check' answers as data
;;   plan       `financial-chart-explain' -> template, backend + why, args,
;;              data summary; pure, never renders
;;   render     `financial-chart-plot' (string), `-plot-insert' (at point),
;;              `-plot-view' (buffer), `-plot-spec' (from a spec),
;;              `financial-chart-sparkline'
;;   candles    `financial-chart-render' / `-render-svg' / `-view' /
;;              `-export-svg' / `-export-png'
;;   health     `financial-chart-doctor' (M-x) / `financial-chart-doctor-checks'
;;
;; Non-Emacs callers use eas.el's bin/eas with this package's templates
;; loaded (see README, "Charts from the shell").
;; Modules: core/ (config, errors, shapes, validation), indicators/,
;; charts/ (the kind registry and each kind), integrations/ (eas
;; adapters, transforms, templates and parity checks).

;;; Code:

;; Keep the package split into functional subdirectories while allowing
;; package-vc and a plain load-path entry to load the public entry point.
(let ((source-directory (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (directory '("." "core" "indicators" "charts" "integrations"))
    (let ((dir (expand-file-name directory source-directory)))
      (when (file-directory-p dir)
        (add-to-list 'load-path (directory-file-name dir))))))

(require 'cl-lib)
(require 'financial-chart-core)
(require 'financial-chart-series)
(require 'financial-chart-validate)
(require 'financial-chart-indicators)
(require 'financial-chart-plot)
(require 'financial-chart-payoff-curves)
(require 'financial-chart-multi)
(require 'financial-chart-returns)
(require 'financial-chart-depth)
(require 'financial-chart-matrix)
(require 'financial-chart-eas)
(require 'financial-chart-eas-parity)

(defconst financial-chart-version
  (eval-when-compile
    (require 'lisp-mnt)
    (lm-version (macroexp-file-name)))
  "Version of the financial-chart package.
Read from this file's Version header when it is compiled or loaded
from source, so releases bump one place.")

(defconst financial-chart-entry-points
  '((discover financial-chart-list-kinds financial-chart-describe-kind
              financial-chart-describe financial-chart-list-cohorts
              financial-chart-list-indicators)
    (validate financial-chart-validate financial-chart-check
              financial-chart-validate-indicator-series)
    (plan financial-chart-explain financial-chart-resolve-cohort)
    (render financial-chart-plot financial-chart-plot-spec financial-chart-plot-insert
            financial-chart-plot-view financial-chart-sparkline financial-chart-render
            financial-chart-render-svg financial-chart-view)
    (export financial-chart-export-svg financial-chart-export-png)
    (extend financial-chart-register-kind financial-chart-register-indicator
            financial-chart-indicator-evaluate financial-chart-indicator-cohorts
            financial-chart-recipe-evaluators)
    (health financial-chart-doctor financial-chart-doctor-checks))
  "Public entry points grouped by what a caller is doing.")

(defun financial-chart--vec (list)
  "LIST as a vector, so `json-encode' emits an array, never an object."
  (apply #'vector list))

;;;###autoload
(defun financial-chart-describe ()
  "The whole package as data: version, kinds, shapes, cohorts, indicators,
entry points.  Lists are vectors, so the result round-trips `json-encode'."
  (list :package "financial-chart" :version financial-chart-version
        :kinds (financial-chart--vec
                (mapcar (lambda (k)
                          (list :kind (symbol-name (car k))
                                :shape (symbol-name (plist-get (cdr k) :shape))
                                :template (plist-get (cdr k) :template)
                                :doc (plist-get (cdr k) :doc)))
                        (financial-chart-list-kinds)))
        :shapes (financial-chart--vec
                 (mapcar (lambda (s) (list :shape (symbol-name (car s))
                                           :doc (plist-get (cdr s) :doc)))
                         financial-chart-shapes))
        :cohorts (financial-chart--vec (mapcar (lambda (c) (symbol-name (car c)))
                                               financial-chart-indicator-cohorts))
        :indicators (financial-chart--vec
                     (mapcar (lambda (indicator)
                               (list :name (symbol-name (plist-get indicator :name))
                                     :label (plist-get indicator :label)
                                     :unit (when (plist-get indicator :unit)
                                             (substring (symbol-name (plist-get indicator :unit)) 1))
                                     :panel (when (plist-get indicator :panel)
                                              (substring (symbol-name (plist-get indicator :panel)) 1))
                                     :description (plist-get indicator :description)))
                             (financial-chart-list-indicators)))
        :entry-points (financial-chart--vec
                       (mapcar (lambda (g)
                                 (list :verb (symbol-name (car g))
                                       :functions (financial-chart--vec
                                                   (mapcar #'symbol-name (cdr g)))))
                               financial-chart-entry-points))))

;;;###autoload
(defun financial-chart-doctor-checks ()
  "Every package health row: (:name :status :detail :remediation), :status
pass, fail or skip.  Covers chart kinds, template parity and cohorts.
No network."
  (append (financial-chart-plot-doctor-checks)
          (financial-chart-eas-parity-doctor-checks)
          (financial-chart-cohort-doctor-checks)))

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
