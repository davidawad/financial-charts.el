;;; financial-chart-core.el --- Configuration, errors and bar windowing for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The base every financial-chart module builds on: the customization
;; group, the error hierarchy, the candlestick defcustoms eas's `ohlc'
;; template reads, and bar windowing.

;;; Code:

(require 'cl-lib)
(require 'seq)

(define-error 'financial-chart-error "financial-chart error")
(define-error 'financial-chart-unknown-kind
  "financial-chart: unknown chart kind" 'financial-chart-error)
(define-error 'financial-chart-invalid-data
  "financial-chart: invalid chart data" 'financial-chart-error)

(defconst financial-chart-root
  (let ((here (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (seq-find (lambda (dir) (file-exists-p (expand-file-name "templates/ohlc.json" dir)))
              (list here (expand-file-name "../../" here))
              here))
  "Directory holding financial-chart's templates/ and examples/.
The repository root when loaded from src/core/; the package directory
itself in the flat layout MELPA installs.")

(defgroup financial-chart nil
  "Financial charts from caller-supplied data, drawn by eas."
  :group 'tools)

(defun financial-chart--invalid (index field code fmt &rest args)
  "Signal `financial-chart-invalid-data' for element INDEX.
FIELD (a string or nil) names the offending field, CODE is the stable
reason code and FMT/ARGS say what is wrong and how to fix it.  The
error data is (MESSAGE :code CODE :index INDEX :field FIELD)."
  (signal 'financial-chart-invalid-data
          (list (if index
                    (format "element %d%s: %s" index
                            (if field (format " (%s)" field) "")
                            (apply #'format fmt args))
                  (apply #'format fmt args))
                :code code :index index :field field)))

(defun financial-chart-error-data (err)
  "ERR, a `financial-chart-error' condition, as a plist.
\(:code CODE :index INDEX :field FIELD :message MESSAGE): what agents
and JSON callers read instead of the error object."
  (let ((props (cddr err)))
    (list :code (or (plist-get props :code) "error")
          :index (plist-get props :index)
          :field (plist-get props :field)
          :message (cadr err))))

;; -----------------------------------------------------------------------
;; Candlesticks
;; -----------------------------------------------------------------------

(defcustom financial-chart-height 20
  "Text rows a candlestick chart from `financial-chart-render' uses."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-max-bars 80
  "Maximum number of bars to render; older bars are trimmed.
A nil or non-positive value disables windowing entirely (render every
bar given, however wide that makes the chart)."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'financial-chart)

(defcustom financial-chart-show-volume t
  "Whether to render a volume pane below the price panel.
Only takes effect when at least one bar carries a non-nil :volume."
  :type 'boolean
  :group 'financial-chart)

(defcustom financial-chart-indicators nil
  "List of overlay indicator specs, each a plist:
`:fn' (required) -- a function of one argument, the (already-windowed)
bars list, returning a list of the same length where each element is
either a number (the indicator's value for that bar, on the same price
scale as the candles) or nil (no value yet, e.g. a warm-up period).
`:label' -- the overlay's legend name (default: the function's name)."
  :type '(repeat plist)
  :group 'financial-chart)

(defcustom financial-chart-oscillators nil
  "List of oscillator specs, each a plist like `financial-chart-indicators'.
Configured oscillators render in their own pane under the price pane."
  :type '(repeat plist)
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Bar windowing
;; -----------------------------------------------------------------------

(defun financial-chart--window-bars (bars)
  "Trim BARS to the most recent `financial-chart-max-bars', if set."
  (if (and financial-chart-max-bars
           (> financial-chart-max-bars 0)
           (> (length bars) financial-chart-max-bars))
      (last bars financial-chart-max-bars)
    bars))

(provide 'financial-chart-core)
;;; financial-chart-core.el ends here
