;;; financial-chart-symbol.el --- Chart a ticker symbol through market-data.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The provider-agnostic bridge: fetch bars for a SYMBOL through
;; market-data.el (soft dependency, never required) and render them.

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-text)
(require 'financial-chart-svg)

;; -----------------------------------------------------------------------
;; Provider-agnostic bridge -- all market data flows through market-data.el,
;; which owns provider selection, request defaults and the bar/v1 shape;
;; this file never calls a broker. market-data.el is soft-wired via
;; `fboundp', so the package still loads and renders bars from any other
;; source (a broker package's own normalizer, a CSV import, test data).
;; -----------------------------------------------------------------------

;; Soft-dependency declarations: market-data.el is never hard-required
;; (see the comment above); these keep the byte-compiler quiet about the
;; fboundp-guarded calls below without creating a load-time dependency.
(declare-function market-data-bars "market-data")
(declare-function market-data-quote "market-data")
(declare-function market-data-explain "market-data")
(declare-function market-data-capabilities "market-data")

(defun financial-chart--require-market-data ()
  "Signal `financial-chart-error' unless market-data.el is loaded."
  (unless (fboundp 'market-data-bars)
    (signal 'financial-chart-error
            (list "charting by symbol needs market-data.el: (require 'market-data) and a broker package, or pass bars to `financial-chart-plot' directly"
                  :code "market_data_missing"))))

(defun financial-chart--symbol-md-keys (keys)
  "Return the market-data fetch keys present in KEYS (a flat plist).
Only the keys `market-data-bars'/`market-data-explain' accept are
forwarded; render options are `let'-bound defcustoms, never passed here."
  (let (out)
    (dolist (k '(:period-type :period :frequency-type :frequency :provider :fields))
      (when (plist-member keys k)
        (setq out (plist-put out k (plist-get keys k)))))
    out))

(defun financial-chart--symbol-title (symbol plan bar-count fetched-at)
  "Compose the provenance title for SYMBOL.
PLAN is a `market-data-explain' plist. The title names the symbol, the
provider actually used, the period/frequency, the bar count, and the
fetch time, so an agent reading any rendered output (buffer, SVG, PNG)
knows exactly what it is looking at without re-deriving it."
  (let* ((params (plist-get plan :params))
         (period (plist-get params :period))
         (period-type (plist-get params :period-type))
         (frequency (plist-get params :frequency))
         (frequency-type (plist-get params :frequency-type)))
    (format "%s · %s · %s %s/%s %s · %d bars · %s"
            (upcase symbol)
            (or (plist-get plan :provider) "?")
            period period-type frequency frequency-type
            bar-count fetched-at)))

(defun financial-chart--symbol-bars-and-title (symbol keys)
  "Fetch SYMBOL's bars via market-data and build a provenance title.
Returns (BARS . TITLE). KEYS is a flat plist of market-data fetch keys
(see `financial-chart--symbol-md-keys'). The provider is resolved once
via `market-data-explain' and forced on the fetch, so the title's
provider is always the provider actually used. market-data's typed
errors propagate untouched."
  (financial-chart--require-market-data)
  (let* ((md-keys (financial-chart--symbol-md-keys keys))
         (plan (apply #'market-data-explain symbol md-keys))
         (chosen (plist-get plan :provider))
         (bars (apply #'market-data-bars symbol
                      (plist-put (copy-sequence md-keys) :provider chosen)))
         (fetched-at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (title (financial-chart--symbol-title symbol plan (length bars) fetched-at)))
    (cons bars title)))

;;;###autoload
(defun financial-chart-view-symbol (symbol &rest keys)
  "Fetch SYMBOL via market-data and view it as candlesticks.
KEYS are market-data fetch keys -- :period-type :period :frequency-type
:frequency :provider :fields -- forwarded to `market-data-bars'. Every
render knob remains a `let'-bindable defcustom as elsewhere in this file.
The buffer header is the provenance title (symbol, provider
actually used, period/frequency, bar count, fetched-at)."
  (interactive (list (read-string "Symbol: ")))
  (let ((bt (financial-chart--symbol-bars-and-title symbol keys)))
    (financial-chart-view (car bt) (cdr bt))))

;;;###autoload
(defun financial-chart-export-symbol-svg (symbol file &rest keys)
  "Fetch SYMBOL via market-data, export as an SVG chart to FILE. Returns FILE.
KEYS are as in `financial-chart-view-symbol'. Interactively, FILE
defaults to SYMBOL-chart.svg under `financial-chart-export-directory'."
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (expand-file-name (format "%s-chart.svg" (upcase symbol))
                             financial-chart-export-directory))))
  (let ((bt (financial-chart--symbol-bars-and-title symbol keys)))
    (financial-chart-export-svg (car bt) file (cdr bt))
    (message "financial-chart: wrote %s" file)
    file))

;;;###autoload
(defun financial-chart-export-symbol-png (symbol file &rest keys)
  "Fetch SYMBOL via market-data, export as a PNG chart to FILE. Returns FILE.
KEYS are as in `financial-chart-view-symbol'. Interactively, FILE
defaults to SYMBOL-chart.png under `financial-chart-export-directory'."
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (expand-file-name (format "%s-chart.png" (upcase symbol))
                             financial-chart-export-directory))))
  (let ((bt (financial-chart--symbol-bars-and-title symbol keys)))
    (financial-chart-export-png (car bt) file (cdr bt))
    (message "financial-chart: wrote %s" file)
    file))

;;;###autoload
(defun financial-chart-explain-symbol (symbol &rest keys)
  "Return the expanded plan for a `financial-chart-view-symbol' call.
Performs ZERO I/O. Merges `market-data-explain' (chosen provider + why +
normalized fetch params) with the effective render configuration (each
render defcustom's current value, under `:render'). Never fetches; agents
inspect the plan, then execute."
  (financial-chart--require-market-data)
  (let ((plan (apply #'market-data-explain symbol
                     (financial-chart--symbol-md-keys keys))))
    (append
     plan
     (list :render
           (list :height financial-chart-height
                 :max-bars financial-chart-max-bars
                 :scale financial-chart-scale
                 :show-volume financial-chart-show-volume
                 :show-x-axis financial-chart-show-x-axis
                 :export-directory financial-chart-export-directory
                 :png-converter financial-chart-png-converter)))))

;; -- doctor rows (eager; assembled by `financial-chart-doctor-checks') --

(defun financial-chart-symbol-doctor-checks ()
  "Eager doctor rows for symbol charting and export; no network.
Each row is (:name :status pass|fail|skip :detail :remediation).
market-data.el is optional, so its absence is a skip, not a failure."
  (let* ((dir (expand-file-name financial-chart-export-directory))
         (probe (if (file-directory-p dir) dir
                  (file-name-directory (directory-file-name dir))))
         (conv (or financial-chart-png-converter
                   (cl-find-if (lambda (name) (executable-find (symbol-name name)))
                               '(rsvg-convert convert magick))))
         (providers (and (fboundp 'market-data-capabilities)
                         (cl-remove-if-not (lambda (cell) (plist-get (cdr cell) :loaded))
                                           (market-data-capabilities)))))
    (list
     (cond
      ((not (fboundp 'market-data-capabilities))
       (list :name "symbol charts: market-data provider" :status 'skip
             :detail "market-data.el not loaded; symbol/preset charts unavailable, plain-data charts unaffected"
             :remediation "(require 'market-data), then load a broker package"))
      (providers
       (list :name "symbol charts: market-data provider" :status 'pass
             :detail (format "providers loaded: %s" (mapcar #'car providers))
             :remediation nil))
      (t
       (list :name "symbol charts: market-data provider" :status 'fail
             :detail "market-data loaded but no provider is"
             :remediation "load schwab-broker or alpaca-broker-data, then authenticate")))
     (list :name "export directory writable"
           :status (if (file-writable-p probe) 'pass 'fail)
           :detail (format "%s %s" dir (if (file-writable-p probe) "writable" "not writable"))
           :remediation (unless (file-writable-p probe)
                          "set financial-chart-export-directory to a writable path"))
     (list :name "PNG converter"
           :status (if conv 'pass 'fail)
           :detail (if conv (format "PNG converter: %s" conv) "no SVG->PNG converter found")
           :remediation (unless conv
                          "install rsvg-convert or ImageMagick (brew install librsvg), or set financial-chart-png-converter")))))

;; -----------------------------------------------------------------------

(provide 'financial-chart-symbol)
;;; financial-chart-symbol.el ends here
