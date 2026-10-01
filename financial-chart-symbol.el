;;; financial-chart-symbol.el --- Chart a ticker symbol through market-data.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The provider-agnostic bridge: fetch bars for a SYMBOL through
;; market-data.el (soft dependency, never required) and render them.

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-text)
(require 'financial-chart-svg)

;; -----------------------------------------------------------------------
;; Schwab bridge defaults
;; -----------------------------------------------------------------------

(defcustom financial-chart-schwab-default-period-type "month"
  "Default `:period-type' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'string
  :group 'financial-chart)

(defcustom financial-chart-schwab-default-period 1
  "Default `:period' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-schwab-default-frequency-type "daily"
  "Default `:frequency-type' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'string
  :group 'financial-chart)

(defcustom financial-chart-schwab-default-frequency 1
  "Default `:frequency' passed to `schwab-broker-price-history-sync'
by `financial-chart-schwab-view' when the caller doesn't supply one."
  :type 'integer
  :group 'financial-chart)


;; -----------------------------------------------------------------------
;; Provider-agnostic bridge (L2 of the financial data abstraction tower)
;; -- all market data flows through market-data.el (L1); this file never
;; calls a broker function directly. market-data.el is soft-wired via
;; `fboundp', so financial-chart.el still loads (and renders any bar
;; source: Alpaca, a CSV import, synthetic test data) on a machine that
;; lacks it. The schwab-specific entry points below survive as thin,
;; obsolete-marked wrappers with :provider 'schwab.
;; -----------------------------------------------------------------------

;; Soft-dependency declarations: market-data.el (L1) is never hard-required
;; (see the comment above); these keep the byte-compiler quiet about the
;; fboundp-guarded calls below without creating a load-time dependency.
(declare-function market-data-bars "market-data")
(declare-function market-data-quote "market-data")
(declare-function market-data-explain "market-data")
(declare-function market-data-capabilities "market-data")

(defun financial-chart--require-market-data ()
  "Signal a clear `user-error' unless market-data.el (L1) is loaded."
  (unless (fboundp 'market-data-bars)
    (user-error
     "market-data not loaded -- financial-chart's provider-agnostic bridge needs market-data.el (L1); (require 'market-data)")))

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
  "Compose the Law-7 provenance title for SYMBOL.
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
  "Fetch SYMBOL's bars via market-data and build a Law-7 title.
Returns (BARS . TITLE). KEYS is a flat plist of market-data fetch keys
(see `financial-chart--symbol-md-keys'). The provider is resolved once
via `market-data-explain' and forced on the fetch, so the title's
provider is always the provider actually used. market-data's typed
errors propagate untouched (Law 4)."
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
The buffer header is the Law-7 provenance title (symbol, provider
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
  "Return the expanded plan for a `financial-chart-view-symbol' call (Law 3).
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

;; -- deprecated schwab-specific entry points (dot-financial-abstraction-
;; tower-s15we.2): thin wrappers over the provider-agnostic fns with
;; :provider 'schwab. Kept working, not deleted, so existing callers and
;; muscle memory don't break; obsolete-marked so the byte-compiler points
;; callers at the replacements. Data now flows through market-data.el, not
;; schwab-broker directly, so the provenance title and typed errors come
;; for free. (These use market-data's request defaults, not the dormant
;; `financial-chart-schwab-default-*' customs.)

;;;###autoload
(defun financial-chart-schwab-view (symbol &rest keys)
  "Obsolete alias: `financial-chart-view-symbol' with the schwab provider.
KEYS are forwarded verbatim (period/frequency fetch keys)."
  (declare (obsolete financial-chart-view-symbol "2026-09"))
  (interactive (list (read-string "Symbol: ")))
  (apply #'financial-chart-view-symbol symbol :provider 'schwab keys))

;;;###autoload
(defun financial-chart-schwab-export-svg (symbol file &rest keys)
  "Obsolete alias: `financial-chart-export-symbol-svg' with the schwab provider."
  (declare (obsolete financial-chart-export-symbol-svg "2026-09"))
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (expand-file-name (format "%s-chart.svg" (upcase symbol))
                             financial-chart-export-directory))))
  (apply #'financial-chart-export-symbol-svg symbol file :provider 'schwab keys))

;;;###autoload
(defun financial-chart-schwab-export-png (symbol file &rest keys)
  "Obsolete alias: `financial-chart-export-symbol-png' with the schwab provider."
  (declare (obsolete financial-chart-export-symbol-png "2026-09"))
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (expand-file-name (format "%s-chart.png" (upcase symbol))
                             financial-chart-export-directory))))
  (apply #'financial-chart-export-symbol-png symbol file :provider 'schwab keys))

;; -- doctor hook (consumed by the tower doctor, child .7) --

(defun financial-chart-doctor-checks ()
  "Return a list of (LABEL . CHECK-FN) probes for `financial-tower-doctor' (.7).
Each CHECK-FN tolerates optional keyword args and returns a plist
(:ok BOOL :detail STRING :remediation STRING-or-nil); no network calls."
  (list
   (cons
    "financial-chart package loadable"
    (lambda (&rest _)
      (if (featurep 'financial-chart)
          (list :ok t :detail "financial-chart loaded")
        (list :ok nil :detail "financial-chart not loaded"
              :remediation "(require 'financial-chart)"))))
   (cons
    "financial-chart bridge resolves a provider via market-data"
    (lambda (&rest _)
      (if (not (fboundp 'market-data-capabilities))
          (list :ok nil :detail "market-data (L1) not loaded"
                :remediation "(require 'market-data), then load a broker package")
        (let ((loaded (cl-remove-if-not
                       (lambda (cell) (plist-get (cdr cell) :loaded))
                       (market-data-capabilities))))
          (if loaded
              (list :ok t :detail (format "market-data provider(s) available: %s"
                                          (mapcar #'car loaded)))
            (list :ok nil :detail "no market-data provider loaded"
                  :remediation "load schwab-broker or alpaca-broker-data, then authenticate"))))))
   (cons
    "financial-chart export directory writable"
    (lambda (&rest _)
      (let* ((dir (expand-file-name financial-chart-export-directory))
             (probe (if (file-directory-p dir)
                        dir
                      (file-name-directory (directory-file-name dir)))))
        (if (file-writable-p probe)
            (list :ok t :detail (format "%s writable" dir))
          (list :ok nil :detail (format "%s not writable" dir)
                :remediation "set financial-chart-export-directory to a writable path")))))
   (cons
    "financial-chart PNG converter available"
    (lambda (&rest _)
      (let ((conv (or financial-chart-png-converter
                      (cl-find-if (lambda (name) (executable-find (symbol-name name)))
                                  '(rsvg-convert convert magick)))))
        (if conv
            (list :ok t :detail (format "PNG converter: %s" conv))
          (list :ok nil :detail "no SVG->PNG converter found"
                :remediation "install rsvg-convert or ImageMagick (brew install librsvg), or set financial-chart-png-converter")))))))

;; -----------------------------------------------------------------------

(provide 'financial-chart-symbol)
;;; financial-chart-symbol.el ends here
