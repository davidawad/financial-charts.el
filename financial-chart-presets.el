;;; financial-chart-presets.el --- Named chart presets: render, fetch and cohort bundles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A preset names a full render + fetch + cohort bundle as one data
;; entry; `financial-chart-resolve-preset' expands it with zero I/O.

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-indicators)
(require 'financial-chart-text)
(require 'financial-chart-svg)
(require 'financial-chart-symbol)

(declare-function market-data-bars "market-data")

;; Chart presets -- named full-config bundles
;;
;; A preset is DATA: one `financial-chart-presets' entry names a full
;; render+fetch+cohort bundle, so `M-x financial-chart-view-preset AAPL
;; swing' replaces a page of `let'-bindings. `financial-chart-resolve-preset'
;; is the pure inspect-before-execute plan: one call expands the merged
;; render config (with per-key provenance), the resolved cohort overlays
;; (via `financial-chart-resolve-cohort'), and the provider decision (via
;; `market-data-explain') with zero network.
;; -----------------------------------------------------------------------

(define-error 'financial-chart-unresolvable-preset
  "financial-chart: preset cannot be resolved" 'financial-chart-error)

(defcustom financial-chart-presets
  '((daytrade
     :doc "Intraday day-trading view: 5-minute bars, trend overlay, volume."
     :period-type "day" :period 5 :frequency-type "minute" :frequency 5
     :cohort trend-following :show-volume t)
    (swing
     :doc "Multi-week swing view: daily bars over ~3 months, trend overlay."
     :period-type "month" :period 3 :frequency-type "daily" :frequency 1
     :cohort trend-following)
    (options-memo
     :doc "Options-memo snapshot: 1 month of daily bars, mean-reversion \
cohort, no volume pane."
     :period-type "month" :period 1 :frequency-type "daily" :frequency 1
     :cohort mean-reversion :show-volume nil))
  "Named full-config chart presets.
Each entry is (NAME :doc DOC [FETCH-KEY VAL...] [:provider P] :cohort
COHORT-NAME [RENDER-KEY VAL...]).  FETCH keys (:period-type/:period/
:frequency-type/:frequency/:provider) go to `market-data-bars'; RENDER
keys (see `financial-chart--preset-render-keys') override the matching
render defcustom for this preset only; :cohort names a
`financial-chart-indicator-cohorts' entry whose resolved members overlay
the chart.  An unset key inherits the render defcustom's current value.
Adding a preset is a pure data edit; expand one with
`financial-chart-resolve-preset'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'financial-chart)

(defconst financial-chart--preset-render-keys
  '((:height . financial-chart-height)
    (:max-bars . financial-chart-max-bars)
    (:candle-width . financial-chart-candle-width)
    (:candle-gap . financial-chart-candle-gap)
    (:scale . financial-chart-scale)
    (:axis-label-count . financial-chart-axis-label-count)
    (:show-volume . financial-chart-show-volume)
    (:volume-height . financial-chart-volume-height)
    (:show-x-axis . financial-chart-show-x-axis))
  "Map each preset render-key to the render defcustom it overrides.
`financial-chart-resolve-preset'/`-view-preset' bind these dynamically
for the duration of one render; a preset that omits a key inherits the
defcustom's current value.")

(defconst financial-chart--preset-fetch-keys
  '(:period-type :period :frequency-type :frequency :provider)
  "Preset keys forwarded to `market-data-bars'/`market-data-explain'.")

(defun financial-chart--preset (name)
  "Return preset NAME's plist from `financial-chart-presets', or nil."
  (cdr (assq name financial-chart-presets)))

(defun financial-chart--preset-fetch-plist (preset)
  "The market-data fetch keys present in PRESET, as a flat plist."
  (let (out)
    (dolist (k financial-chart--preset-fetch-keys)
      (when (plist-member preset k)
        (setq out (plist-put out k (plist-get preset k)))))
    out))

(defun financial-chart--preset-render-config (preset)
  "Return the merged render config for PRESET: one (KEY :value V :source
SRC) per `financial-chart--preset-render-keys' key.  SRC is `preset-set'
when PRESET supplies KEY, else `inherited-default' (V is the render
defcustom's current value)."
  (mapcar
   (lambda (cell)
     (let ((key (car cell)) (sym (cdr cell)))
       (if (plist-member preset key)
           (list key :value (plist-get preset key) :source 'preset-set)
         (list key :value (symbol-value sym) :source 'inherited-default))))
   financial-chart--preset-render-keys))

(defun financial-chart-resolve-preset (name symbol &rest keys)
  "Return the fully-expanded plan for viewing SYMBOL with preset NAME.
PURE: performs ZERO network I/O (`market-data-explain' is a no-fetch
planner; `financial-chart-resolve-cohort' is pure).  This is the
inspect-before-execute plan -- one call shows everything a render
would do.  KEYS may supply :provider to override the preset's provider.

Returns a plist: (:preset NAME :symbol SYMBOL :render RENDER :cohort
SPECS :cohort-name COHORT :fetch FETCH :market-data PLAN), where RENDER is
`financial-chart--preset-render-config' output, SPECS is the resolved
overlay list, and PLAN is `market-data-explain' output (nil when
market-data is not loaded).  Signals `financial-chart-unresolvable-preset'
-- naming the preset (and, for a bad :cohort, the cohort and the fix) --
when the preset or its cohort cannot be resolved."
  (let ((preset (financial-chart--preset name)))
    (unless preset
      (signal 'financial-chart-unresolvable-preset
              (list (format "no preset named `%s' in `financial-chart-presets'"
                            name))))
    (let* ((cohort-name (plist-get preset :cohort))
           (specs
            (when cohort-name
              (condition-case err
                  (financial-chart-resolve-cohort cohort-name)
                (financial-chart-unresolvable-cohort
                 (signal 'financial-chart-unresolvable-preset
                         (list (format "preset `%s' cohort `%s': %s"
                                       name cohort-name
                                       (error-message-string err))))))))
           (fetch (financial-chart--preset-fetch-plist preset))
           (fetch (if (plist-member keys :provider)
                      (plist-put (copy-sequence fetch)
                                 :provider (plist-get keys :provider))
                    fetch))
           (plan (when (fboundp 'market-data-explain)
                   (apply #'market-data-explain symbol fetch))))
      (list :preset name
            :symbol (upcase symbol)
            :render (financial-chart--preset-render-config preset)
            :cohort specs
            :cohort-name cohort-name
            :fetch fetch
            :market-data plan))))

(defun financial-chart-list-presets ()
  "Return a summary of every preset: (NAME :doc DOC :cohort COHORT :valid
BOOL) per entry.  VALID reflects whether the preset's :cohort resolves.
Pure: no network I/O."
  (mapcar
   (lambda (entry)
     (let* ((name (car entry))
            (preset (cdr entry))
            (cohort-name (plist-get preset :cohort))
            (valid (condition-case nil
                       (progn
                         (when cohort-name
                           (financial-chart-resolve-cohort cohort-name))
                         t)
                     (financial-chart-unresolvable-cohort nil))))
       (list name :doc (plist-get preset :doc)
             :cohort cohort-name :valid valid)))
   financial-chart-presets))

(defun financial-chart-describe-preset (name)
  "Describe preset NAME: provenance, every effective render key with its
`:source' (preset-set vs inherited-default), the fetch params, and the
cohort disposition (via `financial-chart-describe-cohort').  Agents see
exactly which values will apply and why.  Signals
`financial-chart-unresolvable-preset' when NAME is undefined."
  (let ((preset (financial-chart--preset name)))
    (unless preset
      (signal 'financial-chart-unresolvable-preset
              (list (format "no preset named `%s'" name))))
    (list :preset name
          :doc (plist-get preset :doc)
          :provenance
          (format "defcustom `financial-chart-presets' (%s)"
                  (or (ignore-errors
                        (symbol-file 'financial-chart-presets 'defvar))
                      "financial-chart.el"))
          :render (financial-chart--preset-render-config preset)
          :fetch (financial-chart--preset-fetch-plist preset)
          :cohort
          (let ((cohort-name (plist-get preset :cohort)))
            (when cohort-name
              (condition-case err
                  (financial-chart-describe-cohort cohort-name)
                (financial-chart-unresolvable-cohort
                 (list :cohort cohort-name
                       :error (error-message-string err)))))))))

(defun financial-chart--preset-title (name symbol plan bar-count fetched-at)
  "Provenance title with preset NAME prepended to the symbol title.
Reuses `financial-chart--symbol-title' (symbol · provider · period/
frequency · bars · fetched-at) so the preset render's provenance never
drifts from a plain symbol render's."
  (format "%s · %s"
          (symbol-name name)
          (financial-chart--symbol-title symbol plan bar-count fetched-at)))

(defun financial-chart--preset-render (name symbol keys render-fn)
  "Resolve preset NAME for SYMBOL, fetch its bars via market-data, and
call RENDER-FN with (BARS TITLE) under the preset's render defcustoms
\(bound dynamically via `cl-progv', restored afterwards) and its resolved
cohort overlays (`financial-chart-indicators').  KEYS may override
:provider.  market-data's typed errors propagate untouched."
  (financial-chart--require-market-data)
  (let* ((plan (apply #'financial-chart-resolve-preset name symbol keys))
         (md (plist-get plan :market-data))
         (fetch (plist-get plan :fetch))
         (chosen (plist-get md :provider))
         (bars (apply #'market-data-bars symbol
                      (plist-put (copy-sequence fetch) :provider chosen)))
         (fetched-at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (title (financial-chart--preset-title name symbol md (length bars)
                                               fetched-at))
         (render (plist-get plan :render))
         (syms (mapcar (lambda (kv)
                         (cdr (assq (car kv) financial-chart--preset-render-keys)))
                       render))
         (vals (mapcar (lambda (kv) (plist-get (cdr kv) :value)) render)))
    (cl-progv syms vals
      (let ((financial-chart-indicators (plist-get plan :cohort)))
        (funcall render-fn bars title)))))

;;;###autoload
(defun financial-chart-view-preset (symbol name &rest keys)
  "View SYMBOL with the named chart preset NAME (a `financial-chart-presets'
key).  Fetches via market-data, applies the preset's render config +
cohort overlays, and headers the buffer with the provenance title
\(preset · symbol · provider · period/frequency · bars · fetched-at).
KEYS may override :provider."
  (interactive
   (list (read-string "Symbol: ")
         (intern (completing-read
                  "Preset: "
                  (mapcar #'car financial-chart-presets) nil t))))
  (financial-chart--preset-render
   name symbol keys
   (lambda (bars title) (financial-chart-view bars title))))

;;;###autoload
(defun financial-chart-export-preset-svg (symbol name file &rest keys)
  "Export SYMBOL under preset NAME as an SVG chart to FILE.  Returns FILE.
NAME/KEYS as in `financial-chart-view-preset'.  Interactively FILE
defaults to SYMBOL-chart.svg under `financial-chart-export-directory'."
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (intern (completing-read
                    "Preset: "
                    (mapcar #'car financial-chart-presets) nil t))
           (expand-file-name (format "%s-chart.svg" (upcase symbol))
                             financial-chart-export-directory))))
  (financial-chart--preset-render
   name symbol keys
   (lambda (bars title)
     (financial-chart-export-svg bars file title)
     (message "financial-chart: wrote %s" file)
     file)))

;;;###autoload
(defun financial-chart-export-preset-png (symbol name file &rest keys)
  "Export SYMBOL under preset NAME as a PNG chart to FILE.  Returns FILE.
NAME/KEYS as in `financial-chart-view-preset'.  Interactively FILE
defaults to SYMBOL-chart.png under `financial-chart-export-directory'."
  (interactive
   (let ((symbol (read-string "Symbol: ")))
     (list symbol
           (intern (completing-read
                    "Preset: "
                    (mapcar #'car financial-chart-presets) nil t))
           (expand-file-name (format "%s-chart.png" (upcase symbol))
                             financial-chart-export-directory))))
  (financial-chart--preset-render
   name symbol keys
   (lambda (bars title)
     (financial-chart-export-png bars file title)
     (message "financial-chart: wrote %s" file)
     file)))

(defun financial-chart-preset-doctor-checks ()
  "Doctor rows for the presets, one per preset:
\(:name NAME :status pass|fail :detail D :remediation R).
PASS iff the preset resolves against a sentinel symbol (its :cohort
resolves) without a `financial-chart-unresolvable-preset' error."
  (mapcar
   (lambda (entry)
     (let ((name (car entry)))
       (condition-case err
           (progn
             (financial-chart-resolve-preset name "AAPL")
             (list :name (format "preset:%s" name) :status 'pass
                   :detail (format "resolves (cohort %s)"
                                   (plist-get (cdr entry) :cohort))
                   :remediation nil))
         (financial-chart-unresolvable-preset
          (list :name (format "preset:%s" name) :status 'fail
                :detail (error-message-string err)
                :remediation "fix the preset's :cohort or add the missing \
`financial-chart-recipe-evaluators' entry")))))
   financial-chart-presets))

(provide 'financial-chart-presets)
;;; financial-chart-presets.el ends here
