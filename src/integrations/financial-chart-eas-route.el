;;; financial-chart-eas-route.el --- financial-chart kinds drawn by eas templates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every financial-chart kind has an eas template (templates/, and
;; templates/financial/ for those needing financial-chart's transforms).
;; `financial-chart-eas-kinds' maps a kind to its template, the adapter
;; that lowers the kind's data to the template's rows and the props
;; that become slots.  `financial-chart-eas-bindings' builds the
;; bindings, `financial-chart-eas-render' draws them, and
;; `financial-chart-eas-parity' checks, as data, that the template plots
;; the numbers the kind's own renderer plots.
;;
;; `financial-chart-plot' keeps its own renderers unless
;; `financial-chart-eas-route' says otherwise: t routes each kind whose
;; parity check passes on its example data, a list routes those kinds.
;; The public API is unchanged either way.

;;; Code:

(require 'cl-lib)
(require 'eas)
(require 'financial-chart-eas)
(require 'financial-chart-plot)
(require 'financial-chart-indicators)
(require 'financial-chart-returns)
(require 'financial-chart-multi)
(require 'financial-chart-matrix)
(require 'financial-chart-depth)
(require 'financial-chart-payoff-curves)

(defcustom financial-chart-eas-route nil
  "Which kinds `financial-chart-plot' draws with eas templates.
nil draws every kind with its own renderer.  t routes each kind in
`financial-chart-eas-kinds' whose `financial-chart-eas-parity' passes
on the kind's example data.  A list of kinds routes exactly those."
  :type '(choice (const :tag "None" nil) (const :tag "Kinds at parity" t)
                 (repeat :tag "These kinds" symbol))
  :group 'financial-chart)

(defvar financial-chart-eas-kinds
  '((ohlc :template "ohlc" :slot :bars :bindings financial-chart-eas--ohlc-bindings
          :parity financial-chart-eas--ohlc-parity)
    (area :template "area" :adapter "series" :bindings financial-chart-eas--series-bindings
          :parity financial-chart-eas--series-parity)
    (line :template "series-line" :adapter "series" :bindings financial-chart-eas--series-bindings
          :parity financial-chart-eas--series-parity)
    (sparkline :template "sparkline" :adapter "series" :bindings financial-chart-eas--series-bindings
               :parity financial-chart-eas--series-parity)
    (payoff :template "payoff" :adapter "payoff" :parity financial-chart-eas--payoff-parity)
    (bars :template "diverging-bars" :adapter "labeled" :parity financial-chart-eas--bars-parity)
    (multi :template "multi" :adapter "multi-series" :props ((:normalize . :normalize))
           :parity financial-chart-eas--multi-parity)
    (payoff-curves :template "payoff-curves" :adapter "payoff-curves"
                   :parity financial-chart-eas--payoff-curves-parity)
    (drawdown :template "drawdown" :adapter "series" :bindings financial-chart-eas--series-bindings
              :parity financial-chart-eas--drawdown-parity)
    (histogram :template "histogram" :adapter "series" :props ((:bins . :bins))
               :parity financial-chart-eas--histogram-parity)
    (heatmap :template "heatmap" :adapter "matrix" :parity financial-chart-eas--heatmap-parity)
    (depth :template "depth" :adapter "order-book" :parity financial-chart-eas--depth-parity)
    (volume-profile :template "volume-profile" :slot :bars :props ((:bins . :bins))
                    :parity financial-chart-eas--volume-profile-parity))
  "Kinds with an eas template: (KIND :template NAME [:slot SLOT]
\[:adapter ADAPTER] [:props ((PROP . SLOT) ...)] [:bindings FN] :parity FN).
DATA is lowered by eas adapter ADAPTER (else passed as is) into data
slot SLOT (default :data); each PROP given becomes SLOT.  :bindings
\(FN DATA PROPS) returns more bindings.  :parity (FN DATA PROPS SCENE)
returns checks (NAME EXPECTED ACTUAL) against the kind's own numbers.")

(defun financial-chart-eas--entry (kind)
  "KIND's `financial-chart-eas-kinds' plist, or signal."
  (or (alist-get kind financial-chart-eas-kinds)
      (signal 'financial-chart-unknown-kind
              (list (format "%S has no eas template; templated kinds: %s" kind
                            (mapconcat (lambda (e) (symbol-name (car e))) financial-chart-eas-kinds ", "))
                    :code "unknown_kind" :kind kind))))

;;; Bindings

(defun financial-chart-eas--json-value (value)
  "VALUE as a slot value: symbols become strings."
  (if (and (symbolp value) value (not (keywordp value)) (not (eq value t)))
      (symbol-name value)
    value))

(defun financial-chart-eas--temporal-p (data)
  "Non-nil when every X in series DATA is epoch milliseconds."
  (let ((xs (delq nil (mapcar #'financial-chart-series--point-x (append data nil)))))
    (and xs (cl-every (lambda (x) (and (numberp x) (> x 1e11))) xs))))

(defun financial-chart-eas--series-bindings (data props)
  "Series slots for DATA: x_type, y_title from :unit, scale from :scale."
  (append (when (financial-chart-eas--temporal-p data) (list :x_type "temporal"))
          (let ((unit (plist-get props :unit)))
            (when (and (stringp unit) (not (string-empty-p unit))) (list :y_title unit)))
          (when (eq (plist-get props :scale) 'log) (list :scale "log"))))

(defun financial-chart-eas--overlay-items (bars specs)
  "SPECS (`financial-chart-indicators' entries) as precomputed template items."
  (let (seen)
    (cl-loop for series in (financial-chart--compute-series bars specs)
             for i from 1
             for label = (plist-get series :label)
             ;; Each overlay is its own colour, so a repeated label gets its index.
             do (when (member label seen) (setq label (format "%s %d" label i)))
             (push label seen)
             collect (list :transform "values"
                           :values (vconcat (plist-get series :series))
                           :as label))))

(defun financial-chart-eas--ohlc-bindings (bars _props)
  "ohlc slots: the volume pane, overlays and oscillators as configured."
  (list :bars (financial-chart--window-bars bars)
        :volume (and financial-chart-show-volume (cl-some (lambda (b) (plist-get b :volume)) bars) t)
        :indicators (vconcat (financial-chart-eas--overlay-items
                              (financial-chart--window-bars bars) financial-chart-indicators))
        :oscillators (vconcat (financial-chart-eas--overlay-items
                               (financial-chart--window-bars bars) financial-chart-oscillators))))

(defun financial-chart-eas-bindings (kind data &optional props)
  "Bindings for KIND's eas template that draw DATA with PROPS."
  (let* ((entry (financial-chart-eas--entry kind))
         (adapter (plist-get entry :adapter))
         (slot (or (plist-get entry :slot) :data))
         (extra (when-let* ((fn (plist-get entry :bindings))) (funcall fn data props)))
         (bindings (list slot (if adapter (eas-data-rows (eas-data-from adapter data)) data))))
    (when (stringp (plist-get props :title))
      (setq bindings (plist-put bindings :title (plist-get props :title))))
    (dolist (pair (plist-get entry :props))
      (when-let* ((value (plist-get props (car pair))))
        (setq bindings (plist-put bindings (cdr pair) (financial-chart-eas--json-value value)))))
    (cl-loop for (key value) on extra by #'cddr
             do (setq bindings (plist-put bindings key value)))
    bindings))

;;; Render

(defun financial-chart-eas-resolve (kind data &optional props)
  "The pure Vega-Lite spec KIND's template resolves to for DATA and PROPS."
  (eas-resolve (plist-get (financial-chart-eas--entry kind) :template)
               (financial-chart-eas-bindings kind data props)))

(defun financial-chart-eas-scene (kind data backend &optional props)
  "KIND's scene for DATA on BACKEND (`text' or `svg') sized by PROPS.
Text takes :width columns and :height rows; svg :width and :height
pixels (what `financial-chart-plot' hands a renderer)."
  (let ((w (plist-get props :width)) (h (plist-get props :height)))
    (eas-compile (financial-chart-eas-resolve kind data props)
                 :target backend
                 :size (if (eq backend 'text)
                           (list :cols (or w 60) :rows (or h financial-chart-plot-height))
                         (and w h (cons w h))))))

(defun financial-chart-eas-render (kind data backend &rest props)
  "KIND drawn from DATA by its eas template on BACKEND, as a string.
Text keeps eas's text properties (help-echo, datum); svg is a document."
  (if (null data)
      (if (eq kind 'sparkline) "" nil)
    (let ((scene (financial-chart-eas-scene kind data backend props)))
      (if (eq backend 'text) (eas-text-render scene) (eas-svg-render scene)))))

;;; Parity

(defun financial-chart-eas--rows (scene mark)
  "The rows SCENE's MARK (an id) plots, as a list."
  (append (plist-get (eas-scene-mark scene mark) :rows) nil))

(defun financial-chart-eas--column (scene mark field)
  "FIELD (a keyword) of each row of SCENE's MARK, nulls dropped."
  (delq nil (mapcar (lambda (row) (let ((v (plist-get row field))) (and (numberp v) v)))
                    (financial-chart-eas--rows scene mark))))

(defun financial-chart-eas--same-p (a b)
  "Non-nil when A and B are equal, numbers within a relative 1e-9."
  (cond ((and (numberp a) (numberp b))
         (<= (abs (- a b)) (* 1e-9 (max 1.0 (abs a) (abs b)))))
        ((and (consp a) (consp b))
         (and (= (length a) (length b)) (cl-every #'financial-chart-eas--same-p a b)))
        (t (equal a b))))

(defun financial-chart-eas--series-parity (data props scene)
  "The plotted points and their range against financial-chart's summary."
  (let ((summary (financial-chart--data-summary 'series data props))
        (ys (financial-chart-eas--column scene "series" :y)))
    (list (list "points" (plist-get summary :points) (length (financial-chart-eas--rows scene "series")))
          (list "values" (financial-chart-series-values data) ys))))

(defun financial-chart-eas--payoff-parity (data _props scene)
  "P/L points and breakevens against `financial-chart-payoff-breakevens'."
  (list (list "pnl" (financial-chart-series-values data) (financial-chart-eas--column scene "payoff" :pnl))
        (list "breakevens" (financial-chart-payoff-breakevens data)
              (sort (financial-chart-eas--column scene "breakevens" :breakeven) #'<))))

(defun financial-chart-eas--bars-parity (data _props scene)
  "Labels and values, in order."
  (list (list "bars" (mapcar (lambda (p) (list (format "%s" (car p)) (cdr p))) data)
              (mapcar (lambda (r) (list (plist-get r :label) (plist-get r :value)))
                      (financial-chart-eas--rows scene "bars")))))

(defun financial-chart-eas--multi-parity (data props scene)
  "Each series' plotted values against `financial-chart-multi--prepare'."
  (let ((rows (financial-chart-eas--rows scene "series")))
    (cl-loop for (label . values) in (financial-chart-multi--prepare data (plist-get props :normalize))
             collect (list (format "series %s" label) values
                           (cl-loop for r in rows
                                    when (equal (plist-get r :series) (format "%s" label))
                                    collect (plist-get r :value))))))

(defun financial-chart-eas--payoff-curves-parity (data _props scene)
  "Every curve's P/L points."
  (let ((rows (financial-chart-eas--rows scene "curves")))
    (cl-loop for (label . curve) in data
             collect (list (format "curve %s" label) (financial-chart-series-values curve)
                           (cl-loop for r in rows
                                    when (equal (plist-get r :curve) (format "%s" label))
                                    collect (plist-get r :pnl))))))

(defun financial-chart-eas--drawdown-parity (data _props scene)
  "Running drawdowns against `financial-chart-drawdowns'."
  (list (list "drawdowns" (mapcar #'cdr (financial-chart-drawdowns data))
              (financial-chart-eas--column scene "drawdown" :drawdown))))

(defun financial-chart-eas--histogram-parity (data _props scene)
  "Return count, mean and sample deviation against financial-chart's."
  (let* ((returns (financial-chart-returns data))
         (stats (financial-chart-returns--statistics returns))
         (row (car (financial-chart-eas--rows scene "mean"))))
    (list (list "returns" (length returns) (plist-get row :n))
          (list "mean" (car stats) (plist-get row :mean))
          (list "stdev" (cdr stats) (plist-get row :stdev)))))

(defun financial-chart-eas--heatmap-parity (data _props scene)
  "Every cell, row-major."
  (list (list "cells" (apply #'append (plist-get data :rows))
              (financial-chart-eas--column scene "cells" :value))))

(defun financial-chart-eas--depth-parity (data _props scene)
  "Cumulative size per level against `financial-chart-depth--cumulative-levels'."
  (cl-loop for (side mark) in '((:bids "bids") (:asks "asks"))
           collect (list (format "cumulative %s" (substring (symbol-name side) 1))
                         (mapcar #'caddr (financial-chart-depth--cumulative-levels
                                          (financial-chart-depth--sorted-levels data side)))
                         (mapcar (lambda (r) (plist-get r :cumulative))
                                 (sort (financial-chart-eas--rows scene mark)
                                       (lambda (a b) (< (plist-get a :distance) (plist-get b :distance))))))))

(defun financial-chart-eas--volume-profile-parity (data props scene)
  "Volume per level and the point of control against financial-chart's."
  (let ((profile (financial-chart-matrix--volume-data data (or (plist-get props :bins) 24)))
        (rows (financial-chart-eas--rows scene "levels")))
    (list (list "volumes" (plist-get profile :volumes) (mapcar (lambda (r) (plist-get r :volume)) rows))
          (list "poc" (plist-get profile :poc)
                (cl-loop for r in rows when (eq (plist-get r :poc) t) return (plist-get r :bin))))))

(defun financial-chart-eas--ohlc-parity (data _props scene)
  "Candles and each configured overlay against financial-chart's series."
  (let* ((bars (financial-chart--window-bars data))
         (candles (financial-chart-eas--rows scene "candles")))
    (append
     (cl-loop for key in '(:open :high :low :close)
              collect (list (format "candles %s" (substring (symbol-name key) 1))
                            (mapcar (lambda (b) (plist-get b key)) bars)
                            (mapcar (lambda (r) (plist-get r key)) candles)))
     (cl-loop for series in (financial-chart--compute-series bars financial-chart-indicators)
              for i from 1
              ;; Overlay I is the price view's layer 1+I: after wicks and candles.
              collect (list (format "overlay %d" i) (delq nil (copy-sequence (plist-get series :series)))
                            (financial-chart-eas--column scene (format "price/%d" (1+ i)) :value))))))

(defun financial-chart-eas-parity (kind &optional data &rest props)
  "Check that KIND's eas template plots what KIND's renderer plots.
DATA defaults to the kind's example.  Returns (:kind :template :pass
:checks), each check (:check NAME :pass BOOL [:expected E :actual A]):
the template resolves to the native subset, draws as text and svg, and
every :parity number matches financial-chart's own computation."
  (let* ((entry (financial-chart-eas--entry kind))
         (data (or data (plist-get (alist-get (plist-get (financial-chart--kind kind) :shape)
                                              financial-chart-shapes)
                                   :example)))
         checks)
    (cl-flet ((check (name pass &rest more) (push (append (list :check name :pass (and pass t)) more) checks)))
      (condition-case err
          (let* ((resolved (financial-chart-eas-resolve kind data props))
                 (unsupported (eas-spec-unsupported resolved)))
            (check "native" (null unsupported)
                   :detail (mapcar (lambda (f) (plist-get f :path)) unsupported))
            (let ((scene (financial-chart-eas-scene kind data 'svg props)))
              (check "renders" (and (stringp (eas-svg-render scene))
                                    (stringp (eas-text-render
                                              (financial-chart-eas-scene kind data 'text props)))))
              (dolist (c (funcall (plist-get entry :parity) data props scene))
                (check (nth 0 c) (financial-chart-eas--same-p (nth 1 c) (nth 2 c))
                       :expected (nth 1 c) :actual (nth 2 c)))))
        (error (check "renders" nil :detail (error-message-string err)))))
    (setq checks (nreverse checks))
    (list :kind kind :template (plist-get entry :template)
          :pass (cl-every (lambda (c) (plist-get c :pass)) checks) :checks checks)))

;;; Routing

(defvar financial-chart-eas--parity-cache (make-hash-table :test 'eq)
  "KIND -> parity on its example, computed once per session.")

(defun financial-chart-eas-routed-p (kind)
  "Non-nil when `financial-chart-plot' should draw KIND with its template."
  (and (alist-get kind financial-chart-eas-kinds)
       (pcase financial-chart-eas-route
         ('nil nil)
         ('t (with-memoization (gethash kind financial-chart-eas--parity-cache)
               (plist-get (financial-chart-eas-parity kind) :pass)))
         (kinds (memq kind kinds)))))

(defun financial-chart-eas--route (kind backend)
  "`financial-chart-plot-route-functions' entry: KIND's template on BACKEND."
  (when (financial-chart-eas-routed-p kind)
    (list :renderer (lambda (data &rest args) (apply #'financial-chart-eas-render kind data backend args))
          :template (plist-get (alist-get kind financial-chart-eas-kinds) :template)
          :reason (if (eq financial-chart-eas-route t)
                      "financial-chart-eas-route is t and the template is at parity"
                    "financial-chart-eas-route lists this kind"))))

(add-hook 'financial-chart-plot-route-functions #'financial-chart-eas--route)

(provide 'financial-chart-eas-route)
;;; financial-chart-eas-route.el ends here
