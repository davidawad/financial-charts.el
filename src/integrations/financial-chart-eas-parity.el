;;; financial-chart-eas-parity.el --- Check that eas templates plot financial-chart's numbers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every financial-chart kind is drawn by an eas template.  Some
;; templates compute in Vega-Lite what financial-chart also computes in
;; Lisp (drawdowns, breakevens, return statistics, cumulative depth,
;; volume per level).  `financial-chart-eas-parity' checks, as data,
;; that a template plots the same numbers as financial-chart's own
;; functions; `financial-chart-eas-parity-checks' maps each kind to its
;; check.

;;; Code:

(require 'cl-lib)
(require 'eas)
(require 'financial-chart-plot)
(require 'financial-chart-indicators)
(require 'financial-chart-returns)
(require 'financial-chart-multi)
(require 'financial-chart-matrix)
(require 'financial-chart-depth)
(require 'financial-chart-payoff-curves)

(defvar financial-chart-eas-parity-checks
  '((ohlc . financial-chart-eas--ohlc-parity)
    (area . financial-chart-eas--series-parity)
    (line . financial-chart-eas--series-parity)
    (sparkline . financial-chart-eas--series-parity)
    (payoff . financial-chart-eas--payoff-parity)
    (bars . financial-chart-eas--bars-parity)
    (multi . financial-chart-eas--multi-parity)
    (payoff-curves . financial-chart-eas--payoff-curves-parity)
    (drawdown . financial-chart-eas--drawdown-parity)
    (histogram . financial-chart-eas--histogram-parity)
    (heatmap . financial-chart-eas--heatmap-parity)
    (depth . financial-chart-eas--depth-parity)
    (volume-profile . financial-chart-eas--volume-profile-parity))
  "KIND -> (FN DATA PROPS SCENE) returning checks (NAME EXPECTED ACTUAL)
of the numbers KIND's template plots against financial-chart's own.")

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
  (let ((profile (financial-chart-volume-profile data (or (plist-get props :bins) 24)))
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
  "Check that KIND's eas template plots what financial-chart computes.
DATA defaults to the kind's example.  Returns (:kind :template :pass
:checks), each check (:check NAME :pass BOOL [:expected E :actual A]):
the template resolves to the native subset, draws as text and svg, and
every :parity number matches financial-chart's own computation."
  (let* ((check-fn (or (alist-get kind financial-chart-eas-parity-checks)
                       (signal 'financial-chart-unknown-kind
                               (list (format "%S has no parity check; checked kinds: %s" kind
                                             (mapconcat (lambda (e) (symbol-name (car e)))
                                                        financial-chart-eas-parity-checks ", "))
                                     :code "unknown_kind" :kind kind))))
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
              (dolist (c (funcall check-fn data props scene))
                (check (nth 0 c) (financial-chart-eas--same-p (nth 1 c) (nth 2 c))
                       :expected (nth 1 c) :actual (nth 2 c)))))
        (error (check "renders" nil :detail (error-message-string err)))))
    (setq checks (nreverse checks))
    (list :kind kind :template (plist-get (financial-chart--kind kind) :template)
          :pass (cl-every (lambda (c) (plist-get c :pass)) checks) :checks checks)))

(defun financial-chart-eas-parity-doctor-checks ()
  "Eager doctor rows: each kind's template plots financial-chart's numbers."
  (mapcar (lambda (entry)
            (let* ((result (financial-chart-eas-parity (car entry)))
                   (failed (cl-remove-if (lambda (c) (plist-get c :pass))
                                         (plist-get result :checks))))
              (financial-chart--check
               (format "kind %s template parity" (car entry)) (plist-get result :pass)
               (if failed
                   (format "mismatched: %s"
                           (mapconcat (lambda (c) (format "%s" (plist-get c :check))) failed ", "))
                 (format "template %s plots the same numbers" (plist-get result :template)))
               (format "(financial-chart-eas-parity '%s) shows expected and actual" (car entry)))))
          financial-chart-eas-parity-checks))

(provide 'financial-chart-eas-parity)
;;; financial-chart-eas-parity.el ends here
