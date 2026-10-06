;;; financial-chart-validate.el --- Strict validation of caller-supplied chart data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; financial-chart draws only data its caller supplies, so it checks
;; that data before anything is drawn.  Every validator here signals
;; `financial-chart-invalid-data' with data (MESSAGE :code CODE :index
;; INDEX :field FIELD): CODE is a stable reason, INDEX the offending
;; element (nil when the whole value is wrong), FIELD the offending
;; field.  `financial-chart-error-data' turns the error into a plist.
;;
;; Codes: not_a_list, invalid_point, not_a_number, price_not_ascending,
;; invalid_label, not_a_plist, missing_field, high_below_body,
;; low_above_body, negative_volume, time_not_increasing, invalid_scale,
;; nonpositive_log, indicator_length, indicator_misaligned.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-core)
(require 'financial-chart-series)

(defun financial-chart--finite-p (value)
  "Non-nil when VALUE is a number that is neither NaN nor infinite."
  (and (numberp value)
       (or (integerp value)
           (and (not (isnan value)) (< (abs value) 1.0e+INF)))))

(defun financial-chart--validate-series (data)
  "Signal unless DATA is a SERIES: numbers, (X Y) or (X . Y), nil Y allowed."
  (unless (or (proper-list-p data) (vectorp data))
    (financial-chart--invalid nil nil "not_a_list" "series must be a list or vector, got %S" data))
  (let ((i 0))
    (seq-doseq (p data)
      (unless (or (numberp p) (consp p))
        (financial-chart--invalid i nil "invalid_point"
                                  "expected a number, (X Y) or (X . Y), got %S" p))
      (let ((y (financial-chart-series--point-y p)))
        (unless (or (and (consp p) (null y)) (financial-chart--finite-p y))
          (financial-chart--invalid i "y" "not_a_number"
                                    "Y must be a finite number (or nil to skip), got %S" y)))
      (cl-incf i))))

(defun financial-chart--validate-payoff (data)
  "Signal unless DATA is a PAYOFF: numeric (PRICE PNL), ascending price."
  (financial-chart--validate-series data)
  (let ((i 0) prev)
    (seq-doseq (p data)
      (let ((x (financial-chart-series--point-x p)))
        (unless (financial-chart--finite-p x)
          (financial-chart--invalid i "price" "not_a_number"
                                    "payoff points need a numeric PRICE: (PRICE PNL), got %S" p))
        (unless (financial-chart--finite-p (financial-chart-series--point-y p))
          (financial-chart--invalid i "pnl" "not_a_number"
                                    "payoff points need a numeric PNL: (PRICE PNL), got %S" p))
        (when (and prev (< x prev))
          (financial-chart--invalid i "price" "price_not_ascending"
                                    "prices must ascend; %s follows %s (sort by price)" x prev))
        (setq prev x))
      (cl-incf i))))

(defun financial-chart--validate-labeled (data)
  "Signal unless DATA is a list of (LABEL . NUMBER)."
  (unless (proper-list-p data)
    (financial-chart--invalid nil nil "not_a_list" "labeled data must be a list, got %S" data))
  (cl-loop for p in data for i from 0
           do (unless (and (consp p) (or (stringp (car p)) (symbolp (car p)) (numberp (car p))))
                (financial-chart--invalid i "label" "invalid_label"
                                          "expected (LABEL . NUMBER) with a string or symbol LABEL, got %S" p))
           (unless (financial-chart--finite-p (cdr p))
             (financial-chart--invalid i "value" "not_a_number"
                                       "expected (LABEL . NUMBER), got %S" p))))

(defun financial-chart--validate-bar (bar i)
  "Signal unless BAR, element I, is a bar/v1 plist with consistent prices."
  (unless (and (proper-list-p bar) (cl-evenp (length bar)) (keywordp (car bar)))
    (financial-chart--invalid i nil "not_a_plist"
                              "a bar is a plist (:open :high :low :close [:volume] [:time]), got %S" bar))
  (dolist (key '(:open :high :low :close))
    (let ((field (substring (symbol-name key) 1))
          (value (plist-get bar key)))
      (unless (plist-member bar key)
        (financial-chart--invalid i field "missing_field" "required field %s is missing" key))
      (unless (financial-chart--finite-p value)
        (financial-chart--invalid i field "not_a_number" "%s must be a finite number, got %S" key value))))
  (let ((open (plist-get bar :open)) (high (plist-get bar :high))
        (low (plist-get bar :low)) (close (plist-get bar :close))
        (volume (plist-get bar :volume)))
    (when (< high (max open close))
      (financial-chart--invalid i "high" "high_below_body"
                                "high %s is below max(open %s, close %s); high must be the bar's top"
                                high open close))
    (when (> low (min open close))
      (financial-chart--invalid i "low" "low_above_body"
                                "low %s is above min(open %s, close %s); low must be the bar's bottom"
                                low open close))
    (when (and volume (not (and (financial-chart--finite-p volume) (>= volume 0))))
      (financial-chart--invalid i "volume" "negative_volume"
                                "volume must be a non-negative number when present, got %S" volume))))

(defun financial-chart--validate-ohlc (data)
  "Signal unless DATA is a list of bar/v1 plists, oldest first.
Each bar has finite :open :high :low :close with high >= max(open,
close) >= min(open, close) >= low, and a non-negative :volume when
present.  When any bar has :time, every bar has one (epoch
milliseconds) and the times strictly increase."
  (unless (proper-list-p data)
    (financial-chart--invalid nil nil "not_a_list" "bars must be a list of bar/v1 plists, got %S" data))
  (let ((timed (cl-some (lambda (bar) (and (proper-list-p bar) (cl-evenp (length bar))
                                           (plist-get bar :time)))
                        data))
        prev)
    (cl-loop for bar in data for i from 0
             do (financial-chart--validate-bar bar i)
             (when timed
               (let ((time (plist-get bar :time)))
                 (unless time
                   (financial-chart--invalid i "time" "missing_field"
                                             "other bars carry :time, so every bar needs one"))
                 (unless (financial-chart--finite-p time)
                   (financial-chart--invalid i "time" "not_a_number"
                                             ":time must be epoch milliseconds, got %S" time))
                 (when (and prev (<= time prev))
                   (financial-chart--invalid i "time" "time_not_increasing"
                                             ":time %s does not follow %s; bars must be oldest first with distinct times"
                                             time prev))
                 (setq prev time))))))

(defun financial-chart--validate-scale (series scale)
  "Signal unless SCALE (`linear' or `log', nil = linear) suits SERIES.
A log scale needs every Y positive."
  (unless (memq scale '(nil linear log))
    (financial-chart--invalid nil "scale" "invalid_scale" ":scale must be `linear' or `log', got %S" scale))
  (when (eq scale 'log)
    (let ((index 0))
      (seq-doseq (point series)
        (let ((value (financial-chart-series--point-y point)))
          (when (and value (<= value 0))
            (financial-chart--invalid index "y" "nonpositive_log"
                                      "log scale requires positive values; use :scale 'linear or a positive Y")))
        (cl-incf index)))))

(defun financial-chart-validate-indicator-series (series bars &optional name)
  "Signal unless indicator SERIES lines up with BARS, one value per bar.
SERIES is a list or vector of numbers-or-nil, or an indicator-series/v1
plist whose :values are those and whose optional :timestamps must equal
each bar's :time.  NAME (default SERIES' :name) labels the messages
and is the error's :field.  Returns t."
  (let* ((plist (and (consp series) (keywordp (car series))))
         (values (append (if plist (plist-get series :values) series) nil))
         (stamps (and plist (append (plist-get series :timestamps) nil)))
         (name (format "%s" (or name (and plist (plist-get series :name)) "indicator"))))
    (unless (= (length values) (length bars))
      (financial-chart--invalid (min (length values) (length bars)) name "indicator_length"
                                "%s has %d values for %d bars; give one per bar (nil for none)"
                                name (length values) (length bars)))
    (cl-loop for v in values for i from 0
             unless (or (null v) (financial-chart--finite-p v))
             do (financial-chart--invalid i name "not_a_number"
                                          "%s value must be a finite number or nil, got %S" name v))
    (when stamps
      (unless (= (length stamps) (length bars))
        (financial-chart--invalid (min (length stamps) (length bars)) name "indicator_length"
                                  "%s has %d timestamps for %d bars" name (length stamps) (length bars)))
      (cl-loop for stamp in stamps for bar in bars for i from 0
               unless (equal stamp (plist-get bar :time))
               do (financial-chart--invalid i name "indicator_misaligned"
                                            "%s timestamp %s does not match the bar's :time %s"
                                            name stamp (plist-get bar :time))))
    t))

(provide 'financial-chart-validate)
;;; financial-chart-validate.el ends here
