;;; financial-chart-series.el --- Plain-data series shapes and helpers for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The shapes the chart kinds accept and the pure numeric helpers over
;; them: point access, payoff breakevens and the OHLCV volume profile.
;;
;;   SERIES  numbers, (X Y) lists or (X . Y) conses, oldest first
;;   PAYOFF  a SERIES of (PRICE PNL), sorted by price
;;   LABELED a list of (LABEL . VALUE) conses, e.g. P/L per position
;;   OHLC    bar/v1 plists (:open :high :low :close [:volume] [:time]),
;;           oldest first

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-core)

(defface financial-chart-dim '((t :inherit shadow))
  "Face for notes around a chart, such as \"no data\"."
  :group 'financial-chart)

(defcustom financial-chart-plot-height 12
  "Default chart height in text rows."
  :type 'natnum
  :group 'financial-chart)

(defcustom financial-chart-plot-width nil
  "Default chart width in columns; nil derives it from the window."
  :type '(choice (const :tag "From window" nil) natnum)
  :group 'financial-chart)

;; --- data normalization -------------------------------------------------------

(defun financial-chart-series--point-x (p)
  "X of series element P (nil for a bare number)."
  (cond
   ((numberp p) nil)
   ((consp (cdr-safe p)) (car p))
   ((consp p) (car p))))

(defun financial-chart-series--point-y (p)
  "Y of series element P: P itself, the cadr of (X Y), or the cdr of (X . Y)."
  (cond
   ((numberp p) p)
   ((consp (cdr-safe p)) (cadr p))
   ((consp p) (cdr p))))

(defun financial-chart-series-values (series)
  "Return SERIES (see Commentary) as a list of numbers, dropping nils."
  (delq nil (mapcar #'financial-chart-series--point-y (append series nil))))

(defun financial-chart-series-xs (series)
  "Return the X of every element of SERIES that has a numeric Y."
  (let (out)
    (seq-doseq (p series)
      (when (financial-chart-series--point-y p)
        (push (financial-chart-series--point-x p) out)))
    (nreverse out)))

(defun financial-chart-fmt (v)
  "V to at most one decimal, no trailing zeros: 89.3333 -> \"89.3\"."
  (format "%g" (/ (round (* v 10)) 10.0)))

(defun financial-chart-payoff-breakevens (payoff)
  "Prices where PAYOFF (see Commentary) crosses zero, by linear interpolation.
A point sitting exactly on zero counts once."
  (let ((xs (financial-chart-series-xs payoff))
        (ys (financial-chart-series-values payoff))
        out)
    (cl-loop
     for (x0 x1) on xs
     for (y0 y1) on ys
     while x1
     do (cond
         ((zerop y0) (push x0 out))
         ((< (* y0 y1) 0)
          (push (+ x0 (* (- x1 x0) (/ (float (- y0)) (- y1 y0)))) out))))
    (when (and ys (zerop (car (last ys))))
      (push (car (last xs)) out))
    (delete-dups (nreverse out))))

(defun financial-chart-ohlc-closes (bars)
  "The :close of every OHLC plist in BARS, as a SERIES of (TIME CLOSE)."
  (mapcar (lambda (b) (list (plist-get b :time) (plist-get b :close))) bars))

;; --- volume profile -------------------------------------------------------

(defconst financial-chart-volume-profile-max-bins 512
  "Maximum number of volume-profile price bins.")

(defun financial-chart-volume-profile (bars bins)
  "Aggregate OHLCV BARS into BINS equal price intervals.
Each bar's volume is spread evenly over the bins its low-high range
touches: an OHLCV estimate, not trade-level data.  Returns (:low :high
:step :volumes :poc :last-close); :poc is the index of the level with
the most volume (nil when there is none).  nil for no BARS."
  (unless (and (integerp bins) (> bins 0)
               (<= bins financial-chart-volume-profile-max-bins))
    (financial-chart--invalid nil "bins" "invalid_bins"
                              ":bins must be an integer from 1 to %d"
                              financial-chart-volume-profile-max-bins))
  (when bars
    (let* ((raw-low (apply #'min (mapcar (lambda (bar) (plist-get bar :low)) bars)))
           (raw-high (apply #'max (mapcar (lambda (bar) (plist-get bar :high)) bars)))
           (flat (= raw-low raw-high))
           (low (if flat (- raw-low 0.5) raw-low))
           (high (if flat (+ raw-high 0.5) raw-high))
           (step (/ (- high low) (float bins)))
           (volumes (make-vector bins 0.0)))
      (cl-loop for bar in bars
               for bar-index from 0
               for bar-low = (min (plist-get bar :low) (plist-get bar :high))
               for bar-high = (max (plist-get bar :low) (plist-get bar :high))
               for volume = (or (plist-get bar :volume) 0)
               do (unless (and (numberp volume) (>= volume 0))
                    (financial-chart--invalid bar-index "volume" "negative_volume"
                                              "volume must be a non-negative number when present"))
               do (if (= bar-low bar-high)
                      (let ((index (min (1- bins)
                                        (max 0 (floor (/ (- bar-low low) step))))))
                        (aset volumes index (+ (aref volumes index) volume)))
                    (cl-loop for index from 0 below bins
                             for bin-low = (+ low (* index step))
                             for bin-high = (+ bin-low step)
                             for overlap = (max 0 (- (min bar-high bin-high)
                                                    (max bar-low bin-low)))
                             when (> overlap 0)
                             do (aset volumes index
                                      (+ (aref volumes index)
                                         (* volume (/ overlap (- bar-high bar-low))))))))
      (list :low low :high high :step step :volumes (append volumes nil)
            :poc (and (> (apply #'+ (append volumes nil)) 0)
                      (cl-position (apply #'max (append volumes nil)) volumes))
            :last-close (plist-get (car (last bars)) :close)))))

(provide 'financial-chart-series)
;;; financial-chart-series.el ends here
