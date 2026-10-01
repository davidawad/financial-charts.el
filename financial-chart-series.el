;;; financial-chart-series.el --- Plain-data series shapes and helpers for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The shapes the generic chart kinds accept, plus the pure numeric
;; helpers both backends share (resampling, ranges, number formatting,
;; payoff breakevens), so text and SVG can never disagree.
;;
;;   SERIES  numbers, (X Y) lists or (X . Y) conses, oldest first
;;   PAYOFF  a SERIES of (PRICE PNL), sorted by price
;;   LABELED a list of (LABEL . VALUE) conses, e.g. P/L per position
;;   OHLC    (:open :high :low :close [:volume] [:time]) plists, oldest first

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-core)

(defface financial-chart-up '((t :inherit success))
  "Face for rising series, positive P/L and bid-side depth."
  :group 'financial-chart)

(defface financial-chart-down '((t :inherit error))
  "Face for falling series, negative P/L and ask-side depth."
  :group 'financial-chart)

(defface financial-chart-dim '((t :inherit shadow))
  "Face for axis labels and secondary annotations."
  :group 'financial-chart)

(defface financial-chart-accent '((t :inherit font-lock-keyword-face))
  "Face for the headline number of a chart (e.g. the last value)."
  :group 'financial-chart)

(defconst financial-chart-blocks " ▁▂▃▄▅▆▇█"
  "Eighth-height block glyphs, index = filled eighths (0..8).")

(defcustom financial-chart-plot-height 12
  "Default chart height in text rows for the generic chart kinds."
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

(defun financial-chart-resample (values width)
  "Average VALUES (numbers) into at most WIDTH column means.
Fewer values than WIDTH are returned unchanged (one column each)."
  (let* ((v (vconcat values))
         (n (length v))
         (per (max 1 (/ (float n) width))))
    (cl-loop
     for col from 0 below (min width n)
     for start = (floor (* col per))
     for end = (max (1+ start) (floor (* (1+ col) per)))
     for lo = (min start (1- n))
     for hi = (min end n)
     when (< lo hi)
     collect (/ (cl-loop for i from lo below hi sum (aref v i))
                (float (- hi lo))))))

(defun financial-chart-range (values &optional include-zero)
  "Return (LO . HI) over VALUES; INCLUDE-ZERO widens it to span 0."
  (let ((lo (apply #'min values))
        (hi (apply #'max values)))
    (if include-zero
        (cons (min lo 0) (max hi 0))
      (cons lo hi))))

(defun financial-chart-payoff-range (payoff)
  "Return PAYOFF's (MIN . MAX) P/L range, including zero.
Use the original payoff points so renderer sampling cannot hide an
extreme.  Return nil when PAYOFF has no values."
  (let ((values (financial-chart-series-values payoff)))
    (when values (financial-chart-range values t))))

(defun financial-chart-fmt (v)
  "V to at most one decimal, no trailing zeros: 89.3333 -> \"89.3\"."
  (format "%g" (/ (round (* v 10)) 10.0)))

(defun financial-chart-fmt-money (v &optional unit)
  "V signed, then UNIT, then magnitude: 75 \"$\" -> \"+$75\", -25 -> \"-$25\"."
  (concat (cond ((> v 0) "+") ((< v 0) "-") (t "")) (or unit "") (financial-chart-fmt (abs v))))

(defun financial-chart-direction-face (values up-face down-face)
  "UP-FACE when the last of VALUES >= the first, else DOWN-FACE."
  (if (>= (car (last values)) (car values)) up-face down-face))

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

(defun financial-chart-interpolate (series width)
  "WIDTH Y values sampled evenly across SERIES' X span, linearly interpolated.
SERIES needs numeric X sorted ascending (a PAYOFF); with fewer than two
such points, or at least WIDTH of them, this is `financial-chart-resample' of
its values instead -- interpolation only ever adds resolution."
  (let ((xs (financial-chart-series-xs series))
        (ys (financial-chart-series-values series)))
    (if (or (< (length ys) 2) (>= (length ys) width) (not (cl-every #'numberp xs))
            (= (car xs) (car (last xs))))
        (financial-chart-resample ys width)
      (let* ((xv (vconcat xs)) (yv (vconcat ys)) (n (length xv))
             (x0 (aref xv 0)) (step (/ (float (- (aref xv (1- n)) x0)) (1- width)))
             (i 0))
        (cl-loop for c from 0 below width
                 for x = (+ x0 (* c step))
                 do (while (and (< i (- n 2)) (> x (aref xv (1+ i)))) (cl-incf i))
                 collect (let ((xa (aref xv i)) (xb (aref xv (1+ i))))
                           (+ (aref yv i)
                              (* (- (aref yv (1+ i)) (aref yv i))
                                 (if (= xa xb) 0 (/ (- x xa) (float (- xb xa))))))))))))

(defun financial-chart-series--interpolate-xs (xs ys width)
  "Sample Y values evenly over numeric XS across WIDTH columns."
  (let* ((xv (vconcat xs))
         (yv (vconcat ys))
         (n (length xv))
         (width (max 1 width))
         (x0 (aref xv 0))
         (x1 (aref xv (1- n)))
         (ascending (< x0 x1))
         (step (if (= width 1) 0 (/ (float (- x1 x0)) (1- width))))
         (i 0))
    (cl-loop for c from 0 below width
             for x = (if (= width 1) (/ (+ x0 x1) 2.0) (+ x0 (* c step)))
             do (while (and (< i (- n 2))
                            (if ascending (> x (aref xv (1+ i)))
                              (< x (aref xv (1+ i)))))
                  (cl-incf i))
             collect
             (let ((xa (aref xv i))
                   (xb (aref xv (1+ i))))
               (+ (aref yv i)
                  (* (- (aref yv (1+ i)) (aref yv i))
                     (if (= xa xb) 0 (/ (- x xa) (float (- xb xa))))))))))

(defun financial-chart-series-x-aware-p (series)
  "Whether SERIES has distinct numeric X coordinates worth projecting."
  (let ((xs (financial-chart-series-xs series)))
    (and (>= (length xs) 2)
         (cl-every #'numberp xs)
         (/= (car xs) (car (last xs))))))

(defun financial-chart-series-resample (series width)
  "Sample SERIES into WIDTH columns, respecting numeric X coordinates.
Plain values retain `financial-chart-resample''s existing output.  Numeric
X is linearly interpolated over its full span so wider X gaps occupy more
columns."
  (let* ((xs (financial-chart-series-xs series))
         (ys (financial-chart-series-values series))
         (width (max 1 width)))
    (if (financial-chart-series-x-aware-p series)
        (financial-chart-series--interpolate-xs xs ys width)
      (financial-chart-resample ys width))))

(defun financial-chart-series-x-axis-labels (series &optional width)
  "Return (POSITION LABEL) ticks for epoch-millisecond X in SERIES, or nil.
POSITION is a fraction from 0 to 1.  There are 2 to 4 evenly-spaced
labels, formatted with `financial-chart-x-axis-format'.  WIDTH limits
the label count when the text labels would otherwise overlap."
  (let ((xs (financial-chart-series-xs series)))
    (when (and (>= (length xs) 2)
               (cl-every (lambda (x) (and (numberp x) (> x 1e11))) xs)
               (/= (apply #'min xs) (apply #'max xs)))
      (let* ((x0 (car xs))
             (x1 (car (last xs)))
             (limit (min 4 (length xs) (max 2 financial-chart-x-axis-label-count)))
             (width (max 1 (or width 60)))
             (count limit)
             labels)
        (while (and (> count 2)
                    (< (/ (float (1- width)) (1- count))
                    (length (format-time-string financial-chart-x-axis-format
                                                   (/ x0 1000.0)))))
          (cl-decf count))
        (setq labels
              (cl-loop for i from 0 below count
                       for position = (/ (float i) (1- count))
                       for time = (+ x0 (* position (- x1 x0)))
                       collect
                       (list position
                             (format-time-string financial-chart-x-axis-format
                                                 (/ time 1000.0)))))
        labels))))

(defun financial-chart-series-validate-scale (series scale)
  "Validate SCALE for SERIES, signaling for non-positive log values."
  (unless (memq scale '(linear log))
    (error "financial-chart: scale must be `linear' or `log', got %S" scale))
  (when (eq scale 'log)
    (let ((index 0))
      (seq-doseq (point series)
        (let ((value (financial-chart-series--point-y point)))
          (when (and value (<= value 0))
            (signal 'financial-chart-invalid-data
                    (list (format "element %d: log scale requires positive values; use :scale 'linear or provide a positive Y" index)
                          :code "invalid_data" :index index))))
        (cl-incf index)))))

(defun financial-chart-series-scale-value (value scale)
  "Convert VALUE into SCALE space for `linear' or `log'."
  (if (eq scale 'log) (log value) value))

(defun financial-chart-series-unscale-value (value scale)
  "Convert SCALE-space VALUE back into data space."
  (if (eq scale 'log) (exp value) value))

(defun financial-chart-series-scale-span (lo hi scale)
  "Range span for scaled values LO and HI under SCALE.
Preserve narrow nonzero log ranges; retain the linear renderer's floor."
  (let ((span (- hi lo)))
    (if (eq scale 'log)
        (if (zerop span) 0.001 span)
      (max 0.001 span))))

(defun financial-chart-ohlc-closes (bars)
  "The :close of every OHLC plist in BARS, as a SERIES of (TIME CLOSE)."
  (mapcar (lambda (b) (list (plist-get b :time) (plist-get b :close))) bars))

(provide 'financial-chart-series)
;;; financial-chart-series.el ends here
