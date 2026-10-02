;;; financial-chart-volume-indicators.el --- Built-in volume indicators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure-Elisp volume indicators over provider-neutral normalized bars.
;; A bar is a plist with :high, :low, :close and :volume fields. Missing
;; or invalid input values produce nil at that position. Cumulative
;; indicators restart their accumulation after a missing bar.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-indicator-api)

(defun financial-chart--volume-number (bar key &optional nonnegative)
  "Return BAR's numeric KEY, or nil when absent or invalid.
NONNEGATIVE means a negative value is invalid."
  (let ((value (plist-get bar key)))
    (and (numberp value)
         (or (not nonnegative) (>= value 0))
         value)))

(defun financial-chart--volume-bar-fields (bar)
  "Return (HIGH LOW CLOSE VOLUME) for BAR, or nil when invalid."
  (let ((high (financial-chart--volume-number bar :high))
        (low (financial-chart--volume-number bar :low))
        (close (financial-chart--volume-number bar :close))
        (volume (financial-chart--volume-number bar :volume t)))
    (and high low close volume (>= high low)
         (list high low close volume))))

(defun financial-chart-obv (bars)
  "Return on-balance volume (OBV) for BARS.
The first valid close/volume pair seeds OBV with its volume.  Later
volume is added on a higher close and subtracted on a lower close.  A
missing or invalid close/volume produces nil and restarts the cumulative
series at the next valid bar."
  (let ((previous-close nil)
        (total nil))
    (mapcar
     (lambda (bar)
       (let ((close (financial-chart--volume-number bar :close))
             (volume (financial-chart--volume-number bar :volume t)))
         (if (not (and close volume))
             (progn (setq previous-close nil total nil) nil)
           (let ((value
                  (if (null previous-close)
                      (setq total volume)
                    (setq total
                          (+ total
                             (cond ((> close previous-close) volume)
                                   ((< close previous-close) (- volume))
                                   (t 0)))))))
             (setq previous-close close)
             value))))
     bars)))

(defun financial-chart-accumulation-distribution (bars)
  "Return the cumulative accumulation/distribution line for BARS.
Invalid bars produce nil and restart the cumulative line on the next
valid bar.  A zero high-low range contributes zero money flow."
  (let ((total nil))
    (mapcar
     (lambda (bar)
       (let ((fields (financial-chart--volume-bar-fields bar)))
         (if (null fields)
             (progn (setq total nil) nil)
           (pcase-let ((`(,high ,low ,close ,volume) fields))
             (let ((flow
                    (if (= high low)
                        0.0
                      (* (/ (float (- (* 2 close) high low)) (- high low))
                         volume))))
               (setq total (+ (or total 0.0) flow)))))))
     bars)))

(defun financial-chart-money-flow-index (bars &optional period)
  "Return the money flow index (MFI) for BARS over PERIOD bars.
PERIOD defaults to 14 money-flow observations (15 bars including the
initial typical price).  Values are in [0,100].  Missing or invalid
bars break the calculation; output resumes after PERIOD contiguous
valid money-flow observations.  A flat window returns 50."
  (let* ((period (or period 14))
         (typical-prices nil)
         (positive-flows nil)
         (negative-flows nil)
         (run 0)
         result)
    (unless (and (integerp period) (> period 0))
      (signal 'financial-chart-invalid-indicator
              (list "MFI period must be a positive integer" :period period)))
    (dolist (bar bars (nreverse result))
      (let ((fields (financial-chart--volume-bar-fields bar)))
        (if (null fields)
            (setq typical-prices nil positive-flows nil negative-flows nil run 0
                  result (cons nil result))
          (pcase-let ((`(,high ,low ,close ,volume) fields))
            (let* ((typical (/ (+ high low close) 3.0))
                   (previous (car typical-prices))
                   (money-flow (* typical volume)))
              (push typical typical-prices)
              (when previous
                (push (if (> typical previous) money-flow 0.0) positive-flows)
                (push (if (< typical previous) money-flow 0.0) negative-flows)
                (setq run (1+ run)))
              (push
               (if (< run period)
                   nil
                 (let ((positive (cl-loop for flow in (cl-subseq positive-flows 0 period)
                                          sum flow))
                       (negative (cl-loop for flow in (cl-subseq negative-flows 0 period)
                                          sum flow)))
                   (cond ((and (= positive 0) (= negative 0)) 50.0)
                         ((= negative 0) 100.0)
                         ((= positive 0) 0.0)
                         (t (- 100.0 (/ 100.0 (1+ (/ positive negative))))))))
               result))))))))

(defun financial-chart-chaikin-money-flow (bars &optional period)
  "Return Chaikin money flow (CMF) for BARS over PERIOD bars.
PERIOD defaults to 20.  Output is nil until a complete contiguous
window of valid money-flow volume is available, and whenever that
window's total volume is zero.  Invalid bars break the window."
  (let* ((period (or period 20))
         (flows nil)
         (volumes nil)
         (run 0)
         result)
    (unless (and (integerp period) (> period 0))
      (signal 'financial-chart-invalid-indicator
              (list "CMF period must be a positive integer" :period period)))
    (dolist (bar bars (nreverse result))
      (let ((fields (financial-chart--volume-bar-fields bar)))
        (if (null fields)
            (setq flows nil volumes nil run 0 result (cons nil result))
          (pcase-let ((`(,high ,low ,close ,volume) fields))
            (let ((flow
                   (if (= high low)
                       0.0
                     (* (/ (float (- (* 2 close) high low)) (- high low)) volume))))
              (push flow flows)
              (push volume volumes)
              (setq run (1+ run))
              (push
               (if (< run period)
                   nil
                 (let ((total-volume (cl-loop for value in (cl-subseq volumes 0 period)
                                              sum value)))
                   (and (> total-volume 0)
                        (/ (cl-loop for value in (cl-subseq flows 0 period) sum value)
                           (float total-volume)))))
               result))))))))

(defun financial-chart--volume-ema (values period)
  "Return EMA of VALUES seeded by PERIOD values, preserving nil gaps."
  (let ((alpha (/ 2.0 (1+ period)))
        (seed nil)
        (run 0)
        (previous nil))
    (mapcar
     (lambda (value)
       (if (not (numberp value))
           (progn (setq seed nil run 0 previous nil) nil)
         (if previous
             (setq previous (+ (* alpha value) (* (- 1 alpha) previous)))
           (push value seed)
           (setq run (1+ run))
           (when (>= run period)
             (setq previous (/ (apply #'+ (cl-subseq (nreverse seed) 0 period))
                               (float period)))))
         previous))
     values)))

(defun financial-chart-chaikin-oscillator (bars &optional fast slow)
  "Return the Chaikin oscillator for BARS using FAST and SLOW EMAs.
FAST defaults to 3 and SLOW to 10.  It is the difference between those
EMAs of the cumulative accumulation/distribution line.  Output is nil
until the slow EMA has enough contiguous valid observations."
  (let* ((fast (or fast 3))
         (slow (or slow 10))
         (ad (financial-chart-accumulation-distribution bars)))
    (unless (and (integerp fast) (> fast 0) (integerp slow) (> slow 0)
                 (< fast slow))
      (signal 'financial-chart-invalid-indicator
              (list "Chaikin periods must be positive integers with fast < slow"
                    :fast fast :slow slow)))
    (cl-mapcar (lambda (fast-value slow-value)
                 (and fast-value slow-value (- fast-value slow-value)))
               (financial-chart--volume-ema ad fast)
               (financial-chart--volume-ema ad slow))))

(financial-chart-register-indicator
 'obv #'financial-chart-obv
 :label "OBV" :unit :volume :panel :oscillator :scale :linear
 :description "On-balance volume.")
(financial-chart-register-indicator
 'accumulation-distribution #'financial-chart-accumulation-distribution
 :label "A/D" :unit :volume :panel :oscillator :scale :linear
 :description "Cumulative accumulation/distribution line.")
(financial-chart-register-indicator
 'money-flow-index #'financial-chart-money-flow-index
 :label "MFI" :unit :percent :panel :oscillator :scale :bounded
 :bounds '(0 . 100)
 :description "Money flow index (default period 14).")
(financial-chart-register-indicator
 'chaikin-money-flow #'financial-chart-chaikin-money-flow
 :label "CMF" :unit :ratio :panel :oscillator :scale :linear
 :description "Chaikin money flow (default period 20).")
(financial-chart-register-indicator
 'chaikin-oscillator #'financial-chart-chaikin-oscillator
 :label "Chaikin Oscillator" :unit :volume :panel :oscillator :scale :linear
 :description "Difference of 3- and 10-period EMAs of the A/D line.")

(provide 'financial-chart-volume-indicators)
;;; financial-chart-volume-indicators.el ends here

