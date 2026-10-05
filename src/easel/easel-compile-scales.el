;;; easel-compile-scales.el --- scale domains for a compiled view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  One view (a unit or a layer) shares one scale per
;; channel, as in Vega-Lite's default "shared" resolution.  Domains
;; follow Vega-Lite's defaults: zero and nice for linear x/y unless
;; binned or explicit, discrete domains sorted ascending, bars on band
;; scales, other discrete marks on point scales.  Zoomed domains from
;; view state replace the computed ones.  Ranges are set after layout.

;;; Code:

(require 'easel-core)
(require 'easel-scale)
(require 'easel-encode)

(defun easel-compile--defs (units channel)
  "Return (UNIT . DEF) pairs for CHANNEL across UNITS with a data def."
  (cl-loop for u in units
           for d = (plist-get (plist-get u :encoding) channel)
           when (and d (easel-object-p d) (or (plist-get d :field) (plist-member d :datum)))
           collect (cons u d)))

(defun easel-compile--values (pairs channel)
  "Raw values feeding CHANNEL's domain from PAIRS of (UNIT . DEF)."
  (let (out)
    (dolist (pair pairs)
      (let* ((u (car pair)) (d (cdr pair)) (enc (plist-get u :encoding))
             (partner (plist-get enc (if (eq channel :x) :x2 :y2)))
             (keys (delq nil (list (easel-encode-field d)
                                   (and partner (easel-encode-field partner))
                                   (and (plist-get d :stack-start) (easel-key (plist-get d :stack-start)))
                                   (and (plist-get d :bin-end) (easel-key (plist-get d :bin-end)))))))
        (if (plist-member d :datum)
            (push (plist-get d :datum) out)
          (seq-doseq (row (plist-get u :rows))
            (dolist (k keys)
              (let ((v (plist-get row k)))
                (unless (memq v '(nil :null)) (push v out))))))))
    (nreverse out)))

(defun easel-compile--scale-type (pairs channel)
  "Default scale type for positional CHANNEL from PAIRS."
  (let* ((def (cdar pairs)) (explicit (plist-get (plist-get def :scale) :type)))
    (cond
     (explicit (if (equal explicit "utc") "time" explicit))
     ((plist-get def :bin-end) "linear")
     ((equal (plist-get def :type) "quantitative") "linear")
     ((equal (plist-get def :type) "temporal") "time")
     ((and (memq channel '(:x :y))
           (seq-some (lambda (p) (member (plist-get (plist-get (car p) :mark) :type) '("bar" "rect" "tick")))
                     pairs))
      "band")
     ((memq channel '(:x :y)) "point")
     (t "ordinal"))))

(defun easel-compile--less (a b)
  "Ascending order for mixed domain values A and B."
  (if (and (numberp a) (numberp b)) (< a b) (string< (format "%s" a) (format "%s" b))))

(defun easel-compile--discrete-domain (pairs values)
  "Discrete domain from VALUES, ordered per the first def in PAIRS."
  (let* ((def (cdar pairs)) (sort (plist-get def :sort))
         (unique (delete-dups (copy-sequence values))))
    (vconcat
     (cond
      ((eq sort :null) unique)
      ((vectorp sort) (append (seq-filter (lambda (v) (member v unique)) sort)
                              (seq-remove (lambda (v) (seq-contains-p sort v)) unique)))
      ((equal sort "descending") (reverse (sort unique #'easel-compile--less)))
      ((and (stringp sort) (string-match "\\`\\(-?\\)\\([xy]\\)\\'" sort))
       (let* ((desc (equal (match-string 1 sort) "-"))
              (other (easel-key (match-string 2 sort)))
              (u (caar pairs)) (odef (plist-get (plist-get u :encoding) other))
              (sums (make-hash-table :test 'equal)))
         (seq-doseq (row (plist-get u :rows))
           (let ((k (easel-encode-raw def row)) (v (easel-encode-raw odef row)))
             (when (numberp v) (puthash k (+ v (gethash k sums 0)) sums))))
         (let ((sorted (sort unique (lambda (a b) (< (gethash a sums 0) (gethash b sums 0))))))
           (if desc (nreverse sorted) sorted))))
      (t (sort unique #'easel-compile--less))))))

(defun easel-compile--continuous (type pairs channel values zoom)
  "Continuous scale of TYPE for CHANNEL over VALUES (ZOOM overrides domain)."
  (let* ((def (cdar pairs)) (sp (plist-get def :scale))
         (explicit (and (vectorp (plist-get sp :domain)) (plist-get sp :domain)))
         (nums (if (equal type "time") (delq nil (mapcar #'easel-time-parse values))
                 (seq-filter #'numberp values)))
         (positional (memq channel '(:x :y)))
         (binned (plist-get def :bin-end))
         (lo (cond (zoom (aref zoom 0)) (explicit (easel-compile--num type (aref explicit 0)))
                   ((plist-get sp :domainMin) (easel-compile--num type (plist-get sp :domainMin)))
                   (nums (apply #'min nums)) (t 0)))
         (hi (cond (zoom (aref zoom 1)) (explicit (easel-compile--num type (aref explicit 1)))
                   ((plist-get sp :domainMax) (easel-compile--num type (plist-get sp :domainMax)))
                   (nums (apply #'max nums)) (t 1)))
         (custom (or zoom explicit))
         (zero (if (plist-member sp :zero) (eq (plist-get sp :zero) t)
                 (and (equal type "linear") (not binned) (not custom)
                      (or (eq channel :size)
                          (and positional (not (easel-compile--dimension-p pairs channel)))))))
         (nice (if (plist-member sp :nice) (eq (plist-get sp :nice) t)
                 (and (member type '("linear" "log")) positional (not binned) (not custom)))))
    (append (easel-scale-continuous type lo hi [0 1] :zero zero :nice nice
                                    :field (plist-get def :field)
                                    :reverse (if (eq (plist-get sp :reverse) t) t :false))
            ;; Vega-Lite pads a bar's continuous dimension by continuousBandSize.
            (let ((pad (or (plist-get sp :padding)
                           (and (not custom) (not binned) (not (plist-get def :derived))
                                (easel-compile--dimension-p
                                 (seq-filter (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "bar")) pairs)
                                 channel)
                                5))))
              (when pad (list :padding pad))))))

(defun easel-compile--dimension-p (pairs channel)
  "Non-nil when CHANNEL is the dimension (not the measure) of a bar, area
or line in PAIRS; Vega-Lite does not extend dimension scales to zero."
  (seq-some (lambda (p)
              (let* ((u (car p)) (enc (plist-get u :encoding))
                     (type (plist-get (plist-get u :mark) :type))
                     (x (plist-get enc :x)) (y (plist-get enc :y))
                     (horizontal (and x y (easel-encode-discrete-p y) (not (easel-encode-discrete-p x)))))
                (and (member type '("bar" "area" "line"))
                     (eq channel (if horizontal :y :x)))))
            pairs))

(defun easel-compile--num (type v)
  "Domain bound V as a number for scale TYPE."
  (if (equal type "time") (easel-time-parse v) v))

(defun easel-compile-position-scale (units channel zoom)
  "Scale for positional CHANNEL shared by UNITS, or nil.
ZOOM is a [LO HI] domain from view state, or nil."
  (when-let* ((pairs (easel-compile--defs units channel)))
    (let* ((type (easel-compile--scale-type pairs channel))
           (values (easel-compile--values pairs channel))
           (sp (plist-get (cdar pairs) :scale)))
      (if (member type '("band" "point"))
          (let* ((rect (seq-every-p (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "rect")) pairs))
                 (inner (or (plist-get sp :paddingInner) (plist-get sp :padding) (if rect 0 nil)))
                 (outer (or (plist-get sp :paddingOuter) (plist-get sp :padding) (if rect 0 nil))))
            (append (easel-scale-band type (easel-compile--discrete-domain pairs values) [0 1] inner outer)
                    (list :field (plist-get (cdar pairs) :field) :padding-inner inner :padding-outer outer)))
        (easel-compile--continuous type pairs channel values zoom)))))

(defun easel-compile-color-scale (units)
  "Return (CHANNEL DEF SCALE) for the first field-mapped color channel."
  (cl-loop for channel in '(:color :fill :stroke)
           for pairs = (seq-filter (lambda (p) (plist-get (cdr p) :field))
                                   (easel-compile--defs units channel))
           when pairs
           return (let* ((def (cdar pairs)) (sp (plist-get def :scale))
                         (values (easel-compile--values pairs channel)))
                    (list channel def
                          (if (easel-encode-discrete-p def)
                              (append (easel-scale-ordinal (easel-compile--discrete-domain pairs values)
                                                           (or (and (vectorp (plist-get sp :range)) (plist-get sp :range))
                                                               easel-scale-tableau10))
                                      (list :field (plist-get def :field)))
                            (let ((nums (if (equal (plist-get def :type) "temporal")
                                            (delq nil (mapcar #'easel-time-parse values))
                                          (seq-filter #'numberp values))))
                              (list :type "sequential"
                                    :domain (vector (if nums (apply #'min nums) 0) (if nums (apply #'max nums) 1))
                                    :range (or (and (vectorp (plist-get sp :range)) (plist-get sp :range))
                                               ;; Vega-Lite: config.range.heatmap for rect, ramp otherwise.
                                               (if (seq-some (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "rect"))
                                                             pairs)
                                                   easel-scale-yellowgreenblue
                                                 easel-scale-blues))
                                    :field (plist-get def :field))))))))

(defun easel-compile-aux-scale (units channel range)
  "Linear scale for CHANNEL (size or opacity) onto RANGE, or nil."
  (when-let* ((pairs (seq-filter (lambda (p) (plist-get (cdr p) :field)) (easel-compile--defs units channel))))
    (let ((nums (seq-filter #'numberp (easel-compile--values pairs channel))))
      (easel-scale-continuous "linear" (if (eq channel :size) 0 (if nums (apply #'min nums) 0))
                              (if nums (apply #'max nums) 1) range :field (plist-get (cdar pairs) :field)))))

(defun easel-compile-set-range (scale range)
  "Return SCALE mapped onto RANGE (recomputing band geometry).
A continuous :padding P (pixels) widens the domain about its centre so
the data spans the range less P on each side, like Vega's padDomain."
  (let ((range (if (eq (plist-get scale :reverse) t) (vector (aref range 1) (aref range 0)) range)))
    (if (member (plist-get scale :type) '("band" "point"))
        (append (easel-scale-band (plist-get scale :type) (plist-get scale :domain) range
                                  (plist-get scale :padding-inner) (plist-get scale :padding-outer))
                (list :field (plist-get scale :field)))
      (let ((out (plist-put (copy-sequence scale) :range range))
            (pad (plist-get scale :padding))
            (span (abs (- (aref range 1) (aref range 0)))))
        (when (and pad (> span (* 2 pad)) (member (plist-get scale :type) '("linear" "time" "utc")))
          (let* ((d (plist-get scale :domain)) (c (/ (+ (aref d 0) (aref d 1)) 2.0))
                 (frac (/ span (- span (* 2.0 pad)))))
            (setq out (plist-put out :domain (vector (+ c (* frac (- (aref d 0) c))) (+ c (* frac (- (aref d 1) c))))))
            (setq out (plist-put out :padding nil))))
        out))))

(provide 'easel-compile-scales)
;;; easel-compile-scales.el ends here
