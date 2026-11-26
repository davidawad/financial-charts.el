;;; financial-chart-eas-labels.el --- keep annotation labels from sitting on each other -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One placement pass over every text label of a pane (fc-gty.20).  Each
;; label row of a text layer gets a box in estimated pixels: its x and y
;; scaled into the pane (the pane's width and height, its x extent and
;; its y range), widened by its text at `financial-chart-label-char-width'
;; per character and moved by the layer's align, baseline, dx and dy.
;; In layer order each label tries its place and whole label heights
;; up or down (at most `financial-chart-label-max-steps', 2, each way,
;; never out of the pane) and takes the least bad: overlapping a label
;; already placed is worst, then a candle body, then a series line, then
;; the distance moved (fc-gty.22).  A moved label gets a thin leader line
;; back to where it was, so it still points at its marker.  A pinned
;; layer (`financial-chart-labels-pin': every hand-placed annotation's
;; label, and level, Fibonacci and S/R labels, which belong on their
;; line) keeps its labels and the others go around them;
;; its "y" is changed by the same amount in price units.  A label with no
;; free place stays where it was.  The pass knows nothing of which
;; annotation drew a label: levels, patterns, Fibonacci and text all go
;; through it alike, and the first drawn keeps its place.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar financial-chart-label-char-width 7
  "Estimated pixels per label character.")

(defvar financial-chart-label-height 12
  "Estimated pixel height of a label.")

(defvar financial-chart-label-max-steps 2
  "Label heights a label may move up or down to find a free place.")

(defvar financial-chart-label-assumed-width 800
  "Pane width in pixels assumed when the chart's width is \"container\".")

(defun financial-chart-labels--text-layer-p (layer)
  "Non-nil when LAYER is a text layer of inline rows on time and y."
  (let ((mark (plist-get layer :mark)) (enc (plist-get layer :encoding)))
    (and (equal (plist-get mark :type) "text")
         (vectorp (plist-get (plist-get layer :data) :values))
         (equal (plist-get (plist-get enc :y) :field) "y")
         (plist-get (plist-get enc :x) :field))))

(defun financial-chart-labels--box (row mark scale)
  "ROW's label box (X0 Y0 X1 Y1) in pixels for text MARK under SCALE.
SCALE is (:x0 :x1 :y0 :y1 :width :height), the pane's data extents and
size."
  (let* ((text (format "%s" (or (plist-get row :label) "")))
         (w (* financial-chart-label-char-width (length text)))
         (h financial-chart-label-height)
         (xspan (max 1e-9 (float (- (plist-get scale :x1) (plist-get scale :x0)))))
         (yspan (max 1e-9 (float (- (plist-get scale :y1) (plist-get scale :y0)))))
         (px (+ (* (/ (- (plist-get row :time) (plist-get scale :x0)) xspan) (plist-get scale :width))
                (or (plist-get mark :dx) 0)))
         (py (+ (* (/ (- (plist-get scale :y1) (plist-get row :y)) yspan) (plist-get scale :height))
                (or (plist-get mark :dy) 0)))
         (x0 (pcase (plist-get mark :align) ("right" (- px w)) ((or "center" 'nil) (- px (/ w 2.0))) (_ px)))
         (y0 (pcase (plist-get mark :baseline) ("bottom" (- py h)) ("top" py) (_ (- py (/ h 2.0))))))
    (list x0 y0 (+ x0 w) (+ y0 h))))

(defun financial-chart-labels--overlap-p (a b)
  "Non-nil when boxes A and B overlap."
  (and (< (nth 0 a) (nth 2 b)) (< (nth 0 b) (nth 2 a))
       (< (nth 1 a) (nth 3 b)) (< (nth 1 b) (nth 3 a))))

(defun financial-chart-labels--shift (box dy)
  "BOX moved DY pixels down."
  (list (nth 0 box) (+ (nth 1 box) dy) (nth 2 box) (+ (nth 3 box) dy)))

(defun financial-chart-labels--segment-hits-box-p (x1 y1 x2 y2 box)
  "Non-nil when the segment (X1, Y1)-(X2, Y2) crosses BOX (X0 Y0 X1 Y1).
Liang-Barsky clipping."
  (let ((t0 0.0) (t1 1.0) (dx (- x2 x1)) (dy (- y2 y1)) (inside t))
    (cl-loop for (p q) in (list (list (- dx) (- x1 (nth 0 box))) (list dx (- (nth 2 box) x1))
                                (list (- dy) (- y1 (nth 1 box))) (list dy (- (nth 3 box) y1)))
             while inside
             do (cond ((zerop p) (when (< q 0) (setq inside nil)))
                      (t (let ((r (/ q (float p))))
                           (if (< p 0) (when (> r t0) (setq t0 r)) (when (< r t1) (setq t1 r)))
                           (when (> t0 t1) (setq inside nil))))))
    inside))

(defun financial-chart-labels--px (scale x y)
  "Data point (X, Y) in pane pixels under SCALE, as (PX . PY)."
  (cons (* (/ (- x (plist-get scale :x0)) (max 1e-9 (float (- (plist-get scale :x1) (plist-get scale :x0)))))
           (plist-get scale :width))
        (* (/ (- (plist-get scale :y1) y) (max 1e-9 (float (- (plist-get scale :y1) (plist-get scale :y0)))))
           (plist-get scale :height))))

(defun financial-chart-labels-obstacles (scale xs bars series)
  "Obstacles of a pane for `financial-chart-labels-declutter' under SCALE.
XS are the bars' x positions; BARS (bar plists, or nil off the price
pane) give candle bodies as boxes; SERIES (vectors of one value or nil
per bar) give their lines as segments.  Return (:boxes :segments) in
pixels."
  (let* ((xs (vconcat xs)) (n (length xs))
         (half (* 0.35 (/ (plist-get scale :width) (max 1.0 (float n))))))
    (list
     :boxes (cl-loop for b in (append bars nil) for i from 0 below n
                     for o = (plist-get b :open) for c = (plist-get b :close)
                     when (and (numberp o) (numberp c))
                     collect (let ((top (financial-chart-labels--px scale (aref xs i) (max o c)))
                                   (bot (financial-chart-labels--px scale (aref xs i) (min o c))))
                               (list (- (car top) half) (cdr top) (+ (car top) half) (max (cdr bot) (+ (cdr top) 1)))))
     :segments (cl-loop for values in series
                        append (cl-loop for i from 1 below (min n (length values))
                                        for a = (elt values (1- i)) for b = (elt values i)
                                        when (and (numberp a) (numberp b))
                                        collect (let ((p (financial-chart-labels--px scale (aref xs (1- i)) a))
                                                      (q (financial-chart-labels--px scale (aref xs i) b)))
                                                  (list (car p) (cdr p) (car q) (cdr q))))))))

(defun financial-chart-labels--cost (box placed obstacles k)
  "How bad BOX is: other labels in PLACED, then candle bodies and series
lines of OBSTACLES, then the distance K it moved."
  (+ (* 100 (cl-count-if (lambda (p) (financial-chart-labels--overlap-p box p)) placed))
     (* 10 (cl-count-if (lambda (o) (financial-chart-labels--overlap-p box o)) (plist-get obstacles :boxes)))
     (* 3 (cl-count-if (lambda (s) (financial-chart-labels--segment-hits-box-p
                                    (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s) box))
                       (plist-get obstacles :segments)))
     ;; A step costs less than a candle body, more than one line.
     (* 4 (/ (abs k) (float financial-chart-label-height)))))

(defun financial-chart-labels--leaders (layer rows)
  "A thin rule layer joining each moved label of LAYER to where it was.
ROWS are (:time :y ANCHOR :y2 NEW); nil without any."
  (when rows
    (let ((enc (plist-get layer :encoding)))
      (list :name (concat (or (plist-get layer :name) "labels") "-leaders")
            :data (list :values (vconcat rows))
            :mark (list :type "rule" :color (or (plist-get (plist-get layer :mark) :color) "gray")
                        :strokeWidth 0.6 :opacity 0.6)
            :encoding (list :x (plist-get enc :x) :y (plist-get enc :y) :y2 '(:field "y2"))))))

(defun financial-chart-labels-pin (layer)
  "LAYER, a text layer, with its labels pinned to their line: the
placement pass keeps each on its y (a level's or a Fibonacci ratio's
label belongs on its line) but may slide it along the line, by whole
label widths, off another label (fc-gty.24, .25).  The others are placed
around them.  A layer whose :label-pin is t (a hand-placed annotation's
label) never moves at all."
  (plist-put (copy-sequence layer) :label-pin 'slide))

(defvar financial-chart-label-max-slides 4
  "Label widths a pinned label may slide along its line, each way.")

(defun financial-chart-labels--shift-x (box dx)
  "BOX moved DX pixels right."
  (list (+ (nth 0 box) dx) (nth 1 box) (+ (nth 2 box) dx) (nth 3 box)))

(defun financial-chart-labels--unpin (layer)
  "LAYER without the :label-pin key (not part of the spec)."
  (if (plist-member layer :label-pin)
      (cl-loop for (k v) on layer by #'cddr unless (eq k :label-pin) append (list k v))
    layer))

(defun financial-chart-labels-declutter (layers scale &optional obstacles)
  "LAYERS with their text labels placed off each other and off OBSTACLES.
SCALE is (:x0 :x1 :y0 :y1 :width :height) of the pane; OBSTACLES is
`financial-chart-labels-obstacles' (candle bodies, series lines).  Each
label tries its place and up to `financial-chart-label-max-steps' label
heights up or down inside the pane, and takes the least bad (another
label worst, then a candle body, then a line, then distance); a label
that moved gets a leader line back to its place."
  (let ((placed nil)
        (h financial-chart-label-height)
        (price-per-px (/ (- (plist-get scale :y1) (plist-get scale :y0)) (float (plist-get scale :height)))))
    (cl-loop
     for layer in layers
     append
     (cond
      ((not (financial-chart-labels--text-layer-p layer)) (list (financial-chart-labels--unpin layer)))
      ((eq (plist-get layer :label-pin) t)
       (cl-loop for row across (plist-get (plist-get layer :data) :values)
                when (and (numberp (plist-get row :time)) (numberp (plist-get row :y)))
                do (push (financial-chart-labels--box row (plist-get layer :mark) scale) placed))
       (list (financial-chart-labels--unpin layer)))
      ((plist-get layer :label-pin)
       ;; Slide along the line: same y, whole label widths left or right,
       ;; inside the pane, to the least bad place among the labels.
       (let* ((mark (plist-get layer :mark))
              (x-per-px (/ (- (plist-get scale :x1) (plist-get scale :x0)) (float (plist-get scale :width))))
              (rows (mapcar
                     (lambda (row)
                       (if (not (and (numberp (plist-get row :time)) (numberp (plist-get row :y)))) row
                         (let* ((box (financial-chart-labels--box row mark scale))
                                (w (+ 6 (- (nth 2 box) (nth 0 box))))
                                (ds (cons 0 (cl-loop for k from 1 to financial-chart-label-max-slides
                                                     append (list (- (* k w)) (* k w)))))
                                (inside (seq-filter (lambda (d) (let ((m (financial-chart-labels--shift-x box d)))
                                                                  (and (>= (nth 0 m) 0)
                                                                       (<= (nth 2 m) (plist-get scale :width)))))
                                                    ds))
                                (cost (lambda (d) (financial-chart-labels--cost
                                                   (financial-chart-labels--shift-x box d) placed obstacles
                                                   ;; A slide costs a quarter of a step: labels stay on their line.
                                                   (* 0.25 financial-chart-label-height (/ (abs d) (float w))))))
                                (d (or (car (sort (copy-sequence (or inside '(0)))
                                                  (lambda (a b) (< (funcall cost a) (funcall cost b)))))
                                       0)))
                           (push (financial-chart-labels--shift-x box d) placed)
                           (if (zerop d) row
                             (plist-put (copy-sequence row) :time (+ (plist-get row :time) (* d x-per-px)))))))
                     (append (plist-get (plist-get layer :data) :values) nil))))
         (list (financial-chart-labels--unpin
                (plist-put (copy-sequence layer) :data
                           (plist-put (copy-sequence (plist-get layer :data)) :values (vconcat rows)))))))
      (t
       (let* ((mark (plist-get layer :mark)) leaders
              (rows (mapcar
                     (lambda (row)
                       (if (not (and (numberp (plist-get row :time)) (numberp (plist-get row :y)))) row
                         (let* ((box (financial-chart-labels--box row mark scale))
                                (ks (cons 0 (cl-loop for s from 1 to financial-chart-label-max-steps
                                                     append (list (- (* s h)) (* s h)))))
                                (inside (seq-filter (lambda (k) (let ((m (financial-chart-labels--shift box k)))
                                                                  (and (>= (nth 1 m) 0)
                                                                       (<= (nth 3 m) (plist-get scale :height)))))
                                                    ks))
                                (k (or (car (sort (copy-sequence (or inside '(0)))
                                                  (lambda (a b)
                                                    (< (financial-chart-labels--cost
                                                        (financial-chart-labels--shift box a) placed obstacles a)
                                                       (financial-chart-labels--cost
                                                        (financial-chart-labels--shift box b) placed obstacles b)))))
                                       0)))
                           (push (financial-chart-labels--shift box k) placed)
                           (if (zerop k) row
                             (let ((y (- (plist-get row :y) (* k price-per-px))))
                               (push (list :time (plist-get row :time) :y (plist-get row :y) :y2 y) leaders)
                               (plist-put (copy-sequence row) :y y))))))
                     (append (plist-get (plist-get layer :data) :values) nil))))
         (delq nil (list (plist-put (copy-sequence layer) :data
                                    (plist-put (copy-sequence (plist-get layer :data)) :values (vconcat rows)))
                         (financial-chart-labels--leaders layer (nreverse leaders))))))))))

(defun financial-chart-labels-scale (layers xs width height y-range)
  "The SCALE of `financial-chart-labels-declutter' for a pane.
XS are its x positions, WIDTH its width (a number, else
`financial-chart-label-assumed-width'), HEIGHT its height, Y-RANGE
\(LOW . HIGH) its data's range or nil; the label rows of LAYERS widen it."
  (let* ((ys (cl-loop for l in layers when (financial-chart-labels--text-layer-p l)
                      append (cl-loop for r across (plist-get (plist-get l :data) :values)
                                      when (numberp (plist-get r :y)) collect (plist-get r :y))))
         (lo (apply #'min (append (and y-range (list (car y-range))) ys (list most-positive-fixnum))))
         (hi (apply #'max (append (and y-range (list (cdr y-range))) ys (list most-negative-fixnum))))
         (hi (if (= hi lo) (1+ lo) hi))
         ;; The axis rounds its domain out to about ten nice steps.
         (step (let* ((raw (/ (- hi lo) 10.0)) (mag (expt 10.0 (floor (log raw 10)))))
                 (* mag (cl-find-if (lambda (m) (>= (* m mag) raw)) '(1 2 5 10))))))
    (list :x0 (seq-min xs) :x1 (seq-max xs)
          :y0 (* step (ffloor (/ lo step))) :y1 (* step (fceiling (/ hi step)))
          :width (if (numberp width) width financial-chart-label-assumed-width)
          :height (or height 200))))

(provide 'financial-chart-eas-labels)
;;; financial-chart-eas-labels.el ends here
