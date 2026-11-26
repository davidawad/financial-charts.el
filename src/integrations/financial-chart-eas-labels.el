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
;; In layer order, a label whose box meets one already placed is moved
;; up or down by whole label heights (up first, at most
;; `financial-chart-label-max-steps' each way, never out of the pane)
;; to the first free place;
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

(defun financial-chart-labels-declutter (layers scale)
  "LAYERS with their text labels moved off each other (see the commentary).
SCALE is (:x0 :x1 :y0 :y1 :width :height) of the pane."
  (let ((placed nil)
        (h financial-chart-label-height)
        (price-per-px (/ (- (plist-get scale :y1) (plist-get scale :y0)) (float (plist-get scale :height)))))
    (mapcar
     (lambda (layer)
       (if (not (financial-chart-labels--text-layer-p layer)) layer
         (let* ((mark (plist-get layer :mark))
                (rows (mapcar
                       (lambda (row)
                         (if (not (and (numberp (plist-get row :time)) (numberp (plist-get row :y)))) row
                           (let* ((box (financial-chart-labels--box row mark scale))
                                  (dy (cl-loop for k in (cons 0 (cl-loop for s from 1 to financial-chart-label-max-steps
                                                                         append (list (- (* s h)) (* s h))))
                                               for moved = (financial-chart-labels--shift box k)
                                               ;; Stay inside the pane: a label never widens its scale.
                                               when (and (>= (nth 1 moved) 0) (<= (nth 3 moved) (plist-get scale :height))
                                                         (not (cl-some (lambda (p) (financial-chart-labels--overlap-p moved p))
                                                                       placed)))
                                               return k)))
                             (push (financial-chart-labels--shift box (or dy 0)) placed)
                             (if (or (null dy) (zerop dy)) row
                               (plist-put (copy-sequence row) :y (- (plist-get row :y) (* dy price-per-px)))))))
                       (append (plist-get (plist-get layer :data) :values) nil))))
           (plist-put (copy-sequence layer) :data
                      (plist-put (copy-sequence (plist-get layer :data)) :values (vconcat rows))))))
     layers)))

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
