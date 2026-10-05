;;; easel-marks-bounds.el --- mark defaults, mark bounds and legend looks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, around easel-marks.el.  `easel-marks-resolve-mark' gives
;; a mark definition its config defaults the way Vega-Lite does
;; (config.mark, then config.<type>, then the spec).  `easel-marks-bounds'
;; bounds a unit's items as vega-scenegraph does (symbols by
;; sqrt(size)/2, strokes widen bounds by their full width) so compile
;; can grow the canvas for marks overhanging the plot.  Series marks
;; with a grouping channel are drawn by Vega inside facet ("scope")
;; groups, which also push legends right: `easel-marks-scope-p'.

;;; Code:

(require 'easel-core)
(require 'easel-theme)
(require 'easel-layout)

(defun easel-marks-resolve-mark (mark config)
  "MARK (a string or definition) with CONFIG's mark defaults filled in."
  (let* ((def (if (stringp mark) (list :type mark) mark))
         (type (easel-key (plist-get def :type))))
    (easel-theme-merge (easel--plist-without (easel-theme-get config :mark) :type)
                       (easel-theme-get config type)
                       def)))

(defun easel-marks-legend-style (unit _metrics)
  "UNIT's constant look that legend symbols copy.
A plist (:fill :stroke :stroke-width :opacity :stroked)."
  (let* ((mark (plist-get unit :mark)) (type (plist-get mark :type))
         (stroked (or (member type '("line" "rule"))
                      (and (equal type "point") (not (eq (plist-get mark :filled) t)))))
         (enc (plist-get unit :encoding))
         (value (lambda (ch) (let ((d (plist-get enc ch))) (and (easel-object-p d) (plist-get d :value))))))
    (list :fill (or (funcall value :fill) (plist-get mark :fill) (funcall value :color) (plist-get mark :color))
          :stroke (or (funcall value :stroke) (let ((s (plist-get mark :stroke))) (and (stringp s) s)))
          :stroke-width (plist-get mark :strokeWidth)
          :opacity (or (funcall value :opacity) (plist-get mark :opacity)
                       (and (member type '("point" "circle" "square" "tick")) (not (plist-get unit :aggregated)) 0.7))
          :stroked stroked)))

(defun easel-marks-scope-p (unit)
  "Non-nil when UNIT is a line or area split into series by a field."
  (and (member (plist-get (plist-get unit :mark) :type) '("line" "area"))
       (seq-some (lambda (ch) (let ((d (plist-get (plist-get unit :encoding) ch)))
                                (and (easel-object-p d) (plist-get d :field))))
                 '(:color :fill :stroke :detail))))

(defun easel-marks--grow (box item)
  "BOX widened by ITEM's stroke width when it is stroked."
  (let ((stroke (plist-get item :stroke)))
    (if (and box (stringp stroke) (not (equal stroke "none")))
        (let ((sw (or (plist-get item :strokeWidth) 1)))
          (vector (- (aref box 0) sw) (- (aref box 1) sw) (+ (aref box 2) sw) (+ (aref box 3) sw)))
      box)))

(defun easel-marks--points-box (points)
  "Bounds of POINTS, a vector of [X Y]."
  (when (> (length points) 0)
    (let ((xs (mapcar (lambda (p) (aref p 0)) points)) (ys (mapcar (lambda (p) (aref p 1)) points)))
      (vector (apply #'min xs) (apply #'min ys) (apply #'max xs) (apply #'max ys)))))

(defun easel-marks-item-bounds (type item metrics)
  "Vega's bounds of ITEM of mark TYPE, or nil.
Transparent items count too, so hover and selection never move layout."
  (pcase type
      ((or "bar" "rect")
       (easel-marks--grow (vector (plist-get item :x) (plist-get item :y)
                                  (+ (plist-get item :x) (plist-get item :w)) (+ (plist-get item :y) (plist-get item :h)))
                          item))
      ((or "rule" "tick")
       (easel-marks--grow (vector (min (plist-get item :x1) (plist-get item :x2)) (min (plist-get item :y1) (plist-get item :y2))
                                  (max (plist-get item :x1) (plist-get item :x2)) (max (plist-get item :y1) (plist-get item :y2)))
                          item))
      ("text" (easel-layout-text-bounds metrics (plist-get item :text) (plist-get item :fontSize)
                                        (plist-get item :x) (plist-get item :y) (plist-get item :align)
                                        (plist-get item :baseline)))
      ("line" (easel-marks--grow (easel-marks--points-box (plist-get item :points)) item))
      ("area" (easel-layout-union (easel-marks--points-box (plist-get item :points))
                                  (easel-marks--points-box (plist-get item :base))))
      (_ (let ((r (/ (sqrt (or (plist-get item :size) 0)) 2.0)) (x (plist-get item :x)) (y (plist-get item :y)))
           (easel-marks--grow (vector (- x r) (- y r) (+ x r) (+ y r)) item)))))

(defun easel-marks-bounds (unit metrics)
  "Union of the bounds of UNIT's items, or nil."
  (let ((type (plist-get (plist-get unit :mark) :type)) box)
    (seq-doseq (item (plist-get unit :items))
      (setq box (easel-layout-union box (easel-marks-item-bounds type item metrics))))
    box))

(defun easel-marks--shift-points (points dx dy)
  "POINTS (a vector of [X Y]) moved by DX DY."
  (vconcat (mapcar (lambda (p) (vector (+ (aref p 0) dx) (+ (aref p 1) dy))) points)))

(defun easel-marks-translate (items dx dy)
  "ITEMS (a vector) moved by DX DY; ITEMS itself when both are zero."
  (if (and (zerop dx) (zerop dy)) items
    (vconcat
     (mapcar (lambda (item)
               (let ((out (copy-sequence item)))
                 (dolist (k '(:x :x1 :x2)) (when (numberp (plist-get out k)) (setq out (plist-put out k (+ (plist-get out k) dx)))))
                 (dolist (k '(:y :y1 :y2)) (when (numberp (plist-get out k)) (setq out (plist-put out k (+ (plist-get out k) dy)))))
                 (dolist (k '(:points :base :anchors))
                   (when (vectorp (plist-get out k))
                     (setq out (plist-put out k (easel-marks--shift-points (plist-get out k) dx dy)))))
                 out))
             items))))

(provide 'easel-marks-bounds)
;;; easel-marks-bounds.el ends here
