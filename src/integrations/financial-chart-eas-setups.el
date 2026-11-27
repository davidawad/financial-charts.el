;;; financial-chart-eas-setups.el --- annotations of the named setups: ma-cross, volume-profile -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two more annotation types for composed charts (fc-nyt), views of
;; detectors like those of `financial-chart-eas-markups':
;;
;;   ma-cross        cross.ma: a triangle under the bar where the "fast"
;;                   (50) average crosses above the "slow" (200) one
;;                   (the golden cross) and over the bar where it
;;                   crosses below (the death cross); "average": sma|ema,
;;                   "labels": true|false, "since": the last N bars
;;   volume-profile  profile.volume as of the last bar: the value area
;;                   shaded, the point of control (POC) and the value
;;                   area's high and low (VAH, VAL) as labelled levels
;;                   and, unless "histogram": false, the volume by price
;;                   as bars from the "side" (right|left) edge, the
;;                   longest "width" (0.2) of the window; "window",
;;                   "bins" (24) and "value_area" (0.7) as the detector.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-detectors-setups)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-styles)
(require 'financial-chart-eas-annotations)
(require 'financial-chart-eas-markups)
(require 'financial-chart-eas-labels)

(defun financial-chart-setups-annotation--ma-cross (ctx a name path)
  "Layers of the ma-cross annotation A named NAME at PATH in CTX."
  (let* ((fast (financial-chart-markup--int a :fast path 50 2))
         (slow (financial-chart-markup--int a :slow path 200 2))
         (since (financial-chart-markup--int a :since path nil))
         (labels (not (eq (financial-chart-series-get a :labels) :false)))
         (canonical (and (= fast 50) (= slow 200)
                         (equal (financial-chart-markup--choice a :average path '("sma" "ema")) "sma")))
         (bars (vconcat (plist-get ctx :bars))) (n (length bars))
         (found (seq-filter (lambda (e) (or (null since) (>= (plist-get e :index) (- n since))))
                            (financial-chart-markup--events ctx a path "cross.ma" '(:fast :slow :average))))
         (range (financial-chart-annotation--range ctx))
         (pad (* 0.04 (- (cdr range) (car range)))))
    (cl-loop for (direction shape key golden plain) in '((bullish "triangle-up" :up "golden cross" "cross up")
                                                         (bearish "triangle-down" :down "death cross" "cross down"))
             for group = (mapcar
                          (lambda (e)
                            (let* ((i (plist-get e :index)) (bar (aref bars i)) (below (eq direction 'bullish)))
                              (list :time (financial-chart-markup--bar-x ctx i)
                                    :y (if below (- (plist-get bar :low) pad) (+ (plist-get bar :high) pad))
                                    :id (plist-get e :id) :strength (plist-get e :strength)
                                    :label (if canonical golden plain))))
                          (seq-filter (lambda (e) (equal (plist-get e :direction) (symbol-name direction))) found))
             for colour = (or (financial-chart-series-get a :color) (plist-get ctx key))
             for layer = (format "%s-%s" name direction)
             when group
             append (append
                     (list (list :name layer :data (list :values (vconcat group))
                                 :mark (list :type "point" :shape shape :filled t :size 70 :color colour :opacity 1)
                                 :encoding (list :x (financial-chart-annotation--x-enc ctx)
                                                 :y (financial-chart-styles-y "y")
                                                 :tooltip [(:field "label" :type "nominal")
                                                           (:field "strength" :type "quantitative" :format ".2f")
                                                           (:field "id" :type "nominal")])))
                     (when labels
                       (financial-chart-markup--labels
                        ctx (concat layer "-labels") group colour
                        :baseline (if (eq direction 'bullish) "top" "bottom")
                        :dy (if (eq direction 'bullish) 9 -9)))))))

(defun financial-chart-setups-annotation--volume-profile (ctx a name path)
  "Layers of the volume-profile annotation A named NAME at PATH in CTX."
  (unless (plist-get ctx :volume)
    (financial-chart-series-fail path "NO_VOLUME" "volume-profile needs bars with \"volume\""))
  (let* ((window (financial-chart-markup--int a :window path nil 2))
         (bins (financial-chart-markup--int a :bins path 24 4))
         (fraction (financial-chart-markup--positive a :value_area path 0.7))
         (width (financial-chart-markup--positive a :width path 0.2))
         (side (financial-chart-markup--choice a :side path '("right" "left")))
         (bars (vconcat (plist-get ctx :bars))) (n (length bars))
         (start (if window (max 0 (- n window)) 0))
         (_ (when (< (- n start) 2)
              (financial-chart-series-fail path "INVALID_ANNOTATION" "volume-profile needs at least 2 bars")))
         (_ (financial-chart-markup--events ctx a path "profile.volume" '(:window :bins :value_area)))
         (profile (financial-chart-setups-volume-profile bars start (1- n) bins fraction))
         (colour (financial-chart-markup--colour a))
         (x0 (financial-chart-markup--bar-x ctx start))
         (end (financial-chart-markup--end ctx))
         (lo (plist-get profile :lo)) (hi (plist-get profile :hi))
         (vols (plist-get profile :bins)) (peak (cl-loop for v across vols maximize v))
         (step (/ (- hi lo) bins))
         (reach (max 1 (round (* width (- n start)))))
         (rows (cl-loop for k below bins
                        for len = (round (* reach (/ (aref vols k) (max peak 1e-12))))
                        when (> len 0)
                        collect (list :time (financial-chart-markup--bar-x
                                             ctx (if (equal side "right") (- n len) start))
                                      :time2 (financial-chart-markup--bar-x
                                              ctx (if (equal side "right") (1- n) (1- (+ start len))))
                                      :y (+ lo (* k step)) :y2 (+ lo (* (+ k 0.92) step)))))
         (poc (plist-get profile :poc)) (vah (plist-get profile :vah)) (val (plist-get profile :val)))
    (append
     (list (list :name (concat name "-area")
                 :data (list :values (vector (list :time x0 :time2 end :y val :y2 vah)))
                 :mark (list :type "rect" :color colour :opacity 0.07)
                 :encoding (financial-chart-annotation--encoding ctx '(:x "time" :x2 "time2" :y "y" :y2 "y2"))))
     (unless (eq (financial-chart-series-get a :histogram) :false)
       (list (list :name (concat name "-bins") :data (list :values (vconcat rows))
                   :mark (list :type "rect" :color colour :opacity 0.3)
                   :encoding (financial-chart-annotation--encoding ctx '(:x "time" :x2 "time2" :y "y" :y2 "y2")))))
     (financial-chart-setups-annotation--levels
      ctx name x0 side
      (list (list "POC" poc nil 1.5) (list "VAH" vah [4 2] 1) (list "VAL" val [4 2] 1))
      colour))))

(defun financial-chart-setups-annotation--levels (ctx name x0 side levels colour)
  "Rules NAME across CTX's bars at LEVELS, (LABEL PRICE DASH WIDTH) each.
Labels sit at the left (X0) edge unless SIDE is left, then at the right."
  (let* ((end (financial-chart-markup--end ctx)) (left (equal side "right")))
    (cl-loop for (label y dash width) in levels for i from 0
             for row = (list :time x0 :time2 end :y y :span0 x0 :span1 end :label (format "%s %.2f" label y))
             append (list (financial-chart-annotation--rule ctx (format "%s-level-%d" name i) (list row) colour dash width
                                                            :x "time" :x2 "time2" :y "y")
                          (financial-chart-labels-pin
                           (financial-chart-annotation--text
                            ctx (format "%s-level-%d-label" name i)
                            (list (plist-put (copy-sequence row) :time (if left x0 end)))
                            colour :align (if left "left" "right") :baseline "bottom" :dx (if left 3 0) :dy -1))))))

(financial-chart-annotation-register "ma-cross" #'financial-chart-setups-annotation--ma-cross)
(financial-chart-annotation-register "volume-profile" #'financial-chart-setups-annotation--volume-profile)

(add-to-list 'financial-chart-markup-types '("ma-cross" . financial-chart-setups-annotation--ma-cross) t)
(add-to-list 'financial-chart-markup-types '("volume-profile" . financial-chart-setups-annotation--volume-profile) t)
(add-to-list 'financial-chart-markup-detectors '("ma-cross" "cross.ma" (:fast :slow :average)) t)
(add-to-list 'financial-chart-markup-detectors '("volume-profile" "profile.volume" (:window :bins :value_area)) t)

(provide 'financial-chart-eas-setups)
;;; financial-chart-eas-setups.el ends here
