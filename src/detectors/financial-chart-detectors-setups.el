;;; financial-chart-detectors-setups.el --- moving-average crosses and the volume profile -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two detectors the named presets (fc-nyt) need:
;;
;;   cross.ma        the fast moving average crossing the slow one: the
;;                   golden cross (up) and death cross (down)
;;   profile.volume  the volume profile of the last WINDOW bars as of
;;                   each bar: point of control and value area

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-detector-api)
(require 'financial-chart-detectors-swings)

(defun financial-chart-setups--average (v kind period)
  "KIND (\"sma\" or \"ema\") of V's closes over PERIOD bars, nil in warm-up."
  (let ((n (length v)) (alpha (/ 2.0 (1+ period))) (prev nil) out)
    (dotimes (i n)
      (push
       (cond
        ((< i (1- period)) nil)
        ((equal kind "sma")
         (/ (cl-loop for j from (- i period -1) to i sum (financial-chart-swing--c v j)) (float period)))
        ((= i (1- period))
         (setq prev (/ (cl-loop for j to i sum (financial-chart-swing--c v j)) (float period))))
        (t (setq prev (+ (* alpha (financial-chart-swing--c v i)) (* (- 1 alpha) prev)))))
       out))
    (nreverse out)))

(defun financial-chart-setups--crosses (v p)
  "Raw cross.ma events of V with params P."
  (let* ((fast (plist-get p :fast)) (slow (plist-get p :slow)) (kind (plist-get p :average))
         (a (financial-chart-setups--average v kind fast))
         (b (financial-chart-setups--average v kind slow))
         (diff (cl-mapcar (lambda (x y) (and x y (- x y))) a b))
         events)
    (when (>= fast slow)
      (financial-chart-detection-fail "/params/fast" "INVALID_PARAM"
                                      "fast (%d) must be below slow (%d)" fast slow))
    (cl-loop for i from 1 below (length v)
             for d = (nth i diff) for prev = (nth (1- i) diff)
             when (and d prev (or (and (> d 0) (<= prev 0)) (and (< d 0) (>= prev 0))))
             do (let* ((up (> d 0))
                       (unit (financial-chart-swing--range v (max 0 (- i 9)) i))
                       (since (max 0 (- i 9)))
                       (slope (and (nth since b) (/ (- (nth i b) (nth since b)) unit))))
                  (push (list :index i :confirmed i :direction (if up 'bullish 'bearish)
                              :strength (financial-chart-swing--clamp (/ (abs d) unit))
                              :geometry (list :at i :y (nth i b))
                              :evidence (list :gap (/ d unit) :slow_slope (or slope 0.0)))
                        events)))
    (nreverse events)))

(financial-chart-register-detector
 'cross.ma #'financial-chart-setups--crosses
 :kind 'trend :direction 'both
 :params '((:name fast :type integer :default 50 :min 2 :doc "bars of the fast average")
           (:name slow :type integer :default 200 :min 3 :doc "bars of the slow average")
           (:name average :type choice :choices ("sma" "ema") :default "sma" :doc "kind of both averages"))
 :lag 0 :lookback 200 :needs '(high low close)
 :definition "Moving-average crossover (Murphy, J. 1999, Technical Analysis of the Financial Markets; the 50/200-day pair is the golden cross, its reverse the death cross): the bar whose fast average is above the slow one when the bar before had it at or below (bullish), or below when it was at or above (bearish).  Evidence: the gap between the averages in average ranges and the slow average's change over the last 10 bars in ranges.  Strength: the gap in ranges, clamped."
 :invariance '(scale shift)
 :variants '(((:fast 5 :slow 20) scale shift) ((:fast 10 :slow 30 :average "ema") scale shift))
 :mark '(:type "marker")
 :example "examples/indicators/ma-cross.json"
 :doc "the fast average crossing the slow one")

;;; Volume profile

(defun financial-chart-setups--profile-bins (v start end bins lo hi)
  "The volume of V's bars START..END in BINS equal price bins from LO to HI.
Each bar's volume is spread across the bins its high-low range covers,
in proportion to the overlap; a bar with no range goes in one bin."
  (let ((vols (make-vector bins 0.0)) (width (/ (- hi lo) bins)))
    (cl-loop for j from start to end
             for h = (financial-chart-swing--h v j) for l = (financial-chart-swing--l v j)
             for vol = (float (or (plist-get (aref v j) :volume) 0))
             do (if (<= (- h l) 0)
                    (cl-incf (aref vols (min (1- bins) (floor (/ (- l lo) width)))) vol)
                  (cl-loop for k from (max 0 (floor (/ (- l lo) width)))
                           to (min (1- bins) (floor (/ (- h lo) width)))
                           for overlap = (- (min h (+ lo (* (1+ k) width))) (max l (+ lo (* k width))))
                           when (> overlap 0)
                           do (cl-incf (aref vols k) (* vol (/ overlap (- h l)))))))
    vols))

(defun financial-chart-setups--value-area (vols fraction)
  "The bins (LOW . HIGH) of the value area of VOLS holding FRACTION of the volume.
It grows from the point of control, adding at each step the two bins on
whichever side holds more (Dalton, Jones and Dalton 1990).  Also return
the point-of-control bin as the cdr: ((LOW . HIGH) . POC)."
  ;; Volumes within a part in 10^9 are equal: ties go to the lower bin (the
  ;; POC) and to the side above (the area), so price scaling and shifting
  ;; cannot change the answer through rounding.
  (let* ((n (length vols)) (total (cl-loop for x across vols sum x))
         (poc (cl-loop with best = 0 for k from 1 below n when (> (aref vols k) (* (aref vols best) (+ 1 1e-9))) do (setq best k)
                       finally return best))
         (low poc) (high poc) (sum (aref vols poc)))
    (while (and (< sum (* fraction total)) (or (> low 0) (< high (1- n))))
      (let* ((up (cl-loop for k from (1+ high) to (min (1- n) (+ high 2)) sum (aref vols k)))
             (down (cl-loop for k from (max 0 (- low 2)) below low sum (aref vols k)))
             (go-up (or (= low 0) (and (< high (1- n)) (>= up (* down (- 1 1e-9)))))))
        (if go-up
            (progn (setq sum (+ sum up)) (setq high (min (1- n) (+ high 2))))
          (setq sum (+ sum down)) (setq low (max 0 (- low 2))))))
    (cons (cons low high) poc)))

(defun financial-chart-setups-volume-profile (bars start end bins fraction)
  "The volume profile of BARS (a vector) START..END in BINS price bins.
Return (:lo :hi :bins VOLUMES :poc PRICE :vah PRICE :val PRICE :poc-share S):
point of control (middle of the busiest bin) and the value area holding
FRACTION of the volume (its upper and lower edges)."
  (let* ((lo (cl-loop for j from start to end minimize (financial-chart-swing--l bars j)))
         (hi (cl-loop for j from start to end maximize (financial-chart-swing--h bars j)))
         (hi (if (> hi lo) hi (+ lo 1e-9)))
         (vols (financial-chart-setups--profile-bins bars start end bins lo hi))
         (width (/ (- hi lo) bins))
         (area (financial-chart-setups--value-area vols fraction))
         (total (cl-loop for x across vols sum x)))
    (list :lo lo :hi hi :bins vols
          :poc (+ lo (* width (+ (cdr area) 0.5)))
          :vah (+ lo (* width (1+ (cdr (car area)))))
          :val (+ lo (* width (car (car area))))
          :poc-share (if (> total 0) (/ (aref vols (cdr area)) total) 0.0))))

(defun financial-chart-setups--profiles (v p)
  "Raw profile.volume events of V with params P: one per bar."
  (let ((window (plist-get p :window)) (bins (plist-get p :bins)) (fraction (plist-get p :value_area))
        events)
    (cl-loop for i from 1 below (length v)
             for start = (if window (max 0 (- i window -1)) 0)
             for profile = (financial-chart-setups-volume-profile v start i bins fraction)
             when (> (cl-loop for x across (plist-get profile :bins) sum x) 0)
             do (let* ((lo (plist-get profile :lo)) (hi (plist-get profile :hi))
                       (span (- hi lo))
                       (ratio (lambda (y) (/ (- y lo) span)))
                       (poc (plist-get profile :poc)) (c (financial-chart-swing--c v i)))
                  (push (list :index i :confirmed i :direction 'neutral
                              :strength (financial-chart-swing--clamp (/ (* (plist-get profile :poc-share) bins) 5.0))
                              :geometry (list :levels (list (cons (funcall ratio (plist-get profile :val)) (plist-get profile :val))
                                                            (cons (funcall ratio poc) poc)
                                                            (cons (funcall ratio (plist-get profile :vah)) (plist-get profile :vah)))
                                              :from (cons start lo) :to (cons i hi))
                              :evidence (list :poc_share (plist-get profile :poc-share)
                                              :area_width (/ (- (plist-get profile :vah) (plist-get profile :val)) span)
                                              :close_vs_poc (/ (- c poc) span)))
                        events)))
    (nreverse events)))

(financial-chart-register-detector
 'profile.volume #'financial-chart-setups--profiles
 :kind 'level :direction 'neutral
 :params '((:name window :type integer :default nil :min 2 :doc "bars of the profile; null: every bar so far")
           (:name bins :type integer :default 24 :min 4 :max 200 :doc "price bins between the lowest low and highest high")
           (:name value_area :type number :default 0.7 :min 0.1 :max 1 :doc "share of the volume in the value area"))
 :lag 0 :lookback 1 :needs '(high low close volume)
 :definition "Volume profile (Steidlmayer, J. P. and Koy, K. 1986, Markets and Market Logic; Dalton, J., Jones, E. and Dalton, R. 1990, Mind Over Markets): each bar's volume spread over the price bins its range covers; the point of control (POC) is the busiest bin and the value area the contiguous bins around it holding 70% of the volume, grown two bins at a time toward the busier side.  An event per bar: the profile of the bars so far, geometry levels the value-area low, POC and value-area high (ratio = position between the lowest low and highest high), from and to the window's first and last bar at its low and high.  Evidence: the POC's share of the volume, the value area's width and the close's distance from the POC, in ranges of the whole window.  Strength: the POC's share times the bins, over 5, clamped."
 :invariance '(scale shift)
 :variants '(((:window 30 :bins 12) scale shift) ((:value_area 0.5) scale shift))
 :mark '(:type "level")
 :example "examples/indicators/volume-profile.json"
 :doc "the volume profile's POC and value area as of each bar")

(provide 'financial-chart-detectors-setups)
;;; financial-chart-detectors-setups.el ends here
