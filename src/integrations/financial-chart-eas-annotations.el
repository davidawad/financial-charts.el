;;; financial-chart-eas-annotations.el --- markers and annotations on composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; "annotations" of any pane of a composed chart (fc-gbo.3), drawn over
;; its series.  Each is an object with a "type":
;;
;;   buy, sell   an arrow under the bar's low (buy) or over its high
;;               (sell) at "at" (a bar's time, or its index without
;;               times; an array marks several bars); "y" places it
;;   level       a horizontal line at "y", optionally "from"/"to" times
;;   trendline   "from": [AT, Y] to "to": [AT, Y]; "extend": "right"
;;               carries it to the chart's right edge
;;   event       a vertical line at "at" with its label at the top
;;   text        "label" at "at" and "y"
;;   box         a shaded rectangle "from": [AT, Y] to "to": [AT, Y]
;;   fibonacci   retracement levels from "from" to "to" ([AT, Y] each),
;;               by default the highest high and lowest low of the bars
;;               (or the last "window" bars), "levels" ratios
;;
;; Each takes "label", "color", "dash" and "width" where they apply.
;; On a trading-time axis a time names the bar at it, or the nearest
;; bar (a weekend event lands on Friday's or Monday's); a time outside
;; the bars (and the slots of forward shifts) is NO_SUCH_BAR.
;; A bad one signals `financial-chart-invalid-chart' with :code and the
;; JSON :path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-styles)
(require 'financial-chart-eas-shift)
(require 'financial-chart-eas-trading-time)

(defconst financial-chart-annotation-types
  '("buy" "sell" "level" "trendline" "event" "text" "box" "fibonacci")
  "Annotation types of a composed chart.")

(defconst financial-chart-annotation-color "#607d8b"
  "Default colour of levels, trend lines, events, text and Fibonacci levels.")

(defconst financial-chart-fibonacci-ratios [0 0.236 0.382 0.5 0.618 0.786 1]
  "Default Fibonacci retracement ratios.")

;;; Positions

(defun financial-chart-annotation--x (ctx at path)
  "The x position of AT in CTX: a time (ISO or epoch ms) or a bar index.
On a trading-time axis a time is the slot of the bar at or nearest it.
PATH locates AT."
  (let* ((times (plist-get ctx :times-ext))
         (x (cond ((or times (equal (plist-get ctx :x-type) "temporal"))
                   (and (or (stringp at) (numberp at)) (eas-time-parse at)))
                  ((integerp at) at))))
    (unless x
      (financial-chart-series-fail path "INVALID_TIME"
                                   (if (or times (equal (plist-get ctx :x-type) "temporal"))
                                       "%S is not a time; give an ISO date or epoch ms"
                                     "%S is not a bar index; these bars have no times")
                                   at))
    (if (not times) x
      (or (financial-chart-trading-index times x)
          (financial-chart-series-fail path "NO_SUCH_BAR"
                                       "%S is outside the bars; give a time from %s to %s"
                                       at (financial-chart-annotation--when ctx 0)
                                       (financial-chart-annotation--when ctx (1- (length times))))))))

(defun financial-chart-annotation--bar (ctx at path)
  "The index of the bar at AT in CTX; PATH locates AT."
  (let* ((x (financial-chart-annotation--x ctx at path))
         (i (seq-position (plist-get ctx :xs) x #'=)))
    (or i (financial-chart-series-fail path "NO_SUCH_BAR"
                                       "No bar at %S; give the time of one of the bars (%s to %s)"
                                       at (financial-chart-annotation--when ctx 0)
                                       (financial-chart-annotation--when ctx (1- (length (plist-get ctx :xs))))))))

(defun financial-chart-annotation--when (ctx i)
  "Bar I's position in CTX as text."
  (let ((x (aref (or (plist-get ctx :times-ext) (plist-get ctx :xs)) i)))
    (if (or (plist-get ctx :times-ext) (equal (plist-get ctx :x-type) "temporal"))
        (format-time-string "%F" (floor x 1000) t)
      (format "%s" x))))

(defun financial-chart-annotation--point (ctx point path)
  "POINT, [AT, Y], as (X . Y) in CTX; PATH locates it."
  (let ((point (and (or (vectorp point) (consp point)) (append point nil))))
    (unless (and (= (length point) 2) (numberp (cadr point)))
      (financial-chart-series-fail path "INVALID_ANNOTATION" "A point is [AT, Y], got %S" point))
    (cons (financial-chart-annotation--x ctx (car point) (concat path "/0")) (cadr point))))

(defun financial-chart-annotation--number (a key path &optional default)
  "A's KEY as a number (else DEFAULT); signal at PATH when not a number."
  (let ((v (financial-chart-series-get a key)))
    (cond ((numberp v) v)
          ((and (null v) default) default)
          (t (financial-chart-series-fail (format "%s/%s" path (substring (symbol-name key) 1))
                                          "INVALID_ANNOTATION" "%s needs a number \"%s\", got %S"
                                          (financial-chart-series-get a :type)
                                          (substring (symbol-name key) 1) v)))))

(defun financial-chart-annotation--range (ctx)
  "The bars' price range in CTX as (LOW . HIGH)."
  (let ((bars (plist-get ctx :bars)))
    (cons (apply #'min (mapcar (lambda (b) (plist-get b :low)) bars))
          (apply #'max (mapcar (lambda (b) (plist-get b :high)) bars)))))

;;; Layers

(defun financial-chart-annotation--x-enc (ctx &optional field)
  "The x encoding of an annotation in CTX on FIELD (default time)."
  (append (list :field (or field "time")) (cdr (cdr (financial-chart-styles-x ctx)))))

(defun financial-chart-annotation--rule (ctx name rows colour dash width &rest encoding)
  "A rule layer NAME over ROWS in COLOUR, DASH and WIDTH with ENCODING."
  (list :name name :data (list :values (vconcat rows))
        :mark (financial-chart-styles-line-mark "rule" colour (or width 1) dash)
        :encoding (financial-chart-annotation--encoding ctx encoding)))

(defun financial-chart-annotation--encoding (ctx encoding)
  "ENCODING with its :x and :x2 fields encoded for CTX and :y quantitative."
  (cl-loop for (k v) on encoding by #'cddr
           append (list k (pcase k
                            (:x (financial-chart-annotation--x-enc ctx v))
                            (:y (if (stringp v) (financial-chart-styles-y v) v))
                            ((or :x2 :y2) (list :field v))
                            (_ v)))))

(defun financial-chart-annotation--text (ctx name rows colour &rest mark)
  "A text layer NAME labelling ROWS (with time, y, label) in COLOUR; MARK props."
  (list :name name :data (list :values (vconcat rows))
        :mark (append (list :type "text" :color colour) mark)
        :encoding (append (list :x (financial-chart-annotation--x-enc ctx)
                                :y (if (plist-get (car rows) :y) (financial-chart-styles-y "y") '(:value 0))
                                :text (list :field "label")))))

(defun financial-chart-annotation--price-pane-p (path)
  "Non-nil when annotation PATH is in the price pane."
  (string-prefix-p "/price/" path))

(defun financial-chart-annotation--marker (ctx a name path)
  "Layers of buy or sell marker A named NAME at PATH in CTX."
  (let* ((buy (equal (financial-chart-series-get a :type) "buy"))
         (_ (unless (or (financial-chart-annotation--price-pane-p path) (financial-chart-series-get a :y))
              (financial-chart-series-fail (concat path "/y") "INVALID_ANNOTATION"
                                           "A marker outside the price pane needs \"y\" (bars place it only on price)")))
         (_ (when (financial-chart-series-get a :y) (financial-chart-annotation--number a :y path)))
         (at (financial-chart-series-get a :at))
         (ats (if (or (vectorp at) (consp at)) (append at nil) (list at)))
         (range (financial-chart-annotation--range ctx))
         (pad (* 0.04 (- (cdr range) (car range))))
         (y (financial-chart-series-get a :y))
         (label (financial-chart-series-get a :label))
         (colour (or (financial-chart-series-get a :color) (plist-get ctx (if buy :up :down))))
         (rows (seq-map-indexed
                (lambda (at i)
                  (let* ((index (financial-chart-annotation--bar
                                 ctx at (if (cdr ats) (format "%s/at/%d" path i) (concat path "/at"))))
                         (bar (nth index (plist-get ctx :bars))))
                    (list :time (aref (plist-get ctx :xs) index)
                          :y (cond ((numberp y) y)
                                   (buy (- (plist-get bar :low) pad))
                                   (t (+ (plist-get bar :high) pad)))
                          :label (or label (if buy "buy" "sell")))))
                ats)))
    (unless ats
      (financial-chart-series-fail (concat path "/at") "INVALID_ANNOTATION" "A %s marker needs \"at\""
                                   (if buy "buy" "sell")))
    (append
     (list (list :name name :data (list :values (vconcat rows))
                 :mark (list :type "point" :shape (if buy "triangle-up" "triangle-down")
                             :filled t :size 70 :color colour :opacity 1)
                 :encoding (list :x (financial-chart-annotation--x-enc ctx) :y (financial-chart-styles-y "y")
                                 :tooltip [(:field "label" :type "nominal")
                                           (:field "y" :type "quantitative" :format ".2f")])))
     (when label
       (list (financial-chart-annotation--text ctx (concat name "-label") rows colour
                                               :baseline (if buy "top" "bottom") :dy (if buy 8 -8)))))))

(defun financial-chart-annotation--level (ctx a name path)
  "Layers of level A named NAME at PATH in CTX."
  (let* ((y (financial-chart-annotation--number a :y path))
         (xs (financial-chart-shift-xs ctx))
         (from (if-let* ((f (financial-chart-series-get a :from)))
                   (financial-chart-annotation--x ctx f (concat path "/from"))
                 (aref xs 0)))
         (to (if-let* ((v (financial-chart-series-get a :to)))
                 (financial-chart-annotation--x ctx v (concat path "/to"))
               (aref xs (1- (length xs)))))
         (colour (or (financial-chart-series-get a :color) financial-chart-annotation-color))
         (label (financial-chart-series-get a :label)))
    (append
     (list (financial-chart-annotation--rule ctx name (list (list :time from :time2 to :y y)) colour
                                             (or (financial-chart-series-get a :dash) [4 2])
                                             (financial-chart-series-get a :width)
                                             :x "time" :x2 "time2" :y "y"))
     (when label
       (list (financial-chart-annotation--text ctx (concat name "-label") (list (list :time to :y y :label label))
                                               colour :align "right" :baseline "bottom" :dy -2))))))

(defun financial-chart-annotation--trendline (ctx a name path)
  "Layers of trend line A named NAME at PATH in CTX."
  (pcase-let* ((`(,x1 . ,y1) (financial-chart-annotation--point
                              ctx (financial-chart-series-get a :from) (concat path "/from")))
               (`(,x2 . ,y2) (financial-chart-annotation--point
                              ctx (financial-chart-series-get a :to) (concat path "/to")))
               (extend (or (financial-chart-series-get a :extend) "none"))
               ;; Left to right, so extending keeps the drawn segment.
               (`((,x1 . ,y1) (,x2 . ,y2)) (if (<= x1 x2) (list (cons x1 y1) (cons x2 y2))
                                             (list (cons x2 y2) (cons x1 y1))))
               (xs (financial-chart-shift-xs ctx))
               (slope (if (= x1 x2) 0 (/ (float (- y2 y1)) (- x2 x1))))
               (`(,x1 . ,y1) (if (and (member extend '("left" "both")) (/= x1 x2))
                                 (cons (aref xs 0) (+ y1 (* slope (- (aref xs 0) x1)))) (cons x1 y1)))
               (`(,x2 . ,y2) (if (and (member extend '("right" "both")) (/= x1 x2))
                                 (let ((end (aref xs (1- (length xs))))) (cons end (+ y2 (* slope (- end x2)))))
                               (cons x2 y2)))
               (colour (or (financial-chart-series-get a :color) financial-chart-annotation-color))
               (label (financial-chart-series-get a :label)))
    (unless (member extend '("none" "left" "right" "both"))
      (financial-chart-series-fail (concat path "/extend") "INVALID_ANNOTATION"
                                   "extend is none, left, right or both, got %S" extend))
    (append
     (list (financial-chart-annotation--rule ctx name (list (list :time x1 :time2 x2 :y y1 :y2 y2)) colour
                                             (financial-chart-series-get a :dash)
                                             (or (financial-chart-series-get a :width) 1.5)
                                             :x "time" :x2 "time2" :y "y" :y2 "y2"))
     (when label
       (list (financial-chart-annotation--text ctx (concat name "-label") (list (list :time x2 :y y2 :label label))
                                               colour :align "right" :baseline "bottom" :dy -2))))))

(defun financial-chart-annotation--event (ctx a name path)
  "Layers of event A named NAME at PATH in CTX."
  (let* ((x (financial-chart-annotation--x ctx (financial-chart-series-get a :at) (concat path "/at")))
         (colour (or (financial-chart-series-get a :color) financial-chart-annotation-color))
         (label (financial-chart-series-get a :label)))
    (append
     (list (financial-chart-annotation--rule ctx name (list (list :time x :label (or label "")))
                                             colour (or (financial-chart-series-get a :dash) [2 2])
                                             (financial-chart-series-get a :width)
                                             :x "time" :tooltip [(:field "label" :type "nominal")]))
     (when label
       (list (financial-chart-annotation--text ctx (concat name "-label") (list (list :time x :label label))
                                               colour :align "left" :baseline "top" :dx 3))))))

(defun financial-chart-annotation--label (ctx a name path)
  "The text annotation A named NAME at PATH in CTX."
  (let ((label (financial-chart-series-get a :label)))
    (unless (stringp label)
      (financial-chart-series-fail (concat path "/label") "INVALID_ANNOTATION" "A text annotation needs a \"label\""))
    (list (financial-chart-annotation--text
           ctx name (list (list :time (financial-chart-annotation--x ctx (financial-chart-series-get a :at)
                                                                     (concat path "/at"))
                                :y (financial-chart-annotation--number a :y path) :label label))
           (or (financial-chart-series-get a :color) financial-chart-annotation-color)
           :align "center"))))

(defun financial-chart-annotation--box (ctx a name path)
  "Layers of box A named NAME at PATH in CTX."
  (pcase-let* ((`(,x1 . ,y1) (financial-chart-annotation--point
                              ctx (financial-chart-series-get a :from) (concat path "/from")))
               (`(,x2 . ,y2) (financial-chart-annotation--point
                              ctx (financial-chart-series-get a :to) (concat path "/to")))
               (colour (or (financial-chart-series-get a :color) financial-chart-annotation-color))
               (label (financial-chart-series-get a :label)))
    (append
     (list (list :name name :data (list :values (vector (list :time (min x1 x2) :time2 (max x1 x2)
                                                               :y (min y1 y2) :y2 (max y1 y2))))
                 :mark (list :type "rect" :color colour
                             :opacity (or (financial-chart-series-get a :opacity) 0.15))
                 :encoding (financial-chart-annotation--encoding ctx '(:x "time" :x2 "time2" :y "y" :y2 "y2"))))
     (when label
       (list (financial-chart-annotation--text ctx (concat name "-label")
                                               (list (list :time (min x1 x2) :y (max y1 y2) :label label))
                                               colour :align "left" :baseline "top" :dx 2 :dy 2))))))

;;; Fibonacci

(defun financial-chart-fibonacci-swing (bars xs &optional window)
  "The swing of BARS at XS over the last WINDOW bars: ((X1 . Y1) (X2 . Y2)).
It runs from whichever of the highest high and lowest low comes first
to the other."
  (let* ((n (length bars))
         (start (if window (max 0 (- n window)) 0))
         (hi start) (lo start))
    (cl-loop for i from start below n for b = (nth i bars)
             do (when (> (plist-get b :high) (plist-get (nth hi bars) :high)) (setq hi i))
             do (when (< (plist-get b :low) (plist-get (nth lo bars) :low)) (setq lo i)))
    (let ((high (cons (aref xs hi) (plist-get (nth hi bars) :high)))
          (low (cons (aref xs lo) (plist-get (nth lo bars) :low))))
      (if (<= lo hi) (list low high) (list high low)))))

(defun financial-chart-fibonacci-levels (from to ratios)
  "Retracement prices of RATIOS for a move FROM a price TO a price.
Ratio 0 is TO (where the move ended), 1 is FROM.  Return (RATIO . PRICE)s."
  (mapcar (lambda (r) (cons r (- to (* r (- to from))))) (append ratios nil)))

(defun financial-chart-annotation--fibonacci (ctx a name path)
  "Layers of Fibonacci retracement A named NAME at PATH in CTX."
  (let* ((_ (unless (or (financial-chart-annotation--price-pane-p path)
                        (and (financial-chart-series-get a :from) (financial-chart-series-get a :to)))
              (financial-chart-series-fail (concat path "/from") "INVALID_ANNOTATION"
                                           "Fibonacci outside the price pane needs \"from\" and \"to\"")))
         (window (financial-chart-series-get a :window))
         (_ (unless (or (null window) (and (integerp window) (> window 1)))
              (financial-chart-series-fail (concat path "/window") "INVALID_ANNOTATION"
                                           "window is a bar count above 1, got %S" window)))
         (swing (financial-chart-fibonacci-swing (plist-get ctx :bars) (plist-get ctx :xs) window))
         (from (if (financial-chart-series-get a :from)
                   (financial-chart-annotation--point ctx (financial-chart-series-get a :from) (concat path "/from"))
                 (car swing)))
         (to (if (financial-chart-series-get a :to)
                 (financial-chart-annotation--point ctx (financial-chart-series-get a :to) (concat path "/to"))
               (cadr swing)))
         (ratios (or (financial-chart-series-get a :levels) financial-chart-fibonacci-ratios))
         (_ (unless (and (or (vectorp ratios) (consp ratios)) (seq-every-p #'numberp ratios))
              (financial-chart-series-fail (concat path "/levels") "INVALID_ANNOTATION"
                                           "levels are ratios such as 0.382, got %S" ratios)))
         (levels (financial-chart-fibonacci-levels (cdr from) (cdr to) ratios))
         (xs (financial-chart-shift-xs ctx))
         (start (min (car from) (car to)))
         (end (aref xs (1- (length xs))))
         (colour (or (financial-chart-series-get a :color) financial-chart-annotation-color))
         (rows (mapcar (lambda (l) (list :time start :time2 end :y (cdr l)
                                         :label (format "%s%% %.2f" (/ (round (* 1000 (car l))) 10.0) (cdr l))))
                       levels))
         (sorted (sort (mapcar #'cdr levels) #'<)))
    (append
     ;; Alternate bands between the levels, lighter and darker.
     (cl-loop for parity in '(0 1) for opacity in '(0.06 0.12)
              for bands = (cl-loop for (lo hi) on sorted while hi for i from 0
                                   when (= (% i 2) parity)
                                   collect (list :time start :time2 end :y lo :y2 hi))
              when bands
              collect (list :name (format "%s-bands-%d" name parity) :data (list :values (vconcat bands))
                            :mark (list :type "rect" :color colour :opacity opacity)
                            :encoding (financial-chart-annotation--encoding
                                       ctx '(:x "time" :x2 "time2" :y "y" :y2 "y2"))))
     (list
          (financial-chart-annotation--rule ctx name rows colour nil 1 :x "time" :x2 "time2" :y "y")
          (financial-chart-annotation--rule ctx (concat name "-swing")
                                            (list (list :time (car from) :time2 (car to)
                                                        :y (cdr from) :y2 (cdr to)))
                                            colour [3 3] 1 :x "time" :x2 "time2" :y "y" :y2 "y2")
          (financial-chart-annotation--text ctx (concat name "-labels")
                                            (mapcar (lambda (r) (plist-put (copy-sequence r) :time end)) rows)
                                            colour :align "right" :baseline "bottom" :dy -1)))))

;;; Entry point

(defun financial-chart-annotation-layers (ctx pane name path)
  "The layers of PANE's \"annotations\" in CTX, named after pane NAME.
PATH locates PANE."
  (let ((annotations (financial-chart-series-get pane :annotations)))
    (unless (or (null annotations) (vectorp annotations) (consp annotations))
      (financial-chart-series-fail (concat path "/annotations") "INVALID_ANNOTATION"
                                   "annotations is an array of objects with a \"type\""))
    (cl-loop
     for a in (append annotations nil) for i from 0
     for at = (format "%s/annotations/%d" path i)
     for type = (and (listp a) (keywordp (car a)) (financial-chart-series-get a :type))
     for layer-name = (format "%s-annotation-%d" name i)
     append
     (pcase type
       ((or "buy" "sell") (financial-chart-annotation--marker ctx a layer-name at))
       ("level" (financial-chart-annotation--level ctx a layer-name at))
       ("trendline" (financial-chart-annotation--trendline ctx a layer-name at))
       ("event" (financial-chart-annotation--event ctx a layer-name at))
       ("text" (financial-chart-annotation--label ctx a layer-name at))
       ("box" (financial-chart-annotation--box ctx a layer-name at))
       ("fibonacci" (financial-chart-annotation--fibonacci ctx a layer-name at))
       ('nil (financial-chart-series-fail at "INVALID_ANNOTATION"
                                          "An annotation is an object with a \"type\" (%s), got %S"
                                          (string-join financial-chart-annotation-types ", ") a))
       (_ (financial-chart-series-fail (concat at "/type") "UNKNOWN_ANNOTATION"
                                       "Unknown annotation type %S; types: %s" type
                                       (string-join financial-chart-annotation-types ", ")))))))

(provide 'financial-chart-eas-annotations)
;;; financial-chart-eas-annotations.el ends here
