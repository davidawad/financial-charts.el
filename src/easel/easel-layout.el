;;; easel-layout.el --- axes, legends, titles and their geometry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Layout is parameterized by METRICS, so one compile
;; serves both renderers: the svg target uses Vega's config defaults in
;; pixels; the text target snaps everything to character cells (every
;; font is one cell high, every glyph one cell wide, y titles are
;; horizontal).  Renderers never place anything themselves.

;;; Code:

(require 'easel-core)
(require 'easel-scale)
(require 'easel-encode)

(defun easel-layout-metrics (target &optional cell)
  "Return layout metrics for TARGET (svg or text); CELL is [W H] in px."
  (let* ((cell (or cell [7 14])) (cw (aref cell 0)) (ch (aref cell 1)))
    (if (eq target 'text)
        (list :target "text" :cell cell :pad 0 :tick-bottom ch :tick-left cw :label-pad 0 :label-size ch
              :title-size ch :title-pad 0 :chart-title-size ch :chart-title-pad 0
              :legend-offset (* 2 cw) :symbol cw :row ch :spacing ch :char-w cw
              :x-tick-spacing (* 12 cw) :y-tick-spacing (* 3 ch))
      (list :target "svg" :cell cell :pad 5 :tick-bottom 5 :tick-left 5 :label-pad 2 :label-size 10
            :title-size 11 :title-pad 4 :chart-title-size 13 :chart-title-pad 4
            :legend-offset 18 :symbol 11 :row 16 :spacing 20 :char-w nil
            :x-tick-spacing 40 :y-tick-spacing 40))))

(defun easel-layout-text-p (metrics)
  "Non-nil for text-target METRICS."
  (equal (plist-get metrics :target) "text"))

(defun easel-layout-text-width (metrics text size)
  "Estimated width of TEXT at font SIZE under METRICS.
The svg target uses Vega's own headless estimate, floor(0.8 * size *
length), so layout matches Vega rendered without a canvas."
  (let ((n (string-width (or text ""))))
    (if (plist-get metrics :char-w) (* n (plist-get metrics :char-w))
      (floor (* 0.8 size n)))))

;;; Axes

(defun easel-layout-time-unit-format (field)
  "Vega-Lite's label format for a timeUnit-derived FIELD (UNIT_SOURCE)."
  (let ((unit (car (split-string field "_"))))
    (cond ((string-match-p "date\\'" unit) (if (string-match-p "year" unit) "%b %d, %Y" "%b %d"))
          ((string-match-p "month\\'" unit) (if (string-match-p "year" unit) "%b %Y" "%b"))
          ((string-match-p "quarter" unit) "Q%q")
          ((string-match-p "day\\'" unit) "%a")
          ((string-match-p "hours\\'" unit) "%H:00")
          ((string-match-p "minutes\\'" unit) "%H:%M")
          ((equal unit "year") "%Y")
          (t "%b %d, %Y"))))

(defun easel-layout-axis (channel def scale plot-size metrics)
  "Return the axis model for CHANNEL's DEF and SCALE, or nil when disabled.
PLOT-SIZE is the plot extent along the axis."
  (let ((axis (plist-get def :axis)))
    (unless (or (memq axis '(:null :false)) (null scale) (null def))
      (let* ((discrete (member (plist-get scale :type) '("band" "point")))
             (spacing (plist-get metrics (if (eq channel :x) :x-tick-spacing :y-tick-spacing)))
             (count (or (plist-get axis :tickCount) (max 2 (ceiling (/ plot-size (float spacing))))))
             (fmt (if (and discrete (equal (plist-get def :derived) "timeUnit") (null (plist-get axis :format)))
                      (let ((f (easel-layout-time-unit-format (plist-get def :field))))
                        (lambda (v) (if (numberp v) (let ((system-time-locale "C")) (easel-time-format v f))
                                      (format "%s" v))))
                    (easel-scale-tick-format scale count (or (plist-get axis :format) (plist-get def :format)))))
             (values (if (plist-get axis :values) (append (plist-get axis :values) nil)
                       (easel-scale-ticks scale count)))
             (title (if (plist-member axis :title)
                        (let ((tt (plist-get axis :title))) (and (stringp tt) tt))
                      (easel-encode-title def)))
             (angle (cond ((plist-get axis :labelAngle))
                          ((easel-layout-text-p metrics) 0)
                          ((and (eq channel :x) discrete (not (equal (plist-get def :derived) "timeUnit"))) 270)
                          (t 0))))
        (list :channel (easel-key-name channel)
              :orient (if (eq channel :x) "bottom" "left")
              ;; Distance from the plot's top edge to the highest labelled tick.
              :top-gap (when (and (eq channel :y) (not discrete))
                         (let* ((probe (plist-put (copy-sequence scale) :range (vector plot-size 0)))
                                (ys (delq nil (mapcar (lambda (v) (and (not (string-empty-p (funcall fmt v)))
                                                                       (easel-scale-apply probe v)))
                                                      values))))
                           (and ys (apply #'min ys))))
              :title title :discrete (if discrete t :false) :labelAngle angle
              :grid (if (and (not discrete) (not (eq (plist-get axis :grid) :false))) t :false)
              :ticks (vconcat (mapcar (lambda (v) (list :value v :label (funcall fmt v))) values)))))))

(defun easel-layout-axis-label-extent (axis metrics)
  "Thickness of AXIS's labels across the axis."
  (let ((widths (mapcar (lambda (tk) (easel-layout-text-width metrics (plist-get tk :label)
                                                              (plist-get metrics :label-size)))
                        (plist-get axis :ticks))))
    (if (and (equal (plist-get axis :orient) "bottom") (zerop (plist-get axis :labelAngle)))
        (plist-get metrics :label-size)
      (if widths (apply #'max widths) 0))))

(defun easel-layout--tick (axis metrics)
  "Tick length of AXIS under METRICS (one cell in text)."
  (plist-get metrics (if (equal (plist-get axis :orient) "bottom") :tick-bottom :tick-left)))

(defun easel-layout-axis-extent (axis metrics)
  "Space AXIS needs outside the plot: (SIDE . PIXELS), SIDE :left/:bottom/:top."
  (let* ((labels (+ (easel-layout--tick axis metrics) (plist-get metrics :label-pad)
                    (easel-layout-axis-label-extent axis metrics)))
         (title (and (plist-get axis :title)
                     (+ (plist-get metrics :title-pad) (plist-get metrics :title-size))))
         (left (equal (plist-get axis :orient) "left")))
    (append (list (cons (if left :left :bottom) (+ labels (if (and title (not (and left (easel-layout-text-p metrics)))) title 0))))
            (when (and left title (easel-layout-text-p metrics))
              (list (cons :top (plist-get metrics :title-size))))
            ;; Vega pads for the part of the top y label that overhangs the plot.
            (when (and left (not (easel-layout-text-p metrics)) (plist-get axis :top-gap))
              (let ((over (- (/ (plist-get metrics :label-size) 2.0) (plist-get axis :top-gap))))
                (when (> over 0) (list (cons :top over))))))))

(defun easel-layout--thin (ticks key extent-fn gap)
  "Vega's parity overlap removal over TICKS ordered by KEY.
EXTENT-FN maps a tick to its label's (LO . HI) along the axis; labels
closer than GAP count as overlapping."
  (let ((ticks (sort (append ticks nil) (lambda (a b) (< (plist-get a key) (plist-get b key))))))
    (while (and (> (length ticks) 2)
                (cl-loop for (a b) on ticks while b
                         thereis (> (+ (cdr (funcall extent-fn a)) gap) (car (funcall extent-fn b)))))
      (setq ticks (cl-loop for tk in ticks for i from 0 when (cl-evenp i) collect tk)))
    (vconcat ticks)))

(defun easel-layout--bottom-align (p x0 w flush angle)
  "Label alignment of a bottom tick at P (Vega-Lite labelFlush when FLUSH)."
  (cond ((not (zerop angle)) "right")
        ((and flush (< (abs (- p x0)) 0.5)) "left")
        ((and flush (< (abs (- p (+ x0 w))) 0.5)) "right")
        (t "center")))

(defun easel-layout-axis-place (axis scale bounds metrics)
  "Return AXIS with geometry for SCALE inside plot BOUNDS [x0 y0 w h]."
  (let* ((x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3))
         (tick (easel-layout--tick axis metrics)) (pad (plist-get metrics :label-pad))
         ;; Text axes sit in their own cell row/column: lines at its centre.
         (inset (if (easel-layout-text-p metrics) (/ tick 2.0) 0))
         (size (plist-get metrics :label-size))
         (bottom (equal (plist-get axis :orient) "bottom"))
         (flush (and bottom (eq (plist-get axis :discrete) :false)))
         (half (/ (or (plist-get scale :bandwidth) 0) 2.0))
         (angle (plist-get axis :labelAngle))
         (ticks (seq-filter
                 (lambda (tk) (plist-get tk :pos))
                 (seq-map (lambda (tk)
                            (let ((p (easel-scale-apply scale (plist-get tk :value))))
                              (append tk (list :pos (and p (+ p half))))))
                          (plist-get axis :ticks))))
         (gap (or (plist-get metrics :char-w) 1))
         (ticks (if (and bottom (zerop angle))
                    (easel-layout--thin
                     ticks :pos
                     (lambda (tk)
                       (let ((p (plist-get tk :pos))
                             (lw (easel-layout-text-width metrics (plist-get tk :label) size)))
                         (pcase (easel-layout--bottom-align p x0 w flush angle)
                           ("left" (cons p (+ p lw))) ("right" (cons (- p lw) p))
                           (_ (cons (- p (/ lw 2.0)) (+ p (/ lw 2.0)))))))
                     gap)
                  (easel-layout--thin ticks :pos (lambda (tk) (cons (- (plist-get tk :pos) (/ size 2.0))
                                                                    (+ (plist-get tk :pos) (/ size 2.0))))
                                      (if (easel-layout-text-p metrics) 0 1))))
         (label-extent (easel-layout-axis-label-extent axis metrics))
         (placed
          (vconcat
           (seq-map
            (lambda (tk)
              (let ((p (plist-get tk :pos)))
                (append tk
                        (if bottom
                            (list :tick (vector p (+ y0 h inset) p (+ y0 h tick))
                                  :lx p :ly (+ y0 h tick pad)
                                  :align (easel-layout--bottom-align p x0 w flush angle)
                                  :baseline (if (zerop angle) "top" "middle"))
                          (list :tick (vector (- x0 tick) p (- x0 inset) p)
                                :lx (- x0 tick pad) :ly p :align "right" :baseline "middle"))
                        (when (eq (plist-get axis :grid) t)
                          (list :grid (if bottom (vector p y0 p (+ y0 h)) (vector x0 p (+ x0 w) p)))))))
            ticks)))
         (title (plist-get axis :title)))
    (append (easel--plist-without axis :ticks)
            (list :ticks placed
                  :domain-line (if bottom (vector x0 (+ y0 h inset) (+ x0 w) (+ y0 h inset))
                                 (vector (- x0 inset) y0 (- x0 inset) (+ y0 h))))
            (when title
              (list :title-mark
                    (cond
                     (bottom (list :text title :x (+ x0 (/ w 2.0))
                                   :y (+ y0 h tick pad label-extent (plist-get metrics :title-pad))
                                   :align "center" :baseline "top" :angle 0))
                     ((easel-layout-text-p metrics)
                      (list :text title :x (- x0 tick pad label-extent)
                            :y (- y0 (plist-get metrics :title-size))
                            :align "left" :baseline "top" :angle 0))
                     (t (list :text title
                              :x (- x0 tick pad label-extent (plist-get metrics :title-pad))
                              :y (+ y0 (/ h 2.0)) :align "center" :baseline "bottom" :angle -90))))))))

;;; Legends

(defun easel-layout-legend (channel def scale shape _metrics)
  "Return the legend model for color CHANNEL's DEF and SCALE drawn as SHAPE."
  (let ((legend (plist-get def :legend)))
    (unless (or (memq legend '(:null :false)) (null scale))
      (let ((title (if (plist-member legend :title)
                       (let ((tt (plist-get legend :title))) (and (stringp tt) tt))
                     (easel-encode-title def))))
        (if (equal (plist-get scale :type) "ordinal")
            (list :channel (easel-key-name channel) :type "symbol" :title title :shape shape
                  :entries (vconcat (seq-map (lambda (v)
                                               (list :value v :label (easel-expr--string v)
                                                     :color (easel-scale-apply scale v)))
                                             (plist-get scale :domain))))
          (list :channel (easel-key-name channel) :type "gradient" :title title
                :stops (plist-get scale :range) :domain (plist-get scale :domain)
                :entries []))))))

(defun easel-layout--gradient-entries (legend metrics)
  "LEGEND's gradient labels: d3 ticks with one per ~40px of its length."
  (let* ((domain (plist-get legend :domain))
         (count (max 2 (ceiling (/ (easel-layout--gradient-length legend metrics) 40.0))))
         (fmt (easel-scale-tick-format (list :type "linear" :domain domain) count)))
    (vconcat (mapcar (lambda (v) (list :value v :label (funcall fmt v)))
                     (easel-scale-linear-ticks (aref domain 0) (aref domain 1) count)))))

(defun easel-layout-legend-sized (legend metrics)
  "LEGEND with its gradient entries filled in for its final length."
  (if (equal (plist-get legend :type) "gradient")
      (easel-plist-put legend :entries (easel-layout--gradient-entries legend metrics))
    legend))

(defun easel-layout-legend-size (legend metrics)
  "Return (WIDTH . HEIGHT) of LEGEND, including its offset from the plot."
  (let* ((size (plist-get metrics :label-size))
         (labels (mapcar (lambda (e) (easel-layout-text-width metrics (plist-get e :label) size))
                         (plist-get legend :entries)))
         (title-w (if (plist-get legend :title)
                      (easel-layout-text-width metrics (plist-get legend :title) (plist-get metrics :title-size))
                    0))
         (sym (plist-get metrics :symbol))
         (gradient (equal (plist-get legend :type) "gradient"))
         (body-w (+ (if gradient (* 1.5 sym) sym) (if (easel-layout-text-p metrics) (plist-get metrics :char-w) 4)
                    (if labels (apply #'max labels) 0)))
         (title-h (if (plist-get legend :title) (+ (plist-get metrics :title-size) (if (easel-layout-text-p metrics) 0 5)) 0))
         (rows (length labels)))
    (cons (+ (plist-get metrics :legend-offset) (max title-w body-w))
          (+ title-h (if gradient (easel-layout--gradient-length legend metrics)
                       (* rows (plist-get metrics :row)))))))

(defun easel-layout--gradient-length (legend metrics)
  "Vega-Lite's gradient length: clamp(plot height, 64, 200); rows in text."
  (if (easel-layout-text-p metrics)
      (* (max 5 (length (plist-get legend :entries))) (plist-get metrics :row))
    (max 64 (min 200 (or (plist-get legend :plot-h) 200)))))

(defun easel-layout-legend-place (legend x y metrics)
  "Return LEGEND with geometry, its top-left corner at X Y."
  (let* ((text (easel-layout-text-p metrics))
         (sym (plist-get metrics :symbol)) (row (plist-get metrics :row))
         (title-h (if (plist-get legend :title) (+ (plist-get metrics :title-size) (if text 0 5)) 0))
         (gap (if text (plist-get metrics :char-w) 4))
         (width (- (car (easel-layout-legend-size legend metrics)) (plist-get metrics :legend-offset)))
         (gradient (equal (plist-get legend :type) "gradient"))
         (glen (easel-layout--gradient-length legend metrics)))
    (append (easel--plist-without legend :entries)
            (list :x x :y y :width width)
            (when (plist-get legend :title)
              (list :title-mark (list :text (plist-get legend :title) :x x :y y
                                      :align "left" :baseline "top")))
            (when gradient
              (list :bar (vector x (+ y title-h) (* 1.5 sym) glen)))
            (list :entries
                  (vconcat
                   (seq-map-indexed
                    (lambda (e i)
                      (let* ((d (plist-get legend :domain))
                             (top (+ y title-h
                                     (if gradient
                                         (* glen (/ (- (aref d 1) (plist-get e :value)) (float (max 1e-9 (- (aref d 1) (aref d 0))))))
                                       (* i row))))
                             (cy (if gradient top (+ top (/ row 2.0)))))
                        (append e
                                (list :sx (+ x (/ sym 2.0)) :sy cy
                                      :lx (+ x (if gradient (* 1.5 sym) sym) gap) :ly cy
                                      :bounds (vector x top width row)))))
                    (plist-get legend :entries)))))))

(provide 'easel-layout)
;;; easel-layout.el ends here
