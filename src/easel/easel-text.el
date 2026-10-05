;;; easel-text.el --- scene/v1 -> propertized character grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L5, terminal half.  Draws a text-target scene into a grid of cells
;; (pixel X,Y lands in cell floor(X/W), floor(Y/H) for the scene's cell
;; size).  Series are braille (2x4 dots per cell), areas and bars use
;; eighth blocks, axes are box-drawing lines.  Every mark cell carries
;; `easel-view', `easel-mark', `easel-datum' and `help-echo', so moving
;; point is the terminal's hover.  Output is a deterministic string.
;; Like the SVG renderer it reads only the scene.

;;; Code:

(require 'easel-core)
(require 'easel-glyph)
(require 'easel-hit)

(defface easel-axis '((t :inherit shadow)) "Face for easel axis lines and grid." :group 'faces)
(defface easel-label '((t :inherit default)) "Face for easel tick labels." :group 'faces)
(defface easel-title '((t :inherit bold)) "Face for easel chart and axis titles." :group 'faces)

(cl-defstruct (easel-text--grid (:constructor easel-text--grid-make))
  cols rows cw ch chars props prio dots dot-props)

(defun easel-text--new (scene)
  "An empty grid sized for SCENE."
  (let* ((size (plist-get scene :size)) (cell (plist-get size :cell))
         (cw (aref cell 0)) (ch (aref cell 1))
         (cols (max 1 (round (/ (float (plist-get size :w)) cw))))
         (rows (max 1 (round (/ (float (plist-get size :h)) ch))))
         (n (* cols rows)))
    (easel-text--grid-make :cols cols :rows rows :cw cw :ch ch
                           :chars (make-vector n ?\s) :props (make-vector n nil)
                           :prio (make-vector n -1) :dots (make-vector n 0) :dot-props (make-vector n nil))))

(defun easel-text--put (g col row char props prio)
  "Put CHAR with PROPS at COL ROW of grid G when PRIO wins."
  (when (and (< -1 col (easel-text--grid-cols g)) (< -1 row (easel-text--grid-rows g)))
    (let ((i (+ col (* row (easel-text--grid-cols g)))))
      (when (>= prio (aref (easel-text--grid-prio g) i))
        (aset (easel-text--grid-chars g) i char)
        (aset (easel-text--grid-props g) i props)
        (aset (easel-text--grid-prio g) i prio)))))

(defun easel-text--string (g x y text align props prio)
  "Put TEXT anchored at pixel X Y with ALIGN into grid G."
  (let* ((len (string-width text))
         (c (round (/ x (float (easel-text--grid-cw g)))))
         (start (pcase align ("left" c) ("right" (- c len)) (_ (round (- (/ x (float (easel-text--grid-cw g))) (/ len 2.0))))))
         (row (floor (/ y (float (easel-text--grid-ch g))))))
    (dotimes (k (length text))
      (easel-text--put g (+ start k) row (aref text k) props prio))))

(defun easel-text--col (g x) "Cell column of pixel X." (floor (/ x (float (easel-text--grid-cw g)))))
(defun easel-text--row (g y) "Cell row of pixel Y." (floor (/ y (float (easel-text--grid-ch g)))))

(defun easel-text--dot (g dx dy props clip)
  "Set braille dot DX DY (dot coordinates) with PROPS inside CLIP cells."
  (let ((col (floor dx 2)) (row (floor dy 4)))
    (when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3))
               (< -1 col (easel-text--grid-cols g)) (< -1 row (easel-text--grid-rows g)))
      (let ((i (+ col (* row (easel-text--grid-cols g)))))
        (aset (easel-text--grid-dots g) i
              (logior (aref (easel-text--grid-dots g) i)
                      (aref (aref easel-glyph-braille-dots (mod dy 4)) (mod dx 2))))
        (aset (easel-text--grid-dot-props g) i props)))))

(defun easel-text--dot-line (g x1 y1 x2 y2 props-fn clip)
  "Braille line from pixel X1 Y1 to X2 Y2; PROPS-FN maps a pixel x to props."
  (let* ((sx (/ 2.0 (easel-text--grid-cw g))) (sy (/ 4.0 (easel-text--grid-ch g)))
         (a (floor (* x1 sx))) (b (floor (* y1 sy))) (c (floor (* x2 sx))) (d (floor (* y2 sy)))
         (dx (abs (- c a))) (dy (- (abs (- d b)))) (stepx (if (< a c) 1 -1)) (stepy (if (< b d) 1 -1))
         (err (+ dx dy)) (done nil))
    (while (not done)
      (easel-text--dot g a b (funcall props-fn (/ (+ a 0.5) sx)) clip)
      (if (and (= a c) (= b d)) (setq done t)
        (let ((e2 (* 2 err)))
          (when (>= e2 dy) (setq err (+ err dy) a (+ a stepx)))
          (when (<= e2 dx) (setq err (+ err dx) b (+ b stepy))))))))

(defun easel-text--tooltip (item)
  "help-echo text for ITEM."
  (when-let* ((tip (plist-get item :tooltip)))
    (mapconcat (lambda (p) (format "%s: %s" (plist-get p :title) (plist-get p :value))) tip "\n")))

(defun easel-text--item-props (view mark item datum)
  "Text properties for a cell of ITEM (DATUM) in MARK of VIEW."
  (let ((color (let ((f (plist-get item :fill)) (s (plist-get item :stroke)))
                 (if (or (null f) (equal f "none")) s f))))
    (append (list 'easel-view (plist-get view :id) 'easel-mark (plist-get mark :id) 'easel-datum datum)
            (when-let* ((tip (easel-text--tooltip item))) (list 'help-echo tip))
            (when (and color (not (equal color "none"))) (list 'face (list :foreground color))))))

(defun easel-text--interp (points x)
  "Linear interpolation of the polyline POINTS ([x y] vector) at X, or nil."
  (let ((n (length points)))
    (when (and (> n 0) (<= (aref (aref points 0) 0) x (aref (aref points (1- n)) 0)))
      (let ((i (easel-hit--bisect (vconcat (mapcar (lambda (p) (aref p 0)) points)) x)))
        (let* ((p (aref points i))
               (q (aref points (if (and (< (aref p 0) x) (< i (1- n))) (1+ i) (if (> i 0) (1- i) i)))))
          (if (= (aref p 0) (aref q 0)) (aref p 1)
            (+ (aref p 1) (* (- (aref q 1) (aref p 1)) (/ (- x (aref p 0)) (float (- (aref q 0) (aref p 0))))))))))))

(defun easel-text--series (g view mark item clip)
  "Draw line or area ITEM of MARK in VIEW."
  (let* ((points (plist-get item :points))
         (anchors (easel-hit--anchors item))
         (axs (vconcat (mapcar (lambda (p) (aref p 0)) anchors)))
         (props-fn (lambda (x) (easel-text--item-props
                                view mark item (aref (plist-get item :datum) (easel-hit--bisect axs x)))))
         (ch (easel-text--grid-ch g)))
    (if-let* ((base (plist-get item :base)))
        (cl-loop for col from (aref clip 0) below (aref clip 2)
                 for cx = (* (+ col 0.5) (easel-text--grid-cw g))
                 for top = (easel-text--interp points cx)
                 for bottom = (easel-text--interp base cx)
                 when (and top bottom)
                 do (cl-loop for row from (max (aref clip 1) (floor top ch)) below (min (aref clip 3) (ceiling bottom ch))
                             for y0 = (* row ch) for y1 = (* (1+ row) ch)
                             for covered = (- (min y1 bottom) (max y0 top))
                             do (cond
                                 ((>= covered (- ch 0.01)) (easel-text--put g col row ?█ (funcall props-fn cx) 1))
                                 ((and (> top y0) (> covered 0))
                                  (let ((e (round (* 8 (/ covered ch)))))
                                    (when (> e 0) (easel-text--put g col row (easel-glyph-lower e) (funcall props-fn cx) 1))))
                                 ((>= covered (/ ch 2.0)) (easel-text--put g col row ?█ (funcall props-fn cx) 1)))))
      (dotimes (k (max 0 (1- (length points))))
        (let ((p (aref points k)) (q (aref points (1+ k))))
          (easel-text--dot-line g (aref p 0) (aref p 1) (aref q 0) (aref q 1) props-fn clip)))
      (when (= (length points) 1)
        (let ((p (aref points 0))) (easel-text--dot-line g (aref p 0) (aref p 1) (aref p 0) (aref p 1) props-fn clip))))))

(defun easel-text--rect (g view mark item clip i)
  "Draw bar, rect or brush ITEM (index I) of MARK with eighth blocks."
  (let* ((cw (easel-text--grid-cw g)) (ch (easel-text--grid-ch g))
         (x (plist-get item :x)) (y (plist-get item :y)) (w (plist-get item :w)) (h (plist-get item :h))
         (orient (plist-get item :orient))
         (props (if (equal (plist-get mark :mark) "brush")
                    (list 'face (list :background "#555555") 'easel-brush (plist-get mark :param))
                  (easel-text--item-props view mark item (plist-get item :datum))))
         (c0 (if (equal orient "horizontal") (floor x cw) (round x cw)))
         (c1 (max (1+ c0) (if (equal orient "horizontal") (ceiling (+ x w) cw) (round (+ x w) cw))))
         (r0 (if (equal orient "vertical") (floor y ch) (round y ch)))
         (r1 (max (1+ r0) (if (equal orient "vertical") (ceiling (+ y h) ch) (round (+ y h) ch)))))
    (ignore i)
    (cl-loop for row from (max r0 (aref clip 1)) below (min r1 (aref clip 3))
             do (cl-loop for col from (max c0 (aref clip 0)) below (min c1 (aref clip 2))
                         for char = (cond
                                     ((equal (plist-get mark :mark) "brush") nil)
                                     ((and (equal orient "vertical") (= row r0))
                                      (easel-glyph-lower (round (* 8 (/ (- (* (1+ row) ch) y) (float ch))))))
                                     ((and (equal orient "horizontal") (= col (1- c1)))
                                      (easel-glyph-left (round (* 8 (/ (- (+ x w) (* col cw)) (float cw))))))
                                     (t ?█))
                         do (if char
                                (unless (eq char ?\s) (easel-text--put g col row char props 1))
                              (let ((idx (+ col (* row (easel-text--grid-cols g)))))
                                (when (< -1 idx (length (easel-text--grid-props g)))
                                  (aset (easel-text--grid-props g) idx
                                        (append props (aref (easel-text--grid-props g) idx))))))))))

(defun easel-text--segment (g seg props clip prio)
  "Draw segment SEG [x1 y1 x2 y2] with box glyphs (braille when diagonal)."
  (let ((x1 (aref seg 0)) (y1 (aref seg 1)) (x2 (aref seg 2)) (y2 (aref seg 3)))
    (cond
     ((< (abs (- x1 x2)) 0.5)
      (let ((col (easel-text--col g x1)))
        (cl-loop for row from (easel-text--row g (min y1 y2)) to (easel-text--row g (- (max y1 y2) 0.01))
                 when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3)))
                 do (easel-text--put g col row ?│ props prio))))
     ((< (abs (- y1 y2)) 0.5)
      (let ((row (easel-text--row g y1)))
        (cl-loop for col from (easel-text--col g (min x1 x2)) to (easel-text--col g (- (max x1 x2) 0.01))
                 when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3)))
                 do (easel-text--put g col row ?─ props prio))))
     (t (easel-text--dot-line g x1 y1 x2 y2 (lambda (_) props) clip)))))

(defun easel-text--inside (bounds x)
  "X, nudged inside the plot when it lies on BOUNDS' right edge."
  (if (< (abs (- x (+ (aref bounds 0) (aref bounds 2)))) 0.005) (- x 0.01) x))

(defun easel-text--marks (g view)
  "Draw VIEW's marks into grid G, clipped to its plot."
  (let* ((b (plist-get view :bounds))
         (clip (vector (easel-text--col g (aref b 0)) (easel-text--row g (aref b 1))
                       (easel-text--col g (+ (aref b 0) (aref b 2) -0.01)) (easel-text--row g (+ (aref b 1) (aref b 3) -0.01))))
         (clip (vector (aref clip 0) (aref clip 1) (1+ (aref clip 2)) (1+ (aref clip 3)))))
    (seq-doseq (mark (plist-get view :marks))
      (seq-do-indexed
       (lambda (item i)
         (unless (equal (plist-get item :opacity) 0)
           (pcase (plist-get mark :mark)
             ((or "line" "area") (easel-text--series g view mark item clip))
             ((or "bar" "rect" "brush") (easel-text--rect g view mark item clip i))
             ((or "rule" "tick")
              ;; A rule on the plot's right edge (the last datum's crosshair)
              ;; belongs to the last column, not the clipped one past it.
              (easel-text--segment g (if (equal (plist-get mark :mark) "rule")
                                         (vector (easel-text--inside b (plist-get item :x1)) (plist-get item :y1)
                                                 (easel-text--inside b (plist-get item :x2)) (plist-get item :y2))
                                       (vector (plist-get item :x1) (plist-get item :y1) (plist-get item :x2) (plist-get item :y2)))
                                   (easel-text--item-props view mark item (plist-get item :datum)) clip 3))
             ("text" (easel-text--string g (plist-get item :x) (plist-get item :y) (plist-get item :text)
                                         (plist-get item :align)
                                         (easel-text--item-props view mark item (plist-get item :datum)) 3))
             (_ (let ((filled (not (equal (plist-get item :fill) "none")))
                      ;; A point on the plot's far edge belongs to the last cell.
                      (col (min (easel-text--col g (plist-get item :x)) (1- (aref clip 2))))
                      (row (min (easel-text--row g (plist-get item :y)) (1- (aref clip 3)))))
                  (when (and (<= (aref clip 0) col) (<= (aref clip 1) row))
                    (easel-text--put g col row
                                     (if (equal (plist-get item :shape) "square") (if filled ?■ ?□) (if filled ?● ?○))
                                     (easel-text--item-props view mark item (plist-get item :datum)) 3)))))))
       (plist-get mark :items)))))

(defun easel-text--axes (g view)
  "Draw VIEW's axes and grid into G."
  (let ((all (vector 0 0 (easel-text--grid-cols g) (easel-text--grid-rows g)))
        (axis-props (list 'face 'easel-axis)) left bottom)
    (seq-doseq (axis (plist-get view :axes))
      (seq-doseq (tk (plist-get axis :ticks))
        (when-let* ((grid (plist-get tk :grid)))
          (easel-text--segment g grid (list 'face 'easel-axis) all 0)
          ;; Grid lines are dotted so marks stay legible.
          (let ((vertical (< (abs (- (aref grid 0) (aref grid 2))) 0.5)))
            (cl-loop for i across (easel-text--grid-prio g) for k from 0
                     when (and (= i 0) (memq (aref (easel-text--grid-chars g) k) '(?│ ?─)))
                     do (aset (easel-text--grid-chars g) k (if vertical ?┊ ?┈)))))))
    (seq-doseq (axis (plist-get view :axes))
      (let ((seg (plist-get axis :domain-line)) (is-bottom (equal (plist-get axis :orient) "bottom")))
        (easel-text--segment g seg axis-props all 4)
        (if is-bottom (setq bottom seg) (setq left seg))
        (seq-doseq (tk (plist-get axis :ticks))
          (let ((ts (plist-get tk :tick)))
            (easel-text--put g (easel-text--col g (aref ts (if is-bottom 0 2))) (easel-text--row g (aref ts (if is-bottom 1 3)))
                             (if is-bottom ?┬ ?┤) axis-props 4))
          (easel-text--string g (plist-get tk :lx) (plist-get tk :ly) (plist-get tk :label) (plist-get tk :align)
                              (list 'face 'easel-label) 5))
        (when-let* ((tm (plist-get axis :title-mark)))
          (easel-text--string g (plist-get tm :x) (plist-get tm :y) (plist-get tm :text) (plist-get tm :align)
                              (list 'face 'easel-title) 5))))
    (when (and left bottom)
      (easel-text--put g (easel-text--col g (aref left 0)) (easel-text--row g (aref bottom 1)) ?└ axis-props 4))))

(defun easel-text--legends (g view)
  "Draw VIEW's legends into G."
  (seq-doseq (legend (plist-get view :legends))
    (when-let* ((tm (plist-get legend :title-mark)))
      (easel-text--string g (plist-get tm :x) (plist-get tm :y) (plist-get tm :text) "left" (list 'face 'easel-title) 5))
    (seq-doseq (e (plist-get legend :entries))
      (let ((props (list 'easel-view (plist-get view :id) 'easel-legend (plist-get e :value)
                         'help-echo (plist-get e :label))))
        (when (plist-get e :color)
          (easel-text--put g (easel-text--col g (plist-get e :sx)) (easel-text--row g (plist-get e :sy))
                           (pcase (plist-get legend :shape) ("square" ?■) ("stroke" ?━) (_ ?●))
                           (append props (list 'face (list :foreground (plist-get e :color)))) 5))
        (easel-text--string g (plist-get e :lx) (plist-get e :ly) (plist-get e :label) "left"
                            (append props (list 'face 'easel-label)) 5)))))

(defun easel-text--compose (g)
  "Return grid G as a propertized string, braille dots merged, lines trimmed."
  (let ((cols (easel-text--grid-cols g)) lines)
    (dotimes (row (easel-text--grid-rows g))
      (let ((runs nil) (chars nil) (props :unset))
        (dotimes (col cols)
          (let* ((i (+ col (* row cols)))
                 (dots (aref (easel-text--grid-dots g) i))
                 (use-dots (and (> dots 0) (<= (aref (easel-text--grid-prio g) i) 2)))
                 (char (if use-dots (+ #x2800 dots) (aref (easel-text--grid-chars g) i)))
                 (p (if use-dots (aref (easel-text--grid-dot-props g) i) (aref (easel-text--grid-props g) i))))
            (unless (equal p props)
              (when chars (push (apply #'propertize (apply #'string (nreverse chars)) (unless (eq props :unset) props)) runs))
              (setq chars nil props p))
            (push char chars)))
        (when chars (push (apply #'propertize (apply #'string (nreverse chars)) (unless (eq props :unset) props)) runs))
        (let ((line (apply #'concat (nreverse runs))))
          (push (if (string-match "[ ]+\\'" line) (substring line 0 (match-beginning 0)) line) lines))))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun easel-text-render (scene)
  "Return SCENE drawn as a propertized string (rows joined by newlines)."
  (let ((g (easel-text--new scene)))
    (seq-doseq (view (plist-get scene :views))
      (easel-text--axes g view)
      (easel-text--marks g view)
      (easel-text--legends g view))
    (when-let* ((title (plist-get scene :title)))
      (easel-text--string g (plist-get title :x) (plist-get title :y) (plist-get title :text) "center"
                          (list 'face 'easel-title) 5))
    (easel-text--compose g)))

(provide 'easel-text)
;;; easel-text.el ends here
