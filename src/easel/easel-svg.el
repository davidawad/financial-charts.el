;;; easel-svg.el --- scene/v1 -> SVG image with :map hot spots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L5, GUI half.  Draws a scene exactly as compiled; it reads only the
;; scene and the theme and never branches on chart kind.  Per the
;; fc-qx1.14 spike the DOM is consed directly (svg.el's per-node
;; append is quadratic) and every series is a single <path>.
;;
;; The theme is a Vega config object (the JSON bin/chart accepts):
;; background, font, axis.{domain,tick,grid,label,title}Color,
;; legend.{label,title}Color, title.color.  With no theme, GUI frames
;; map it from Emacs faces; batch uses Vega's defaults.
;;
;; `easel-svg-image' adds :map hot spots for discrete items (bars,
;; points, text, legend entries) with help-echo and pointer; continuous
;; series hover by scale inversion through `easel-hit' instead.

;;; Code:

(require 'dom)
(require 'svg)
(require 'easel-core)

(defconst easel-svg-default-theme
  '(:background "white" :font "sans-serif"
    :axis (:domainColor "#888" :tickColor "#888" :gridColor "#ddd" :labelColor "#000" :titleColor "#000")
    :legend (:labelColor "#000" :titleColor "#000")
    :title (:color "#000"))
  "Vega's default look, as a Vega config object.")

(defun easel-svg--face-color (face attribute)
  "FACE's ATTRIBUTE color as a string, or nil when unspecified."
  (let ((c (face-attribute face attribute nil t)))
    (and (stringp c) (not (string-prefix-p "unspecified" c))
         (if (string-prefix-p "#" c) c
           (when-let* ((rgb (color-values c)))
             (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb)))))))

(defun easel-svg-theme-from-faces ()
  "A Vega config object mapped from the current Emacs faces."
  (let ((fg (or (easel-svg--face-color 'default :foreground) "#000"))
        (bg (or (easel-svg--face-color 'default :background) "white"))
        (dim (or (easel-svg--face-color 'shadow :foreground) "#888")))
    (list :background bg :font (or (face-attribute 'default :family nil t) "sans-serif")
          :axis (list :domainColor dim :tickColor dim :gridColor dim :gridOpacity 0.3
                      :labelColor fg :titleColor fg)
          :legend (list :labelColor fg :titleColor fg)
          :title (list :color fg))))

(defun easel-svg--theme (theme)
  "THEME merged over the default, or the face-mapped theme in GUI frames."
  (let ((base (if (and (null theme) (display-graphic-p)) (easel-svg-theme-from-faces)
                easel-svg-default-theme)))
    (cl-loop for (k v) on theme by #'cddr
             do (setq base (easel-plist-put base k (if (and (easel-object-p v) (easel-object-p (plist-get base k)))
                                                       (let ((m (plist-get base k)))
                                                         (cl-loop for (k2 v2) on v by #'cddr
                                                                  do (setq m (easel-plist-put m k2 v2)))
                                                         m)
                                                     v))))
    base))

(defun easel-svg--n (v)
  "Format number V compactly for SVG attributes."
  (if (integerp v) (number-to-string v)
    (let ((s (format "%.2f" v)))
      (replace-regexp-in-string "\\.?0+\\'" "" s))))

(defun easel-svg--escape (text)
  "Escape TEXT for XML."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;")))
   (format "%s" text) t t))

(defun easel-svg--node (tag &rest attrs)
  "DOM node TAG with ATTRS (a plist, nil values dropped) and no children."
  (dom-node tag (cl-loop for (k v) on attrs by #'cddr
                         when v collect (cons (intern (substring (symbol-name k) 1))
                                              (if (numberp v) (easel-svg--n v) (easel-svg--escape v))))))

(defun easel-svg--anchor (align)
  "SVG text-anchor for scene ALIGN."
  (pcase align ("left" "start") ("right" "end") (_ "middle")))

(defun easel-svg--text (text x y size &rest props)
  "A <text> node for TEXT at X Y with font SIZE.
PROPS: :align :baseline :angle :fill :weight."
  (let* ((baseline (plist-get props :baseline))
         (dy (pcase baseline ("top" (* 0.8 size)) ("middle" (* 0.35 size)) (_ 0)))
         (angle (or (plist-get props :angle) 0))
         (node (easel-svg--node 'text :x x :y (+ y (if (zerop angle) dy 0))
                                :dy (unless (zerop angle) (easel-svg--n dy))
                                :font-size size :fill (plist-get props :fill)
                                :font-weight (plist-get props :weight)
                                :text-anchor (easel-svg--anchor (plist-get props :align))
                                :transform (unless (zerop angle)
                                             (format "rotate(%s %s %s)" (easel-svg--n angle)
                                                     (easel-svg--n x) (easel-svg--n y))))))
    (append node (list (easel-svg--escape text)))))

(defun easel-svg--line (seg color &optional width opacity dash)
  "A <line> for SEG [x1 y1 x2 y2] in COLOR."
  (easel-svg--node 'line :x1 (aref seg 0) :y1 (aref seg 1) :x2 (aref seg 2) :y2 (aref seg 3)
                   :stroke color :stroke-width (or width 1) :stroke-opacity opacity
                   :stroke-dasharray (and dash (mapconcat #'easel-svg--n dash ","))))

(defun easel-svg--path (points &optional base)
  "SVG path data through POINTS, closing along BASE reversed when given."
  (concat (mapconcat (lambda (p) (concat (easel-svg--n (aref p 0)) "," (easel-svg--n (aref p 1))))
                     points "L")
          (when base
            (concat "L" (mapconcat (lambda (p) (concat (easel-svg--n (aref p 0)) "," (easel-svg--n (aref p 1))))
                                   (reverse base) "L")
                    "Z"))))

(defun easel-svg--item (mark item)
  "SVG node for ITEM of MARK."
  (let ((fill (plist-get item :fill)) (stroke (plist-get item :stroke))
        (opacity (let ((o (plist-get item :opacity))) (and o (/= o 1) o))))
    (pcase (plist-get mark :mark)
      ((or "bar" "rect" "brush")
       (easel-svg--node 'rect :x (plist-get item :x) :y (plist-get item :y)
                        :width (max 0 (plist-get item :w)) :height (max 0 (plist-get item :h))
                        :fill fill :stroke (unless (equal stroke "none") stroke) :opacity opacity))
      ((or "rule" "tick")
       (easel-svg--line (vector (plist-get item :x1) (plist-get item :y1) (plist-get item :x2) (plist-get item :y2))
                        stroke (plist-get item :strokeWidth) opacity (plist-get item :strokeDash)))
      ("text" (apply #'easel-svg--text (plist-get item :text) (plist-get item :x) (plist-get item :y)
                     (plist-get item :fontSize)
                     (list :align (plist-get item :align) :baseline (plist-get item :baseline) :fill fill)))
      ((or "line" "area")
       (let ((area (plist-get item :base)))
         (easel-svg--node 'path :d (concat "M" (easel-svg--path (plist-get item :points) area))
                          :fill (if area fill "none") :stroke (unless (or area (equal stroke "none")) stroke)
                          :stroke-width (unless area (plist-get item :strokeWidth))
                          :stroke-dasharray (and (plist-get item :strokeDash)
                                                 (mapconcat #'easel-svg--n (plist-get item :strokeDash) ","))
                          :opacity opacity)))
      (_ (let ((r (sqrt (/ (plist-get item :size) float-pi))))
           (if (equal (plist-get item :shape) "square")
               (let ((side (sqrt (plist-get item :size))))
                 (easel-svg--node 'rect :x (- (plist-get item :x) (/ side 2)) :y (- (plist-get item :y) (/ side 2))
                                  :width side :height side :fill fill :stroke (unless (equal stroke "none") stroke)
                                  :opacity opacity))
             (easel-svg--node 'circle :cx (plist-get item :x) :cy (plist-get item :y) :r r
                              :fill fill :stroke (unless (equal stroke "none") stroke)
                              :stroke-width (unless (equal stroke "none") (plist-get item :strokeWidth))
                              :opacity opacity)))))))

(defun easel-svg--axis (axis theme)
  "SVG nodes for placed AXIS under THEME."
  (let* ((at (plist-get theme :axis)) out)
    (seq-doseq (tk (plist-get axis :ticks))
      (when (plist-get tk :grid)
        (push (easel-svg--line (plist-get tk :grid) (plist-get at :gridColor) 1 (plist-get at :gridOpacity)) out)))
    (push (easel-svg--line (plist-get axis :domain-line) (plist-get at :domainColor)) out)
    (seq-doseq (tk (plist-get axis :ticks))
      (push (easel-svg--line (plist-get tk :tick) (plist-get at :tickColor)) out)
      (push (easel-svg--text (plist-get tk :label) (plist-get tk :lx) (plist-get tk :ly) 10
                             :align (plist-get tk :align) :baseline (plist-get tk :baseline)
                             :angle (if (equal (plist-get axis :orient) "bottom")
                                        (plist-get axis :labelAngle) 0)
                             :fill (plist-get at :labelColor))
            out))
    (when-let* ((tm (plist-get axis :title-mark)))
      (push (easel-svg--text (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) 11
                             :align (plist-get tm :align) :baseline (plist-get tm :baseline)
                             :angle (plist-get tm :angle) :weight "bold" :fill (plist-get at :titleColor))
            out))
    (nreverse out)))

(defun easel-svg--gradient-id (legend)
  "Stable id of LEGEND's gradient definition."
  (format "grad-%s-%s" (plist-get legend :channel) (abs (sxhash-equal (plist-get legend :stops)))))

(defun easel-svg--gradient-def (legend)
  "A vertical <linearGradient> for LEGEND's color stops (high values on top)."
  (let* ((stops (plist-get legend :stops)) (n (length stops)))
    (append (easel-svg--node 'linearGradient :id (easel-svg--gradient-id legend) :x1 0 :y1 1 :x2 0 :y2 0)
            (seq-map-indexed (lambda (c i) (easel-svg--node 'stop :offset (/ i (float (max 1 (1- n)))) :stop-color c))
                             stops))))

(defun easel-svg--legend (legend theme)
  "SVG nodes for placed LEGEND under THEME."
  (let ((lt (plist-get theme :legend)) out)
    (when-let* ((tm (plist-get legend :title-mark)))
      (push (easel-svg--text (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) 11 :align "left"
                             :baseline "top" :weight "bold" :fill (plist-get lt :titleColor))
            out))
    (when-let* ((bar (plist-get legend :bar)))
      (push (easel-svg--node 'rect :x (aref bar 0) :y (aref bar 1) :width (aref bar 2) :height (aref bar 3)
                             :fill (format "url(#%s)" (easel-svg--gradient-id legend)))
            out))
    (seq-doseq (e (plist-get legend :entries))
      (when (plist-get e :color)
        (push (pcase (plist-get legend :shape)
                ("square" (easel-svg--node 'rect :x (- (plist-get e :sx) 5) :y (- (plist-get e :sy) 5)
                                           :width 10 :height 10 :fill (plist-get e :color)))
                ("stroke" (easel-svg--line (vector (- (plist-get e :sx) 5) (plist-get e :sy)
                                                   (+ (plist-get e :sx) 5) (plist-get e :sy))
                                           (plist-get e :color) 2))
                (_ (easel-svg--node 'circle :cx (plist-get e :sx) :cy (plist-get e :sy) :r 5
                                    :fill (plist-get e :color))))
              out))
      (push (easel-svg--text (plist-get e :label) (plist-get e :lx) (plist-get e :ly) 10 :align "left"
                             :baseline "middle" :fill (plist-get lt :labelColor))
            out))
    (nreverse out)))

(defun easel-svg-dom (scene &optional theme)
  "Return the SVG DOM for SCENE under THEME (a Vega config plist)."
  (let* ((theme (easel-svg--theme theme))
         (size (plist-get scene :size))
         (children nil) (defs nil))
    (push (easel-svg--node 'rect :width "100%" :height "100%"
                           :fill (or (plist-get theme :background) (plist-get scene :background)))
          children)
    (seq-doseq (view (plist-get scene :views))
      (let* ((b (plist-get view :bounds))
             (clip (concat "clip-" (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (plist-get view :id)))))
        (push (append (easel-svg--node 'clipPath :id clip)
                      (list (easel-svg--node 'rect :x (aref b 0) :y (aref b 1) :width (aref b 2) :height (aref b 3))))
              defs)
        (when-let* ((frame (plist-get view :frame)))
          (push (easel-svg--node 'rect :x (aref b 0) :y (aref b 1) :width (aref b 2) :height (aref b 3)
                                 :fill "none" :stroke (plist-get frame :stroke))
                children))
        (seq-doseq (axis (plist-get view :axes))
          (setq children (append (reverse (easel-svg--axis axis theme)) children)))
        (push (apply #'dom-node 'g (when (eq (plist-get view :clip) t)
                                     (list (cons 'clip-path (format "url(#%s)" clip))))
                     (apply #'append
                            (mapcar (lambda (mark)
                                      (delq nil (mapcar (lambda (item)
                                                          ;; Fully transparent items draw nothing.
                                                          (unless (equal (plist-get item :opacity) 0)
                                                            (easel-svg--item mark item)))
                                                        (plist-get mark :items))))
                                    (plist-get view :marks))))
              children)
        (seq-doseq (legend (plist-get view :legends))
          (when (plist-get legend :bar) (push (easel-svg--gradient-def legend) defs))
          (setq children (append (reverse (easel-svg--legend legend theme)) children)))))
    (when-let* ((title (plist-get scene :title)))
      (push (easel-svg--text (plist-get title :text) (plist-get title :x) (plist-get title :y)
                             (plist-get title :fontSize) :align "center" :baseline "top" :weight "bold"
                             :fill (plist-get (plist-get theme :title) :color))
            children))
    (apply #'dom-node 'svg
           `((xmlns . "http://www.w3.org/2000/svg")
             (width . ,(easel-svg--n (plist-get size :w))) (height . ,(easel-svg--n (plist-get size :h)))
             (viewBox . ,(format "0 0 %s %s" (easel-svg--n (plist-get size :w)) (easel-svg--n (plist-get size :h))))
             (font-family . ,(easel-svg--escape (or (plist-get theme :font) "sans-serif"))))
           (cons (apply #'dom-node 'defs nil (nreverse defs)) (nreverse children)))))

(defun easel-svg-render (scene &optional theme)
  "Return SCENE drawn as an SVG string under THEME."
  (with-temp-buffer
    (svg-print (easel-svg-dom scene theme))
    (buffer-string)))

;;; Hot spots

(defun easel-svg--tooltip-text (tooltip)
  "Render TOOLTIP pairs as \"title: value\" lines."
  (mapconcat (lambda (p) (format "%s: %s" (plist-get p :title) (plist-get p :value))) tooltip "\n"))

(defun easel-svg-hot-spots (scene)
  "Image :map areas for SCENE's discrete items and legend entries.
Each area id is a symbol easel:VIEW|MARK|ITEM (or easel-legend:VIEW|CHANNEL|I)."
  (let (areas)
    (seq-doseq (view (plist-get scene :views))
      (seq-doseq (mark (plist-get view :marks))
        (when (member (plist-get mark :mark) '("bar" "rect" "point" "circle" "square" "text"))
          (seq-do-indexed
           (lambda (item i)
             (let ((id (intern (format "easel:%s|%s|%d" (plist-get view :id) (plist-get mark :id) i)))
                   (props (list 'help-echo (and (plist-get item :tooltip) (easel-svg--tooltip-text (plist-get item :tooltip)))
                                'pointer (if (plist-get item :href) 'hand 'arrow))))
               (push (list (if (plist-member item :w)
                               (cons 'rect (cons (cons (round (plist-get item :x)) (round (plist-get item :y)))
                                                 (cons (round (+ (plist-get item :x) (max 1 (plist-get item :w))))
                                                       (round (+ (plist-get item :y) (max 1 (plist-get item :h)))))))
                             (cons 'circle (cons (cons (round (plist-get item :x)) (round (plist-get item :y)))
                                                 (max 3 (round (sqrt (/ (or (plist-get item :size) 30) float-pi)))))))
                           id props)
                     areas)))
           (plist-get mark :items))))
      (seq-doseq (legend (plist-get view :legends))
        (seq-do-indexed
         (lambda (e i)
           (let ((b (plist-get e :bounds)))
             (push (list (cons 'rect (cons (cons (round (aref b 0)) (round (aref b 1)))
                                           (cons (round (+ (aref b 0) (aref b 2))) (round (+ (aref b 1) (aref b 3))))))
                         (intern (format "easel-legend:%s|%s|%d" (plist-get view :id) (plist-get legend :channel) i))
                         (list 'help-echo (plist-get e :label) 'pointer 'hand))
                   areas)))
         (plist-get legend :entries))))
    (nreverse areas)))

(defun easel-svg-image (scene &rest props)
  "Return an image descriptor for SCENE with :map hot spots.
PROPS may include :theme and any image property (:scale, :ascent)."
  (let* ((theme (plist-get props :theme))
         (img-props (append (list :map (easel-svg-hot-spots scene))
                            (easel--plist-without props :theme)
                            (unless (plist-member props :ascent) (list :ascent 'center))))
         (data (easel-svg-render scene theme)))
    ;; `create-image' signals in a batch NS Emacs ("Window system frame
    ;; should be used"); the plain descriptor is equivalent for callers.
    (if (and (display-images-p) (image-type-available-p 'svg))
        (apply #'create-image data 'svg t img-props)
      (append (list 'image :type 'svg :data data) img-props))))

(provide 'easel-svg)
;;; easel-svg.el ends here
