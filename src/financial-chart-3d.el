;;; financial-chart-3d.el --- 3D variants of financial-chart's templates via graphics.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every financial-chart template has a 3D variant in templates3d/:
;; the template with its body unchanged plus an `x-eas.graphics'
;; naming a graphics.el mark (graphics.el's
;; docs/design/eas-integration.md).  Loading this file registers the
;; directory with eas under the namespace "fc3d", so "fc3d/ohlc" is the
;; variant of "ohlc"; eas still draws it in 2D, graphics.el in 3D.
;; scripts/templates3d.py writes the variants; docs/3d.md shows them.
;;
;; graphics.el is an optional dependency.  Without it this file still
;; loads, the variants still draw in 2D, and every 3D command signals
;; `financial-chart-3d-unavailable' saying how to install it.
;;
;;   (financial-chart-3d-open "ohlc" :bindings '(:bars ...) :show t)
;;   M-x financial-chart-3d-gallery     ; every variant, a grid of windows
;;   M-x financial-chart-3d-demo-book   ; the order-book surface, streaming
;;   (financial-chart-3d-book-open SNAPSHOT) + `financial-chart-3d-book-push'
;;
;; The marks graphics.el lacks are registered here with
;; `graphics-eas-define-mark' when a 3D command loads graphics.el (or at
;; load, when graphics.el is already loaded):
;;
;;   fc-candle3d          candle3d's geometry (bodies as deep as their
;;                        volume) with scales over the whole series, so a
;;                        replay adds bars in place instead of rescaling
;;   fc-panes3d           price, volume and RSI panels stacked on one
;;                        time axis
;;   fc-volume-profile3d  candles plus horizontal volume-at-price bars
;;   fc-drawdown3d        the underwater curve hanging below the
;;                        high-water line
;;   fc-payoff3d          a P/L heightfield over price and days to
;;                        expiry (or curve); expiry-only rows are priced
;;                        back in time (`financial-chart-3d-payoff-surface')
;;   fc-depth3d           cumulative depth walls per side
;;   fc-diverging3d       bar3d coloured by sign
;;
;; The series variants replay through graphics.el's time encoding
;; (`encoding.time', mode cumulative): SPC plays or pauses in the 3D
;; buffer.  Headless frames and the goldens show a replay's end.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'financial-chart)
(require 'financial-chart-eas-book)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function graphics-eas-primitives "graphics-eas-marks" (graphics rows &optional params domain-rows))
(declare-function graphics-eas-variant-spec "graphics-eas" (name &optional bindings))
(declare-function graphics-eas-open "graphics-eas" (source &rest args))
(declare-function graphics-eas-frame "graphics-eas" (view cols rows &optional palette))
(declare-function graphics-eas-show "graphics-eas" (view &optional cols rows window))
(declare-function graphics-eas-timeline "graphics-eas" (view))
(declare-function graphics-eas-timeline-set "graphics-eas" (view &rest args))
(declare-function graphics-eas-braille-text "graphics-eas" (frame cols))
(declare-function graphics-eas-ensure-core "graphics-eas" ())
(declare-function graphics-ensure-core "graphics" (&optional core))
(declare-function graphics-frame-write-file "graphics-frame" (path frame cols rows))
(declare-function eas-play-detach "eas-play" (view))
(declare-function eas-stream-attach "eas-stream" (view &optional options))

(defvar graphics-eas-domain-rows)
(defvar graphics-eas-mark-params)

;;; Templates

(defconst financial-chart-3d-namespace "fc3d"
  "The eas namespace of the 3D variants: \"fc3d/ohlc\" is ohlc's.")

(defconst financial-chart-3d-templates-directory
  (expand-file-name "templates3d" financial-chart-root)
  "Directory of the 3D variants of financial-chart's templates.")

(eas-template-add-directory financial-chart-3d-templates-directory financial-chart-3d-namespace)

(defun financial-chart-3d-variants ()
  "Names of the 3D variants (\"ohlc\", ...), one per financial-chart template."
  (mapcar #'file-name-base
          (directory-files financial-chart-3d-templates-directory nil "\\.json\\'")))

(defun financial-chart-3d--name (name)
  "The registry name of variant NAME (\"ohlc\" or \"fc3d/ohlc\")."
  (let ((prefix (concat financial-chart-3d-namespace "/")))
    (if (string-prefix-p prefix name) name (concat prefix name))))

(defun financial-chart-3d--base (name)
  "The financial-chart template variant NAME draws in 3D (\"ohlc\")."
  (string-remove-prefix (concat financial-chart-3d-namespace "/") name))

;;; graphics.el, optionally

(define-error 'financial-chart-3d-unavailable
  "financial-chart: 3D charts need graphics.el" 'financial-chart-error)

(defvar financial-chart-3d-graphics-feature 'graphics-eas
  "The graphics.el feature the 3D commands load.")

(defun financial-chart-3d-available-p ()
  "Non-nil when graphics.el (its eas integration) can be loaded.
Loading it registers this package's marks (`financial-chart-3d-marks')."
  (when (require financial-chart-3d-graphics-feature nil t)
    (financial-chart-3d--register-marks)
    t))

(defun financial-chart-3d--ensure ()
  "Load graphics.el or signal `financial-chart-3d-unavailable'."
  (unless (financial-chart-3d-available-p)
    (signal 'financial-chart-3d-unavailable
            (list (concat "3D charts need graphics.el, which is not on `load-path': clone "
                          "gitlab.com/davidawad/graphics-el, build its Rust module (make module) "
                          "and add its lisp/ to `load-path' (2D charts need none of it)")
                  :code "GRAPHICS_MISSING"
                  :feature (symbol-name financial-chart-3d-graphics-feature)))))

;;; Pure helpers: rows, fields, scales

(defun financial-chart-3d--key (name)
  "Plist keyword for NAME (a string or keyword)."
  (if (keywordp name) name (intern (concat ":" name))))

(defun financial-chart-3d--opt (graphics name &optional default)
  "Option NAME (a string) of GRAPHICS, DEFAULT when absent or null."
  (let ((v (plist-get graphics (financial-chart-3d--key name))))
    (if (or (null v) (eq v :null)) default v)))

(defun financial-chart-3d--field (graphics channel default)
  "The field keyword of CHANNEL in GRAPHICS's encoding, else DEFAULT's."
  (let* ((spec (plist-get (financial-chart-3d--opt graphics "encoding") (financial-chart-3d--key channel)))
         (name (cond ((stringp spec) spec)
                     ((consp spec) (plist-get spec :field))
                     (t default))))
    (financial-chart-3d--key name)))

(defun financial-chart-3d--value (row key)
  "ROW's value at KEY; nil for null, false or missing."
  (let ((v (plist-get row key)))
    (unless (memq v '(:null :false)) v)))

(defun financial-chart-3d--rows (rows)
  "ROWS as a list: a vector or list of plists, or a data/v1 object."
  (if (keywordp (car-safe rows)) (append (plist-get rows :rows) nil) (append rows nil)))

(defun financial-chart-3d--categories (values)
  "Distinct VALUES: sorted when all are numbers, else in first-seen order."
  (let ((seen (delete-dups (copy-sequence (delq nil values)))))
    (if (seq-every-p #'numberp seen) (sort seen #'<) seen)))

(defun financial-chart-3d--r3 (v)
  "V rounded to 3 decimals, as graphics.el's marks round."
  (/ (fround (* v 1000.0)) 1000.0))

(defun financial-chart-3d--domain (rows)
  "ROWS and the rows a time encoding's scales span (`graphics-eas-domain-rows')."
  (if (and (boundp 'graphics-eas-domain-rows) graphics-eas-domain-rows)
      (append graphics-eas-domain-rows rows)
    rows))

(defun financial-chart-3d--mark-params ()
  "The params the mark is lowered under, if any."
  (and (boundp 'graphics-eas-mark-params) graphics-eas-mark-params))

(defun financial-chart-3d--shift (primitives dy prefix)
  "PRIMITIVES (a sequence) raised by DY, their ids prefixed with PREFIX."
  (mapcar
   (lambda (p)
     (let ((p (copy-sequence p)))
       (setq p (plist-put p :id (concat prefix (plist-get p :id))))
       (pcase (plist-get p :type)
         ((or "segments" "boxes")
          (let ((key (if (equal (plist-get p :type) "boxes") :boxes :segments)))
            (plist-put p key (vconcat (mapcar (lambda (s)
                                                (let ((s (copy-sequence s)))
                                                  (aset s 1 (financial-chart-3d--r3 (+ (aref s 1) dy)))
                                                  (aset s 4 (financial-chart-3d--r3 (+ (aref s 4) dy)))
                                                  s))
                                              (plist-get p key))))))
         ("mesh"
          (plist-put p :vertices (vconcat (mapcar (lambda (v) (vector (aref v 0) (financial-chart-3d--r3 (+ (aref v 1) dy))
                                                                      (aref v 2)))
                                                  (plist-get p :vertices)))))
         ("heightfield" (plist-put p :base (financial-chart-3d--r3 (+ (or (plist-get p :base) 0) dy))))
         (_ p))))
   primitives))

(defun financial-chart-3d--axes (id x0 x1 y1 z0 z1 &optional color)
  "Axis segments ID along the back left corner of X0..X1, 0..Y1, Z0..Z1.
COLOR defaults to graphics.el's axis grey."
  (list :id id :type "segments"
        :segments (vector (vector x0 0 z0 x1 0 z0) (vector x0 0 z0 x0 0 z1)
                          (vector x0 0 z0 x0 (financial-chart-3d--r3 y1) z0))
        :color (or color "#d2ddeb")))

;;; Candles on whole-series scales

(defconst financial-chart-3d-up-color "#26a69a" "Rising candle colour.")
(defconst financial-chart-3d-down-color "#ef5350" "Falling candle colour.")

(defun financial-chart-3d--candle-layout (graphics rows)
  "Layout of ROWS' candles under GRAPHICS, over the whole series.
A plist: :keys (the bars' x values in order, from the domain rows and
ROWS), :index (x value -> position), :x0, :lo, :hi, :height, :half,
:dmax, :vmax.  With a time encoding the domain rows are every bar, so
each replay frame keeps the full series' positions and price scale."
  (let* ((xk (financial-chart-3d--field graphics "x" "time"))
         (dk (financial-chart-3d--field graphics "depth" "volume"))
         (dom (financial-chart-3d--domain rows))
         (keys (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (index (make-hash-table :test #'equal))
         (n (max 1 (length keys)))
         (lows (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r :low))) (and (numberp v) v))) dom)))
         (highs (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r :high))) (and (numberp v) v))) dom)))
         (lo (if lows (apply #'min lows) 0.0))
         (hi (if highs (apply #'max highs) 1.0))
         (half (/ (financial-chart-3d--opt graphics "band" 0.6) 2.0)))
    (cl-loop for k in keys for i from 0 do (puthash k i index))
    (list :xk xk :dk dk :keys keys :index index :n n :x0 (/ (1- n) 2.0)
          :lo (float lo) :hi (float (if (> hi lo) hi (1+ lo)))
          :height (float (financial-chart-3d--opt graphics "height" (max 4.0 (* 0.5 n))))
          :half half
          :dmax (float (financial-chart-3d--opt graphics "depth" (max (* 2 half) (* 0.06 n))))
          :vmax (float (apply #'max 0 (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r dk)))
                                                                       (and (numberp v) v)))
                                                        dom)))))))

(defun financial-chart-3d--price-y (layout price)
  "World height of PRICE in candle LAYOUT."
  (financial-chart-3d--r3 (* (- price (plist-get layout :lo))
                             (/ (plist-get layout :height) (- (plist-get layout :hi) (plist-get layout :lo))))))

(defun financial-chart-3d--bar-x (layout row)
  "World x of bar ROW in candle LAYOUT, or nil."
  (when-let* ((i (gethash (financial-chart-3d--value row (plist-get layout :xk)) (plist-get layout :index))))
    (- i (plist-get layout :x0))))

(defun financial-chart-3d--candles (graphics layout rows)
  "Primitives of ROWS' candles in LAYOUT under GRAPHICS: axes, wicks, bodies.
Bodies are as deep as the bar's volume (`depth' channel) when it has
one, as graphics.el's candle3d draws them."
  (let* ((half (plist-get layout :half)) (height (plist-get layout :height))
         (dmax (plist-get layout :dmax)) (vmax (plist-get layout :vmax)) (dk (plist-get layout :dk))
         (x0 (plist-get layout :x0))
         (up (financial-chart-3d--opt graphics "up_color" financial-chart-3d-up-color))
         (down (financial-chart-3d--opt graphics "down_color" financial-chart-3d-down-color))
         bodies wicks colors)
    (cl-loop for r in rows for i from 0 do
             (let ((x (financial-chart-3d--bar-x layout r))
                   (o (financial-chart-3d--value r :open)) (h (financial-chart-3d--value r :high))
                   (l (financial-chart-3d--value r :low)) (c (financial-chart-3d--value r :close)))
               (unless (and (numberp o) (numberp h) (numberp l) (numberp c))
                 (error "Bar %d needs numeric open, high, low and close (financial-chart-3d)" i))
               (when x
                 (let* ((b0 (financial-chart-3d--price-y layout (min o c)))
                        (b1 (max (financial-chart-3d--price-y layout (max o c)) (+ b0 (* 0.004 height))))
                        (v (financial-chart-3d--value r dk))
                        (d (if (and (numberp v) (> vmax 0)) (+ half (* (- dmax half) (/ v vmax))) half)))
                   (push (vector (financial-chart-3d--r3 (- x half)) b0 (financial-chart-3d--r3 (- d))
                                 (financial-chart-3d--r3 (+ x half)) (financial-chart-3d--r3 b1) (financial-chart-3d--r3 d))
                         bodies)
                   (push (vector (financial-chart-3d--r3 x) (financial-chart-3d--price-y layout l) 0
                                 (financial-chart-3d--r3 x) (financial-chart-3d--price-y layout h) 0)
                         wicks)
                   (push (if (>= c o) up down) colors)))))
    (let ((colors (vconcat (nreverse colors))))
      (list (financial-chart-3d--axes "axes" (- -0.5 x0) (+ 0.5 x0) height 0 dmax)
            (list :id "wicks" :type "segments" :segments (vconcat (nreverse wicks)) :colors colors)
            (list :id "bodies" :type "boxes" :boxes (vconcat (nreverse bodies)) :colors colors)))))

(defun financial-chart-3d--candle-mark (graphics rows)
  "Primitives of the fc-candle3d mark: GRAPHICS over bar ROWS."
  (vconcat (financial-chart-3d--candles graphics (financial-chart-3d--candle-layout graphics rows) rows)))

;;; Panes: price, volume and RSI on one time axis

(defun financial-chart-3d--panes-mark (graphics rows)
  "Primitives of the fc-panes3d mark: GRAPHICS over bar ROWS in three panels.
The panels stack upward on one time axis (RSI at the bottom, then
volume, then price), each bar at the same x in all three, as the panes
template stacks them down the page."
  (let* ((layout (financial-chart-3d--candle-layout graphics rows))
         (height (plist-get layout :height))
         (half (plist-get layout :half)) (x0 (plist-get layout :x0))
         (panel (* 0.3 height)) (gap (* 0.12 height))
         (vmax (plist-get layout :vmax))
         (period (financial-chart-3d--opt graphics "rsi" 14))
         (rsi (financial-chart-rsi rows period))
         (left (- -0.5 x0)) (right (+ 0.5 x0))
         volume volume-colors line bands)
    (dolist (r rows)
      (when-let* ((x (financial-chart-3d--bar-x layout r)))
        (let ((v (financial-chart-3d--value r (plist-get layout :dk))))
          (when (and (numberp v) (> vmax 0))
            (push (vector (financial-chart-3d--r3 (- x half)) 0 (financial-chart-3d--r3 (- half))
                          (financial-chart-3d--r3 (+ x half)) (financial-chart-3d--r3 (* panel (/ v vmax)))
                          (financial-chart-3d--r3 half))
                  volume)
            (push (if (>= (financial-chart-3d--value r :close) (financial-chart-3d--value r :open))
                      financial-chart-3d-up-color financial-chart-3d-down-color)
                  volume-colors)))))
    (cl-loop for (a b) on (cl-mapcar #'cons rows rsi) while b
             when (and (cdr a) (cdr b))
             do (push (vector (financial-chart-3d--r3 (financial-chart-3d--bar-x layout (car a)))
                              (financial-chart-3d--r3 (* panel (/ (cdr a) 100.0))) 0
                              (financial-chart-3d--r3 (financial-chart-3d--bar-x layout (car b)))
                              (financial-chart-3d--r3 (* panel (/ (cdr b) 100.0))) 0)
                      line))
    (dolist (level '(30 70))
      (let ((y (financial-chart-3d--r3 (* panel (/ level 100.0)))))
        (push (vector left y 0 right y 0) bands)))
    (vconcat
     (list (financial-chart-3d--axes "rsi-axes" left right panel 0 half)
           (list :id "rsi-bands" :type "segments" :segments (vconcat bands) :color "#5b6b86")
           (list :id "rsi" :type "segments" :segments (vconcat (nreverse line)) :color "#b279a2"))
     (financial-chart-3d--shift
      (list (financial-chart-3d--axes "axes" left right panel 0 half)
            (list :id "bars" :type "boxes" :boxes (vconcat (nreverse volume))
                  :colors (vconcat (nreverse volume-colors))))
      (+ panel gap) "volume-")
     (financial-chart-3d--shift (financial-chart-3d--candles graphics layout rows)
                                (* 2 (+ panel gap)) "price-"))))

;;; Volume profile beside the candles

(defun financial-chart-3d-profile-volumes (bars low high bins)
  "Volume of BARS per price level: BINS equal levels from LOW to HIGH.
Each bar's volume is spread evenly over the part of its low-high range
in each level, as `financial-chart-volume-profile' estimates it; over
the bars' own range the two agree.  A vector of BINS numbers."
  (let* ((step (/ (- high low) (float bins)))
         (volumes (make-vector bins 0.0)))
    (dolist (bar bars)
      (let* ((l (min (plist-get bar :low) (plist-get bar :high)))
             (h (max (plist-get bar :low) (plist-get bar :high)))
             (v (or (financial-chart-3d--value bar :volume) 0)))
        (if (= l h)
            (let ((i (min (1- bins) (max 0 (floor (/ (- l low) step))))))
              (aset volumes i (+ (aref volumes i) v)))
          (dotimes (i bins)
            (let* ((b0 (+ low (* i step)))
                   (overlap (max 0 (- (min h (+ b0 step)) (max l b0)))))
              (when (> overlap 0)
                (aset volumes i (+ (aref volumes i) (* v (/ overlap (- h l)))))))))))
    volumes))

(defun financial-chart-3d--volume-profile-mark (graphics rows)
  "Primitives of the fc-volume-profile3d mark: GRAPHICS over bar ROWS.
ROWS are bars; the profile's `bins' levels span the whole series' price
range, and each level is a horizontal bar right of the last candle, as
long as its volume, the point of control bright and the last close
ruled across."
  (let* ((layout (financial-chart-3d--candle-layout graphics rows))
         (bins (max 1 (financial-chart-3d--opt graphics "bins" 24)))
         (lo (plist-get layout :lo)) (hi (plist-get layout :hi))
         (volumes (financial-chart-3d-profile-volumes rows lo hi bins))
         (vmax (seq-reduce #'max volumes 0.0))
         (poc (and (> vmax 0) (seq-position volumes vmax)))
         (n (plist-get layout :n))
         (start (+ (- (1- n) (plist-get layout :x0)) 1.0))
         (width (max 3.0 (* 0.35 n)))
         (half (plist-get layout :half))
         (step (/ (- hi lo) bins))
         (last-close (financial-chart-3d--value (car (last rows)) :close))
         boxes colors)
    (dotimes (i bins)
      (let ((v (aref volumes i)))
        (when (> v 0)
          (push (vector (financial-chart-3d--r3 start)
                        (financial-chart-3d--price-y layout (+ lo (* i step)))
                        (financial-chart-3d--r3 (- half))
                        (financial-chart-3d--r3 (+ start (* width (/ v vmax))))
                        (financial-chart-3d--r3 (- (financial-chart-3d--price-y layout (+ lo (* (1+ i) step)))
                                                   (* 0.15 (financial-chart-3d--price-y layout (+ lo step)))))
                        (financial-chart-3d--r3 half))
                boxes)
          (push (if (eql i poc) "#ffb74d" "#4c78a8") colors))))
    (vconcat
     (financial-chart-3d--candles graphics layout rows)
     (list (list :id "profile" :type "boxes" :boxes (vconcat (nreverse boxes)) :colors (vconcat (nreverse colors)))
           (list :id "profile-close" :type "segments" :color "#d2ddeb"
                 :segments (if (numberp last-close)
                               (let ((y (financial-chart-3d--price-y layout last-close)))
                                 (vector (vector (financial-chart-3d--r3 start) y 0
                                                 (financial-chart-3d--r3 (+ start width)) y 0)))
                             []))))))

;;; Drawdown: the underwater curve

(defun financial-chart-3d-drawdown-rows (rows xk yk)
  "ROWS with :depth, the percent below the running high of field YK.
Rows without a number at XK or YK are dropped.  Depth is 0 at a new
high and positive below it."
  (let ((peak nil))
    (delq nil (mapcar (lambda (r)
                        (let ((x (financial-chart-3d--value r xk)) (y (financial-chart-3d--value r yk)))
                          (when (and x (numberp y))
                            (setq peak (if peak (max peak y) y))
                            (list :x x :depth (if (> peak 0) (* 100.0 (/ (- peak y) (float peak))) 0.0)))))
                      rows))))

(defun financial-chart-3d--drawdown-mark (graphics rows)
  "Primitives of the fc-drawdown3d mark: GRAPHICS over value ROWS.
The percent drawdown from the running high as an area3d slab, turned
upside down: its flat top is the high-water line and the curve hangs
below it, deepest at the maximum drawdown."
  (let* ((xk (financial-chart-3d--field graphics "x" "x"))
         (yk (financial-chart-3d--field graphics "y" "y"))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (sub (list :mark "area3d" :encoding (list :x "x" :y "depth")
                    :height height :zero t
                    :extent (financial-chart-3d--opt graphics "extent" [12 0])
                    :color (financial-chart-3d--opt graphics "color" "#4c78a8")))
         (domain (and (boundp 'graphics-eas-domain-rows) graphics-eas-domain-rows
                      (financial-chart-3d-drawdown-rows graphics-eas-domain-rows xk yk)))
         (prims (graphics-eas-primitives sub (financial-chart-3d-drawdown-rows rows xk yk)
                                         (financial-chart-3d--mark-params) domain))
         (flip (lambda (y) (financial-chart-3d--r3 (- height y)))))
    (vconcat
     (mapcar (lambda (p)
               (pcase (plist-get p :type)
                 ("mesh" (plist-put (copy-sequence p) :vertices
                                    (vconcat (mapcar (lambda (v) (vector (aref v 0) (funcall flip (aref v 1)) (aref v 2)))
                                                     (plist-get p :vertices)))))
                 ("segments" (plist-put (copy-sequence p) :segments
                                        (vconcat (mapcar (lambda (s) (vector (aref s 0) (funcall flip (aref s 1)) (aref s 2)
                                                                             (aref s 3) (funcall flip (aref s 4)) (aref s 5)))
                                                         (plist-get p :segments)))))
                 (_ p)))
             prims))))

;;; Payoff surface over price and days to expiry

(defun financial-chart-3d--payoff-pairs (payoff)
  "PAYOFF as a price-sorted list of (PRICE . PNL).
PAYOFF is rows {price, pnl} (a vector or list, or data/v1) or (PRICE
PNL) pairs."
  (sort (delq nil (mapcar (lambda (p)
                            (let ((price (if (keywordp (car-safe p)) (plist-get p :price) (car-safe p)))
                                  (pnl (if (keywordp (car-safe p)) (plist-get p :pnl) (car-safe (cdr-safe p)))))
                              (and (numberp price) (numberp pnl) (cons (float price) (float pnl)))))
                          (financial-chart-3d--rows payoff)))
        (lambda (a b) (< (car a) (car b)))))

(defun financial-chart-3d--interpolate (pairs s)
  "The P/L of sorted (PRICE . PNL) PAIRS at price S.
Linear between them and extended along the end segments beyond them."
  (let* ((n (length pairs)) (v (vconcat pairs)))
    (if (= n 1) (cdr (aref v 0))
      (let* ((i (cond ((<= s (car (aref v 0))) 0)
                      ((>= s (car (aref v (1- n)))) (- n 2))
                      (t (cl-loop for i from 0 below (1- n)
                                  when (<= (car (aref v i)) s (car (aref v (1+ i)))) return i))))
             (a (aref v i)) (b (aref v (1+ i)))
             (dx (- (car b) (car a))))
        (if (zerop dx) (cdr a)
          (+ (cdr a) (* (- s (car a)) (/ (- (cdr b) (cdr a)) dx))))))))

(defun financial-chart-3d--round4 (v)
  "V rounded to 4 decimals."
  (/ (fround (* v 10000.0)) 10000.0))

(cl-defun financial-chart-3d-payoff-surface (payoff &key vol days)
  "P/L rows {price, dte, day, pnl} of expiry PAYOFF at earlier DAYS.
PAYOFF is the expiry P/L by price (rows {price, pnl} or (PRICE PNL)
pairs, as the payoff template takes).  Each of DAYS (days to expiry,
default 60 45 30 21 14 7 3 0) prices it back: the expected expiry P/L
when the underlying, now at the row's price, moves lognormally at
annualised volatility VOL (default 0.3) with a zero rate -- the exact
value of a position of same-expiry options and stock, whose expiry P/L
is a function of price alone.  Day 0 is PAYOFF itself.  The expiry P/L
is linear between PAYOFF's prices and along its end segments beyond
them.  Rows run furthest DAYS first, prices ascending; day counts days
elapsed since the furthest."
  (let* ((pairs (financial-chart-3d--payoff-pairs payoff))
         (vol (float (or vol 0.3)))
         (days (append (or days [60 45 30 21 14 7 3 0]) nil))
         (furthest (apply #'max days))
         ;; z from -5 to 5: the normal weights, normalised.
         (zs (cl-loop for i from -40 to 40 collect (/ i 8.0)))
         (ws (let ((raw (mapcar (lambda (z) (exp (* -0.5 z z))) zs)))
               (mapcar (lambda (w) (/ w (apply #'+ raw))) raw))))
    (unless pairs
      (signal 'financial-chart-invalid-data
              (list "payoff surface needs rows {price, pnl}" :code "empty_payoff" :index nil :field "data")))
    (cl-loop for dte in days
             append (let* ((tau (/ dte 365.0)) (sd (* vol (sqrt tau))) (mu (* -0.5 sd sd)))
                      (mapcar (lambda (p)
                                (list :price (car p) :dte dte :day (- furthest dte)
                                      :pnl (financial-chart-3d--round4
                                            (if (<= dte 0) (cdr p)
                                              (cl-loop for z in zs for w in ws
                                                       sum (* w (financial-chart-3d--interpolate
                                                                 pairs (* (car p) (exp (+ mu (* sd z)))))))))))
                              pairs)))))

(defun financial-chart-3d--payoff-mark (graphics rows)
  "Primitives of the fc-payoff3d mark: GRAPHICS over ROWS as a P/L heightfield.
Channels x (price), z (days to expiry, or a curve label) and y (P/L).
Columns are the distinct x values, rows the z values (numbers
furthest first, labels as they come), height the P/L, coloured red to
green over the whole P/L range with the zero (breakeven) level framed.
Rows that carry no z value (the payoff template's expiry rows) are
priced back to the `days' at volatility `vol' first
\(`financial-chart-3d-payoff-surface').  Under a time encoding the
surface spans every row's grid and scale, the rows present so far are
drawn in place and the newest is traced bright, so a replay grows the
surface toward expiry without rescaling."
  (let* ((xk (financial-chart-3d--field graphics "x" "price"))
         (zk (financial-chart-3d--field graphics "z" "dte"))
         (yk (financial-chart-3d--field graphics "y" "pnl"))
         (has-z (seq-some (lambda (r) (financial-chart-3d--value r zk)) rows))
         (rows (if (or has-z (null rows)) rows
                 (progn (setq xk :price zk :dte yk :pnl)
                        (financial-chart-3d-payoff-surface
                         (mapcar (lambda (r) (list (financial-chart-3d--value r xk)
                                                   (financial-chart-3d--value r yk)))
                                 rows)
                         :vol (financial-chart-3d--opt graphics "vol")
                         :days (financial-chart-3d--opt graphics "days")))))
         (dom (if has-z (financial-chart-3d--domain rows) rows))
         (xs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (zs (let ((zs (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom))))
               (if (and zs (seq-every-p #'numberp zs)) (reverse zs) zs)))
         (ys (delq nil (mapcar (lambda (r) (let ((v (financial-chart-3d--value r yk))) (and (numberp v) (float v))))
                               dom)))
         (lo (if ys (apply #'min ys) 0.0))
         (hi (if ys (apply #'max ys) 1.0))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (if (> hi lo) (/ height (- hi lo)) 1.0))
         (base (financial-chart-3d--r3 (- (* lo scale))))
         (extent (financial-chart-3d--opt graphics "extent" [12 9]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (cells (make-hash-table :test #'equal))
         (present nil))
    (dolist (r rows)
      (let ((x (financial-chart-3d--value r xk)) (z (financial-chart-3d--value r zk))
            (y (financial-chart-3d--value r yk)))
        (when (and x z (numberp y))
          (puthash (cons x z) (float y) cells)
          (push z present))))
    (let* ((shown (seq-filter (lambda (z) (member z present)) zs))
           (nx (length xs)) (nz (length zs)) (k (length shown))
           (x-at (lambda (i) (financial-chart-3d--r3 (if (> nx 1) (- (* 2 w (/ i (float (1- nx)))) w) 0))))
           (z-at (lambda (j) (financial-chart-3d--r3 (if (> nz 1) (- (* 2 d (/ j (float (1- nz)))) d) 0))))
           (row-heights (lambda (z) (vconcat (mapcar (lambda (x) (financial-chart-3d--r3 (gethash (cons x z) cells lo)))
                                                     xs))))
           (newest (car (last shown)))
           (zero (and (< lo 0) (> hi 0) base)))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (if (and (>= nx 2) (>= k 2))
           (list :id "surface" :type "heightfield" :x (vector (- w) w)
                 :z (vector (- d) (funcall z-at (1- k)))
                 :base base :y_scale (financial-chart-3d--r3 scale)
                 :heights (vconcat (mapcar row-heights shown))
                 :low_color (financial-chart-3d--opt graphics "low_color" financial-chart-3d-down-color)
                 :high_color (financial-chart-3d--opt graphics "high_color" financial-chart-3d-up-color)
                 :color_range (vector lo hi))
         (list :id "surface" :type "segments" :segments []))
       (list :id "front" :type "segments" :color "#ffffff"
             :segments (if newest
                           (let ((z (funcall z-at (1- k))) (hs (funcall row-heights newest)))
                             (vconcat (cl-loop for i from 0 below (1- nx)
                                               collect (vector (funcall x-at i)
                                                               (financial-chart-3d--r3 (+ base (* scale (aref hs i)))) z
                                                               (funcall x-at (1+ i))
                                                               (financial-chart-3d--r3 (+ base (* scale (aref hs (1+ i))))) z))))
                         []))
       (list :id "zero" :type "segments" :color "#8a94a6"
             :segments (if zero
                           (vector (vector (- w) zero (- d) w zero (- d)) (vector w zero (- d) w zero d)
                                   (vector w zero d (- w) zero d) (vector (- w) zero d (- w) zero (- d)))
                         []))))))

;;; Depth walls and diverging bars

(defun financial-chart-3d-cumulative-depth (rows &optional xk yk sk)
  "Order-book ROWS {side, price, size} as cumulative depth rows.
Each side's size is summed outward from its best price (bids down from
the highest, asks up from the lowest), as the depth template draws
them; rows {price, cumulative, side}, bids then asks, by price.  XK,
YK and SK name the price, size and side fields."
  (let ((xk (or xk :price)) (yk (or yk :size)) (sk (or sk :side)))
    (cl-loop for (side . descending) in '(("bid" . t) ("ask" . nil))
             append (let* ((levels (seq-filter (lambda (r) (and (equal (financial-chart-3d--value r sk) side)
                                                                 (numberp (financial-chart-3d--value r xk))
                                                                 (numberp (financial-chart-3d--value r yk))))
                                               rows))
                           (levels (sort (copy-sequence levels)
                                         (lambda (a b) (funcall (if descending #'> #'<)
                                                                (financial-chart-3d--value a xk)
                                                                (financial-chart-3d--value b xk)))))
                           (total 0.0)
                           (out (mapcar (lambda (r)
                                          (setq total (+ total (financial-chart-3d--value r yk)))
                                          (list :price (float (financial-chart-3d--value r xk))
                                                :cumulative total :side side))
                                        levels)))
                      (if descending (nreverse out) out)))))

(defun financial-chart-3d--depth-mark (graphics rows)
  "Primitives of the fc-depth3d mark: GRAPHICS over order-book ROWS.
An area3d slab per side over price, bids in front and asks behind,
each rising away from the mid."
  (graphics-eas-primitives
   (list :mark "area3d" :encoding (list :x "price" :y "cumulative" :z "side")
         :zero t :height (financial-chart-3d--opt graphics "height" 4)
         :extent (financial-chart-3d--opt graphics "extent" [12 0])
         :colors (financial-chart-3d--opt graphics "colors" '(:bid "#26a69a" :ask "#ef5350")))
   (financial-chart-3d-cumulative-depth rows (financial-chart-3d--field graphics "x" "price")
                                        (financial-chart-3d--field graphics "y" "size")
                                        (financial-chart-3d--field graphics "color" "side"))
   (financial-chart-3d--mark-params)))

(defun financial-chart-3d--diverging-mark (graphics rows)
  "Primitives of the fc-diverging3d mark: GRAPHICS over ROWS {label, value}.
graphics.el's bar3d, gains green and losses red."
  (let ((yk (financial-chart-3d--field graphics "y" "value")))
    (graphics-eas-primitives
     (list :mark "bar3d"
           :encoding (list :x (substring (symbol-name (financial-chart-3d--field graphics "x" "label")) 1)
                           :y (substring (symbol-name yk) 1) :color "sign")
           :colors (list :gain (financial-chart-3d--opt graphics "up_color" financial-chart-3d-up-color)
                         :loss (financial-chart-3d--opt graphics "down_color" financial-chart-3d-down-color)))
     (mapcar (lambda (r)
               (let ((v (financial-chart-3d--value r yk)))
                 (append (list :sign (if (and (numberp v) (< v 0)) "loss" "gain")) r)))
             rows)
     (financial-chart-3d--mark-params))))

(defconst financial-chart-3d-marks
  '(("fc-candle3d" . financial-chart-3d--candle-mark)
    ("fc-panes3d" . financial-chart-3d--panes-mark)
    ("fc-volume-profile3d" . financial-chart-3d--volume-profile-mark)
    ("fc-drawdown3d" . financial-chart-3d--drawdown-mark)
    ("fc-payoff3d" . financial-chart-3d--payoff-mark)
    ("fc-depth3d" . financial-chart-3d--depth-mark)
    ("fc-diverging3d" . financial-chart-3d--diverging-mark))
  "The 3D marks this package adds to graphics.el: (MARK . FUNCTION).")

(defun financial-chart-3d--register-marks ()
  "Register `financial-chart-3d-marks' with graphics.el."
  (dolist (m financial-chart-3d-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; graphics.el loaded first: register now; else when a 3D command loads
;; it (`financial-chart-3d-available-p').
(when (featurep 'graphics-eas-marks)
  (financial-chart-3d--register-marks))
(require 'financial-chart-3d-vol-surface)
(with-eval-after-load 'financial-chart-3d (require 'financial-chart-3d-valuation))

;;; Opening variants

(defun financial-chart-3d-bindings (name &optional bindings)
  "BINDINGS for variant NAME (default its example), completed for 3D.
The payoff variant's `surface' is computed from its `data', `vol' and
`days' when not given (`financial-chart-3d-payoff-surface'), so it
replays toward expiry."
  (let* ((name (financial-chart-3d--name name))
         (bindings (copy-sequence (or bindings (eas-template-example name)))))
    (when (and (equal (financial-chart-3d--base name) "payoff")
               (not (plist-get bindings :surface)))
      (let ((slots (plist-get (plist-get (eas-template-get name) :meta) :slots)))
        (setq bindings
              (plist-put bindings :surface
                         (vconcat (financial-chart-3d-payoff-surface
                                   (plist-get bindings :data)
                                   :vol (or (plist-get bindings :vol) (plist-get (plist-get slots :vol) :default))
                                   :days (or (plist-get bindings :days)
                                             (plist-get (plist-get slots :days) :default))))))))
    bindings))

(defun financial-chart-3d-spec (name &optional bindings)
  "The chart/v1 spec of 3D variant NAME bound to BINDINGS (default its example).
The template resolved by eas plus its x-eas.graphics with slots filled
\(graphics.el's `graphics-eas-variant-spec')."
  (financial-chart-3d--ensure)
  (let ((name (financial-chart-3d--name name)))
    (graphics-eas-variant-spec name (financial-chart-3d-bindings name bindings))))

(cl-defun financial-chart-3d-open (name &key bindings id show)
  "Open 3D variant NAME (\"ohlc\" or \"fc3d/ohlc\") and return its eas view.
BINDINGS default to the variant's example; ID names the view (default
\"fc3d/NAME\").  With SHOW, draw it in a graphics.el buffer.  Signals
`financial-chart-3d-unavailable' without graphics.el."
  (let* ((name (financial-chart-3d--name name))
         (spec (financial-chart-3d-spec name bindings))
         (view (graphics-eas-open spec :id (or id name))))
    (when show (graphics-eas-show view))
    view))

(defun financial-chart-3d--close (view)
  "Close VIEW and its graphics.el buffer, if any."
  (when-let* ((buffer (get-buffer (format "*graphics-eas %s*" (eas-view-id view)))))
    (kill-buffer buffer))
  (eas-play-detach view)
  (eas-view-close view))

(defun financial-chart-3d-seek-end (view)
  "Move VIEW's replay, if it has one, to its end and pause it."
  (when (graphics-eas-timeline view)
    (graphics-eas-timeline-set view :seconds 1.0e6 :playing nil)))

(defun financial-chart-3d-frame (name &optional cols rows palette)
  "The FRAME of 3D variant NAME with its example, at its replay's end.
COLS x ROWS default to 60 x 24 and PALETTE to `dark' (the goldens'),
drawn at the default camera by the loaded core.  Headless."
  (financial-chart-3d--ensure)
  (let ((view (financial-chart-3d-open name :id (format "financial-chart-3d-frame:%s" name))))
    (unwind-protect
        (progn (financial-chart-3d-seek-end view)
               (graphics-eas-frame view (or cols 60) (or rows 24) palette))
      (financial-chart-3d--close view))))

;;; Goldens and the gallery page

(defconst financial-chart-3d-golden-directory
  (expand-file-name "test/golden/3d" financial-chart-root)
  "Where the 3D goldens live: NAME.frame and NAME.txt, 60x24, dark.")

(defun financial-chart-3d-write-goldens (&optional directory names)
  "Write DIRECTORY/NAME.frame and NAME.txt for each 3D variant NAME.
DIRECTORY defaults to `financial-chart-3d-golden-directory', NAMES to
every variant; 60 x 24, dark, the default camera, the Rust core."
  (financial-chart-3d--ensure)
  (require 'graphics-frame)
  (graphics-ensure-core 'rs)
  (let ((directory (or directory financial-chart-3d-golden-directory)))
    (make-directory directory t)
    (dolist (name (or names (financial-chart-3d-variants)))
      (let ((frame (financial-chart-3d-frame name)))
        (graphics-frame-write-file (expand-file-name (concat name ".frame") directory) frame 60 24)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (graphics-eas-braille-text frame 60) nil
                        (expand-file-name (concat name ".txt") directory) nil 'silent))))))

(defun financial-chart-3d--describe (name)
  "Variant NAME's mark, reading and timeline as a plist."
  (let* ((meta (plist-get (eas-template-get (financial-chart-3d--name name)) :meta))
         (graphics (plist-get meta :graphics))
         (doc (plist-get meta :doc)))
    (list :name name :mark (plist-get graphics :mark)
          :reading (and (string-match ": \\(.*?\\)\\.  2D:" doc) (match-string 1 doc))
          :replay (and (plist-get (plist-get graphics :encoding) :time) t)
          :example (plist-get meta :example))))

(defun financial-chart-3d-write-gallery (&optional file directory)
  "Write FILE (default docs/3d.md): every variant with its golden render.
The renders are read from DIRECTORY (default
`financial-chart-3d-golden-directory'); run
`financial-chart-3d-write-goldens' first."
  (let ((file (or file (expand-file-name "docs/3d.md" financial-chart-root)))
        (directory (or directory financial-chart-3d-golden-directory))
        (names (financial-chart-3d-variants)))
    (with-temp-file file
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert "# 3D financial charts (graphics.el)\n\n"
              "Generated by `make golden-3d` (`financial-chart-3d-write-gallery`); do not\n"
              "edit by hand.\n\n"
              "Every financial-chart template has a 3D variant in `templates3d/`: the\n"
              "template with its body unchanged plus an `x-eas.graphics` naming a\n"
              "[graphics.el](https://gitlab.com/davidawad/graphics-el) mark\n"
              "(graphics.el's `docs/design/eas-integration.md`).\n"
              "`src/financial-chart-3d.el` registers them with eas under the namespace\n"
              "`fc3d`, so `fc3d/ohlc` is the variant of `ohlc`. eas still draws each in\n"
              "2D; graphics.el draws it in 3D.\n\n"
              "graphics.el is optional: without it everything 2D works as before and the\n"
              "3D commands signal `financial-chart-3d-unavailable`, saying how to install\n"
              "it.\n\n"
              "```elisp\n"
              "(require 'financial-chart-3d)\n"
              "(financial-chart-3d-open \"ohlc\" :show t)                 ; its example\n"
              "(financial-chart-3d-open \"ohlc\" :bindings '(:bars BARS) :show t)\n"
              "M-x financial-chart-3d-gallery     ; every variant, a grid of windows\n"
              "M-x financial-chart-3d-demo-book   ; the order-book surface, streaming\n"
              "(setq v (financial-chart-3d-book-open SNAPSHOT :show t))  ; live book\n"
              "(financial-chart-3d-book-push v DELTAS)\n"
              "```\n\n"
              "In a 3D buffer the arrows orbit, `+`/`-` zoom and `0` resets; a replay\n"
              "plays and pauses with `SPC`, steps with `,` and `.` and changes speed\n"
              "with `[` and `]`.\n\n"
              "Marks: `bar3d` and `depth3d` are graphics.el's own. The `fc-` marks are\n"
              "registered by `financial-chart-3d.el` with `graphics-eas-define-mark`:\n"
              "candles whose scales span the whole series (so a replay adds bars in\n"
              "place), stacked panes, a volume profile beside candles, the underwater\n"
              "curve, a payoff heightfield, depth walls and sign-coloured bars.\n\n"
              "Each render below is the variant's golden (`test/golden/3d/NAME.txt`): its\n"
              "example at 60x24 cells, dark palette, the default camera, at the end of\n"
              "its replay, drawn by the Rust core. The Braille text keeps the dots, not\n"
              "the colours. `make test-3d` checks every variant against its template and\n"
              "draws its golden bit for bit.\n\n"
              "| variant | mark | replay | 3D reading |\n|---|---|---|---|\n")
      (dolist (name names)
        (let ((d (financial-chart-3d--describe name)))
          (insert (format "| [%s](#%s) | `%s` | %s | %s |\n" name name (plist-get d :mark)
                          (if (plist-get d :replay) "yes" "") (plist-get d :reading)))))
      (dolist (name names)
        (let ((d (financial-chart-3d--describe name)))
          (insert (format "\n## %s\n\n`fc3d/%s`, mark `%s`, example `%s`. %s.\n\n```text\n"
                          name name (plist-get d :mark) (plist-get d :example)
                          (financial-chart-3d--capitalize (plist-get d :reading))))
          (insert-file-contents (expand-file-name (concat name ".txt") directory))
          (goto-char (point-max))
          (insert "```\n"))))))

(defun financial-chart-3d--capitalize (string)
  "STRING with its first letter upper case."
  (if (string-empty-p string) string
    (concat (upcase (substring string 0 1)) (substring string 1))))

;;; The gallery

(defun financial-chart-3d--grid (n)
  "Columns and rows of a grid of N panes, nearest square, wider first."
  (let* ((cols (max 1 (ceiling (sqrt n)))) (rows (max 1 (ceiling n (float cols)))))
    (list cols rows)))

;;;###autoload
(defun financial-chart-3d-gallery (&optional names)
  "Open every 3D variant with its example in a grid of windows.
NAMES default to all of `financial-chart-3d-variants'; with a prefix
argument, read them.  Each window orbits with its own keys; replays
play with SPC."
  (interactive
   (list (and current-prefix-arg
              (completing-read-multiple "Variants: " (financial-chart-3d-variants) nil t))))
  (financial-chart-3d--ensure)
  (graphics-eas-ensure-core)
  (let* ((names (or names (financial-chart-3d-variants)))
         (grid (financial-chart-3d--grid (length names)))
         (cols (car grid)) (rows (cadr grid))
         (window-min-height 2) (window-min-width 8)
         panes)
    (delete-other-windows)
    (let ((row-windows (list (selected-window))))
      (dotimes (_ (1- rows))
        (push (split-window (car row-windows) nil 'below) row-windows))
      (dolist (row (nreverse row-windows))
        (let ((line (list row)))
          (dotimes (_ (1- cols))
            (push (split-window (car line) nil 'right) line))
          (setq panes (append panes (nreverse line))))))
    (balance-windows)
    (dolist (pane (nthcdr (length names) panes)) (delete-window pane))
    (cl-loop for name in names for pane in panes
             do (let ((old (gethash (financial-chart-3d--name name) eas-views)))
                  ;; A gallery opened before: its views go first.
                  (when old (financial-chart-3d--close old))
                  (graphics-eas-show (financial-chart-3d-open name) nil nil pane)))
    (select-window (car panes))))

;;; Live order books in 3D

(defvar financial-chart-3d--books (make-hash-table :test 'equal)
  "Live 3D book views by view id.
Each is a plist (:view :book :levels :step :history :template).")

(defun financial-chart-3d--book-rows (book levels template step)
  "BOOK's LEVELS per side for TEMPLATE, stamped with STEP.
As `financial-chart-book-rows' draws them.
Depth-live rows carry :step, the surface's depth axis."
  (let ((rows (financial-chart-book-rows book :levels levels :flash nil)))
    (if (equal template "depth-live")
        (vconcat (mapcar (lambda (r) (append r (list :step step))) rows))
      rows)))

(defun financial-chart-3d--book-window (live rows)
  "The eas stream window of LIVE given a frame of ROWS."
  (if (equal (plist-get live :template) "depth-live")
      (* (plist-get live :history) (1+ (* 2 (plist-get live :levels))))
    (length rows)))

(cl-defun financial-chart-3d-book-open (snapshot &key (template "depth-live") levels (history 60) id show)
  "Open a live 3D view of order book SNAPSHOT and return it.
TEMPLATE is \"depth-live\" (default: a surface of cumulative depth by
price and step, the newest HISTORY steps) or \"ladder\" (3D bars of the
current book).  SNAPSHOT and LEVELS are as in
`financial-chart-book-open'.  ID names the view; SHOW draws it.  Feed
it with `financial-chart-3d-book-push'."
  (unless (member template financial-chart-book-templates)
    (signal 'financial-chart-invalid-book
            (list (format "No 3D order-book template %S; templates: %s" template
                          (string-join financial-chart-book-templates ", "))
                  :code "UNKNOWN_TEMPLATE" :path "/template")))
  (financial-chart-3d--ensure)
  (require 'eas-stream)
  (when-let* ((old (and id (gethash id financial-chart-3d--books))))
    (when (eq (gethash id eas-views) (plist-get old :view))
      (financial-chart-3d-book-close id)))
  (let* ((book (if (financial-chart-book-p snapshot) snapshot (financial-chart-book-make snapshot)))
         (levels (or levels financial-chart-book-levels))
         (rows (financial-chart-3d--book-rows book levels template 0))
         (view (financial-chart-3d-open template :bindings (list :data rows)
                                        :id (or id (concat "fc3d/book:" template))))
         (live (list :view view :book book :levels levels :step 0 :history history :template template)))
    (eas-stream-attach view (list :max-fps (or financial-chart-book-max-fps
                                               (financial-chart-book-default-fps levels))
                                  :window (financial-chart-3d--book-window live rows)))
    (puthash (eas-view-id view) live financial-chart-3d--books)
    (when show (graphics-eas-show view))
    view))

(defun financial-chart-3d--book-live (view)
  "The live 3D book of VIEW (an id or view), or signal NO_BOOK."
  (let* ((id (if (eas-view-p view) (eas-view-id view) view))
         (live (gethash id financial-chart-3d--books)))
    (unless (and live (eq (gethash id eas-views) (plist-get live :view)))
      (remhash id financial-chart-3d--books)
      (signal 'financial-chart-invalid-book
              (list (format "View %S is not a live 3D order book; open one with financial-chart-3d-book-open" id)
                    :code "NO_BOOK" :path "")))
    live))

(defun financial-chart-3d-book-push (view deltas &optional now)
  "Apply order-book DELTAS to 3D book VIEW at NOW and stream the new frame.
DELTAS are as in `financial-chart-book-push'.  A depth-live view adds
the book as the next step of its surface; a ladder view replaces its
bars, which grow and shrink to the new sizes.  Returns the book's
summary with :step."
  (let* ((live (financial-chart-3d--book-live view))
         (book (plist-get live :book)))
    (financial-chart-book-apply book deltas now)
    (plist-put live :step (1+ (plist-get live :step)))
    (let ((rows (financial-chart-3d--book-rows book (plist-get live :levels) (plist-get live :template)
                                               (plist-get live :step))))
      (eas-stream-attach (plist-get live :view)
                         (list :max-fps (or financial-chart-book-max-fps
                                            (financial-chart-book-default-fps (plist-get live :levels)))
                               :window (financial-chart-3d--book-window live rows)))
      (eas-push (plist-get live :view) rows))
    (append (list :view (eas-view-id (plist-get live :view)) :step (plist-get live :step))
            (financial-chart-book-summary book))))

(defun financial-chart-3d-book-close (view)
  "Stop 3D book VIEW and close it."
  (let ((live (financial-chart-3d--book-live view)))
    (remhash (eas-view-id (plist-get live :view)) financial-chart-3d--books)
    (financial-chart-3d--close (plist-get live :view))))

;;; A deterministic book replay (examples, the demo)

(defun financial-chart-3d--lcg (seed)
  "A deterministic generator of floats in [0, 1) from integer SEED."
  (let ((state seed))
    (lambda ()
      (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
      (/ state 2147483648.0))))

(cl-defun financial-chart-3d-book-deltas (snapshot steps &key (seed 7))
  "Return STEPS batches of deltas that make SNAPSHOT's liquidity breathe.
SNAPSHOT is a plist (:bids [[PRICE SIZE] ...] :asks ...).  Each
batch updates every level of the book: its size follows a slow
wave that travels outward from the mid, plus noise (a generator
seeded with SEED), never below 0.5.  Prices never change, so the book
never crosses.  A list of delta vectors, deterministic."
  (let* ((random (financial-chart-3d--lcg seed))
         (levels (cl-loop for (side . key) in '(("bid" . :bids) ("ask" . :asks))
                          append (cl-loop for l across (vconcat (plist-get snapshot key))
                                          for i from 0
                                          collect (list side (float (elt l 0)) (float (elt l 1)) i)))))
    (cl-loop for step from 1 to steps
             collect (vconcat
                      (mapcar (lambda (l)
                                (pcase-let ((`(,side ,price ,size ,i) l))
                                  (list :op "update" :side side :price price
                                        :size (/ (fround (* 100 (max 0.5 (* size (+ 1.0 (* 0.6 (sin (- (/ step 4.0) (/ i 1.5))))
                                                                                     (* 0.3 (- (funcall random) 0.5)))))))
                                                 100.0))))
                              levels)))))

(cl-defun financial-chart-3d-book-history (snapshot steps &key levels (seed 7))
  "Return depth-live rows of SNAPSHOT replayed through STEPS delta batches.
Step 0 is SNAPSHOT; each later step applies the next batch of
`financial-chart-3d-book-deltas' (SEED).  A vector of
`financial-chart-book-rows' (LEVELS per side), each with :step."
  (let* ((book (financial-chart-book-make snapshot))
         (levels (or levels financial-chart-book-levels))
         (frames (list (financial-chart-3d--book-rows book levels "depth-live" 0))))
    (cl-loop for batch in (financial-chart-3d-book-deltas snapshot steps :seed seed)
             for step from 1
             do (financial-chart-book-apply book batch 0)
             (push (financial-chart-3d--book-rows book levels "depth-live" step) frames))
    (apply #'vconcat (nreverse frames))))

(defun financial-chart-3d--example-snapshot ()
  "The order-book snapshot of examples/order-book/book.json."
  (eas-json-read-file (expand-file-name "examples/order-book/book.json" financial-chart-root)))

;;;###autoload
(defun financial-chart-3d-demo-book (&optional steps interval)
  "Stream the example order book into a live 3D depth surface.
STEPS batches of deltas (default 240), one every INTERVAL seconds
\(default 0.2), from `financial-chart-3d-book-deltas'; killing the
buffer stops it."
  (interactive)
  (let* ((snapshot (financial-chart-3d--example-snapshot))
         (batches (financial-chart-3d-book-deltas snapshot (or steps 240)))
         (view (financial-chart-3d-book-open snapshot :id "fc3d/book-demo" :show t))
         (buffer (get-buffer (format "*graphics-eas %s*" (eas-view-id view))))
         (timer nil))
    (setq timer (run-with-timer
                 0 (or interval 0.2)
                 (lambda ()
                   (if (or (null batches) (not (buffer-live-p buffer)))
                       (cancel-timer timer)
                     (financial-chart-3d-book-push view (pop batches))))))
    view))

;;; Example data

(defun financial-chart-3d--write-json (file object)
  "Write plist OBJECT to FILE as JSON, an array of rows one row per line."
  (with-temp-file file
    (set-buffer-file-coding-system 'utf-8-unix)
    (let ((json-encoding-pretty-print nil))
      (insert "{\n")
      (cl-loop for (key value) on object by #'cddr for first = t then nil
               do (insert (if first "" ",\n") "  " (json-encode (substring (symbol-name key) 1)) ": ")
               (if (and (vectorp value) (> (length value) 0) (keywordp (car-safe (aref value 0))))
                   (progn (insert "[\n")
                          (cl-loop for row across value for i from 0
                                   do (insert (if (> i 0) ",\n" "") "    " (json-encode row)))
                          (insert "\n  ]"))
                 (insert (json-encode value))))
      (insert "\n}\n"))))

(defun financial-chart-3d--iron-condor ()
  "Expiry P/L of a short 90/95/105/110 iron condor for a 2.5 credit, 80 to 120."
  (cl-loop for s from 80 to 120 by 2
           collect (list s (- 2.5 (max 0 (- 95 s)) (- (max 0 (- 90 s))) (max 0 (- s 105)) (- (max 0 (- s 110)))))))

(defun financial-chart-3d-write-examples (&optional directory)
  "Write the 3D variants' own examples into DIRECTORY (examples/3d/).
payoff (the payoff example with its surface), payoff-curves (a short
iron condor at eight dates to expiry) and depth-live (40 steps of the
example book)."
  (let* ((directory (or directory (expand-file-name "examples/3d" financial-chart-root)))
         (payoff (eas-json-read-file (expand-file-name "examples/payoff.data.json" financial-chart-root)))
         (surface (financial-chart-3d-payoff-surface (plist-get payoff :data))))
    (make-directory directory t)
    (financial-chart-3d--write-json
     (expand-file-name "payoff.data.json" directory)
     (list :title "Long straddle, decaying to expiry"
           :data (plist-get payoff :data)
           :surface (vconcat surface)))
    (financial-chart-3d--write-json
     (expand-file-name "payoff-curves.data.json" directory)
     (list :title "Short iron condor by days to expiry"
           :data (vconcat
                  (mapcar (lambda (r)
                            (list :curve (if (zerop (plist-get r :dte)) "expiry"
                                           (format "T-%d" (plist-get r :dte)))
                                  :price (plist-get r :price) :pnl (plist-get r :pnl)))
                          (financial-chart-3d-payoff-surface
                           (financial-chart-3d--iron-condor) :vol 0.25)))))
    (financial-chart-3d--write-json
     (expand-file-name "depth-live.data.json" directory)
     (list :title "Order book, 40 steps"
           :data (financial-chart-3d-book-history (financial-chart-3d--example-snapshot) 39)))))

(provide 'financial-chart-3d)
;;; financial-chart-3d.el ends here
