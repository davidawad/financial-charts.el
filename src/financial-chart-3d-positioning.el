;;; financial-chart-3d-positioning.el --- 3D options positioning: OI, GEX, skew and VRP -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Options positioning in 3D, four variants in templates3d/ (each the
;; 3D reading of its 2D template in templates/financial/):
;;
;;   options-oi    open interest by strike x expiry (opt.chain_stats),
;;                 calls up and puts down, spot a plane through the
;;                 strike axis, each expiry's max pain (opt.max_pain)
;;                 a gold post
;;   options-gex   dealer gamma by strike x expiry (opt.gex), the same
;;                 reading, plus the gamma flip as a second plane
;;   options-skew  25d risk-reversal and butterfly term structures
;;                 (opt.skew_rr25_bf25, or opt.positioning_skew) as
;;                 ribbons over tenor, one per snapshot; replays
;;   options-vrp   implied against realized volatility (opt.vrp,
;;                 stats.realized_vol) as one ribbon filled by the
;;                 premium's sign, IV rank (opt.iv_rank) and the
;;                 front/back IV ratio (opt.calendar_iv_ratio) beside
;;                 it; replays
;;
;; This package never fetches data: each template takes the rows the
;; named invest-metrics metric emits (docs/3d-positioning.md, the data
;; contracts).  The marks are registered with graphics.el when it loads
;; (`graphics-eas-define-mark'), as financial-chart-3d.el registers its
;; own:
;;
;;   fc-strikes3d  bars up and down from a strike by expiry floor, with
;;                 spot and gamma-flip planes and max-pain posts
;;   fc-skew3d     term-structure ribbons per snapshot, panels stacked
;;   fc-vrp3d      the IV/RV ribbon, IV-rank bars, IV-ratio line

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-3d)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))

(defconst financial-chart-3d-positioning-call-color "#26a69a" "Calls and call gamma.")
(defconst financial-chart-3d-positioning-put-color "#ef5350" "Puts and put gamma.")
(defconst financial-chart-3d-positioning-spot-color "#ffffff" "The spot plane.")
(defconst financial-chart-3d-positioning-flip-color "#b279a2" "The gamma-flip plane.")
(defconst financial-chart-3d-positioning-max-pain-color "#ffb74d" "Max-pain posts.")
(defconst financial-chart-3d-positioning-grid-color "#344769" "Floor lines.")

;;; Pure helpers

(defun financial-chart-3d-positioning--number (row key)
  "ROW's number at KEY as a float, or nil."
  (let ((v (financial-chart-3d--value row key)))
    (and (numberp v) (float v))))

(defun financial-chart-3d-positioning--numbers (rows key)
  "The numbers at KEY of ROWS."
  (delq nil (mapcar (lambda (r) (financial-chart-3d-positioning--number r key)) rows)))

(defun financial-chart-3d-positioning-max-pain (rows &optional strike-key expiry-key call-key put-key)
  "Max-pain strike of each expiry of open-interest ROWS.
ROWS are {expiry, strike, call_oi, put_oi} (STRIKE-KEY, EXPIRY-KEY,
CALL-KEY and PUT-KEY name other fields).  Max pain is the strike, of
those the expiry lists, at which its options expiring are worth least
to their holders: the sum over strikes K of call OI x max(0, S - K)
plus put OI x max(0, K - S) is least at S; ties go to the lower
strike.  Rows (:expiry E :max_pain K), expiries in first-seen order."
  (let* ((sk (or strike-key :strike)) (ek (or expiry-key :expiry))
         (ck (or call-key :call_oi)) (pk (or put-key :put_oi))
         (rows (financial-chart-3d--rows rows)))
    (delq nil
          (mapcar
           (lambda (expiry)
             (let* ((chain (seq-filter (lambda (r) (and (equal (financial-chart-3d--value r ek) expiry)
                                                        (financial-chart-3d-positioning--number r sk)))
                                       rows))
                    (strikes (sort (delete-dups (financial-chart-3d-positioning--numbers chain sk)) #'<))
                    (pain (lambda (s)
                            (cl-loop for r in chain
                                     for k = (financial-chart-3d-positioning--number r sk)
                                     sum (+ (* (or (financial-chart-3d-positioning--number r ck) 0) (max 0 (- s k)))
                                            (* (or (financial-chart-3d-positioning--number r pk) 0) (max 0 (- k s))))))))
               (when strikes
                 (let ((best (car strikes)) (least (funcall pain (car strikes))))
                   (dolist (s (cdr strikes))
                     (let ((p (funcall pain s)))
                       (when (< p least) (setq best s least p))))
                   (list :expiry expiry :max_pain (if (= best (ftruncate best)) (truncate best) best))))))
           (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r ek)) rows))))))

(defun financial-chart-3d-positioning-net-by-strike (rows &optional strike-key up-key down-key)
  "ROWS' net exposure by strike: ((STRIKE . NET) ...), strikes ascending.
NET sums |UP-KEY| - |DOWN-KEY| (call_gex and put_gex by default) over
the rows at the strike, every expiry together."
  (let ((sk (or strike-key :strike)) (uk (or up-key :call_gex)) (dk (or down-key :put_gex))
        (net nil))
    (dolist (r (financial-chart-3d--rows rows))
      (when-let* ((k (financial-chart-3d-positioning--number r sk)))
        (let ((cell (assoc k net)))
          (unless cell (setq cell (cons k 0.0)) (push cell net))
          (setcdr cell (+ (cdr cell) (abs (or (financial-chart-3d-positioning--number r uk) 0))
                          (- (abs (or (financial-chart-3d-positioning--number r dk) 0))))))))
    (sort net (lambda (a b) (< (car a) (car b))))))

(defun financial-chart-3d-positioning-gamma-flip (rows &optional spot strike-key up-key down-key)
  "The gamma flip of GEX ROWS: where net gamma by strike changes sign.
Net gamma by strike is `financial-chart-3d-positioning-net-by-strike'
\(STRIKE-KEY, UP-KEY, DOWN-KEY); each sign change between neighbouring
strikes is interpolated linearly, and the one nearest SPOT (the
lowest without SPOT) is returned, rounded to 2 decimals.  nil when net
gamma never changes sign."
  (let* ((net (financial-chart-3d-positioning-net-by-strike rows strike-key up-key down-key))
         (flips (cl-loop for (a b) on net while b
                         for na = (cdr a) for nb = (cdr b)
                         when (or (and (< na 0) (> nb 0)) (and (> na 0) (< nb 0)))
                         collect (+ (car a) (* (- (car b) (car a)) (/ na (- na nb))))
                         when (and (= na 0) (/= nb 0) (not (eq a (car net)))) collect (car a))))
    (when flips
      (let ((best (if (numberp spot)
                      (car (sort flips (lambda (x y) (< (abs (- x spot)) (abs (- y spot))))))
                    (car flips))))
        (/ (fround (* best 100.0)) 100.0)))))

(defun financial-chart-3d-positioning--place (keys value)
  "World x of numeric VALUE among sorted numeric KEYS placed at 0, 1, ...
centred on 0: linear between neighbouring keys and along the end
steps beyond them."
  (let* ((v (vconcat keys)) (n (length v)) (x0 (/ (1- n) 2.0)))
    (cond ((= n 0) 0.0)
          ((= n 1) 0.0)
          (t (let ((i (cond ((<= value (aref v 0)) 0)
                            ((>= value (aref v (1- n))) (- n 2))
                            (t (cl-loop for i from 0 below (1- n)
                                        when (<= (aref v i) value (aref v (1+ i))) return i)))))
               (- (+ i (/ (- value (aref v i)) (float (- (aref v (1+ i)) (aref v i))))) x0))))))

(defun financial-chart-3d-positioning--ramp (from to u)
  "The colour U (0 to 1) of the way from colour FROM to TO."
  (let ((rgb (lambda (c) (mapcar (lambda (i) (string-to-number (substring c i (+ i 2)) 16)) '(1 3 5))))
        (u (max 0.0 (min 1.0 u))))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (round (+ x (* u (- y x))))) (funcall rgb from) (funcall rgb to)))))

(defun financial-chart-3d-positioning--frame (id x0 y0 z0 x1 y1 z1 color)
  "Segments ID outlining the rectangle from X0 Y0 Z0 to X1 Y1 Z1 in COLOR.
The rectangle is in a plane of constant x (X0 = X1) or of constant y
\(Y0 = Y1)."
  (let* ((r #'financial-chart-3d--r3)
         (corners (if (= x0 x1)
                      (list (vector x0 y0 z0) (vector x0 y1 z0) (vector x0 y1 z1) (vector x0 y0 z1))
                    (list (vector x0 y0 z0) (vector x1 y0 z0) (vector x1 y0 z1) (vector x0 y0 z1)))))
    (list :id id :type "segments" :color color
          :segments (vconcat (cl-loop for (a b) on (append corners (list (car corners))) while b
                                      collect (vconcat (mapcar r a) (mapcar r b)))))))

;;; Strikes by expiry: OI and GEX

(defun financial-chart-3d-positioning--strikes-mark (graphics rows)
  "Primitives of the fc-strikes3d mark: GRAPHICS over strike ROWS.
Channels x (strike), z (expiry), up (calls: call_oi, call_gex) and
down (puts).  Each row is a column on the strike by expiry floor: |up|
rises above it and |down| hangs below it, on one scale.  Options:
`spot' (a plane through the strike axis), `flip' (t: the gamma flip,
`financial-chart-3d-positioning-gamma-flip'; or a price), `max_pain' (rows
{expiry, max_pain}) or `max_pain_auto' (compute them,
`financial-chart-3d-positioning-max-pain'), `height' (of the tallest
column, default 3), `band' (column width in strike steps, default
0.5), `spacing' (between expiries, default 2.4) and `depth' (column
depth, a fraction of the spacing, default 0.3)."
  (let* ((xk (financial-chart-3d--field graphics "x" "strike"))
         (zk (financial-chart-3d--field graphics "z" "expiry"))
         (uk (financial-chart-3d--field graphics "up" "call_oi"))
         (dk (financial-chart-3d--field graphics "down" "put_oi"))
         (dom (financial-chart-3d--domain rows))
         (strikes (sort (delete-dups (financial-chart-3d-positioning--numbers dom xk)) #'<))
         (expiries (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom)))
         (nx (max 1 (length strikes))) (nz (max 1 (length expiries)))
         (x0 (/ (1- nx) 2.0))
         (gap (float (financial-chart-3d--opt graphics "spacing" 2.4)))
         (zc (/ (* gap (1- nz)) 2.0))
         (z-at (lambda (e) (- (* gap (or (cl-position e expiries :test #'equal) 0)) zc)))
         (top (apply #'max 1e-9 (mapcar #'abs (append (financial-chart-3d-positioning--numbers dom uk)
                                                       (financial-chart-3d-positioning--numbers dom dk)))))
         (height (float (financial-chart-3d--opt graphics "height" 3)))
         (scale (/ height top))
         (half (/ (financial-chart-3d--opt graphics "band" 0.5) 2.0))
         (zhalf (* (/ (financial-chart-3d--opt graphics "depth" 0.3) 2.0) gap))
         (left (- -0.5 x0)) (right (+ 0.5 x0))
         (back (- (- zc) (* 0.5 gap))) (front (+ zc (* 0.5 gap)))
         (r #'financial-chart-3d--r3)
         (spot (let ((s (financial-chart-3d--opt graphics "spot"))) (and (numberp s) s)))
         (flip (let ((f (financial-chart-3d--opt graphics "flip")))
                 (cond ((numberp f) f)
                       ((eq f t) (financial-chart-3d-positioning-gamma-flip
                                  dom spot xk uk dk)))))
         (max-pain (let ((given (financial-chart-3d--opt graphics "max_pain")))
                     (cond ((and given (financial-chart-3d--rows given)))
                           ((eq (financial-chart-3d--opt graphics "max_pain_auto") t)
                            (financial-chart-3d-positioning-max-pain dom xk zk uk dk)))))
         boxes colors grid posts tops path)
    (dolist (row rows)
      (let ((k (financial-chart-3d-positioning--number row xk)) (e (financial-chart-3d--value row zk)))
        (when (and k e)
          (let ((x (financial-chart-3d-positioning--place strikes k)) (z (funcall z-at e)))
            (cl-loop for (key sign color) in `((,uk 1 ,financial-chart-3d-positioning-call-color)
                                               (,dk -1 ,financial-chart-3d-positioning-put-color))
                     for v = (financial-chart-3d-positioning--number row key)
                     when (and v (/= v 0))
                     do (let ((y (* sign scale (abs v))))
                          (push (vector (funcall r (- x half)) (funcall r (min 0 y)) (funcall r (- z zhalf))
                                        (funcall r (+ x half)) (funcall r (max 0 y)) (funcall r (+ z zhalf)))
                                boxes)
                          (push color colors)))))))
    ;; The floor: its outline and a line under each expiry's lane.
    (dolist (e expiries)
      (let ((z (funcall r (funcall z-at e))))
        (push (vector left 0 z right 0 z) grid)))
    (dolist (m max-pain)
      (let ((k (financial-chart-3d-positioning--number m :max_pain)) (e (financial-chart-3d--value m :expiry)))
        (when (and k (member e expiries))
          (let* ((x (funcall r (financial-chart-3d-positioning--place strikes k)))
                 (z (funcall r (funcall z-at e)))
                 (y (funcall r (* 1.18 height))) (c 0.16))
            (push (vector x 0 z x y z) posts)
            (push (vector (funcall r (- x c)) (funcall r (- y c)) (funcall r (- z c))
                          (funcall r (+ x c)) (funcall r (+ y c)) (funcall r (+ z c)))
                  tops)
            (push (vector x y z) path)))))
    (let ((path (nreverse path)))
      (vconcat
       (list (list :id "axes" :type "segments" :color "#d2ddeb"
                   :segments (vector (vector left 0 back right 0 back) (vector left 0 back left 0 front)
                                     (vector left (funcall r (- height)) back left (funcall r height) back)))
             (financial-chart-3d-positioning--frame "floor" left 0 back right 0 front
                                                financial-chart-3d-positioning-grid-color)
             (list :id "lanes" :type "segments" :color financial-chart-3d-positioning-grid-color
                   :segments (vconcat (nreverse grid)))
             (list :id "bars" :type "boxes" :boxes (vconcat (nreverse boxes)) :colors (vconcat (nreverse colors))))
       (when spot
         (let ((x (funcall r (financial-chart-3d-positioning--place strikes spot))))
           (list (financial-chart-3d-positioning--frame "spot" x (- height) back x height front
                                                    financial-chart-3d-positioning-spot-color))))
       (when flip
         (let ((x (funcall r (financial-chart-3d-positioning--place strikes flip))))
           (list (financial-chart-3d-positioning--frame "flip" x (- height) back x height front
                                                    financial-chart-3d-positioning-flip-color))))
       (when posts
         (list (list :id "max-pain" :type "segments" :color financial-chart-3d-positioning-max-pain-color
                     :segments (vconcat (nreverse posts)
                                        (cl-loop for (a b) on path while b collect (vconcat a b))))
               (list :id "max-pain-tops" :type "boxes" :boxes (vconcat (nreverse tops))
                     :color financial-chart-3d-positioning-max-pain-color)))))))

;;; Skew term structure over time

(defun financial-chart-3d-positioning--sort-values (values)
  "Distinct VALUES sorted: numbers ascending, else as strings (ISO dates)."
  (sort (financial-chart-3d--categories values)
        (lambda (a b) (if (and (numberp a) (numberp b)) (< a b) (string< (format "%s" a) (format "%s" b))))))

(defun financial-chart-3d-positioning--skew-mark (graphics rows)
  "Primitives of the fc-skew3d mark: GRAPHICS over skew snapshot ROWS.
Channels x (the snapshot's date, evenly spaced in order) and z (tenor,
one lane each, shortest in front); `panels' names the value fields
\(default rr25 and bf25), one panel each, stacked top to bottom.  In
each panel every tenor is a ribbon over time at its value, short
tenors warm and long ones cool, the zero level framed, and the newest
snapshot's term structure traced bright across the tenors.  Under a
time encoding the panels span every snapshot's dates and values, so a
replay grows the ribbons in place.  Options: `extent' ([width depth],
default [12 5]), `height' (of a panel, default 2.4) and `band' (ribbon
width in lane steps, default 0.4)."
  (let* ((xk (financial-chart-3d--field graphics "x" "date"))
         (zk (financial-chart-3d--field graphics "z" "tenor"))
         (panels (mapcar #'financial-chart-3d--key
                         (append (financial-chart-3d--opt graphics "panels" ["rr25" "bf25"]) nil)))
         (dom (financial-chart-3d--domain rows))
         (dates (financial-chart-3d-positioning--sort-values (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (tenors (financial-chart-3d-positioning--sort-values (mapcar (lambda (r) (financial-chart-3d--value r zk)) dom)))
         (extent (financial-chart-3d--opt graphics "extent" [12 5]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (nd (length dates)) (nt (length tenors))
         (x-at (lambda (date) (let ((i (or (cl-position date dates :test #'equal) 0)))
                                (if (> nd 1) (- (* 2 w (/ i (float (1- nd)))) w) 0.0))))
         ;; The shortest tenor in front (+z), the longest at the back.
         (z-at (lambda (tenor) (let ((j (or (cl-position tenor tenors :test #'equal) 0)))
                                 (if (> nt 1) (- d (* 2 d (/ j (float (1- nt))))) 0.0))))
         (zhalf (* 0.5 (financial-chart-3d--opt graphics "band" 0.4) (if (> nt 1) (/ (* 2 d) (1- nt)) 1.0)))
         (height (float (financial-chart-3d--opt graphics "height" 2.4)))
         (gap (* 0.5 height))
         (np (length panels))
         (newest (car (last (financial-chart-3d-positioning--sort-values
                             (mapcar (lambda (r) (financial-chart-3d--value r xk)) rows)))))
         (r #'financial-chart-3d--r3)
         (out nil))
    (cl-loop
     for field in panels for p from 0
     do (let* ((base (* (- np 1 p) (+ height gap)))
               (values (financial-chart-3d-positioning--numbers dom field))
               (lo (apply #'min 0.0 values)) (hi (apply #'max 0.0 values))
               (span (if (> hi lo) (- hi lo) 1.0))
               (y-at (lambda (v) (funcall r (+ base (* height (/ (- v lo) span))))))
               (zero (funcall y-at 0.0))
               (name (substring (symbol-name field) 1))
               verts tris tri-colors lines line-colors front (n 0))
          (dolist (tenor tenors)
            (let* ((points (sort (delq nil (mapcar (lambda (row)
                                                     (when-let* (((equal (financial-chart-3d--value row zk) tenor))
                                                                 (v (financial-chart-3d-positioning--number row field))
                                                                 (date (financial-chart-3d--value row xk)))
                                                       (list (funcall x-at date) (funcall y-at v) date)))
                                                   rows))
                                 (lambda (a b) (< (car a) (car b)))))
                   (z (funcall z-at tenor))
                   (u (if (> nt 1) (/ (cl-position tenor tenors :test #'equal) (float (1- nt))) 0.0))
                   (color (financial-chart-3d-positioning--ramp "#f58518" "#4c78a8" u))
                   (first n))
              (dolist (pt points)
                (push (vector (funcall r (car pt)) (nth 1 pt) (funcall r (- z zhalf))) verts)
                (push (vector (funcall r (car pt)) (nth 1 pt) (funcall r (+ z zhalf))) verts)
                (cl-incf n 2)
                (when (equal (nth 2 pt) newest)
                  (push (vector (funcall r (car pt)) (nth 1 pt) (funcall r z)) front)))
              (cl-loop for i from first below (- n 2) by 2 do
                       (push (vector i (1+ i) (+ i 2)) tris)
                       (push (vector (1+ i) (+ i 3) (+ i 2)) tris)
                       (push color tri-colors) (push color tri-colors))
              (cl-loop for (a b) on points while b do
                       (push (vector (funcall r (car a)) (nth 1 a) (funcall r z) (funcall r (car b)) (nth 1 b) (funcall r z))
                             lines)
                       (push color line-colors))))
          (setq front (nreverse front))
          (setq out
                (append out
                        (list (list :id (concat name "-axes") :type "segments" :color "#d2ddeb"
                                    :segments (vector (vector (funcall r (- w)) (funcall r base) (funcall r (- d))
                                                              (funcall r (- w)) (funcall r (+ base height)) (funcall r (- d)))
                                                      (vector (funcall r (- w)) (funcall r base) (funcall r (- d))
                                                              (funcall r w) (funcall r base) (funcall r (- d)))))
                              (financial-chart-3d-positioning--frame (concat name "-zero") (funcall r (- w)) zero (funcall r (- d))
                                                                 (funcall r w) zero (funcall r d) "#8a94a6")
                              (list :id (concat name "-ribbons") :type "mesh" :vertices (vconcat (nreverse verts))
                                    :triangles (vconcat (nreverse tris)) :colors (vconcat (nreverse tri-colors)))
                              (list :id (concat name "-lines") :type "segments" :segments (vconcat (nreverse lines))
                                    :colors (vconcat (nreverse line-colors)))
                              (list :id (concat name "-newest") :type "segments" :color "#ffffff"
                                    :segments (vconcat (cl-loop for (a b) on front while b collect (vconcat a b)))))))))
    (vconcat out)))

;;; Implied against realized volatility

(defun financial-chart-3d-positioning-vrp-rows (rows &optional iv-key rv-key)
  "ROWS with :vrp, implied minus realized volatility, where missing.
IV-KEY and RV-KEY name the fields (default iv and rv).  Rows without
both numbers keep no :vrp."
  (let ((ik (or iv-key :iv)) (rk (or rv-key :rv)))
    (mapcar (lambda (r)
              (let ((iv (financial-chart-3d-positioning--number r ik)) (rv (financial-chart-3d-positioning--number r rk)))
                (if (or (financial-chart-3d-positioning--number r :vrp) (not (and iv rv))) r
                  (append r (list :vrp (/ (fround (* 100 (- iv rv))) 100.0))))))
            (financial-chart-3d--rows rows))))

(defun financial-chart-3d-positioning--vrp-mark (graphics rows)
  "Primitives of the fc-vrp3d mark: GRAPHICS over daily volatility ROWS.
Channel x is the date (evenly spaced, in order); fields `iv' and `rv'
\(encoding channels iv and rv) are implied and realized volatility on
one scale.  The middle lane is one slab between the two, its top edge
IV and bottom edge RV, filled green where IV is above RV (the premium
an option seller earns) and red where RV overtakes it.  Behind it,
when the rows carry iv_rank (0 to 100), a bar per day as tall as the
rank; in front, when they carry calendar_ratio (front over back
implied volatility), its line on its own scale with 1.0
\(backwardation above it) ruled.  Options: `extent' ([width depth],
default [12 4.5]) and `height' (default 4)."
  (let* ((xk (financial-chart-3d--field graphics "x" "date"))
         (ik (financial-chart-3d--field graphics "iv" "iv"))
         (rk (financial-chart-3d--field graphics "rv" "rv"))
         (nk (financial-chart-3d--field graphics "rank" "iv_rank"))
         (ck (financial-chart-3d--field graphics "ratio" "calendar_ratio"))
         (dom (financial-chart-3d--domain rows))
         (dates (financial-chart-3d--categories (mapcar (lambda (r) (financial-chart-3d--value r xk)) dom)))
         (n (length dates))
         (extent (financial-chart-3d--opt graphics "extent" [12 4.5]))
         (w (/ (float (elt extent 0)) 2)) (lane (/ (float (elt extent 1)) 3))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (x-at (lambda (date) (let ((i (or (cl-position date dates :test #'equal) 0)))
                                (if (> n 1) (- (* 2 w (/ i (float (1- n)))) w) 0.0))))
         (step (if (> n 1) (/ (* 2 w) (1- n)) 1.0))
         (vols (append (financial-chart-3d-positioning--numbers dom ik) (financial-chart-3d-positioning--numbers dom rk)))
         (vlo (if vols (apply #'min vols) 0.0)) (vhi (if vols (apply #'max vols) 1.0))
         (vlo (- vlo (* 0.15 (max 1e-9 (- vhi vlo)))))
         (vspan (if (> vhi vlo) (- vhi vlo) 1.0))
         (r #'financial-chart-3d--r3)
         (y-at (lambda (v) (funcall r (* height (/ (- v vlo) vspan)))))
         (ratios (financial-chart-3d-positioning--numbers dom ck))
         (has-rank (financial-chart-3d-positioning--numbers dom nk))
         (z-main 0.0) (z-rank (- lane)) (z-ratio lane)
         (half (* 0.22 lane))
         (rows (seq-filter (lambda (row) (and (financial-chart-3d--value row xk)
                                              (financial-chart-3d-positioning--number row ik)
                                              (financial-chart-3d-positioning--number row rk)))
                           rows))
         (points (sort (mapcar (lambda (row) (list (funcall x-at (financial-chart-3d--value row xk))
                                                   (financial-chart-3d-positioning--number row ik)
                                                   (financial-chart-3d-positioning--number row rk)))
                               rows)
                       (lambda (a b) (< (car a) (car b)))))
         verts tris tri-colors (nv 0) iv-line rv-line rank-boxes rank-colors ratio-line ratio-colors)
    ;; The slab, a segment at a time, split where IV crosses RV.
    (let ((add (lambda (pts color)
                 ;; PTS: a polygon of (X . Y), drawn front and back.
                 (let ((first nv) (k (length pts)))
                   (dolist (z (list (- z-main half) (+ z-main half)))
                     (dolist (pt pts)
                       (push (vector (funcall r (car pt)) (funcall r (cdr pt)) (funcall r z)) verts)
                       (cl-incf nv)))
                   (dotimes (face 2)
                     (cl-loop for i from 1 below (1- k)
                              do (push (vector (+ first (* face k)) (+ first (* face k) i) (+ first (* face k) i 1)) tris)
                              (push color tri-colors))))))
          (green financial-chart-3d-positioning-call-color) (red financial-chart-3d-positioning-put-color))
      (cl-loop for (a b) on points while b
               do (pcase-let* ((`(,x0 ,i0 ,r0) a) (`(,x1 ,i1 ,r1) b)
                               (d0 (- i0 r0)) (d1 (- i1 r1)))
                    (if (or (and (>= d0 0) (>= d1 0)) (and (<= d0 0) (<= d1 0)))
                        (funcall add (list (cons x0 (funcall y-at i0)) (cons x1 (funcall y-at i1))
                                           (cons x1 (funcall y-at r1)) (cons x0 (funcall y-at r0)))
                                 (if (>= (+ d0 d1) 0) green red))
                      (let* ((u (/ d0 (- d0 d1)))
                             (xc (+ x0 (* u (- x1 x0))))
                             (yc (funcall y-at (+ i0 (* u (- i1 i0))))))
                        (funcall add (list (cons x0 (funcall y-at i0)) (cons xc yc) (cons x0 (funcall y-at r0)))
                                 (if (> d0 0) green red))
                        (funcall add (list (cons xc yc) (cons x1 (funcall y-at i1)) (cons x1 (funcall y-at r1)))
                                 (if (> d1 0) green red)))))
               (push (vector (funcall r (car a)) (funcall y-at (nth 1 a)) (funcall r (+ z-main half))
                             (funcall r (car b)) (funcall y-at (nth 1 b)) (funcall r (+ z-main half)))
                     iv-line)
               (push (vector (funcall r (car a)) (funcall y-at (nth 2 a)) (funcall r (+ z-main half))
                             (funcall r (car b)) (funcall y-at (nth 2 b)) (funcall r (+ z-main half)))
                     rv-line)))
    ;; IV rank behind: a bar per day, up to half the height at rank 100.
    (when has-rank
      (dolist (row rows)
        (when-let* ((rank (financial-chart-3d-positioning--number row nk)))
          (let ((x (funcall x-at (financial-chart-3d--value row xk))) (bh (* 0.3 step)))
            (push (vector (funcall r (- x bh)) 0 (funcall r (- z-rank half))
                          (funcall r (+ x bh)) (funcall r (max 0.01 (* 0.5 height (/ (min 100 (max 0 rank)) 100.0))))
                          (funcall r (+ z-rank half)))
                  rank-boxes)
            (push (financial-chart-3d-positioning--ramp "#2c3e7a" "#ffb74d" (/ rank 100.0)) rank-colors)))))
    ;; The front/back ratio in front, on its own scale: 1.0 ruled.
    (when ratios
      (let* ((lo (min 1.0 (apply #'min ratios))) (hi (max 1.0 (apply #'max ratios)))
             (span (if (> hi lo) (- hi lo) 1.0))
             (ry (lambda (v) (funcall r (* 0.45 height (/ (- v lo) span)))))
             (pts (sort (delq nil (mapcar (lambda (row)
                                            (when-let* ((v (financial-chart-3d-positioning--number row ck)))
                                              (cons (funcall x-at (financial-chart-3d--value row xk)) v)))
                                          rows))
                        (lambda (a b) (< (car a) (car b))))))
        (cl-loop for (a b) on pts while b
                 do (push (vector (funcall r (car a)) (funcall ry (cdr a)) (funcall r z-ratio)
                                  (funcall r (car b)) (funcall ry (cdr b)) (funcall r z-ratio))
                          ratio-line)
                 (push (if (> (+ (cdr a) (cdr b)) 2.0) financial-chart-3d-positioning-put-color "#9ecae9") ratio-colors))
        (push (vector (funcall r (- w)) (funcall ry 1.0) (funcall r z-ratio) (funcall r w) (funcall ry 1.0) (funcall r z-ratio))
              ratio-line)
        (push "#8a94a6" ratio-colors)))
    (let ((back (funcall r (* -1.5 lane))) (front (funcall r (* 1.5 lane))))
      (vconcat
       (list (list :id "axes" :type "segments" :color "#d2ddeb"
                   :segments (vector (vector (funcall r (- w)) 0 back (funcall r w) 0 back)
                                     (vector (funcall r (- w)) 0 back (funcall r (- w)) 0 front)
                                     (vector (funcall r (- w)) 0 back (funcall r (- w)) (funcall r height) back)))
             (list :id "premium" :type "mesh" :vertices (vconcat (nreverse verts))
                   :triangles (vconcat (nreverse tris)) :colors (vconcat (nreverse tri-colors)))
             (list :id "iv" :type "segments" :color "#ffffff" :segments (vconcat (nreverse iv-line)))
             (list :id "rv" :type "segments" :color "#9ecae9" :segments (vconcat (nreverse rv-line))))
       (when has-rank
         (list (list :id "iv-rank" :type "boxes" :boxes (vconcat (nreverse rank-boxes))
                     :colors (vconcat (nreverse rank-colors)))))
       (when ratios
         (list (list :id "iv-ratio" :type "segments" :segments (vconcat (nreverse ratio-line))
                     :colors (vconcat (nreverse ratio-colors)))))))))

;;; Registration

(defun financial-chart-3d-positioning--vrp-replay (graphics rows)
  "The fc-vrp3d mark replayed day by day (`financial-chart-3d--replay')."
  (financial-chart-3d--replay graphics rows "date" #'financial-chart-3d-positioning--vrp-mark))

(defconst financial-chart-3d-positioning-marks
  '(("fc-strikes3d" . financial-chart-3d-positioning--strikes-mark)
    ("fc-skew3d" . financial-chart-3d-positioning--skew-mark)
    ("fc-vrp3d" . financial-chart-3d-positioning--vrp-replay))
  "The options-positioning 3D marks: (MARK . FUNCTION).")

(defun financial-chart-3d-positioning--register-marks ()
  "Register `financial-chart-3d-positioning-marks' with graphics.el."
  (dolist (m financial-chart-3d-positioning-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;; graphics.el loaded already: register now; else when a 3D command
;; loads it.
(with-eval-after-load 'graphics-eas-marks
  (financial-chart-3d-positioning--register-marks))

(provide 'financial-chart-3d-positioning)
;;; financial-chart-3d-positioning.el ends here
