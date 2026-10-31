;;; financial-chart-3d-vol-surface.el --- Implied-volatility surfaces in 3D via graphics.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The vol-surface template (templates/financial/vol-surface.json) and
;; its 3D variant (templates3d/vol-surface.json, "fc3d/vol-surface"):
;; an option chain's implied volatility by strike and days to expiry.
;; In 2D a heatmap of the latest quote time; in 3D a heightfield,
;; strikes across, expiries in depth, IV up, that replays the session
;; quote time by quote time (graphics.el's `encoding.time', mode
;; "frame", the surface tweening between snapshots) while the camera
;; sways.  docs/3d-vol-surface.md has the data contract.
;;
;;   (financial-chart-3d-vol-surface-rows CHAIN)   ; snapshots -> rows
;;   (financial-chart-3d-vol-surface-open CHAIN :show t)
;;
;; The mark, registered with `graphics-eas-define-mark' when graphics.el
;; loads:
;;
;;   fc-vol-surface3d  the IV heightfield on the whole session's strike,
;;                     expiry and IV scales, coloured low to high, the
;;                     at-the-money term structure traced across it
;;
;; financial-chart-3d.el loads this file; it needs nothing of graphics.el
;; to load, and its rows and 2D template need none at all.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart)

(declare-function graphics-eas-define-mark "graphics-eas-marks" (mark function))
(declare-function graphics-eas--ramp "graphics-eas-marks" (stops v))
(declare-function eas-json-read-file "eas-core" (file))
(declare-function financial-chart-3d-open "financial-chart-3d" (name &rest args))
(declare-function financial-chart-3d--write-json "financial-chart-3d" (file object))
(declare-function financial-chart-3d--field "financial-chart-3d" (graphics channel default))
(declare-function financial-chart-3d--opt "financial-chart-3d" (graphics name &optional default))
(declare-function financial-chart-3d--value "financial-chart-3d" (row key))
(declare-function financial-chart-3d--categories "financial-chart-3d" (values))
(declare-function financial-chart-3d--domain "financial-chart-3d" (rows))
(declare-function financial-chart-3d--r3 "financial-chart-3d" (v))
(declare-function financial-chart-3d--axes "financial-chart-3d" (id x0 x1 y1 z0 z1 &optional color))

;;; Rows from chain snapshots

(defun financial-chart-3d--vol-get (object key)
  "OBJECT's value at KEY, nil for null or missing."
  (let ((v (plist-get object key)))
    (unless (memq v '(:null :false)) v)))

(defun financial-chart-3d--vol-days (time expiry)
  "Calendar days from quote TIME to EXPIRY (ISO 8601 date strings), or nil."
  (when (and (stringp time) (stringp expiry)
             (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" time)
             (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" expiry))
    (let ((day (lambda (s) (time-to-days (encode-time (list 0 0 12
                                                           (string-to-number (substring s 8 10))
                                                           (string-to-number (substring s 5 7))
                                                           (string-to-number (substring s 0 4))
                                                           nil -1 t))))))
      (- (funcall day expiry) (funcall day time)))))

(defun financial-chart-3d--vol-snapshots (chain)
  "CHAIN's snapshots as a list.
CHAIN is a plist with :snapshots, or a vector or list of snapshots."
  (append (if (keywordp (car-safe chain)) (plist-get chain :snapshots) chain) nil))

(defun financial-chart-3d--vol-iv-surface-p (object)
  "Non-nil if OBJECT is one `opt.iv_surface' result: a plist with :iv."
  (and (keywordp (car-safe object)) (plist-member object :iv) (plist-member object :strikes)))

(defun financial-chart-3d--vol-series-p (chain)
  "Non-nil if CHAIN is a sequence of {time, surface} pairs."
  (and (sequencep chain) (not (stringp chain)) (not (keywordp (car-safe chain)))
       (> (length chain) 0)
       (let ((first (elt chain 0))) (and (keywordp (car-safe first)) (plist-member first :surface)))))

(defun financial-chart-3d--vol-now ()
  "The local time now, ISO 8601 to the second."
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defun financial-chart-3d--vol-surface-snapshot (surface time index)
  "SURFACE, one `opt.iv_surface' result, as a chain snapshot at TIME.
SURFACE is {underlying_price?, strikes, moneyness?, days_to_expiry,
iv: [[IV per strike] per expiry]} with null for a missing IV.  The spot
is `underlying_price', else the mean of strike / moneyness.  INDEX
names the snapshot in errors."
  (let* ((strikes (append (financial-chart-3d--vol-get surface :strikes) nil))
         (moneyness (append (financial-chart-3d--vol-get surface :moneyness) nil))
         (dtes (append (financial-chart-3d--vol-get surface :days_to_expiry) nil))
         (grid (append (financial-chart-3d--vol-get surface :iv) nil))
         (spot (or (financial-chart-3d--vol-get surface :underlying_price)
                   (let ((spots (cl-loop for k in strikes for m in moneyness
                                         when (and (numberp k) (numberp m) (> m 0)) collect (/ k (float m)))))
                     (and spots (/ (apply #'+ spots) (length spots)))))))
    (unless (and strikes (seq-every-p #'numberp strikes))
      (financial-chart--invalid index "strikes" "missing_strike" "the surface needs numeric strikes"))
    (unless (and dtes (seq-every-p #'numberp dtes))
      (financial-chart--invalid index "days_to_expiry" "missing_dte" "the surface needs numeric days_to_expiry"))
    (unless (numberp spot)
      (financial-chart--invalid index "underlying_price" "missing_spot"
                                "the surface needs underlying_price, or moneyness beside its strikes"))
    (unless (and (= (length grid) (length dtes))
                 (seq-every-p (lambda (row) (and (sequencep row) (= (length row) (length strikes)))) grid))
      (financial-chart--invalid index "iv" "bad_shape" "iv must be %d expiries of %d strikes"
                                (length dtes) (length strikes)))
    (list :time time :spot spot
          :chain (vconcat (cl-loop for dte in dtes for row in grid
                                   nconc (cl-loop for k in strikes for iv across (vconcat row)
                                                  collect (list :strike k :dte dte :iv iv)))))))

(defun financial-chart-3d--vol-normalize (chain time)
  "CHAIN's snapshots as a list, whatever shape CHAIN has.
CHAIN is chain snapshots (see `financial-chart-3d-vol-surface-rows'),
one `opt.iv_surface' result (at TIME, or its own `time', or now), or a
sequence of {time, surface} pairs."
  (cond
   ((financial-chart-3d--vol-iv-surface-p chain)
    (list (financial-chart-3d--vol-surface-snapshot
           chain (or time (financial-chart-3d--vol-get chain :time) (financial-chart-3d--vol-now)) 0)))
   ((financial-chart-3d--vol-series-p chain)
    (cl-loop for pair in (append chain nil) for i from 0
             for at = (financial-chart-3d--vol-get pair :time)
             for surface = (financial-chart-3d--vol-get pair :surface)
             do (unless (stringp at)
                  (financial-chart--invalid i "time" "missing_time" "each pair needs a time (an ISO 8601 string)"))
             (unless (financial-chart-3d--vol-iv-surface-p surface)
               (financial-chart--invalid i "surface" "missing_surface" "each pair needs an opt.iv_surface surface"))
             collect (financial-chart-3d--vol-surface-snapshot surface at i)))
   (t (financial-chart-3d--vol-snapshots chain))))

(cl-defun financial-chart-3d-vol-surface-rows (chain &key time)
  "Rows {time, spot, strike, dte, expiry, iv} of option CHAIN, a vector.
CHAIN is any of:

- {snapshots: [{time, spot, chain: [{strike, expiry, dte, iv}
  ...]} ...]} (as the fixture test/fixtures/3d/vol-surface/spy-chain.json
  holds it), or just its snapshots.  A snapshot's quotes may be under
  `chain' or `quotes'; a quote without `dte' gets the calendar days from
  the snapshot's time to its `expiry'.
- One result of `invest metric run opt.iv_surface', verbatim:
  {underlying_price?, strikes, moneyness, days_to_expiry, iv: [[IV per
  strike] per expiry]}, one snapshot at TIME (default its own `time',
  else now).  Without `underlying_price' the spot is strike / moneyness.
- A sequence of {time, surface} pairs, SURFACE such a result: the
  snapshots of a replay.

Quotes without an IV (null) are left out: a chain's far strikes often
have none.  IV is a fraction (0.18 is 18%).  Rows are by quote time,
then days to expiry, then strike.  Signals `financial-chart-invalid-data'
naming the snapshot and field of the first malformed one."
  (let ((rows nil))
    (cl-loop
     for snap in (financial-chart-3d--vol-normalize chain time) for i from 0 do
     (let ((time (financial-chart-3d--vol-get snap :time))
           (spot (financial-chart-3d--vol-get snap :spot))
           (quotes (or (financial-chart-3d--vol-get snap :chain) (financial-chart-3d--vol-get snap :quotes))))
       (unless (stringp time)
         (financial-chart--invalid i "time" "missing_time" "snapshot needs a quote time (an ISO 8601 string)"))
       (unless (numberp spot)
         (financial-chart--invalid i "spot" "missing_spot" "snapshot needs the underlying's spot price"))
       (seq-doseq (q (or quotes []))
         (let* ((strike (financial-chart-3d--vol-get q :strike))
                (expiry (financial-chart-3d--vol-get q :expiry))
                (dte (or (financial-chart-3d--vol-get q :dte) (financial-chart-3d--vol-days time expiry)))
                (iv (financial-chart-3d--vol-get q :iv)))
           (unless (numberp strike)
             (financial-chart--invalid i "strike" "missing_strike" "every quote needs a numeric strike"))
           (unless (numberp dte)
             (financial-chart--invalid i "dte" "missing_dte" "every quote needs dte or an ISO 8601 expiry date"))
           (when (numberp iv)
             (unless (> iv 0)
               (financial-chart--invalid i "iv" "bad_iv" "IV must be positive, got %S" iv))
             (push (list :time time :spot spot :strike strike :dte dte
                         :expiry (or expiry :null) :iv iv)
                   rows))))))
    (unless rows
      (financial-chart--invalid nil "snapshots" "empty_chain" "the chain has no quotes with an IV"))
    (vconcat (sort (nreverse rows)
                   (lambda (a b)
                     (let ((ta (plist-get a :time)) (tb (plist-get b :time)))
                       (cond ((not (equal ta tb)) (string< ta tb))
                             ((/= (plist-get a :dte) (plist-get b :dte)) (< (plist-get a :dte) (plist-get b :dte)))
                             (t (< (plist-get a :strike) (plist-get b :strike))))))))))

;;; The mark

(defconst financial-chart-3d-vol-surface-low-color "#3b528b"
  "Colour of the surface's lowest IV (viridis' blue).")

(defconst financial-chart-3d-vol-surface-mid-color "#21918c"
  "Colour of the wire mesh's middle IV (viridis' teal).")

(defconst financial-chart-3d-vol-surface-high-color "#fde725"
  "Colour of the surface's highest IV (viridis' yellow).")

(defun financial-chart-3d--vol-fill (values)
  "VALUES (a vector, nil where missing) with the gaps filled.
Each gap is linear between its neighbours, or the nearest value at an
end; all missing stays all nil."
  (let* ((n (length values))
         (known (cl-loop for i below n when (aref values i) collect i))
         (out (copy-sequence values)))
    (when known
      (dotimes (i n)
        (unless (aref out i)
          (let ((before (seq-filter (lambda (k) (< k i)) known))
                (after (seq-find (lambda (k) (> k i)) known)))
            (setq before (car (last before)))
            (aset out i (cond ((and before after)
                               (+ (aref values before)
                                  (* (/ (- i before) (float (- after before)))
                                     (- (aref values after) (aref values before)))))
                              (before (aref values before))
                              (t (aref values after))))))))
    out))

(defconst financial-chart-3d-vol-surface-max-strikes 41
  "Most strikes the wire mesh draws (option `max_strikes').")

(defun financial-chart-3d--vol-columns (n most)
  "Indices of N strikes the mesh draws: all, or MOST evenly spread.
The first and the last are always among them."
  (if (or (<= n most) (< most 2))
      (number-sequence 0 (1- n))
    (delete-dups (cl-loop for k below most collect (round (* k (1- n)) (1- most))))))

(defun financial-chart-3d--vol-at (strikes ivs spot)
  "SPOT's place among STRIKES (a list) and its IV from IVS (a vector).
A cons (POSITION . IV): POSITION is SPOT's fractional index among the
strikes, clamped to their range."
  (let* ((v (vconcat strikes)) (n (length v)))
    (cond ((or (< n 2) (<= spot (aref v 0))) (cons 0.0 (aref ivs 0)))
          ((>= spot (aref v (1- n))) (cons (float (1- n)) (aref ivs (1- n))))
          (t (cl-loop for i below (1- n)
                      when (<= (aref v i) spot (aref v (1+ i)))
                      return (let ((u (/ (- spot (aref v i)) (float (- (aref v (1+ i)) (aref v i))))))
                               (cons (+ i u) (+ (aref ivs i) (* u (- (aref ivs (1+ i)) (aref ivs i)))))))))))

(defun financial-chart-3d--vol-surface-mark (graphics rows)
  "Primitives of the fc-vol-surface3d mark: GRAPHICS over chain ROWS.
Channels x (strike), z (days to expiry) and y (IV).  Columns are the
distinct strikes, rows the distinct expiries, nearest at the back; the
height is the IV, coloured `low_color' to `high_color' over the whole
IV range.  A strike an expiry lacks is interpolated along its smile.
The at-the-money IV of each expiry (at the rows' `spot') is traced
across the surface: the term structure.  Under a time encoding the
grid and scales span every snapshot (`graphics-eas-domain-rows'), so
the surface moves in place from one quote time to the next."
  (let* ((xk (financial-chart-3d--field graphics "x" "strike"))
         (zk (financial-chart-3d--field graphics "z" "dte"))
         (yk (financial-chart-3d--field graphics "y" "iv"))
         (num (lambda (r k) (let ((v (financial-chart-3d--value r k))) (and (numberp v) v))))
         (rows (seq-filter (lambda (r) (and (funcall num r xk) (funcall num r zk) (funcall num r yk))) rows))
         (dom (seq-filter (lambda (r) (and (funcall num r xk) (funcall num r zk) (funcall num r yk)))
                          (financial-chart-3d--domain rows)))
         (strikes (financial-chart-3d--categories (mapcar (lambda (r) (funcall num r xk)) dom)))
         (dtes (financial-chart-3d--categories (mapcar (lambda (r) (funcall num r zk)) dom)))
         (ivs (mapcar (lambda (r) (float (funcall num r yk))) dom))
         (lo (if ivs (apply #'min ivs) 0.0))
         (hi (if ivs (apply #'max ivs) 1.0))
         (height (float (financial-chart-3d--opt graphics "height" 4)))
         (scale (if (> hi lo) (/ height (- hi lo)) 1.0))
         (base (financial-chart-3d--r3 (- (* lo scale))))
         (extent (financial-chart-3d--opt graphics "extent" [12 9]))
         (w (/ (float (elt extent 0)) 2)) (d (/ (float (elt extent 1)) 2))
         (nx (length strikes)) (nz (length dtes))
         (x-at (lambda (i) (financial-chart-3d--r3 (+ (- w) (* 2 w (/ i (float (max 1 (1- nx)))))))))
         ;; The nearest expiry at the back (-z), where its skew stands
         ;; up behind the flatter longer expiries; the furthest in front.
         (z-at (lambda (j) (financial-chart-3d--r3 (+ (- d) (* 2 d (/ j (float (max 1 (1- nz)))))))))
         (y-at (lambda (iv) (financial-chart-3d--r3 (* scale (- iv lo)))))
         (stops (list (financial-chart-3d--opt graphics "low_color" financial-chart-3d-vol-surface-low-color)
                      (financial-chart-3d--opt graphics "mid_color" financial-chart-3d-vol-surface-mid-color)
                      (financial-chart-3d--opt graphics "high_color" financial-chart-3d-vol-surface-high-color)))
         (color (lambda (iv) (graphics-eas--ramp stops (if (> hi lo) (/ (- iv lo) (- hi lo)) 0.5))))
         (cells (make-hash-table :test #'equal))
         (spots (make-hash-table :test #'eql)))
    (dolist (r rows)
      (puthash (cons (funcall num r xk) (funcall num r zk)) (float (funcall num r yk)) cells)
      (when-let* ((s (funcall num r :spot)))
        (puthash (funcall num r zk) (float s) spots)))
    (let* ((smiles (mapcar (lambda (dte)
                             (financial-chart-3d--vol-fill
                              (vconcat (mapcar (lambda (k) (gethash (cons k dte) cells)) strikes))))
                           dtes))
           (complete (and (>= nx 2) (>= nz 2) (seq-every-p (lambda (s) (aref s 0)) smiles)))
           (atm (and complete
                     (cl-loop for dte in dtes for smile in smiles for j from 0
                              for spot = (gethash dte spots)
                              when spot collect (let ((p (financial-chart-3d--vol-at strikes smile spot)))
                                                  (vector (financial-chart-3d--r3
                                                           (+ (- w) (* 2 w (/ (car p) (float (1- nx))))))
                                                          ;; Lifted a hair so it shows on the surface.
                                                          (financial-chart-3d--r3 (+ (funcall y-at (cdr p)) (* 0.03 height)))
                                                          (funcall z-at j)))))))
      (vector
       (financial-chart-3d--axes "axes" (- w) w height (- d) d)
       (cond
        ((not complete) (list :id "surface" :type "segments" :segments []))
        ((equal (financial-chart-3d--opt graphics "style" "wire") "surface")
         (list :id "surface" :type "heightfield" :x (vector (- w) w)
               ;; Heightfield rows run from z[0] to z[1]: nearest first.
               :z (vector (- d) d)
               :base base :y_scale (financial-chart-3d--r3 scale)
               :heights (vconcat (mapcar (lambda (smile) (vconcat (mapcar #'financial-chart-3d--r3 smile)))
                                         smiles))
               :low_color (car stops) :high_color (car (last stops))
               :color_range (vector lo hi)))
        (t
         ;; A wire mesh: each expiry's smile and each strike's term
         ;; structure, every segment coloured by its mean IV.
         ;; A wide chain (a live one has 125 strikes) is drawn on at
         ;; most `max_strikes' of them, evenly spread: every strike's
         ;; segment would blur to a solid sheet.  The ATM line keeps
         ;; them all.
         (let* ((cols (financial-chart-3d--vol-columns
                       nx (financial-chart-3d--opt graphics "max_strikes" financial-chart-3d-vol-surface-max-strikes)))
                (every (max 1 (financial-chart-3d--opt graphics "strike_every" (max 1 (round (length cols) 6)))))
                segments colors)
           (cl-loop for smile in smiles for j from 0 do
                    (cl-loop for (i i2) on cols while i2 do
                      (push (vector (funcall x-at i) (funcall y-at (aref smile i)) (funcall z-at j)
                                    (funcall x-at i2) (funcall y-at (aref smile i2)) (funcall z-at j))
                            segments)
                      (push (funcall color (/ (+ (aref smile i) (aref smile i2)) 2)) colors)))
           (cl-loop for i in cols for c from 0 do
             (when (or (zerop (% c every)) (= i (1- nx)))
              (cl-loop for (a b) on smiles for j from 0 while b do
                      (push (vector (funcall x-at i) (funcall y-at (aref a i)) (funcall z-at j)
                                    (funcall x-at i) (funcall y-at (aref b i)) (funcall z-at (1+ j)))
                            segments)
                      (push (funcall color (/ (+ (aref a i) (aref b i)) 2)) colors))))
           (list :id "surface" :type "segments" :segments (vconcat (nreverse segments))
                 :colors (vconcat (nreverse colors))))))
       (list :id "atm" :type "segments" :color (financial-chart-3d--opt graphics "atm_color" "#ffffff")
             :segments (vconcat (cl-loop for (a b) on atm while b collect (vconcat a b))))))))

(defconst financial-chart-3d-vol-surface-marks
  '(("fc-vol-surface3d" . financial-chart-3d--vol-surface-mark))
  "The 3D marks this file adds to graphics.el: (MARK . FUNCTION).")

;; Registered when graphics.el's marks load, or now if they have.
(with-eval-after-load 'graphics-eas-marks
  (dolist (m financial-chart-3d-vol-surface-marks)
    (graphics-eas-define-mark (car m) (cdr m))))

;;; From invest metric outputs to bindings

(declare-function financial-chart-3d-metric-entries "financial-chart-3d" (output))
(declare-function financial-chart-3d-metric-stamp "financial-chart-3d" (entry args keys &optional default))

(defun financial-chart-3d-vol-surface--from-iv-surface (_template output args)
  "vol-surface bindings from opt.iv_surface OUTPUT, verbatim.
One output (at ARGS's :time, else its own, else now) or entries {time,
output}, the snapshots of a replay: `financial-chart-3d-vol-surface-rows'.
ARGS's :symbol titles it (\"NVDA implied volatility\")."
  (let* ((entries (financial-chart-3d-metric-entries output))
         (chain (if (and (= (length entries) 1) (not (financial-chart-3d-metric-stamp (car entries) nil '(:time))))
                    (plist-get (car entries) :output)
                  (cl-loop for e in entries for i from 0
                           collect (list :time (or (financial-chart-3d-metric-stamp e nil '(:time :as_of :date))
                                                   (financial-chart--invalid i "time" "missing_time"
                                                                             "each snapshot needs its time"))
                                         :surface (plist-get e :output)))))
         (symbol (plist-get args :symbol)))
    (append (list :data (financial-chart-3d-vol-surface-rows
                         (if (keywordp (car-safe chain)) chain (vconcat chain))
                         :time (plist-get args :time)))
            (and symbol (list :title (format "%s implied volatility" symbol))))))

(defconst financial-chart-3d-vol-surface-metric-adapters
  '(("opt.iv_surface" "vol-surface" financial-chart-3d-vol-surface--from-iv-surface))
  "The vol-surface template's metric adapter, (METRIC-ID TEMPLATE FUNCTION):
`financial-chart-3d-bindings-from-metric' calls it.")

;;; Opening

(cl-defun financial-chart-3d-vol-surface-open (surface-or-series &key symbol title time id show)
  "Open the 3D IV surface of SURFACE-OR-SERIES and return its eas view.
SURFACE-OR-SERIES is anything `financial-chart-3d-vol-surface-rows'
takes: one `opt.iv_surface' result verbatim (one keyframe at TIME,
default now), a sequence of {time, surface} pairs (a replay), chain
snapshots, or their rows already.  SYMBOL names the underlying in the
title (\"NVDA implied volatility\"); TITLE overrides it.  ID names the
view (default \"fc3d/vol-surface\"); SHOW draws it in a graphics.el
buffer, where SPC plays the replay.  Signals
`financial-chart-3d-unavailable' without graphics.el."
  (let ((rows (if (and (sequencep surface-or-series) (not (keywordp (car-safe surface-or-series)))
                       (> (length surface-or-series) 0) (plist-get (elt surface-or-series 0) :strike))
                  (vconcat surface-or-series)
                (financial-chart-3d-vol-surface-rows surface-or-series :time time))))
    (financial-chart-3d-open "vol-surface"
                             :bindings (list :data rows
                                             :title (or title (if symbol (format "%s implied volatility" symbol) "")))
                             :id id :show show)))

;;; The example

(defconst financial-chart-3d-vol-surface-fixture
  "test/fixtures/3d/vol-surface/spy-chain.json"
  "The SPY chain fixture the example is made from, under `financial-chart-root'.")

(defun financial-chart-3d-vol-surface-write-example (&optional file)
  "Write FILE (examples/vol-surface.data.json): the fixture chain's rows."
  (let ((chain (eas-json-read-file (expand-file-name financial-chart-3d-vol-surface-fixture
                                                     financial-chart-root))))
    (financial-chart-3d--write-json
     (or file (expand-file-name "examples/vol-surface.data.json" financial-chart-root))
     (list :title (format "%s implied volatility, %s" (plist-get chain :symbol) (plist-get chain :date))
           :data (financial-chart-3d-vol-surface-rows chain)))))

(provide 'financial-chart-3d-vol-surface)
;;; financial-chart-3d-vol-surface.el ends here
