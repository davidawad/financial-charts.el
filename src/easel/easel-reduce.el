;;; easel-reduce.el --- pure reducers: (state event scene) -> state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  `easel-reduce' is the whole of interaction: it reads the
;; current scene (scales, hit index, params) and returns the next view
;; state.  It never touches buffers, so every interaction runs in
;; --batch and replays from a log.  State:
;;
;;   :domains  (VIEW-KEY (:x [LO HI] :y [LO HI]))  zoom/pan (bind "scales")
;;   :params   (NAME STORE)                        selection stores
;;   :hover    hit plist or nil                     readout and crosshair
;;   :drag     in-progress pointer drag
;;   :history / :future                             zoom history stack
;;   :wheel    (VIEW PX DOMAINS) of the last wheel  one history entry per gesture
;;   :stream-cursor                                 rows received by push

;;; Code:

(require 'easel-core)
(require 'easel-scale)
(require 'easel-hit)
(require 'easel-params)
(require 'easel-zoom)

(defconst easel-reduce-click-slop 3 "Pixels a press may move and still be a click.")
(defconst easel-reduce-hover-radius 30 "Pixels beyond which hover finds nothing.")
(defconst easel-reduce-zoom-step 1.25 "Domain scale factor per +/- key.")
(defconst easel-reduce-wheel-step easel-zoom-wheel-step "Domain scale factor per wheel step.")
(defconst easel-reduce-history-limit 50 "Zoom history entries kept.")

(defun easel-reduce--view (scene id)
  "SCENE view with ID."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get scene :views)))

(defun easel-reduce--view-at (scene px)
  "SCENE view under PX, or nil."
  (let ((x (aref px 0)) (y (aref px 1)))
    (seq-find (lambda (v) (easel-hit--contains (plist-get v :bounds) x y)) (plist-get scene :views))))

(defun easel-reduce--params (scene view-id pred)
  "Selection params of SCENE's VIEW-ID satisfying PRED."
  (seq-filter (lambda (p) (and (equal (plist-get p :view) view-id) (funcall pred p)))
              (easel-params-of scene)))

(defun easel-reduce--scales-param-p (p) "Non-nil for bind-scales P." (equal (plist-get p :bind) "scales"))
(defun easel-reduce--brush-p (p)
  "Non-nil for an interval P not bound to scales."
  (and (equal (plist-get (plist-get p :def) :type) "interval") (not (easel-reduce--scales-param-p p))))
(defun easel-reduce--on-p (p on)
  "Non-nil when point P triggers on event name ON."
  (and (equal (plist-get (plist-get p :def) :type) "point")
       (not (equal (plist-get p :bind) "legend"))
       (member (plist-get (plist-get p :def) :on)
               (if (equal on "pointermove") '("pointermove" "mouseover" "mousemove") (list on)))))

(defun easel-reduce--put (state key value)
  "STATE with KEY set to VALUE."
  (easel-plist-put state key value))

(defun easel-reduce--store (state name store)
  "STATE with selection NAME holding STORE (nil empties it)."
  (easel-reduce--put state :params (easel-plist-put (plist-get state :params) (easel-key name) store)))

(defun easel-reduce--scale (scene view-id channel)
  "Continuous scale of CHANNEL in SCENE's VIEW-ID, or nil."
  (let ((s (plist-get (plist-get (easel-reduce--view scene view-id) :scales) channel)))
    (and (member (plist-get s :type) '("linear" "log" "time" "utc")) s)))

(defun easel-reduce--channels (param)
  "Positional channels (keywords) PARAM's encodings cover."
  (mapcar #'easel-key (or (plist-get (plist-get param :def) :encodings) ["x" "y"])))

(defun easel-reduce--set-domain (state view-id channel domain)
  "STATE with VIEW-ID's CHANNEL domain set to DOMAIN."
  (let* ((key (easel-key view-id)) (domains (plist-get state :domains)))
    (easel-reduce--put state :domains
                       (easel-plist-put domains key (easel-plist-put (plist-get domains key) channel domain)))))

(defun easel-reduce--remember (state &optional domains)
  "STATE with DOMAINS (default: its current domains) pushed on the history.
An unzoomed state is recorded as `:none'."
  (let ((history (cons (or (if domains (car domains) (plist-get state :domains)) :none)
                       (plist-get state :history))))
    (easel-reduce--put (easel-reduce--put state :history (seq-take history easel-reduce-history-limit))
                       :future nil)))

(defun easel-reduce--target-views (scene state)
  "Views whose scales keyboard zoom and pan act on: the hovered one, else all."
  (let* ((bound (delete-dups (mapcar (lambda (p) (plist-get p :view))
                                     (seq-filter #'easel-reduce--scales-param-p (easel-params-of scene)))))
         (hovered (plist-get (plist-get state :hover) :view)))
    (if (member hovered bound) (list hovered) bound)))

(defun easel-reduce--zoom (state scene view-id factor &optional px)
  "Scale VIEW-ID's bound domains by FACTOR around PX (default: the centre)."
  (let ((params (easel-reduce--params scene view-id #'easel-reduce--scales-param-p)))
    (dolist (channel (delete-dups (apply #'append (mapcar #'easel-reduce--channels params))))
      (when-let* ((scale (easel-reduce--scale scene view-id channel))
                  (domain (easel-zoom-domain scale factor (and px (aref px (if (eq channel :x) 0 1))))))
        (setq state (easel-reduce--set-domain state view-id channel domain))))
    state))

(defun easel-reduce--pan (state scene view-id channel fraction)
  "Shift VIEW-ID's CHANNEL domain by FRACTION of its range."
  (if-let* ((scale (easel-reduce--scale scene view-id channel))
            (domain (easel-zoom-step-domain scale fraction)))
      (easel-reduce--set-domain state view-id channel domain)
    state))

(defun easel-reduce--if-moved (state next)
  "NEXT, or STATE when NEXT leaves the domains as they were.
Keeps no-op zooms (unbound views, saturated domains) out of the history."
  (if (equal (plist-get state :domains) (plist-get next :domains)) state next))

(defun easel-reduce--wheel (state scene px delta)
  "Zoom the view under PX by DELTA wheel steps around PX.
Consecutive wheel events at one pointer position are one gesture and
one history entry."
  (if-let* ((view (easel-reduce--view-at scene px)))
      (let* ((id (plist-get view :id))
             (last (plist-get state :wheel))
             (same (and (equal (nth 0 last) id) (equal (nth 1 last) px)
                        (equal (nth 2 last) (plist-get state :domains))))
             (next (easel-reduce--if-moved
                    state (easel-reduce--zoom (if same state (easel-reduce--remember state)) scene id
                                              (expt easel-reduce-wheel-step delta) px))))
        (if (eq next state) state
          (easel-reduce--put next :wheel (list id px (plist-get next :domains)))))
    state))

;;; Pointer

(defun easel-reduce--hover (state scene px)
  "Hover at PX: nearest datum plus pointermove-driven selections."
  (let* ((view (easel-reduce--view-at scene px))
         (id (plist-get view :id))
         (movers (and view (easel-reduce--params scene id (lambda (p) (easel-reduce--on-p p "pointermove")))))
         (x-only (seq-some (lambda (p) (and (plist-get (plist-get p :def) :nearest)
                                            (equal (append (plist-get (plist-get p :def) :encodings) nil) '("x"))))
                           movers))
         (hit (and view (easel-hit scene id px x-only)))
         (hit (and hit (or x-only (<= (plist-get hit :distance) easel-reduce-hover-radius)) hit)))
    (setq state (easel-reduce--put state :hover hit))
    (dolist (p movers)
      (setq state (easel-reduce--store
                   state (plist-get p :name)
                   (and hit (or (plist-get (plist-get p :def) :nearest)
                                (<= (plist-get hit :distance) easel-reduce-click-slop))
                        (easel-params-point-store (easel-params-point-fields scene p) (list (plist-get hit :row)))))))
    state))

(defun easel-reduce--legend-click (state scene px)
  "Toggle a legend-bound selection when PX is on a legend entry; nil otherwise."
  (cl-loop for view across (plist-get scene :views)
           thereis
           (cl-loop for legend across (plist-get view :legends)
                    thereis
                    (cl-loop for entry across (plist-get legend :entries)
                             when (easel-hit--contains (plist-get entry :bounds) (aref px 0) (aref px 1))
                             return
                             (when-let* ((p (car (easel-reduce--params scene (plist-get view :id)
                                                                       (lambda (p) (equal (plist-get p :bind) "legend")))))
                                         (field (easel-params-channel-field scene (plist-get view :id)
                                                                            (plist-get legend :channel))))
                               (easel-reduce--store
                                state (plist-get p :name)
                                (easel-params-toggle (plist-get (plist-get state :params) (easel-key (plist-get p :name)))
                                                     (list field)
                                                     (list (easel-key field) (plist-get entry :value)))))))))

(defun easel-reduce--click (state scene event)
  "A click EVENT: legend toggles, point selections, brush clears."
  (let* ((px (plist-get event :px)))
    (or (easel-reduce--legend-click state scene px)
        (let* ((view (easel-reduce--view-at scene px)) (id (plist-get view :id)))
          (dolist (p (and view (easel-reduce--params scene id (lambda (p) (easel-reduce--on-p p "click")))))
            (let* ((def (plist-get p :def))
                   (x-only (equal (append (plist-get def :encodings) nil) '("x")))
                   (hit (easel-hit scene id px x-only))
                   (hit (and hit (or (plist-get def :nearest) (<= (plist-get hit :distance) easel-reduce-click-slop)) hit))
                   (fields (easel-params-point-fields scene p))
                   (old (plist-get (plist-get state :params) (easel-key (plist-get p :name)))))
              (setq state (easel-reduce--store
                           state (plist-get p :name)
                           (cond ((null hit) nil)
                                 ((and (eq (plist-get event :shift) t) (plist-get def :toggle))
                                  (easel-params-toggle old fields (plist-get hit :row)))
                                 (t (easel-params-point-store fields (list (plist-get hit :row)))))))))
          (dolist (p (and view (easel-reduce--params scene id #'easel-reduce--brush-p)))
            (setq state (easel-reduce--store state (plist-get p :name) nil)))
          state))))

(defun easel-reduce--brush-store (scene param start end)
  "Interval store for PARAM spanning pixels START..END in SCENE."
  (let ((fields nil) (store (list :type "interval")))
    (dolist (channel (easel-reduce--channels param))
      (when-let* ((scale (easel-reduce--scale scene (plist-get param :view) channel)))
        (let ((i (if (eq channel :x) 0 1)))
          (setq fields (append fields (list channel (plist-get scale :field))))
          (setq store (append store (list channel (vector (min (easel-scale-invert scale (aref start i))
                                                               (easel-scale-invert scale (aref end i)))
                                                          (max (easel-scale-invert scale (aref start i))
                                                               (easel-scale-invert scale (aref end i))))))))))
    (when fields (append store (list :fields fields)))))

(defun easel-reduce--drag-to (state scene px)
  "Continue the drag in STATE to PX."
  (let* ((drag (plist-get state :drag)) (start (plist-get drag :start))
         (moved (or (plist-get drag :moved)
                    (> (+ (abs (- (aref px 0) (aref start 0))) (abs (- (aref px 1) (aref start 1))))
                       easel-reduce-click-slop)))
         (state (easel-reduce--put state :drag (easel-plist-put (easel-plist-put drag :current px) :moved moved))))
    (if (not moved) state
      (pcase (plist-get drag :mode)
        ("brush" (let ((p (seq-find (lambda (p) (equal (plist-get p :name) (plist-get drag :param))) (easel-params-of scene))))
                   (easel-reduce--store state (plist-get drag :param) (easel-reduce--brush-store scene p start px))))
        ("pan" (let ((view-id (plist-get drag :view)))
                 (dolist (channel '(:x :y))
                   (when-let* ((snap (plist-get (plist-get drag :snapshot) channel))
                               (scale (easel-reduce--scale scene view-id channel))
                               (i (if (eq channel :x) 0 1))
                               (domain (easel-zoom-pan-domain (plist-put (copy-sequence scale) :domain snap)
                                                              (- (aref px i) (aref start i)))))
                     (setq state (easel-reduce--set-domain state view-id channel domain))))
                 state))
        (_ state)))))

(defun easel-reduce--press (state scene px)
  "Start a drag at PX: brush when the view has a brush, pan when bound to scales."
  (let* ((view (easel-reduce--view-at scene px)) (id (plist-get view :id))
         (brush (and view (car (easel-reduce--params scene id #'easel-reduce--brush-p))))
         (pan (and view (car (easel-reduce--params scene id #'easel-reduce--scales-param-p)))))
    (easel-reduce--put state :drag
                       (list :view id :start px :current px :moved nil
                             :mode (cond (brush "brush") (pan "pan") (t "none"))
                             :param (and brush (plist-get brush :name))
                             :snapshot (and pan (cl-loop for ch in (easel-reduce--channels pan)
                                                         for s = (easel-reduce--scale scene id ch)
                                                         when s append (list ch (plist-get s :domain))))
                             :domains0 (plist-get state :domains)))))

(defun easel-reduce--release (state scene px)
  "End the drag at PX; an unmoved press is a click."
  (let ((drag (plist-get state :drag)) (state (easel-reduce--put state :drag nil)))
    (cond ((null drag) state)
          ((not (plist-get drag :moved)) (easel-reduce--click state scene (list :px px)))
          ((equal (plist-get drag :mode) "pan")
           (easel-reduce--remember state (list (plist-get drag :domains0))))
          (t state))))

;;; Entry point

(defun easel-reduce--zoom-to-brush (state scene)
  "Zoom every brushed view to its brush's ranges, then clear the brushes.
The previous domains go on the history, so [ undoes it."
  (let ((brushes (seq-filter (lambda (p) (and (easel-reduce--brush-p p)
                                              (plist-get (plist-get state :params) (easel-key (plist-get p :name)))))
                             (easel-params-of scene))))
    (if (null brushes) state
      (let ((s (easel-reduce--remember state)))
        (dolist (p brushes s)
          (let ((store (plist-get (plist-get s :params) (easel-key (plist-get p :name)))))
            (dolist (ch '(:x :y))
              (when-let* ((r (plist-get store ch))
                          ((/= (aref r 0) (aref r 1))))
                (setq s (easel-reduce--set-domain s (plist-get p :view) ch
                                                  (vector (min (aref r 0) (aref r 1)) (max (aref r 0) (aref r 1)))))))
            (setq s (easel-reduce--store s (plist-get p :name) nil))))))))

(defun easel-reduce--brush-event (state scene event)
  "Apply an agent's brush EVENT (data-space ranges)."
  (let* ((params (easel-params-of scene "interval"))
         (p (if (plist-get event :param)
                (seq-find (lambda (p) (equal (plist-get p :name) (plist-get event :param))) params)
              (or (seq-find #'easel-reduce--brush-p params) (car params))))
         (num (lambda (v) (if (numberp v) v (easel-time-parse v)))))
    (unless p
      (easel-signal "EVENT_INVALID" "This view has no interval selection param to brush"
                    :field "param" :params (vconcat (mapcar (lambda (p) (plist-get p :name)) (easel-params-of scene)))))
    (if (easel-reduce--scales-param-p p)
        (progn
          (setq state (easel-reduce--remember state))
          (dolist (ch '(:x :y) state)
            (when-let* ((r (plist-get event ch)))
              (setq state (easel-reduce--set-domain state (plist-get p :view) ch
                                                    (vector (funcall num (aref r 0)) (funcall num (aref r 1)))))))
          state)
      (let ((store (list :type "interval")) (fields nil))
        (dolist (ch '(:x :y))
          (when-let* ((r (plist-get event ch)))
            (setq fields (append fields (list ch (easel-params-channel-field scene (plist-get p :view) (easel-key-name ch))))
                  store (append store (list ch (vector (funcall num (aref r 0)) (funcall num (aref r 1))))))))
        (easel-reduce--store state (plist-get p :name) (append store (list :fields fields)))))))

(defun easel-reduce (state event scene)
  "Return the view state after EVENT, given the current SCENE.  Pure."
  (let ((px (plist-get event :px)))
    (pcase (plist-get event :type)
      ("pointermove" (if (plist-get state :drag) (easel-reduce--drag-to state scene px)
                       (easel-reduce--hover state scene px)))
      ("pointerdown" (easel-reduce--press state scene px))
      ("pointerup" (if (plist-get state :drag)
                       (easel-reduce--release (easel-reduce--drag-to state scene px) scene px)
                     state))
      ("click" (easel-reduce--click state scene event))
      ("dblclick"
       (let ((view (easel-reduce--view-at scene px)))
         (dolist (p (easel-params-of scene))
           (when (equal (plist-get p :view) (plist-get view :id))
             (setq state (easel-reduce--store state (plist-get p :name) nil))))
         (if (plist-get (plist-get state :domains) (easel-key (plist-get view :id)))
             (easel-reduce--put (easel-reduce--remember state) :domains
                                (easel--plist-without (plist-get state :domains) (easel-key (plist-get view :id))))
           state)))
      ("pointerleave"
       (dolist (p (easel-params-of scene "point"))
         (when (easel-reduce--on-p p "pointermove") (setq state (easel-reduce--store state (plist-get p :name) nil))))
       (easel-reduce--put (easel-reduce--put state :hover nil) :drag nil))
      ("wheel" (easel-reduce--wheel state scene px (plist-get event :delta)))
      ("drag"
       (let* ((s (easel-reduce--press state scene (plist-get event :from)))
              (s (easel-reduce--drag-to s scene (plist-get event :to))))
         (easel-reduce--release s scene (plist-get event :to))))
      ("brush" (easel-reduce--brush-event state scene event))
      ("key" (easel-reduce--key state scene (plist-get event :key)))
      ("push" (easel-reduce--put state :stream-cursor
                                 (+ (or (plist-get state :stream-cursor) 0) (length (plist-get event :rows)))))
      (_ state))))

(defun easel-reduce--key (state scene key)
  "Apply KEY to STATE."
  (let ((views (easel-reduce--target-views scene state)))
    (pcase key
      ((or "+" "=" "-")
       (let ((s (easel-reduce--remember state)))
         (dolist (v views (easel-reduce--if-moved state s))
           (setq s (easel-reduce--zoom s scene v (if (equal key "-") easel-reduce-zoom-step
                                                   (/ 1.0 easel-reduce-zoom-step)))))))
      ("0" (let ((s (easel-reduce--remember state)))
             (easel-reduce--if-moved
              state (easel-reduce--put s :domains (cl-loop for (k v) on (plist-get s :domains) by #'cddr
                                                           unless (member (easel-key-name k) views)
                                                           append (list k v))))))
      ((or "left" "right" "up" "down")
       (let ((s (easel-reduce--remember state))
             (channel (if (member key '("left" "right")) :x :y))
             (fraction (if (member key '("left" "down")) -0.1 0.1)))
         (dolist (v views (easel-reduce--if-moved state s)) (setq s (easel-reduce--pan s scene v channel fraction)))))
      ("z" (easel-reduce--zoom-to-brush state scene))
      ("escape" (easel-reduce--put (easel-reduce--put state :params nil) :hover nil))
      ("[" (if-let* ((prev (car (plist-get state :history))))
               (thread-first state
                             (easel-reduce--put :future (cons (or (plist-get state :domains) :none)
                                                              (plist-get state :future)))
                             (easel-reduce--put :history (cdr (plist-get state :history)))
                             (easel-reduce--put :domains (if (eq prev :none) nil prev)))
             state))
      ("]" (if-let* ((next (car (plist-get state :future))))
               (thread-first state
                             (easel-reduce--put :history (cons (or (plist-get state :domains) :none)
                                                               (plist-get state :history)))
                             (easel-reduce--put :future (cdr (plist-get state :future)))
                             (easel-reduce--put :domains (if (eq next :none) nil next)))
             state))
      (_ state))))

(provide 'easel-reduce)
;;; easel-reduce.el ends here
