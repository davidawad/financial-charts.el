;;; easel-stream.el --- live data: frame-capped, interaction-aware pushes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A template (or spec) opts into live data with
;;
;;   "x-easel": {"stream": {"max-fps": 5, "window": 500}}
;;
;; Every `easel-push' to such a view is schema-checked at once (so a
;; bad row fails the caller, not a timer) and queued.  Queued rows reach
;; the view as one windowed push event, at most max-fps times a second,
;; and never while the user is interacting: a drag (brush or pan) is in
;; progress, or a pointer event arrived within `easel-stream-hover-hold'
;; seconds and no pointerleave followed.  When that ends the queue
;; catches up in a single frame.  The frame is an ordinary push event,
;; so the view log replays exactly what was drawn.
;;
;; The redraw itself stays idle-coalesced (easel-mode); the cap bounds
;; how often the scene is recompiled.  The default cap comes from the
;; measured cost of a push frame (docs/design/engine-spikes.md, section
;; 8).  Time is `easel-stream-clock' and timers can be turned off, so
;; ERT drives every case headlessly with `easel-stream-tick'.

;;; Code:

(require 'easel-core)
(require 'easel-template)
(require 'easel-adapters)
(require 'easel-view)
(require 'easel-describe)

(defvar easel-stream-default-max-fps 5
  "Frame cap for streams that do not set max-fps.
See engine-spikes.md section 8 for the measured frame costs behind it.")

(defconst easel-stream-max-fps-limit 30
  "Highest accepted max-fps.")

(defvar easel-stream-hover-hold 2.0
  "Seconds after a pointer event that a stream stays paused.
Neither GUI nor terminal glue reliably reports the pointer leaving (in
a terminal, point always hovers), so the pause lapses.  nil holds until
pointerleave.")

(defvar easel-stream-clock #'float-time
  "Function returning the current time in seconds.")

(defvar easel-stream-use-timers t
  "Non-nil schedules deferred frames with timers.
Tests bind it to nil and call `easel-stream-tick' themselves.")

(defconst easel-stream--pointer-events
  '("pointermove" "pointerdown" "pointerup" "click" "dblclick" "wheel")
  "Event types that mean the pointer is over the chart.")

(cl-defstruct (easel-stream (:constructor easel-stream--make) (:copier nil))
  view object max-fps window pending (queued 0) last-frame pointer-at timer
  (frames 0) (pushes 0))

(defvar easel-streams (make-hash-table :test 'equal)
  "Live streams by view id.")

;;; Configuration

(defun easel-stream--invalid (key message)
  "Signal INVALID_INPUT for x-easel.stream KEY with MESSAGE."
  (easel-signal "INVALID_INPUT" message :path (concat "/x-easel/stream" (and key (concat "/" key)))))

(defun easel-stream-check (config)
  "Validate CONFIG (an x-easel.stream object); return it with defaults.
The result is (:max-fps N :window N-or-nil)."
  (unless (and config (easel-object-p config))
    (easel-stream--invalid nil "x-easel.stream is an object: {\"max-fps\": N, \"window\": N}"))
  (dolist (key (easel-plist-keys config))
    (unless (memq key '(:max-fps :window))
      (easel-stream--invalid (easel-key-name key)
                             (format "Unknown x-easel.stream key %s; keys: max-fps, window"
                                     (easel-key-name key)))))
  (let ((fps (or (plist-get config :max-fps) easel-stream-default-max-fps))
        (window (plist-get config :window)))
    (unless (and (numberp fps) (> fps 0) (<= fps easel-stream-max-fps-limit))
      (easel-stream--invalid "max-fps" (format "max-fps is a number in (0, %d]" easel-stream-max-fps-limit)))
    (unless (or (null window) (and (natnump window) (> window 0)))
      (easel-stream--invalid "window" "window is a positive integer: the rows to keep, oldest dropped"))
    (list :max-fps fps :window window)))

(defun easel-stream-config (source)
  "The x-easel.stream object SOURCE declares, or nil.
SOURCE is a template name, a template plist or a chart/v1 spec (plist
or JSON string).  The object is returned as written; see
`easel-stream-check' for defaults."
  (cond ((and (stringp source) (not (string-prefix-p "{" (string-trim-left source))))
         (easel-stream-config (easel-template-get source)))
        ((and (easel-object-p source) (plist-get source :meta))
         (plist-get (plist-get source :meta) :stream))
        (t (plist-get (plist-get (if (stringp source) (easel-json-parse source) source) :x-easel) :stream))))

;;; Attaching

(defun easel-stream-get (view)
  "The stream attached to VIEW (an id or view), or nil.
A stream left by a closed view whose id was reused is dropped."
  (let* ((view (easel-view-get view)) (stream (gethash (easel-view-id view) easel-streams)))
    (if (and stream (not (eq (easel-stream-object stream) view)))
        (progn (easel-stream--cancel stream) (remhash (easel-view-id view) easel-streams) nil)
      stream)))

(defun easel-stream-attach (view &optional config)
  "Stream live data into VIEW with CONFIG (default: its template's).
Signals INVALID_INPUT when neither declares x-easel.stream.  Returns
the stream; attaching again replaces the configuration."
  (let* ((view (easel-view-get view))
         (config (or config (and (easel-view-template view) (easel-stream-config (easel-view-template view)))))
         (checked (if config (easel-stream-check config)
                    (easel-stream--invalid nil (format "View %s declares no x-easel.stream; pass a config"
                                                       (easel-view-id view)))))
         (stream (or (easel-stream-get view)
                     (easel-stream--make :view (easel-view-id view) :object view))))
    (setf (easel-stream-max-fps stream) (plist-get checked :max-fps)
          (easel-stream-window stream) (plist-get checked :window))
    (puthash (easel-view-id view) stream easel-streams)))

(defun easel-stream-detach (view)
  "Stop streaming into VIEW (an id or view), drawing queued rows first."
  (when-let* ((stream (gethash (if (easel-view-p view) (easel-view-id view) view) easel-streams)))
    (when (and (easel-stream-pending stream) (gethash (easel-stream-view stream) easel-views))
      (easel-stream--flush stream (funcall easel-stream-clock)))
    (easel-stream--cancel stream)
    (remhash (easel-stream-view stream) easel-streams)))

(cl-defun easel-stream-open (source &rest args &key stream &allow-other-keys)
  "Open a view of SOURCE (as `easel-view-open' with ARGS) and attach a stream.
STREAM overrides the config SOURCE declares; one of them must exist."
  (let ((view (apply #'easel-view-open source (easel--plist-without args :stream))))
    (condition-case err
        (easel-stream-attach view (or stream (unless (easel-view-template view)
                                               (easel-stream-config source))))
      (error (easel-view-close view) (signal (car err) (cdr err))))
    view))

;;; Frames

(defun easel-stream--held (stream view now)
  "Why STREAM's VIEW must not take a frame at NOW (drag or pointer), or nil."
  (cond ((plist-get (easel-view-state view) :drag) "drag")
        ((and (easel-stream-pointer-at stream)
              (or (null easel-stream-hover-hold)
                  (< (- now (easel-stream-pointer-at stream)) easel-stream-hover-hold)))
         "pointer")))

(defun easel-stream--next-frame (stream)
  "Earliest time STREAM may take its next frame."
  (if (easel-stream-last-frame stream)
      (+ (easel-stream-last-frame stream) (/ 1.0 (easel-stream-max-fps stream)))
    0))

(defun easel-stream--flush (stream now)
  "Dispatch STREAM's queued rows as one windowed push at NOW."
  (let* ((rows (apply #'vconcat (reverse (easel-stream-pending stream))))
         (window (easel-stream-window stream))
         (rows (if (and window (> (length rows) window)) (seq-subseq rows (- (length rows) window)) rows)))
    (setf (easel-stream-pending stream) nil (easel-stream-queued stream) 0
          (easel-stream-last-frame stream) now)
    (cl-incf (easel-stream-frames stream))
    (easel-dispatch (easel-stream-view stream)
                    (append (list :type "push" :rows rows) (and window (list :window window))))))

(defun easel-stream--cancel (stream)
  "Cancel STREAM's pending timer."
  (when (easel-stream-timer stream)
    (cancel-timer (easel-stream-timer stream))
    (setf (easel-stream-timer stream) nil)))

(defun easel-stream--schedule (stream at now)
  "Arrange for STREAM to be ticked at time AT (seen from NOW)."
  (when (and easel-stream-use-timers (not (easel-stream-timer stream)))
    (setf (easel-stream-timer stream)
          (run-at-time (max 0 (- at now)) nil
                       (lambda (id)
                         (when-let* ((s (gethash id easel-streams)))
                           (setf (easel-stream-timer s) nil)
                           (if (gethash id easel-views) (easel-stream-tick id)
                             (remhash id easel-streams))))
                       (easel-stream-view stream)))))

(defun easel-stream-tick (view &optional now)
  "Draw VIEW's queued rows at NOW (default: the clock) if allowed.
A frame is taken when rows are queued, 1/max-fps seconds have passed
since the last one and no interaction holds it; otherwise the next try
is scheduled.  Returns non-nil when a frame was taken."
  (let* ((stream (easel-stream-get view)) (now (or now (funcall easel-stream-clock))))
    (when (and stream (easel-stream-pending stream))
      (let ((held (easel-stream--held stream (easel-view-get view) now))
            (next (easel-stream--next-frame stream)))
        (cond ((and (equal held "pointer") easel-stream-hover-hold)
               (easel-stream--schedule stream (max next (+ (easel-stream-pointer-at stream) easel-stream-hover-hold)) now)
               nil)
              (held (easel-stream--schedule stream (max next (+ now (/ 1.0 (easel-stream-max-fps stream)))) now)
                    nil)
              ((< now next) (easel-stream--schedule stream next now) nil)
              (t (easel-stream--cancel stream) (easel-stream--flush stream now) t))))))

(defun easel-stream-push (view rows)
  "Queue ROWS for VIEW's stream and draw when the cap and interaction allow.
ROWS are schema-checked now (SHAPE_INVALID, :index within ROWS).  A
view whose template declares x-easel.stream is attached on first push;
any other view gets the rows at once.  Returns `easel-inspect'."
  (let* ((view (easel-view-get view))
         (rows (vconcat rows))
         (stream (or (easel-stream-get view)
                     (and (easel-view-interactive view) (easel-view-template view)
                          (easel-stream-config (easel-view-template view))
                          (easel-stream-attach view)))))
    (if (not stream)
        (easel-dispatch view (list :type "push" :rows rows))
      (easel-data-append (list :schema (plist-get (easel-view-data view) :schema) :rows []) rows)
      (when (> (length rows) 0)
        (push rows (easel-stream-pending stream))
        (cl-incf (easel-stream-queued stream) (length rows))
        (cl-incf (easel-stream-pushes stream))
        (easel-stream-tick view))
      (easel-inspect view))))

(defun easel-stream-flush (view)
  "Draw VIEW's queued rows now, ignoring the cap and interaction; return inspect."
  (let ((stream (easel-stream-get view)))
    (when (and stream (easel-stream-pending stream))
      (easel-stream--cancel stream)
      (easel-stream--flush stream (funcall easel-stream-clock)))
    (easel-inspect view)))

(defun easel-stream--observe (view event &rest _)
  "Track pointer presence for VIEW's stream from dispatched EVENT.
The hook's OLD-STATE and OLD-SCENE arguments are ignored."
  (when-let* ((stream (easel-stream-get view)))
    (let ((type (plist-get event :type)))
      (cond ((member type easel-stream--pointer-events)
             (setf (easel-stream-pointer-at stream) (funcall easel-stream-clock)))
            ((equal type "pointerleave")
             (setf (easel-stream-pointer-at stream) nil)))
      ;; Leaving the chart or ending a drag may end a pause: catch up.
      (when (and (member type '("pointerleave" "pointerup")) (easel-stream-pending stream)
                 (not (easel-stream--held stream view (funcall easel-stream-clock))))
        (easel-stream--cancel stream)
        (easel-stream-tick view)))))

(add-hook 'easel-view-dispatch-functions #'easel-stream--observe)
(setq easel-push-function #'easel-stream-push)

;;; Inspect

(defun easel-stream-inspect (view)
  "VIEW's stream as JSON-ready data, or :null when it has none."
  (if-let* ((stream (easel-stream-get view)))
      (list :view (easel-stream-view stream)
            :max-fps (easel-stream-max-fps stream)
            :window (or (easel-stream-window stream) :null)
            :queued (easel-stream-queued stream)
            :held (or (easel-stream--held stream (easel-view-get view) (funcall easel-stream-clock)) :null)
            :frames (easel-stream-frames stream)
            :pushes (easel-stream-pushes stream))
    :null))

(defun easel-stream--describe ()
  "The describe section for live data."
  (list :stream (list :key "x-easel.stream"
                      :keys (list :max-fps (format "frames per second; default %s, at most %d"
                                                   easel-stream-default-max-fps easel-stream-max-fps-limit)
                                  :window "rows kept, oldest dropped; default all")
                      :hover-hold (or easel-stream-hover-hold :null)
                      :verbs ["easel-push" "easel-stream-open" "easel-stream-attach"
                              "easel-stream-flush" "easel-stream-inspect" "easel-stream-detach"])))

(add-hook 'easel-describe-functions #'easel-stream--describe)

(provide 'easel-stream)
;;; easel-stream.el ends here
