;;; easel-view.el --- view/v1: live views, dispatch, replay, inspect -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A view is one chart someone (or something) is looking
;; at: {id, spec-hash, bindings, state, log}.  Ids are stable and
;; readable ("line:daily", "ohlc:TSM").  `easel-dispatch' applies an
;; event/v1 through the pure reducer, re-compiles only when the visible
;; output changes, appends to a bounded log and returns `easel-inspect'.
;; `easel-replay' re-applies a log.  None of this needs a display, so
;; ERT and agents drive every interaction headlessly.  Buffer glue
;; (easel-mode.el) only translates native events and redraws.

;;; Code:

(require 'easel-core)
(require 'easel-data)
(require 'easel-resolve)
(require 'easel-compile)
(require 'easel-compile-patch)
(require 'easel-event)
(require 'easel-reduce)
(require 'easel-params)
(require 'easel-adapters)
(require 'easel-chart)
(require 'easel-tip)

(defvar easel-views (make-hash-table :test 'equal)
  "Live views by id.")

(defvar easel-view-log-size 200
  "Events kept in each view's log.")

(defvar easel-view-changed-functions nil
  "Hook run with a VIEW after dispatch changed its visible output.")

(defvar easel-view-dispatch-functions nil
  "Hook run with VIEW, EVENT, OLD-STATE and OLD-SCENE after every dispatch.
It runs once the reducer has updated VIEW and before the inspect is
taken, so a function may record data in VIEW's state (clicks, fc-qx1.1).")

(defvar easel-view-replaying nil
  "Non-nil while `easel-replay' re-applies a log.
Hook functions with side effects (actions, echo) skip them then.")
  "Hook run with VIEW and the parsed EVENT after every interactive dispatch.")

(defvar easel-push-function nil
  "When non-nil, `easel-push' calls it with VIEW and ROWS instead of
dispatching a push now.  easel-stream sets it to coalesce live data.")

(cl-defstruct (easel-view (:constructor easel-view--make) (:copier nil))
  id template subject spec spec-hash bindings data size target cell
  state log (seq 0) scene plan buffer (interactive t) fallback warnings)

(defun easel-view--unique-id (base)
  "BASE, or BASE<N> when BASE is taken."
  (if (not (gethash base easel-views)) base
    (cl-loop for n from 2 for id = (format "%s<%d>" base n)
             unless (gethash id easel-views) return id)))

(defun easel-view-get (view)
  "Return VIEW (an id or view) or signal VIEW_NOT_FOUND."
  (cond ((easel-view-p view) view)
        ((gethash view easel-views))
        (t (easel-signal "VIEW_NOT_FOUND"
                         (format "No view %S; live views: %s" view
                                 (let (ids) (maphash (lambda (k _) (push k ids)) easel-views)
                                      (if ids (string-join (sort ids #'string<) ", ") "none")))
                         :view view))))

(defun easel-view--compile (view &optional patch old-state)
  "Compile VIEW's scene from its spec, data and state.
With PATCH (data and size unchanged since OLD-STATE), try patching the
cached plan for a selection-only change before compiling from scratch."
  (let ((state (easel-view-state view)))
    (easel-params-with-state state
      (let ((plan (or (and patch (easel-view-plan view)
                           (easel-compile-patch (easel-view-plan view) old-state state))
                      (easel-compile-plan (easel-view-spec view)
                                          :rows (plist-get (easel-view-data view) :rows)
                                          :size (easel-view-size view) :target (easel-view-target view)
                                          :cell (easel-view-cell view) :state state))))
        (setf (easel-view-plan view) plan)
        (easel-compile-scene plan state)))))

(defun easel-view--root-rows (spec)
  "Inline root rows of resolved SPEC, or nil."
  (let ((values (plist-get (plist-get spec :data) :values)))
    (and (vectorp values) values)))

(cl-defun easel-view-open (source &key id subject bindings rows size target cell)
  "Open a live view of SOURCE and register it; return the view.
SOURCE is a template name (resolved with BINDINGS) or a chart/v1 spec.
SUBJECT names what is shown (\"TSM\") for a readable id
TEMPLATE:SUBJECT; ID overrides.  ROWS replaces the root data.  SIZE,
TARGET and CELL are as in `easel-compile'."
  (let* ((template (and (stringp source) (not (string-prefix-p "{" (string-trim-left source)))
                        (not (string-suffix-p ".json" source))
                        source))
         (spec (if template (easel-resolve template bindings) (easel-resolve-spec source)))
         (data (easel-data-make (or rows (easel-view--root-rows spec) []) ))
         (view (easel-view--make
                :id (easel-view--unique-id (or id (if subject (format "%s:%s" (or template "chart") subject)
                                                    (or template "chart"))))
                :template template :subject subject :spec spec :spec-hash (easel-resolve-hash spec)
                :bindings bindings :data data :size size :target (or target 'svg) :cell cell
                :state nil :log nil)))
    (condition-case err
        (setf (easel-view-scene view) (easel-view--compile view))
      (easel-unsupported-feature (easel-view--fallback view err)))
    (puthash (easel-view-id view) view easel-views)
    view))

(defun easel-view--fallback (view err)
  "Make VIEW a static, non-interactive view after unsupported-feature ERR.
Every unsupported path becomes a warning; the picture comes from
bin/chart when it is installed."
  (let ((spec (easel-view-spec view)))
    (setf (easel-view-interactive view) nil
          (easel-view-warnings view)
          (vconcat (or (let ((easel-spec-supported-function easel-spec-supported-function))
                         (mapcar (lambda (f) (list :code "UNSUPPORTED_FEATURE" :path (plist-get f :path)
                                                   :feature (plist-get f :feature) :message (plist-get f :message)))
                                 (easel-spec-unsupported spec)))
                       (list (easel-error-plist err))))
          (easel-view-fallback view)
          (condition-case e (list :type "svg" :data (easel-chart-build spec "svg"))
            (easel-error (list :error (easel-error-plist e)))))))

(defun easel-view-close (view)
  "Forget VIEW."
  (remhash (easel-view-id (easel-view-get view)) easel-views))

(defun easel-view-ids ()
  "Ids of live views, sorted."
  (let (ids) (maphash (lambda (k _) (push k ids)) easel-views) (sort ids #'string<)))

(defun easel-view--visible (view)
  "The parts of VIEW's state that change what is drawn."
  (let ((state (easel-view-state view)))
    (list (plist-get state :domains) (plist-get state :params)
          (length (plist-get (easel-view-data view) :rows)))))

(defun easel-view--log (view event)
  "Append EVENT to VIEW's bounded log."
  (cl-incf (easel-view-seq view))
  (setf (easel-view-log view)
        (seq-take (cons (list :seq (easel-view-seq view) :type (plist-get event :type)
                              :summary (easel-event-describe event) :event event)
                        (easel-view-log view))
                  easel-view-log-size)))

(defun easel-view-resize (view size &optional target)
  "Recompile VIEW at SIZE (and TARGET); return its scene (nil when static)."
  (let ((view (easel-view-get view)))
    (setf (easel-view-size view) size)
    (when target (setf (easel-view-target view) target))
    (when (easel-view-interactive view)
      (setf (easel-view-scene view) (easel-view--compile view)))))

(defun easel-dispatch (view event)
  "Apply EVENT (event/v1) to VIEW and return the new `easel-inspect'.
Events are validated (EVENT_INVALID); push rows are schema-checked
\(SHAPE_INVALID).  The scene is recompiled only when domains, selections
or data changed."
  (if (not (easel-view-interactive (easel-view-get view)))
      (let ((view (easel-view-get view)))
        ;; Static fallback: record what was asked, change nothing.
        (easel-view--log view (easel-event-parse event))
        (easel-inspect view))
    (easel-view--dispatch (easel-view-get view) event)))

(defun easel-view--dispatch (view event)
  "Apply EVENT to interactive VIEW and return the new inspect."
  (let* ((event (easel-event-parse event))
         (before (easel-view--visible view))
         (old-state (easel-view-state view))
         (old-scene (easel-view-scene view))
         (push (equal (plist-get event :type) "push")))
    (when push
      (setf (easel-view-data view)
            (easel-view--window (easel-data-append (easel-view-data view) (vconcat (plist-get event :rows)))
                                (plist-get event :window))))
    (setf (easel-view-state view) (easel-reduce old-state event (easel-view-scene view)))
    (easel-view--log view event)
    ;; A windowed push can keep the row count while changing the rows.
    (unless (and (equal before (easel-view--visible view))
                 (not (and push (> (length (plist-get event :rows)) 0))))
      (setf (easel-view-scene view) (easel-view--compile view (not push) old-state))
      (run-hook-with-args 'easel-view-changed-functions view))
    (run-hook-with-args 'easel-view-dispatch-functions view event old-state old-scene)
    (run-hook-with-args 'easel-view-dispatch-functions view event)
    (easel-inspect view)))

(defun easel-view--window (data window)
  "DATA keeping only its last WINDOW rows (all when WINDOW is nil)."
  (let ((rows (plist-get data :rows)))
    (if (and window (> (length rows) window))
        (easel-plist-put data :rows (seq-subseq rows (- (length rows) window)))
      data)))

(defun easel-replay (view log)
  "Re-apply LOG (events, or log entries with :event) to VIEW, oldest first.
LOG may be a vector or list, in order, or a view's own `easel-view-log'
\(newest first, detected by descending :seq).  Returns the final inspect."
  (let* ((entries (append log nil))
         (entries (if (and (cdr entries) (plist-get (car entries) :seq)
                           (> (plist-get (car entries) :seq) (plist-get (cadr entries) :seq)))
                      (reverse entries) entries))
         (result (easel-inspect view))
         (easel-view-replaying t))
    (dolist (entry entries result)
      (setq result (easel-dispatch view (or (plist-get entry :event) entry))))))

(defun easel-push (view rows)
  "Append ROWS to VIEW's data (schema-checked) and redraw; return inspect.
With `easel-push-function' set (easel-stream), the push may be coalesced."
  (if easel-push-function (funcall easel-push-function view rows)
    (easel-dispatch view (list :type "push" :rows (vconcat rows)))))

;;; Inspect and selection

(defun easel-view--fmt (scale v)
  "Data value V formatted for SCALE (ISO for time)."
  (if (and (member (plist-get scale :type) '("time" "utc")) (numberp v)) (easel-time-iso v) v))

(defun easel-view--visible-summary (scene-view)
  "n/min/max/first/last/change of the y field over SCENE-VIEW's x domain."
  (let* ((scales (plist-get scene-view :scales))
         (xs (plist-get scales :x)) (ys (plist-get scales :y))
         (mark (seq-find (lambda (m) (> (length (plist-get m :rows)) 0)) (plist-get scene-view :marks)))
         (xf (and (plist-get xs :field) (easel-key (plist-get xs :field))))
         (yf (and (plist-get ys :field) (easel-key (plist-get ys :field))))
         (d (plist-get xs :domain))
         (rows (and mark yf
                    (seq-filter (lambda (r)
                                  (or (not (and xf (member (plist-get xs :type) '("linear" "time" "utc"))))
                                      (let ((x (easel-params--number (plist-get r xf))))
                                        (and x (<= (min (aref d 0) (aref d 1)) x (max (aref d 0) (aref d 1)))))))
                                (plist-get mark :rows))))
         (values (seq-filter #'numberp (mapcar (lambda (r) (plist-get r yf)) rows))))
    (when values
      (let ((first (car values)) (last (car (last values))))
        (list :field (plist-get ys :field) :n (length values)
              :min (apply #'min values) :max (apply #'max values) :first first :last last
              :change (- last first)
              :change-pct (if (zerop first) :null (/ (round (* 10000.0 (/ (- last first) (float first)))) 100.0)))))))

(defun easel-inspect (view)
  "What VIEW shows right now, as JSON-ready data."
  (let* ((view (easel-view-get view)) (scene (easel-view-scene view)) (state (easel-view-state view))
         (hover (plist-get state :hover)))
    (list :id (easel-view-id view) :template (or (easel-view-template view) :null)
          :spec-hash (easel-view-spec-hash view)
          :interactive (if (easel-view-interactive view) t :false)
          :target (symbol-name (easel-view-target view))
          :size (or (plist-get scene :size) :null)
          :static (if (easel-view-interactive view) :null
                    (let ((fb (easel-view-fallback view)))
                      (if (plist-get fb :data) (list :source "bin/chart" :type (plist-get fb :type))
                        (list :source :null :error (plist-get fb :error)))))
          :rows (length (plist-get (easel-view-data view) :rows))
          :views (vconcat
                  (seq-map (lambda (v)
                             (let ((scales (plist-get v :scales)))
                               (append
                                (list :id (plist-get v :id)
                                      :domains (cl-loop for ch in '(:x :y)
                                                        for s = (plist-get scales ch)
                                                        when s append
                                                        (list ch (vconcat (seq-map (lambda (x) (easel-view--fmt s x))
                                                                                   (plist-get s :domain)))))
                                      :zoomed (if (plist-get (plist-get state :domains) (easel-key (plist-get v :id))) t :false))
                                (when-let* ((sum (easel-view--visible-summary v))) (list :visible sum)))))
                           (plist-get scene :views)))
          :params (vconcat (seq-map (lambda (p)
                                      (let ((store (plist-get (plist-get state :params) (easel-key (plist-get p :name)))))
                                        (list :name (plist-get p :name) :view (plist-get p :view)
                                              :type (plist-get (plist-get p :def) :type)
                                              :bind (or (plist-get p :bind) :null)
                                              :summary (easel-params-summary store)
                                              :value (or store :null))))
                                    (easel-params-of scene)))
          :hover (if hover (list :view (plist-get hover :view) :mark (plist-get hover :mark)
                                 :datum (plist-get hover :datum)
                                 :row (easel--plist-without (plist-get hover :row) easel-params-row-key)
                                 :tooltip (or (easel-tip-tooltip scene (easel-view-plan view) hover) :null))
                   :null)
          :click (or (plist-get state :click) :null)
          :warnings (vconcat (easel-view-warnings view))
          :last-event (let ((e (car (easel-view-log view)))) (if e (plist-get e :summary) :null)))))

(defun easel-view-log-entries (view &optional n)
  "VIEW's last N (default all) log entries, oldest first."
  (let ((log (easel-view-log (easel-view-get view))))
    (vconcat (reverse (if n (seq-take log n) log)))))

(defun easel-selection (view &optional name as)
  "Rows selected in VIEW by selection NAME (default: first non-empty).
AS is rows (default, a vector of row plists), json or org."
  (let* ((view (easel-view-get view)) (scene (easel-view-scene view))
         (state (easel-view-state view))
         (param (if name (seq-find (lambda (p) (equal (plist-get p :name) name)) (easel-params-of scene))
                  (seq-find (lambda (p) (plist-get (plist-get state :params) (easel-key (plist-get p :name))))
                            (easel-params-of scene))))
         (store (and param (plist-get (plist-get state :params) (easel-key (plist-get param :name)))))
         (scene-view (and param (seq-find (lambda (v) (equal (plist-get v :id) (plist-get param :view)))
                                          (plist-get scene :views))))
         (mark (and scene-view (seq-find (lambda (m) (> (length (plist-get m :rows)) 0)) (plist-get scene-view :marks))))
         (rows (and store mark
                    (vconcat (mapcar (lambda (r) (easel--plist-without r easel-params-row-key))
                                     (seq-filter (lambda (r) (easel-params-contains store r)) (plist-get mark :rows)))))))
    (when (and name (null param))
      (easel-signal "NOT_FOUND" (format "View %s has no param %s" (easel-view-id view) name) :param name))
    (pcase (or as 'rows)
      ((or 'rows "rows") (or rows []))
      ((or 'json "json") (easel-json-encode (or rows [])))
      ((or 'org "org") (easel-view--org-table (or rows [])))
      (_ (easel-signal "INVALID_INPUT" "selection AS is rows, json or org" :as as)))))

(defun easel-view--org-table (rows)
  "ROWS as an org table string."
  (if (zerop (length rows)) ""
    (let ((keys (easel-plist-keys (aref rows 0))))
      (concat "| " (mapconcat #'easel-key-name keys " | ") " |\n|"
              (mapconcat (lambda (_) "---") keys "+") "|\n"
              (mapconcat (lambda (r) (concat "| " (mapconcat (lambda (k) (format "%s" (plist-get r k))) keys " | ") " |"))
                         rows "\n")
              "\n"))))

(provide 'easel-view)
;;; easel-view.el ends here
