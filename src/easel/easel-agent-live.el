;;; easel-agent-live.el --- live agent verbs: views, inspect, dispatch, log, selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The verbs that read or drive views in a running Emacs, reached with
;; emacsclient --eval '(easel-agent-json "inspect" "line:daily")':
;;
;;   views                              live views and what each shows
;;   open SOURCE [--data B] [--show]    open a view (and show it to the human)
;;   close VIEW                         forget a view
;;   inspect VIEW                       domains, selections, hover, visible summary
;;   dispatch VIEW EVENT|[EVENT...]     apply event/v1 (array = replay), then inspect
;;   log VIEW [--n N]                   recent events, oldest first
;;   selection VIEW [--name P] [--as rows|json|org]   the selected data
;;
;; They wrap the runtime (easel-view.el) and add nothing to it, so a
;; feature that lands in the runtime is drivable here at once.

;;; Code:

(require 'easel-agent-core)
(require 'easel-agent-verbs)
(require 'easel-view)

(declare-function easel-show "easel-mode" (view &optional target))

(defun easel-agent--view-id (verb pos)
  "The VIEW positional of VERB in POS."
  (format "%s" (easel-agent-arg-required pos 0 "a view id (see views)" verb)))

(defun easel-agent--inspect-next (id)
  "The next[] commands after looking at view ID."
  (list (easel-agent-live-cmd "dispatch" id "{\"type\":\"key\",\"key\":\"+\"}")
        (easel-agent-live-cmd "log" id)
        (easel-agent-live-cmd "selection" id)))

(defun easel-agent--view-row (id)
  "One `views' row for view ID."
  (let* ((view (easel-view-get id)) (buffer (easel-view-buffer view))
         (last (car (easel-view-log view))))
    (list :id id :template (or (easel-view-template view) :null)
          :subject (or (easel-view-subject view) :null)
          :interactive (if (easel-view-interactive view) t :false)
          :target (symbol-name (easel-view-target view))
          :buffer (if (buffer-live-p buffer) (buffer-name buffer) :null)
          :size (or (plist-get (easel-view-scene view) :size) :null)
          :rows (length (plist-get (easel-view-data view) :rows))
          :last-event (if last (plist-get last :summary) :null))))

(defun easel-agent-views (_pos _opts)
  "Answer views: every live view."
  (let ((ids (easel-view-ids)))
    (apply #'easel-agent-ok (vconcat (mapcar #'easel-agent--view-row ids))
           (if ids (list (easel-agent-live-cmd "inspect" (car ids)))
             (list (easel-agent-live-cmd "open" "TEMPLATE" :data "BINDINGS.json")
                   (easel-agent-cmd "describe" "templates"))))))

(defun easel-agent-open (pos opts)
  "Answer open SOURCE (first of POS): register a live view.
With :show in OPTS, display it."
  (let* ((source (easel-agent-arg-required pos 0 "a template name or a chart/v1 spec" "open"))
         (template (easel-agent-template-name-p source))
         (backend (easel-agent-arg-choice opts :backend easel-agent-backends "svg"))
         (view (easel-view-open (if template source (easel-agent-arg-json source))
                                :bindings (and template (easel-agent-bindings opts))
                                :id (plist-get opts :id) :subject (plist-get opts :subject)
                                :size (easel-agent-size opts backend) :target (intern backend)))
         (id (easel-view-id view)))
    (when (and (easel-true-p (plist-get opts :show)) (not noninteractive))
      (require 'easel-mode)
      (easel-show view (and (plist-get opts :backend) (intern backend))))
    (apply #'easel-agent-ok (easel-inspect view) (easel-agent--inspect-next id))))

(defun easel-agent-close (pos _opts)
  "Answer close VIEW (first of POS): forget it."
  (let ((id (easel-agent--view-id "close" pos)))
    (easel-view-close id)
    (easel-agent-ok (list :closed id) (easel-agent-live-cmd "views"))))

(defun easel-agent-inspect (pos _opts)
  "Answer inspect VIEW (first of POS): what it draws right now."
  (let ((id (easel-agent--view-id "inspect" pos)))
    (apply #'easel-agent-ok (easel-inspect id) (easel-agent--inspect-next id))))

(defun easel-agent-dispatch (pos _opts)
  "Answer dispatch VIEW EVENT (POS): apply event/v1 and inspect.
An array of events replays them."
  (let* ((id (easel-agent--view-id "dispatch" pos))
         (event (easel-agent-arg-json
                 (easel-agent-arg-required pos 1 "an event/v1 object (see describe events)" "dispatch"))))
    (easel-view-get id)
    (apply #'easel-agent-ok
           (if (vectorp event) (easel-replay id event) (easel-dispatch id event))
           (easel-agent--inspect-next id))))

(defun easel-agent-log (pos opts)
  "Answer log VIEW (first of POS): recent events, oldest first.
OPTS :n limits the count."
  (let ((id (easel-agent--view-id "log" pos)))
    (easel-agent-ok (easel-view-log-entries id (easel-agent-arg-number opts :n))
                    (easel-agent-live-cmd "inspect" id))))

(defun easel-agent-selection (pos opts)
  "Answer selection VIEW (first of POS): the selected rows.
OPTS :name picks the param and :as is rows, json or org."
  (let* ((id (easel-agent--view-id "selection" pos))
         (as (easel-agent-arg-choice opts :as '("rows" "json" "org") "rows"))
         (name (plist-get opts :name))
         (rows (easel-selection id name 'rows)))
    (easel-agent-ok (append (list :view id :param (or name :null) :as as :n (length rows))
                            (if (equal as "org") (list :org (easel-selection id name 'org))
                              (list :rows rows)))
                    (easel-agent-live-cmd "inspect" id))))

(easel-agent-register-verb
 "views" #'easel-agent-views :live t :doc "Live views: id, template, buffer, size, last event"
 :usage "views")
(easel-agent-register-verb
 "open" #'easel-agent-open :live t :doc "Open SOURCE as a live view; --show displays it"
 :usage "open SOURCE [--data B] [--id ID] [--subject S] [--backend svg|text] [--show]"
 :options (append '(:id :subject :show) easel-agent--source-options) :flags '(:show))
(easel-agent-register-verb
 "close" #'easel-agent-close :live t :doc "Forget VIEW" :usage "close VIEW")
(easel-agent-register-verb
 "inspect" #'easel-agent-inspect :live t
 :doc "Domains, selections, hovered datum and visible-range summary of VIEW" :usage "inspect VIEW")
(easel-agent-register-verb
 "dispatch" #'easel-agent-dispatch :live t
 :doc "Apply an event/v1 (or replay an array of them) to VIEW; returns inspect"
 :usage "dispatch VIEW EVENT")
(easel-agent-register-verb
 "log" #'easel-agent-log :live t :doc "VIEW's recent events, oldest first"
 :usage "log VIEW [--n N]" :options '(:n))
(easel-agent-register-verb
 "selection" #'easel-agent-selection :live t :doc "Rows selected in VIEW"
 :usage "selection VIEW [--name PARAM] [--as rows|json|org]" :options '(:name :as))

(provide 'easel-agent-live)
;;; easel-agent-live.el ends here
