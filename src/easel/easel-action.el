;;; easel-action.el --- click targets: the action registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.1; legend and param keys grow in fc-qx1.5).  A
;; click (mouse-1, RET at point, or a click event an agent sends)
;; that lands on a datum becomes a click target, recorded in the view
;; state as :click and shown by `easel-inspect'.  The target then runs
;; one named action from `easel-actions'.  Which one, first match wins:
;;
;;   1. `easel-action-bind' on this view, keyed by mark id or param name
;;   2. the template's "x-easel": {"actions": {KEY: NAME}}, same keys
;;   3. key "*" in either
;;   4. "open-href" when the datum has an encoding.href
;;
;; A param key matches when the click sits in a view whose point
;; selection param of that name fires on click.  Built-in actions:
;; open-href, echo (the tooltip in the echo area) and copy-row (the
;; row as JSON on the kill ring).  An action receives the target plist
;; and the view; what it returns (a string or number) is recorded as
;; :result, and a failure as :error (ENGINE_FAILED with :action), so a
;; click never breaks dispatch.  Binding an unknown action signals
;; NOT_FOUND with :action.
;; Set `easel-action-inhibit' to record the action without running it;
;; `easel-replay' never re-runs actions.

;;; Code:

(require 'easel-core)
(require 'easel-template)
(require 'easel-params)
(require 'easel-view)
(require 'easel-tip)

(defvar easel-actions nil
  "Registered actions: alist of (NAME . (:fn FN :doc DOC)).")

(defvar easel-action-inhibit nil
  "Non-nil records each click's action in the view state without running it.")

(defvar easel-action-browse-function #'browse-url
  "Function the open-href action calls with the URL.")

(defvar easel-action--bindings (make-hash-table :test 'eq :weakness 'key)
  "Per-view action bindings: view -> plist of (KEY NAME).")

(cl-defun easel-action-define (name fn &key doc)
  "Register action NAME (a string) running FN with the click target and view.
FN's string or number return value is recorded as the click's :result."
  (unless (and (stringp name) (functionp fn))
    (easel-signal "INVALID_INPUT" "An action is a name (string) and a function" :action name))
  (setf (alist-get name easel-actions nil nil #'equal) (list :fn fn :doc (or doc "")))
  name)

(defun easel-action-names ()
  "Sorted names of every registered action."
  (sort (mapcar #'car easel-actions) #'string<))

(defun easel-action-describe ()
  "Every action as (:name :doc), JSON-ready."
  (vconcat (mapcar (lambda (name) (list :name name :doc (plist-get (alist-get name easel-actions nil nil #'equal) :doc)))
                   (easel-action-names))))

(defun easel-action--check (name)
  "Signal NOT_FOUND (with :action) unless NAME is registered."
  (unless (alist-get name easel-actions nil nil #'equal)
    (easel-signal "NOT_FOUND"
                  (format "No action %S; define it with `easel-action-define' or use one of: %s"
                          name (string-join (easel-action-names) ", "))
                  :action name)))

(defun easel-action-bind (view key action)
  "Make clicks on mark or param KEY (or \"*\") in VIEW run ACTION.
ACTION nil removes the binding.  Returns VIEW's bindings."
  (let ((view (easel-view-get view)))
    (when action (easel-action--check action))
    (puthash view (if action
                      (easel-plist-put (gethash view easel-action--bindings) (easel-key key) action)
                    (easel--plist-without (gethash view easel-action--bindings) (easel-key key)))
             easel-action--bindings)))

(defun easel-action--template-actions (view)
  "The x-easel actions of VIEW's template, as a plist, or nil."
  (when-let* ((name (easel-view-template view)))
    (plist-get (plist-get (easel-template-get name) :meta) :actions)))

(defun easel-action--keys (view target)
  "Binding keys TARGET in VIEW answers to, most specific first."
  (append (list (plist-get target :mark))
          (mapcar (lambda (p) (plist-get p :name))
                  (seq-filter (lambda (p) (and (equal (plist-get p :view) (plist-get target :view))
                                               (equal (plist-get (plist-get p :def) :type) "point")
                                               (equal (plist-get (plist-get p :def) :on) "click")))
                              (easel-params-of (easel-view-scene view))))
          (list "*")))

(defun easel-action-for (view target)
  "The action NAME a click on TARGET in VIEW runs, or nil."
  (let ((tables (list (gethash view easel-action--bindings)
                      (easel-action--template-actions view))))
    (or (cl-loop for key in (easel-action--keys view target)
                 thereis (cl-loop for table in tables
                                  thereis (plist-get table (easel-key key))))
        (and (stringp (plist-get target :href)) "open-href"))))

(defun easel-action-run (view target)
  "Run TARGET's action in VIEW; return TARGET with :action and its outcome."
  (let ((name (easel-action-for view target)))
    (append target
            (list :action (or name :null))
            (cond
             ((null name) nil)
             ((or easel-action-inhibit easel-view-replaying) (list :ran :false))
             (t (condition-case err
                    (progn (easel-action--check name)
                           (let ((result (funcall (plist-get (alist-get name easel-actions nil nil #'equal) :fn)
                                                  target view)))
                             (append (list :ran t) (when (or (stringp result) (numberp result))
                                                     (list :result result)))))
                  (easel-error (list :ran :false :error (easel-error-plist err)))
                  (error (list :ran :false
                               :error (list :code "ENGINE_FAILED" :action name
                                            :message (format "Action %s failed: %s; fix the action function"
                                                             name (error-message-string err)))))))))))

(defun easel-action--on-dispatch (view event old-state old-scene)
  "Record and act on a click EVENT made in OLD-SCENE under OLD-STATE.
A click on empty space clears :click; other events leave it alone."
  (when (easel-tip-click-px event old-state)
    (let ((target (easel-tip-click-target event old-state old-scene (easel-view-plan view))))
      (setf (easel-view-state view)
            (easel-plist-put (easel-view-state view) :click (and target (easel-action-run view target)))))))

(add-hook 'easel-view-dispatch-functions #'easel-action--on-dispatch)

;;; Built-in actions

(easel-action-define
 "open-href" (lambda (target _view)
               (let ((href (plist-get target :href)))
                 (unless (stringp href)
                   (easel-signal "ENGINE_FAILED" "This datum has no href; add encoding.href to the mark"
                                 :action "open-href"))
                 (funcall easel-action-browse-function href)
                 href))
 :doc "Open the datum's encoding.href with `easel-action-browse-function'.")

(easel-action-define
 "echo" (lambda (target _view)
          (let ((text (or (easel-tip-text (let ((tip (plist-get target :tooltip))) (and (vectorp tip) tip)))
                          (easel-json-encode (plist-get target :row)))))
            (message "%s" text)
            text))
 :doc "Show the datum's tooltip (or its row) in the echo area.")

(easel-action-define
 "copy-row" (lambda (target _view)
              (let ((json (easel-json-encode (plist-get target :row))))
                (kill-new json)
                json))
 :doc "Copy the datum's row as JSON to the kill ring.")

(provide 'easel-action)
;;; easel-action.el ends here
