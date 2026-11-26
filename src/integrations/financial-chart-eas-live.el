;;; financial-chart-eas-live.el --- markup events in the readout, live verbs and doctor rows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The live end of the markups tower (fc-gty.10):
;;
;; - A composed chart whose annotations come from detectors carries its
;;   events in x-eas.markups ({id, x, label, direction, strength}), and
;;   its readout gains a `markup-events' component: hovering a bar shows
;;   the marker events on it (patterns, pivots, VWAP crosses: "H 0.30"),
;;   coloured by direction.
;; - Marker and level rows carry the event "id", so eas `inspect' (the
;;   hovered row) and `selection' return ids.
;; - Two live verbs for the eas agent door:
;;     markups VIEW            the view's events, and those on the hovered bar
;;     focus VIEW ID [--bars N]   zoom VIEW to N bars (20) around event ID
;;   as `(eas-agent-json "focus" "fc:tsm" "pattern.hammer@2026-04-27")'.
;; - `financial-chart-markups-doctor-checks': registry lint, TA-Lib
;;   oracle, preset lint and the evidence directory.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-detector-api)
(require 'financial-chart-eas-palette)
(require 'financial-chart-eas-readout)
(require 'financial-chart-eas-presets)
(require 'financial-chart-evaluate)

(declare-function eas-agent-register-verb "eas-agent-core")
(declare-function eas-agent-ok "eas-agent-core")
(declare-function eas-agent-live-cmd "eas-agent-core")
(declare-function eas-agent-arg-required "eas-agent-core")
(declare-function eas-agent-arg-number "eas-agent-core")
(declare-function eas-agent-fail "eas-agent-core")
(declare-function financial-chart-chart-events "financial-chart-eas-markups" (chart))
(declare-function financial-chart-candle-pattern-list "financial-chart-patterns" ())

;;; Events in the spec

(defun financial-chart-live-markups (chart xs)
  "The events CHART's annotations show, for x-eas.markups.
XS are the bars' x positions (trading slots or times).  Each is
\(:id :x :marker :label :direction :strength); hand-placed annotations are
left out.  :marker is true for events a view marks on one bar."
  (let ((xs (vconcat xs)))
    (vconcat
     (cl-loop for e in (financial-chart-chart-events chart)
              for d = (plist-get e :detector)
              unless (string-prefix-p "constant." d)
              for mark = (plist-get (financial-chart-detector d) :mark)
              collect (list :id (plist-get e :id) :x (aref xs (plist-get e :index))
                            :marker (if (equal (plist-get mark :type) "marker") t :false)
                            :label (or (plist-get mark :label)
                                       (car (last (split-string d "\\."))))
                            :direction (plist-get e :direction)
                            :strength (/ (fround (* 100 (plist-get e :strength))) 100.0)
                            :verdict (or (plist-get (financial-chart-evidence-verdict d) :status) "untested"))))))

(defun financial-chart-live-markups-p (chart)
  "Non-nil when CHART has an annotation drawn from a detector."
  (cl-loop for pane in (cons (plist-get chart :price) (append (plist-get chart :panes) nil))
           thereis (cl-loop for a across (vconcat (plist-get pane :annotations))
                            thereis (and (listp a)
                                         (member (plist-get a :type)
                                                 '("patterns" "support-resistance" "zigzag" "auto-trendlines"
                                                   "fib-extension" "pitchfork" "measured-move" "channel"
                                                   "anchored-vwap"))))))

;;; Readout

(defun financial-chart-live--events-at (events x &optional markers)
  "EVENTS (x-eas.markups rows) whose x is X; only markers when MARKERS."
  (seq-filter (lambda (e) (and (equal (plist-get e :x) x) (or (not markers) (eq (plist-get e :marker) t))))
              (append events nil)))

(eas-define-component
 "markup-events"
 :doc "The markup events on the hovered bar (or the last bar): each one's
short label and strength, coloured UP for bullish and DOWN for bearish."
 :props '((events :type array :doc "[{id, x, label, direction, strength}] (x-eas.markups)")
          (up :type color) (down :type color)
          (x_field :type string :default "time")
          (priority :type number :default 20))
 :render (lambda (props ctx)
           (let* ((bar (financial-chart-readout--bar ctx))
                  (x (and bar (plist-get bar (eas-key (plist-get props :x_field)))))
                  (here (financial-chart-live--events-at (plist-get props :events) x t))
                  (up (or (plist-get props :up) financial-chart-palette-up))
                  (down (or (plist-get props :down) financial-chart-palette-down)))
             (financial-chart-readout--spaced
              (seq-map-indexed
               (lambda (e i)
                 (financial-chart-readout--atom
                  (plist-get e :label)
                  (concat (format "%.2f" (plist-get e :strength))
                          (if (member (plist-get e :verdict) '(nil "untested")) ""
                            (concat " " (plist-get e :verdict))))
                  (pcase (plist-get e :direction)
                    ("bullish" (list :color up)) ("bearish" (list :color down)) (_ nil))
                  :priority (- (plist-get props :priority) (* i 0.01))))
               here)))))

;;; Live verbs

(defvar financial-chart-live--markups (make-hash-table :test #'equal)
  "View id to the x-eas.markups of the spec it opened with.
Resolving strips x-eas from a view's spec, so `eas-view-open-functions'
keeps them here.")

(defun financial-chart-live--remember (view source)
  "Keep the x-eas.markups of SOURCE, the spec VIEW opened with."
  (let ((spec (cond ((and (listp source) (keywordp (car source))) source)
                    ((and (stringp source) (string-prefix-p "{" (string-trim-left source)))
                     (ignore-errors (eas-json-parse source))))))
    (if-let* ((markups (plist-get (plist-get spec :x-eas) :markups)))
        (puthash (eas-view-id view) markups financial-chart-live--markups)
      (remhash (eas-view-id view) financial-chart-live--markups))))

(add-hook 'eas-view-open-functions #'financial-chart-live--remember)

(defun financial-chart-live--view-events (view)
  "The x-eas.markups VIEW opened with."
  (append (gethash (eas-view-id view) financial-chart-live--markups) nil))

(defun financial-chart-live-markups-verb (pos _opts)
  "Answer markups VIEW (first of POS): its events and those on the hovered bar."
  (let* ((id (format "%s" (eas-agent-arg-required pos 0 "a view id (see views)" "markups")))
         (view (eas-view-get id))
         (events (financial-chart-live--view-events view))
         (hover (plist-get (plist-get (eas-inspect id) :hover) :row))
         (x (and hover (plist-get hover :time))))
    (eas-agent-ok (list :view id :n (length events) :events (vconcat events)
                        :hovered (vconcat (if (plist-get hover :id)
                                              (seq-filter (lambda (e) (equal (plist-get e :id) (plist-get hover :id))) events)
                                            (financial-chart-live--events-at events x))))
                  (eas-agent-live-cmd "focus" id "ID"))))

(defun financial-chart-live-focus-event (view id &optional bars)
  "The event/v1 zooming VIEW to BARS bars (20) around markup event ID, or nil."
  (when-let* ((e (seq-find (lambda (e) (equal (plist-get e :id) id)) (financial-chart-live--view-events view))))
    (let* ((x (plist-get e :x)) (half (/ (or bars 20) 2)))
      (list :type "brush" :param "zoom" :x (vector (- x half) (+ x half))))))

(defun financial-chart-live-focus-verb (pos opts)
  "Answer focus VIEW ID (POS): zoom VIEW around event ID; OPTS :bars is the width."
  (let* ((id (format "%s" (eas-agent-arg-required pos 0 "a view id (see views)" "focus")))
         (event-id (eas-agent-arg-required pos 1 "a markup event id (see markups)" "focus"))
         (view (eas-view-get id))
         (event (financial-chart-live-focus-event view event-id (eas-agent-arg-number opts :bars))))
    (if (not event)
        (eas-agent-fail "NOT_FOUND" (list :view id :id event-id
                                          :message (format "View %s has no markup event %s" id event-id))
                        nil (eas-agent-live-cmd "markups" id))
      (eas-agent-ok (append (list :focused event-id) (eas-dispatch id event))
                    (eas-agent-live-cmd "markups" id)))))

(defun financial-chart-live-register-verbs ()
  "Register the markups and focus verbs with the eas agent door."
  (when (fboundp 'eas-agent-register-verb)
    (eas-agent-register-verb
     "markups" #'financial-chart-live-markups-verb :live t
     :doc "Markup events of a financial-chart VIEW, and those on the hovered bar" :usage "markups VIEW")
    (eas-agent-register-verb
     "focus" #'financial-chart-live-focus-verb :live t
     :doc "Zoom a financial-chart VIEW around markup event ID" :usage "focus VIEW ID [--bars N]"
     :options '(:bars))))

(with-eval-after-load 'eas-agent-core (financial-chart-live-register-verbs))

;;; Doctor

(defun financial-chart-markups-doctor-checks ()
  "Health rows of the markups tower: (:name :status :detail :remediation)."
  (let* ((lint (financial-chart-detector-lint))
         (oracle-file (expand-file-name "test/fixtures/detectors/talib.json" financial-chart-root))
         (presets (financial-chart-presets-lint))
         (evidence (and (boundp 'financial-chart-evidence-directory)
                        (symbol-value 'financial-chart-evidence-directory))))
    (list
     (list :name "markups:registry" :status (if lint 'fail 'pass)
           :detail (if lint (mapconcat (lambda (p) (format "%s: %s" (car p) (cdr p))) lint "; ")
                     (format "%d detectors, each cited, typed, with lag, invariance and example"
                             (length financial-chart-detector-registry)))
           :remediation (and lint "fix the entry's metadata in its financial-chart-register-detector call"))
     (if (not (file-readable-p oracle-file))
         (list :name "markups:oracle" :status 'skip :detail "no TA-Lib goldens (a package install has none)"
               :remediation "test/oracles/talib-goldens in a checkout")
       (let* ((gold (eas-json-read-file oracle-file))
              (bars (seq-take (eas-json-read-file (expand-file-name "oracle-bars.json" (file-name-directory oracle-file)))
                              1000))
              (bad (cl-loop for name in (financial-chart-detector-names)
                            for e = (financial-chart-detector name)
                            for o = (plist-get e :oracle)
                            for g = (and o (plist-get (plist-get gold :functions)
                                                      (intern (concat ":" (string-remove-prefix "talib:" o)))))
                            for ours = (and g (mapcar (lambda (ev) (plist-get ev :index))
                                                      (financial-chart-detect name bars (plist-get e :oracle-params))))
                            for theirs = (and g (cl-loop for h across (plist-get g :hits)
                                                         when (< (aref h 0) 1000) collect (aref h 0)))
                            when (and g (not (cl-subsetp ours theirs)))
                            collect name)))
         (list :name "markups:oracle" :status (if bad 'fail 'pass)
               :detail (if bad (format "disagree with TA-Lib: %s" (string-join bad ", "))
                         (format "TA-Lib %s goldens: every oracle'd detector within its function on 1000 bars"
                                 (plist-get gold :talib)))
               :remediation (and bad "make test (financial-chart-harness-talib-oracle) shows the bars"))))
     (list :name "markups:presets" :status (if presets 'fail 'pass)
           :detail (if presets (mapconcat (lambda (p) (format "%s: %s" (car p) (cdr p))) presets "; ")
                     (format "%d presets expand without cycles" (length (financial-chart-presets-list))))
           :remediation (and presets "(financial-chart-presets-lint)"))
     (let* ((verdicts (and evidence (file-directory-p evidence)
                           (cl-loop for name in (financial-chart-detector-names)
                                    for v = (financial-chart-evidence-verdict name)
                                    when v collect (cons name v))))
            (stale (cl-loop for (name . v) in verdicts when (eq (plist-get v :stale) t) collect name)))
       (list :name "markups:evidence"
             :status (cond ((null evidence) 'skip) ((not (file-directory-p evidence)) 'fail) (stale 'fail) (t 'pass))
             :detail (cond ((null evidence) "no evidence directory set: every detector is untested")
                           ((not (file-directory-p evidence)) (format "%s is not a directory" evidence))
                           (stale (format "stale evidence (code changed or > 90 days): %s" (string-join stale ", ")))
                           (t (format "%d detectors with evidence in %s, none stale" (length verdicts) evidence)))
             :remediation (cond ((not (and evidence (file-directory-p evidence))) "set financial-chart-evidence-directory")
                                (stale "(financial-chart-evaluate DETECTORS UNIVERSE)")))))))

(provide 'financial-chart-eas-live)
;;; financial-chart-eas-live.el ends here
