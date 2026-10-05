;;; easel-agent-verbs.el --- stateless agent verbs: describe to export -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The verbs that need no live Emacs, so bin/easel runs them in batch:
;;
;;   describe [SECTION]                 registries, verbs, events, reasons
;;   example TEMPLATE                   bindings that render as-is
;;   check SOURCE [--data B]            reason codes, before drawing
;;   explain SOURCE --stage S           the artifact at resolve|compile|scene
;;   render SOURCE --backend text|svg   the chart (text: the agent's eyes)
;;   export SOURCE --vl                 resolved pure Vega-Lite for bin/chart
;;
;; SOURCE is a template name (bound with --data) or a chart/v1 spec
;; (inline JSON, a .json file, or "-" for stdin).

;;; Code:

(require 'easel-agent-core)
(require 'easel-describe)
(require 'easel-resolve)
(require 'easel-compile)
(require 'easel-scene)
(require 'easel-svg)
(require 'easel-text)
(require 'easel-event)
(require 'easel-chart)
(require 'easel-conformance)

(defconst easel-agent-backends '("text" "svg") "Render backends.")

(defconst easel-agent-stages '("resolve" "compile" "scene") "Stages of explain.")

(defconst easel-agent--source-options '(:data :backend :width :height :cols :rows)
  "Options shared by the verbs that take a SOURCE.")

(defun easel-agent-resolve-source (verb pos opts)
  "Resolve VERB's SOURCE (first of POS) with OPTS's bindings.
Return (:spec RESOLVED :template NAME-or-nil :label LABEL :words W),
W being how next[] commands name the source and its bindings."
  (let ((source (easel-agent-arg-required pos 0 "a template name or a chart/v1 spec" verb)))
    (if (easel-agent-template-name-p source)
        (list :spec (easel-resolve source (easel-agent-bindings opts)) :template source :label source
              :words (list source "--data" (easel-agent-data-word (plist-get opts :data))))
      (when (plist-get opts :data)
        (easel-signal "INVALID_INPUT"
                      (format "--data binds template slots, but %s is not a template; templates: %s"
                              (easel-agent-source-label source)
                              (string-join (easel-template-names) ", "))
                      :option "data"))
      (list :spec (easel-resolve-spec (easel-agent-arg-json source)) :template nil
            :label (easel-agent-source-label source)
            :words (list (easel-agent-source-label source))))))

(defun easel-agent-data-word (data)
  "How next[] commands pass bindings DATA: its file, else BINDINGS.json."
  (if (and (stringp data) (not (equal data "-")) (not (easel-agent-json-text-p data)))
      data "BINDINGS.json"))

(defun easel-agent-next (verb src &rest words)
  "A next[] command running VERB on SRC's source (and bindings) with WORDS."
  (apply #'easel-agent-cmd verb (append (plist-get src :words) words)))

(defun easel-agent-size (opts backend)
  "The compile SIZE that OPTS ask for under BACKEND, or nil (spec's own)."
  (let ((w (easel-agent-arg-number opts :width)) (h (easel-agent-arg-number opts :height))
        (c (easel-agent-arg-number opts :cols)) (r (easel-agent-arg-number opts :rows)))
    (cond ((and c r (equal backend "text")) (list :cols c :rows r))
          ((and w h) (cons w h))
          ((or w h c r)
           (easel-signal "INVALID_INPUT"
                         (if (equal backend "text") "Give --cols and --rows together (or --width and --height)"
                           "Give --width and --height together; --cols/--rows are for --backend text")
                         :option "size")))))

(defun easel-agent-compile (spec opts backend)
  "Compile resolved SPEC for BACKEND at OPTS's size; return the scene."
  (easel-compile spec :size (easel-agent-size opts backend) :target (intern backend)))

(defun easel-agent-draw (scene backend)
  "SCENE drawn by BACKEND as a plain string."
  (if (equal backend "text") (substring-no-properties (easel-text-render scene))
    (easel-svg-render scene)))

;;; describe

(defun easel-agent--verbs-describe ()
  "Return every verb as describe data."
  (vconcat (mapcar (lambda (name)
                     (let ((e (easel-agent-verb name)))
                       (list :name name :doc (plist-get e :doc) :usage (plist-get e :usage)
                             :live (if (plist-get e :live) t :false)
                             :options (vconcat (mapcar #'easel-key-name (plist-get e :options))))))
                   (easel-agent-verb-names))))

(defun easel-agent--describe-extra ()
  "The agent surface's own describe sections."
  (list :contract easel-agent-contract
        :verbs (easel-agent--verbs-describe)
        :events (list :types (vconcat easel-event-types) :keys (vconcat easel-event-keys))
        :reasons (vconcat (mapcar (lambda (e) (list :code (car e) :title (nth 2 e)))
                                  easel-reason-codes))))

(defun easel-agent-describe (pos _opts)
  "Answer describe [SECTION] (POS): registries, verbs, events, reasons."
  (let* ((section (car pos))
         (extra (easel-agent--describe-extra))
         (key (and section (easel-key (format "%s" section))))
         (first (car (easel-template-names))))
    (easel-agent-ok (cond ((null section) (append (easel-describe) extra))
                          ((plist-member extra key) (list key (plist-get extra key)))
                          (t (easel-describe section)))
                    (and first (easel-agent-cmd "example" first))
                    (easel-agent-cmd "check" "SPEC.json"))))

;;; example

(defun easel-agent-example (pos _opts)
  "Answer example TEMPLATE (first of POS): bindings that render as-is."
  (let* ((name (easel-agent-arg-required pos 0 "a template name" "example"))
         (bindings (easel-template-example name))
         (file (easel-template-example-file (easel-template-get name))))
    (easel-agent-ok bindings
                    (easel-agent-cmd "check" name "--data" file)
                    (easel-agent-cmd "render" name "--data" file "--backend" "text"))))

;;; check

(defun easel-agent--check-findings (spec)
  "SPEC's check findings as (ERRORS . UNSUPPORTED)."
  (let (errors unsupported)
    (dolist (f (easel-spec-check spec))
      (if (equal (plist-get f :code) "UNSUPPORTED_FEATURE") (push f unsupported) (push f errors)))
    (cons (nreverse errors) (nreverse unsupported))))

(defun easel-agent-check (pos opts)
  "Answer check SOURCE (POS, bound with OPTS :data) without drawing.
Resolve, validate and compile.
Unsupported features are not failures: the chart still shows, as a
static bin/chart image, so they come back as warnings with native false."
  (let* ((src (easel-agent-resolve-source "check" pos opts))
         (spec (plist-get src :spec))
         (findings (easel-agent--check-findings spec))
         (errors (car findings)) (unsupported (cdr findings))
         (data (list :template (or (plist-get src :template) :null)
                     :hash (easel-resolve-hash spec)
                     :native (if unsupported :false t)
                     :warnings (vconcat unsupported))))
    (cond
     (errors
      (easel-agent-fail (plist-get (car errors) :code) (car errors)
                        (append data (list :findings (vconcat errors)))
                        (easel-agent-next "explain" src "--stage" "resolve")
                        (easel-agent-cmd "describe" "supported")))
     (unsupported
      (easel-agent-ok data (easel-agent-next "export" src "--vl")
                      (easel-agent-cmd "describe" "supported")))
     (t
      (easel-agent-compile spec opts "text")
      (easel-agent-ok data
                      (easel-agent-next "render" src "--backend" "text")
                      (easel-agent-next "explain" src "--stage" "scene"))))))

;;; explain

(defun easel-agent--scene-compiled (scene)
  "Return the compile stage of SCENE: views, scales and item counts."
  (let ((summary (easel-scene-summary scene)))
    (easel-scene-round
     (list :size (plist-get summary :size)
           :views (vconcat
                   (seq-mapn (lambda (sv v)
                               (append sv (list :scales
                                                (cl-loop for (ch s) on (plist-get v :scales) by #'cddr
                                                         append (list ch s)))))
                             (plist-get summary :views) (plist-get scene :views)))
           :params (vconcat (mapcar (lambda (p) (list :name (plist-get p :name) :view (plist-get p :view)))
                                    (plist-get scene :params)))))))

(defun easel-agent-explain (pos opts)
  "Answer explain SOURCE (POS) with OPTS :stage: one layer's artifact.
The stage is resolve, compile or scene."
  (let* ((stage (easel-agent-arg-choice opts :stage easel-agent-stages "resolve"))
         (backend (easel-agent-arg-choice opts :backend easel-agent-backends "svg"))
         (src (easel-agent-resolve-source "explain" pos opts))
         (spec (plist-get src :spec))
         (artifact (pcase stage
                     ("resolve" spec)
                     ("compile" (easel-agent--scene-compiled (easel-agent-compile spec opts backend)))
                     (_ (easel-scene-round (easel-agent-compile spec opts backend))))))
    (easel-agent-ok (list :stage stage :backend backend :template (or (plist-get src :template) :null)
                          :hash (easel-resolve-hash spec) :artifact artifact)
                    (easel-agent-next "render" src "--backend" "text")
                    (unless (equal stage "scene") (easel-agent-next "explain" src "--stage" "scene")))))

;;; render

(defun easel-agent-render (pos opts)
  "Answer render SOURCE (POS) with OPTS :backend text or svg.
A spec outside the native subset renders as SVG through bin/chart
when it is installed; otherwise UNSUPPORTED_FEATURE."
  (let* ((backend (easel-agent-arg-choice opts :backend easel-agent-backends "text"))
         (src (easel-agent-resolve-source "render" pos opts))
         (spec (plist-get src :spec))
         (out (condition-case err
                  (let ((scene (easel-agent-compile spec opts backend)))
                    (list :output (easel-agent-draw scene backend) :size (plist-get scene :size)
                          :static :false))
                (easel-unsupported-feature
                 (if (and (equal backend "svg") (easel-chart-available-p))
                     (list :output (easel-chart-build spec "svg") :size :null :static "bin/chart")
                   (signal (car err) (cdr err)))))))
    (easel-agent-ok (append (list :backend backend :template (or (plist-get src :template) :null)
                                  :hash (easel-resolve-hash spec))
                            out)
                    (easel-agent-next "export" src "--vl")
                    (easel-agent-next "explain" src "--stage" "scene")
                    (and (plist-get src :template)
                         (easel-agent-live-cmd "open" (plist-get src :template)
                                               :data (nth 2 (plist-get src :words)))))))

;;; export

(defun easel-agent-export (pos opts)
  "Answer export SOURCE (POS, OPTS): the resolved, standalone Vega-Lite."
  (let* ((src (easel-agent-resolve-source "export" pos opts))
         (spec (plist-get src :spec)))
    (easel-agent-ok spec
                    "chart check SPEC.vl.json"
                    "chart build SPEC.vl.json --out chart.svg")))

(easel-agent-register-verb
 "describe" #'easel-agent-describe
 :doc "Templates with slots and examples, transforms, adapters, supported features, verbs, events, reasons"
 :usage "describe [templates|transforms|adapters|supported|verbs|events|reasons]")
(easel-agent-register-verb
 "example" #'easel-agent-example :doc "Bindings for TEMPLATE that render as-is"
 :usage "example TEMPLATE")
(easel-agent-register-verb
 "check" #'easel-agent-check :doc "Resolve, validate and compile without drawing; reason codes with paths"
 :usage "check SOURCE [--data BINDINGS]" :options '(:data))
(easel-agent-register-verb
 "explain" #'easel-agent-explain :doc "The intermediate artifact at one layer"
 :usage "explain SOURCE [--data B] --stage resolve|compile|scene [--backend svg|text] [--width W --height H]"
 :options (cons :stage easel-agent--source-options))
(easel-agent-register-verb
 "render" #'easel-agent-render :doc "The chart as text (deterministic) or SVG"
 :usage "render SOURCE [--data B] [--backend text|svg] [--cols C --rows R | --width W --height H]"
 :options easel-agent--source-options)
(easel-agent-register-verb
 "export" #'easel-agent-export :doc "Resolved pure Vega-Lite for bin/chart and documents"
 :usage "export SOURCE [--data B] --vl" :options '(:data :vl) :flags '(:vl))

(provide 'easel-agent-verbs)
;;; easel-agent-verbs.el ends here
