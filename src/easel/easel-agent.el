;;; easel-agent.el --- one verb set and envelope for agents: Lisp, shell, emacsclient -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L7, the agent surface (design section 2 L7 and section 3).  Every
;; verb answers with bin/chart's chart/v1 envelope
;;
;;   {"contract":"chart/v1","ok":BOOL,"data":...,"reason"?:CODE,
;;    "evidence"?:{...},"next":["command",...]}
;;
;; from three entry points:
;;
;;   (easel-agent "render" "line" :data BINDINGS :backend "text")  ; Lisp, a plist
;;   bin/easel render line --data b.json --backend text            ; shell, batch
;;   emacsclient --eval '(easel-agent-json "inspect" "line:daily")' ; live Emacs
;;
;; `(easel-agent "describe" "verbs")' lists the verbs.  Failures never
;; signal: they come back with ok false, a reason code, evidence and
;; at least one next command.

;;; Code:

(require 'easel)
(require 'easel-agent-core)
(require 'easel-agent-verbs)
(require 'easel-agent-live)
(require 'easel-agent-health)

(defun easel-agent--failure-next (verb pos evidence)
  "The next[] commands for a VERB failure with EVIDENCE; POS are its positionals."
  (let* ((source (car pos))
         (live (plist-get (alist-get verb easel-agent-verbs nil nil #'equal) :live))
         (template (or (plist-get evidence :template)
                       (and (not live) (easel-agent-template-name-p source) source)))
         (label (and source (not live) (easel-agent-source-label source))))
    (pcase (plist-get evidence :code)
      ("VIEW_NOT_FOUND" (list (easel-agent-live-cmd "views")))
      ("EVENT_INVALID" (list (easel-agent-cmd "describe" "events")
                             (and source (easel-agent-live-cmd "inspect" source))))
      ((or "SLOT_MISSING" "SLOT_TYPE" "SHAPE_INVALID" "FIELD_MISSING")
       (list (and (stringp template) (easel-agent-cmd "example" template))
             (easel-agent-cmd "describe" "templates")))
      ("TRANSFORM_UNKNOWN" (list (easel-agent-cmd "describe" "transforms")))
      ("UNSUPPORTED_FEATURE" (list (easel-agent-cmd "describe" "supported")
                                   (and label (easel-agent-cmd "export" label "--vl"))))
      ("NOT_FOUND" (list (easel-agent-cmd "describe" (if (plist-get evidence :template) "templates"))))
      ("ENGINE_FAILED" (list (easel-agent-cmd "doctor")))
      (_ (list (and label (not (equal verb "check")) (easel-agent-cmd "check" label))
               (easel-agent-cmd "describe" "verbs"))))))

(defun easel-agent--json-safe (plist)
  "PLIST with every value that does not encode as JSON printed instead."
  (cl-loop for (k v) on plist by #'cddr
           append (list k (if (ignore-errors (easel-json-encode (list :v v))) v
                            (format "%S" v)))))

(defun easel-agent--error-envelope (verb pos err)
  "The failure envelope for VERB (positionals POS) after condition ERR."
  (let ((evidence (easel-agent--json-safe (easel-error-plist err))))
    (apply #'easel-agent-fail (plist-get evidence :code) evidence nil
           (or (delq nil (easel-agent--failure-next verb pos evidence))
               (list (easel-agent-cmd "describe"))))))

(defun easel-agent (verb &rest args)
  "Run agent VERB with ARGS; return the chart/v1 envelope as a plist.
VERB is a string or symbol (see `(easel-agent \"describe\" \"verbs\")').
ARGS are positionals followed by keyword options, e.g.
  (easel-agent \"render\" \"line\" :data BINDINGS :backend \"text\")
Never signals: failures are envelopes with ok `:false'."
  (let ((verb (format "%s" verb)) pos)
    (condition-case err
        (let* ((entry (easel-agent-verb verb))
               (split (easel-agent-split-args args)))
          (setq pos (car split))
          (easel-agent-check-options verb entry (cdr split))
          (funcall (plist-get entry :function) (car split) (cdr split)))
      (error (easel-agent--error-envelope verb pos err)))))

(defun easel-agent-encode (envelope)
  "ENVELOPE as compact JSON; an unencodable one becomes ENGINE_FAILED."
  (condition-case err
      (easel-json-encode envelope)
    (error (easel-json-encode
            (easel-agent-fail "ENGINE_FAILED"
                              (list :message (format "Verb result is not JSON (%s); report it"
                                                     (error-message-string err)))
                              nil (easel-agent-cmd "doctor"))))))

(defun easel-agent-json (verb &rest args)
  "Like `easel-agent' with VERB and ARGS, but return the envelope as JSON.
For emacsclient --eval, which prints the result as a Lisp string:
pipe it through jq -r . to get the JSON."
  (easel-agent-encode (apply #'easel-agent verb args)))

(provide 'easel-agent)
;;; easel-agent.el ends here
