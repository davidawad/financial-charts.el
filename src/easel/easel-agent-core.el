;;; easel-agent-core.el --- the chart/v1 envelope, verb registry and arguments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L7 plumbing shared by every agent verb.  A verb is a function of
;; (POSITIONALS OPTIONS) returning an envelope, which `easel-agent-ok'
;; and `easel-agent-fail' build:
;;
;;   {"contract": "chart/v1", "ok": true|false, "data": ...,
;;    "reason": CODE?, "evidence": {...}?, "next": ["command", ...]}
;;
;; REASON is a section 5 reason code (shared with bin/chart), EVIDENCE
;; the failure plist (:message, plus :path :index :field ... when
;; known) and NEXT runnable commands: `bin/easel ...' for stateless
;; verbs, `emacsclient --eval ...' for live ones.  Every failure
;; carries at least one.
;;
;; Arguments look the same from every surface: positionals first, then
;; options.  The shell spells `--backend text' and `--vl'; Lisp spells
;; :backend "text" and :vl t.  Option values may be strings (the shell)
;; or parsed values (Lisp); the `easel-agent-arg-*' helpers take both.

;;; Code:

(require 'easel-core)
(require 'easel-template)

(defconst easel-agent-contract "chart/v1"
  "The envelope contract every verb returns (bin/chart's).")

(defvar easel-agent-verbs nil
  "Registered verbs: alist of (NAME . PLIST).
PLIST has :function :doc :usage :options :flags and :live.")

(defvar easel-agent-stdin-function nil
  "Function of no arguments returning stdin as a string, or nil.
bin/easel sets it so that an argument \"-\" reads stdin.")

(defvar easel-agent-shell-program "bin/easel"
  "How next[] commands spell the shell entry point.")

(cl-defun easel-agent-register-verb (name function &key doc usage options flags live)
  "Register verb NAME (a string) implemented by FUNCTION.
FUNCTION takes (POSITIONALS OPTIONS) and returns an envelope.  DOC is
one line, USAGE the shell synopsis.  OPTIONS are the keywords the verb
accepts; FLAGS the subset that take no value on the command line.
LIVE non-nil means the verb reads views in a running Emacs."
  (setf (alist-get name easel-agent-verbs nil nil #'equal)
        (list :function function :doc doc :usage usage :options options
              :flags flags :live live))
  name)

(defun easel-agent-verb-names ()
  "Sorted names of every registered verb."
  (sort (mapcar #'car easel-agent-verbs) #'string<))

(defun easel-agent-verb (name)
  "The registry entry of verb NAME or signal INVALID_INPUT."
  (or (alist-get name easel-agent-verbs nil nil #'equal)
      (easel-signal "INVALID_INPUT"
                    (format "Unknown verb %S; verbs: %s" name
                            (string-join (easel-agent-verb-names) ", "))
                    :verb name)))

;;; Envelope

(defun easel-agent-ok (data &rest next)
  "A successful envelope carrying DATA with NEXT commands (nil dropped)."
  (list :contract easel-agent-contract :ok t :data data
        :next (vconcat (delq nil next))))

(defun easel-agent-fail (reason evidence data &rest next)
  "A failed envelope: REASON code, EVIDENCE plist, DATA and NEXT commands."
  (list :contract easel-agent-contract :ok :false :data (or data :null)
        :reason reason :evidence evidence
        :next (vconcat (delq nil next))))

;;; next[] commands

(defun easel-agent--shell-word (word)
  "WORD quoted for a POSIX shell when it needs it."
  (let ((word (format "%s" word)))
    (if (string-match-p "\\`[-A-Za-z0-9_./:=@,+%]+\\'" word) word
      (concat "'" (replace-regexp-in-string "'" "'\\\\''" word t t) "'"))))

(defun easel-agent-cmd (verb &rest words)
  "A `bin/easel VERB WORDS...' command line; nil WORDS are dropped."
  (mapconcat #'easel-agent--shell-word
             (cons easel-agent-shell-program (cons verb (delq nil words)))
             " "))

(defun easel-agent-live-cmd (verb &rest args)
  "An emacsclient command line running (easel-agent-json VERB ARGS...)."
  (concat "emacsclient --eval "
          (easel-agent--shell-word
           (format "(easel-agent-json %s)"
                   (mapconcat #'prin1-to-string (cons verb (delq nil args)) " ")))))

;;; Arguments

(defun easel-agent-split-args (args)
  "Split ARGS into (POSITIONALS . OPTIONS).
Positionals are the leading non-keyword elements; the rest must be
keyword/value pairs."
  (let (pos)
    (while (and args (not (keywordp (car args))))
      (push (pop args) pos))
    (let ((opts args))
      (while opts
        (unless (and (keywordp (car opts)) (cdr opts))
          (easel-signal "INVALID_INPUT"
                        (format "Expected option keyword and value, got %S; positionals come first"
                                (car opts))))
        (setq opts (cddr opts))))
    (cons (nreverse pos) args)))

(defun easel-agent-check-options (verb entry opts)
  "Signal INVALID_INPUT when OPTS has a key VERB's ENTRY does not accept."
  (let ((allowed (plist-get entry :options)))
    (dolist (key (easel-plist-keys opts))
      (unless (memq key allowed)
        (easel-signal "INVALID_INPUT"
                      (format "%s takes no option --%s; options: %s" verb (easel-key-name key)
                              (if allowed
                                  (mapconcat (lambda (k) (concat "--" (easel-key-name k))) allowed " ")
                                "none"))
                      :option (easel-key-name key))))))

(defun easel-agent--stdin ()
  "Stdin as a string, or signal INVALID_INPUT when there is none."
  (if easel-agent-stdin-function (funcall easel-agent-stdin-function)
    (easel-signal "INVALID_INPUT"
                  "\"-\" reads stdin, which only bin/easel has; pass the value itself")))

(defun easel-agent-json-text-p (value)
  "Return non-nil when string VALUE is inline JSON (an object or array)."
  (and (stringp value) (string-match-p "\\`[ \t\n]*[[{]" value)))

(defun easel-agent-arg-json (value)
  "VALUE as a parsed JSON value.
A string is \"-\" (stdin), inline JSON or a readable JSON file;
anything else is returned as it is (already parsed)."
  (cond ((not (stringp value)) value)
        ((equal value "-") (easel-json-parse (easel-agent--stdin)))
        ((easel-agent-json-text-p value) (easel-json-parse value))
        ((file-readable-p value) (easel-json-read-file value))
        (t (easel-signal "INVALID_INPUT"
                         (format "%S is neither JSON nor a readable JSON file" value)
                         :value value))))

(defun easel-agent-arg-number (opts key)
  "OPTS's KEY as a number, or nil when absent."
  (let ((v (plist-get opts key)))
    (cond ((null v) nil)
          ((numberp v) v)
          ((and (stringp v) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" v))
           (string-to-number v))
          (t (easel-signal "INVALID_INPUT"
                           (format "--%s needs a number, got %S" (easel-key-name key) v)
                           :option (easel-key-name key))))))

(defun easel-agent-arg-choice (opts key choices default)
  "OPTS's KEY as a string from CHOICES (DEFAULT when absent)."
  (let ((v (plist-get opts key)))
    (if (null v) default
      (let ((s (format "%s" v)))
        (unless (member s choices)
          (easel-signal "INVALID_INPUT"
                        (format "--%s is one of %s, got %S" (easel-key-name key)
                                (string-join choices "|") s)
                        :option (easel-key-name key)))
        s))))

(defun easel-agent-arg-required (pos n what verb)
  "Positional N of POS, or signal INVALID_INPUT saying VERB needs WHAT."
  (or (nth n pos)
      (easel-signal "INVALID_INPUT"
                    (format "%s needs %s; usage: %s" verb what
                            (plist-get (easel-agent-verb verb) :usage))
                    :verb verb)))

;;; Sources: a template name with bindings, or a chart/v1 spec

(defun easel-agent-template-name-p (source)
  "Non-nil when SOURCE names a registered template."
  (and (stringp source) (member source (easel-template-names)) t))

(defun easel-agent-source-label (source)
  "How next[] commands refer to SOURCE."
  (if (and (stringp source) (not (easel-agent-json-text-p source))) source "SPEC.json"))

(defun easel-agent-bindings (opts)
  "The template bindings in OPTS's :data, parsed (nil when absent)."
  (let ((b (easel-agent-arg-json (plist-get opts :data))))
    (unless (easel-object-p b)
      (easel-signal "INVALID_INPUT" "--data is a JSON object keyed by template slot"
                    :option "data"))
    b))

(provide 'easel-agent-core)
;;; easel-agent-core.el ends here
