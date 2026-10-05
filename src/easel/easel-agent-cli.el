;;; easel-agent-cli.el --- bin/easel: the stateless agent verbs from a shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; bin/easel runs
;;
;;   Emacs -Q --batch -L src/easel -l easel-agent-cli -f easel-agent-cli-main -- VERB ARGS...
;;
;; ARGS are positionals then options: `--backend text', `--backend=text'
;; or a bare flag such as `--vl'.  An argument "-" reads stdin.  The
;; chart/v1 envelope is printed as JSON on stdout; the exit status is 0
;; when ok and 1 otherwise.  `--raw' prints only data on success (the
;; text chart, the SVG, the Vega-Lite spec), still the envelope on
;; failure.  Live verbs need a running Emacs, so bin/easel answers them
;; with the emacsclient command to use instead.

;;; Code:

(require 'easel-agent)

(defun easel-agent-cli--stdin ()
  "All of stdin as a string."
  ;; Not `insert-file-contents' on /dev/stdin: Emacs 29.1 rejects a pipe
  ;; there.  In batch mode `read-from-minibuffer' reads one stdin line.
  (with-temp-buffer
    (let (line)
      (while (setq line (ignore-errors (read-from-minibuffer "")))
        (insert line "\n")))
    (buffer-string)))

(defun easel-agent-cli-args (verb argv)
  "Parse command-line ARGV for VERB into `easel-agent' arguments.
Return (ARGS . RAW), RAW non-nil when --raw was given."
  (let* ((flags (ignore-errors (plist-get (easel-agent-verb verb) :flags)))
         pos opts raw)
    (while argv
      (let ((arg (pop argv)))
        (cond
         ((equal arg "--raw") (setq raw t))
         ((string-match "\\`--\\([^=]+\\)=\\(.*\\)\\'" arg)
          (setq opts (append opts (list (easel-key (match-string 1 arg)) (match-string 2 arg)))))
         ((string-prefix-p "--" arg)
          (let ((key (easel-key (substring arg 2))))
            (setq opts (append opts (list key (if (or (memq key flags) (null argv)
                                                      (string-prefix-p "--" (car argv)))
                                                  t
                                                (pop argv)))))))
         (opts (easel-signal "INVALID_INPUT"
                             (format "Positional %S after options; put positionals first" arg)))
         (t (push arg pos)))))
    (cons (append (nreverse pos) opts) raw)))

(defun easel-agent-cli-run (argv)
  "Run bin/easel ARGV; return (EXIT . OUTPUT)."
  (let* ((verb (or (car argv) "describe"))
         (entry (alist-get verb easel-agent-verbs nil nil #'equal))
         (easel-agent-stdin-function #'easel-agent-cli--stdin)
         raw
         (envelope
          (cond
           ((member verb '("-h" "--help" "help")) (easel-agent "describe" "verbs"))
           ((plist-get entry :live)
            (easel-agent-fail "INVALID_INPUT"
                              (list :code "INVALID_INPUT" :verb verb
                                    :message (format "%s reads live views; ask the running Emacs" verb))
                              nil (apply #'easel-agent-live-cmd verb (cdr argv))))
           (t (condition-case err
                  (let ((parsed (easel-agent-cli-args verb (cdr argv))))
                    (setq raw (cdr parsed))
                    (apply #'easel-agent verb (car parsed)))
                (error (easel-agent--error-envelope verb nil err)))))))
    (if (eq (plist-get envelope :ok) t)
        (cons 0 (let ((data (plist-get envelope :data)))
                  (cond ((not raw) (easel-json-pretty (easel-json-parse (easel-agent-encode envelope))))
                        ((stringp data) (if (string-suffix-p "\n" data) data (concat data "\n")))
                        ((stringp (plist-get data :output)) (concat (plist-get data :output) "\n"))
                        (t (easel-json-pretty data)))))
      (cons 1 (easel-json-pretty (easel-json-parse (easel-agent-encode envelope)))))))

(defun easel-agent-cli-main ()
  "Entry point for bin/easel: run `command-line-args-left' and exit."
  (let ((result (easel-agent-cli-run (if (equal (car command-line-args-left) "--")
                                          (cdr command-line-args-left)
                                        command-line-args-left))))
    (setq command-line-args-left nil)
    (princ (cdr result))
    (kill-emacs (car result))))

(provide 'easel-agent-cli)
;;; easel-agent-cli.el ends here
