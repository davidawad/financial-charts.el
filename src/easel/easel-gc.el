;;; easel-gc.el --- defer garbage collection while a chart is in use -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.9).  On Linux/Xvfb at Emacs's default
;; `gc-cons-threshold', a 1k-row hover collected 2.5-3 times per move,
;; 80-97 ms of a 123-141 ms move, and every collection traces the whole
;; heap (engine-spikes.md section 8.5).  Binding the threshold around
;; the handler alone only moves that collection to just after it.
;;
;; So, in the style of gcmh: the first event of an interaction raises
;; `gc-cons-threshold' to `easel-gc-cons-threshold', and once Emacs has
;; been idle for `easel-gc-idle-delay' seconds easel collects once and
;; puts the user's value back.  It never lowers a larger value, and it
;; leaves alone a value someone else changed in between.  Set
;; `easel-gc-cons-threshold' to nil to leave GC to the user entirely.
;; Batch Emacs (tests, bin/easel) is left alone.

;;; Code:

(defgroup easel-gc nil
  "Garbage collection around easel chart interaction."
  :group 'easel :prefix "easel-gc-")

(defcustom easel-gc-cons-threshold (* 64 1024 1024)
  "`gc-cons-threshold' while a chart is in use, or nil to leave GC alone."
  :type '(choice (const :tag "Leave GC alone" nil) integer))

(defcustom easel-gc-idle-delay 1.0
  "Idle seconds after which easel collects and restores `gc-cons-threshold'."
  :type 'number)

(defvar easel-gc--saved nil
  "The user's `gc-cons-threshold' while easel has raised it, else nil.")

(defvar easel-gc--timer nil "Pending idle collection.")

(defun easel-gc-defer ()
  "Defer collection until idle; call before handling a chart event."
  (when (and easel-gc-cons-threshold (not noninteractive) (not easel-gc--saved)
             (< gc-cons-threshold easel-gc-cons-threshold))
    (setq easel-gc--saved gc-cons-threshold
          gc-cons-threshold easel-gc-cons-threshold))
  (when (and easel-gc--saved (not (timerp easel-gc--timer)))
    (setq easel-gc--timer (run-with-idle-timer easel-gc-idle-delay nil #'easel-gc-collect))))

(defun easel-gc-collect ()
  "Restore the user's `gc-cons-threshold' and collect once."
  (when (timerp easel-gc--timer) (cancel-timer easel-gc--timer))
  (setq easel-gc--timer nil)
  (when easel-gc--saved
    (when (eql gc-cons-threshold easel-gc-cons-threshold)
      (setq gc-cons-threshold easel-gc--saved))
    (setq easel-gc--saved nil)
    (garbage-collect)))

(provide 'easel-gc)
;;; easel-gc.el ends here
