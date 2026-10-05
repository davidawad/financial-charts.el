;;; easel-tty.el --- terminal parity: keyboard and xterm-mouse in text frames -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 glue (fc-qx1.8).  A text frame gets the GUI's keymap and
;; sends the same event/v1 stream; only the pointer differs:
;;
;;   inspection  point is the pointer: moving it hovers (easel-mode),
;;               and n / p step the hover to the next / previous datum
;;               in x order, moving point onto its cell.  In GUI frames
;;               n / p step the same way, so inspection needs no mouse.
;;   zoom, pan   + - 0 [ ] < > z and S-arrows, as in GUI frames.
;;   brush       C-SPC, move point, b (easel-brush).
;;   clear       ESC ESC as well as c (a terminal sends ESC as a prefix,
;;               so the GUI's lone escape never arrives).
;;   mouse       with `easel-tty-xterm-mouse', showing a chart in a
;;               terminal whose TERM speaks the xterm protocol turns on
;;               `xterm-mouse-mode': clicks, wheel and motion then reach
;;               the same commands the GUI uses (fc-qx1.14 measured
;;               down, motion and up under tmux; drags are built from
;;               them).
;;
;; `easel-parity-replay' proves the result: one log, both backends, the
;; same view state.

;;; Code:

(require 'easel-core)
(require 'easel-hit)
(require 'easel-view)
(require 'easel-mode)

(defvar easel-tty-xterm-mouse t
  "Non-nil turns on `xterm-mouse-mode' when a chart opens in a capable terminal.
Nil leaves the mode alone; the keyboard still drives every interaction.")

(defconst easel-tty-mouse-terms
  "\\`\\(xterm\\|screen\\|tmux\\|rxvt\\|kitty\\|alacritty\\|foot\\|wezterm\\|vte\\|gnome\\|konsole\\|st-\\)"
  "TERM prefixes whose terminals report mouse events in the xterm protocol.")

(defun easel-tty-mouse-capable-p (&optional term)
  "Non-nil when TERM (default: the selected frame's) speaks xterm mouse."
  (let ((term (or term (getenv "TERM" (selected-frame)))))
    (and (stringp term) (string-match-p easel-tty-mouse-terms term))))

(defun easel-tty--enable-mouse ()
  "Turn on `xterm-mouse-mode' for an easel buffer in a capable terminal."
  (when (and easel-tty-xterm-mouse (not noninteractive) (not (display-graphic-p))
             (not (bound-and-true-p xterm-mouse-mode)) (easel-tty-mouse-capable-p))
    (xterm-mouse-mode 1)
    (message "easel: xterm-mouse-mode on (set `easel-tty-xterm-mouse' to nil to keep it off)")))

(add-hook 'easel-view-mode-hook #'easel-tty--enable-mouse)

;;; Stepping the hover through data

(defun easel-tty--anchors (scene view-id)
  "Visible anchors of SCENE's VIEW-ID as (X Y MARK DATUM), in x then y order.
Anchors outside the plot (zoomed or panned away) are skipped.  Where
marks share an x (a line and its crosshair rule), only the first mark's
anchors are kept, so stepping moves to new data each time."
  (let* ((view (seq-find (lambda (v) (equal (plist-get v :id) view-id)) (plist-get scene :views)))
         (bounds (plist-get view :bounds))
         anchors kept)
    (seq-doseq (mark (plist-get view :marks))
      (unless (plist-get mark :interactive-off)
        (let ((index (plist-get mark :index)) (items (plist-get mark :items)))
          (if (equal (plist-get index :kind) "x-sorted")
              (seq-doseq (ref (plist-get index :refs))
                (push (easel-hit--candidate mark (aref ref 0) (aref ref 1) 0 0 nil) anchors))
            (dotimes (i (length items))
              (push (easel-hit--candidate mark i :null 0 0 nil) anchors))))))
    (dolist (a (sort (mapcar (lambda (c) (list (plist-get c :x) (plist-get c :y) (plist-get c :mark) (plist-get c :datum)))
                             anchors)
                     (lambda (a b) (or (< (nth 0 a) (nth 0 b)) (and (= (nth 0 a) (nth 0 b)) (< (nth 1 a) (nth 1 b)))))))
      (unless (or (not (easel-hit--contains bounds (nth 0 a) (nth 1 a)))
                  (seq-find (lambda (k) (and (= (nth 0 k) (nth 0 a)) (not (equal (nth 2 k) (nth 2 a))))) kept))
        (push a kept)))
    (nreverse kept)))

(defun easel-tty--nearest (anchors hover)
  "Index of the anchor in ANCHORS at HOVER's datum, else the nearest in x then y."
  (or (cl-position-if (lambda (a) (and (equal (nth 2 a) (plist-get hover :mark))
                                       (equal (nth 3 a) (plist-get hover :datum))))
                      anchors)
      (let ((x (plist-get hover :x)) (y (plist-get hover :y)) best best-d)
        (seq-do-indexed (lambda (a i)
                          (let ((d (cons (abs (- (nth 0 a) x)) (abs (- (nth 1 a) y)))))
                            (when (or (null best-d) (< (car d) (car best-d))
                                      (and (= (car d) (car best-d)) (< (cdr d) (cdr best-d))))
                              (setq best i best-d d))))
                        anchors)
        best)))

(defun easel-tty-step-px (scene hover n)
  "Scene pixel of the datum N anchors after HOVER in SCENE's x order.
HOVER is the view state's hover (nil starts before the first datum of
the first view).  Stops at either end; nil when there is no data."
  (let* ((view-id (or (plist-get hover :view) (plist-get (seq-first (plist-get scene :views)) :id)))
         (anchors (easel-tty--anchors scene view-id))
         (here (and hover anchors (easel-tty--nearest anchors hover)))
         (i (if here (+ here n) (if (> n 0) (1- n) (+ (length anchors) n)))))
    (when anchors
      (let ((a (nth (max 0 (min (1- (length anchors)) i)) anchors)))
        (vector (float (nth 0 a)) (float (nth 1 a)))))))

(defun easel-tty--goto-px (px)
  "Move point to the text cell holding scene pixel PX."
  (let* ((cell (plist-get (plist-get (easel-view-scene easel-mode--view) :size) :cell))
         (col (floor (aref px 0) (aref cell 0))) (row (floor (aref px 1) (aref cell 1))))
    (goto-char (point-min))
    (forward-line (max 0 row))
    (move-to-column (max 0 col))))

(defun easel-tty-next-datum (&optional n)
  "Hover the datum N (default 1) places later in x order.
Sends a pointermove at the datum, so it is logged like any hover; in
text buffers point moves onto the datum's cell."
  (interactive "p")
  (let* ((view easel-mode--view)
         (px (easel-tty-step-px (easel-view-scene view) (plist-get (easel-view-state view) :hover) (or n 1))))
    (if (null px) (message "easel: no data to step through")
      (unless (eq (easel-view-target view) 'svg)
        (easel-tty--goto-px px)
        ;; Point now sits on the datum's cell: post-command must not
        ;; re-hover the cell centre, which may be a neighbouring datum.
        (setq easel-mode--last-cell (cons (line-number-at-pos) (current-column))))
      (easel-mode--send (list :type "pointermove" :px px))
      (easel-mode--readout))))

(defun easel-tty-previous-datum (&optional n)
  "Hover the datum N (default 1) places earlier in x order."
  (interactive "p")
  (easel-tty-next-datum (- (or n 1))))

(define-key easel-view-mode-map "n" #'easel-tty-next-datum)
(define-key easel-view-mode-map "p" #'easel-tty-previous-datum)
(define-key easel-view-mode-map (kbd "ESC ESC")
            (lambda () (interactive) (easel-mode--send '(:type "key" :key "escape"))))

(provide 'easel-tty)
;;; easel-tty.el ends here
