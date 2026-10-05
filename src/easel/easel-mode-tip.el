;;; easel-mode-tip.el --- hover tooltips in GUI and terminal buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 glue (fc-qx1.1).  Where a tooltip comes from:
;;
;;   GUI, discrete marks  the image's :map areas carry help-echo, so
;;                        Emacs shows them itself (a tooltip frame, or
;;                        the echo area when `tooltip-mode' is off)
;;   GUI, series          no map area per point (fc-qx1.14 spike): the
;;                        hovered datum comes from the hit index and is
;;                        shown with `tooltip-show', or in the echo area
;;                        when `tooltip-mode' is off
;;   terminal             moving point over the chart is hovering: the
;;                        hovered datum's tooltip goes to the echo area;
;;                        cells also carry help-echo for xterm-mouse
;;
;; It runs from `easel-view-dispatch-functions' whenever the hovered
;; datum changes in a view that has a buffer.

;;; Code:

(require 'easel-view)
(require 'easel-tip)

(defconst easel-mode-tip-map-marks '("bar" "rect" "point" "circle" "square" "text")
  "Marks `easel-svg-hot-spots' gives :map areas, whose help-echo Emacs shows.")

(defvar easel-mode-tip-display-function #'easel-mode-tip-display
  "Function called with TEXT (nil to clear) and VIEW to show a hover tooltip.")

(defvar-local easel-mode-tip--shown nil "Non-nil while this buffer's tooltip is showing.")

(defun easel-mode-tip-display (text view)
  "Show TEXT for VIEW: a tooltip in a GUI frame with `tooltip-mode'.
Elsewhere, or with `tooltip-mode' off, TEXT goes to the echo area."
  (if (and (eq (easel-view-target view) 'svg) (display-graphic-p)
           (bound-and-true-p tooltip-mode) (fboundp 'tooltip-show))
      (if text (tooltip-show text) (tooltip-hide))
    (let ((message-log-max nil))
      (cond (text (message "%s" text))
            (easel-mode-tip--shown (message nil)))))
  (setq easel-mode-tip--shown (and text t)))

(defun easel-mode-tip--key (hover)
  "What identifies HOVER's datum."
  (and hover (list (plist-get hover :view) (plist-get hover :mark) (plist-get hover :datum))))

(defun easel-mode-tip-text (view)
  "Tooltip text of VIEW's hovered datum, or nil."
  (easel-tip-text (easel-tip-tooltip (easel-view-scene view) (easel-view-plan view)
                                     (plist-get (easel-view-state view) :hover))))

(defun easel-mode-tip--own-p (view)
  "Non-nil unless Emacs shows VIEW's hovered datum itself through a :map area."
  (let ((hover (plist-get (easel-view-state view) :hover)))
    (not (and hover (eq (easel-view-target view) 'svg)
              (member (plist-get (easel-tip--mark (easel-view-scene view) (plist-get hover :view)
                                                  (plist-get hover :mark))
                                 :mark)
                      easel-mode-tip-map-marks)))))

(defun easel-mode-tip--on-dispatch (view event old-state _old-scene)
  "Show VIEW's tooltip when EVENT moved the hover off OLD-STATE's datum."
  (let ((buffer (easel-view-buffer view)))
    (when (and (member (plist-get event :type) '("pointermove" "pointerleave"))
               (not easel-view-replaying) (buffer-live-p buffer)
               (not (equal (easel-mode-tip--key (plist-get old-state :hover))
                           (easel-mode-tip--key (plist-get (easel-view-state view) :hover)))))
      (with-current-buffer buffer
        (let ((text (and (easel-mode-tip--own-p view) (easel-mode-tip-text view))))
          (when (or text easel-mode-tip--shown)
            (funcall easel-mode-tip-display-function text view)
            (setq easel-mode-tip--shown (and text t))))))))

(add-hook 'easel-view-dispatch-functions #'easel-mode-tip--on-dispatch)

(provide 'easel-mode-tip)
;;; easel-mode-tip.el ends here
