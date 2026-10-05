;;; easel-mode.el --- native glue: buffers, keys and mouse -> event/v1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  `easel-show' displays a view in a buffer: an SVG image
;; with :map hot spots in GUI frames, propertized text in terminals.
;; The glue only translates native input into event/v1 and calls
;; `easel-dispatch'; all behaviour lives in the reducer.
;;
;;   GUI       posn-object-x-y inside the image (divided by its :scale)
;;             -> px; mouse-movement needs `track-mouse', set locally.
;;   terminal  point motion, and xterm-mouse clicks/motion, -> the
;;             pixel centre of the cell.  Drags are built from
;;             down/motion/up (fc-qx1.14: no drag-mouse-1 under tmux).
;;
;; One keymap serves both: + - 0 zoom, [ ] history, < > pan, ESC
;; clears selections, RET clicks at point, g redraws, q quits.  In GUI
;; buffers the arrows pan; in text buffers they move point (hover) and
;; S-arrows pan.  The wheel (xterm-mouse's mouse-4/5 in terminals) and a
;; trackpad pinch zoom around the pointer; horizontal scrolling pans.  Redraws are idle-coalesced, the default the spikes
;; chose until GUI re-raster latency is measured.

;;; Code:

(require 'easel-view)
(require 'easel-svg)
(require 'easel-text)
(require 'easel-zoom)

(defvar-local easel-mode--view nil "The view this buffer shows.")
(defvar-local easel-mode--timer nil "Pending idle redraw.")
(defvar-local easel-mode--last-px nil "Last pointer position sent.")
(defvar-local easel-mode--last-cell nil "Point's (LINE . COLUMN) when it last hovered.")
(defvar-local easel-mode--pinch nil "Last pinch scale of the current gesture.")

(defun easel-mode--gui-p ()
  "Non-nil when this buffer draws an image."
  (eq (easel-view-target easel-mode--view) 'svg))

(defun easel-mode--cell ()
  "The scene's cell size [W H]."
  (plist-get (plist-get (easel-view-scene easel-mode--view) :size) :cell))

(defun easel-mode-event-px (event &optional snap)
  "Scene pixels of mouse EVENT, or nil when it is not over the chart.
GUI events use `posn-object-x-y' divided by the image :scale; text
events use the cell under the pointer (SNAP as in `easel-mode-point-px')."
  (let* ((posn (event-start event)))
    (if (and (posn-image posn) (posn-object-x-y posn))
        (let* ((xy (posn-object-x-y posn))
               (scale (plist-get (cdr (posn-image posn)) :scale))
               ;; `create-image' leaves :scale `default' when none is given.
               (scale (if (numberp scale) scale 1)))
          (vector (/ (car xy) (float scale)) (/ (cdr xy) (float scale))))
      (when-let* ((pos (posn-point posn)))
        (easel-mode-point-px pos snap)))))

(defun easel-mode-point-px (&optional pos snap)
  "Scene pixels at the centre of the text cell at POS (default point).
With SNAP, the datum drawn in the cell when it holds one: a cell is
wider than the click slop, so clicks and presses snap to hit what is
shown.  Hover never snaps; what a cell shows changes as hover redraws."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((cell (easel-mode--cell)))
      (or (and snap (easel-mode--datum-px (get-text-property (point) 'easel-view)
                                          (get-text-property (point) 'easel-mark)
                                          (get-text-property (point) 'easel-datum)
                                          (current-column) (1- (line-number-at-pos)) cell))
          (vector (* (+ (current-column) 0.5) (aref cell 0))
                  (* (+ (1- (line-number-at-pos)) 0.5) (aref cell 1)))))))

(defun easel-mode--datum-px (view-id mark-id datum col row cell)
  "Pixel of DATUM of MARK-ID in VIEW-ID when it lies in or next to cell COL ROW.
The text renderer pulls glyphs on the plot's edge one cell inward."
  (when-let* ((datum)
              (view (seq-find (lambda (v) (equal (plist-get v :id) view-id))
                              (plist-get (easel-view-scene easel-mode--view) :views)))
              (mark (seq-find (lambda (m) (equal (plist-get m :id) mark-id)) (plist-get view :marks))))
    (cl-loop for item across (plist-get mark :items)
             for d = (plist-get item :datum)
             for k = (and (vectorp d) (seq-position d datum))
             for p = (cond (k (easel-hit--item-point item k)) ((equal d datum) (easel-hit--item-point item nil)))
             when (and p (<= (abs (- (floor (car p) (aref cell 0)) col)) 1)
                       (<= (abs (- (floor (cdr p) (aref cell 1)) row)) 1))
             return (vector (float (car p)) (float (cdr p))))))

(defun easel-mode--send (event)
  "Dispatch EVENT to this buffer's view, reporting failures in the echo area."
  (condition-case err
      (easel-dispatch easel-mode--view event)
    (easel-error (message "easel: %s" (plist-get (easel-error-plist err) :message)) nil)))

;;; Rendering

(defun easel-mode--window-size (window target)
  "Size to compile for WINDOW and TARGET."
  (if (eq target 'svg)
      (cons (window-body-width window t) (- (window-body-height window t) 4))
    (list :cols (max 20 (1- (window-body-width window))) :rows (max 6 (1- (window-body-height window))))))

(defun easel-mode-redraw (&optional buffer)
  "Redraw BUFFER (default current) from its view's scene."
  (with-current-buffer (or buffer (current-buffer))
    (setq easel-mode--timer nil)
    (let* ((view easel-mode--view) (scene (easel-view-scene view))
           (inhibit-read-only t) (pos (point))
           ;; Text lines change length as labels change: keep point's cell.
           (line (line-number-at-pos)) (col (current-column)))
      (let ((old (get-text-property (point-min) 'display)))
        (erase-buffer)
        ;; Each redraw is a new image; without a flush the image cache
        ;; keeps every one (fc-qx1.23: +1.28 MB per 800x400 move).
        (when (and (eq (car-safe old) 'image) (fboundp 'image-flush)) (image-flush old t)))
      (cond
       ((not (easel-view-interactive view)) (easel-mode--insert-static view))
       ((easel-mode--gui-p)
          (let ((image (easel-svg-image scene :scale 1)))
            (insert-image image "[chart]")
            (easel-mode--hot-spot-keys image)))
       (t (insert (easel-text-render scene))))
      (if (easel-mode--gui-p) (goto-char (min pos (point-max)))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col))
      (easel-mode--readout))))

(defun easel-mode--insert-static (view)
  "Insert VIEW's static fallback: bin/chart's image, and why it is static."
  (let ((fallback (easel-view-fallback view)))
    (when (and (plist-get fallback :data) (display-graphic-p) (image-type-available-p 'svg))
      (insert-image (create-image (plist-get fallback :data) 'svg t) "[chart]")
      (insert "\n"))
    (insert "Static chart (not interactive): the native engine does not support\n")
    (seq-doseq (w (easel-view-warnings view))
      (insert (format "  %s at %s\n" (or (plist-get w :feature) (plist-get w :code)) (plist-get w :path))))
    (when-let* ((err (plist-get fallback :error)))
      (insert (format "No static image: %s\n" (plist-get err :message))))))

(defun easel-mode--hot-spot-keys (image)
  "Bind IMAGE's :map area ids so clicks on hot spots reach the reducer."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map easel-view-mode-map)
    (dolist (area (plist-get (cdr image) :map))
      (dolist (key '(mouse-1 down-mouse-1 mouse-movement))
        (define-key map (vector (nth 1 area) key) (lookup-key easel-view-mode-map (vector key)))))
    (use-local-map map)))

(defun easel-mode--readout ()
  "Show the hovered datum (or the view id) in the header line."
  (let* ((inspect (easel-inspect easel-mode--view)) (hover (plist-get inspect :hover)))
    (setq header-line-format
          (if (and hover (not (eq hover :null)))
              (format " %s  %s" (easel-view-id easel-mode--view)
                      (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cdr pair)))
                                 (cl-loop for (k v) on (plist-get hover :row) by #'cddr
                                          collect (cons (easel-key-name k) v))
                                 "  "))
            (format " %s  %s" (easel-view-id easel-mode--view)
                    (or (plist-get inspect :last-event) ""))))))

(defun easel-mode--schedule (view)
  "Coalesce a redraw of every buffer showing VIEW."
  (when-let* ((buffer (easel-view-buffer view)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless easel-mode--timer
          (setq easel-mode--timer
                (if noninteractive (progn (easel-mode-redraw buffer) nil)
                  (run-with-idle-timer 0 nil #'easel-mode-redraw buffer))))))))

(add-hook 'easel-view-changed-functions #'easel-mode--schedule)

;;; Commands

(defun easel-mode-pointer (event)
  "Translate mouse motion EVENT into pointermove."
  (interactive "e")
  (when-let* ((px (easel-mode-event-px event)))
    (unless (equal px easel-mode--last-px)
      (setq easel-mode--last-px px)
      (easel-mode--send (list :type "pointermove" :px px))
      (easel-mode--readout))))

(defun easel-mode-down (event)
  "Translate a mouse press EVENT into pointerdown."
  (interactive "e")
  (when-let* ((px (easel-mode-event-px event t)))
    (easel-mode--send (list :type "pointerdown" :px px))))

(defun easel-mode-up (event)
  "Translate a mouse release (or drag end) EVENT into pointerup."
  (interactive "e")
  (when-let* ((px (easel-mode-event-px (if (memq 'drag (event-modifiers event))
                                           (list (car event) (event-end event))
                                         event)
                                       t)))
    (easel-mode--send (list :type "pointerup" :px px))
    (easel-mode--readout)))

(defun easel-mode-dblclick (event)
  "Translate a double click EVENT into dblclick."
  (interactive "e")
  (when-let* ((px (easel-mode-event-px event t)))
    (easel-mode--send (list :type "dblclick" :px px))))

(defun easel-mode-wheel (event)
  "Translate wheel EVENT into wheel zoom around the pointer."
  (interactive "e")
  (when-let* ((px (easel-mode-event-px event)))
    (easel-mode--send (list :type "wheel" :px px :delta (easel-zoom-wheel-delta event)))))

(defun easel-mode-pinch (event)
  "Translate a trackpad pinch EVENT into wheel zoom around the pointer.
EVENT is (pinch POSITION DX DY SCALE ANGLE); SCALE is relative to the
gesture's start, so each event zooms by its change from the last."
  (interactive "e")
  (let ((scale (nth 4 event)))
    (when (or (null easel-mode--pinch) (and (= scale 1.0) (zerop (nth 2 event)) (zerop (nth 3 event))))
      (setq easel-mode--pinch 1.0))
    (let ((delta (easel-zoom-pinch-delta scale easel-mode--pinch)))
      (setq easel-mode--pinch scale)
      (when-let* (((/= delta 0)) (px (easel-mode-event-px event)))
        (easel-mode--send (list :type "wheel" :px px :delta delta))))))

(defun easel-mode-hscroll (event)
  "Translate horizontal scroll EVENT into a pan along x."
  (interactive "e")
  (easel-mode--send (list :type "key" :key (if (memq (event-basic-type event) '(wheel-left mouse-6))
                                               "left" "right"))))

(defun easel-mode-key ()
  "Send the key that invoked this command as an event/v1 key."
  (interactive)
  (let* ((key (this-command-keys-vector)) (k (aref key (1- (length key)))))
    (easel-mode--send (list :type "key"
                            :key (pcase k
                                   ((or 'left 'S-left ?<) "left") ((or 'right 'S-right ?>) "right")
                                   ((or 'up 'S-up) "up") ((or 'down 'S-down) "down")
                                   ((or 27 'escape) "escape") (_ (string k)))))))

(defun easel-mode-arrow ()
  "Pan in GUI buffers; move point (hover) in text buffers."
  (interactive)
  (if (easel-mode--gui-p) (easel-mode-key)
    (pcase (aref (this-command-keys-vector) 0)
      ('left (backward-char)) ('right (forward-char))
      ('up (line-move -1)) ('down (line-move 1)))))

(defun easel-mode-click-at-point ()
  "Click at point (terminal RET)."
  (interactive)
  (easel-mode--send (list :type "click" :px (easel-mode-point-px nil t))))

(defun easel-mode--post-command ()
  "In text buffers, moving point is hovering.
Only a move to another cell hovers: mouse events (xterm-mouse) carry
their own pointer, and a key pressed after them keeps the mouse's hover."
  (when (and easel-mode--view (not (easel-mode--gui-p)))
    (let ((cell (cons (line-number-at-pos) (current-column))))
      (cond ((consp last-command-event) (setq easel-mode--last-cell cell))
            ((not (equal cell easel-mode--last-cell))
             (setq easel-mode--last-cell cell)
             (let ((px (easel-mode-point-px)))
               (unless (equal px easel-mode--last-px)
                 (setq easel-mode--last-px px)
                 (easel-mode--send (list :type "pointermove" :px px))
                 (easel-mode--readout))))))))

(defun easel-mode-refresh ()
  "Recompile at the window's size and redraw."
  (interactive)
  (easel-view-resize easel-mode--view
                     (easel-mode--window-size (get-buffer-window) (easel-view-target easel-mode--view)))
  (easel-mode-redraw))

(defvar easel-view-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (k '("+" "=" "-" "0" "[" "]" "<" ">")) (define-key map k #'easel-mode-key))
    (dolist (k '([escape] "c")) (define-key map k (lambda () (interactive) (easel-mode--send '(:type "key" :key "escape")))))
    (dolist (k '([left] [right] [up] [down])) (define-key map k #'easel-mode-arrow))
    (dolist (k '([S-left] [S-right] [S-up] [S-down])) (define-key map k #'easel-mode-key))
    (define-key map (kbd "RET") #'easel-mode-click-at-point)
    (define-key map "g" #'easel-mode-refresh)
    (define-key map [mouse-movement] #'easel-mode-pointer)
    (define-key map [down-mouse-1] #'easel-mode-down)
    (define-key map [mouse-1] #'easel-mode-up)
    (define-key map [drag-mouse-1] #'easel-mode-up)
    (define-key map [double-mouse-1] #'easel-mode-dblclick)
    (dolist (k '([wheel-up] [wheel-down] [mouse-4] [mouse-5])) (define-key map k #'easel-mode-wheel))
    (dolist (k '([wheel-left] [wheel-right] [mouse-6] [mouse-7])) (define-key map k #'easel-mode-hscroll))
    (define-key map [pinch] #'easel-mode-pinch)
    map)
  "Keymap shared by GUI and terminal easel buffers.")

(define-derived-mode easel-view-mode special-mode "Easel"
  "Major mode for a live easel chart.  See `easel-view-mode-map'."
  (setq-local track-mouse t)
  (setq truncate-lines t)
  (add-hook 'post-command-hook #'easel-mode--post-command nil t))

(defun easel-show (view &optional target)
  "Show VIEW (an id or view) in its buffer; TARGET overrides svg/text.
The target defaults to svg in graphic frames with SVG support, else text."
  (let* ((view (easel-view-get view))
         (buffer (get-buffer-create (format "*easel %s*" (easel-view-id view))))
         (target (or target (if (and (display-graphic-p) (image-type-available-p 'svg)) 'svg 'text))))
    (with-current-buffer buffer
      (easel-view-mode)
      (setq easel-mode--view view)
      (setf (easel-view-buffer view) buffer))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (easel-view-resize view (easel-mode--window-size (get-buffer-window buffer) target) target)
      (easel-mode-redraw buffer))
    buffer))

(provide 'easel-mode)
;;; easel-mode.el ends here
