;;; financial-chart-eas-colours.el --- every overlay its own colour and legend entry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two passes of a composed chart (fc-gty.23):
;;
;; - `financial-chart-colours-distinct': in each pane, series form groups
;;   (a study's parts, "bollinger-20-2.upper" and ".lower", are one
;;   group; any other series is its own).  A colour the caller chose is
;;   kept; otherwise the first group to use a colour keeps it; a later group whose colour is the same or close
;;   (`financial-chart-colours-near-p') gets the first palette colour
;;   not close to any colour already in the pane, in all its series and
;;   in the fills between them.  So Bollinger after Ichimoku's teal
;;   cloud, or SMA 7 after Tenkan-sen's blue, takes a colour of its own.
;; - `financial-chart-colours-legend': annotation layers that draw a line
;;   of their own (an anchored VWAP) declare :legend (:label :auto); they
;;   join the pane's legend, an :auto one in the first free palette
;;   colour, and their :legend-member layers (its label) follow it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-eas-palette)

(defvar financial-chart-colours-min-distance 60
  "Colours closer than this (RGB distance) count as the same on a chart.")

(defun financial-chart-colours--rgb (hex)
  "HEX (#rrggbb) as (R G B), or nil."
  (when (and (stringp hex) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" hex))
    (mapcar (lambda (i) (string-to-number (substring hex i (+ i 2)) 16)) '(1 3 5))))

(defun financial-chart-colours-near-p (a b)
  "Non-nil when colours A and B are equal or close."
  (let ((x (financial-chart-colours--rgb a)) (y (financial-chart-colours--rgb b)))
    (if (and x y)
        (< (sqrt (apply #'+ (cl-mapcar (lambda (p q) (expt (- p q) 2)) x y)))
           financial-chart-colours-min-distance)
      (equal a b))))

(defun financial-chart-colours-pick (used)
  "The first palette colour not close to any of USED (else not equal to one)."
  (or (cl-find-if (lambda (c) (not (cl-some (lambda (u) (financial-chart-colours-near-p c u)) used)))
                  financial-chart-palette)
      (cl-find-if (lambda (c) (not (member c used))) financial-chart-palette)
      (car financial-chart-palette)))

(defun financial-chart-colours--group (s)
  "Series S's colour group: its study id, or its own id."
  (let ((id (or (plist-get s :id) (plist-get s :column) "")))
    (car (split-string id "\\."))))

(defun financial-chart-colours-distinct (series)
  "SERIES (finished, with :pane and :color) with each pane's groups in
distinct colours (see the commentary).  Return (SERIES . REMAPS), REMAPS
an alist ((PANE . GROUP) . ((OLD . NEW) ...))."
  (let ((used (make-hash-table :test #'equal))      ; pane -> colours of earlier groups
        (seen (make-hash-table :test #'equal))      ; (pane . group) -> t
        remaps)
    ;; Groups with a colour the caller chose claim it first.
    (dolist (s (append (seq-filter (lambda (s) (plist-get s :chosen-colour)) series)
                       (seq-remove (lambda (s) (plist-get s :chosen-colour)) series)))
      (let* ((pane (plist-get s :pane)) (group (financial-chart-colours--group s))
             (key (cons pane group)))
        (unless (gethash key seen)
          (puthash key t seen)
          (let* ((colours (delete-dups (mapcar (lambda (x) (plist-get x :color))
                                               (seq-filter (lambda (x) (and (eql (plist-get x :pane) pane)
                                                                            (equal (financial-chart-colours--group x) group)))
                                                           series))))
                 (earlier (gethash pane used))
                 (map (cl-loop with taken = (append earlier colours)
                               for c in colours
                               when (and c (cl-some (lambda (u) (financial-chart-colours-near-p c u)) earlier))
                               collect (let ((new (financial-chart-colours-pick taken)))
                                         (push new taken)
                                         (cons c new)))))
            (when map (push (cons key map) remaps))
            (puthash pane (append earlier (mapcar (lambda (c) (or (cdr (assoc c map)) c)) colours)) used)))))
    (cons (mapcar (lambda (s)
                    (let ((map (cdr (assoc (cons (plist-get s :pane) (financial-chart-colours--group s)) remaps))))
                      (if-let* ((new (cdr (assoc (plist-get s :color) map))))
                          (plist-put (copy-sequence s) :color new)
                        s)))
                  series)
          remaps)))

(defun financial-chart-colours-remap-fill (fill pane series remaps)
  "FILL of PANE with the colours of the group it fills between remapped.
SERIES are the finished series; REMAPS from `financial-chart-colours-distinct'."
  (let* ((between (append (plist-get fill :between) nil))
         (group (cl-loop for ref in between
                         for s = (cl-find-if (lambda (s) (and (eql (plist-get s :pane) pane)
                                                              (or (equal (plist-get s :id) ref)
                                                                  (equal (plist-get s :label) ref))))
                                             series)
                         when s return (financial-chart-colours--group s)))
         (map (and group (cdr (assoc (cons pane group) remaps)))))
    (if (not map) fill
      (cl-loop for (k v) on fill by #'cddr
               append (list k (if (memq k '(:color :above :below)) (or (cdr (assoc v map)) v) v))))))

(defun financial-chart-colours-legend (layers legend)
  "LAYERS (a pane's annotation layers) joined to LEGEND (LABELS . COLOURS).
Return (LAYERS . LEGEND): a layer with :legend (:label L :auto A) is
coloured through the legend's scale (an :auto one in the first palette
colour free in the pane), its :legend-member layers take that colour,
and the keys are dropped."
  (let ((labels (car legend)) (colours (cdr legend)) chosen)
    (dolist (l layers)
      (when-let* ((spec (plist-get l :legend)))
        (let ((c (if (plist-get spec :auto) (financial-chart-colours-pick colours)
                   (plist-get (plist-get l :mark) :color))))
          (push (cons (plist-get l :name) c) chosen)
          (setq labels (append labels (list (plist-get spec :label)))
                colours (append colours (list c))))))
    (let ((scale (list :domain (vconcat labels) :range (vconcat colours))))
      (cons (mapcar
             (lambda (l)
               (let* ((spec (plist-get l :legend))
                      (member (plist-get l :legend-member))
                      (c (cdr (assoc (or member (plist-get l :name)) chosen)))
                      (l (cl-loop for (k v) on l by #'cddr
                                  unless (memq k '(:legend :legend-member)) append (list k v))))
                 (cond (spec
                        (let ((mark (cl-loop for (k v) on (plist-get l :mark) by #'cddr
                                             unless (eq k :color) append (list k v))))
                          (plist-put (plist-put (copy-sequence l) :mark mark) :encoding
                                     (plist-put (copy-sequence (plist-get l :encoding)) :color
                                                (list :datum (plist-get spec :label) :type "nominal"
                                                      :title :null :scale scale)))))
                       ((and member c)
                        (plist-put (copy-sequence l) :mark (plist-put (copy-sequence (plist-get l :mark)) :color c)))
                       (t l))))
             layers)
            (cons labels colours)))))

(provide 'financial-chart-eas-colours)
;;; financial-chart-eas-colours.el ends here
