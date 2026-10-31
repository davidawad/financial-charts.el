;;; financial-chart-3d-framing-test.el --- every 3D variant reads at every size and phase -*- lexical-binding: t; -*-

;;; Commentary:

;; The goldens draw one frame per variant (60x24, the end of its
;; replay).  These draw every variant of `financial-chart-3d-variants'
;; (new ones too) at the sizes a gallery pane or a window takes, at five
;; phases of its timeline, and check each frame reads: enough lit
;; cells, and the data (every lit cell not in a guide colour: axes,
;; the breakeven frame, RSI bands) spanning at least
;; `financial-chart-3d-framing-span' of the frame's width and height.
;; A surface that drops to one row, a replay that starts from one bar
;; or a camera that leaves the data in a corner fails here.  `make
;; test-3d' runs them.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(require 'graphics-frame)
(financial-chart-3d--ensure)

(defconst financial-chart-3d-framing-sizes '((40 . 16) (54 . 20) (60 . 24) (100 . 40) (160 . 48))
  "Frame sizes, (COLS . ROWS): small and gallery panes up to a wide window.")

(defconst financial-chart-3d-framing-phases '(0.0 0.25 0.5 0.75 0.99)
  "Phases of a variant's timeline, as fractions of its duration.")

(defconst financial-chart-3d-framing-span 0.4
  "Least fraction of a frame's width and height the data's lit cells span.")

(defconst financial-chart-3d-framing-guides '((210 221 235) (138 148 166) (91 107 134))
  "Guide colours (axes, the zero frame, RSI bands): not data.")

(defconst financial-chart-3d-framing-floors
  '(("depth" . 0.07) ("depth-live" . 0.14) ("diverging-bars" . 0.09) ("drawdown" . 0.09)
    ("ladder" . 0.15) ("ohlc" . 0.06) ("panes" . 0.1) ("payoff" . 0.14)
    ("payoff-curves" . 0.12) ("volume-profile" . 0.06))
  "Least fraction of a frame's cells lit, per variant: about 70% of the
fewest its example lights at any size and phase.")

(defconst financial-chart-3d-framing-default-floor 0.04
  "Least fraction of cells lit for a variant `financial-chart-3d-framing-floors' lacks.")

(defun financial-chart-3d-framing-measure (frame cols rows)
  "FRAME's lit cells and the span of its data cells: (:lit :width :height).
:lit counts the cells with a dot; :width and :height are the fractions
of COLS and ROWS that the bounding box of the lit cells not in a
guide colour (`financial-chart-3d-framing-guides') covers."
  (let ((lit 0) c0 c1 r0 r1)
    (dotimes (i (* cols rows))
      (let ((cell (graphics-frame-cell frame i)))
        (when (> (car cell) 0)
          (cl-incf lit)
          (unless (member (cdr cell) financial-chart-3d-framing-guides)
            (let ((c (% i cols)) (r (/ i cols)))
              (setq c0 (min (or c0 c) c) c1 (max (or c1 c) c)
                    r0 (min (or r0 r) r) r1 (max (or r1 r) r)))))))
    (list :lit lit
          :width (if c0 (/ (- c1 c0 -1) (float cols)) 0.0)
          :height (if r0 (/ (- r1 r0 -1) (float rows)) 0.0))))

(defun financial-chart-3d-framing-failures (name)
  "The frames of variant NAME that do not read, as strings; nil when all do."
  (let ((view (financial-chart-3d-open name :id (concat "fc3d-framing:" name)))
        (floor (alist-get name financial-chart-3d-framing-floors
                          financial-chart-3d-framing-default-floor nil #'equal))
        failures)
    (unwind-protect
        (dolist (phase financial-chart-3d-framing-phases)
          (when-let* ((timeline (graphics-eas-timeline view)))
            (graphics-eas-timeline-set view :seconds (* phase (or (plist-get timeline :duration) 0))
                                       :playing nil))
          (pcase-dolist (`(,cols . ,rows) financial-chart-3d-framing-sizes)
            (let* ((m (financial-chart-3d-framing-measure (graphics-eas-frame view cols rows) cols rows))
                   (lit (/ (plist-get m :lit) (float (* cols rows)))))
              (unless (and (>= lit floor)
                           (>= (plist-get m :width) financial-chart-3d-framing-span)
                           (>= (plist-get m :height) financial-chart-3d-framing-span))
                (push (format "%s %dx%d t=%s: lit %.3f (floor %s), data spans %.2f x %.2f"
                              name cols rows phase lit floor (plist-get m :width) (plist-get m :height))
                      failures)))))
      (financial-chart-3d--close view))
    (nreverse failures)))

(ert-deftest financial-chart-3d-every-variant-reads-at-every-size-and-phase ()
  (graphics-ensure-core 'rs)
  (let ((failures (mapcan #'financial-chart-3d-framing-failures (financial-chart-3d-variants))))
    (should (equal failures nil))))

(provide 'financial-chart-3d-framing-test)
;;; financial-chart-3d-framing-test.el ends here
