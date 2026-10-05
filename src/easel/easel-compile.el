;;; easel-compile.el --- compile: resolved spec + rows -> scene/v1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L4.  `easel-compile' turns a resolved chart/v1 spec (pure Vega-Lite
;; with inline data), optional replacement rows, a size and the
;; runtime's view state into scene/v1: views with bounds, invertible
;; scales, placed axes and legends, and marks whose items carry datum
;; back-references, resolved style, tooltip and href, plus a hit-test
;; index per mark.  It is pure; renderers and hit-testing read only the
;; scene.  A unit or layer is one scene view; each concat cell is its
;; own view.  Source rows are tagged with :_easel_row so a datum can be
;; traced to its input row through filters and calculations.

;;; Code:

(require 'easel-core)
(require 'easel-spec)
(require 'easel-data)
(require 'easel-transform)
(require 'easel-encode)
(require 'easel-scale)
(require 'easel-layout)
(require 'easel-marks)
(require 'easel-hit)
(require 'easel-compile-scales)
(require 'easel-compile-place)
(require 'easel-marks-bounds)
(require 'easel-theme)
(require 'easel-legend)

(defvar easel-compile-gc-threshold (* 64 1024 1024)
  "GC threshold compile runs under; compile allocates many small plists.")

(defconst easel-compile-row-key :_easel_row
  "Key tagging each source row with its index in the input data.")

(defun easel-compile--tag (rows)
  "Return ROWS (a vector or list of plists) tagged with their indices."
  (let ((i -1))
    (vconcat (seq-map (lambda (row) (setq i (1+ i))
                        (append row (list easel-compile-row-key i)))
                      rows))))

(defun easel-compile--view-id (node path)
  "Stable id for the view at PATH: its name, else derived from PATH."
  (or (plist-get node :name)
      (if (string-empty-p path) "main"
        (replace-regexp-in-string "\\`_" "" (replace-regexp-in-string "/" "_" path)))))

(defun easel-compile--node-data (node ctx override)
  "Rows for NODE given CTX; OVERRIDE replaces the root data."
  (let ((data (plist-get node :data)))
    (cond
     ((and override (string-empty-p (plist-get ctx :path))) (easel-compile--tag override))
     ((null data) (plist-get ctx :rows))
     ((vectorp (plist-get data :values)) (easel-compile--tag (plist-get data :values)))
     ((plist-get data :name)
      (easel-signal "INVALID_INPUT"
                    (format "Data %s is a template slot; resolve the template first" (plist-get data :name))
                    :path (concat (plist-get ctx :path) "/data")))
     (t (easel-signal "UNSUPPORTED_FEATURE" "Only inline data.values compiles natively"
                      :path (concat (plist-get ctx :path) "/data") :feature "data/url")))))

(defun easel-compile--unit (node ctx env)
  "Compile unit NODE under CTX into a unit plist (rows, encoding, mark)."
  (let* ((path (plist-get ctx :path))
         (rows (easel-transform-run (vconcat (plist-get ctx :transforms)) (plist-get ctx :rows) env
                                    (concat path "/transform")))
         (enc (easel-encode-normalize (plist-get ctx :encoding) rows))
         (derived (easel-encode-derive enc rows env))
         (mark (plist-get node :mark))
         (unit (list :path path :name (plist-get node :name)
                     :mark (easel-marks-resolve-mark
                            mark (or (plist-get ctx :config)
                                     (easel-theme-merge easel-theme-vega-lite easel-theme-default)))
                     :encoding (car derived) :rows (vconcat (cdr derived)) :env env
                     :aggregated (seq-some (lambda (d) (and (easel-object-p d) (plist-get d :aggregate)))
                                           (cl-loop for (_ d) on (plist-get ctx :encoding) by #'cddr collect d))
                     :params (plist-get node :params))))
    (easel-marks-stack unit)))

(defun easel-compile--child-ctx (node ctx key i rows)
  "Context for child I of NODE's KEY array, inheriting from CTX with ROWS."
  (list :rows rows
        :transforms (append (plist-get ctx :transforms) (append (plist-get node :transform) nil))
        :encoding (if (eq key :layer)
                      (let ((enc (plist-get ctx :encoding)))
                        (cl-loop for (ch d) on (plist-get node :encoding) by #'cddr
                                 do (setq enc (easel-plist-put enc ch d)))
                        enc)
                    nil)
        :path (format "%s/%s/%d" (plist-get ctx :path) (easel-key-name key) i)
        :config (plist-get ctx :config)
        :width (or (plist-get node :width) (plist-get ctx :width))
        :height (or (plist-get node :height) (plist-get ctx :height))))

(defun easel-compile--collect (node ctx env override group)
  "Walk NODE under CTX; return a layout tree.  GROUP collects layer units."
  (let* ((rows (easel-compile--node-data node ctx override))
         (ctx (plist-put (copy-sequence ctx) :rows rows))
         (own (let ((enc (plist-get ctx :encoding)))
                (cl-loop for (ch d) on (plist-get node :encoding) by #'cddr
                         do (setq enc (easel-plist-put enc ch d)))
                enc)))
    (cond
     ((or (plist-get node :vconcat) (plist-get node :hconcat))
      (let ((key (if (plist-get node :vconcat) :vconcat :hconcat)))
        (list :concat (if (eq key :vconcat) "v" "h")
              :children (seq-map-indexed
                         (lambda (child i)
                           (easel-compile--collect child (easel-compile--child-ctx node ctx key i rows)
                                                   env nil nil))
                         (plist-get node key)))))
     (t
      (let* ((g (or group (list :id (easel-compile--view-id node (plist-get ctx :path))
                                :path (plist-get ctx :path) :units nil
                                :spec-w (or (plist-get node :width) (plist-get ctx :width))
                                :spec-h (or (plist-get node :height) (plist-get ctx :height))
                                :params nil))))
        (when (plist-get node :params)
          (plist-put g :params (append (plist-get g :params)
                                       (mapcar (lambda (p) (append p (list :view (plist-get g :id))))
                                               (plist-get node :params)))))
        (if (plist-get node :layer)
            (seq-do-indexed
             (lambda (child i)
               (easel-compile--collect child (easel-compile--child-ctx node ctx :layer i rows) env nil g))
             (plist-get node :layer))
          (let ((uctx (plist-put (copy-sequence ctx) :encoding own)))
            (plist-put uctx :transforms (append (plist-get ctx :transforms)
                                                (append (plist-get node :transform) nil)))
            (plist-put g :units (append (plist-get g :units)
                                        (list (append (easel-compile--unit node uctx env)
                                                      (list :node node :ctx uctx)))))))
        (unless group (list :group g)))))))

(defun easel-compile--scales (group state metrics)
  "Build GROUP's scales, axis defs and legend specs; STATE gives zooms."
  (let* ((units (plist-get group :units))
         (config (plist-get metrics :config))
         (zoom (plist-get (plist-get state :domains) (easel-key (plist-get group :id))))
         (color (easel-compile-color-scale units config))
         (size (easel-compile-aux-scale units :size [4 361]))
         (opacity (easel-compile-aux-scale units :opacity [0.3 0.8]))
         (first-def (lambda (ch) (cdar (easel-compile--defs units ch))))
         (unit (car units))
         (shape (let ((type (plist-get (plist-get unit :mark) :type)))
                  (cond ((member type '("bar" "rect" "area" "square")) "square")
                        ((member type '("line" "rule")) "stroke") (t "circle"))))
         (style (easel-marks-legend-style unit metrics))
         (spec (lambda (channel def scale)
                 (list :channel channel :def def :scale scale :shape shape :style style))))
    (plist-put group :scales
               (append (cl-loop for ch in '(:x :y)
                                for s = (easel-compile-position-scale units ch (plist-get zoom ch))
                                when s append (list ch s))
                       (when color (list (nth 0 color) (nth 2 color)))
                       (when size (list :size size))
                       (when opacity (list :opacity opacity))))
    (plist-put group :axis-defs (list :x (funcall first-def :x) :y (funcall first-def :y)))
    (plist-put group :legend-specs
               (delq nil (list (when color (funcall spec (nth 0 color) (nth 1 color) (nth 2 color)))
                               (when size (funcall spec :size (funcall first-def :size) size))
                               (when opacity (funcall spec :opacity (funcall first-def :opacity) opacity)))))))

(defun easel-compile--ranges (group)
  "Map GROUP's positional scales onto its placed plot."
  (let ((x0 (plist-get group :x0)) (y0 (plist-get group :y0))
        (w (plist-get group :w)) (h (plist-get group :h)) (scales (plist-get group :scales)))
    (when (plist-get scales :x)
      (setq scales (plist-put scales :x (easel-compile-set-range (plist-get scales :x) (vector x0 (+ x0 w))))))
    (when-let* ((y (plist-get scales :y)))
      (setq scales (plist-put scales :y (easel-compile-set-range
                                         y (if (member (plist-get y :type) '("band" "point"))
                                               (vector y0 (+ y0 h)) (vector (+ y0 h) y0))))))
    (plist-put group :scales scales)))

(defun easel-compile--brushes (group state)
  "Brush marks for GROUP's interval params that hold a value in STATE."
  (let ((bounds (vector (plist-get group :x0) (plist-get group :y0) (plist-get group :w) (plist-get group :h)))
        out)
    (dolist (p (plist-get group :params))
      (let* ((select (plist-get p :select))
             (value (plist-get (plist-get state :params) (easel-key (plist-get p :name))))
             (xs (plist-get (plist-get group :scales) :x)))
        (when (and (or (equal select "interval") (equal (plist-get select :type) "interval"))
                   (not (equal (plist-get p :bind) "scales"))
                   (vectorp (plist-get value :x)) xs)
          (let ((a (easel-scale-apply xs (aref (plist-get value :x) 0)))
                (b (easel-scale-apply xs (aref (plist-get value :x) 1))))
            (when (and a b)
              (push (list :id (format "%s/brush:%s" (plist-get group :id) (plist-get p :name))
                          :mark "brush" :param (plist-get p :name) :interactive-off t :rows []
                          :items (vector (list :x (min a b) :y (aref bounds 1) :w (abs (- b a)) :h (aref bounds 3)
                                               :fill "#333333" :opacity 0.125 :stroke "#ffffff")))
                    out))))))
    (nreverse out)))

(defun easel-compile--view (group metrics state)
  "Assemble GROUP into a scene view."
  (let* ((bounds (vector (plist-get group :x0) (plist-get group :y0) (plist-get group :w) (plist-get group :h)))
         (scales (plist-get group :scales))
         (marks (seq-map-indexed
                 (lambda (unit k)
                   (unless (plist-get unit :items)
                     (setq unit (plist-put unit :items (easel-marks-items unit scales bounds metrics)))
                     (setq unit (plist-put unit :index nil)))
                   (let ((mark (list :id (or (plist-get unit :name) (format "%s/%d" (plist-get group :id) k))
                                     :mark (plist-get (plist-get unit :mark) :type)
                                     :path (plist-get unit :path)
                                     :rows (plist-get unit :rows)
                                     :items (plist-get unit :items))))
                     (unless (plist-get unit :index)
                       (plist-put unit :index (easel-hit-index mark)))
                     (append mark (list :index (plist-get unit :index)))))
                 (plist-get group :units)))
         (legend-y (plist-get group :y0)))
    (list :id (plist-get group :id) :path (plist-get group :path) :bounds bounds
          :clip (if (easel-compile--clipped-p group state) t :false)
          ;; Vega-Lite frames each plot with config.view.stroke; terminals don't.
          :frame (let ((stroke (plist-get (easel-theme-get (plist-get metrics :config) :view) :stroke)))
                   (unless (or (easel-layout-text-p metrics) (eq stroke :null))
                     (list :stroke (if (stringp stroke) stroke "#ddd"))))
          :scales scales
          :axes (vconcat (mapcar (lambda (axis)
                                   (easel-layout-axis-place
                                    axis (plist-get scales (easel-key (plist-get axis :channel))) bounds metrics))
                                 (plist-get group :axes-model)))
          :legends (vconcat
                    (if (easel-layout-text-p metrics)
                        (mapcar (lambda (legend)
                                  (prog1 (easel-legend-place
                                          legend (+ (aref bounds 0) (aref bounds 2) (plist-get metrics :legend-offset))
                                          legend-y metrics)
                                    (setq legend-y (+ legend-y (cdr (easel-legend-size legend metrics))))))
                                (plist-get group :legends-model))
                      (cl-mapcar (lambda (legend at)
                                   (easel-legend-place legend (+ (aref bounds 0) (car at)) (+ (aref bounds 1) (cdr at))
                                                       metrics))
                                 (plist-get group :legends-model) (plist-get group :legend-offsets))))
          :marks (vconcat (append marks (easel-compile--brushes group state)))
          :params (vconcat (mapcar (lambda (p) (plist-get p :name)) (plist-get group :params))))))

(defun easel-compile--title (spec)
  "The chart title text of SPEC, or nil."
  (let ((title (plist-get spec :title)))
    (cond ((and (stringp title) (not (string-empty-p title))) title)
          ((and (easel-object-p title) (stringp (plist-get title :text))) (plist-get title :text)))))

(defun easel-compile--env (spec state)
  "Param values visible to expressions: spec param :value defaults, then STATE."
  (let ((env nil))
    (cl-labels ((walk (node)
                  (seq-doseq (p (plist-get node :params))
                    (when (plist-member p :value)
                      (setq env (easel-plist-put env (easel-key (plist-get p :name)) (plist-get p :value)))))
                  (dolist (key '(:layer :vconcat :hconcat))
                    (seq-doseq (child (plist-get node key)) (walk child)))))
      (walk spec))
    (cl-loop for (k v) on (plist-get state :params) by #'cddr
             do (setq env (easel-plist-put env k v)))
    env))

(cl-defun easel-compile-plan (spec &key rows size target cell state)
  "Everything `easel-compile' derives before items: units, scales, layout.
Arguments as in `easel-compile'.  The runtime keeps the plan so that a
selection change can patch it (`easel-compile-patch') instead of
compiling again."
  (let* ((gc-cons-threshold (max gc-cons-threshold easel-compile-gc-threshold))
         (spec (easel-spec-validate spec))
         (unsupported (car (easel-spec-unsupported spec))))
    (when unsupported
      (easel-signal "UNSUPPORTED_FEATURE" (plist-get unsupported :message)
                    :path (plist-get unsupported :path) :feature (plist-get unsupported :feature)))
    (let* ((target (or target 'svg))
           (config (easel-theme-merge easel-theme-vega-lite easel-theme-default
                                      (let ((c (plist-get spec :config))) (and (easel-object-p c) c))))
           (metrics (easel-layout-metrics target cell config))
           (cellv (plist-get metrics :cell))
           (size (cond ((and (consp size) (plist-get size :cols))
                        (cons (* (plist-get size :cols) (aref cellv 0)) (* (plist-get size :rows) (aref cellv 1))))
                       (t size)))
           (rows (if (easel-data-p rows) (plist-get rows :rows) rows))
           (env (easel-compile--env spec state))
           (tree (easel-compile--collect spec (list :path "" :rows [] :transforms nil :encoding nil :config config)
                                         env rows nil))
           (groups (easel-place--groups tree))
           (title (easel-compile--title spec))
           (title-h (if title (+ (plist-get metrics :chart-title-size) (plist-get metrics :chart-title-pad)) 0)))
      (dolist (g groups) (easel-compile--scales g state metrics))
      (let ((total (easel-place-layout tree metrics title-h size)))
        (dolist (g groups) (easel-compile--ranges g))
        ;; Vega's canvas also holds whatever the marks overhang: measure
        ;; them, lay out again around them and move the items along.
        ;; Charts fitted to a size (Emacs windows) skip this; their
        ;; overhang mostly falls in the padding, and measuring would
        ;; compute every item twice.
        (when (and (null size) (not (easel-layout-text-p metrics))
                   (easel-compile--measure-marks groups metrics state))
          (let ((origins (mapcar (lambda (g) (cons (plist-get g :x0) (plist-get g :y0))) groups)))
            (setq total (easel-place-layout tree metrics title-h nil t))
            (cl-loop for g in groups for o in origins
                     do (easel-compile--ranges g)
                     (let ((dx (- (plist-get g :x0) (car o))) (dy (- (plist-get g :y0) (cdr o))))
                       (dolist (u (plist-get g :units))
                         (plist-put u :items (easel-marks-translate (plist-get u :items) dx dy)))))))
        (list :spec spec :metrics metrics :groups groups :total total :title title :env env)))))

(defun easel-compile--clipped-p (group state)
  "Non-nil when GROUP's marks are clipped to its plot.
Vega-Lite clips zoomable views (a param bound to scales), marks with
clip: true, and easel clips views zoomed in STATE."
  (or (plist-get (plist-get state :domains) (easel-key (plist-get group :id)))
      (seq-some (lambda (p) (equal (plist-get p :bind) "scales")) (plist-get group :params))
      (seq-some (lambda (u) (eq (plist-get (plist-get u :mark) :clip) t)) (plist-get group :units))))

(defun easel-compile--measure-marks (groups metrics state)
  "Compute GROUPS' items and record how far they overhang each plot.
Sets :mark-over [LEFT TOP RIGHT BOTTOM] and :scope-over (series marks'
overhang on the right) in pixels; clipped views overhang nothing, as in
Vega.  Return non-nil when anything overhangs, so chrome may grow."
  (let (grew)
    (dolist (g groups)
      (let* ((x0 (plist-get g :x0)) (y0 (plist-get g :y0)) (w (plist-get g :w)) (h (plist-get g :h))
             (bounds (vector x0 y0 w h))
             (over (lambda (b) (if b (vector (max 0 (- x0 (aref b 0))) (max 0 (- y0 (aref b 1)))
                                             (max 0 (- (aref b 2) x0 w)) (max 0 (- (aref b 3) y0 h)))
                                 (vector 0 0 0 0))))
             mbox sbox)
        (dolist (u (plist-get g :units))
          (unless (plist-get u :items)
            (plist-put u :items (easel-marks-items u (plist-get g :scales) bounds metrics)))
          (unless (easel-compile--clipped-p g state)
            (let ((b (easel-marks-bounds u metrics)))
              (setq mbox (easel-layout-union mbox b))
              (when (easel-marks-scope-p u) (setq sbox (easel-layout-union sbox b))))))
        (let ((m (funcall over mbox)))
          (when (seq-some #'cl-plusp m) (setq grew t))
          (plist-put g :mark-over m)
          (plist-put g :scope-over (aref (funcall over sbox) 2)))))
    grew))

(defun easel-compile-scene (plan state)
  "Assemble scene/v1 from PLAN under view STATE, reusing cached items."
  (let* ((gc-cons-threshold (max gc-cons-threshold easel-compile-gc-threshold))
         (metrics (plist-get plan :metrics)) (total (plist-get plan :total))
         (spec (plist-get plan :spec)) (groups (plist-get plan :groups)) (title (plist-get plan :title)))
    (append
     (list :contract "scene/v1" :target (plist-get metrics :target)
           :size (list :w (car total) :h (cdr total) :cell (plist-get metrics :cell))
           :background (let ((bg (plist-get spec :background)))
                         (if (stringp bg) bg (or (easel-theme-get (plist-get metrics :config) :background) "white")))
           :config (plist-get metrics :config))
     (when title
       (let* ((x1 (apply #'min (mapcar (lambda (g) (plist-get g :x0)) groups)))
              (x2 (apply #'max (mapcar (lambda (g) (+ (plist-get g :x0) (plist-get g :w))) groups)))
              (anchor (if (easel-layout-text-p metrics) "middle" (plist-get metrics :chart-title-anchor))))
         (list :title (list :text title
                            :x (pcase anchor ("start" x1) ("end" x2)
                                 (_ (if (easel-layout-text-p metrics) (/ (car total) 2.0) (/ (+ x1 x2) 2.0))))
                            :y (plist-get metrics :pad)
                            :align (pcase anchor ("start" "left") ("end" "right") (_ "center")) :baseline "top"
                            :fontSize (plist-get metrics :chart-title-size)
                            :fontWeight (plist-get metrics :chart-title-weight)))))
     (list :views (vconcat (mapcar (lambda (g) (easel-compile--view g metrics state)) groups))
           :params (vconcat (apply #'append (mapcar (lambda (g) (plist-get g :params)) groups)))))))

(cl-defun easel-compile (spec &key rows size target cell state)
  "Compile resolved chart/v1 SPEC to scene/v1.
ROWS (a vector of row plists, or data/v1) replaces the root data.
TARGET is `svg' (default) or `text'; CELL is the [W H] pixel size of a
text cell (default [7 14]).  SIZE is (W . H) in pixels to fit, or for
the text target (:cols C :rows R); nil keeps the spec's own sizes.
STATE is view state: (:domains (VIEW-KEY (:x [LO HI]) ...) :params ...).
Signals UNSUPPORTED_FEATURE (with the JSON path) for anything outside
the native subset."
  (easel-compile-scene (easel-compile-plan spec :rows rows :size size :target target :cell cell :state state)
                       state))

(provide 'easel-compile)
;;; easel-compile.el ends here
