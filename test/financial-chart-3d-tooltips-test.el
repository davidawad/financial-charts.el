;;; financial-chart-3d-tooltips-test.el --- every 3D variant hovers -*- lexical-binding: t; -*-

;;; Commentary:

;; Each 3D variant's mark gives its elements short tooltips (at most
;; `financial-chart-3d-tooltip-lines' lines, never a whole row), one per
;; element of the primitive that carries them.  The mark-level tests
;; lower each variant's mark over its example: with graphics.el, through
;; its `graphics-eas-primitives', reading each element's pick back as the
;; text a hover shows (the mark's own, else the variant's
;; `encoding.tooltip' over its datum); without it, this package's marks
;; but those built on graphics.el's (bar3d, area3d).  The hover test
;; draws each example at 60x24 and reads the tooltips graphics.el's pick
;; gives its cells, skipped when graphics.el or its pick is absent.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'financial-chart-3d-rates)
(require 'financial-chart-3d-valuation)
(require 'graphics-eas nil t)

(declare-function graphics-eas-primitives "graphics-eas-marks" (graphics rows &optional params domain-rows))
(declare-function graphics-eas-show "graphics-eas" (view &optional cols rows window))
(declare-function graphics-eas-picks-create "graphics-eas-marks")
(declare-function graphics-eas-pick-entry "graphics-eas-marks" (picks id))
(declare-function graphics-eas-tooltip "graphics-eas-marks" (tooltip entry))
(defvar graphics-eas-picks)

(defconst financial-chart-3d-tooltips-test--marks
  (append financial-chart-3d-marks financial-chart-3d-rates-marks financial-chart-3d-risk-marks
          financial-chart-3d-options-marks financial-chart-3d-market-marks financial-chart-3d-futures-marks
          financial-chart-3d-positioning-marks financial-chart-3d-vol-surface-marks
          financial-chart-3d-valuation-marks)
  "Every mark this package defines: (MARK . FUNCTION).")

(defun financial-chart-3d-tooltips-test--fill (x bindings slots)
  "X, a variant's x-eas.graphics, with its slot references from BINDINGS or SLOTS' defaults."
  (cond ((and (consp x) (keywordp (car x)) (plist-member x :x-eas:slot))
         (let ((slot (financial-chart-3d--key (plist-get x :x-eas:slot))))
           (or (plist-get bindings slot) (plist-get (plist-get slots slot) :default))))
        ((and (consp x) (keywordp (car x)))
         (cl-loop for (k v) on x by #'cddr
                  append (list k (financial-chart-3d-tooltips-test--fill v bindings slots))))
        ((vectorp x) (vconcat (mapcar (lambda (e) (financial-chart-3d-tooltips-test--fill e bindings slots)) x)))
        (t x)))

(defun financial-chart-3d-tooltips-test--texts (p picks tooltip)
  "Primitive P with its pick `:ids' read back from PICKS as `:tooltips',
each the text a hover shows under `encoding.tooltip' TOOLTIP."
  (let ((ids (plist-get p :ids)))
    (if (not (seq-some #'integerp ids)) p
      (append p (list :tooltips (cl-map 'vector (lambda (id)
                                                 (let ((entry (graphics-eas-pick-entry picks id)))
                                                   (and entry (graphics-eas-tooltip tooltip entry))))
                                        ids))))))

(defun financial-chart-3d-tooltips-test--primitives (name)
  "The primitives of 3D variant NAME's mark over its example, at its replay's end.
Nil when the mark is graphics.el's and graphics.el is not loaded."
  (let* ((q (financial-chart-3d--name name))
         (meta (plist-get (eas-template-get q) :meta))
         (bindings (financial-chart-3d-bindings q))
         (json (eas-json-read-file (expand-file-name (concat name ".json") financial-chart-3d-templates-directory)))
         (slot (financial-chart-3d--key (or (plist-get (plist-get json :data) :name) "data")))
         (shape (plist-get (plist-get (plist-get meta :slots) slot) :shape))
         (data (plist-get bindings slot))
         (rows (financial-chart-3d--rows (if shape (plist-get (eas-data-from shape data) :rows) data)))
         (graphics (financial-chart-3d-tooltips-test--fill (plist-get meta :graphics) bindings
                                                           (plist-get meta :slots)))
         (fn (cdr (assoc (plist-get graphics :mark) financial-chart-3d-tooltips-test--marks))))
    (cond ((financial-chart-3d-available-p) ; registers this package's marks with graphics.el
           ;; graphics.el turns each element's tooltip into a pick id; read them back.
           (let ((picks (graphics-eas-picks-create)))
             (mapcar (lambda (p) (financial-chart-3d-tooltips-test--texts
                                  p picks (plist-get (plist-get graphics :encoding) :tooltip)))
                     (let ((graphics-eas-picks picks)) (graphics-eas-primitives graphics rows)))))
          ;; Without graphics.el: this package's marks, but those built on graphics.el's.
          ((and fn (not (memq fn '(financial-chart-3d--drawdown-mark financial-chart-3d--depth-mark
                                                                     financial-chart-3d--diverging-mark))))
           (append (funcall fn graphics rows) nil)))))

(defun financial-chart-3d-tooltips-test--elements (p)
  "How many elements primitive P has: boxes, segments, points, samples or triangles."
  (pcase (plist-get p :type)
    ("boxes" (length (plist-get p :boxes)))
    ("segments" (length (plist-get p :segments)))
    ("points" (length (plist-get p :points)))
    ("heightfield" (seq-reduce #'+ (mapcar #'length (plist-get p :heights)) 0))
    ("mesh" (length (plist-get p :triangles)))))

(defun financial-chart-3d-tooltips-test--compact-p (tip)
  "Non-nil when TIP is a non-empty tooltip of at most the allowed lines."
  (and (stringp tip) (not (string-empty-p tip))
       (<= (length (split-string tip "\n")) financial-chart-3d-tooltip-lines)))

(ert-deftest financial-chart-3d-tooltips-every-variant-has-compact-tooltips ()
  "Each variant's mark tips the elements of some primitive, a short tooltip each."
  (dolist (name (financial-chart-3d-variants))
    (ert-info ((format "variant %s" name))
      (let ((prims (financial-chart-3d-tooltips-test--primitives name)))
        (when prims
          (let ((tipped (seq-filter (lambda (p) (> (length (plist-get p :tooltips)) 0)) prims)))
            (should tipped)
            (dolist (p (seq-filter (lambda (p) (plist-get p :tooltips)) prims))
              (ert-info ((format "primitive %s" (plist-get p :id)))
                (should (= (length (plist-get p :tooltips)) (financial-chart-3d-tooltips-test--elements p)))
                (should (seq-every-p #'financial-chart-3d-tooltips-test--compact-p (plist-get p :tooltips)))))))))))

(ert-deftest financial-chart-3d-tooltips-cds-term-structure-segments ()
  "The ISDA history example tips every curve segment: entity, date, tenors, spread, PD, source."
  (let* ((prims (financial-chart-3d-tooltips-test--primitives "cds-term-structure"))
         (front (seq-find (lambda (p) (equal (plist-get p :id) "front")) prims))
         (tip (aref (plist-get front :tooltips) 0)))
    (should (= (length (plist-get front :tooltips)) (length (plist-get front :segments))))
    (should (equal tip (concat "Northwind Industrials Inc  2026-10-20  1Y to 2Y\n"
                               "spread 58.0 to 77.0 bp\n"
                               "PD 1.13% to 2.79%\n"
                               "source finance-rs ISDA CDS Standard Model")))))

(ert-deftest financial-chart-3d-tooltips-cds-bond-ladder-is-compact ()
  "A bond's tooltip names its numbers, never the run's summary."
  (let* ((output (plist-get (eas-json-read-file
                             (expand-file-name "test/fixtures/3d/rates/cds-bond-ladder.data.json"
                                               financial-chart-test-root))
                            :data))
         (rows (financial-chart-3d-rates-cds-bond-rows output))
         (bars (seq-find (lambda (p) (equal (plist-get p :id) "bars"))
                         (financial-chart-3d-rates--cds-bond-mark '(:mark "fc-cds-bond-ladder3d") rows)))
         (tip (aref (plist-get bars :tooltips) 0)))
    (should (plist-get (car rows) :summary))
    (should (equal tip (concat "1.55% Notes Due 2028  matures 2028-06-15  coupon 1.55%\n"
                               "model price 93.331  yield 5.912%  CDS 142.6 bp\n"
                               "mod. duration 1.556  DV01 $182609  expected loss $28650023\n"
                               "extrapolated: spread below")))
    (dolist (tip (append (plist-get bars :tooltips) nil))
      (should (financial-chart-3d-tooltips-test--compact-p tip))
      (should-not (string-match-p "summary\\|total_\\|cds_tenors_skipped" tip)))))

(ert-deftest financial-chart-3d-tooltips-helpers ()
  (should (equal (financial-chart-3d-num 3) "3"))
  (should (equal (financial-chart-3d-num 3.14159) "3.14"))
  (should (equal (financial-chart-3d-num 3.14159 0) "3"))
  (should (equal (financial-chart-3d-num :null) "-"))
  (should (equal (financial-chart-3d-tip "a" nil "" "b" "c" "d" "e") "a\nb\nc\nd"))
  (should (equal (financial-chart-3d-row-tip "T" '(:a 1 :b :null :c 2.5) '(:a :b (:c "cee" 1)))
                 "T\na 1  cee 2.5")))

(ert-deftest financial-chart-3d-tooltips-every-example-frame-hovers ()
  "Drawn at 60x24, every variant's example has a cell whose hover is a short tooltip."
  (skip-unless (and (fboundp 'graphics-eas-show) (fboundp 'graphics-eas-view-picks)
                    (financial-chart-3d-available-p)))
  (dolist (name (financial-chart-3d-variants))
    (ert-info ((format "variant %s" name))
      (let* ((view (financial-chart-3d-open name :id (format "fc3d-test-hover:%s" name)))
             texts)
        (unwind-protect
            (progn
              (financial-chart-3d-seek-end view)
              (let* ((buffer (graphics-eas-show view 60 24))
                     (window (get-buffer-window buffer t)))
                (with-current-buffer buffer
                  (let ((pos (point-min)))
                    (while (< pos (point-max))
                      (let ((help (get-text-property pos 'help-echo)))
                        (cond ((functionp help)
                               (let ((text (funcall help window buffer pos)))
                                 (when text (push text texts))))
                              ((stringp help) (push help texts))))
                      (setq pos (1+ pos)))))))
          (financial-chart-3d--close view))
        (should (seq-some #'financial-chart-3d-tooltips-test--compact-p texts))))))

(provide 'financial-chart-3d-tooltips-test)
;;; financial-chart-3d-tooltips-test.el ends here
