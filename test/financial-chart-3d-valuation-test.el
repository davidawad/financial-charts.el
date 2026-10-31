;;; financial-chart-3d-valuation-test.el --- 3D valuation surfaces -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these with financial-chart-3d-test.el, against
;; the graphics.el and eas.el checkouts of `make deps-3d'.  The
;; variants' goldens, gallery entries, 2D drawing and replays are
;; checked there with every other variant; these check what the
;; valuation surfaces compute: the fixtures against the formulas
;; docs/3d-valuation.md derives them from, the Kelly growth rates, the
;; contour, plane and traces the marks draw, and the replays.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'financial-chart-3d-valuation)
(require 'graphics-eas)
(financial-chart-3d--ensure)

(defconst financial-chart-3d-valuation-test--names '("dcf-sensitivity" "revenue-surface" "kelly-surface"))

(defun financial-chart-3d-valuation-test--example (name)
  "The example bindings of variant NAME."
  (eas-template-example (financial-chart-3d--name name)))

(defun financial-chart-3d-valuation-test--graphics (name)
  "Variant NAME's x-eas.graphics with its slots bound to its example."
  (plist-get (plist-get (financial-chart-3d-spec name) :x-eas) :graphics))

(defun financial-chart-3d-valuation-test--without (plist keys)
  "PLIST without KEYS."
  (cl-loop for (k v) on plist by #'cddr unless (memq k keys) append (list k v)))

(defun financial-chart-3d-valuation-test--prims (name &optional rows)
  "Variant NAME's primitives over ROWS (default its example's data)."
  (let ((graphics (financial-chart-3d-valuation-test--graphics name)))
    (graphics-eas-primitives (financial-chart-3d-valuation-test--without graphics '(:data))
                             (append (or rows (plist-get (financial-chart-3d-valuation-test--example name) :data)) nil))))

(defun financial-chart-3d-valuation-test--prim (prims id)
  "The primitive ID among PRIMS."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

;;; Registration and the gallery

(ert-deftest financial-chart-3d-valuation-variants-are-registered ()
  (dolist (name financial-chart-3d-valuation-test--names)
    (should (member name (financial-chart-3d-variants)))
    (should (eas-template-get name))
    (should (member (plist-get (financial-chart-3d-valuation-test--graphics name) :mark) (graphics-eas-all-marks))))
  (dolist (mark '("fc-dcf3d" "fc-kelly3d"))
    (should (eq (alist-get mark graphics-eas-mark-functions nil nil #'equal)
                'financial-chart-3d-valuation--surface-mark)))
  (should (eq (alist-get "fc-revenue3d" graphics-eas-mark-functions nil nil #'equal)
              'financial-chart-3d-valuation--revenue-mark)))

(ert-deftest financial-chart-3d-valuation-2d-fallbacks ()
  ;; The 2D templates draw their fixtures; the DCF draws the latest
  ;; snapshot only (its top cell is the last snapshot's).
  (dolist (name financial-chart-3d-valuation-test--names)
    (let ((resolved (eas-resolve name (eas-template-example name))))
      (should-not (eas-spec-unsupported resolved))
      (should (> (length (eas-text-render (eas-compile resolved :target 'text :size '(:cols 60 :rows 16)))) 0))))
  (let* ((rows (append (plist-get (eas-template-example "dcf-sensitivity") :data) nil))
         (latest (seq-filter (lambda (r) (equal (plist-get r :as_of) "2026-09-30")) rows))
         (first (seq-filter (lambda (r) (equal (plist-get r :as_of) "2025-12-31")) rows))
         (text (eas-text-render (eas-compile (eas-resolve "dcf-sensitivity" (eas-template-example "dcf-sensitivity"))
                                             :target 'text :size '(:cols 60 :rows 16)))))
    (should (= (length latest) 63))
    (should (string-search (format "%d" (round (seq-max (mapcar (lambda (r) (plist-get r :value_per_share)) latest))))
                           text))
    ;; The first snapshot's lowest value is below all of the latest's.
    (should-not (string-match-p (format "[^0-9.]%d[^0-9]"
                                        (round (seq-min (mapcar (lambda (r) (plist-get r :value_per_share)) first))))
                                text))))

;;; Fixtures, against the formulas docs/3d-valuation.md gives

(defun financial-chart-3d-valuation-test--dcf (fcf1 wacc growth)
  "Value per share: five years of FCF1 growing 8%, a Gordon terminal value, less 12 net debt."
  (let ((pv 0.0) (f fcf1) last)
    (dotimes (i 5)
      (setq pv (+ pv (/ f (expt (+ 1 wacc) (1+ i)))) last f f (* f 1.08)))
    (- (+ pv (/ (/ (* last (+ 1 growth)) (- wacc growth)) (expt (+ 1 wacc) 5))) 12.0)))

(ert-deftest financial-chart-3d-valuation-dcf-fixture-is-derived ()
  (let ((rows (append (plist-get (financial-chart-3d-valuation-test--example "dcf-sensitivity") :data) nil))
        (fcf '(("2025-12-31" . 5.40) ("2026-03-31" . 5.65) ("2026-06-30" . 5.90) ("2026-09-30" . 6.20))))
    (should (= (length rows) (* 4 9 7)))
    (dolist (r rows)
      (should (< (abs (- (plist-get r :value_per_share)
                         (financial-chart-3d-valuation-test--dcf (cdr (assoc (plist-get r :as_of) fcf))
                                                                 (plist-get r :wacc) (plist-get r :terminal_growth))))
                 0.006)))
    ;; Value falls with WACC and rises with terminal growth.
    (let ((at (lambda (w g) (plist-get (seq-find (lambda (r) (and (equal (plist-get r :as_of) "2026-09-30")
                                                                  (= (plist-get r :wacc) w)
                                                                  (= (plist-get r :terminal_growth) g)))
                                                 rows)
                                       :value_per_share))))
      (should (> (funcall at 0.07 0.025) (funcall at 0.09 0.025) (funcall at 0.11 0.025)))
      (should (< (funcall at 0.09 0.01) (funcall at 0.09 0.025) (funcall at 0.09 0.04))))))

(ert-deftest financial-chart-3d-valuation-revenue-fixture-is-derived ()
  ;; Lognormal from FY2026 revenue 48.2: drift log 1.09, volatility 0.12.
  (let ((rows (append (plist-get (financial-chart-3d-valuation-test--example "revenue-surface") :data) nil))
        (z '((0.05 . -1.6449) (0.1 . -1.2816) (0.25 . -0.6745) (0.5 . 0.0) (0.75 . 0.6745)
             (0.9 . 1.2816) (0.95 . 1.6449))))
    (should (= (length rows) (* 8 7)))
    (dolist (r rows)
      (let ((tt (- (plist-get r :year) 2026)))
        (should (< (abs (- (plist-get r :revenue)
                           (* 48.2 (exp (+ (* (log 1.09) tt)
                                           (* (cdr (assoc (plist-get r :quantile) z)) 0.12 (sqrt tt)))))))
                   0.006))))))

(ert-deftest financial-chart-3d-valuation-kelly-fixture-is-computed ()
  (let ((example (financial-chart-3d-valuation-test--example "kelly-surface")))
    (cl-loop for a across (plist-get example :data)
             for b in (financial-chart-3d-kelly-surface :model (plist-get example :model)
                                                        :sigma (plist-get example :sigma))
             do (should (= (plist-get a :edge) (plist-get b :edge)))
             (should (= (plist-get a :fraction) (plist-get b :fraction)))
             (should (< (abs (- (plist-get a :growth) (plist-get b :growth))) 1.5e-4)))
    (should (= (length (plist-get example :data)) (* 12 13)))))

;;; Kelly

(ert-deftest financial-chart-3d-valuation-kelly-growth ()
  ;; Continuous: 10% excess return at 20% vol, held at 1.5x.
  (should (< (abs (- (financial-chart-3d-kelly-growth "continuous" 0.1 1.5 0.2) (- 0.15 0.045))) 1e-12))
  ;; Binary: even odds at 55% (edge 0.1), 10% staked.
  (should (< (abs (- (financial-chart-3d-kelly-growth "binary" 0.1 0.1 nil 1)
                     (+ (* 0.55 (log 1.1)) (* 0.45 (log 0.9)))))
             1e-12))
  (should-not (financial-chart-3d-kelly-growth "binary" 0.1 1.0))
  ;; The Kelly fraction is where growth peaks, and twice it grows nothing
  ;; (continuous).
  (dolist (model '(("continuous" 0.08 0.25 nil) ("binary" 0.2 nil 2.0)))
    (pcase-let* ((`(,m ,edge ,sigma ,odds) model)
                 (f (financial-chart-3d-kelly-fraction m edge sigma odds))
                 (g (lambda (x) (financial-chart-3d-kelly-growth m edge x sigma odds))))
      (should (> (funcall g f) (funcall g (- f 0.01))))
      (should (> (funcall g f) (funcall g (+ f 0.01))))))
  (should (< (abs (financial-chart-3d-kelly-growth "continuous" 0.08 (* 2 (/ 0.08 0.04)) 0.2)) 1e-12))
  ;; Ruin is the floor.
  (should (equal (plist-get (car (last (financial-chart-3d-kelly-surface :model "binary" :edges '(0.1)
                                                                          :fractions '(0.5 1.0) :floor -2.0)))
                            :growth)
                 -2.0)))

;;; The marks

(ert-deftest financial-chart-3d-valuation-crossings ()
  (should (equal (financial-chart-3d-valuation--crossings [[0.0 1.0] [1.0 2.0]] [0.0 1.0] [0.0 1.0] 0.5)
                 (list [0.5 0.0 0.0 0.5])))
  (should-not (financial-chart-3d-valuation--crossings [[1.0 1.0] [1.0 2.0]] [0.0 1.0] [0.0 1.0] 0.5))
  ;; A level across a slope: one segment per cell it passes through.
  (should (= (length (financial-chart-3d-valuation--crossings [[0.0 1.0 2.0] [0.0 1.0 2.0]] [0.0 1.0 2.0] [0.0 1.0] 1.5))
             1)))

(ert-deftest financial-chart-3d-valuation-dcf-plane-and-contour ()
  (let* ((rows (seq-filter (lambda (r) (equal (plist-get r :as_of) "2026-09-30"))
                           (append (plist-get (financial-chart-3d-valuation-test--example "dcf-sensitivity") :data) nil)))
         (prims (financial-chart-3d-valuation-test--prims "dcf-sensitivity" rows))
         (surface (financial-chart-3d-valuation-test--prim prims "surface"))
         (plane (plist-get (financial-chart-3d-valuation-test--prim prims "plane") :segments))
         (contour (plist-get (financial-chart-3d-valuation-test--prim prims "contour") :segments))
         (base (plist-get surface :base)) (scale (plist-get surface :y_scale))
         (price (plist-get (car rows) :price)))
    (should (equal (plist-get surface :type) "heightfield"))
    (should (= (length (plist-get surface :heights)) 7))
    (should (= (length (aref (plist-get surface :heights) 0)) 9))
    ;; The plane sits at the price, the colours split there.
    (should (< (abs (- (aref (aref plane 0) 1) (+ base (* scale price)))) 0.01))
    (should (< (abs (- (/ (+ (aref (plist-get surface :color_range) 0) (aref (plist-get surface :color_range) 1)) 2)
                       price))
               1e-9))
    ;; The fair-value line runs across the grid at the price.
    (should (> (length contour) 4))
    (seq-doseq (s contour)
      (should (< (abs (- (aref s 1) (aref (aref plane 0) 1))) 0.1)))))

(ert-deftest financial-chart-3d-valuation-revenue-traces-the-median ()
  (let* ((prims (financial-chart-3d-valuation-test--prims "revenue-surface"))
         (trace (plist-get (financial-chart-3d-valuation-test--prim prims "trace") :segments))
         (heights (plist-get (financial-chart-3d-valuation-test--prim prims "surface") :heights)))
    (should (= (length heights) 7))
    (should (= (length trace) 7))
    ;; The median rises every year, along the middle row.
    (seq-doseq (s trace)
      (should (< (aref s 1) (aref s 4)))
      (should (= (aref s 2) 0.0)))
    (should (equal (plist-get (financial-chart-3d-valuation-test--prim prims "plane") :segments) []))))

(ert-deftest financial-chart-3d-valuation-kelly-ridge-is-the-kelly-fraction ()
  (let* ((example (financial-chart-3d-valuation-test--example "kelly-surface"))
         (prims (financial-chart-3d-valuation-test--prims "kelly-surface"))
         (trace (plist-get (financial-chart-3d-valuation-test--prim prims "trace") :segments))
         (graphics (financial-chart-3d-valuation-test--graphics "kelly-surface"))
         (w (/ (float (elt (financial-chart-3d--opt graphics "extent" [12 9]) 0)) 2))
         (edges (delete-dups (mapcar (lambda (r) (plist-get r :edge)) (plist-get example :data)))))
    (should (= (length trace) (1- (length edges))))
    ;; Each edge's ridge point is the grid fraction nearest edge / sigma^2.
    (cl-loop for s across trace for e in edges
             do (let* ((fraction (* 3.0 (/ (+ (aref s 0) w) (* 2 w))))
                       (kelly (min 3.0 (financial-chart-3d-kelly-fraction "continuous" e 0.2))))
                  (should (<= (abs (- fraction kelly)) 0.125))))
    (should (plist-get (financial-chart-3d-valuation-test--prim prims "contour") :segments))))

;;; Replays

(ert-deftest financial-chart-3d-valuation-replays ()
  ;; The DCF's snapshots are keyframes; revenue replays year by year.
  (dolist (case '(("dcf-sensitivity" 4) ("revenue-surface" nil)))
    (let ((view (financial-chart-3d-open (car case) :id (concat "fc3d-valuation-test:" (car case)))))
      (unwind-protect
          (let ((timeline (graphics-eas-timeline view)))
            (should timeline)
            (if (cadr case) (should (= (plist-get timeline :keyframes) (cadr case)))
              (should (> (plist-get timeline :duration) 0)))
            (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
            (let ((start (graphics-eas-frame view 60 24)))
              (financial-chart-3d-seek-end view)
              (should-not (equal start (graphics-eas-frame view 60 24)))))
        (financial-chart-3d--close view))))
  ;; Each DCF snapshot carries its own price: the plane moves.
  (let* ((rows (append (plist-get (financial-chart-3d-valuation-test--example "dcf-sensitivity") :data) nil))
         (plane-y (lambda (as-of)
                    (let ((prims (let ((graphics-eas-domain-rows rows))
                                   (financial-chart-3d-valuation--surface-mark
                                    (financial-chart-3d-valuation-test--graphics "dcf-sensitivity")
                                    (seq-filter (lambda (r) (equal (plist-get r :as_of) as-of)) rows)))))
                      (aref (aref (plist-get (financial-chart-3d-valuation-test--prim prims "plane") :segments) 0) 1)))))
    (should (< (funcall plane-y "2026-06-30") (funcall plane-y "2025-12-31") (funcall plane-y "2026-09-30")))))

(provide 'financial-chart-3d-valuation-test)
;;; financial-chart-3d-valuation-test.el ends here
