;;; financial-chart-3d-options-test.el --- Options surfaces in 3D -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these with financial-chart-3d-test.el, against
;; the graphics.el and eas.el checkouts in test/deps/.  The fixtures
;; (test/fixtures/3d/options/, scripts/options3d_fixtures.py) are
;; checked against Black-Scholes-Merton computed here; the marks, the
;; replays, the bindings from an invest metric run and the 2D fallback
;; are checked as the user meets them.  The goldens and the gallery
;; page are financial-chart-3d-test.el's, which covers every variant.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(financial-chart-3d--ensure)

(defun financial-chart-3d-options-test--fixture (name)
  "The bindings of options fixture NAME."
  (eas-json-read-file (expand-file-name (format "test/fixtures/3d/options/%s.data.json" name)
                                        financial-chart-test-root)))

(defun financial-chart-3d-options-test--rows (name)
  "The rows of options fixture NAME, as a list."
  (append (plist-get (financial-chart-3d-options-test--fixture name) :data) nil))

(defun financial-chart-3d-options-test--prim (prims id)
  "The primitive ID among PRIMS."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

(defun financial-chart-3d-options-test--without (plist key)
  "PLIST without KEY."
  (cl-loop for (k v) on plist by #'cddr unless (eq k key) append (list k v)))

(defun financial-chart-3d-options-test--strips (prims)
  "The heightfield strips among PRIMS."
  (seq-filter (lambda (p) (equal (plist-get p :type) "heightfield")) prims))

;;; Black-Scholes-Merton, no dividends: the fixtures' reference

(defconst financial-chart-3d-options-test--vol 0.25)
(defconst financial-chart-3d-options-test--rate 0.04)

(defun financial-chart-3d-options-test--ncdf (x)
  "The standard normal CDF at X (Abramowitz and Stegun 26.2.17, 7.5e-8)."
  (let* ((k (/ 1.0 (+ 1.0 (* 0.2316419 (abs x)))))
         (poly (* k (+ 0.319381530 (* k (+ -0.356563782 (* k (+ 1.781477937 (* k (+ -1.821255978 (* k 1.330274429))))))))))
         (upper (* (financial-chart-3d-options-test--npdf x) poly)))
    (if (>= x 0) (- 1.0 upper) upper)))

(defun financial-chart-3d-options-test--npdf (x)
  "The standard normal density at X."
  (/ (exp (* -0.5 x x)) (sqrt (* 2 float-pi))))

(defun financial-chart-3d-options-test--call (s k days)
  "A K call's value at spot S with DAYS to expiry."
  (if (<= days 0) (max 0.0 (- s k))
    (let* ((tau (/ days 365.0)) (vol financial-chart-3d-options-test--vol)
           (r financial-chart-3d-options-test--rate) (sd (* vol (sqrt tau)))
           (d1 (/ (+ (log (/ s k)) (* (+ r (* 0.5 vol vol)) tau)) sd)))
      (- (* s (financial-chart-3d-options-test--ncdf d1))
         (* k (exp (- (* r tau))) (financial-chart-3d-options-test--ncdf (- d1 sd)))))))

(defun financial-chart-3d-options-test--greeks (s k days)
  "A long K call's greeks at spot S with DAYS to expiry, as the fixture has them."
  (let* ((tau (/ days 365.0)) (vol financial-chart-3d-options-test--vol)
         (r financial-chart-3d-options-test--rate) (sq (sqrt tau)) (sd (* vol sq))
         (d1 (/ (+ (log (/ s k)) (* (+ r (* 0.5 vol vol)) tau)) sd)) (d2 (- d1 sd))
         (n (financial-chart-3d-options-test--npdf d1)))
    (list :delta (financial-chart-3d-options-test--ncdf d1)
          :gamma (/ n (* s sd))
          :vega (/ (* s n sq) 100)
          :theta (/ (- (/ (* -1 s n vol) (* 2 sq))
                       (* r k (exp (- (* r tau))) (financial-chart-3d-options-test--ncdf d2)))
                    365)
          :vanna (/ (* -1 n d2) vol 100)
          :charm (/ (* -1 n (- (* 2 r tau) (* d2 sd))) (* 2 tau sd) 365))))

(defun financial-chart-3d-options-test--close (a b &optional tolerance)
  "Non-nil when A and B agree to TOLERANCE (1e-4) of the larger, or absolutely."
  (<= (abs (- a b)) (* (or tolerance 1e-4) (max 1.0 (abs a) (abs b)))))

;;; Fixtures

(ert-deftest financial-chart-3d-options-greeks-fixture-is-bsm ()
  (let ((rows (financial-chart-3d-options-test--rows "greeks-surface")))
    (should (= (length rows) (* 7 25)))
    (dolist (row rows)
      (let ((want (financial-chart-3d-options-test--greeks (plist-get row :spot) 100.0 (plist-get row :dte))))
        (dolist (greek financial-chart-3d-options-greeks)
          (let ((key (intern (concat ":" greek))))
            (ert-info ((format "%s at spot %s, %s days" greek (plist-get row :spot) (plist-get row :dte)))
              (should (financial-chart-3d-options-test--close (plist-get row key) (plist-get want key) 1e-5)))))
        (should (= (plist-get row :day) (- 60 (plist-get row :dte))))))
    ;; The shapes the surface shows: gamma peaks at the strike and
    ;; sharpens toward expiry; theta is all decay.
    (let ((gamma (lambda (s d) (plist-get (seq-find (lambda (r) (and (= (plist-get r :spot) s) (= (plist-get r :dte) d)))
                                                    rows)
                                          :gamma))))
      (should (> (funcall gamma 100.0 3) (* 4 (funcall gamma 100.0 60))))
      (should (> (funcall gamma 100.0 3) (funcall gamma 90.0 3))))
    (should (seq-every-p (lambda (r) (< (plist-get r :theta) 0)) rows))))

(ert-deftest financial-chart-3d-options-pnl-fixture-decays-to-the-payoff ()
  (let* ((rows (financial-chart-3d-options-test--rows "position-pnl-surface"))
         (value (lambda (s d) (+ (financial-chart-3d-options-test--call s 90.0 d)
                                 (* -2 (financial-chart-3d-options-test--call s 100.0 d))
                                 (financial-chart-3d-options-test--call s 110.0 d))))
         (cost (funcall value 100.0 45)))
    (dolist (row rows)
      (should (< (abs (- (plist-get row :pnl)
                         (* 100 (- (funcall value (plist-get row :spot) (plist-get row :dte)) cost))))
                 0.02)))
    ;; Opened flat at 100; the expiry edge is the butterfly's payoff.
    (should (= (plist-get (seq-find (lambda (r) (and (= (plist-get r :dte) 45) (= (plist-get r :spot) 100.0))) rows)
                          :pnl)
               0.0))
    (let ((expiry (seq-filter (lambda (r) (= (plist-get r :dte) 0)) rows)))
      (should (= (length expiry) 21))
      (should (= (plist-get (seq-find (lambda (r) (= (plist-get r :spot) 100.0)) expiry) :pnl)
                 (seq-max (mapcar (lambda (r) (plist-get r :pnl)) expiry)))))))

(ert-deftest financial-chart-3d-options-pop-fixture-is-lognormal ()
  (let ((rows (financial-chart-3d-options-test--rows "pop-surface")))
    (should (= (length rows) (* 7 17)))
    (dolist (row rows)
      (let* ((tau (/ (plist-get row :dte) 365.0)) (vol financial-chart-3d-options-test--vol)
             (r financial-chart-3d-options-test--rate) (sd (* vol (sqrt tau)))
             (be (plist-get row :breakeven)))
        (should (< (abs (- be (- (plist-get row :strike) (plist-get row :credit)))) 1e-3))
        (should (< (abs (- (plist-get row :pop)
                           (financial-chart-3d-options-test--ncdf
                            (/ (+ (log (/ 100.0 be)) (* (- r (* 0.5 vol vol)) tau)) sd))))
                   2e-4))
        ;; Touching the strike is likelier than finishing through it.
        (should (>= (plist-get row :prob_touch) (- 1 (plist-get row :pop) 1e-4)))))))

(ert-deftest financial-chart-3d-options-examples-are-the-fixtures ()
  ;; The templates' examples ship (examples/3d/); the fixtures are the
  ;; same bytes (scripts/options3d_fixtures.py writes both).
  (dolist (name financial-chart-3d-options-templates)
    (let ((read (lambda (file) (with-temp-buffer
                                 (insert-file-contents-literally (expand-file-name file financial-chart-test-root))
                                 (buffer-string)))))
      (should (equal (funcall read (format "examples/3d/%s.data.json" name))
                     (funcall read (format "test/fixtures/3d/options/%s.data.json" name))))
      (should (equal (eas-template-example-file (eas-template-get name))
                     (eas-template-example-file (eas-template-get (financial-chart-3d--name name)))))
      (should (file-exists-p (eas-template-example-file (eas-template-get name)))))))

;;; Marks

(ert-deftest financial-chart-3d-options-variants-and-marks-are-registered ()
  (dolist (name financial-chart-3d-options-templates)
    (should (member name (financial-chart-3d-variants)))
    (should (eas-template-get name))
    (should (eas-template-get (financial-chart-3d--name name))))
  (dolist (m financial-chart-3d-options-marks)
    (should (member (car m) (graphics-eas-all-marks)))))

(ert-deftest financial-chart-3d-options-greeks-mark-selects-the-greek ()
  (let* ((rows (financial-chart-3d-options-test--rows "greeks-surface"))
         (mark (lambda (greek)
                 (financial-chart-3d-options--greeks-mark
                  (list :mark "fc-greeks3d" :spot 100 :encoding (list :y (list :field greek)))
                  rows)))
         (gamma (funcall mark "gamma"))
         (vanna (funcall mark "vanna")))
    ;; A strip per gap between the 7 dates, 25 spots across.
    (should (= (length (financial-chart-3d-options-test--strips gamma)) 6))
    (should (= (length (aref (plist-get (car (financial-chart-3d-options-test--strips gamma)) :heights) 0)) 25))
    (should-not (equal (financial-chart-3d-options-test--strips gamma) (financial-chart-3d-options-test--strips vanna)))
    ;; Gamma never changes sign; vanna does, so its zero is framed.
    (should (equal (plist-get (financial-chart-3d-options-test--prim gamma "level") :segments) []))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim vanna "level") :segments)) 4))
    ;; The slice at spot 100 crosses every date and drops at the front.
    (should (= (length (plist-get (financial-chart-3d-options-test--prim gamma "spot") :segments)) 7))
    ;; Default greek: gamma.
    (should (equal (financial-chart-3d-options--greeks-mark '(:mark "fc-greeks3d") rows)
                   (financial-chart-3d-options--greeks-mark '(:mark "fc-greeks3d" :encoding (:y (:field "gamma"))) rows)))))

(ert-deftest financial-chart-3d-options-pnl-mark-ghosts-the-expiry-edge ()
  (let* ((rows (financial-chart-3d-options-test--rows "position-pnl-surface"))
         (early (seq-filter (lambda (r) (>= (plist-get r :dte) 24)) rows))
         (graphics '(:mark "fc-pnl3d" :spot 100))
         (full (financial-chart-3d-options--pnl-mark graphics rows))
         ;; Mid-replay: every row spans the grid, the first four dates are drawn.
         (mid (let ((graphics-eas-domain-rows rows)) (financial-chart-3d-options--pnl-mark graphics early))))
    (should (= (length (financial-chart-3d-options-test--strips full)) 8))
    (should (= (length (financial-chart-3d-options-test--strips mid)) 3))
    ;; Same scale mid-replay as at the end: strips keep their place.
    (should (equal (car (financial-chart-3d-options-test--strips mid)) (car (financial-chart-3d-options-test--strips full))))
    ;; The expiry payoff waits dim at the front until the replay reaches it,
    ;; then it is the bright front edge.
    (should (= (length (plist-get (financial-chart-3d-options-test--prim mid "ghost") :segments)) 20))
    (should (equal (plist-get (financial-chart-3d-options-test--prim full "ghost") :segments) []))
    (let ((front (plist-get (financial-chart-3d-options-test--prim full "front") :segments))
          (ghost (plist-get (financial-chart-3d-options-test--prim mid "ghost") :segments)))
      (should (equal front ghost)))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim full "level") :segments)) 4))))

(ert-deftest financial-chart-3d-options-pop-mark-colours-absolute-odds ()
  (let* ((prims (financial-chart-3d-options--pop-mark
                 '(:mark "fc-pop3d" :encoding (:y (:field "prob_touch")))
                 (financial-chart-3d-options-test--rows "pop-surface")))
         (strips (financial-chart-3d-options-test--strips prims)))
    (should (= (length strips) 6))
    (should (seq-every-p (lambda (p) (equal (plist-get p :color_range) [0 1])) strips))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "level") :segments)) 4))
    (should (equal (plist-get (financial-chart-3d-options-test--prim prims "ghost") :segments) []))))

(ert-deftest financial-chart-3d-options-surface-edge-cases ()
  (should (equal (financial-chart-3d-options--at '(1 2 3) [10 20 30] 2.5) '(1.5 . 25.0)))
  (should-not (financial-chart-3d-options--at '(1 2 3) [10 20 30] 4))
  ;; One date only: no strips, but the date is traced and nothing fails.
  (let ((prims (financial-chart-3d-options--pnl-mark
                '(:mark "fc-pnl3d") '((:spot 90 :dte 0 :pnl -1) (:spot 100 :dte 0 :pnl 4) (:spot 110 :dte 0 :pnl -1)))))
    (should-not (financial-chart-3d-options-test--strips prims))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "front") :segments)) 2)))
  (should (vectorp (financial-chart-3d-options--greeks-mark '(:mark "fc-greeks3d") nil))))

;;; Replays

(ert-deftest financial-chart-3d-options-replay-toward-expiry ()
  (dolist (name '("greeks-surface" "position-pnl-surface"))
    (ert-info ((format "variant %s" name))
      (let ((view (financial-chart-3d-open name :id (concat "fc3d-options-test-replay:" name))))
        (unwind-protect
            (let ((timeline (graphics-eas-timeline view)))
              (should timeline)
              (should (> (plist-get timeline :keyframes) 2))
              (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
              (let ((start (graphics-eas-frame view 60 24)))
                (financial-chart-3d-seek-end view)
                (should-not (equal start (graphics-eas-frame view 60 24)))))
          (financial-chart-3d--close view)))))
  ;; Strikes by expiry are not a time series: no replay.
  (let ((view (financial-chart-3d-open "pop-surface" :id "fc3d-options-test-pop")))
    (unwind-protect (should-not (graphics-eas-timeline view))
      (financial-chart-3d--close view))))

(ert-deftest financial-chart-3d-options-greek-slot-redraws ()
  (let* ((bindings (financial-chart-3d-options-test--fixture "greeks-surface"))
         (frame (lambda (greek)
                  (let ((view (financial-chart-3d-open "greeks-surface" :id (concat "fc3d-options-test-" greek)
                                                       :bindings (plist-put (copy-sequence bindings) :greek greek))))
                    (unwind-protect (progn (financial-chart-3d-seek-end view) (graphics-eas-frame view 60 24))
                      (financial-chart-3d--close view))))))
    (should-not (equal (funcall frame "gamma") (funcall frame "delta")))
    (should-error (financial-chart-3d-open "greeks-surface"
                                           :bindings (plist-put (copy-sequence bindings) :greek "rho"))
                  :type 'eas-error)))

;;; From an invest metric run

(ert-deftest financial-chart-3d-options-bindings-from-a-metric-run ()
  (let* ((rows (mapcar (lambda (r) (financial-chart-3d-options-test--without r :day))
                       (financial-chart-3d-options-test--rows "greeks-surface")))
         (output (list :metric "opt.greeks_grid" :title "AAPL 100C" :params (list :spot 101.5 :strike 100)
                       :rows (vconcat rows)))
         (bindings (financial-chart-3d-options-bindings "greeks-surface" output :greek "vega")))
    (should (equal (plist-get bindings :spot) 101.5))
    (should (equal (plist-get bindings :title) "AAPL 100C"))
    (should (equal (plist-get bindings :greek) "vega"))
    ;; :day is added for the replay, 0 at the furthest date.
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :day)) (plist-get bindings :data)))
                   '(0 15 30 39 46 53 57)))
    (let ((view (financial-chart-3d-open "greeks-surface" :bindings bindings :id "fc3d-options-test-metric")))
      (unwind-protect (should (> (plist-get (graphics-eas-timeline view) :keyframes) 2))
        (financial-chart-3d--close view))))
  (should-error (financial-chart-3d-options-bindings "nope" '(:rows [(:dte 1)])) :type 'financial-chart-invalid-data)
  (should-error (financial-chart-3d-options-bindings "pop-surface" '(:rows [])) :type 'financial-chart-invalid-data))

;;; Without graphics.el: the 2D templates

(ert-deftest financial-chart-3d-options-2d-fallback ()
  (dolist (name financial-chart-3d-options-templates)
    (ert-info ((format "template %s" name))
      (let ((text (eas-text-render (eas-compile (eas-resolve name (eas-template-example name))
                                                :target 'text :size '(:cols 60 :rows 14)))))
        (should (> (length text) 0))
        (should (string-search "DTE" text)))))
  (let ((financial-chart-3d-graphics-feature 'financial-chart-3d-options-test-no-graphics)
        (view nil))
    (unwind-protect
        (progn
          (setq view (financial-chart-3d-options-show "pop-surface" (financial-chart-3d-options-test--fixture "pop-surface")))
          (should (equal (eas-view-id view) "fc/pop-surface"))
          (should (get-buffer "*eas fc/pop-surface*")))
      (when view
        (when-let* ((buffer (get-buffer "*eas fc/pop-surface*"))) (kill-buffer buffer))
        (eas-view-close view)))))

(provide 'financial-chart-3d-options-test)
;;; financial-chart-3d-options-test.el ends here
