;;; financial-chart-3d-options-test.el --- 3D options positioning: OI, GEX, skew, VRP -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these with financial-chart-3d-test.el, against
;; the graphics.el and eas.el checkouts in test/deps/.  The variants'
;; goldens, gallery sections and 2D drawing are checked there with every
;; other variant's; these check what the options readings compute, on
;; the fixtures in test/fixtures/3d/options/ (docs/3d-options.md).

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(financial-chart-3d--ensure)

(defconst financial-chart-3d-options-test--names
  '("options-oi" "options-gex" "options-skew" "options-vrp")
  "The options-positioning variants.")

(defun financial-chart-3d-options-test--fixture (name)
  "The fixture test/fixtures/3d/options/NAME.data.json, parsed as eas parses JSON."
  (eas-json-read-file (expand-file-name (format "test/fixtures/3d/options/%s.data.json" name)
                                        financial-chart-test-root)))

(defun financial-chart-3d-options-test--rows (name)
  "The data rows of fixture NAME, a list."
  (append (plist-get (financial-chart-3d-options-test--fixture name) :data) nil))

(defun financial-chart-3d-options-test--prim (prims id)
  "The primitive of PRIMS with ID."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

(defun financial-chart-3d-options-test--graphics (name &optional bindings)
  "The x-eas.graphics of variant NAME with its slots filled.
BINDINGS default to its example."
  (let* ((qualified (financial-chart-3d--name name))
         (spec (financial-chart-3d-spec qualified bindings)))
    (plist-get (plist-get spec :x-eas) :graphics)))

;;; Registration and coverage

(ert-deftest financial-chart-3d-options-variants-and-marks ()
  (dolist (name financial-chart-3d-options-test--names)
    (should (member name (financial-chart-3d-variants)))
    ;; The 2D fallback is a template of its own.
    (should (eas-template-get name)))
  (dolist (mark '("fc-strikes3d" "fc-skew3d" "fc-vrp3d"))
    (should (member mark (graphics-eas-all-marks)))))

(ert-deftest financial-chart-3d-options-slots-reach-the-mark ()
  (let ((oi (financial-chart-3d-options-test--graphics "options-oi"))
        (gex (financial-chart-3d-options-test--graphics "options-gex")))
    (should (= (plist-get oi :spot) 586.15))
    (should (= (length (financial-chart-3d--rows (plist-get oi :max_pain))) 4))
    (should (eq (plist-get gex :flip) t))))

;;; Max pain and the gamma flip

(ert-deftest financial-chart-3d-options-max-pain-by-hand ()
  ;; Calls at 100 (10) and 110 (5), puts at 90 (20) and 100 (5).  Paid
  ;; to holders at expiry: at 90, the puts at 100: 5 x 10 = 50; at 100,
  ;; nothing; at 110, the calls at 100: 10 x 10 = 100.  Max pain is 100.
  (should (equal (financial-chart-3d-options-max-pain
                  '((:expiry "a" :strike 90 :call_oi 0 :put_oi 20)
                    (:expiry "a" :strike 100 :call_oi 10 :put_oi 5)
                    (:expiry "a" :strike 110 :call_oi 5 :put_oi 0)))
                 '((:expiry "a" :max_pain 100))))
  ;; Expiries keep their order; ties go to the lower strike.
  (should (equal (financial-chart-3d-options-max-pain
                  '((:expiry "b" :strike 10 :call_oi 1 :put_oi 1)
                    (:expiry "b" :strike 20 :call_oi 1 :put_oi 1)
                    (:expiry "a" :strike 10 :call_oi 0 :put_oi 0)))
                 '((:expiry "b" :max_pain 10) (:expiry "a" :max_pain 10)))))

(ert-deftest financial-chart-3d-options-fixture-max-pain-is-the-chains ()
  ;; The fixture's opt.max_pain rows are what its chain gives.
  (let ((fixture (financial-chart-3d-options-test--fixture "oi")))
    (should (equal (financial-chart-3d-options-max-pain (plist-get fixture :data))
                   (append (plist-get fixture :max_pain) nil)))
    (should (equal (mapcar (lambda (r) (plist-get r :max_pain)) (append (plist-get fixture :max_pain) nil))
                   '(590 580 580 585)))))

(ert-deftest financial-chart-3d-options-gamma-flip ()
  (let ((rows '((:strike 90 :call_gex 1 :put_gex -5) (:strike 100 :call_gex 3 :put_gex -4)
                (:strike 110 :call_gex 6 :put_gex -1) (:strike 120 :call_gex 1 :put_gex -2))))
    (should (equal (financial-chart-3d-options-net-by-strike rows)
                   '((90.0 . -4.0) (100.0 . -1.0) (110.0 . 5.0) (120.0 . -1.0))))
    ;; Net crosses up between 100 and 110 (at 100 + 10/6) and down
    ;; between 110 and 120; the one nearest spot wins.
    (should (= (financial-chart-3d-options-gamma-flip rows 104) 101.67))
    (should (= (financial-chart-3d-options-gamma-flip rows 119) 118.33))
    (should (= (financial-chart-3d-options-gamma-flip rows) 101.67))
    (should-not (financial-chart-3d-options-gamma-flip '((:strike 1 :call_gex 1 :put_gex 0)
                                                         (:strike 2 :call_gex 2 :put_gex 0))))))

(ert-deftest financial-chart-3d-options-fixture-gex-is-signed-and-flips-below-spot ()
  (let* ((fixture (financial-chart-3d-options-test--fixture "gex"))
         (rows (append (plist-get fixture :data) nil))
         (flip (financial-chart-3d-options-gamma-flip rows (plist-get fixture :spot))))
    (dolist (r rows)
      (should (>= (plist-get r :call_gex) 0))
      (should (<= (plist-get r :put_gex) 0))
      (should (< (abs (- (plist-get r :net_gex) (+ (plist-get r :call_gex) (plist-get r :put_gex)))) 0.051)))
    ;; Puts dominate below 585, calls above: spot sits in positive gamma.
    (should (< 580 flip 585))
    (should (< flip (plist-get fixture :spot)))))

;;; fc-strikes3d

(ert-deftest financial-chart-3d-options-strikes-mark ()
  (let* ((graphics (financial-chart-3d-options-test--graphics "options-oi"))
         (rows (financial-chart-3d-options-test--rows "oi"))
         (prims (financial-chart-3d-options--strikes-mark graphics rows))
         (bars (financial-chart-3d-options-test--prim prims "bars"))
         (spot (financial-chart-3d-options-test--prim prims "spot"))
         (posts (financial-chart-3d-options-test--prim prims "max-pain")))
    (should (vectorp prims))
    ;; A column up (calls) and down (puts) per row.
    (should (= (length (plist-get bars :boxes)) (* 2 (length rows))))
    (should (= (seq-count (lambda (b) (< (aref b 1) 0)) (plist-get bars :boxes)) (length rows)))
    ;; The tallest column is `height' (3) above or below the floor.
    (should (= (seq-reduce (lambda (m b) (max m (aref b 4) (- (aref b 1)))) (plist-get bars :boxes) 0) 3.0))
    ;; Spot 586.15 is 0.23 of the way from strike 585 (index 6 of 12,
    ;; x 0.5) to 590.
    (should (= (aref (aref (plist-get spot :segments) 0) 0) 0.73))
    ;; Four posts and the path joining their tops.
    (should (= (length (plist-get posts :segments)) 7))
    (should-not (financial-chart-3d-options-test--prim prims "flip"))))

(ert-deftest financial-chart-3d-options-strikes-mark-computes-what-is-left-out ()
  (let* ((rows (financial-chart-3d-options-test--rows "oi"))
         (auto (financial-chart-3d-options--strikes-mark
                '(:mark "fc-strikes3d" :max_pain_auto t :spot 586.15) rows))
         (gex (financial-chart-3d-options--strikes-mark
               (financial-chart-3d-options-test--graphics "options-gex")
               (financial-chart-3d-options-test--rows "gex"))))
    ;; Max pain from the chain when no rows are given.
    (should (= (length (plist-get (financial-chart-3d-options-test--prim auto "max-pain-tops") :boxes)) 4))
    ;; The template binds an unbound max_pain slot as null: computed too.
    (let ((graphics (financial-chart-3d-options-test--graphics "options-oi" (list :data (vconcat rows) :spot 586.15))))
      (should (eq (plist-get graphics :max_pain) :null))
      (should (equal (plist-get (financial-chart-3d-options-test--prim
                                 (financial-chart-3d-options--strikes-mark graphics rows) "max-pain")
                                :segments)
                     (plist-get (financial-chart-3d-options-test--prim
                                 (financial-chart-3d-options--strikes-mark
                                  (financial-chart-3d-options-test--graphics "options-oi") rows)
                                 "max-pain")
                                :segments))))
    ;; GEX: a flip plane left of spot, no max pain.
    (let ((flip (aref (aref (plist-get (financial-chart-3d-options-test--prim gex "flip") :segments) 0) 0))
          (spot (aref (aref (plist-get (financial-chart-3d-options-test--prim gex "spot") :segments) 0) 0)))
      (should (< flip spot)))
    (should-not (financial-chart-3d-options-test--prim gex "max-pain"))))

;;; fc-skew3d

(ert-deftest financial-chart-3d-options-skew-mark ()
  (let* ((rows (financial-chart-3d-options-test--rows "skew"))
         (graphics (financial-chart-3d-options-test--graphics "options-skew"))
         (prims (financial-chart-3d-options--skew-mark graphics rows))
         (dates (delete-dups (mapcar (lambda (r) (plist-get r :date)) rows)))
         (tenors (delete-dups (mapcar (lambda (r) (plist-get r :tenor)) rows))))
    (should (equal tenors '(7 30 60 90 180)))
    (dolist (panel '("rr25" "bf25"))
      (let ((lines (financial-chart-3d-options-test--prim prims (concat panel "-lines")))
            (newest (financial-chart-3d-options-test--prim prims (concat panel "-newest"))))
        ;; A ribbon per tenor over every date.
        (should (= (length (plist-get lines :segments)) (* (length tenors) (1- (length dates)))))
        ;; The newest term structure across the tenors.
        (should (= (length (plist-get newest :segments)) (1- (length tenors))))))
    ;; Risk reversals are negative: the RR panel's zero level is its top.
    (let* ((zero (plist-get (financial-chart-3d-options-test--prim prims "rr25-zero") :segments))
           (axes (plist-get (financial-chart-3d-options-test--prim prims "rr25-axes") :segments)))
      (should (= (aref (aref zero 0) 1) (aref (aref axes 0) 4))))))

(ert-deftest financial-chart-3d-options-skew-mark-takes-any-panels ()
  ;; opt.positioning_skew rows bind with panels naming their field.
  (let* ((rows '((:date "2026-10-23" :tenor 30 :pos_skew 1.2) (:date "2026-10-23" :tenor 60 :pos_skew 1.4)
                 (:date "2026-10-30" :tenor 30 :pos_skew 1.5) (:date "2026-10-30" :tenor 60 :pos_skew 1.3)))
         (prims (financial-chart-3d-options--skew-mark '(:mark "fc-skew3d" :panels ["pos_skew"]) rows)))
    (should (= (length prims) 5))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "pos_skew-lines") :segments)) 2))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "pos_skew-newest") :segments)) 1))))

;;; fc-vrp3d

(ert-deftest financial-chart-3d-options-vrp-rows ()
  (should (equal (financial-chart-3d-options-vrp-rows '((:date "a" :iv 20 :rv 15.5) (:date "b" :iv 10)
                                                        (:date "c" :iv 12 :rv 13 :vrp -1)))
                 '((:date "a" :iv 20 :rv 15.5 :vrp 4.5) (:date "b" :iv 10) (:date "c" :iv 12 :rv 13 :vrp -1))))
  ;; The fixture's vrp is its iv - rv.
  (dolist (r (financial-chart-3d-options-test--rows "vrp"))
    (should (< (abs (- (plist-get r :vrp) (- (plist-get r :iv) (plist-get r :rv)))) 0.005))))

(ert-deftest financial-chart-3d-options-vrp-mark ()
  (let* ((rows (financial-chart-3d-options-test--rows "vrp"))
         (prims (financial-chart-3d-options--vrp-mark '(:mark "fc-vrp3d") rows))
         (premium (financial-chart-3d-options-test--prim prims "premium"))
         (colors (delete-dups (append (plist-get premium :colors) nil))))
    ;; Implied rich most days, realized above it in mid-October.
    (should (equal (sort colors #'string<) (list financial-chart-3d-options-call-color
                                                 financial-chart-3d-options-put-color)))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "iv") :segments)) (1- (length rows))))
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "iv-rank") :boxes)) (length rows)))
    ;; The ratio's line plus its 1.0 rule.
    (should (= (length (plist-get (financial-chart-3d-options-test--prim prims "iv-ratio") :segments)) (length rows)))
    ;; Without rank and ratio, just the ribbon.
    (let ((bare (financial-chart-3d-options--vrp-mark
                 '(:mark "fc-vrp3d")
                 (mapcar (lambda (r) (list :date (plist-get r :date) :iv (plist-get r :iv) :rv (plist-get r :rv))) rows))))
      (should (= (length bare) 4)))))

;;; Replays

(ert-deftest financial-chart-3d-options-snapshot-series-replay ()
  (dolist (name '("options-skew" "options-vrp"))
    (ert-info ((format "variant %s" name))
      (let ((view (financial-chart-3d-open name :id (concat "fc3d-options-test:" name))))
        (unwind-protect
            (let ((timeline (graphics-eas-timeline view)))
              (should timeline)
              (should (> (plist-get timeline :keyframes) 10))
              (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
              (let ((start (graphics-eas-frame view 60 24)))
                (financial-chart-3d-seek-end view)
                (should-not (equal start (graphics-eas-frame view 60 24)))))
          (financial-chart-3d--close view)))))
  ;; A single chain snapshot does not replay.
  (let ((view (financial-chart-3d-open "options-oi" :id "fc3d-options-test:oi")))
    (unwind-protect (should-not (graphics-eas-timeline view))
      (financial-chart-3d--close view))))

;;; Without graphics.el

(ert-deftest financial-chart-3d-options-draw-in-2d-without-graphics ()
  (let* ((eas (file-name-directory (locate-library "eas")))
         (dirs (mapcar (lambda (d) (expand-file-name d financial-chart-test-root))
                       '("src" "src/core" "src/indicators" "src/charts" "src/integrations")))
         (form `(progn
                  (require 'financial-chart-3d)
                  (when (featurep 'graphics-eas) (error "graphics.el was found"))
                  (dolist (name '("options-oi" "options-gex" "options-skew" "options-vrp"))
                    (dolist (source (list name (concat "fc3d/" name)))
                      (unless (> (length (eas-text-render
                                          (eas-compile (eas-resolve source (eas-template-example source))
                                                       :target 'text :size '(:cols 60 :rows 14))))
                                 0)
                        (error "%s did not draw in 2D" source))))
                  (condition-case err (progn (financial-chart-3d-open "options-oi") (error "no error"))
                    (financial-chart-3d-unavailable (princ (format "OK %s" (plist-get (cddr err) :code)))))))
         (output (with-temp-buffer
                   (list (apply #'call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" eas
                                (append (mapcan (lambda (d) (list "-L" d)) dirs)
                                        (list "--eval" (prin1-to-string form))))
                         (buffer-string)))))
    (should (equal (car output) 0))
    (should (string-match-p "OK GRAPHICS_MISSING" (cadr output)))))

(provide 'financial-chart-3d-options-test)
;;; financial-chart-3d-options-test.el ends here
