;;; financial-chart-3d-risk-test.el --- 3D portfolio-risk charts -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these with financial-chart-3d-test.el, against the
;; graphics.el and eas.el checkouts in test/deps/.  The risk templates'
;; goldens, gallery entries and template-body checks are that file's
;; (every variant); here: the fixtures hold what their contracts say
;; (docs/3d-risk.md), the marks draw what the numbers say, the series
;; replay, and without graphics.el the templates draw in 2D.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(require 'financial-chart-3d-test)
(financial-chart-3d--ensure)

(defun financial-chart-3d-risk-test--rows (name)
  "The example rows of risk template NAME (test/fixtures/3d/risk/NAME.json)."
  (append (plist-get (eas-json-read-file
                      (expand-file-name (format "test/fixtures/3d/risk/%s.json" name) financial-chart-test-root))
                     :data)
          nil))

(defun financial-chart-3d-risk-test--graphics (name)
  "The x-eas.graphics of risk variant NAME."
  (plist-get (plist-get (eas-template-get (financial-chart-3d--name name)) :meta) :graphics))

(defun financial-chart-3d-risk-test--prim (prims id)
  "The primitive of PRIMS with ID."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

(defun financial-chart-3d-risk-test--where (rows &rest pairs)
  "ROWS whose fields match PAIRS (KEY VALUE ...)."
  (seq-filter (lambda (r) (cl-loop for (k v) on pairs by #'cddr always (equal (plist-get r k) v))) rows))

;;; Templates

(ert-deftest financial-chart-3d-risk-templates-exist-in-2d-and-3d ()
  (dolist (name financial-chart-3d-risk-templates)
    (ert-info ((format "template %s" name))
      (should (file-exists-p (expand-file-name (format "templates/financial/%s.json" name) financial-chart-test-root)))
      (should (member name (financial-chart-3d-variants)))
      (should (member (plist-get (financial-chart-3d-risk-test--graphics name) :mark)
                      (mapcar #'car financial-chart-3d-risk-marks)))
      ;; The 2D template and the 3D variant draw the same example.
      (should (equal (eas-template-example name) (eas-template-example (financial-chart-3d--name name))))
      (let ((resolved (eas-resolve name (eas-template-example name))))
        (should-not (eas-spec-unsupported resolved))
        (should (> (length (eas-text-render (eas-compile resolved :target 'text :size '(:cols 60 :rows 14)))) 0))))))

(ert-deftest financial-chart-3d-risk-series-replay ()
  (dolist (name '("risk-correlation" "risk-contribution" "risk-factor-exposure" "risk-underwater"
                  "risk-return-distribution" "risk-frontier"))
    (ert-info ((format "variant %s" name))
      (financial-chart-3d-test--with-view (list name :id (concat "fc3d-risk-test-replay:" name))
        (let ((timeline (graphics-eas-timeline view)))
          (should timeline)
          (should (> (plist-get timeline :keyframes) 2))
          (should (plist-get timeline :loop))
          (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
          (let ((start (graphics-eas-frame view 60 24)))
            (financial-chart-3d-seek-end view)
            (should-not (equal start (graphics-eas-frame view 60 24)))))))))

;;; Fixtures hold what their contracts say

(ert-deftest financial-chart-3d-risk-fixtures-are-consistent ()
  ;; Every window's matrix is symmetric with a unit diagonal, in [-1, 1].
  (let ((rows (financial-chart-3d-risk-test--rows "risk-correlation")))
    (should (= (length rows) (* 12 25)))
    (dolist (r rows)
      (should (<= -1 (plist-get r :value) 1))
      (if (equal (plist-get r :row) (plist-get r :column))
          (should (= (plist-get r :value) 1))
        (should (= (plist-get r :value)
                   (plist-get (car (financial-chart-3d-risk-test--where
                                    rows :window (plist-get r :window)
                                    :row (plist-get r :column) :column (plist-get r :row)))
                              :value))))))
  ;; Risk contributions sum to 100 on every date.
  (let ((rows (financial-chart-3d-risk-test--rows "risk-contribution")))
    (dolist (date (delete-dups (mapcar (lambda (r) (plist-get r :date)) rows)))
      (should (< (abs (- 100 (apply #'+ (mapcar (lambda (r) (plist-get r :contribution))
                                                (financial-chart-3d-risk-test--where rows :date date)))))
                 1e-6))))
  ;; The frontiers have as many points on every date.
  (let* ((rows (financial-chart-3d-risk-test--rows "risk-frontier"))
         (dates (delete-dups (mapcar (lambda (r) (plist-get r :date)) rows))))
    (should (= (length (delete-dups (mapcar (lambda (d) (length (financial-chart-3d-risk-test--where rows :date d)))
                                            dates)))
               1))))

;;; Correlation: rows and the grid mark

(ert-deftest financial-chart-3d-risk-matrix-rows ()
  (let ((m '(:labels ["A" "B"] :matrix [[1 0.5] [0.5 1]])))
    (should (equal (financial-chart-3d-risk-matrix-rows m "2026-01-30")
                   '((:window "2026-01-30" :row "A" :column "A" :value 1)
                     (:window "2026-01-30" :row "A" :column "B" :value 0.5)
                     (:window "2026-01-30" :row "B" :column "A" :value 0.5)
                     (:window "2026-01-30" :row "B" :column "B" :value 1))))
    ;; A series of windows, by :windows or as a vector, with their dates.
    (let ((rows (financial-chart-3d-risk-matrix-rows
                 (list :windows (vector (append '(:date "d1") m) (append '(:date "d2") m))))))
      (should (= (length rows) 8))
      (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :window)) rows)) '("d1" "d2"))))
    (should (equal (financial-chart-3d-risk-matrix-rows (vector m m))
                   (financial-chart-3d-risk-matrix-rows (list :windows (list (append '(:window 0) m)
                                                                             (append '(:window 1) m)))))))
  (should (equal (length (financial-chart-3d-risk-matrix-rows '(:assets ("A" "B") :correlation ((1 0) (0 1))))) 4))
  (should-error (financial-chart-3d-risk-matrix-rows '(:labels ["A" "B"] :matrix [[1 0.5]]))
                :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-3d-risk-correlation-bars-follow-the-window ()
  ;; One window drawn at a time: SPY/TLT's bar hangs below the floor in
  ;; the first window and stands above it at the inflation scare's peak.
  (let* ((graphics (financial-chart-3d-risk-test--graphics "risk-correlation"))
         (rows (financial-chart-3d-risk-test--rows "risk-correlation"))
         (bars (lambda (window)
                 (let* ((these (financial-chart-3d-risk-test--where rows :window window))
                        (graphics-eas-domain-rows rows)
                        (prims (financial-chart-3d-risk--grid-mark graphics these)))
                   (plist-get (financial-chart-3d-risk-test--prim prims "bars") :boxes))))
         ;; Columns SPY QQQ TLT GLD HYG, row SPY first, its diagonal a tile:
         ;; SPY/QQQ is bar 0, SPY/TLT bar 1.
         (first (funcall bars "2025-11-30"))
         (peak (funcall bars "2026-05-31")))
    ;; 25 cells less the 5 diagonal tiles, in every frame.
    (should (= (length first) 20))
    (should (= (length peak) 20))
    (should (< (aref (aref first 1) 1) 0))
    (should (= (aref (aref first 1) 4) 0))
    (should (> (aref (aref peak 1) 4) 0))
    ;; The scale is the domain's [-1, 1]: a correlation of 0.924 is 92.4% of the height.
    (let ((height (* 0.45 5)))
      (should (< (abs (- (aref (aref first 0) 4) (* 0.924 height))) 0.002)))))

(ert-deftest financial-chart-3d-risk-stress-net-column ()
  (let* ((graphics (financial-chart-3d-risk-test--graphics "risk-stress"))
         (rows (financial-chart-3d-risk-test--rows "risk-stress"))
         (prims (financial-chart-3d-risk--grid-mark graphics rows))
         (bars (plist-get (financial-chart-3d-risk-test--prim prims "bars") :boxes))
         (colors (plist-get (financial-chart-3d-risk-test--prim prims "bars") :colors))
         (top (apply #'max (mapcar (lambda (s) (abs (apply #'+ (mapcar (lambda (r) (plist-get r :pnl))
                                                                       (financial-chart-3d-risk-test--where rows :scenario s)))))
                                   (delete-dups (mapcar (lambda (r) (plist-get r :scenario)) rows)))))
         (scale (/ (* 0.45 7) top)))
    ;; 7 scenarios x (5 positions + net).
    (should (= (length bars) 42))
    ;; The GFC row's net bar is the row's sum, a loss; 2022's (stocks and
    ;; bonds falling together) is the deepest bar of all.
    (let ((net (aref bars 5))
          (gfc (apply #'+ (mapcar (lambda (r) (plist-get r :pnl))
                                  (financial-chart-3d-risk-test--where rows :scenario "2008 GFC")))))
      (should (< (abs (- (aref net 1) (* scale gfc))) 0.002))
      (should (= (aref (aref bars (+ (* 2 6) 5)) 1) (apply #'min (mapcar (lambda (b) (aref b 1)) bars)))))
    ;; Losses are red, gains green.
    (should (> (car (financial-chart-3d-risk--rgb (aref colors 0))) (cadr (financial-chart-3d-risk--rgb (aref colors 0)))))
    (should (> (cadr (financial-chart-3d-risk--rgb (aref colors 2))) (car (financial-chart-3d-risk--rgb (aref colors 2)))))))

;;; Ribbons: contributions, exposures, underwater

(ert-deftest financial-chart-3d-risk-underwater-rows ()
  (should (equal (financial-chart-3d-risk-underwater-rows
                  '((:date 1 :strategy "a" :equity 100) (:date 1 :strategy "b" :equity 50)
                    (:date 2 :strategy "a" :equity 80) (:date 2 :strategy "b" :equity 55)
                    (:date 3 :strategy "a" :equity 120) (:date 3 :strategy "b" :equity :null)
                    (:date 4 :strategy "a" :equity 90)))
                 '((:date 1 :strategy "a" :equity 100.0 :drawdown 0.0)
                   (:date 1 :strategy "b" :equity 50.0 :drawdown 0.0)
                   (:date 2 :strategy "a" :equity 80.0 :drawdown -20.0)
                   (:date 2 :strategy "b" :equity 55.0 :drawdown 0.0)
                   (:date 3 :strategy "a" :equity 120.0 :drawdown 0.0)
                   (:date 4 :strategy "a" :equity 90.0 :drawdown -25.0)))))

(ert-deftest financial-chart-3d-risk-underwater-marks-each-worst-drawdown ()
  (let* ((graphics (financial-chart-3d-risk-test--graphics "risk-underwater"))
         (rows (financial-chart-3d-risk-test--rows "risk-underwater"))
         (prims (financial-chart-3d-risk--ribbons-mark graphics rows))
         (worst (plist-get (financial-chart-3d-risk-test--prim prims "worst") :segments))
         (drawdowns (financial-chart-3d-risk-underwater-rows rows))
         (deepest (apply #'min (mapcar (lambda (r) (plist-get r :drawdown)) drawdowns)))
         (height 4.0) (lift (* 0.9 height)))
    ;; Two segments per strategy: down to the trough, across the slab.
    (should (= (length worst) 8))
    ;; The deepest strategy's trough is `height' below its high-water line.
    (should (seq-some (lambda (s) (< (abs (- (- (aref s 1) (aref s 4)) height)) 0.002)) worst))
    ;; Each lane's zero sits `stack' x height above the next one toward the front.
    (let ((zeros (sort (delete-dups (mapcar (lambda (s) (aref s 1))
                                            (append (plist-get (financial-chart-3d-risk-test--prim prims "zero") :segments) nil)))
                       #'<)))
      (should (equal (mapcar #'financial-chart-3d-risk--r3 zeros)
                     (mapcar #'financial-chart-3d-risk--r3 (list 0.0 lift (* 2 lift) (* 3 lift))))))
    (should (< deepest -15))))

(ert-deftest financial-chart-3d-risk-exposure-ribbons-cross-zero ()
  ;; HML turns from short to long: its ribbon goes below the zero plane and above it.
  (let* ((graphics (financial-chart-3d-risk-test--graphics "risk-factor-exposure"))
         (rows (financial-chart-3d-risk-test--rows "risk-factor-exposure"))
         (prims (financial-chart-3d-risk--ribbons-mark graphics (financial-chart-3d-risk-test--where rows :factor "HML")))
         (ys (mapcar (lambda (v) (aref v 1)) (plist-get (financial-chart-3d-risk-test--prim prims "slabs") :vertices))))
    (should (< (apply #'min ys) 0))
    (should (> (apply #'max ys) 0))
    ;; One slab per factor in the full chart, the factors' 2D colours.
    (let* ((all (financial-chart-3d-risk--ribbons-mark graphics rows))
           (colors (delete-dups (append (plist-get (financial-chart-3d-risk-test--prim all "slabs") :colors) nil))))
      (should (equal colors (seq-take (append financial-chart-3d-risk-series-colors nil) 5))))))

;;; Distributions

(ert-deftest financial-chart-3d-risk-histograms-and-var ()
  (should (= (financial-chart-3d-risk-quantile '(4 1 3 2 5) 0.25) 2.0))
  (should (= (financial-chart-3d-risk-quantile '(1 2) 0.5) 1.5))
  (let* ((rows (financial-chart-3d-risk-test--rows "risk-return-distribution"))
         (hist (financial-chart-3d-risk-histograms rows :bins 17)))
    (should (equal (plist-get hist :lanes) '("1d" "5d" "21d" "63d")))
    (dolist (h (plist-get hist :histograms))
      (should (= (plist-get h :count) 200))
      (should (< (abs (- 1 (seq-reduce #'+ (plist-get h :shares) 0))) 1e-9))
      ;; CVaR is beyond VaR.
      (should (> (plist-get h :cvar) (plist-get h :var))))
    ;; Risk grows with the horizon.
    (let ((vars (mapcar (lambda (h) (plist-get h :var)) (plist-get hist :histograms))))
      (should (equal vars (sort (copy-sequence vars) #'<))))
    ;; VaR is minus the 5% quantile of the lane's returns.
    (let ((one-day (mapcar (lambda (r) (plist-get r :return)) (financial-chart-3d-risk-test--where rows :horizon "1d"))))
      (should (< (abs (- (plist-get (car (plist-get hist :histograms)) :var)
                         (- (financial-chart-3d-risk-quantile one-day 0.05))))
                 1e-9)))))

(ert-deftest financial-chart-3d-risk-histogram-mark ()
  (let* ((graphics (financial-chart-3d-risk-test--graphics "risk-return-distribution"))
         (rows (financial-chart-3d-risk-test--rows "risk-return-distribution"))
         (prims (financial-chart-3d-risk--histogram-mark (plist-put (copy-sequence graphics) :bins 17) rows))
         (bars (plist-get (financial-chart-3d-risk-test--prim prims "bars") :boxes))
         (var (plist-get (financial-chart-3d-risk-test--prim prims "var") :segments)))
    ;; A gate of three segments per lane.
    (should (= (length var) 12))
    ;; Each lane's tallest bar is the height.
    (should (= (apply #'max (mapcar (lambda (b) (aref b 4)) bars)) 2.4))
    (should (<= (length bars) (* 4 17)))))

;;; Frontier

(ert-deftest financial-chart-3d-risk-frontier-surface ()
  (let* ((graphics (financial-chart-3d-risk-test--graphics "risk-frontier"))
         (rows (financial-chart-3d-risk-test--rows "risk-frontier"))
         (prims (financial-chart-3d-risk--frontier-mark graphics rows))
         (surface (financial-chart-3d-risk-test--prim prims "surface"))
         (best (plist-get (financial-chart-3d-risk-test--prim prims "best") :points)))
    ;; 8 dates of 15 points: 7 ribbons of 14 cells, two triangles each.
    (should (= (length (plist-get surface :triangles)) (* 7 14 2)))
    (should (= (length best) 8))
    ;; Each date's marked point is its greatest Sharpe ratio.
    (let* ((first (financial-chart-3d-risk-test--where rows :date "2024-12-31"))
           (top (seq-reduce (lambda (m r) (if (> (plist-get r :sharpe) (plist-get m :sharpe)) r m)) first (car first)))
           (xs (mapcar (lambda (r) (plist-get r :vol)) rows))
           (x (- (* 12 (/ (- (plist-get top :vol) (apply #'min xs)) (- (apply #'max xs) (apply #'min xs)))) 6)))
      (should (< (abs (- (aref (aref best 0) 0) x)) 0.002)))
    ;; The newest frontier is drawn white.
    (should (member "#ffffff" (append (plist-get (financial-chart-3d-risk-test--prim prims "curves") :colors) nil)))))

;;; Without graphics.el

(ert-deftest financial-chart-3d-risk-falls-back-to-2d ()
  (let ((financial-chart-3d-graphics-feature 'financial-chart-3d-risk-test-no-such-feature))
    (should-not (financial-chart-3d-available-p))
    (dolist (name financial-chart-3d-risk-templates)
      (let ((view (financial-chart-3d-risk-open name :id (concat "fc3d-risk-test-2d:" name))))
        (unwind-protect
            (progn
              (should (equal (eas-view-id view) (concat "fc3d-risk-test-2d:" name)))
              (should (equal (eas-view-template view) name))
              (should-not (graphics-eas-timeline view))
              (should (eas-view-scene view)))
          (eas-view-close view)))))
  (should-error (financial-chart-3d-risk-open "nope") :type 'financial-chart-invalid-data))

(provide 'financial-chart-3d-risk-test)
;;; financial-chart-3d-risk-test.el ends here
