;;; financial-chart-3d-market-test.el --- 3D market structure: rrg, breadth, liquidity, session profiles -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these with financial-chart-3d-test.el, against
;; the graphics.el and eas.el checkouts of `make deps-3d'.  That file's
;; tests cover these variants with every other (template body, 2D
;; draw, goldens, the gallery page); these check what
;; src/financial-chart-3d-market.el computes and draws: the helpers
;; against hand-worked values, the fixtures against their generators,
;; the marks' primitives, the replays and the live liquidity heatmap.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(financial-chart-3d--ensure)

(defconst financial-chart-3d-market-test-names
  '("rrg" "sector-breadth" "liquidity-heatmap" "session-profile")
  "The market-structure variants.")

(defun financial-chart-3d-market-test--example (name)
  "Rows of variant NAME's example (its data slot) as a list."
  (append (plist-get (eas-template-example (financial-chart-3d--name name)) :data) nil))

(defun financial-chart-3d-market-test--prim (prims id)
  "The primitive ID of PRIMS."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

(defmacro financial-chart-3d-market-test--with-view (spec &rest body)
  "Bind `view' to variant SPEC = (NAME . OPEN-ARGS) around BODY, then close it."
  (declare (indent 1))
  `(let ((view (apply #'financial-chart-3d-open (car ,spec) (cdr ,spec))))
     (unwind-protect (progn ,@body)
       (financial-chart-3d--close view))))

;;; The variants

(ert-deftest financial-chart-3d-market-variants-are-registered ()
  (dolist (name financial-chart-3d-market-test-names)
    (should (member name (financial-chart-3d-variants)))
    ;; The 2D fallback is the template the variant keeps the body of.
    (should (eas-template-get name))
    (should (member (plist-get (plist-get (plist-get (eas-template-get (financial-chart-3d--name name)) :meta)
                                          :graphics)
                               :mark)
                    (mapcar #'car financial-chart-3d-market-marks))))
  (dolist (mark financial-chart-3d-market-marks)
    (should (member (car mark) (graphics-eas-all-marks)))))

(ert-deftest financial-chart-3d-market-variants-replay ()
  (dolist (name financial-chart-3d-market-test-names)
    (ert-info ((format "variant %s" name))
      (financial-chart-3d-market-test--with-view (list name :id (concat "fc3d-market-test-replay:" name))
        (let ((timeline (graphics-eas-timeline view)))
          (should timeline)
          (should (> (plist-get timeline :keyframes) 2))
          (should (plist-get timeline :loop))
          (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
          (let ((start (graphics-eas-frame view 60 24)))
            (financial-chart-3d-seek-end view)
            (should-not (equal start (graphics-eas-frame view 60 24)))))))))

(ert-deftest financial-chart-3d-market-2d-without-graphics ()
  ;; The fallbacks draw with eas alone: graphics.el not on the load path.
  (let* ((eas (file-name-directory (locate-library "eas")))
         (dirs (mapcar (lambda (d) (expand-file-name d financial-chart-test-root))
                       '("src" "src/core" "src/indicators" "src/charts" "src/integrations")))
         (form `(progn
                  (require 'financial-chart-3d)
                  (when (financial-chart-3d-available-p) (error "graphics.el was found"))
                  (dolist (name ',financial-chart-3d-market-test-names)
                    (dolist (template (list name (concat "fc3d/" name)))
                      (let ((text (eas-text-render (eas-compile (eas-resolve template (eas-template-example template))
                                                                :target 'text :size '(:cols 60 :rows 14)))))
                        (unless (> (length text) 0) (error "%s did not draw" template)))))
                  (condition-case err (progn (financial-chart-3d-liquidity-open
                                              '(:bids [[99 1]] :asks [[101 1]]))
                                             (error "no error"))
                    (financial-chart-3d-unavailable (princ (format "OK %s" (plist-get (cddr err) :code)))))))
         (output (with-temp-buffer
                   (list (apply #'call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" eas
                                (append (mapcan (lambda (d) (list "-L" d)) dirs)
                                        (list "--eval" (prin1-to-string form))))
                         (buffer-string)))))
    (should (equal (car output) 0))
    (should (string-match-p "OK GRAPHICS_MISSING" (cadr output)))))

;;; Fixtures: generated, realistic

(ert-deftest financial-chart-3d-market-fixtures-are-generated ()
  (let ((dir (make-temp-file "fc3d-market" t)))
    (unwind-protect
        (progn
          (financial-chart-3d-market-write-fixtures dir)
          (dolist (name financial-chart-3d-market-test-names)
            (let ((file (concat name ".data.json")))
              (should (equal (cons file (with-temp-buffer
                                          (insert-file-contents (expand-file-name file dir)) (buffer-string)))
                             (cons file (with-temp-buffer
                                          (insert-file-contents
                                           (expand-file-name file financial-chart-3d-market-fixtures-directory))
                                          (buffer-string))))))))
      (delete-directory dir t))))

(ert-deftest financial-chart-3d-market-fixtures-tell-their-story ()
  ;; Breadth: a broad selloff (under 30% on average) then a recovery past 65%.
  (let* ((rows (financial-chart-3d-market-test--example "sector-breadth"))
         (dates (financial-chart-3d-market--keys (mapcar (lambda (r) (plist-get r :date)) rows)))
         (mean (lambda (date) (let ((v (mapcar (lambda (r) (plist-get r :pct))
                                               (seq-filter (lambda (r) (equal (plist-get r :date) date)) rows))))
                                (/ (apply #'+ v) (length v))))))
    (should (= (length rows) (* 11 (length dates))))
    (should (< (apply #'min (mapcar mean dates)) 30))
    (should (> (funcall mean (car (last dates))) 65))
    (should (seq-every-p (lambda (r) (<= 0 (plist-get r :pct) 100)) rows)))
  ;; Liquidity: the ask wall at 5003 holds, then is gone and the mid is above it.
  (let* ((rows (financial-chart-3d-market-test--example "liquidity-heatmap"))
         (at (lambda (step price) (plist-get (seq-find (lambda (r) (and (= (plist-get r :step) step)
                                                                        (= (plist-get r :price) price)))
                                                       rows)
                                             :size)))
         (mid (lambda (step) (plist-get (seq-find (lambda (r) (and (= (plist-get r :step) step)
                                                                   (equal (plist-get r :side) "mid")))
                                                  rows)
                                        :mid))))
    (should (> (funcall at 10 5003.0) 40))
    (should (< (funcall at 40 5003.0) 10))
    (should (> (funcall mid 47) 5003.0))
    (should (< (funcall mid 0) 5001.0)))
  ;; RRG: every sector, every date after warm-up, centred on 100.
  (let ((rows (financial-chart-3d-market-test--example "rrg")))
    (should (= (length rows) (* 11 27)))
    (should (seq-every-p (lambda (r) (< 85 (plist-get r :rs_ratio) 115)) rows))))

;;; Helpers

(ert-deftest financial-chart-3d-market-rrg-rows ()
  ;; A symbol that moves with the benchmark sits at 100, 100.
  (let ((rows (financial-chart-3d-rrg-rows '(("A" . (10 11 12 13 14 15 16 17))) '(100 110 120 130 140 150 160 170)
                                           :ratio 3 :momentum 2 :dates '(a b c d e f g h))))
    (should (equal (mapcar (lambda (r) (plist-get r :date)) rows) '(d e f g h)))
    (should (seq-every-p (lambda (r) (and (= (plist-get r :rs_ratio) 100.0) (= (plist-get r :rs_momentum) 100.0)))
                         rows)))
  ;; By hand: rs 1, 1, 1.1, 1.21 -> ratio at 3 = 100 * 1.21 / mean(1, 1.1, 1.21).
  (let ((row (car (last (financial-chart-3d-rrg-rows '(("B" . (1 1 1.1 1.21))) '(1 1 1 1) :ratio 3 :momentum 2)))))
    (should (= (plist-get row :rs_ratio) (financial-chart-3d-market--round (/ 121.0 (/ 3.31 3)) 2)))
    (should (= (plist-get row :rs_momentum)
               (financial-chart-3d-market--round
                (* 100 (/ (/ 121.0 (/ 3.31 3)) (/ (+ (/ 121.0 (/ 3.31 3)) (/ 110.0 (/ 3.1 3))) 2))) 2))))
  ;; A relative-strength cycle rotates clockwise: leading, weakening, lagging, improving.
  (let* ((n 80)
         (rows (financial-chart-3d-rrg-rows
                (list (cons "C" (cl-loop for i below n collect (* 100 (exp (* 0.08 (sin (/ (* 2 float-pi i) 24))))))))
                (make-list n 100)))
         (points (mapcar (lambda (r) (cons (- (plist-get r :rs_ratio) 100) (- (plist-get r :rs_momentum) 100))) rows))
         (area (cl-loop for (a b) on points while b sum (- (* (car a) (cdr b)) (* (car b) (cdr a))))))
    (should (< area 0)))
  (should-error (financial-chart-3d-rrg-rows '(("A" . (1 2))) '(1 2) :dates '(x)) :type 'financial-chart-invalid-data))

(ert-deftest financial-chart-3d-market-session-profile ()
  (let* ((bars '((:time "2026-10-19T09:30:00" :open 10 :high 12 :low 10 :close 11 :volume 100)
                 (:time "2026-10-19T10:00:00" :open 11 :high 12 :low 11 :close 12 :volume 50)
                 (:time "2026-10-20T09:30:00" :open 12 :high 14 :low 12 :close 13 :volume 200)))
         (rows (financial-chart-3d-session-profile bars :bins 4))
         (day (lambda (s) (seq-filter (lambda (r) (equal (plist-get r :session) s)) rows))))
    (should (= (length rows) 8))
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :session)) rows)) '("2026-10-19" "2026-10-20")))
    ;; Four levels of 1 from 10 to 14, shared by both sessions.
    (should (equal (mapcar (lambda (r) (plist-get r :price)) (funcall day "2026-10-19")) '(10.5 11.5 12.5 13.5)))
    ;; Day one: 100 over 10-12 (50 each), 50 over 11-12.
    (should (equal (mapcar (lambda (r) (plist-get r :volume)) (funcall day "2026-10-19")) '(50.0 100.0 0.0 0.0)))
    (should (equal (mapcar (lambda (r) (plist-get r :poc)) (funcall day "2026-10-19")) '(0 1 0 0)))
    (should (= (plist-get (car (funcall day "2026-10-19")) :vwap)
               (financial-chart-3d-market--round (/ (+ (* 100 (/ 33 3.0)) (* 50 (/ 35 3.0))) 150) 4)))
    (should (equal (mapcar (lambda (r) (plist-get r :volume)) (funcall day "2026-10-20")) '(0.0 0.0 100.0 100.0))))
  ;; Epoch milliseconds are sessions by their UTC date.
  (should (equal (financial-chart-3d-market--session-of '(:time 1792396800000)) "2026-10-19")))

(ert-deftest financial-chart-3d-market-snapshot-deltas-replay-the-book ()
  (let* ((snapshots (financial-chart-3d-market-liquidity-snapshots :steps 12 :levels 6))
         (book (financial-chart-book-make (car snapshots))))
    (cl-loop for (from to) on snapshots while to
             do (financial-chart-book-apply book (financial-chart-3d-market-snapshot-deltas from to) 0)
             (should (equal (financial-chart-3d-liquidity-rows book 6 1)
                            (financial-chart-3d-liquidity-rows (financial-chart-book-make to) 6 1))))
    (let ((rows (financial-chart-3d-liquidity-rows book 6 7)))
      (should (= (length rows) 13))
      (should (seq-every-p (lambda (r) (= (plist-get r :step) 7)) rows))
      (should (= (seq-count (lambda (r) (equal (plist-get r :side) "mid")) rows) 1)))))

;;; Marks

(ert-deftest financial-chart-3d-market-rrg-mark ()
  (let* ((rows (financial-chart-3d-market-test--example "rrg"))
         (graphics '(:mark "fc-rrg3d" :tail 5 :encoding (:time (:field "date"))))
         (prims (financial-chart-3d-market--rrg-mark graphics rows))
         (heads (financial-chart-3d-market-test--prim prims "heads")))
    (should (= (length (plist-get heads :boxes)) 11))
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "trails") :segments)) (* 11 4)))
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "stems") :segments)) 11))
    ;; Each head stands where its symbol is on the newest date: x by RS-Ratio.
    (let* ((newest (seq-filter (lambda (r) (equal (plist-get r :date) "2026-10-02")) rows))
           (xs (mapcar (lambda (b) (/ (+ (aref b 0) (aref b 3)) 2)) (plist-get heads :boxes))))
      (should (equal (mapcar (lambda (r) (> (plist-get r :rs_ratio) 100)) newest)
                     (mapcar (lambda (x) (> x 0)) xs))))
    ;; Under a replay the axes span every row, so early frames sit where they will.
    (let* ((early (seq-filter (lambda (r) (string< (plist-get r :date) "2026-05-01")) rows))
           (graphics-eas-domain-rows rows)
           (a (financial-chart-3d-market--rrg-mark graphics early)))
      (should (equal (plist-get (financial-chart-3d-market-test--prim a "quadrants") :segments)
                     (plist-get (financial-chart-3d-market-test--prim prims "quadrants") :segments))))))

(ert-deftest financial-chart-3d-market-breadth-mark ()
  (let* ((rows (financial-chart-3d-market-test--example "sector-breadth"))
         (dates (length (delete-dups (mapcar (lambda (r) (plist-get r :date)) (copy-sequence rows)))))
         (surface (financial-chart-3d-market-test--prim
                   (financial-chart-3d-market--breadth-mark '(:mark "fc-breadth3d" :style "surface") rows) "surface"))
         (ridges (financial-chart-3d-market-test--prim
                  (financial-chart-3d-market--breadth-mark '(:mark "fc-breadth3d") rows) "surface")))
    (should (equal (plist-get surface :type) "heightfield"))
    (should (= (length (plist-get surface :heights)) dates))
    (should (= (length (aref (plist-get surface :heights) 0)) 11))
    (should (equal (plist-get surface :color_range) [0.0 100.0]))
    ;; One ridge per sector, along the dates.
    (should (equal (plist-get ridges :type) "segments"))
    (should (= (length (plist-get ridges :segments)) (* 11 (1- dates))))
    ;; One date drawn: no surface yet, the front traced.
    (let ((one (financial-chart-3d-market--breadth-mark
                '(:mark "fc-breadth3d") (seq-take rows 11))))
      (should (equal (plist-get (financial-chart-3d-market-test--prim one "surface") :segments) []))
      (should (= (length (plist-get (financial-chart-3d-market-test--prim one "front") :segments)) 10)))))

(ert-deftest financial-chart-3d-market-liquidity-mark ()
  (let* ((rows (financial-chart-3d-market-test--example "liquidity-heatmap"))
         (prims (financial-chart-3d-market--liquidity-mark '(:mark "fc-liquidity3d") rows))
         (walls (financial-chart-3d-market-test--prim prims "surface"))
         (sizes (sort (mapcar (lambda (r) (plist-get r :size))
                              (seq-filter (lambda (r) (member (plist-get r :side) '("bid" "ask"))) rows))
                      #'<))
         (median (nth (/ (length sizes) 2) sizes))
         (heavy (seq-count (lambda (v) (>= v (* 3 median))) sizes)))
    (should (equal (plist-get walls :type) "boxes"))
    ;; The walls, not the book: a few levels over most steps.
    (should (< 48 heavy 200))
    (should (= (length (plist-get walls :boxes)) heavy))
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "mid") :segments)) 47))
    ;; As a surface: one row per tick from the lowest price to the highest.
    (let* ((surface (financial-chart-3d-market-test--prim
                     (financial-chart-3d-market--liquidity-mark '(:mark "fc-liquidity3d" :style "surface") rows)
                     "surface"))
           (prices (mapcar (lambda (r) (plist-get r :price))
                           (seq-filter (lambda (r) (member (plist-get r :side) '("bid" "ask"))) rows))))
      (should (equal (plist-get surface :type) "heightfield"))
      (should (= (length (plist-get surface :heights))
                 (1+ (round (/ (- (apply #'max prices) (apply #'min prices)) 0.25)))))
      (should (= (length (aref (plist-get surface :heights) 0)) 48)))
    ;; max_rows keeps the newest steps.
    (should (= (length (plist-get (financial-chart-3d-market-test--prim
                                   (financial-chart-3d-market--liquidity-mark
                                    '(:mark "fc-liquidity3d" :max_rows 10) rows)
                                   "mid")
                                  :segments))
               9))))

(ert-deftest financial-chart-3d-market-session-profile-mark ()
  (let* ((rows (financial-chart-3d-market-test--example "session-profile"))
         (prims (financial-chart-3d-market--session-profile-mark '(:mark "fc-session-profile3d") rows))
         (sessions 8) (bins 20))
    ;; A slab per session: four vertices per level, six triangles per step.
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "profiles") :vertices))
               (* sessions bins 4)))
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "profiles") :triangles))
               (* sessions 6 (1- bins))))
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "poc") :segments)) sessions))
    ;; The VWAP path: one segment between consecutive sessions, one tick per session.
    (should (= (length (plist-get (financial-chart-3d-market-test--prim prims "vwap") :segments))
               (+ (1- sessions) sessions)))
    ;; Each cap sits over its session's point of control.
    (let ((caps (plist-get (financial-chart-3d-market-test--prim prims "poc") :segments))
          (pocs (seq-filter (lambda (r) (eql (plist-get r :poc) 1)) rows)))
      (should (= (length pocs) sessions))
      (should (apply #'< (mapcar (lambda (s) (aref s 2)) caps))))))

;;; Live liquidity

(ert-deftest financial-chart-3d-market-liquidity-streams ()
  (let* ((eas-stream-use-timers nil)
         (clock 0)
         (eas-stream-clock (lambda () (cl-incf clock)))
         (snapshots (financial-chart-3d-market-liquidity-snapshots :steps 5 :levels 8))
         (view (financial-chart-3d-liquidity-open (car snapshots) :levels 8 :history 3 :id "fc3d-market-test-live")))
    (unwind-protect
        (progn
          ;; Live: no replay, the newest step drawn.
          (should-not (graphics-eas-timeline view))
          (let ((first (graphics-eas-frame view 60 24)))
            (cl-loop for (from to) on snapshots while to
                     do (should (plist-get (financial-chart-3d-liquidity-push
                                            view (financial-chart-3d-market-snapshot-deltas from to) 0)
                                           :step)))
            ;; The stream keeps the newest history steps.
            (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :step))
                                                (append (plist-get (eas-view-data view) :rows) nil)))
                           '(2 3 4)))
            (should-not (equal first (graphics-eas-frame view 60 24)))))
      (financial-chart-3d-liquidity-close view))
    (should-error (financial-chart-3d-liquidity-push view []) :type 'financial-chart-invalid-book)))

(provide 'financial-chart-3d-market-test)
;;; financial-chart-3d-market-test.el ends here
