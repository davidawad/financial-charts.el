;;; easel-bench-test.el --- tests for the performance bead (fc-qx1.9) -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)
(require 'easel-bench)
(require 'easel-agent)

;; The calibration's best-of-five matters for CI numbers, not for tests.
(setq easel-bench-calibration-runs 1)

;;; Indexed param filters agree with the row-by-row test

(defconst easel-bench-test--values
  [1 1.0 2 -0.0 0 "2026-01-02" "2026-01-02T00:00:00Z" 1767312000000 "abc" "2" :null 2.5]
  "Values that `easel-params--same' treats as equal in non-obvious ways.")

(defun easel-bench-test--rows ()
  "Rows mixing numbers, floats, dates and strings in fields a and b."
  (let ((vs easel-bench-test--values))
    (vconcat (cl-loop for i below 36
                      collect (list :a (aref vs (% i (length vs))) :b (aref vs (% (* i 7) (length vs)))
                                    :_easel_row i)))))

(defun easel-bench-test--stores ()
  "Point stores over several field sets, and an interval store."
  (append
   (cl-loop for v across easel-bench-test--values
            collect (list :type "point" :fields ["a"] :values (vector (vector v))))
   (list (list :type "point" :fields ["a" "b"] :values [[1 "2026-01-02"] [2.5 :null]])
         (list :type "point" :fields ["_easel_row"] :values [[3] [41] [3]])
         (list :type "interval" :fields '(:x "a") :x [0 2]))))

(ert-deftest easel-bench-index-filter-equals-row-test ()
  (let ((rows (easel-bench-test--rows)))
    (dolist (store (easel-bench-test--stores))
      (let ((indexed (easel-params-index-filter store rows nil))
            (scanned (vconcat (seq-filter (lambda (r) (easel-params-contains store r)) rows))))
        (if (equal (plist-get store :type) "interval")
            (should-not indexed)
          (should (equal indexed scanned)))))
    (should (equal (easel-params-index-filter nil rows t) rows))
    (should (equal (easel-params-index-filter nil rows nil) []))))

(ert-deftest easel-bench-index-falls-back-past-exact-floats ()
  (let ((rows (vector (list :a (1+ (expt 2 60))) (list :a (expt 2 60)))))
    (should-not (easel-params-index-filter '(:type "point" :fields ["a"] :values [[1]]) rows nil))
    (should (eq (easel-params-index-changed rows '(("p" . nil)) nil
                                            '(:params (:p (:type "point" :fields ["a"] :values [[1]]))))
                'all))))

(ert-deftest easel-bench-index-changed-covers-every-membership-change ()
  (let ((rows (easel-bench-test--rows)) (stores (cons nil (butlast (easel-bench-test--stores)))))
    (dolist (old stores)
      (dolist (new stores)
        (dolist (empty '(nil t))
          (let* ((so (and old (list :params (list :p old)))) (sn (and new (list :params (list :p new))))
                 (changed (easel-params-index-changed rows (list (cons "p" empty)) so sn)))
            (dotimes (i (length rows))
              (unless (eq (not (easel-params-test so "p" (aref rows i) empty))
                          (not (easel-params-test sn "p" (aref rows i) empty)))
                (should (or (eq changed 'all) (memq i changed)))))))))))

;;; Hover patches equal full compiles at ladder scale

(defun easel-bench-test--zoomable (spec)
  "SPEC with an x zoom (an interval bound to scales) at the top."
  (let ((spec (copy-sequence spec)))
    (plist-put spec :params (vconcat [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")]
                                     (plist-get spec :params)))))

(ert-deftest easel-bench-hover-patches-equal-full-compiles ()
  (let* ((rows (easel-bench-rows 400))
         (dated (vconcat (seq-map (lambda (r) (list :t (easel-time-iso (* 86400000 (plist-get r :t)))
                                                    :p (plist-get r :p)))
                                  rows)))
         (cond-spec (list :data (list :values rows)
                          :params [(:name "h" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
                          :mark "point"
                          :encoding '(:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
                                      :size (:condition (:param "h" :empty :false :value 80) :value 10))))
         (date-spec (easel-bench-line-spec dated)))
    (setf (plist-get (plist-get (aref (plist-get date-spec :layer) 0) :encoding) :x)
          '(:field "t" :type "temporal"))
    (setf (plist-get (plist-get (aref (plist-get date-spec :layer) 1) :encoding) :x)
          '(:field "t" :type "temporal"))
    (dolist (spec (mapcar #'easel-bench-test--zoomable (list (easel-bench-line-spec rows) cond-spec date-spec)))
      (let* ((easel-views (make-hash-table :test 'equal))
             (v (easel-view-open spec :id "b" :size '(400 . 200))))
        (easel-replay v (vconcat (cl-loop for x from 50 to 380 by 47
                                          collect (list :type "pointermove" :px (vector x 100)))
                                 '((:type "key" :key "+") (:type "pointermove" :px [200 100])
                                   (:type "pointerleave"))))
        (dolist (x '(90 91 300))
          (easel-dispatch v (list :type "pointermove" :px (vector x 100)))
          (should (equal (easel-scene-to-json (easel-view-scene v))
                         (easel-scene-to-json
                          (easel-params-with-state (easel-view-state v)
                            (easel-compile (easel-view-spec v) :size '(400 . 200)
                                           :state (easel-view-state v)))))))
        ;; The crosshair param holds the last move, and "+" zoomed.
        (should (seq-some (lambda (p) (and (not (equal (plist-get p :name) "zoom"))
                                           (plist-get (plist-get (easel-view-state v) :params)
                                                      (easel-key (plist-get p :name)))))
                          (easel-params-of (easel-view-scene v))))
        (should (plist-get (easel-view-state v) :domains))))))

(ert-deftest easel-bench-crosshair-rule-follows-hover ()
  (let* ((easel-views (make-hash-table :test 'equal))
         (v (easel-view-open (easel-bench-line-spec (easel-bench-rows 2000)) :id "c" :size '(800 . 400))))
    (dolist (x '(100 400 700))
      (easel-dispatch v (list :type "pointermove" :px (vector x 200)))
      (let* ((scene (easel-view-scene v))
             (rule (aref (plist-get (aref (plist-get scene :views) 0) :marks) 1))
             (hover (plist-get (easel-inspect v) :hover)))
        (should (= (length (plist-get rule :items)) 1))
        (should (equal (plist-get (aref (plist-get rule :rows) 0) :t)
                       (plist-get (plist-get hover :row) :t)))))))

;;; Grid hit-test equals the linear scan

(ert-deftest easel-bench-grid-hit-equals-scan ()
  (let* ((rows (vconcat (cl-loop for i below 800
                                 collect (list :t (% (* i 7919) 300) :p (% (* i 104729) 97)))))
         (scene (easel-compile (easel-bench-points-spec rows) :size '(400 . 300)))
         (mark (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0)))
    (should (equal (plist-get (plist-get mark :index) :kind) "grid"))
    (dotimes (k 100)
      (let ((px (- (* (% (* k 7) 47) 10.0) 30)) (py (- (* (% (* k 13) 37) 10.0) 20)))
        (dolist (x-only '(nil t))
          (should (equal (easel-hit-mark mark px py x-only) (easel-hit--scan mark px py x-only))))))))

;;; Visible summary cache

(ert-deftest easel-bench-visible-summary-follows-zoom ()
  (let* ((easel-views (make-hash-table :test 'equal))
         (v (easel-view-open (easel-bench-test--zoomable (easel-bench-line-spec (easel-bench-rows 500)))
                             :id "s" :size '(800 . 400)))
         (n (lambda () (plist-get (plist-get (aref (plist-get (easel-inspect v) :views) 0) :visible) :n))))
    (should (= (funcall n) 500))
    (easel-dispatch v '(:type "pointermove" :px [300 200]))
    (should (= (funcall n) 500))
    (easel-dispatch v '(:type "key" :key "+"))
    (let ((zoomed (funcall n)))
      (should (< zoomed 500))
      (should (equal (easel-view--visible-summary (aref (plist-get (easel-view-scene v) :views) 0))
                     (let ((sv (aref (plist-get (easel-view-scene v) :views) 0)))
                       (easel-view--summarize (plist-get (plist-get sv :scales) :x)
                                              (plist-get (plist-get sv :scales) :y)
                                              (aref (plist-get sv :marks) 0))))))
    (easel-dispatch v '(:type "key" :key "0"))
    (should (= (funcall n) 500))))

;;; GC deferral

(ert-deftest easel-bench-gc-defers-until-idle-and-restores ()
  (let ((gc-cons-threshold 800000) (noninteractive nil)
        (easel-gc-cons-threshold (* 64 1024 1024)) (easel-gc--saved nil) (easel-gc--timer nil))
    (unwind-protect
        (progn
          (easel-gc-defer)
          (should (= gc-cons-threshold (* 64 1024 1024)))
          (should (timerp easel-gc--timer))
          (let ((timer easel-gc--timer))
            (easel-gc-defer)
            (should (eq easel-gc--timer timer)))
          (easel-gc-collect)
          (should (= gc-cons-threshold 800000))
          (should-not easel-gc--timer)
          ;; Someone else changed it meanwhile: leave theirs.
          (easel-gc-defer)
          (setq gc-cons-threshold 123456789)
          (easel-gc-collect)
          (should (= gc-cons-threshold 123456789))
          ;; A larger user value is never lowered; nil leaves GC alone.
          (easel-gc-defer)
          (should (= gc-cons-threshold 123456789))
          (should-not easel-gc--saved)
          (let ((easel-gc-cons-threshold nil) (gc-cons-threshold 800000))
            (easel-gc-defer)
            (should (= gc-cons-threshold 800000))))
      (when (timerp easel-gc--timer) (cancel-timer easel-gc--timer)))))

(ert-deftest easel-bench-gc-leaves-batch-alone ()
  (let ((gc-cons-threshold 800000) (noninteractive t) (easel-gc--saved nil) (easel-gc--timer nil))
    (easel-gc-defer)
    (should (= gc-cons-threshold 800000))
    (should-not easel-gc--timer)))

;;; The ladder, the budget and the verb

(defconst easel-bench-test--result
  '(:contract "easel-bench/v1" :compiled t :calibration-ms 20.0
    :ladder [(:points 1000 :hover (:mean 1.0) :compile-svg (:mean 10.0))
             (:points 10000 :hover (:mean 30.0) :compile-svg (:mean 100.0))])
  "A synthetic ladder result.")

(defconst easel-bench-test--budget
  '(:compiled t :calibration-ms 10.0 :tolerance 2.0 :floor-ms 1
    :targets [(:points 10000 :stage "hover" :ms 50) (:points 100000 :stage "hover" :ms 50)]
    :reference [(:points 1000 :stages (:hover 0.2 :compile-svg 2.0))
                (:points 10000 :stages (:hover 10.0 :compile-svg 30.0))])
  "A budget for it: the result's machine is twice as slow.")

(ert-deftest easel-bench-check-scales-limits-by-calibration ()
  (let ((v (easel-bench-check easel-bench-test--result easel-bench-test--budget)))
    (should (equal (plist-get v :status) "fail"))
    (should (= (plist-get v :factor) 2.0))
    (should (= (plist-get v :checked) 4))
    ;; 1k compile 10 > 2x2x2 = 8; 10k compile 100 > 120? no; hover 30 < 40; 1k hover 1.0 < floor 2.
    (should (equal (mapcar (lambda (x) (list (plist-get x :points) (plist-get x :stage) (plist-get x :limit)))
                           (plist-get v :violations))
                   '((1000 "compile-svg" 8.0))))
    (should (equal (mapcar (lambda (x) (plist-get x :met)) (plist-get v :targets)) '(t :null)))))

(ert-deftest easel-bench-check-skips-unlike-builds-and-passes-its-own-references ()
  (should (equal (plist-get (easel-bench-check (plist-put (copy-sequence easel-bench-test--result) :compiled :false)
                                               easel-bench-test--budget)
                            :status)
                 "skipped"))
  (let ((own (easel-bench-budget-from easel-bench-test--result easel-bench-test--budget)))
    (should (equal (plist-get own :tolerance) 2.0))
    (should (equal (plist-get (easel-bench-check easel-bench-test--result own) :status) "pass"))
    (should (equal (easel-json-parse (easel-json-encode own)) (easel-json-parse (easel-json-encode own))))))

(ert-deftest easel-bench-ladder-measures-every-stage ()
  (let* ((result (easel-bench-ladder :points '(100) :reps 1))
         (rung (aref (plist-get result :ladder) 0)))
    (should (equal (plist-get result :contract) "easel-bench/v1"))
    (should (memq (plist-get result :compiled) '(t :false)))
    (should (> (plist-get result :calibration-ms) 0))
    (should (= (plist-get rung :points) 100))
    (dolist (s easel-bench-stages)
      (let ((timing (plist-get rung (intern (concat ":" s)))))
        (should (numberp (plist-get timing :mean)))
        (should (>= (plist-get timing :max) (plist-get timing :mean)))))
    (should (easel-json-encode result))))

(ert-deftest easel-bench-verb-runs-the-ladder-and-fails-over-budget ()
  (let ((file (make-temp-file "budget" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (easel-json-encode
                     (list :compiled (if (easel-bench-compiled-p) t :false) :calibration-ms 1000000
                           :tolerance 1 :floor-ms 0 :targets []
                           :reference [(:points 100 :stages (:compile-svg 0.000001))]))))
          (let ((env (easel-agent "bench" :points "100" :n 1 :budget-file file)))
            (should (eq (plist-get env :ok) :false))
            (should (equal (plist-get env :reason) "BUDGET_EXCEEDED"))
            (should (equal (plist-get (plist-get env :evidence) :field) "compile-svg"))
            (should (equal (plist-get (plist-get (plist-get env :data) :budget) :status) "fail"))
            (should (= (plist-get (aref (plist-get (plist-get env :data) :ladder) 0) :points) 100))
            (should (member "make bench" (append (plist-get env :next) nil)))))
      (delete-file file)))
  (dolist (bad '((:points "0") (:points "1k") ("line" :points "300") (:gc "never")))
    (let ((env (apply #'easel-agent "bench" bad)))
      (should (equal (plist-get env :reason) "INVALID_INPUT")))))

(ert-deftest easel-bench-shipped-budget-covers-the-ladder ()
  (let ((budget (easel-bench-read-budget)))
    (should (equal (plist-get budget :contract) "easel-bench-budget/v1"))
    (should (> (plist-get budget :calibration-ms) 0))
    (dolist (n easel-bench-points)
      (let ((ref (easel-bench--reference budget n)))
        (dolist (s easel-bench-stages)
          (should (numberp (plist-get ref (intern (concat ":" s))))))))))

(provide 'easel-bench-test)
;;; easel-bench-test.el ends here
