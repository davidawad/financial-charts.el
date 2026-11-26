;;; financial-chart-evaluate-test.el --- evidence/v1: forward returns against chance -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty.9.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defmacro financial-chart-evaluate-test--with-detector (fn &rest body)
  "Run BODY with test.fires registered as FN, the registry restored after."
  (declare (indent 1))
  `(let ((financial-chart-detector-registry (copy-sequence financial-chart-detector-registry)))
     (financial-chart-register-detector
      'test.fires ,fn :kind 'pattern :direction 'both :params nil :lag 0 :lookback 0 :needs '(open high low close)
      :definition "A test detector (2026)." :invariance '(scale shift) :example "examples/indicators/patterns.json")
     ,@body))

(defun financial-chart-evaluate-test--bars (n)
  "N flat bars at 100 with open 100."
  (cl-loop for i below n collect (list :open 100.0 :high 101.0 :low 99.0 :close 100.0)))

(ert-deftest financial-chart-evaluate-enters-after-confirmation ()
  ;; One bullish event at bar 10, confirmed at 12: entry at the open of 13,
  ;; exit at the open of 13 + 5.  Opens jump at 13 (so an entry at 12 would
  ;; gain) and again at 18.
  (let ((bars (vconcat (financial-chart-evaluate-test--bars 100))))
    (setf (aref bars 12) (list :open 50.0 :high 101.0 :low 49.0 :close 100.0))
    (setf (aref bars 18) (list :open 110.0 :high 111.0 :low 99.0 :close 100.0))
    (financial-chart-evaluate-test--with-detector
        (lambda (_v _p) (list (list :index 10 :confirmed 12 :direction 'bullish :strength 1
                                    :geometry '(:at 10 :y 100) :evidence nil)))
      (let* ((r (car (financial-chart-evaluate '("test.fires") (list (cons "T" bars))
                                               :horizons '(5) :costs-bps 0 :draws 50 :in-sample-end 0.05)))
             (f (plist-get (plist-get r :forward) :5)))
        (should (equal (plist-get r :evidence/v1) "test.fires"))
        (should (= (plist-get f :n) 1))
        (should (< (abs (- (plist-get f :mean) 0.1)) 1e-9))
        (should (equal (plist-get r :verdict) "untested"))
        (should (equal (plist-get r :universe) ["T"]))
        (should (equal (plist-get r :bars_ids) (vector (financial-chart-bars-id bars))))))))

(ert-deftest financial-chart-evaluate-costs-shorts-and-the-baseline ()
  (let ((bars (vconcat (financial-chart-evaluate-test--bars 400))))
    (financial-chart-evaluate-test--with-detector
        (lambda (_v _p) (cl-loop for i from 10 below 370 by 2
                                 collect (list :index i :confirmed i :direction 'bearish :strength 1
                                               :geometry (list :at i :y 100) :evidence nil)))
      (let* ((r (car (financial-chart-evaluate '("test.fires") (list (cons "T" bars))
                                               :horizons '(1 5 20) :costs-bps 10 :draws 100)))
             (f (plist-get (plist-get r :forward) :5)))
        ;; Flat prices: every trade loses its costs, and so does chance.
        (should (< (abs (- (plist-get f :mean) -0.002)) 1e-12))
        (should (< (abs (- (plist-get f :baseline_mean) -0.002)) 1e-12))
        (should (equal (plist-get r :verdict) "noise"))
        (should (equal (sort (mapcar (lambda (k) (substring (symbol-name k) 1))
                                     (cl-loop for (k _) on (plist-get r :forward) by #'cddr collect k))
                             #'string<)
                       '("1" "20" "5")))))))

(ert-deftest financial-chart-evaluate-records-and-verdicts ()
  (let* ((bars (vconcat (seq-take (eas-json-read-file (expand-file-name "test/fixtures/detectors/oracle-bars.json"
                                                                        financial-chart-test-root))
                                  1200)))
         (universe (list (cons "A" (seq-subseq bars 0 600)) (cons "B" (seq-subseq bars 600 1200))))
         (dir (make-temp-file "fc-evidence" t)))
    (unwind-protect
        (let* ((records (financial-chart-evaluate '("pattern.bullish-marubozu" "swing.pivot") universe
                                                  :draws 100 :directory dir))
               (again (financial-chart-evaluate '("pattern.bullish-marubozu" "swing.pivot") universe :draws 100)))
          (should (= (length (directory-files dir nil "\\.json\\'")) 2))
          ;; Same inputs and seed: the same numbers.
          (should (equal (mapcar (lambda (r) (plist-get r :reality_check_p)) records)
                         (mapcar (lambda (r) (plist-get r :reality_check_p)) again)))
          (should (cl-every (lambda (r) (member (plist-get r :verdict) '("edge" "noise" "untested"))) records))
          (should (cl-every (lambda (r) (<= 0 (plist-get r :reality_check_p) 1)) records))
          (let ((v (financial-chart-evidence-verdict "swing.pivot" dir)))
            (should (equal (plist-get v :status) (plist-get (cadr records) :verdict)))
            (should (eq (plist-get v :stale) :false)))
          (should-not (financial-chart-evidence-verdict "pattern.hammer" dir))
          (let ((financial-chart-evidence-directory dir))
            (should (equal (plist-get (plist-get (seq-find (lambda (d) (equal (plist-get d :name) "swing.pivot"))
                                                           (financial-chart-detectors-describe))
                                                 :verdict)
                                      :status)
                           (plist-get (cadr records) :verdict)))
            (should (memq (plist-get (car (last (financial-chart-markups-doctor-checks))) :status) '(pass fail)))))
      (delete-directory dir t))))

;;; Hand-computed cases (fc-gty.19)

(ert-deftest financial-chart-evaluate-adjusts-for-splits-and-dividends ()
  ;; A 2-for-1 split on day 3: earlier prices halve, earlier volume doubles.
  (let ((bars '((:time "2026-01-01" :open 100 :high 104 :low 98 :close 102 :volume 10)
                (:time "2026-01-02" :open 102 :high 103 :low 100 :close 101 :volume 20)
                (:time "2026-01-03" :open 51 :high 52 :low 50 :close 51 :volume 40))))
    (should (equal (financial-chart-adjust-bars bars '((:time "2026-01-03" :split 2)))
                   '((:time "2026-01-01" :open 50.0 :high 52.0 :low 49.0 :close 51.0 :volume 20.0)
                     (:time "2026-01-02" :open 51.0 :high 51.5 :low 50.0 :close 50.5 :volume 40.0)
                     (:time "2026-01-03" :open 51 :high 52 :low 50 :close 51 :volume 40))))
    ;; The input is not modified.
    (should (= (plist-get (car bars) :close) 102)))
  ;; A 2.00 dividend, ex on day 2 after a 100.00 close: earlier prices x 0.98.
  (let ((adjusted (financial-chart-adjust-bars
                   '((:time 1000 :open 100 :high 100 :low 100 :close 100)
                     (:time 2000 :open 98 :high 98 :low 98 :close 98))
                   '((:time 2000 :dividend 2.0)))))
    (should (< (abs (- (plist-get (car adjusted) :close) 98.0)) 1e-12))
    (should (= (plist-get (cadr adjusted) :close) 98)))
  ;; Both, newest applied first: split on day 3 then the day-2 dividend on adjusted closes.
  (let ((adjusted (financial-chart-adjust-bars
                   '((:time 1 :open 100 :high 100 :low 100 :close 100)
                     (:time 2 :open 100 :high 100 :low 100 :close 100)
                     (:time 3 :open 50 :high 50 :low 50 :close 50))
                   '((:time 2 :dividend 1.0) (:time 3 :split 2)))))
    ;; Split: 100 -> 50 for bars 1 and 2; dividend 1.0 on the adjusted 50: factor 0.98.
    (should (< (abs (- (plist-get (car adjusted) :close) 49.0)) 1e-12))
    (should (< (abs (- (plist-get (cadr adjusted) :close) 50.0)) 1e-12)))
  (should-error (financial-chart-adjust-bars '((:time 1 :open 1 :high 1 :low 1 :close 1)) '((:time "x" :split 2)))
                :type 'financial-chart-invalid-detection))

(ert-deftest financial-chart-evaluate-stationary-bootstrap ()
  ;; Mean block 1: every index restarts (iid); a huge mean block never
  ;; restarts after the first: consecutive indices, wrapping.
  (let* ((rng (financial-chart-evaluate--rng 7))
         (long (car (financial-chart-stationary-bootstrap-indices 5 1e9 1 rng))))
    (should (= (length long) 5))
    (should (equal long (mapcar (lambda (k) (% (+ (car long) k) 5)) '(0 1 2 3 4)))))
  (let ((draws (financial-chart-stationary-bootstrap-indices 4 1 200 (financial-chart-evaluate--rng 3))))
    (should (= (length draws) 200))
    (should (cl-every (lambda (d) (cl-every (lambda (i) (<= 0 i 3)) d)) draws))
    ;; iid draws do repeat indices.
    (should (cl-some (lambda (d) (/= (length (delete-dups (copy-sequence d))) 4)) draws))))

(ert-deftest financial-chart-evaluate-reality-check-and-spa-by-hand ()
  ;; One model, f = (-1 1): mean 0.  Draws (0 0), (1 1), (0 1): means -1, 1, 0.
  ;; RC: V = 0; V* = sqrt2 (-1, 1, 0) >= 0 twice: p = 2/3.
  ;; SPA: w = sd of sqrt2 (-1, 1, 0) = sqrt(4/3); T = 0; g(0) = 0;
  ;; T* = max(0, sqrt2 (-1, 1, 0) / w) = (0, 1.2247, 0), all >= 0: p = 1.
  (let ((draws '((0 0) (1 1) (0 1))))
    (let ((rc (financial-chart-reality-check (list [-1.0 1.0]) draws))
          (spa (financial-chart-spa (list [-1.0 1.0]) draws)))
      (should (< (abs (- (plist-get rc :p) (/ 2 3.0))) 1e-12))
      (should (= (plist-get rc :stat) 0.0))
      (should (= (plist-get spa :p) 1.0))
      (should (= (plist-get spa :stat) 0.0)))
    ;; f = (1 3): mean 2, V = 2 sqrt2 = 2.83; V* = sqrt2 (-1, 1, 0): none reach it, p = 0.
    ;; SPA: w = sqrt(4/3), T = 2 sqrt2 / w = 2.449; recentred T* = (0, 1.22, 0): p = 0.
    (let ((rc (financial-chart-reality-check (list [1.0 3.0]) draws))
          (spa (financial-chart-spa (list [1.0 3.0]) draws)))
      (should (= (plist-get rc :p) 0.0))
      (should (< (abs (- (plist-get rc :stat) (* 2 (sqrt 2)))) 1e-12))
      (should (= (plist-get spa :p) 0.0))
      (should (< (abs (- (plist-get spa :stat) (/ (* 2 (sqrt 2)) (sqrt (/ 4 3.0))))) 1e-9)))
    ;; Two models: the max over them.  f1 = (1 3), f2 = (-1 1): V = 2 sqrt2 from f1;
    ;; V* per draw = max(sqrt2 (-1,1,0), sqrt2 (-1,1,0)) never reaches it: p = 0;
    ;; model 2's own p: share of V* >= 0 = 2/3.
    (let ((rc (financial-chart-reality-check (list [1.0 3.0] [-1.0 1.0]) draws)))
      (should (= (plist-get rc :p) 0.0))
      (should (equal (mapcar (lambda (p) (/ (fround (* p 1000)) 1000)) (plist-get rc :model-p))
                     (list 0.0 (/ (fround (* (/ 2 3.0) 1000)) 1000)))))))

(ert-deftest financial-chart-evaluate-records-carry-the-tests ()
  (let* ((bars (vconcat (seq-take (eas-json-read-file (expand-file-name "test/fixtures/detectors/oracle-bars.json"
                                                                        financial-chart-test-root))
                                  800)))
         (records (financial-chart-evaluate '("pattern.bullish-marubozu" "swing.pivot")
                                            (list (cons "A" bars)) :draws 100
                                            :actions '(("A" (:time 400 :split 2))))))
    (dolist (r records)
      (should (<= 0 (plist-get r :spa_p) 1))
      (should (<= 0 (plist-get r :reality_check_p) 1))
      (should (<= 0 (plist-get (plist-get r :family_tests) :spa_p) 1))
      (should (equal (plist-get r :adjustment) "back-adjusted for 1 corporate actions"))))
  ;; The period series: one bullish trade, entry at bar 2, 2 bars, no costs.
  (let ((bars (vector '(:open 100) '(:open 100) '(:open 100) '(:open 110) '(:open 121) '(:open 121))))
    (should (equal (financial-chart-evaluate--period-series
                    (list '(:direction "bullish" :confirmed_index 1)) bars 0 2 0.0)
                   [0.0 0.0 0.1 0.1 0.0]))))

(provide 'financial-chart-evaluate-test)
;;; financial-chart-evaluate-test.el ends here
