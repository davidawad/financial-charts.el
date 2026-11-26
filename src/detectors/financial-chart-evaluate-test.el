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

(provide 'financial-chart-evaluate-test)
;;; financial-chart-evaluate-test.el ends here
