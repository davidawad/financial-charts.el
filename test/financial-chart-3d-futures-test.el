;;; financial-chart-3d-futures-test.el --- 3D futures term structures -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'financial-chart-3d-futures)
(require 'graphics-eas)
(financial-chart-3d--ensure)

(defun financial-chart-3d-futures-test--goldens ()
  (eas-json-read-file (expand-file-name "test/fixtures/invest-metrics/fut_term_structure.json"
                                        financial-chart-test-root)))

(ert-deftest financial-chart-3d-futures-metric-golden-cases-adapt ()
  (dolist (case (append (plist-get (financial-chart-3d-futures-test--goldens) :cases) nil))
    (let* ((output (plist-get case :output))
           (rows (append (financial-chart-3d-futures-rows output) nil))
           (contracts (seq-filter (lambda (r) (equal (plist-get r :kind) "contract")) rows)))
      (ert-info ((plist-get case :name))
        (should (= (length contracts) (length (plist-get output :contracts))))
        (should (equal (plist-get (car contracts) :shape) (plist-get (plist-get case :expect) :shape)))
        (should (numberp (plist-get (car contracts) :years)))
        (should (numberp (plist-get (car contracts) :price)))))))

(ert-deftest financial-chart-3d-futures-three-snapshots-replay-contract-by-date ()
  (let* ((series (vector
                  '(:time "2026-10-30T10:00:00" :output (:as_of "2026-10-30" :spot 100 :shape "contango" :contracts [(:symbol "F1" :years 0.1 :price 101) (:symbol "F2" :years 0.3 :price 104)]))
                  '(:time "2026-10-30T12:00:00" :output (:as_of "2026-10-30" :spot 100.1 :shape "contango" :contracts [(:symbol "F1" :years 0.1 :price 101.1) (:symbol "F2" :years 0.3 :price 104.2)]))
                  '(:time "2026-10-30T15:30:00" :output (:as_of "2026-10-30" :spot 100.2 :shape "backwardation" :contracts [(:symbol "F1" :years 0.1 :price 100.8) (:symbol "F2" :years 0.3 :price 99.5)]))))
         (rows (append (financial-chart-3d-futures-rows series) nil)))
    (should (= (length (seq-filter (lambda (r) (equal (plist-get r :kind) "contract")) rows)) 6))
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :time)) rows))
                   '("2026-10-30T10:00:00" "2026-10-30T12:00:00" "2026-10-30T15:30:00")))
    (should (< (plist-get (nth 2 (seq-filter (lambda (r) (equal (plist-get r :kind) "contract")) rows)) :spread_to_next) 0))))

(ert-deftest financial-chart-3d-futures-mark-tooltips-name-contracts ()
  (let* ((rows (financial-chart-3d-futures-rows
                '(:as_of "2026-10-30" :spot 100 :contracts
                         [(:symbol "/ESH7" :expiry "2027-03-19" :years 0.38 :price 105
                                   :bid 104.9 :ask 105.1 :mark 105 :open_interest 99 :volume 20)])))
         (prims (financial-chart-3d-futures--mark
                 '(:encoding (:x (:field "years") :y (:field "price") :z (:field "time")
                              :color (:field "roll_sign"))) rows))
         (points (seq-find (lambda (p) (equal (plist-get p :id) "contracts")) prims)))
    (should (string-match-p "/ESH7" (aref (plist-get points :tooltips) 0)))
    (should (string-match-p "OI 99" (aref (plist-get points :tooltips) 0)))
    (should (equal (plist-get (aref (plist-get points :data) 0) :symbol) "/ESH7"))))

(ert-deftest financial-chart-3d-futures-metric-template-pair-is-registered ()
  (should (member "futures-curve" (cdr (assoc "fut.term_structure" (financial-chart-3d-metric-templates)))))
  (should (member "futures-curve" (financial-chart-3d-variants)))
  (should (eas-template-get "futures-curve")))

(provide 'financial-chart-3d-futures-test)
;;; financial-chart-3d-futures-test.el ends here
