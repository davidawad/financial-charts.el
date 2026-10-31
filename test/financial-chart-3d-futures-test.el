;;; financial-chart-3d-futures-test.el --- 3D futures term structures -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'financial-chart-3d-futures)
(require 'graphics-eas)
(require 'graphics-frame)
(financial-chart-3d--ensure)

(defun financial-chart-3d-futures-test--goldens ()
  (eas-json-read-file (expand-file-name "test/fixtures/invest-metrics/futures.json"
                                        financial-chart-test-root)))

(ert-deftest financial-chart-3d-futures-metric-golden-cases-adapt ()
  (let ((cases (seq-filter (lambda (case) (and (equal (plist-get case :metric) "fut.term_structure")
                                                t))
                           (append (plist-get (financial-chart-3d-futures-test--goldens) :cases) nil))))
    (should (= (length cases) 8))
    (dolist (case cases)
      (let ((input (plist-get case :input)))
        (ert-info ((plist-get case :name))
          (if (plist-get case :expect_error)
              (should-error (financial-chart-3d-futures-rows input)
                            :type 'financial-chart-invalid-data)
            (let* ((rows (append (financial-chart-3d-futures-rows input) nil))
                   (contracts (seq-filter (lambda (r) (equal (plist-get r :kind) "contract")) rows))
                   (first-contract (car contracts)))
              (should (= (length contracts) (length (plist-get input :contracts))))
              (when-let ((shape (plist-get (plist-get case :expect) :/shape)))
                (should (equal (plist-get first-contract :shape) shape)))
              (should (numberp (plist-get first-contract :years)))
              (should (numberp (plist-get first-contract :price)))
              (should (or (eq (plist-get first-contract :basis) :null)
                          (numberp (plist-get first-contract :basis))))
              (when-let ((spread (plist-get (plist-get case :expect) :/spreads/0/spread)))
                (should (= spread (plist-get first-contract :spread_to_next))))
              (when-let ((roll (plist-get (plist-get case :expect) :/spreads/0/roll_yield)))
                (should (< (abs (- roll (plist-get first-contract :annualized_roll))) 1e-9))))))))))

(ert-deftest financial-chart-3d-futures-real-metric-outputs-adapt ()
  (let* ((runs (eas-json-read-file (expand-file-name "test/fixtures/invest-metrics/runs.json"
                                                      financial-chart-test-root)))
         (cases (seq-filter (lambda (case) (and (equal (plist-get case :metric) "fut.term_structure")
                                                (plist-get case :output)))
                            (append (plist-get runs :cases) nil))))
    (should (= (length cases) 5))
    (dolist (case cases)
      (let* ((output (plist-get case :output))
             (rows (append (financial-chart-3d-futures-rows output) nil))
             (contract (seq-find (lambda (r) (equal (plist-get r :kind) "contract")) rows)))
        (should (equal (plist-get contract :shape) (plist-get output :shape)))
        (should (numberp (plist-get contract :years)))
        (should (numberp (plist-get contract :price)))
        (should (or (numberp (plist-get contract :basis)) (eq (plist-get contract :basis) :null)))
        (should (or (numberp (plist-get contract :carry)) (eq (plist-get contract :carry) :null)))))))

(ert-deftest financial-chart-3d-futures-expiry-tenor-uses-act-365 ()
  (should (= (financial-chart-3d-futures--years "2027-01-01" "2026-01-01") 1.0)))

(ert-deftest financial-chart-3d-futures-schwab-fixture-has-recorded-contract-quote-fields ()
  (let* ((feed (eas-json-read-file (expand-file-name "test/fixtures/3d/futures/curve-es.json"
                                                      financial-chart-test-root)))
         (contract (aref (plist-get feed :contracts) 0)))
    (should (equal (plist-get feed :root) "/ES"))
    (dolist (field '(:symbol :expiry :expiry_source :bid :ask :last :mark :open_interest :volume))
      (should (plist-get contract field)))))

(ert-deftest financial-chart-3d-futures-three-snapshots-replay-contract-by-date ()
  (let* ((series (vector
                  '(:time "2026-10-30T10:00:00" :output (:as_of "2026-10-30" :spot 100 :shape "contango" :contracts [(:symbol "F1" :years 0.1 :price 101) (:symbol "F2" :years 0.3 :price 104)]))
                  '(:time "2026-10-30T12:00:00" :output (:as_of "2026-10-30" :spot 100.1 :shape "contango" :contracts [(:symbol "F1" :years 0.1 :price 101.1) (:symbol "F2" :years 0.3 :price 104.2)]))
                  '(:time "2026-10-30T15:30:00" :output (:as_of "2026-10-30" :spot 100.2 :shape "backwardation" :contracts [(:symbol "F1" :years 0.1 :price 100.8) (:symbol "F2" :years 0.3 :price 99.5)]))))
         (rows (append (financial-chart-3d-futures-rows series) nil)))
    (should (= (length (seq-filter (lambda (r) (equal (plist-get r :kind) "contract")) rows)) 6))
    (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :time)) rows))
                   '("2026-10-30T10:00:00" "2026-10-30T12:00:00" "2026-10-30T15:30:00")))
    (should (< (plist-get (seq-find (lambda (r) (and (equal (plist-get r :kind) "contract")
                                                       (equal (plist-get r :time) "2026-10-30T15:30:00")
                                                       (equal (plist-get r :symbol) "F1"))) rows)
                           :spread_to_next) 0))))

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

;;; Bindings from a real single snapshot open in 3D (fc-jyr)

(defun financial-chart-3d-futures-test--es-output (&rest extra)
  "The recorded /ES fut.term_structure output (years, no expiry), plus EXTRA."
  (append extra (eas-json-read-file (expand-file-name "test/fixtures/3d/futures/term-structure-es.json"
                                                      financial-chart-test-root))))

(ert-deftest financial-chart-3d-futures-adapter-takes-its-rows-in-every-shape ()
  (let* ((rows (plist-get (financial-chart-3d-bindings-from-metric
                           "futures-curve" "fut.term_structure"
                           (financial-chart-3d-futures-test--es-output :spot 7580.07 :as_of "2026-10-31"))
                          :data))
         (contract (seq-find (lambda (r) (equal (plist-get r :kind) "contract")) rows))
         (data (eas-data-from "invest/futures-curve" rows)))
    (should (eq (plist-get contract :expiry) :null))
    (should (numberp (plist-get contract :years)))
    (dolist (shape (list rows (append rows nil) data (list :values rows)
                         (eas-json-parse (eas-json-encode rows))
                         (eas-data-make (vector)) (plist-get data :rows)))
      (ert-info ((format "%S" (type-of shape)))
        (let ((out (eas-data-from "invest/futures-curve" shape)))
          (should (eas-data-p out))
          (unless (equal shape (eas-data-make (vector)))
            (should (= (length (plist-get out :rows)) (length rows)))))))
    ;; The rows function itself is idempotent on its rows.
    (should (equal (financial-chart-3d-futures-rows rows) rows))
    (should (equal (financial-chart-3d-futures-rows (append rows nil)) rows))))

(ert-deftest financial-chart-3d-futures-single-snapshot-opens-in-3d ()
  ;; The live repro: one real output (years only), with and without the
  ;; spot and as_of it was run on, through the full 3D open.
  (dolist (output (list (financial-chart-3d-futures-test--es-output)
                        (financial-chart-3d-futures-test--es-output :spot 7580.07 :as_of "2026-10-31")))
    (ert-info ((format "spot %S" (plist-get output :spot)))
      (let* ((b (financial-chart-3d-bindings-from-metric "futures-curve" "fut.term_structure" output))
             (view (financial-chart-3d-open "futures-curve" :bindings b :id "fc3d-futures-test:single")))
        (unwind-protect
            (let ((frame (progn (financial-chart-3d-seek-end view) (graphics-eas-frame view 60 24)))
                  (lit 0))
              (dotimes (i (* 60 24))
                (when (> (car (graphics-frame-cell frame i)) 0) (cl-incf lit)))
              (should (> lit 0)))
          (financial-chart-3d--close view))))))

(ert-deftest financial-chart-3d-futures-missing-contracts-is-invalid-data ()
  (let ((err (should-error (financial-chart-3d-futures-rows '(:spot 1 :as_of "2026-10-31"))
                           :type 'financial-chart-invalid-data)))
    (should (equal (plist-get (financial-chart-error-data err) :field) "contracts"))
    (should (equal (plist-get (financial-chart-error-data err) :code) "missing_contracts"))))

(provide 'financial-chart-3d-futures-test)
;;; financial-chart-3d-futures-test.el ends here
