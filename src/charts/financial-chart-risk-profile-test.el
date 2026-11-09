;;; financial-chart-risk-profile-test.el --- Tests for the risk-profile chart -*- lexical-binding: t; -*-

;;; Commentary:

;; Reference values.
;;  - Hull, Options, Futures and Other Derivatives (8th ed.), Example 15.6:
;;    S=42 K=40 r=10% sigma=20% T=0.5 gives call 4.76, put 0.81.
;;  - Every other Black-Scholes number below was computed with an
;;    independent implementation (Python 3, math.erf), not this package:
;;      call(100,100,r=.05,q=.02,v=.3,T=1) = 13.020281268727338
;;      put (100,100,r=.05,q=.02,v=.3,T=1) = 10.123356388123227
;;      put(S,165,r=.04,q=0,v=.71,T=4/365): S=178.88 0.888535549952497,
;;        S=165 4.854288062634993, S=154.25 11.866652938149656
;;      put(S,165,...,T=2/365): S=165 -> P/L -240.07331470331144 for the
;;        short NVDA position, S=178.88 -> 79.92306636748063
;; The NVDA position is the screenshot case: short 1x 11/13/2026 165P at
;; 1.04, spot 178.88, valued 2026-11-09, IV 0.71, rate 4%.

;;; Code:

(require 'ert)
(require 'json)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defconst financial-chart-risk-profile-test--nvda
  '(:spot 178.88 :date "2026-11-09" :rate 0.04 :range (154.25 200.75)
    :legs ((:type put :side short :qty 1 :strike 165 :expiry "2026-11-13"
            :price 1.04 :iv 0.71))))

(defconst financial-chart-risk-profile-test--straddle
  '(:spot 100 :date "2026-11-02" :range (80 120)
    :legs ((:type call :side long :qty 1 :strike 100 :expiry "2026-12-18" :price 3.0 :iv 0.3)
           (:type put :side long :qty 1 :strike 100 :expiry "2026-12-18" :price 2.0 :iv 0.3))))

(defconst financial-chart-risk-profile-test--call-spread
  '(:spot 100 :date "2026-11-02" :range (80 120)
    :legs ((:type call :side long :qty 1 :strike 100 :expiry "2026-12-18" :price 4.0 :iv 0.3)
           (:type call :side short :qty 1 :strike 110 :expiry "2026-12-18" :price 1.5 :iv 0.3))))

(defun financial-chart-risk-profile-test--close (a b &optional tol)
  "Non-nil when A and B differ by at most TOL (default 1e-9)."
  (<= (abs (- a b)) (or tol 1e-9)))

;;; Black-Scholes

(ert-deftest financial-chart-risk-profile-bs-matches-hull ()
  (should (financial-chart-risk-profile-test--close
           4.76 (financial-chart-risk-profile-bs-price 'call 42 40 0.10 0.0 0.20 0.5) 0.005))
  (should (financial-chart-risk-profile-test--close
           0.81 (financial-chart-risk-profile-bs-price 'put 42 40 0.10 0.0 0.20 0.5) 0.005)))

(ert-deftest financial-chart-risk-profile-bs-matches-independent-reference ()
  (should (financial-chart-risk-profile-test--close
           13.020281268727338
           (financial-chart-risk-profile-bs-price 'call 100 100 0.05 0.02 0.3 1.0) 1e-8))
  (should (financial-chart-risk-profile-test--close
           10.123356388123227
           (financial-chart-risk-profile-bs-price 'put 100 100 0.05 0.02 0.3 1.0) 1e-8))
  (dolist (case '((178.88 0.888535549952497) (165 4.854288062634993) (154.25 11.866652938149656)))
    (should (financial-chart-risk-profile-test--close
             (cadr case)
             (financial-chart-risk-profile-bs-price 'put (car case) 165 0.04 0.0 0.71 (/ 4.0 365))
             1e-8))))

(ert-deftest financial-chart-risk-profile-bs-put-call-parity-and-expiry ()
  (let* ((s 100) (k 95) (r 0.05) (q 0.02) (v 0.3) (tt 0.75)
         (c (financial-chart-risk-profile-bs-price 'call s k r q v tt))
         (p (financial-chart-risk-profile-bs-price 'put s k r q v tt)))
    ;; C - P = S e^{-qT} - K e^{-rT}
    (should (financial-chart-risk-profile-test--close
             (- c p) (- (* s (exp (- (* q tt)))) (* k (exp (- (* r tt))))) 1e-9)))
  (should (= 5.0 (financial-chart-risk-profile-bs-price 'put 95 100 0.05 0.0 0.3 0.0)))
  (should (= 0.0 (financial-chart-risk-profile-bs-price 'call 95 100 0.05 0.0 0.3 0.0))))

(ert-deftest financial-chart-risk-profile-implied-vol-round-trips ()
  (let* ((price (financial-chart-risk-profile-bs-price 'put 178.88 165 0.04 0.0 0.71 (/ 4.0 365)))
         (iv (financial-chart-risk-profile-implied-vol 'put price 178.88 165 0.04 0.0 (/ 4.0 365))))
    (should (financial-chart-risk-profile-test--close 0.71 iv 1e-6)))
  (should-not (financial-chart-risk-profile-implied-vol 'put 500 178.88 165 0.04 0.0 0.01)))

;;; Payoff at expiry (the screenshot position)

(ert-deftest financial-chart-risk-profile-nvda-expiry-payoff ()
  (let ((pos financial-chart-risk-profile-test--nvda))
    ;; premium collected above the strike
    (should (= 104.0 (financial-chart-risk-profile-expiry-pnl pos 170)))
    (should (= 104.0 (financial-chart-risk-profile-expiry-pnl pos 165)))
    (should (financial-chart-risk-profile-test--close
             0.0 (financial-chart-risk-profile-expiry-pnl pos 163.96)))
    (should (financial-chart-risk-profile-test--close
             -971.0 (financial-chart-risk-profile-expiry-pnl pos 154.25)))))

(ert-deftest financial-chart-risk-profile-nvda-breakeven-and-extremes ()
  (let* ((pos financial-chart-risk-profile-test--nvda)
         (be (financial-chart-risk-profile-breakevens pos))
         (ext (financial-chart-risk-profile-extremes pos)))
    (should (= 1 (length be)))
    (should (financial-chart-risk-profile-test--close 163.96 (car be)))
    (should (= 104.0 (plist-get ext :max-profit)))
    (should (financial-chart-risk-profile-test--close -971.0 (plist-get ext :max-loss)))))

(ert-deftest financial-chart-risk-profile-nvda-loss-slope-is-100-per-dollar ()
  (let ((pos financial-chart-risk-profile-test--nvda))
    ;; below the strike a short put loses 100 shares' worth per $1; above it, nothing
    (should (financial-chart-risk-profile-test--close
             100.0 (/ (- (financial-chart-risk-profile-expiry-pnl pos 160)
                         (financial-chart-risk-profile-expiry-pnl pos 150)) 10.0)))
    (should (financial-chart-risk-profile-test--close
             0.0 (- (financial-chart-risk-profile-expiry-pnl pos 190)
                    (financial-chart-risk-profile-expiry-pnl pos 170))))))

(ert-deftest financial-chart-risk-profile-multi-leg-payoffs ()
  (let ((spread financial-chart-risk-profile-test--call-spread))
    ;; debit 2.5: max loss 250 below 100, max profit 750 above 110, breakeven 102.5
    (should (financial-chart-risk-profile-test--close
             102.5 (car (financial-chart-risk-profile-breakevens spread))))
    (let ((ext (financial-chart-risk-profile-extremes spread)))
      (should (financial-chart-risk-profile-test--close 750.0 (plist-get ext :max-profit)))
      (should (financial-chart-risk-profile-test--close -250.0 (plist-get ext :max-loss)))))
  (let ((straddle financial-chart-risk-profile-test--straddle))
    (should (equal (financial-chart-risk-profile-breakevens straddle) '(95.0 105.0)))
    (should (financial-chart-risk-profile-test--close
             -500.0 (financial-chart-risk-profile-expiry-pnl straddle 100))))
  ;; stock leg: 100 shares bought at 100 covered with a call sold at 105 for 2
  (let ((covered '(:spot 100 :date "2026-11-02" :range (80 120)
                   :legs ((:type stock :side long :qty 100)
                          (:type call :side short :qty 1 :strike 105 :expiry "2026-12-18"
                           :price 2.0 :iv 0.3)))))
    (should (financial-chart-risk-profile-test--close
             700.0 (financial-chart-risk-profile-expiry-pnl covered 110)))
    (should (financial-chart-risk-profile-test--close
             -1600.0 (financial-chart-risk-profile-expiry-pnl covered 82)))
    (should (financial-chart-risk-profile-test--close
             98.0 (car (financial-chart-risk-profile-breakevens covered))))))

;;; T+0 and T+n curves

(ert-deftest financial-chart-risk-profile-t0-curve-matches-reference ()
  (let ((pos financial-chart-risk-profile-test--nvda))
    (dolist (case '((178.88 15.146445004750309) (165 -381.42880626349927)
                    (154.25 -1082.6652938149657)))
      (should (financial-chart-risk-profile-test--close
               (cadr case)
               (financial-chart-risk-profile-pnl-at pos (car case) "2026-11-09") 1e-6)))
    ;; T+2: two days left
    (should (financial-chart-risk-profile-test--close
             -240.07331470331144 (financial-chart-risk-profile-pnl-at pos 165 "2026-11-11") 1e-6))
    (should (financial-chart-risk-profile-test--close
             79.92306636748063 (financial-chart-risk-profile-pnl-at pos 178.88 "2026-11-11") 1e-6))))

(ert-deftest financial-chart-risk-profile-curve-shape-and-convergence ()
  (let* ((pos financial-chart-risk-profile-test--nvda)
         (curve (financial-chart-risk-profile-curve pos "2026-11-09" 100)))
    (should (= 101 (length curve)))
    (should (= 154.25 (car (car curve))))
    (should (financial-chart-risk-profile-test--close 200.75 (car (car (last curve))) 1e-9))
    ;; the T+0 curve lies below the at-expiry line for a short option's seller
    ;; only where time value is owed: it never exceeds the max profit
    (should (cl-every (lambda (p) (<= (cadr p) 104.0)) curve)))
  ;; on the expiry date the curve is the expiry payoff
  (let ((pos financial-chart-risk-profile-test--nvda))
    (dolist (s '(150 163.96 165 170 190))
      (should (financial-chart-risk-profile-test--close
               (financial-chart-risk-profile-expiry-pnl pos s)
               (financial-chart-risk-profile-pnl-at pos s "2026-11-13")))))
  (should (equal (financial-chart-risk-profile-curve-label
                  financial-chart-risk-profile-test--nvda "2026-11-12")
                 "T+3")))

(ert-deftest financial-chart-risk-profile-iv-derived-from-premium ()
  ;; a leg given only its premium is repriced at the IV that premium implies
  (let* ((price (financial-chart-risk-profile-bs-price 'put 178.88 165 0.04 0.0 0.71 (/ 4.0 365)))
         (pos `(:spot 178.88 :date "2026-11-09" :rate 0.04 :range (154.25 200.75)
                :legs ((:type put :side short :qty 1 :strike 165 :expiry "2026-11-13" :price ,price)))))
    ;; entered at fair value, so P/L today at spot is 0
    (should (financial-chart-risk-profile-test--close
             0.0 (financial-chart-risk-profile-pnl-at pos 178.88 "2026-11-09") 1e-5)))
  ;; a leg given only IV is entered at the Black-Scholes premium
  (let ((pos '(:spot 178.88 :date "2026-11-09" :rate 0.04
               :legs ((:type put :side long :qty 1 :strike 165 :expiry "2026-11-13" :iv 0.71)))))
    (should (financial-chart-risk-profile-test--close
             0.0 (financial-chart-risk-profile-pnl-at pos 178.88 "2026-11-09") 1e-9))))

;;; Regions

(ert-deftest financial-chart-risk-profile-regions-split-at-breakevens ()
  (let* ((pos financial-chart-risk-profile-test--straddle)
         (regions (financial-chart-risk-profile-regions pos)))
    (should (equal (mapcar (lambda (r) (plist-get r :side)) regions) '(profit loss profit)))
    ;; breakevens 95 and 105 are the shared boundary points, at P/L zero
    (should (equal (mapcar (lambda (r) (car (car (last (plist-get r :points))))) regions)
                   '(95.0 105.0 120.0)))
    (should (equal (mapcar (lambda (r) (car (car (plist-get r :points)))) regions)
                   '(80.0 95.0 105.0)))
    (dolist (r (butlast regions))
      (should (= 0.0 (cadr (car (last (plist-get r :points)))))))
    ;; the loss region keeps the strike's kink
    (should (member '(100.0 -500.0) (plist-get (cadr regions) :points)))
    ;; every point sits on the expiry line
    (dolist (r regions)
      (dolist (p (plist-get r :points))
        (should (financial-chart-risk-profile-test--close
                 (cadr p) (financial-chart-risk-profile-expiry-pnl pos (car p)) 1e-9))))))

(ert-deftest financial-chart-risk-profile-regions-one-sided-and-nvda ()
  (let ((regions (financial-chart-risk-profile-regions financial-chart-risk-profile-test--nvda)))
    (should (equal (mapcar (lambda (r) (plist-get r :side)) regions) '(loss profit)))
    (should (financial-chart-risk-profile-test--close
            163.96 (car (car (last (plist-get (car regions) :points))))))
    (should (equal (car (plist-get (cadr regions) :points))
                   (car (last (plist-get (car regions) :points))))))
  ;; a position that never loses in range is one profit region
  (let ((regions (financial-chart-risk-profile-regions
                  '(:spot 100 :date "2026-11-02" :range (110 120)
                    :legs ((:type call :side long :qty 1 :strike 100 :expiry "2026-12-18"
                            :price 1.0 :iv 0.3))))))
    (should (equal (mapcar (lambda (r) (plist-get r :side)) regions) '(profit)))))

;;; Rows and shape

(ert-deftest financial-chart-risk-profile-rows-carry-every-mark ()
  (let* ((rows (financial-chart-risk-profile-rows
                (append financial-chart-risk-profile-test--nvda (list :dates '("2026-11-11")))))
         (kinds (mapcar (lambda (r) (plist-get r :kind)) rows))
         (strike (cl-find "strike" rows :key (lambda (r) (plist-get r :kind)) :test #'equal)))
    (should (= 5 (cl-count "expiry" kinds :test #'equal)))
    (should (= 242 (cl-count "curve" kinds :test #'equal)))
    (should (= 1 (cl-count "spot" kinds :test #'equal)))
    (should (equal (plist-get strike :label) "-165P"))
    (should (= 165.0 (plist-get strike :price)))
    (should (equal (plist-get strike :ink) "#ff4d57"))
    (should (equal (delete-dups (cl-loop for r in rows
                                         when (equal (plist-get r :kind) "curve")
                                         collect (plist-get r :series)))
                   '("T+0" "T+2"))))
  (should (equal (financial-chart-risk-profile-strike-label
                  '(:type call :side long :qty 1 :strike 170)) "+170C"))
  (should (equal (financial-chart-risk-profile-strike-label
                  '(:type put :side short :qty 2 :strike 97.5)) "-2x97.5P")))

(ert-deftest financial-chart-risk-profile-checks-bad-input ()
  (let ((good financial-chart-risk-profile-test--nvda))
    (should (eq t (financial-chart-check 'risk-profile good)))
    (should (equal (plist-get (financial-chart-check 'risk-profile (plist-put (copy-sequence good) :spot -1))
                              :code) "not_positive"))
    (should (equal (plist-get (financial-chart-check 'risk-profile (plist-put (copy-sequence good) :legs nil))
                              :code) "no_legs"))
    (should (equal (plist-get (financial-chart-check 'risk-profile (plist-put (copy-sequence good) :range '(200 100)))
                              :code) "bad_range"))
    (should (equal (plist-get (financial-chart-check 'risk-profile (plist-put (copy-sequence good) :dates '("2026-11-01")))
                              :code) "date_before_valuation"))
    (let ((bad (financial-chart-check
                'risk-profile
                (plist-put (copy-sequence good) :legs
                           '((:type put :side short :qty 1 :strike 165 :expiry "2026-11-13")))))) 
      (should (equal (plist-get bad :code) "missing_field"))
      (should (equal (plist-get bad :field) "legs[0].price")))
    (should (equal (plist-get (financial-chart-check
                               'risk-profile
                               (plist-put (copy-sequence good) :legs
                                          '((:type put :side sideways :qty 1 :strike 165
                                             :expiry "2026-11-13" :price 1))))
                              :code) "invalid_side"))
    (should (equal (plist-get (financial-chart-check
                               'risk-profile
                               (plist-put (copy-sequence good) :legs
                                          '((:type put :side short :qty 1 :strike 165
                                             :expiry "11/13/2026" :price 1))))
                              :code) "invalid_date"))))

(ert-deftest financial-chart-risk-profile-recipe-example-is-the-nvda-position ()
  (let* ((shape (alist-get 'risk-profile financial-chart-shapes))
         (file (expand-file-name "examples/risk-profile.data.json" financial-chart-test-root))
         (pos (funcall (plist-get shape :from-json)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer :object-type 'alist :array-type 'array)))))
    (should (equal (car (plist-get pos :legs))
                   (car (plist-get financial-chart-risk-profile-test--nvda :legs))))
    (should (= 178.88 (plist-get pos :spot)))
    (should (equal (plist-get pos :date) "2026-11-09"))
    (should (eq t (financial-chart-validate 'risk-profile pos)))
    ;; JSON round trip of the shape's own example
    (let ((example (plist-get shape :example)))
      (should (equal example (funcall (plist-get shape :from-json)
                                      (funcall (plist-get shape :to-json) example)))))))

;;; Rendering: text, svg, kitty pixels

(ert-deftest financial-chart-risk-profile-parity-checks-the-plotted-numbers ()
  (let ((parity (financial-chart-eas-parity 'risk-profile)))
    (should (plist-get parity :pass))
    (should (> (length (plist-get parity :checks)) 2))))

(ert-deftest financial-chart-risk-profile-nvda-text-golden ()
  (financial-chart-test-golden
   "risk-profile-nvda.txt"
   (substring-no-properties
    (financial-chart-plot 'risk-profile financial-chart-risk-profile-test--nvda
                          :backend 'text :width 80 :height 20))))

(ert-deftest financial-chart-risk-profile-svg-draws-regions-lines-and-labels ()
  (let ((svg (financial-chart-plot 'risk-profile financial-chart-risk-profile-test--nvda
                                   :backend 'svg :pixel-width 800 :pixel-height 360)))
    (should (string-prefix-p "<svg" svg))
    (should (string-match-p "-165P" svg))
    (should (string-match-p "#8f1d27" svg))   ; loss fill
    (should (string-match-p "#1c7c3c" svg))   ; profit fill
    (should (string-match-p "#ff4d57" svg))   ; loss line
    (should (string-match-p "#2ecc71" svg))   ; profit line
    (should (string-match-p "#22d3ee" svg))   ; T+0 curve
    (should (string-match-p "stroke-dasharray" svg))))

(ert-deftest financial-chart-risk-profile-kitty-pixels-from-the-svg ()
  (require 'eas-output)
  (skip-unless (eas-output-rasterizer-find))
  (let* ((svg (financial-chart-plot 'risk-profile financial-chart-risk-profile-test--nvda
                                    :backend 'svg :pixel-width 400 :pixel-height 200))
         (bytes (eas-output-encode svg 400 200 'kitty 7)))
    (should (string-prefix-p "\e_Ga=t,f=100,i=7" bytes))))

(ert-deftest financial-chart-risk-profile-extra-dates-add-curves ()
  (let* ((pos (append financial-chart-risk-profile-test--nvda (list :dates '("2026-11-10" "2026-11-12"))))
         (svg (financial-chart-plot 'risk-profile pos :backend 'svg))
         (text (substring-no-properties
                (financial-chart-plot 'risk-profile pos :backend 'text :width 80 :height 20))))
    (should (equal (financial-chart-risk-profile-valuation-dates pos)
                   '("2026-11-09" "2026-11-10" "2026-11-12")))
    (should (string-match-p "#facc15" svg))
    (should (string-match-p "T\\+3" text))))

(provide 'financial-chart-risk-profile-test)
;;; financial-chart-risk-profile-test.el ends here
