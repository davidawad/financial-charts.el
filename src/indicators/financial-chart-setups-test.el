;;; financial-chart-setups-test.el --- tests for the setup indicators -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defvar financial-chart-setups-test--dir (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-setups-test--dir))
(require 'financial-chart-indicators)

(defun financial-chart-setups-test--bars (closes &optional spread volume)
  "Bars with CLOSES, each high and low SPREAD (1.0) away, with VOLUME (1000)."
  (mapcar (lambda (c) (list :open c :high (+ c (or spread 1.0)) :low (- c (or spread 1.0))
                            :close c :volume (or volume 1000)))
          closes))

(defun financial-chart-setups-test--wave (n)
  "N deterministic wavy bars with varying volume."
  (cl-loop for i below n
           for c = (+ 100 (* 10 (sin (* i 0.21))) (* 3 (sin (* i 1.7))))
           collect (list :open c :high (+ c 1.5 (abs (sin i))) :low (- c 1.5 (abs (cos i)))
                         :close c :volume (+ 1000 (* 100 (% (* i 7) 11))))))

(defun financial-chart-setups-test--outputs (name bars &rest params)
  "Every output of indicator NAME over BARS as a list of (NAME . VALUES)."
  (let ((r (apply #'financial-chart-indicator-evaluate name bars params)))
    (mapcar (lambda (s) (cons (plist-get s :name) (plist-get s :values)))
            (if (and (listp r) (keywordp (car r))) (list r) r))))

(defun financial-chart-setups-test--close (a b) (< (abs (- a b)) 1e-6))

;;; Wilder RSI

(ert-deftest financial-chart-setups-wilder-rsi-matches-wilders-worked-example ()
  ;; The first RSI(14) of the closes in the StockCharts/Wilder worked example is 70.53.
  (let* ((closes '(44.3389 44.0902 44.1497 43.6124 44.3278 44.8264 45.0955 45.4245
                   45.8433 46.0826 45.8931 46.0328 45.6140 46.2820 46.2820))
         (rsi (financial-chart-wilder-rsi (financial-chart-setups-test--bars closes) 14)))
    (should (cl-every #'null (cl-subseq rsi 0 14)))
    (should (< (abs (- (nth 14 rsi) 70.53)) 0.01))))

(ert-deftest financial-chart-setups-wilder-rsi-smooths-after-the-seed ()
  ;; period 2: changes +1 +1 -> gain 1, loss 0 -> 100; then -2: gain .5, loss 1 -> 33.33.
  (let ((rsi (financial-chart-wilder-rsi (financial-chart-setups-test--bars '(10 11 12 10)) 2)))
    (should (equal (cl-subseq rsi 0 2) '(nil nil)))
    (should (= (nth 2 rsi) 100.0))
    (should (financial-chart-setups-test--close (nth 3 rsi) (- 100 (/ 100 (+ 1 0.5))))))
  (should (equal (financial-chart-wilder-rsi '((:close 1) (:close nil) (:close 2) (:close 3)) 1)
                 '(nil nil nil 100.0))))

;;; Force index

(ert-deftest financial-chart-setups-force-index-is-the-ema-of-change-times-volume ()
  (let* ((bars (cl-mapcar (lambda (c v) (list :close c :volume v)) '(10 12 11 13) '(100 200 300 400)))
         (fi (financial-chart-force-index bars 2)))
    ;; raw: nil 400 -300 800; EMA(2) seeds on the mean of 400, -300.
    (should (equal (cl-subseq fi 0 2) '(nil nil)))
    (should (financial-chart-setups-test--close (nth 2 fi) 50.0))
    (should (financial-chart-setups-test--close (nth 3 fi) (+ (* (/ 2.0 3) 800) (* (/ 1.0 3) 50))))))

(ert-deftest financial-chart-setups-force-index-without-volume-has-no-values ()
  (should (cl-every #'null (financial-chart-force-index '((:close 1) (:close 2) (:close 3)) 2))))

;;; 52-week range

(ert-deftest financial-chart-setups-range-is-the-bars-so-far-until-the-window-is-full ()
  (let* ((bars (financial-chart-setups-test--bars '(10 12 11 9 14) 0.5))
         (out (financial-chart-setups-test--outputs 'range-52w bars 3)))
    (should (equal (cdr (assq 'range-high out)) '(10.5 12.5 12.5 12.5 14.5)))
    (should (equal (cdr (assq 'range-low out)) '(9.5 9.5 9.5 8.5 8.5)))))

;;; Squeeze

(ert-deftest financial-chart-setups-squeeze-is-on-when-the-bands-sit-inside-the-channel ()
  ;; Closes barely move while bars have wide ranges: tight Bollinger, wide Keltner.
  (let* ((closes (cl-loop for i below 40 collect (+ 100 (if (cl-evenp i) 0.05 -0.05))))
         (out (financial-chart-setups-test--outputs 'ttm-squeeze (financial-chart-setups-test--bars closes 3.0))))
    (should (cl-every #'null (cl-subseq (cdr (assq 'squeeze-on out)) 0 19)))
    (should (equal (nth 39 (cdr (assq 'squeeze-on out))) 0.0))
    (should (null (nth 39 (cdr (assq 'squeeze-off out)))))))

(ert-deftest financial-chart-setups-squeeze-is-off-when-closes-run-wider-than-ranges ()
  (let* ((closes (cl-loop for i below 40 collect (+ 100 (* 8 (sin i)))))
         (out (financial-chart-setups-test--outputs 'ttm-squeeze (financial-chart-setups-test--bars closes 0.2))))
    (should (equal (nth 39 (cdr (assq 'squeeze-off out))) 0.0))
    (should (null (nth 39 (cdr (assq 'squeeze-on out)))))))

(ert-deftest financial-chart-setups-squeeze-momentum-follows-the-trend-and-splits-by-class ()
  (let* ((up (financial-chart-setups-test--outputs
              'ttm-squeeze (financial-chart-setups-test--bars (cl-loop for i below 60 collect (+ 100 (* 0.5 i))) 1.0)))
         (down (financial-chart-setups-test--outputs
                'ttm-squeeze (financial-chart-setups-test--bars (cl-loop for i below 60 collect (- 100 (* 0.5 i))) 1.0))))
    ;; The first momentum needs 2 x PERIOD - 1 bars.
    (should (null (nth 37 (cdr (assq 'squeeze-momentum up)))))
    (should (numberp (nth 38 (cdr (assq 'squeeze-momentum up)))))
    (should (> (nth 59 (cdr (assq 'squeeze-momentum up))) 0))
    (should (< (nth 59 (cdr (assq 'squeeze-momentum down))) 0))
    ;; Each bar's momentum is in at most one class.
    (dotimes (i 60)
      (should (<= (cl-count-if (lambda (k) (nth i (cdr (assq k up))))
                               '(squeeze-mom-pos-rising squeeze-mom-pos-falling
                                 squeeze-mom-neg-falling squeeze-mom-neg-rising))
                  1)))))

(ert-deftest financial-chart-setups-linreg-end-is-the-line-at-the-last-bar ()
  (should (financial-chart-setups-test--close (financial-chart-setups--linreg-end '(1.0 3.0 5.0 7.0)) 7.0))
  (should (financial-chart-setups-test--close (financial-chart-setups--linreg-end '(0.0 2.0 1.0 3.0)) 2.7)))

;;; Elder impulse

(ert-deftest financial-chart-setups-impulse-is-bull-in-an-uptrend-and-bear-in-a-downtrend ()
  (let ((up (financial-chart-setups-test--outputs
             'elder-impulse (financial-chart-setups-test--bars (cl-loop for i below 60 collect (+ 100 (* i i 0.05))))))
        (down (financial-chart-setups-test--outputs
               'elder-impulse (financial-chart-setups-test--bars (cl-loop for i below 60 collect (- 300 (* i i 0.05)))))))
    (should (equal (nth 59 (cdr (assq 'impulse-bull up))) 0.0))
    (should (equal (nth 59 (cdr (assq 'impulse-bear down))) 0.0))
    (should (null (nth 59 (cdr (assq 'impulse-bear up)))))
    ;; No state until the MACD histogram (34 bars) and its slope exist.
    (should (cl-every #'null (cl-subseq (cdr (assq 'impulse-bull up)) 0 34)))))

;;; Registry and look-ahead

(ert-deftest financial-chart-setups-are-registered ()
  (dolist (name '(wilder-rsi force-index range-52w ttm-squeeze elder-impulse))
    (should (assq name financial-chart-indicator-registry))))

(ert-deftest financial-chart-setups-reject-bad-parameters ()
  (should-error (financial-chart-wilder-rsi nil 0) :type 'financial-chart-invalid-indicator)
  (should-error (financial-chart-force-index nil -3) :type 'financial-chart-invalid-indicator)
  (should-error (financial-chart-ttm-squeeze nil 20 0 1.5) :type 'financial-chart-invalid-indicator)
  (should-error (financial-chart-elder-impulse nil 1.5) :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-setups-do-not-look-ahead ()
  "Every output on the first K bars equals the full run's first K values."
  (let ((bars (financial-chart-setups-test--wave 130)))
    (dolist (spec '((wilder-rsi 14) (wilder-rsi 2) (force-index 13) (force-index 2)
                    (range-52w 40) (ttm-squeeze) (ttm-squeeze 10 2.0 1.5) (elder-impulse)))
      (let ((full (apply #'financial-chart-setups-test--outputs (car spec) bars (cdr spec))))
        (dolist (k '(25 47 80 129))
          (let ((cut (apply #'financial-chart-setups-test--outputs (car spec) (cl-subseq bars 0 k) (cdr spec))))
            (cl-loop for (name . values) in cut
                     do (should (equal values (cl-subseq (cdr (assq name full)) 0 k))))))))))

(provide 'financial-chart-setups-test)
;;; financial-chart-setups-test.el ends here
