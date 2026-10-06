;;; financial-chart-overlay-indicators-test.el --- Ichimoku, envelopes, VWAP bands, pivots, SuperTrend -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.3: the price-scale indicators behind the catalog, checked
;; against values worked by hand on small bar sets.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'financial-chart-overlay-indicators)

(defun financial-chart-overlay-test--bars (rows)
  "Bars from ROWS of (HIGH LOW CLOSE [VOLUME [TIME]]); open is the close."
  (mapcar (lambda (r) (append (list :open (nth 2 r) :high (nth 0 r) :low (nth 1 r) :close (nth 2 r))
                              (when (nth 3 r) (list :volume (nth 3 r)))
                              (when (nth 4 r) (list :time (nth 4 r)))))
          rows))

(defun financial-chart-overlay-test--output (outputs name)
  "Values of output NAME among OUTPUTS."
  (plist-get (cl-find name outputs :key (lambda (o) (plist-get o :name))) :values))

(ert-deftest financial-chart-ichimoku-lines-and-shifts ()
  (let* ((bars (financial-chart-overlay-test--bars
                '((10 8 9) (12 9 11) (11 7 8) (13 10 12) (14 11 13))))
         (out (financial-chart-ichimoku bars 2 3 4 2)))
    (should (equal (mapcar (lambda (o) (plist-get o :name)) out)
                   '(ichimoku-tenkan ichimoku-kijun ichimoku-senkou-a ichimoku-senkou-b ichimoku-chikou)))
    ;; Tenkan (2): midpoints of 2-bar high/low.
    (should (equal (financial-chart-overlay-test--output out 'ichimoku-tenkan) '(nil 10.0 9.5 10.0 12.0)))
    ;; Kijun (3): (12+7)/2, (13+7)/2, (14+7)/2.
    (should (equal (financial-chart-overlay-test--output out 'ichimoku-kijun) '(nil nil 9.5 10.0 10.5)))
    (should (equal (financial-chart-overlay-test--output out 'ichimoku-senkou-a) '(nil nil 9.5 10.0 11.25)))
    (should (equal (financial-chart-overlay-test--output out 'ichimoku-senkou-b) '(nil nil nil 10.0 10.5)))
    (should (equal (financial-chart-overlay-test--output out 'ichimoku-chikou) '(9 11 8 12 13)))
    (should (equal (mapcar (lambda (o) (plist-get o :shift)) out) '(nil nil 2 2 -2))))
  ;; Defaults are 9/26/52/26; the shift survives the registry.
  (let ((series (financial-chart-indicator-evaluate
                 'ichimoku (plist-get (alist-get 'ohlc financial-chart-shapes) :example))))
    (should (equal (mapcar (lambda (s) (plist-get s :shift)) series) '(nil nil 26 26 -26))))
  (should-error (financial-chart-ichimoku nil 0) :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-envelopes-sit-percent-off-the-average ()
  (let ((out (financial-chart-envelopes (financial-chart-overlay-test--bars
                                         '((1 1 10) (1 1 20) (1 1 30)))
                                        2 10)))
    (should (equal (financial-chart-overlay-test--output out 'envelope-middle) '(nil 15.0 25.0)))
    (should (equal (mapcar (lambda (v) (and v (/ (round (* v 100)) 100.0)))
                           (financial-chart-overlay-test--output out 'envelope-upper))
                   '(nil 16.5 27.5)))
    (should (equal (mapcar (lambda (v) (and v (/ (round (* v 100)) 100.0)))
                           (financial-chart-overlay-test--output out 'envelope-lower))
                   '(nil 13.5 22.5))))
  (should-error (financial-chart-envelopes nil 20 -1) :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-vwap-bands-weight-deviation-by-volume ()
  ;; Typical prices 10 (volume 1) and 20 (volume 3): VWAP 17.5,
  ;; variance (100 + 3*400)/4 - 17.5^2 = 18.75.
  (let* ((bars (financial-chart-overlay-test--bars '((10 10 10 1) (20 20 20 3) (30 30 30))))
         (out (financial-chart-vwap-bands bars 2))
         (dev (sqrt 18.75)))
    (should (equal (mapcar (lambda (o) (plist-get o :name)) out) '(vwap vwap-upper-2 vwap-lower-2)))
    (should (equal (financial-chart-overlay-test--output out 'vwap) '(10.0 17.5 nil)))
    (should (equal (financial-chart-overlay-test--output out 'vwap-upper-2) (list 10.0 (+ 17.5 (* 2 dev)) nil)))
    (should (equal (financial-chart-overlay-test--output out 'vwap-lower-2) (list 10.0 (- 17.5 (* 2 dev)) nil))))
  (should (= (length (financial-chart-vwap-bands nil)) 5))
  (should-error (financial-chart-vwap-bands nil 0) :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-pivot-points-come-from-the-previous-period ()
  (should (equal (financial-chart-pivot-levels "classic" 12 6 9)
                 '(9.0 12.0 15.0 18.0 6.0 3.0 0.0)))
  (should (equal (mapcar (lambda (v) (/ (round (* v 1000)) 1000.0))
                         (financial-chart-pivot-levels "fibonacci" 12 6 9))
                 '(9.0 11.292 12.708 15.0 6.708 5.292 3.0)))
  (should (equal (car (financial-chart-pivot-levels "woodie" 12 6 9)) 9.0))
  (should (equal (nth 1 (financial-chart-pivot-levels "camarilla" 12 6 9)) (+ 9 (/ 6.6 12))))
  ;; Blocks of two bars: block 0 has no levels, block 1 uses block 0's
  ;; high 12, low 6 and last close 9.
  (let* ((bars (financial-chart-overlay-test--bars '((10 6 8) (12 7 9) (11 9 10) (13 9 12) (14 12 13))))
         (out (financial-chart-pivot-points bars "classic" 2)))
    (should (equal (mapcar (lambda (o) (plist-get o :name)) out)
                   '(pivot-pp pivot-r1 pivot-r2 pivot-r3 pivot-s1 pivot-s2 pivot-s3)))
    (should (equal (financial-chart-overlay-test--output out 'pivot-pp) (list nil nil 9.0 9.0 (/ (+ 13 9 12) 3.0))))
    (should (equal (financial-chart-overlay-test--output out 'pivot-r1) (list nil nil 12.0 12.0 (- (* 2 (/ 34 3.0)) 9)))))
  ;; Calendar periods follow the bar times (UTC); auto is month for daily bars.
  (let* ((day 86400000)
         (t0 (eas-time-parse "2026-01-29"))
         (bars (financial-chart-overlay-test--bars
                (cl-loop for i below 6 collect (list (+ 10 i) (- 10 i) 10 nil (+ t0 (* i day)))))))
    (should (equal (financial-chart-overlay-test--output (financial-chart-pivot-points bars) 'pivot-pp)
                   (append (make-list 3 nil) (make-list 3 (/ (+ 12 8 10) 3.0)))))
    (should (equal (financial-chart-overlay-test--output (financial-chart-pivot-points bars nil "week")
                                                         'pivot-pp)
                   ;; 2026-01-29 is a Thursday: Thu..Sun, then Mon 02-02.
                   (append (make-list 4 nil) (make-list 2 (/ (+ 13 7 10) 3.0))))))
  (should-error (financial-chart-pivot-points nil "nope") :type 'financial-chart-invalid-indicator)
  (should-error (financial-chart-pivot-points (financial-chart-overlay-test--bars '((1 1 1))) nil "week")
                :type 'financial-chart-invalid-indicator)
  (should-error (financial-chart-pivot-points nil nil "fortnight") :type 'financial-chart-invalid-indicator))

(ert-deftest financial-chart-supertrend-trails-and-flips ()
  (let* ((rows (append (cl-loop for i below 8 collect (list (+ 101 i) (+ 99 i) (+ 100 i)))
                       (cl-loop for i below 6 collect (list (- 97 (* 3 i)) (- 95 (* 3 i)) (- 96 (* 3 i))))))
         (out (financial-chart-supertrend (financial-chart-overlay-test--bars rows) 3 1))
         (line (financial-chart-overlay-test--output out 'supertrend))
         (up (financial-chart-overlay-test--output out 'supertrend-up))
         (down (financial-chart-overlay-test--output out 'supertrend-down)))
    (should (= (length line) 14))
    ;; ATR(3) first exists at bar 2: range 2 every bar, lower band mid - 2.
    (should (equal (nth 2 line) 100.0))
    (should (equal (seq-take up 8) '(nil nil 100.0 101.0 102.0 103.0 104.0 105.0)))
    ;; The drop through the lower band flips it down; the up leg ends.
    (should (null (nth 8 up)))
    (should (numberp (nth 8 down)))
    (should (> (nth 8 down) (nth 8 (mapcar #'caddr rows))))
    ;; The active line is whichever leg exists.
    (should (equal line (cl-mapcar (lambda (u d) (or u d)) up down)))
    ;; While falling, the upper band never rises.
    (should (cl-loop for (a b) on (seq-drop down 8) while b always (<= b a)))))

(ert-deftest financial-chart-overlay-indicators-are-registered ()
  (dolist (name '(ichimoku envelopes vwap-bands pivot-points supertrend))
    (should (assq name financial-chart-indicator-registry))
    (let ((series (financial-chart-indicator-evaluate
                   name (plist-get (alist-get 'ohlc financial-chart-shapes) :example))))
      (should (cl-every (lambda (s) (= (length (plist-get s :values)) 48)) series)))))

(provide 'financial-chart-overlay-indicators-test)
;;; financial-chart-overlay-indicators-test.el ends here
