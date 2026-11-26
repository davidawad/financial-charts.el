;;; financial-chart-patterns-test.el --- swing, level and candlestick list tests -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty: the plain-list answers of `financial-chart-patterns', read
;; off the detectors.  The detectors themselves are tested by the
;; registry harness (src/detectors/financial-chart-detectors-harness-test.el),
;; including the TA-Lib oracle.

;;; Code:

(require 'ert)
(defvar financial-chart-patterns-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(dolist (d '(".." "../core" "../detectors"))
  (add-to-list 'load-path (expand-file-name d financial-chart-patterns-test--dir)))
(require 'financial-chart-patterns)

(defun financial-chart-patterns-test--bar (o h l c &optional v)
  "A bar O H L C with volume V (default 100)."
  (list :open o :high h :low l :close c :volume (or v 100)))

(defun financial-chart-patterns-test--trend (direction n &optional start)
  "N bars stepping DIRECTION (`up' or `down') by 1 from START (default 100)."
  (let ((p (or start 100)) (step (if (eq direction 'up) 1 -1)) bars)
    (dotimes (_ n)
      (let ((next (+ p step)))
        (push (financial-chart-patterns-test--bar p (+ (max p next) 0.2) (- (min p next) 0.2) next) bars)
        (setq p next)))
    (nreverse bars)))

(defun financial-chart-patterns-test--names (bars &optional params)
  "The pattern names found on BARS' last bar with PARAMS."
  (let ((last (1- (length bars))))
    (mapcar #'cadr (seq-filter (lambda (f) (= (car f) last)) (financial-chart-candle-patterns bars nil params)))))

(ert-deftest financial-chart-patterns-swing-points-find-peaks-and-troughs ()
  (let ((bars (mapcar (lambda (h) (financial-chart-patterns-test--bar h h (- h 2) h))
                      '(1 2 3 9 3 2 1 0 -5 0 1 2 3))))
    (should (equal (financial-chart-swing-points bars 2) '((3 high 9) (8 low -7))))
    ;; Unconfirmed: the last RIGHT bars cannot hold a pivot yet.
    (should (equal (financial-chart-swing-points (seq-take bars 10) 2) '((3 high 9))))
    (should-error (financial-chart-swing-points bars 0) :type 'financial-chart-invalid-indicator)))

(ert-deftest financial-chart-patterns-zigzag-keeps-confirmed-pivots ()
  (let* ((closes '(100 104 110 106 100 98 103 109 115))
         (bars (mapcar (lambda (c) (financial-chart-patterns-test--bar c c c c)) closes)))
    ;; The running high at 115 is not confirmed yet.
    (should (equal (financial-chart-zigzag bars 5) '((0 low 100) (2 high 110) (5 low 98))))
    (should (<= (length (financial-chart-zigzag bars 20)) 1))))

(ert-deftest financial-chart-patterns-support-resistance-clusters-and-ranks ()
  ;; Three troughs near 90 and two peaks near 110, then a close at 98.
  (let* ((path '(100 95 90 95 100 105 110 105 100 95 90.4 95 100 105 109.8 105 100 95 90.2 95 96 97 98))
         (bars (mapcar (lambda (c) (financial-chart-patterns-test--bar c c c c)) path))
         (levels (financial-chart-support-resistance bars :left 2 :right 2)))
    (should (= (length levels) 2))
    (should (eq (plist-get (car levels) :kind) 'support))
    (should (= (plist-get (car levels) :touches) 3))
    (should (< (abs (- (plist-get (car levels) :price) 90.2)) 0.01))
    (should (eq (plist-get (cadr levels) :kind) 'resistance))
    (should (= (plist-get (cadr levels) :touches) 2))
    (should (= (plist-get (cadr levels) :first) 6))
    (should (string-prefix-p "level.auto-sr@" (plist-get (car levels) :id)))
    (should (= (length (financial-chart-support-resistance bars :left 2 :min-touches 3)) 1))
    ;; A tight band splits the troughs.
    (should (= (plist-get (car (financial-chart-support-resistance bars :left 2 :tolerance 0.0001)) :touches) 1))
    (should-error (financial-chart-support-resistance bars :tolerance -1)
                  :type 'financial-chart-invalid-indicator)))

(ert-deftest financial-chart-patterns-anchored-vwap-starts-at-the-anchor ()
  (let ((bars (list (financial-chart-patterns-test--bar 10 11 9 10 100)
                    (financial-chart-patterns-test--bar 10 13 11 12 100)
                    (financial-chart-patterns-test--bar 12 15 13 14 300))))
    (should (equal (financial-chart-anchored-vwap bars 1) '(nil 12.0 13.5)))))

(ert-deftest financial-chart-patterns-hammer-needs-its-downtrend ()
  (let* ((down (financial-chart-patterns-test--trend 'down 14))
         (p (plist-get (car (last down)) :close))
         (hammer (financial-chart-patterns-test--bar p (+ p 0.7) (- p 1.6) (+ p 0.6)))
         (up (financial-chart-patterns-test--trend 'up 14))
         (q (plist-get (car (last up)) :close))
         (hanging (financial-chart-patterns-test--bar q (+ q 0.7) (- q 1.6) (+ q 0.6))))
    (should (member "hammer" (financial-chart-patterns-test--names (append down (list hammer)))))
    (should (member "hanging-man" (financial-chart-patterns-test--names (append up (list hanging)))))
    ;; After an advance the same shape is no hammer -- unless the trend test is off (TA-Lib).
    (should-not (member "hammer" (financial-chart-patterns-test--names (append up (list hanging)))))
    (should (member "hammer" (financial-chart-patterns-test--names
                              (append (financial-chart-patterns-test--trend 'down 14)
                                      (list (financial-chart-patterns-test--bar 86 86.7 84.4 86.6)))
                              '(:trend 0))))))

(ert-deftest financial-chart-patterns-names-and-filter ()
  (should (member "hammer" (financial-chart-candle-pattern-list)))
  (should (equal (assoc "hammer" (financial-chart-candle-pattern-names)) '("hammer" "H" bullish)))
  (let ((bars (append (financial-chart-patterns-test--trend 'down 14)
                      (list (financial-chart-patterns-test--bar 86 86.02 85 86.01)))))
    (should (seq-every-p (lambda (f) (equal (nth 1 f) "doji"))
                         (financial-chart-candle-patterns bars '("doji")))))
  (should-error (financial-chart-candle-patterns nil '("rising-hammer"))
                :type 'financial-chart-invalid-indicator))

(provide 'financial-chart-patterns-test)
;;; financial-chart-patterns-test.el ends here
