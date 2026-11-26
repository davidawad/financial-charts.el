;;; financial-chart-patterns-test.el --- Swing, level and candlestick detector tests -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty: the pure detectors behind the markup annotations.

;;; Code:

(require 'ert)
(defvar financial-chart-patterns-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." financial-chart-patterns-test--dir))
(add-to-list 'load-path (expand-file-name "../core" financial-chart-patterns-test--dir))
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

(defun financial-chart-patterns-test--names (bars)
  "The pattern names found on BARS' last bar."
  (let ((last (1- (length bars))))
    (mapcar #'cadr (seq-filter (lambda (f) (= (car f) last)) (financial-chart-candle-patterns bars)))))

(ert-deftest financial-chart-patterns-swing-points-find-peaks-and-troughs ()
  (let ((bars (mapcar (lambda (h) (financial-chart-patterns-test--bar h h (- h 2) h))
                      '(1 2 3 9 3 2 1 0 -5 0 1 2 3))))
    (should (equal (financial-chart-swing-points bars 2)
                   '((3 high 9) (8 low -7))))
    (should-error (financial-chart-swing-points bars 0) :type 'financial-chart-invalid-indicator)))

(ert-deftest financial-chart-patterns-zigzag-alternates-at-the-deviation ()
  (let* ((closes '(100 104 110 106 100 98 103 109 115))
         (bars (mapcar (lambda (c) (financial-chart-patterns-test--bar c c c c)) closes))
         (pivots (financial-chart-zigzag bars 5)))
    (should (equal pivots '((0 low 100) (2 high 110) (5 low 98) (8 high 115))))
    ;; A 20% deviation never confirms a reversal: only the first leg.
    (should (<= (length (financial-chart-zigzag bars 20)) 2))))

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
    (should (= (length (financial-chart-support-resistance bars :left 2 :min-touches 3)) 1))
    ;; A tight tolerance splits the troughs.
    (should (= (plist-get (car (financial-chart-support-resistance bars :left 2 :tolerance 0.0001)) :touches) 1))
    (should-error (financial-chart-support-resistance bars :tolerance -1)
                  :type 'financial-chart-invalid-indicator)))

(ert-deftest financial-chart-patterns-anchored-vwap-starts-at-the-anchor ()
  (let ((bars (list (financial-chart-patterns-test--bar 10 11 9 10 100)
                    (financial-chart-patterns-test--bar 10 13 11 12 100)
                    (financial-chart-patterns-test--bar 12 15 13 14 300))))
    (should (equal (financial-chart-anchored-vwap bars 1) '(nil 12.0 13.5)))))

(ert-deftest financial-chart-patterns-hammer-and-hanging-man-need-their-trend ()
  (let ((hammer (financial-chart-patterns-test--bar 94.0 94.7 92.4 94.6)))
    (should (member "hammer" (financial-chart-patterns-test--names
                              (append (financial-chart-patterns-test--trend 'down 6) (list hammer)))))
    (let ((hanging (financial-chart-patterns-test--bar 106.0 106.7 104.4 106.6)))
      (should (member "hanging-man" (financial-chart-patterns-test--names
                                     (append (financial-chart-patterns-test--trend 'up 6) (list hanging))))))
    ;; Without history the shape alone is not a pattern.
    (should-not (financial-chart-patterns-test--names (list hammer)))))

(ert-deftest financial-chart-patterns-shooting-star-and-inverted-hammer ()
  (should (member "shooting-star"
                  (financial-chart-patterns-test--names
                   (append (financial-chart-patterns-test--trend 'up 6)
                           (list (financial-chart-patterns-test--bar 106.6 108.2 105.95 106.0))))))
  (should (member "inverted-hammer"
                  (financial-chart-patterns-test--names
                   (append (financial-chart-patterns-test--trend 'down 6)
                           (list (financial-chart-patterns-test--bar 94.0 96.2 93.95 94.6)))))))

(ert-deftest financial-chart-patterns-doji-family-and-marubozu ()
  (should (equal (financial-chart-patterns-test--names (list (financial-chart-patterns-test--bar 10 11 9 10.05)))
                 '("doji")))
  (should (equal (financial-chart-patterns-test--names (list (financial-chart-patterns-test--bar 10.95 11 9 11)))
                 '("dragonfly-doji")))
  (should (equal (financial-chart-patterns-test--names (list (financial-chart-patterns-test--bar 9 11 9 9.05)))
                 '("gravestone-doji")))
  (should (equal (financial-chart-patterns-test--names (list (financial-chart-patterns-test--bar 9 11 9 11)))
                 '("bullish-marubozu")))
  (should (equal (financial-chart-patterns-test--names (list (financial-chart-patterns-test--bar 11 11 9 9)))
                 '("bearish-marubozu"))))

(ert-deftest financial-chart-patterns-engulfing-harami-piercing-dark-cloud ()
  (let ((down (financial-chart-patterns-test--trend 'down 6))
        (up (financial-chart-patterns-test--trend 'up 6)))
    (should (member "bullish-engulfing"
                    (financial-chart-patterns-test--names
                     (append down (list (financial-chart-patterns-test--bar 94.5 94.6 93.4 93.6)
                                        (financial-chart-patterns-test--bar 93.4 95.2 93.3 95.0))))))
    (should (member "bearish-engulfing"
                    (financial-chart-patterns-test--names
                     (append up (list (financial-chart-patterns-test--bar 105.5 106.6 105.4 106.4)
                                      (financial-chart-patterns-test--bar 106.6 106.7 104.8 105.0))))))
    (should (member "bullish-harami"
                    (financial-chart-patterns-test--names
                     (append down (list (financial-chart-patterns-test--bar 95 95.1 90.9 91)
                                        (financial-chart-patterns-test--bar 92 93.2 91.8 93))))))
    (should (member "piercing-line"
                    (financial-chart-patterns-test--names
                     (append down (list (financial-chart-patterns-test--bar 96 96.1 91.9 92)
                                        (financial-chart-patterns-test--bar 91.5 95.1 91.4 95))))))
    (should (member "dark-cloud-cover"
                    (financial-chart-patterns-test--names
                     (append up (list (financial-chart-patterns-test--bar 104 108.1 103.9 108)
                                      (financial-chart-patterns-test--bar 108.5 108.6 104.9 105))))))))

(ert-deftest financial-chart-patterns-three-bar-and-five-bar-patterns ()
  (let ((down (financial-chart-patterns-test--trend 'down 6))
        (up (financial-chart-patterns-test--trend 'up 6)))
    (should (member "morning-star"
                    (financial-chart-patterns-test--names
                     (append down (list (financial-chart-patterns-test--bar 98 98.1 93.9 94)
                                        (financial-chart-patterns-test--bar 93.2 93.6 92.8 93.4)
                                        (financial-chart-patterns-test--bar 94 97.1 93.9 97))))))
    (should (member "evening-star"
                    (financial-chart-patterns-test--names
                     (append up (list (financial-chart-patterns-test--bar 102 106.1 101.9 106)
                                      (financial-chart-patterns-test--bar 106.8 107.2 106.4 106.6)
                                      (financial-chart-patterns-test--bar 106 106.1 102.9 103))))))
    (should (member "three-white-soldiers"
                    (financial-chart-patterns-test--names
                     (list (financial-chart-patterns-test--bar 10 12.1 9.9 12)
                           (financial-chart-patterns-test--bar 11.5 14.1 11.4 14)
                           (financial-chart-patterns-test--bar 13.5 16.1 13.4 16)))))
    (should (member "three-black-crows"
                    (financial-chart-patterns-test--names
                     (list (financial-chart-patterns-test--bar 16 16.1 13.9 14)
                           (financial-chart-patterns-test--bar 14.5 14.6 11.9 12)
                           (financial-chart-patterns-test--bar 12.5 12.6 9.9 10)))))
    (should (member "rising-three-methods"
                    (financial-chart-patterns-test--names
                     (list (financial-chart-patterns-test--bar 10 14.1 9.9 14)
                           (financial-chart-patterns-test--bar 13.5 13.8 12.9 13)
                           (financial-chart-patterns-test--bar 13 13.4 12.4 12.6)
                           (financial-chart-patterns-test--bar 12.6 13.2 12.1 12.4)
                           (financial-chart-patterns-test--bar 12.5 15.1 12.4 15)))))
    (should (member "falling-three-methods"
                    (financial-chart-patterns-test--names
                     (list (financial-chart-patterns-test--bar 15 15.1 10.9 11)
                           (financial-chart-patterns-test--bar 11.5 12.1 11.2 12)
                           (financial-chart-patterns-test--bar 12 12.6 11.6 12.4)
                           (financial-chart-patterns-test--bar 12.4 12.9 11.8 12.6)
                           (financial-chart-patterns-test--bar 12.5 12.6 9.9 10)))))))

(ert-deftest financial-chart-patterns-tweezers-and-the-only-filter ()
  (let* ((down (financial-chart-patterns-test--trend 'down 6))
         (bars (append down (list (financial-chart-patterns-test--bar 94.5 94.6 92.0 92.6)
                                  (financial-chart-patterns-test--bar 92.6 94.0 92.02 93.8)))))
    (should (member "tweezer-bottom" (financial-chart-patterns-test--names bars)))
    (should (seq-every-p (lambda (f) (equal (nth 1 f) "tweezer-bottom"))
                         (financial-chart-candle-patterns bars '("tweezer-bottom"))))
    (should (eq (nth 2 (car (financial-chart-candle-patterns bars '("tweezer-bottom")))) 'bullish))
    (should-error (financial-chart-candle-patterns bars '("rising-hammer"))
                  :type 'financial-chart-invalid-indicator)))

(provide 'financial-chart-patterns-test)
;;; financial-chart-patterns-test.el ends here
