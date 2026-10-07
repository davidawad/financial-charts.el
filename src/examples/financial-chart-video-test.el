;;; financial-chart-video-test.el --- Tests for the video frame generator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'financial-chart-video)

(defun financial-chart-video-test--feed (frames)
  "The snapshot and FRAMES delta batches of a fresh seeded feed."
  (let ((feed (financial-chart-video-feed-make 7)))
    (cons (financial-chart-video-feed-snapshot feed)
          (cl-loop repeat frames collect (financial-chart-video-feed-step feed)))))

(ert-deftest financial-chart-video-feed-is-deterministic ()
  (should (equal (financial-chart-video-test--feed 60) (financial-chart-video-test--feed 60))))

(ert-deftest financial-chart-video-feed-keeps-a-valid-book ()
  "Every batch applies cleanly, prices sit on the 0.01 tick and the book never crosses."
  (let* ((feed (financial-chart-video-test--feed 600))
         (book (financial-chart-book-make (car feed)))
         (ops (make-hash-table :test 'equal)))
    (dolist (batch (cdr feed))
      (seq-doseq (d batch)
        (puthash (plist-get d :op) t ops)
        (should (< (abs (- (* 100 (plist-get d :price)) (round (* 100 (plist-get d :price))))) 1e-6)))
      (financial-chart-book-apply book batch 0)
      (let ((s (financial-chart-book-summary book)))
        (should (< (plist-get s :best-bid) (plist-get s :best-ask)))
        (should (>= (plist-get s :bids) financial-chart-video-levels))
        (should (>= (plist-get s :asks) financial-chart-video-levels))))
    (should (gethash "insert" ops))
    (should (gethash "delete" ops))
    (should (gethash "update" ops))))

(ert-deftest financial-chart-video-text-svg-keeps-colors-and-escapes ()
  (let ((svg (financial-chart-video-text-svg
              (concat (propertize "a<b" 'face '(:foreground "#26a69a")) " &\n"
                      (propertize "x" 'face 'eas-title))
              :cols 10 :rows 2 :width 200 :height 60)))
    (should (string-prefix-p "<svg" svg))
    (should (string-match-p "width=\"200\" height=\"60\"" svg))
    (should (string-match-p "fill=\"#26a69a\"[^>]*>&lt;<" svg))
    (should (string-match-p "&amp;" svg))
    (should (string-match-p "font-weight=\"bold\"[^>]*>x<" svg))))

(ert-deftest financial-chart-video-book-frames-flash-and-render ()
  "Book frames are SVG strings; a pushed level flashes in the frame's rows."
  (let ((frames nil))
    (financial-chart-video-book-frames
     "ladder" 3 (lambda (i svg view)
                  (push (list i (string-prefix-p "<svg" svg)
                              (financial-chart-book-inspect view))
                        frames))
     :seed 7 :width 640 :height 360)
    (should (equal (mapcar #'car (reverse frames)) '(0 1 2)))
    (should (cl-every #'cadr frames))
    (should (= (plist-get (plist-get (nth 2 (car frames)) :stream) :frames) 3))))

(ert-deftest financial-chart-video-candle-frames-grow-the-last-bar ()
  (let (frames)
    (financial-chart-video-candle-frames
     4 (lambda (i out) (push (cons i out) frames))
     :backend 'text :width 80 :height 24 :frames-per-bar 2)
    (should (= (length frames) 4))
    (should (cl-every (lambda (f) (stringp (cdr f))) frames))
    (should-not (equal (cdr (nth 0 frames)) (cdr (nth 3 frames))))))

(provide 'financial-chart-video-test)
;;; financial-chart-video-test.el ends here
