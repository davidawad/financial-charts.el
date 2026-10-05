;;; easel-glue-test.el --- GUI glue fixes found by the fc-qx1.23 spikes -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)
(require 'easel-svg)
(require 'easel-mode)

(defun easel-glue-test--scene ()
  "A scene with discrete items, so it has :map hot spots."
  (easel-compile '(:data (:values [(:k "a" :v 1) (:k "b" :v 3) (:k "c" :v 2)])
                   :mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
                 :size '(400 . 200)))

(ert-deftest easel-glue-svg-image-pins-scale-and-original-map ()
  ;; Without :scale, `create-image' adds :scale `default'; without
  ;; :original-map it rasterizes twice through `image-size' to derive one.
  (let* ((image (easel-svg-image (easel-glue-test--scene)))
         (props (cdr image)))
    (should (eql (plist-get props :scale) 1))
    (should (plist-get props :map))
    (should (equal (plist-get props :original-map) (plist-get props :map)))))

(ert-deftest easel-glue-svg-image-scales-the-map-to-display-pixels ()
  (let* ((scene (easel-glue-test--scene))
         (one (plist-get (cdr (easel-svg-image scene :scale 1)) :map))
         (two (cdr (easel-svg-image scene :scale 2))))
    (should (eql (plist-get two :scale) 2))
    (should (equal (plist-get two :original-map) one))
    (cl-loop for a in one for b in (plist-get two :map)
             do (should (equal (nth 1 a) (nth 1 b)))
             do (should (equal (cadr (car b)) (cons (* 2 (car (cadr (car a)))) (* 2 (cdr (cadr (car a))))))))))

(ert-deftest easel-glue-event-px-divides-by-numeric-scale-only ()
  ;; posn: (WINDOW POS (X . Y) TIME OBJECT POS (COL . ROW) IMAGE (DX . DY) (W . H))
  (cl-flet ((event (scale) (list 'mouse-movement
                                 (list (selected-window) 1 '(160 . 80) 0 nil 1 '(0 . 0)
                                       (list 'image :type 'svg :scale scale) '(150 . 76) '(800 . 400)))))
    (should (equal (easel-mode-event-px (event 1)) [150.0 76.0]))
    (should (equal (easel-mode-event-px (event 2)) [75.0 38.0]))
    (should (equal (easel-mode-event-px (event 'default)) [150.0 76.0]))))

(provide 'easel-glue-test)
;;; easel-glue-test.el ends here
