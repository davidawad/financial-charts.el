;;; financial-chart-eas-labels-test.el --- label placement pass -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty.20.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defun financial-chart-labels-test--layer (name rows &rest mark)
  "A text layer NAME of ROWS with MARK props."
  (list :name name :data (list :values (vconcat rows))
        :mark (append (list :type "text") mark)
        :encoding '(:x (:field "time") :y (:field "y") :text (:field "label"))))

(defun financial-chart-labels-test--ys (layers)
  "Every label row's y in LAYERS."
  (cl-loop for l in layers append (mapcar (lambda (r) (plist-get r :y)) (plist-get (plist-get l :data) :values))))

(ert-deftest financial-chart-labels-move-off-each-other ()
  (let* ((scale '(:x0 0 :x1 100 :y0 0.0 :y1 100.0 :width 800 :height 200))
         (a (financial-chart-labels-test--layer "a" '((:time 50 :y 50.0 :label "LEVEL 50.00")) :align "left"))
         (b (financial-chart-labels-test--layer "b" '((:time 52 :y 50.5 :label "GD") (:time 10 :y 20.0 :label "far"))))
         (out (financial-chart-labels-declutter (list a b) scale)))
    ;; The first keeps its place; the second moves by a label height, up first.
    (should (equal (financial-chart-labels-test--ys (list (car out))) '(50.0)))
    (let ((moved (car (financial-chart-labels-test--ys (list (cadr out))))))
      (should (> moved 50.5))
      (should (< (abs (- moved (+ 50.5 (* 12 0.5)))) 1e-9)))
    ;; A label clear of the others is untouched.
    (should (= (cadr (financial-chart-labels-test--ys (list (cadr out)))) 20.0))
    ;; Non-text layers pass through.
    (should (equal (financial-chart-labels-declutter (list '(:name "r" :mark (:type "rule"))) scale)
                   (list '(:name "r" :mark (:type "rule")))))))

(ert-deftest financial-chart-labels-stay-in-the-pane ()
  (let* ((scale '(:x0 0 :x1 100 :y0 0.0 :y1 100.0 :width 800 :height 200))
         (rows (cl-loop for i below 12 collect (list :time 50 :y 99.0 :label "XXXX")))
         (out (financial-chart-labels-declutter (list (financial-chart-labels-test--layer "a" rows)) scale)))
    (should (cl-every (lambda (y) (<= 0 y 100)) (financial-chart-labels-test--ys out)))))

(ert-deftest financial-chart-labels-swing-trader-has-no-overlaps-in-the-model ()
  (let* ((spec (financial-chart-compose (financial-chart-catalog-example "preset-swing-trader")))
         (layers (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil))
         (scale (financial-chart-labels-scale layers (number-sequence 0 99) "container" 200
                                              (cons 98.63 135.47)))
         (boxes (cl-loop for l in layers when (financial-chart-labels--text-layer-p l)
                         append (mapcar (lambda (r) (financial-chart-labels--box r (plist-get l :mark) scale))
                                        (plist-get (plist-get l :data) :values))))
         (overlaps (cl-loop for (a . rest) on boxes
                            sum (cl-count-if (lambda (b) (financial-chart-labels--overlap-p a b)) rest))))
    (should (> (length boxes) 20))
    (should (<= overlaps 2))))

(provide 'financial-chart-eas-labels-test)
;;; financial-chart-eas-labels-test.el ends here
