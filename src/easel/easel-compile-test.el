;;; easel-compile-test.el --- tests for compile to scene/v1 and hit-testing -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-compile-test--stacked
  '(:data (:values [(:k "a" :c "x" :v 1) (:k "a" :c "y" :v 2) (:k "b" :c "x" :v 3) (:k "b" :c "y" :v 1)])
    :vconcat [(:mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                      :color (:field "c" :type "nominal")))
              (:mark "point" :encoding (:x (:field "v" :type "quantitative") :y (:field "k" :type "nominal")))])
  "A stacked bar over a strip plot, with a legend.")

(defconst easel-compile-test--crosshair
  '(:data (:values [(:t "2026-01-01" :p 10) (:t "2026-01-02" :p 12) (:t "2026-01-03" :p 11)
                    (:t "2026-01-04" :p 15)])
    :width 300 :height 150
    :layer [(:mark "line" :encoding (:x (:field "t" :type "temporal") :y (:field "p" :type "quantitative")))
            (:params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "rule"
             :encoding (:x (:field "t" :type "temporal")
                        :opacity (:condition (:param "hover" :empty :false :value 1) :value 0)))])
  "A line with a crosshair rule driven by a point selection.")

(defun easel-compile-test--scene (name spec &rest args)
  "Golden-compare the scene of SPEC compiled with ARGS under NAME."
  (easel-test-golden (format "scene-%s.json" name)
                     (easel-scene-to-json (apply #'easel-compile spec args) t)))

(ert-deftest easel-compile-scene-goldens ()
  (easel-compile-test--scene "bars" (easel-resolve "bars" (easel-template-example "bars")))
  (easel-compile-test--scene "line-points" (easel-resolve "line" (plist-put (easel-template-example "line") :points t)))
  (easel-compile-test--scene "line-text" (easel-resolve "line" (easel-template-example "line"))
                             :target 'text :size '(:cols 60 :rows 16))
  (easel-compile-test--scene "stacked-concat" easel-compile-test--stacked)
  (easel-compile-test--scene "histogram" '(:data (:values [(:a 1) (:a 2) (:a 2) (:a 7) (:a 9)])
                                           :mark "bar" :encoding (:x (:field "a" :bin t) :y (:aggregate "count"))))
  (easel-compile-test--scene "crosshair" easel-compile-test--crosshair))

(ert-deftest easel-compile-is-pure-and-json-serializable ()
  (let ((spec (easel-resolve "bars" (easel-template-example "bars"))))
    (should (equal (easel-scene-to-json (easel-compile spec)) (easel-scene-to-json (easel-compile spec))))
    (should (stringp (easel-json-encode (easel-compile easel-compile-test--stacked))))))

(ert-deftest easel-compile-items-carry-datum-back-references ()
  (let* ((scene (easel-compile easel-compile-test--stacked))
         (mark (easel-scene-mark scene "vconcat_0/0")))
    (seq-doseq (item (plist-get mark :items))
      (let ((row (easel-scene-row scene "vconcat_0/0" (plist-get item :datum))))
        (should (plist-member row :_easel_row))
        (should (plist-get row :v_end))))
    (should (equal (plist-get (easel-scene-row scene "vconcat_0/0" 2) :_easel_row) 2))))

(ert-deftest easel-compile-fits-a-target-size ()
  (let ((scene (easel-compile easel-compile-test--crosshair :size '(640 . 360))))
    (should (equal (plist-get (plist-get scene :size) :w) 640))
    (should (equal (plist-get (plist-get scene :size) :h) 360))
    (let ((b (plist-get (aref (plist-get scene :views) 0) :bounds)))
      (should (> (aref b 2) 500))
      (should (<= (+ (aref b 0) (aref b 2)) 640))))
  (let ((scene (easel-compile easel-compile-test--crosshair :target 'text :size '(:cols 50 :rows 12))))
    (should (equal (plist-get (plist-get scene :size) :w) 350))
    (should (equal (plist-get scene :target) "text"))))

(ert-deftest easel-compile-conditions-follow-the-param-hook ()
  (let* ((rule (lambda (scene) (aref (plist-get (easel-scene-mark scene "main/1") :items) 1)))
         (off (funcall rule (easel-compile easel-compile-test--crosshair))))
    (should (equal (plist-get off :opacity) 0))
    (let* ((easel-encode-param-test-function
            (lambda (param row _empty) (and (equal param "hover") (equal (plist-get row :p) 12))))
           (on (funcall rule (easel-compile easel-compile-test--crosshair))))
      (should (equal (plist-get on :opacity) 1)))))

(ert-deftest easel-compile-zoom-state-replaces-the-domain ()
  (let* ((lo (easel-time-parse "2026-01-02")) (hi (easel-time-parse "2026-01-03"))
         (scene (easel-compile easel-compile-test--crosshair
                               :state (list :domains (list :main (list :x (vector lo hi))))))
         (x (plist-get (plist-get (aref (plist-get scene :views) 0) :scales) :x)))
    (should (equal (plist-get x :domain) (vector (float lo) (float hi))))
    (should (eq (plist-get (aref (plist-get scene :views) 0) :clip) t))))

(ert-deftest easel-compile-draws-an-interval-brush-from-state ()
  (let* ((spec '(:data (:values [(:x 0 :y 0) (:x 10 :y 5)])
                 :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
                 :mark "point" :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative"))))
         (scene (easel-compile spec :state '(:params (:brush (:x [2 4])))))
         (brush (easel-scene-mark scene "main/brush:brush")))
    (should brush)
    (should (equal (plist-get scene :params) [(:name "brush" :select (:type "interval" :encodings ["x"]) :view "main")]))
    (should (= (plist-get (aref (plist-get brush :items) 0) :w) 60.0))))

(ert-deftest easel-compile-decimates-long-series-with-lttb ()
  (let* ((values (vconcat (mapcar (lambda (i) (list :x i :y (if (= i 777) 1000 (% (* i 37) 50))))
                                  (number-sequence 0 4999))))
         (scene (easel-compile (list :data (list :values values) :width 200 :height 100
                                     :mark "line" :encoding '(:x (:field "x" :type "quantitative")
                                                              :y (:field "y" :type "quantitative")))))
         (item (aref (plist-get (easel-scene-mark scene "main/0") :items) 0)))
    (should (= (length (plist-get item :points)) 200))
    (should (= (plist-get item :decimated) 5000))
    (should (seq-contains-p (plist-get item :datum) 777))
    (should (= (aref (plist-get item :datum) 199) 4999))))

(ert-deftest easel-compile-unsupported-and-unresolved-fail-as-data ()
  (should (equal (plist-get (easel-test-should-code "UNSUPPORTED_FEATURE"
                              (easel-compile '(:data (:values []) :mark "arc")))
                            :path)
                 "/mark"))
  (easel-test-should-code "INVALID_INPUT" (easel-compile '(:data (:name "bars") :mark "bar")))
  (easel-test-should-code "UNSUPPORTED_FEATURE" (easel-compile '(:data (:url "x.csv") :mark "bar")))
  (easel-test-should-code "UNSUPPORTED_FEATURE"
    (easel-compile '(:data (:values [(:a 1 :b 2)]) :mark (:type "line" :interpolate "monotone")
                     :encoding (:x (:field "a") :y (:field "b"))))))

(ert-deftest easel-compile-rows-override-the-root-data ()
  (let ((scene (easel-compile (easel-resolve "bars" (easel-template-example "bars"))
                              :rows [(:category "Z" :value 1)])))
    (should (equal (plist-get (plist-get (plist-get (aref (plist-get scene :views) 0) :scales) :x) :domain)
                   ["Z"]))))

;;; Hit-testing

(ert-deftest easel-hit-series-snaps-to-the-nearest-x ()
  (let* ((scene (easel-compile easel-compile-test--crosshair))
         (view (aref (plist-get scene :views) 0))
         (x (plist-get (plist-get view :scales) :x))
         (px (easel-scale-apply x "2026-01-02T10:00:00Z"))
         (hit (easel-hit scene "main" (vector px 60) t)))
    (should (equal (plist-get hit :view) "main"))
    (should (equal (plist-get (plist-get hit :row) :t) "2026-01-02"))
    (should (< (plist-get hit :distance) 50))))

(ert-deftest easel-hit-bars-contain-and-points-are-nearest ()
  (let* ((scene (easel-compile easel-compile-test--stacked))
         (bar (aref (plist-get (easel-scene-mark scene "vconcat_0/0") :items) 3))
         (hit (easel-hit scene nil (vector (+ (plist-get bar :x) 2) (+ (plist-get bar :y) 2)))))
    (should (equal (plist-get hit :mark) "vconcat_0/0"))
    (should (equal (plist-get hit :datum) 3))
    (should (= (plist-get hit :distance) 0)))
  (let* ((scene (easel-compile easel-compile-test--stacked))
         (pt (aref (plist-get (easel-scene-mark scene "vconcat_1/0") :items) 2))
         (hit (easel-hit scene "vconcat_1" (vector (+ 3 (plist-get pt :x)) (plist-get pt :y)))))
    (should (equal (plist-get hit :datum) 2))
    (should (= (plist-get hit :distance) 3.0))))

(ert-deftest easel-hit-index-kinds ()
  (let ((scene (easel-compile easel-compile-test--stacked)))
    (should (equal (plist-get (plist-get (easel-scene-mark scene "vconcat_0/0") :index) :kind) "rects"))
    (should (equal (plist-get (plist-get (easel-scene-mark scene "vconcat_1/0") :index) :kind) "grid")))
  (should (equal (plist-get (plist-get (easel-scene-mark (easel-compile easel-compile-test--crosshair) "main/0")
                                       :index)
                            :kind)
                 "x-sorted")))

(provide 'easel-compile-test)
;;; easel-compile-test.el ends here
