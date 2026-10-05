;;; easel-theme-test.el --- theme, font metrics and local time -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-theme-test--bars
  '(:data (:values [(:k "a" :v 1) (:k "b" :v 3)]) :mark "bar"
    :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
  "A small bar chart.")

(defconst easel-theme-test--points
  '(:data (:values [(:x 1 :y 2) (:x 3 :y 4)]) :mark "point"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")))
  "A small scatter.")

(ert-deftest easel-theme-merge-layers-objects ()
  (should (equal (easel-theme-merge '(:axis (:a 1 :b 2) :range (:c [1 2])) '(:axis (:b 3)) nil '(:range (:c [9])))
                 '(:axis (:a 1 :b 3) :range (:c [9]))))
  (should (equal (easel-theme-axis '(:axis (:grid t) :axisX (:grid :false)) :x :grid) :false))
  (should (eq (easel-theme-axis '(:axis (:grid t) :axisX (:grid :false)) :y :grid) t)))

(ert-deftest easel-theme-default-sizes-views-and-styles-marks ()
  (let* ((scene (easel-compile easel-theme-test--points))
         (view (aref (plist-get scene :views) 0))
         (item (aref (plist-get (aref (plist-get view :marks) 0) :items) 0)))
    (should (= (aref (plist-get view :bounds) 2) (easel-theme-get easel-theme-default :view :continuousWidth)))
    (should (= (aref (plist-get view :bounds) 3) (easel-theme-get easel-theme-default :view :continuousHeight)))
    (should (equal (plist-get scene :background) (easel-theme-get easel-theme-default :background)))
    (should (equal (plist-get item :fill) (easel-theme-get easel-theme-default :mark :color)))
    (should (= (plist-get item :size) (easel-theme-get easel-theme-default :point :size)))
    ;; axisX.grid is false in the theme: only y draws grid lines.
    (should (equal (mapcar (lambda (a) (plist-get a :grid)) (plist-get view :axes)) '(:false t)))))

(ert-deftest easel-theme-spec-config-overrides-the-default ()
  (let* ((spec (append easel-theme-test--points
                       '(:config (:view (:continuousWidth 200) :mark (:color "#123456") :axisX (:grid t)))))
         (scene (easel-compile spec))
         (view (aref (plist-get scene :views) 0)))
    (should (= (aref (plist-get view :bounds) 2) 200))
    (should (equal (plist-get (aref (plist-get (aref (plist-get view :marks) 0) :items) 0) :fill) "#123456"))
    (should (equal (mapcar (lambda (a) (plist-get a :grid)) (plist-get view :axes)) '(t t)))
    ;; Untouched keys keep the default theme.
    (should (= (aref (plist-get view :bounds) 3) (easel-theme-get easel-theme-default :view :continuousHeight)))))

(ert-deftest easel-theme-svg-theme-overrides-the-scene-config ()
  (let* ((scene (easel-compile (append easel-theme-test--bars '(:config (:axis (:labelColor "#aa0000"))))))
         (plain (easel-svg-render scene))
         (themed (easel-svg-render scene '(:axis (:labelColor "#00bb00")))))
    (should (string-match-p "fill=\"#aa0000\"" plain))
    (should (string-match-p "fill=\"#00bb00\"" themed))
    (should-not (string-match-p "fill=\"#aa0000\"" themed))
    (should (string-match-p "font-family=\"Arial, Liberation Sans, sans-serif\"" plain))))

(ert-deftest easel-theme-bars-round-their-end ()
  (let* ((scene (easel-compile easel-theme-test--bars))
         (items (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :items))
         (r (easel-theme-get easel-theme-default :bar :cornerRadiusEnd)))
    (should (equal (mapcar (lambda (i) (plist-get i :corners)) items) (list (vector r r 0 0) (vector r r 0 0))))
    (should (string-match-p "<path d=\"M[^\"]*A4,4" (easel-svg-render scene)))))

(ert-deftest easel-font-measures-arial ()
  ;; Arial's digits are 1139/2048 em, so "100" at 11px is 18.35px.
  (should (< (abs (- (easel-font-text-width "100" 11) 18.353)) 0.001))
  (should (> (easel-font-text-width "Bold" 12 600) (easel-font-text-width "Bold" 12)))
  (should (= (easel-font-text-width "" 11) 0)))

(ert-deftest easel-layout-text-bounds-follow-vega ()
  (let ((m (easel-layout-metrics 'svg)))
    ;; Middle baseline at 11px: round(0.3*11) - round(0.8*11) = -6 above, 5 below.
    (should (equal (easel-layout-text-bounds m "100" 11 0 0 "right" "middle")
                   (vector (- (easel-font-text-width "100" 11)) -6 0.0 5)))
    ;; Rotated -90 about its anchor, a bottom-baseline title spans x -13..-1.
    (let ((b (easel-layout-text-bounds m "value" 12 0 0 "center" "bottom" -90)))
      (should (< (abs (- (aref b 0) -13)) 1e-9))
      (should (< (abs (- (aref b 2) -1)) 1e-9)))))

;;; Local time

(ert-deftest easel-time-zone-gives-vega-local-semantics ()
  (let ((easel-time-zone "America/Chicago"))
    ;; Date-only strings are UTC midnight, as JavaScript parses them;
    ;; date-times without an offset are local.
    (should (= (easel-time-parse "2026-03-02") (* 1000 1772409600)))
    (should (= (easel-time-parse "2026-03-02T00:00") (* 1000 (+ 1772409600 21600))))
    (should (equal (plist-get (easel-time-fields (easel-time-parse "2026-03-02")) :day) 1))
    (should (equal (easel-time-format (easel-time-parse "2026-03-02") "%b %d") "Mar 01"))
    ;; Local-time ticks fall on local midnights (CST is UTC-6).
    (should (equal (mod (/ (car (easel-scale-time-ticks (easel-time-parse "2026-03-02")
                                                        (easel-time-parse "2026-03-06") 4))
                           1000)
                        86400)
                   21600))
    ;; utc time units floor in UTC whatever the zone.
    (should (= (easel-time-unit-floor "utcyearmonthdate" "2026-03-02T03:00:00Z") (easel-time-parse "2026-03-02")))
    (should (= (easel-time-unit-floor "yearmonthdate" "2026-03-02T03:00:00Z")
               (easel-time-ms 2026 3 1))))
  ;; Unbound, everything is UTC, identical on every machine.
  (should (equal (easel-time-format (easel-time-parse "2026-03-02") "%b %d") "Mar 02"))
  (should (= (easel-time-parse "2026-03-02T00:00") (* 1000 1772409600))))

(ert-deftest easel-time-zone-leaves-utc-scales-in-utc ()
  (let* ((easel-time-zone "America/Chicago")
         (spec '(:data (:values [(:d "2026-03-02" :v 1) (:d "2026-03-04" :v 2)]) :mark "point"
                 :encoding (:x (:field "d" :type "temporal" :scale (:type "utc")) :y (:field "v" :type "quantitative"))))
         (ticks (plist-get (car (append (plist-get (aref (plist-get (easel-compile spec) :views) 0) :axes) nil)) :ticks)))
    (should (member "Mon 02" (mapcar (lambda (tk) (plist-get tk :label)) ticks)))))

(provide 'easel-theme-test)
;;; easel-theme-test.el ends here
