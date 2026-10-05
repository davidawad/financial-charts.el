;;; easel-scale-test.el --- tests for scales, nice domains and ticks -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel-scale)

(ert-deftest easel-scale-linear-ticks-match-d3 ()
  ;; Expected values are d3-array 3 ticks().
  (should (equal (easel-scale-linear-ticks 0 10 10) '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)))
  (should (equal (easel-scale-linear-ticks 0 1 5) '(0.0 0.2 0.4 0.6 0.8 1.0)))
  (should (equal (easel-scale-linear-ticks -0.3 1.7 4) '(0.0 0.5 1.0 1.5)))
  (should (equal (easel-scale-linear-ticks 0 8200 5) '(0.0 2000.0 4000.0 6000.0 8000.0)))
  (should (equal (easel-scale-linear-ticks 10 0 2) '(10.0 5.0 0.0))))

(ert-deftest easel-scale-nice-matches-d3 ()
  (should (equal (easel-scale-nice-linear 0.123 9.87) '(0.0 . 10.0)))
  (should (equal (easel-scale-nice-linear 101.8 110.4) '(101.0 . 111.0)))
  (should (equal (easel-scale-nice-linear 0 12050) '(0.0 . 13000.0)))
  (should (equal (easel-scale-nice-linear -0.42 0.0) '(-0.45 . 0.0))))

(ert-deftest easel-scale-continuous-zero-nice-and-degenerate ()
  (should (equal (plist-get (easel-scale-continuous "linear" 3 9 [0 1] :zero t :nice t) :domain) [0.0 9.0]))
  (should (equal (plist-get (easel-scale-continuous "linear" 3 9.3 [0 1] :zero t :nice t) :domain) [0.0 10.0]))
  (should (equal (plist-get (easel-scale-continuous "linear" -3 -1 [0 1] :zero t) :domain) [-3.0 0.0]))
  (should (equal (plist-get (easel-scale-continuous "linear" 5 5 [0 1]) :domain) [2.5 7.5]))
  (should (equal (plist-get (easel-scale-continuous "log" 3 900 [0 1] :nice t) :domain) [1.0 1000.0])))

(ert-deftest easel-scale-apply-and-invert-round-trip ()
  (let ((s (easel-scale-continuous "linear" 0 10 [100 300])))
    (should (= (easel-scale-apply s 2.5) 150.0))
    (should (= (easel-scale-invert s 150) 2.5)))
  (let ((s (easel-scale-continuous "log" 1 100 [0 200])))
    (should (= (easel-scale-apply s 10) 100.0))
    (should (< (abs (- (easel-scale-invert s 100) 10)) 1e-9)))
  (let ((s (easel-scale-continuous "time" (easel-time-parse "2026-01-01") (easel-time-parse "2026-01-11") [0 100])))
    (should (= (easel-scale-apply s "2026-01-06") 50.0))
    (should (equal (easel-time-iso (easel-scale-invert s 50)) "2026-01-06")))
  (let ((s (easel-scale-continuous "linear" 0 10 [200 0])))
    (should (= (easel-scale-apply s 10) 0.0))
    (should (= (easel-scale-invert s 0) 10.0))))

(ert-deftest easel-scale-band-and-point-match-d3 ()
  (let ((band (easel-scale-band "band" ["a" "b" "c" "d" "e"] [0 300])))
    (should (= (plist-get band :step) 60.0))
    (should (= (plist-get band :bandwidth) 54.0))
    (should (= (easel-scale-apply band "a") 3.0))
    (should (= (easel-scale-apply band "e") 243.0))
    (should (equal (easel-scale-invert band 250) "e"))
    (should-not (easel-scale-apply band "z")))
  (let ((point (easel-scale-band "point" ["a" "b"] [0 40])))
    (should (= (easel-scale-apply point "a") 10.0))
    (should (= (easel-scale-apply point "b") 30.0))
    (should (= (plist-get point :bandwidth) 0.0)))
  (let ((rev (easel-scale-band "band" ["a" "b"] [100 0] 0 0)))
    (should (= (easel-scale-apply rev "a") 50.0))))

(ert-deftest easel-scale-color-scales ()
  (let ((ord (easel-scale-ordinal ["x" "y"] easel-scale-tableau10)))
    (should (equal (easel-scale-apply ord "y") "#f58518")))
  (let ((ramp (list :type "sequential" :domain [0 10] :range easel-scale-blues)))
    (should (equal (easel-scale-apply ramp 0) "#cfe1f2"))
    (should (equal (easel-scale-apply ramp 10) "#0a4a90"))
    (should (equal (easel-scale-apply ramp 5) "#5ba3cf"))))

(ert-deftest easel-scale-tick-formats ()
  (let ((s (easel-scale-continuous "linear" 0 12000 [0 1])))
    (should (equal (funcall (easel-scale-tick-format s 5) 10000.0) "10,000")))
  (let ((s (easel-scale-continuous "linear" 0 1 [0 1])))
    (should (equal (funcall (easel-scale-tick-format s 5) 0.4) "0.4"))
    (should (equal (funcall (easel-scale-tick-format s 5 ".0%") 0.4) "40%")))
  (should (equal (easel-scale-format-number -12345.678 1) "−12,345.7")))

(ert-deftest easel-scale-time-ticks-match-d3 ()
  (cl-flet ((fmt (a b n) (mapcar #'easel-scale-time-multi-format
                                 (easel-scale-time-ticks (easel-time-parse a) (easel-time-parse b) n))))
    (should (equal (fmt "2026-03-02" "2026-03-11" 10)
                   '("Mon 02" "Tue 03" "Wed 04" "Thu 05" "Fri 06" "Sat 07" "Mar 08" "Mon 09" "Tue 10" "Wed 11")))
    (should (equal (fmt "2025-01-15" "2026-06-11" 5) '("April" "July" "October" "2026" "April")))
    (should (equal (fmt "2026-03-02T09:00:00Z" "2026-03-02T17:00:00Z" 4)
                   '("09 AM" "12 PM" "03 PM")))
    (should (equal (fmt "2000-01-01" "2030-01-01" 4) '("2000" "2010" "2020" "2030")))))

(ert-deftest easel-scale-log-ticks ()
  ;; d3-scale log.ticks: every k*10^i while decades < count.
  (should (equal (easel-scale-log-ticks 1.0 100.0 10)
                 '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0 90.0 100.0)))
  (should (equal (easel-scale-log-ticks 1e-3 1e12 10)
                 (mapcar (lambda (e) (expt 10.0 e)) '(-2.0 0.0 2.0 4.0 6.0 8.0 10.0 12.0))))
  ;; Labels: only small mantissas, like d3's log tickFormat (1, 2, 10, 20, ...).
  (let* ((s (easel-scale-continuous "log" 1 10000 [0 1]))
         (fmt (easel-scale-tick-format s 8)))
    (should (equal (mapcar fmt '(1.0 2.0 3.0 10.0 20.0 50.0 1000.0 2000.0 10000.0))
                   '("1" "2" "" "10" "20" "" "1,000" "2,000" "10,000")))))

(provide 'easel-scale-test)
;;; easel-scale-test.el ends here
