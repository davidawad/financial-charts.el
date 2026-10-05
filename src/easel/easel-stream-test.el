;;; easel-stream-test.el --- tests for live streaming (x-easel.stream) -*- lexical-binding: t; -*-

;;; Commentary:

;; Every case runs headlessly: the clock is a variable, timers are off
;; and frames are taken by `easel-stream-tick' at chosen times.

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-stream-test--spec
  '(:data (:values [(:t 1 :p 10) (:t 2 :p 12)])
    :width 200 :height 100
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
    :mark "point"
    :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")))
  "Points with an x brush.")

(defvar easel-stream-test--now 0.0 "The test clock.")

(defmacro easel-stream-test--with (var config &rest body)
  "Open the test spec as streamed view VAR with CONFIG in fresh registries."
  (declare (indent 2))
  `(let* ((easel-views (make-hash-table :test 'equal))
          (easel-streams (make-hash-table :test 'equal))
          (easel-stream-use-timers nil)
          (easel-stream-test--now 0.0)
          (easel-stream-clock (lambda () easel-stream-test--now))
          (,var (easel-stream-open easel-stream-test--spec :id "s" :stream ,config)))
     ,@body))

(defun easel-stream-test--at (time)
  "Set the test clock to TIME."
  (setq easel-stream-test--now (float time)))

(defun easel-stream-test--ts (view)
  "The t column of VIEW's rows."
  (seq-map (lambda (r) (plist-get r :t)) (plist-get (easel-view-data view) :rows)))

(defun easel-stream-test--rows (from to)
  "Rows t = FROM..TO."
  (vconcat (mapcar (lambda (i) (list :t i :p (* 2 i))) (number-sequence from to))))

(ert-deftest easel-stream-config-is-validated ()
  (should (equal (easel-stream-check '(:window 10)) (list :max-fps easel-stream-default-max-fps :window 10)))
  (dolist (case '(((:max-fps 0) . "/x-easel/stream/max-fps") ((:max-fps 120) . "/x-easel/stream/max-fps")
                  ((:window -1) . "/x-easel/stream/window") ((:window 2.5) . "/x-easel/stream/window")
                  ((:fps 5) . "/x-easel/stream/fps") ("fast" . "/x-easel/stream")))
    (should (equal (plist-get (easel-test-should-code "INVALID_INPUT" (easel-stream-check (car case))) :path)
                   (cdr case))))
  (should (equal (easel-stream-config "{\"x-easel\": {\"stream\": {\"max-fps\": 2}}, \"mark\": \"point\"}")
                 '(:max-fps 2)))
  (let ((easel-views (make-hash-table :test 'equal)) (easel-streams (make-hash-table :test 'equal)))
    (easel-test-should-code "INVALID_INPUT" (easel-stream-open easel-stream-test--spec :id "plain"))
    (should-not (easel-view-ids))
    (should (easel-stream-get (easel-stream-open (append '(:x-easel (:stream (:window 3))) easel-stream-test--spec)
                                                 :id "own")))))

(ert-deftest easel-stream-push-events-validate-window ()
  (should (equal (plist-get (easel-test-should-code "EVENT_INVALID"
                              (easel-event-parse '(:type "push" :rows [] :window 0)))
                            :field)
                 "window"))
  (should (equal (easel-event-describe '(:type "push" :rows [1 2] :window 9)) "push 2 rows (window 9)")))

(ert-deftest easel-stream-frames-are-capped ()
  (easel-stream-test--with v '(:max-fps 5)
    (easel-push v (easel-stream-test--rows 3 3))
    (should (equal (easel-stream-test--ts v) '(1 2 3)))
    (easel-stream-test--at 0.05)
    (easel-push v (easel-stream-test--rows 4 4))
    (easel-stream-test--at 0.1)
    (let ((inspect (easel-push v (easel-stream-test--rows 5 6))))
      (should (= (plist-get inspect :rows) 3)))
    (should (= (plist-get (easel-stream-inspect v) :queued) 3))
    (should-not (easel-stream-tick v 0.19))
    (should (easel-stream-tick v 0.2))
    (should (equal (easel-stream-test--ts v) '(1 2 3 4 5 6)))
    (let ((s (easel-stream-inspect v)))
      (should (equal (list (plist-get s :frames) (plist-get s :pushes) (plist-get s :queued)) '(2 3 0))))
    ;; Three pushes, two frames: the log holds exactly what was drawn.
    (should (equal (mapcar (lambda (e) (plist-get e :summary)) (easel-view-log-entries v))
                   '("push 1 rows" "push 3 rows")))
    (should-not (easel-stream-tick v 5))))

(ert-deftest easel-stream-window-keeps-the-newest-rows ()
  (easel-stream-test--with v '(:max-fps 10 :window 4)
    (easel-push v (easel-stream-test--rows 3 5))
    (should (equal (easel-stream-test--ts v) '(2 3 4 5)))
    (let ((ends (lambda () (let ((sum (plist-get (aref (plist-get (easel-inspect v) :views) 0) :visible)))
                             (list (plist-get sum :first) (plist-get sum :last))))))
      (should (equal (funcall ends) '(12 10)))
      (easel-stream-test--at 1)
      (easel-push v (easel-stream-test--rows 6 6))
      ;; Same row count, new rows: the scene must still follow.
      (should (equal (easel-stream-test--ts v) '(3 4 5 6)))
      (should (equal (funcall ends) '(6 12))))
    (easel-stream-test--at 1.01)
    (easel-push v (easel-stream-test--rows 7 20))
    (should (equal (easel-stream-test--ts v) '(3 4 5 6)))
    (should (easel-stream-tick v 1.1))
    (should (equal (easel-stream-test--ts v) '(17 18 19 20)))
    (should (equal (plist-get (car (last (append (easel-view-log-entries v) nil))) :summary)
                   "push 4 rows (window 4)"))))

(ert-deftest easel-stream-pauses-while-the-pointer-is-in-the-chart ()
  (easel-stream-test--with v '(:max-fps 5)
    (let ((easel-stream-hover-hold 2.0))
      (easel-stream-test--at 1)
      (easel-dispatch v '(:type "pointermove" :px [100 50]))
      (easel-stream-test--at 1.5)
      (easel-push v (easel-stream-test--rows 3 3))
      (should (equal (plist-get (easel-stream-inspect v) :held) "pointer"))
      (should-not (easel-stream-tick v 2.9))
      (should (equal (easel-stream-test--ts v) '(1 2)))
      ;; Leaving catches up at once.
      (easel-stream-test--at 3)
      (easel-dispatch v '(:type "pointerleave"))
      (should (equal (easel-stream-test--ts v) '(1 2 3)))
      ;; A pointer that stops moving releases the stream after the hold.
      (easel-stream-test--at 10)
      (easel-dispatch v '(:type "pointermove" :px [100 50]))
      (easel-push v (easel-stream-test--rows 4 4))
      (should-not (easel-stream-tick v 11.9))
      (should (easel-stream-tick v 12.0))
      (should (equal (easel-stream-test--ts v) '(1 2 3 4))))
    (let ((easel-stream-hover-hold nil))
      (easel-stream-test--at 20)
      (easel-dispatch v '(:type "pointermove" :px [100 50]))
      (easel-push v (easel-stream-test--rows 5 5))
      (should-not (easel-stream-tick v 1000))
      (should (equal (easel-stream-flush v) (easel-inspect v)))
      (should (equal (easel-stream-test--ts v) '(1 2 3 4 5))))))

(ert-deftest easel-stream-pauses-during-a-brush-then-catches-up ()
  (easel-stream-test--with v '(:max-fps 5)
    (let ((easel-stream-hover-hold 0)
          (x (lambda (value) (easel-scale-apply (plist-get (plist-get (aref (plist-get (easel-view-scene v) :views) 0)
                                                                     :scales)
                                                          :x)
                                               value))))
      (easel-stream-test--at 1)
      (easel-dispatch v (list :type "pointerdown" :px (vector (funcall x 1.2) 50)))
      (easel-dispatch v (list :type "pointermove" :px (vector (funcall x 1.8) 50)))
      (easel-push v (easel-stream-test--rows 3 4))
      (should (equal (plist-get (easel-stream-inspect v) :held) "drag"))
      (should-not (easel-stream-tick v 5))
      (easel-stream-test--at 6)
      (easel-dispatch v (list :type "pointerup" :px (vector (funcall x 1.8) 50)))
      (should (equal (easel-stream-test--ts v) '(1 2 3 4)))
      (should (plist-get (plist-get (easel-view-state v) :params) :brush)))))

(ert-deftest easel-stream-rows-fail-at-push-time ()
  (easel-stream-test--with v '(:max-fps 5)
    (easel-push v (easel-stream-test--rows 3 3))
    (let ((err (easel-test-should-code "SHAPE_INVALID" (easel-push v [(:t 4 :p 1) (:t 5 :q 1)]))))
      (should (equal (list (plist-get err :index) (plist-get err :field)) '(1 "q"))))
    (should (= (plist-get (easel-stream-inspect v) :queued) 0))))

(ert-deftest easel-stream-replays-what-was-drawn ()
  (easel-stream-test--with a '(:max-fps 5 :window 3)
    (dotimes (i 6)
      (easel-stream-test--at (* i 0.07))
      (easel-push a (easel-stream-test--rows (+ 3 i) (+ 3 i))))
    (easel-stream-tick a 1)
    (let ((b (easel-view-open easel-stream-test--spec :id "b")))
      (easel-replay b (easel-view-log a))
      (should (equal (easel-view-data a) (easel-view-data b)))
      (should (equal (easel-scene-to-json (easel-view-scene a)) (easel-scene-to-json (easel-view-scene b)))))))

(ert-deftest easel-stream-template-attaches-on-first-push ()
  (let ((easel--templates (progn (easel-template-names) (copy-sequence easel--templates)))
        (easel-views (make-hash-table :test 'equal))
        (easel-streams (make-hash-table :test 'equal))
        (easel-stream-use-timers nil)
        (easel-stream-test--now 0.0)
        (easel-stream-clock (lambda () easel-stream-test--now)))
    (easel-template-register
     '(:x-easel (:template "test-stream" :version "1.0.0"
                 :slots (:data (:shape "plist" :required t))
                 :stream (:max-fps 2 :window 2))
       :data (:name "data") :mark "line"
       :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative"))))
    (should (equal (easel-stream-config "test-stream") '(:max-fps 2 :window 2)))
    (let ((v (easel-view-open "test-stream" :bindings (list :data (easel-stream-test--rows 1 2)))))
      (should-not (easel-stream-get v))
      (easel-push v (easel-stream-test--rows 3 3))
      (should (equal (plist-get (easel-stream-inspect v) :window) 2))
      (should (equal (easel-stream-test--ts v) '(2 3)))
      (easel-stream-detach v)
      (should (eq (easel-stream-inspect v) :null))
      (easel-stream-attach v)
      (easel-view-close v)
      (should-not (easel-stream-get (easel-view-open "test-stream" :bindings (list :data (easel-stream-test--rows 1 2))))))
    (let ((plain (easel-view-open "line" :bindings (easel-template-example "line"))))
      (should (= (plist-get (easel-push plain [(:date "2026-02-01" :value 1)]) :rows)
                 (1+ (length (plist-get (easel-template-example "line") :data)))))
      (should-not (easel-stream-get plain)))
    (should (equal (plist-get (plist-get (easel-describe 'stream) :stream) :key) "x-easel.stream"))))

(ert-deftest easel-stream-timers-take-the-deferred-frame ()
  (let ((easel-views (make-hash-table :test 'equal))
        (easel-streams (make-hash-table :test 'equal)))
    (let ((v (easel-stream-open easel-stream-test--spec :id "live" :stream '(:max-fps 20))))
      (easel-push v (easel-stream-test--rows 3 3))
      (easel-push v (easel-stream-test--rows 4 4))
      (should (= (plist-get (easel-stream-inspect v) :queued) 1))
      (with-timeout (2 (ert-fail "the deferred frame never ran"))
        (while (> (plist-get (easel-stream-inspect v) :queued) 0) (sleep-for 0.01)))
      (should (equal (easel-stream-test--ts v) '(1 2 3 4))))))

(provide 'easel-stream-test)
;;; easel-stream-test.el ends here
