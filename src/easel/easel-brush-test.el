;;; easel-brush-test.el --- tests for brush select (fc-qx1.4) -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-brush-test--spec
  '(:data (:values [(:t "2026-03-01" :p 10) (:t "2026-03-02" :p 12) (:t "2026-03-03" :p 9)
                    (:t "2026-03-04" :p 15) (:t "2026-03-05" :p 14) (:t "2026-03-06" :p 20)])
    :width 300 :height 150
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
    :mark "line"
    :encoding (:x (:field "t" :type "temporal") :y (:field "p" :type "quantitative")))
  "A temporal line with an x brush.")

(defmacro easel-brush-test--with-view (var &rest body)
  "Open the brush spec as VAR in a fresh registry, quiet hooks, and run BODY."
  (declare (indent 1))
  `(let ((easel-views (make-hash-table :test 'equal))
         (easel-brush-functions nil))
     (let ((,var (easel-view-open easel-brush-test--spec :id "b")))
       ,@body)))

(defun easel-brush-test--px (view date)
  "Scene pixel [X Y] of DATE mid-plot in VIEW."
  (let* ((sv (aref (plist-get (easel-view-scene view) :views) 0))
         (b (plist-get sv :bounds)))
    (vector (easel-scale-apply (plist-get (plist-get sv :scales) :x) date)
            (+ (aref b 1) (/ (aref b 3) 2.0)))))

(defun easel-brush-test--domain (view)
  "VIEW's x domain as reported by inspect."
  (plist-get (plist-get (aref (plist-get (easel-inspect view) :views) 0) :domains) :x))

(ert-deftest easel-brush-drag-summary-counts-the-selection ()
  (easel-brush-test--with-view v
    (easel-dispatch v (list :type "drag" :from (easel-brush-test--px v "2026-03-05T12:00:00Z")
                            :to (easel-brush-test--px v "2026-03-01T12:00:00Z")))
    (let ((s (easel-brush-summary v)))
      (should (equal (plist-get s :param) "brush"))
      (should (equal (plist-get s :field) "p"))
      (should (= (plist-get s :n) 4))
      (should (equal (list (plist-get s :min) (plist-get s :max) (plist-get s :first) (plist-get s :last))
                     '(9 15 12 14)))
      (should (= (plist-get s :change) 2))
      (should (= (plist-get s :change-pct) 16.67))
      (should (string-match-p "\\`brush 2026-03-01T12:00:00Z\\.\\.2026-03-05T12:00:00Z: n=4  p min 9 max 15 first 12 last 14 change 2 (\\+16\\.67%)\\'"
                              (easel-brush-format s))))))

(ert-deftest easel-brush-event-between-is-a-data-space-brush ()
  "The terminal's mark..point becomes one readable brush event, either direction."
  (easel-brush-test--with-view v
    (let* ((scene (easel-view-scene v))
           (a (easel-brush-test--px v "2026-03-02")) (b (easel-brush-test--px v "2026-03-04"))
           (event (easel-brush-event-between scene b a)))
      (should (equal event '(:type "brush" :param "brush" :x ["2026-03-02" "2026-03-04"])))
      (easel-dispatch v event)
      (should (equal (easel-selection v "brush") [(:t "2026-03-02" :p 12) (:t "2026-03-03" :p 9) (:t "2026-03-04" :p 15)]))
      (should (equal (plist-get (aref (easel-view-log-entries v) 0) :summary) "brush x 2026-03-02..2026-03-04"))
      ;; Off the plot's right edge clamps to the domain's end.
      (should (equal (plist-get (easel-brush-event-between scene a (vector 10000 (aref a 1))) :x)
                     ["2026-03-02" "2026-03-06"]))
      (should (equal (plist-get (easel-test-should-code "EVENT_INVALID"
                                  (easel-brush-event-between scene [-50 -50] [-60 -60]))
                                :field)
                     "param")))))

(ert-deftest easel-brush-z-zooms-into-the-brush-and-history-undoes ()
  (easel-brush-test--with-view v
    (should (equal (easel-brush-test--domain v) ["2026-03-01" "2026-03-06"]))
    (easel-dispatch v '(:type "brush" :x ["2026-03-02" "2026-03-04"]))
    (let ((inspect (easel-dispatch v '(:type "key" :key "z"))))
      (should (equal (easel-brush-test--domain v) ["2026-03-02" "2026-03-04"]))
      (should (eq (plist-get (aref (plist-get inspect :views) 0) :zoomed) t))
      (should (= (plist-get (plist-get (aref (plist-get inspect :views) 0) :visible) :n) 3))
      (should-not (plist-get (plist-get (easel-view-state v) :params) :brush))
      (should-not (easel-brush-summary v)))
    ;; Brushing inside the zoom and zooming again narrows further.
    (easel-dispatch v '(:type "brush" :x ["2026-03-03" "2026-03-04"]))
    (easel-dispatch v '(:type "key" :key "z"))
    (should (equal (easel-brush-test--domain v) ["2026-03-03" "2026-03-04"]))
    (easel-dispatch v '(:type "key" :key "["))
    (should (equal (easel-brush-test--domain v) ["2026-03-02" "2026-03-04"]))
    (easel-dispatch v '(:type "key" :key "["))
    (should (equal (easel-brush-test--domain v) ["2026-03-01" "2026-03-06"]))
    ;; z with nothing brushed changes nothing and records no history.
    (let ((state (easel-view-state v)))
      (easel-dispatch v '(:type "key" :key "z"))
      (should (equal (easel-view-state v) state)))))

(ert-deftest easel-brush-emit-sends-rows-org-json-to-echo-kill-ring-or-callback ()
  (easel-brush-test--with-view v
    (should (equal (plist-get (easel-test-should-code "NOT_FOUND" (easel-brush-emit v)) :view) "b"))
    (easel-dispatch v '(:type "brush" :x ["2026-03-05" "2026-03-06"]))
    (should (equal (plist-get (easel-brush-emit v) :selection) [(:t "2026-03-05" :p 14) (:t "2026-03-06" :p 20)]))
    (should (equal (plist-get (easel-brush-emit v :as 'json) :selection)
                   "[{\"t\":\"2026-03-05\",\"p\":14},{\"t\":\"2026-03-06\",\"p\":20}]"))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil) (interprogram-cut-function nil))
      (easel-brush-emit v :to 'kill-ring)
      (should (equal (car kill-ring) "| t | p |\n|---+---|\n| 2026-03-05 | 14 |\n| 2026-03-06 | 20 |\n"))
      (easel-brush-emit v :as 'rows :to 'kill-ring)
      (should (string-prefix-p "[{\"t\":\"2026-03-05\"" (car kill-ring))))
    (let (got)
      (easel-brush-emit v :as "org" :to (lambda (data summary) (setq got (list data summary))))
      (should (string-prefix-p "| t | p |" (car got)))
      (should (equal (plist-get (cadr got) :change-pct) 42.86)))
    (let (echoed)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        (easel-brush-emit v :to 'echo))
      (should (string-match-p "\\`brush 2026-03-05\\.\\.2026-03-06: n=2 " echoed)))
    (easel-test-should-code "INVALID_INPUT" (easel-brush-emit v :to 'printer))))

(ert-deftest easel-brush-functions-run-when-a-brush-changes ()
  (easel-brush-test--with-view v
    (let (calls)
      (setq easel-brush-functions (list (lambda (view param summary)
                                          (push (list (easel-view-id view) param (and summary (plist-get summary :n)))
                                                calls))))
      (easel-dispatch v '(:type "brush" :x ["2026-03-01" "2026-03-02"]))
      (easel-dispatch v '(:type "pointermove" :px [100 60]))
      (easel-dispatch v '(:type "brush" :x ["2026-03-01" "2026-03-03"]))
      (easel-dispatch v '(:type "key" :key "escape"))
      (should (equal (reverse calls) '(("b" "brush" 2) ("b" "brush" 3) ("b" "brush" nil)))))))

(ert-deftest easel-brush-replays-from-the-log ()
  (let ((easel-views (make-hash-table :test 'equal)) (easel-brush-functions nil))
    (let ((a (easel-view-open easel-brush-test--spec :id "a"))
          (b (easel-view-open easel-brush-test--spec :id "b")))
      (easel-dispatch a (list :type "drag" :from (easel-brush-test--px a "2026-03-02")
                              :to (easel-brush-test--px a "2026-03-05")))
      (easel-dispatch a '(:type "key" :key "z"))
      (easel-dispatch a '(:type "brush" :x ["2026-03-03" "2026-03-04"]))
      (easel-replay b (easel-view-log a))
      (should (equal (easel-view-state a) (easel-view-state b)))
      (should (equal (easel-brush-summary a) (easel-brush-summary b)))
      (should (equal (easel-scene-to-json (easel-view-scene a)) (easel-scene-to-json (easel-view-scene b)))))))

(ert-deftest easel-brush-terminal-mark-and-point-brush-then-copy ()
  (let ((easel-views (make-hash-table :test 'equal)) (easel-brush-functions nil))
    (let* ((view (easel-view-open easel-brush-test--spec :id "tty"))
           (buffer (easel-show view 'text)))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq (lookup-key easel-view-mode-map "b") #'easel-brush-region))
            (should-error (let ((mark-ring nil)) (set-marker (mark-marker) nil) (easel-brush-region)) :type 'user-error)
            (let ((from (text-property-any (point-min) (point-max) 'easel-datum 1))
                  (to (text-property-any (point-min) (point-max) 'easel-datum 4)))
              (should (and from to))
              (set-mark from)
              (goto-char to)
              (easel-brush-region)
              (let ((s (easel-brush-summary view)))
                (should (equal (plist-get s :param) "brush"))
                (should (<= 3 (plist-get s :n) 4)))
              (should (text-property-not-all (point-min) (point-max) 'easel-brush nil))
              (let ((kill-ring nil) (kill-ring-yank-pointer nil) (interprogram-cut-function nil)
                    (inhibit-message t))
                (easel-brush-copy)
                (should (string-prefix-p "| t | p |" (car kill-ring))))
              (execute-kbd-macro "z")
              (should (eq (plist-get (aref (plist-get (easel-inspect view) :views) 0) :zoomed) t))
              (should-not (text-property-not-all (point-min) (point-max) 'easel-brush nil))))
        (kill-buffer buffer)))))

(provide 'easel-brush-test)
;;; easel-brush-test.el ends here
