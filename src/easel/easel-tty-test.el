;;; easel-tty-test.el --- tests for terminal parity (fc-qx1.8) -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-tty-test--line
  '(:data (:values [(:t "2026-01-01" :p 10) (:t "2026-01-02" :p 12) (:t "2026-01-03" :p 11)
                    (:t "2026-01-04" :p 15) (:t "2026-01-05" :p 13) (:t "2026-01-06" :p 18)])
    :width 300 :height 150
    :params [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")]
    :layer [(:mark "line" :encoding (:x (:field "t" :type "temporal") :y (:field "p" :type "quantitative")))
            (:params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "rule"
             :encoding (:x (:field "t" :type "temporal")
                        :opacity (:condition (:param "hover" :empty :false :value 1) :value 0)))])
  "A temporal line with a crosshair, zoomable on x.")

(defconst easel-tty-test--points
  '(:data (:values [(:x 1 :y 3 :c "u") (:x 2 :y 5 :c "v") (:x 3 :y 4 :c "u") (:x 4 :y 8 :c "v") (:x 5 :y 6 :c "u")])
    :width 200 :height 100
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))
             (:name "pick" :select "point")
             (:name "legend" :select (:type "point" :fields ["c"]) :bind "legend")]
    :mark "point"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
               :color (:field "c" :type "nominal")
               :opacity (:condition (:param "legend" :value 1) :value 0.2)))
  "Points with a brush, a click selection and a legend toggle.")

(defconst easel-tty-test--text-size '(:cols 72 :rows 24) "Text view size for lockstep replays.")

(defmacro easel-tty-test--with (bindings &rest body)
  "Fresh registry, quiet hooks; BINDINGS as in `let*'; then BODY."
  (declare (indent 1))
  `(let ((easel-views (make-hash-table :test 'equal))
         (easel-brush-functions nil) (easel-action-inhibit t) (inhibit-message t))
     (let* ,bindings ,@body)))

(defun easel-tty-test--px (view x y)
  "Scene pixel of data X, Y in VIEW's first scene view."
  (let ((scales (plist-get (aref (plist-get (easel-view-scene view) :views) 0) :scales)))
    (vector (easel-scale-apply (plist-get scales :x) x) (easel-scale-apply (plist-get scales :y) y))))

(defun easel-tty-test--should-agree (result)
  "Assert parity RESULT is ok, showing its mismatches when not."
  (should (equal (plist-get result :mismatches) []))
  (should (eq (plist-get result :ok) t)))

;;; Translation and comparison

(ert-deftest easel-tty-translate-lands-on-the-same-datum ()
  (easel-tty-test--with ((gui (easel-view-open easel-tty-test--points :id "gui"))
                         (tty (easel-view-open easel-tty-test--points :id "tty" :target 'text
                                               :size easel-tty-test--text-size)))
    (let* ((px (easel-tty-test--px gui 4 8))
           (out (easel-parity-translate-px px (easel-view-scene gui) (easel-view-scene tty))))
      (should (equal (plist-get (easel-hit (easel-view-scene tty) nil out) :datum) 3))
      (should (< (plist-get (easel-hit (easel-view-scene tty) nil out) :distance) 1e-6)))
    ;; Legend entries map to the same entry, keys pass through untouched.
    (let* ((legend (aref (plist-get (aref (plist-get (easel-view-scene gui) :views) 0) :legends) 0))
           (b (plist-get (aref (plist-get legend :entries) 1) :bounds))
           (px (vector (+ (aref b 0) 1.0) (+ (aref b 1) 1.0)))
           (out (easel-parity-translate (list :type "click" :px px) (easel-view-scene gui) (easel-view-scene tty))))
      (should (equal (easel-parity--legend-entry (easel-view-scene tty) (aref (plist-get out :px) 0)
                                                 (aref (plist-get out :px) 1))
                     (list (plist-get (aref (plist-get (easel-view-scene tty) :views) 0) :id) "color" "v"))))
    (should (equal (easel-parity-translate '(:type "key" :key "+") nil nil) '(:type "key" :key "+")))))

(ert-deftest easel-tty-parity-diff-ignores-pixels-and-names-paths ()
  (should-not (easel-parity-diff '(:a [1.0 2.0] :b "x") '(:a [1.0000000000001 2.0] :b "x")))
  (should (equal (easel-parity-diff '(:a [1 2] :b (:c 1)) '(:a [1 3] :b (:c "1")))
                 '((:path "/a/1" :a 2 :b 3) (:path "/b/c" :a 1 :b "1")))))

;;; One log, both backends

(ert-deftest easel-tty-parity-zoom-pan-hover-log ()
  (easel-tty-test--with ((gui (easel-view-open easel-tty-test--line :id "gui"))
                         (tty (easel-view-open easel-tty-test--line :id "tty" :target 'text
                                               :size easel-tty-test--text-size))
                         (d2 (easel-tty-test--px gui "2026-01-02" 12))
                         (d3 (easel-tty-test--px gui "2026-01-03" 11))
                         (d5 (easel-tty-test--px gui "2026-01-05" 13))
                         (log (list (list :type "pointermove" :px d2)
                                    (list :type "wheel" :px d3 :delta -2)
                                    (list :type "wheel" :px d3 :delta -1)
                                    '(:type "key" :key "+") '(:type "key" :key "right")
                                    (list :type "drag" :from d3 :to d2)
                                    (list :type "pointerdown" :px d2) (list :type "pointermove" :px d3)
                                    (list :type "pointerup" :px d5)
                                    '(:type "key" :key "[") '(:type "key" :key "[") '(:type "key" :key "]")
                                    (list :type "pointermove" :px d5)
                                    (list :type "dblclick" :px d3) '(:type "key" :key "0")
                                    '(:type "pointerleave"))))
    (let ((result (easel-parity-replay log gui tty)))
      (easel-tty-test--should-agree result)
      (should (= (plist-get result :steps) (length log))))
    ;; The replay really moved things: history and the zoom happened.
    (should (> (plist-get (easel-parity-state tty) :history) 3))))

(ert-deftest easel-tty-parity-brush-click-legend-log ()
  (easel-tty-test--with ((gui (easel-view-open easel-tty-test--points :id "gui"))
                         (tty (easel-view-open easel-tty-test--points :id "tty" :target 'text
                                               :size easel-tty-test--text-size))
                         (legend (aref (plist-get (aref (plist-get (easel-view-scene gui) :views) 0) :legends) 0))
                         (b (plist-get (aref (plist-get legend :entries) 1) :bounds))
                         (log (list (list :type "pointermove" :px (easel-tty-test--px gui 2 5))
                                    (list :type "click" :px (easel-tty-test--px gui 3 4))
                                    (list :type "drag" :from (easel-tty-test--px gui 1.5 5) :to (easel-tty-test--px gui 4.5 5))
                                    '(:type "key" :key "z") '(:type "key" :key "[")
                                    '(:type "brush" :x [2 4])
                                    (list :type "click" :px (vector (+ (aref b 0) 2.0) (+ (aref b 1) 2.0)))
                                    (list :type "pointermove" :px (easel-tty-test--px gui 5 6))
                                    '(:type "key" :key "escape"))))
    (easel-tty-test--should-agree (easel-parity-replay log gui tty))
    ;; z zoomed to the brush, [ undid it, escape cleared the selections.
    (should (equal (plist-get (easel-parity-state tty) :future) 1))
    (should-not (plist-get (easel-parity-state tty) :params))))

;;; Driving a text buffer, replaying its log on the GUI backend

(defun easel-tty-test--replay-on-gui (spec tty-view)
  "Replay TTY-VIEW's log on a fresh text view and a fresh GUI view of SPEC."
  (let ((text (easel-view-open spec :id "text2" :target 'text :size (easel-view-size tty-view)))
        (gui (easel-view-open spec :id "gui2")))
    (prog1 (easel-parity-replay (easel-view-log tty-view) text gui)
      (should-not (easel-parity-diff (easel-parity-state text) (easel-parity-state tty-view))))))

(ert-deftest easel-tty-keyboard-session-replays-on-the-gui-backend ()
  (easel-tty-test--with ((view (easel-view-open easel-tty-test--line :id "tty"))
                         (buffer (easel-show view 'text)))
    (unwind-protect
        (with-current-buffer buffer
          (execute-kbd-macro "nn")
          (should (equal (plist-get (plist-get (easel-view-state view) :hover) :datum) 1))
          (should (get-text-property (point) 'easel-datum))
          (execute-kbd-macro [right right])
          ;; A zoom redraws with new labels; point keeps its cell, so the
          ;; hover is not re-sent.
          (let ((cell (cons (line-number-at-pos) (current-column))))
            (execute-kbd-macro "+")
            (should (equal (cons (line-number-at-pos) (current-column)) cell))
            (should (equal (plist-get (car (easel-view-log view)) :type) "key")))
          (execute-kbd-macro [S-right])
          (execute-kbd-macro "p")
          (execute-kbd-macro "-[")
          (execute-kbd-macro "\e\e")
          (should-not (plist-get (easel-view-state view) :hover))
          (should (>= (length (easel-view-log view)) 8))
          (easel-tty-test--should-agree (easel-tty-test--replay-on-gui easel-tty-test--line view)))
      (kill-buffer buffer))))

(ert-deftest easel-tty-brush-session-replays-on-the-gui-backend ()
  (easel-tty-test--with ((view (easel-view-open easel-tty-test--points :id "tty"))
                         (buffer (easel-show view 'text)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((from (text-property-any (point-min) (point-max) 'easel-datum 0))
                (to (text-property-any (point-min) (point-max) 'easel-datum 3)))
            (set-mark from)
            (goto-char to)
            (easel-brush-region)
            (should (plist-get (plist-get (easel-view-state view) :params) :brush))
            (execute-kbd-macro "z")
            (execute-kbd-macro "n")
            (execute-kbd-macro (kbd "RET"))
            (should (plist-get (plist-get (easel-view-state view) :params) :pick))
            (easel-tty-test--should-agree (easel-tty-test--replay-on-gui easel-tty-test--points view))))
      (kill-buffer buffer))))

;;; xterm-mouse in a text buffer

(defun easel-tty-test--mouse (type pos)
  "A terminal mouse event of TYPE at buffer position POS."
  (list type (list (selected-window) pos '(0 . 0) 0 nil pos '(0 . 0) nil '(0 . 0) '(1 . 1))))

(ert-deftest easel-tty-xterm-mouse-drag-brushes-and-point-does-not-interfere ()
  (easel-tty-test--with ((view (easel-view-open easel-tty-test--points :id "tty"))
                         (buffer (easel-show view 'text)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let* ((from (text-property-any (point-min) (point-max) 'easel-datum 1))
                 (to (text-property-any (point-min) (point-max) 'easel-datum 4))
                 (run (lambda (cmd ev)
                        (let ((last-command-event ev))
                          (funcall cmd ev)
                          (easel-mode--post-command)))))
            (funcall run #'easel-mode-pointer (easel-tty-test--mouse 'mouse-movement from))
            (should (equal (plist-get (plist-get (easel-view-state view) :hover) :datum) 1))
            (funcall run #'easel-mode-down (easel-tty-test--mouse 'down-mouse-1 from))
            (funcall run #'easel-mode-pointer (easel-tty-test--mouse 'mouse-movement to))
            (funcall run #'easel-mode-up (easel-tty-test--mouse 'mouse-1 to))
            ;; Point stayed at the top: no pointermove from it broke the drag.
            (should (equal (mapcar (lambda (e) (plist-get e :type)) (reverse (easel-view-log view)))
                           '("pointermove" "pointerdown" "pointermove" "pointerup")))
            (let ((store (plist-get (plist-get (easel-view-state view) :params) :brush)))
              (should store)
              (should (easel-parity--same-number (aref (plist-get store :x) 0) 2))
              (should (easel-parity--same-number (aref (plist-get store :x) 1) 5)))
            ;; A key after the mouse keeps the mouse's hover (point did not
            ;; move), and n steps on from it.
            (funcall run #'easel-mode-pointer (easel-tty-test--mouse 'mouse-movement from))
            (execute-kbd-macro "+")
            (should (equal (plist-get (car (easel-view-log view)) :type) "key"))
            (should (equal (plist-get (plist-get (easel-view-state view) :hover) :datum) 1))
            (execute-kbd-macro "n")
            (should (equal (plist-get (plist-get (easel-view-state view) :hover) :datum) 2))
            (easel-tty-test--should-agree (easel-tty-test--replay-on-gui easel-tty-test--points view))))
      (kill-buffer buffer))))

(ert-deftest easel-tty-keymap-and-mouse-capability ()
  (should (eq (lookup-key easel-view-mode-map "n") #'easel-tty-next-datum))
  (should (eq (lookup-key easel-view-mode-map "p") #'easel-tty-previous-datum))
  (should (functionp (lookup-key easel-view-mode-map (kbd "ESC ESC"))))
  (dolist (term '("xterm-256color" "tmux-256color" "screen" "xterm-kitty" "alacritty"))
    (should (easel-tty-mouse-capable-p term)))
  (dolist (term '("dumb" "linux" "vt100"))
    (should-not (easel-tty-mouse-capable-p term)))
  ;; Batch never turns the global mode on.
  (let ((easel-tty-xterm-mouse t))
    (with-temp-buffer
      (easel-tty--enable-mouse)
      (should-not (bound-and-true-p xterm-mouse-mode)))))

(ert-deftest easel-tty-step-clamps-at-both-ends ()
  (easel-tty-test--with ((view (easel-view-open easel-tty-test--points :id "s")))
    (let ((scene (easel-view-scene view)))
      (should (equal (easel-tty-step-px scene nil 1) (easel-tty-test--px view 1 3)))
      (should (equal (easel-tty-step-px scene nil -1) (easel-tty-test--px view 5 6)))
      (easel-dispatch view (list :type "pointermove" :px (easel-tty-step-px scene nil -1)))
      (should (equal (easel-tty-step-px scene (plist-get (easel-view-state view) :hover) 3)
                     (easel-tty-test--px view 5 6)))
      (should (equal (easel-tty-step-px scene (plist-get (easel-view-state view) :hover) -2)
                     (easel-tty-test--px view 3 4))))))

(provide 'easel-tty-test)
;;; easel-tty-test.el ends here
