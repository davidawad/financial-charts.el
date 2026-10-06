;;; financial-chart-eas-book-bench.el --- frame cost of live order books -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; What one live order-book frame costs (fc-gbo.4), so the frame cap
;; can be chosen from measurements: for each LEVELS per side, template
;; and backend, a view is opened on a synthetic book and fed FRAMES
;; batches of DELTAS updates.  Each frame is timed in three parts:
;;
;;   apply   the delta batch applied to the book
;;   push    rows built, pushed and the scene recompiled by eas
;;   draw    the scene drawn to a string (text grid or SVG)
;;
;; The clock is stepped past the cap between frames, so every batch is
;; drawn.  `financial-chart-book-bench' returns rows of milliseconds
;; (mean and worst); `financial-chart-book-bench-report' prints them as
;; a Markdown table.  Results are recorded in docs/design/order-book.md.
;;
;;   emacs -Q --batch -L $EAS/src -L src/... -l financial-chart-eas-book-bench \
;;     -f financial-chart-book-bench-report

;;; Code:

(require 'cl-lib)
(require 'financial-chart-eas-book)

(defun financial-chart-book-bench-snapshot (levels)
  "A book of LEVELS bids and asks a cent apart around 100."
  (list :bids (vconcat (cl-loop for i below levels
                                collect (vector (- 99.99 (* 0.01 i)) (+ 1 (% (* 7 i) 13)))))
        :asks (vconcat (cl-loop for i below levels
                                collect (vector (+ 100.01 (* 0.01 i)) (+ 1 (% (* 5 i) 11)))))))

(defun financial-chart-book-bench--deltas (levels count seed)
  "COUNT size updates within the nearest LEVELS of each side, from SEED."
  (let ((state seed) out)
    (dotimes (_ count)
      (setq state (% (+ (* state 1103515245) 12345) 2147483648))
      (let* ((bid (zerop (% state 2)))
             (i (% (/ state 2) levels)))
        (push (list :op "update" :side (if bid "bid" "ask")
                    :price (if bid (- 99.99 (* 0.01 i)) (+ 100.01 (* 0.01 i)))
                    :size (+ 1 (% (/ state 7) 20)))
              out)))
    (vconcat out)))

(defun financial-chart-book-bench--size (backend levels)
  "The view size for BACKEND with LEVELS per side."
  (if (eq backend 'text) '(:cols 100 :rows 40)
    (cons 640 (max 360 (* 6 (1+ (* 2 levels)))))))

(cl-defun financial-chart-book-bench-one (levels backend template &key (frames 20) (deltas 10))
  "Time FRAMES frames of DELTAS updates on a LEVELS book in TEMPLATE on BACKEND.
Return (:levels :backend :template :rows :apply :push :draw :frame :worst
:fps), times in milliseconds per frame."
  (let* ((clock 0.0)
         (eas-stream-clock (lambda () clock))
         (eas-stream-use-timers nil)
         (view (financial-chart-book-open (financial-chart-book-bench-snapshot levels)
                                          :template template :levels levels :flash 0.5
                                          :target backend
                                          :size (financial-chart-book-bench--size backend levels)))
         (live (financial-chart-book--live view))
         (apply 0.0) (push 0.0) (draw 0.0) (worst 0.0))
    (unwind-protect
        (dotimes (frame (1+ frames))
          (setq clock (+ clock 1.0))
          (let* ((batch (financial-chart-book-bench--deltas levels deltas (1+ frame)))
                 (t0 (float-time))
                 (_ (financial-chart-book-apply (plist-get live :book) batch clock))
                 (t1 (float-time))
                 (_ (financial-chart-book--offer live clock))
                 (t2 (float-time))
                 (scene (eas-view-scene (eas-view-get view)))
                 (_ (if (eq backend 'text) (eas-text-render scene) (eas-svg-render scene)))
                 (t3 (float-time)))
            ;; Frame 0 warms caches and is not counted.
            (when (> frame 0)
              (cl-incf apply (- t1 t0)) (cl-incf push (- t2 t1)) (cl-incf draw (- t3 t2))
              (setq worst (max worst (- t3 t0))))))
      (financial-chart-book-close view))
    (let ((ms (lambda (s) (/ (round (* 10000 (/ s frames))) 10.0)))
          (frame (/ (+ apply push draw) frames)))
      (list :levels levels :backend (symbol-name backend) :template template
            :rows (1+ (* 2 levels))
            :apply (funcall ms apply) :push (funcall ms push) :draw (funcall ms draw)
            :frame (/ (round (* 10000 frame)) 10.0)
            :worst (/ (round (* 10000 worst)) 10.0)
            :fps (if (> frame 0) (floor (/ 1.0 frame)) :null)))))

(cl-defun financial-chart-book-bench (&key (levels '(50 100 200)) (backends '(svg text))
                                           (templates financial-chart-book-templates)
                                           (frames 20) (deltas 10))
  "`financial-chart-book-bench-one' over LEVELS, BACKENDS and TEMPLATES.
FRAMES and DELTAS are passed through."
  (cl-loop for n in levels
           nconc (cl-loop for backend in backends
                          nconc (cl-loop for template in templates
                                         collect (financial-chart-book-bench-one
                                                  n backend template :frames frames :deltas deltas)))))

(cl-defun financial-chart-book-bench-soak (levels backend template &key (seconds 3.0) (hz 50) (deltas 5))
  "Feed a LEVELS book in TEMPLATE on BACKEND HZ delta batches a second.
For SECONDS with real timers, as a live feed would.  A 10 ms probe
timer measures how late Emacs runs it: the worst lateness is how long
the book can keep Emacs from other work.  Return (:levels :backend
:template :max-fps :pushes :frames :fps :busy :probe-worst), busy as
the fraction of wall time spent in frames and probe-worst in ms."
  (let* ((view (financial-chart-book-open (financial-chart-book-bench-snapshot levels)
                                          :template template :levels levels :target backend
                                          :size (financial-chart-book-bench--size backend levels)))
         (fps (plist-get (eas-stream-inspect view) :max-fps))
         (busy 0.0) (pushes 0) (seed 1) (worst 0.0)
         (draw (lambda (v &rest _)
                 (when (eq v (eas-view-get view))
                   (let ((t0 (float-time)) (scene (eas-view-scene v)))
                     (if (eq backend 'text) (eas-text-render scene) (eas-svg-render scene))
                     (cl-incf busy (- (float-time) t0))))))
         (feeding nil)
         ;; Frames a timer takes (not inside a feed) are timed here.
         (timed (lambda (orig v event &rest args)
                  (if (and (not feeding) (eq (eas-view-get v) (eas-view-get view))
                           (equal (plist-get event :type) "push"))
                      (let ((t0 (float-time)))
                        (prog1 (apply orig v event args)
                          (cl-incf busy (- (float-time) t0))))
                    (apply orig v event args))))
         (feeder (run-at-time 0 (/ 1.0 hz)
                              (lambda ()
                                (cl-incf seed) (cl-incf pushes)
                                (let ((t0 (float-time)))
                                  (setq feeding t)
                                  (unwind-protect
                                      (financial-chart-book-push
                                       view (financial-chart-book-bench--deltas levels deltas seed))
                                    (setq feeding nil))
                                  (cl-incf busy (- (float-time) t0))))))
         (last (float-time))
         (probe (run-at-time 0 0.01 (lambda ()
                                      (let ((now (float-time)))
                                        (setq worst (max worst (- now last 0.01)) last now)))))
         (start (float-time)))
    (advice-add 'eas-dispatch :around timed)
    (add-hook 'eas-view-changed-functions draw)
    (unwind-protect
        (while (< (- (float-time) start) seconds)
          (accept-process-output nil 0.005))
      (cancel-timer feeder) (cancel-timer probe)
      (advice-remove 'eas-dispatch timed)
      (remove-hook 'eas-view-changed-functions draw))
    (let* ((wall (- (float-time) start))
           (frames (plist-get (eas-stream-inspect view) :frames)))
      (financial-chart-book-close view)
      (list :levels levels :backend (symbol-name backend) :template template :max-fps fps
            :pushes pushes :frames frames :fps (/ (round (* 10 (/ frames wall))) 10.0)
            :busy (/ (round (* 100 (/ busy wall))) 100.0)
            :probe-worst (round (* 1000 worst))))))

(defun financial-chart-book-bench-soak-report ()
  "Print `financial-chart-book-bench-soak' over 50/100/200 levels as Markdown."
  (princ "| levels/side | backend | template | cap fps | delta batches | frames | fps | busy | worst probe delay ms |\n")
  (princ "|---|---|---|---|---|---|---|---|---|\n")
  (dolist (n '(50 100 200))
    (dolist (backend '(svg text))
      (dolist (template financial-chart-book-templates)
        (let ((r (financial-chart-book-bench-soak n backend template)))
          (princ (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
                         n backend template (plist-get r :max-fps) (plist-get r :pushes)
                         (plist-get r :frames) (plist-get r :fps) (plist-get r :busy)
                         (plist-get r :probe-worst))))))))

(defun financial-chart-book-bench-report ()
  "Print `financial-chart-book-bench' as a Markdown table."
  (garbage-collect)
  (princ "| levels/side | rows | backend | template | apply ms | push ms | draw ms | frame ms | worst ms | max fps |\n")
  (princ "|---|---|---|---|---|---|---|---|---|---|\n")
  (dolist (r (financial-chart-book-bench))
    (princ (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
                   (plist-get r :levels) (plist-get r :rows) (plist-get r :backend)
                   (plist-get r :template) (plist-get r :apply) (plist-get r :push)
                   (plist-get r :draw) (plist-get r :frame) (plist-get r :worst) (plist-get r :fps)))))

(provide 'financial-chart-eas-book-bench)
;;; financial-chart-eas-book-bench.el ends here
