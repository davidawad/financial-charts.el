;;; easel-agent-health.el --- agent verbs bench and doctor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;   bench SOURCE [--data B] [--n N]   measured resolve, compile, render and hover ms
;;   doctor                            eager (:name :status :detail :remediation) rows
;;
;; bench measures in the Emacs it runs in (batch for bin/easel), so
;; its numbers are the Lisp half only; a GUI frame adds rasterization
;; (docs/design/engine-spikes.md).  doctor status is pass, fail or
;; skip; a skip names what is missing and never fails the envelope.

;;; Code:

(require 'easel-agent-core)
(require 'easel-agent-verbs)
(require 'easel-view)

;;; bench

(defun easel-agent--time (n fn)
  "Run FN N times; return (:mean MS :max MS)."
  (let ((total 0.0) (worst 0.0))
    (dotimes (_ n)
      (let ((start (float-time)))
        (funcall fn)
        (let ((ms (* 1000.0 (- (float-time) start))))
          (setq total (+ total ms) worst (max worst ms)))))
    (list :mean (easel-scene-round (/ total n)) :max (easel-scene-round worst))))

(defun easel-agent--hover-points (scene n)
  "N pointer positions sweeping across SCENE's first view."
  (let* ((b (plist-get (aref (plist-get scene :views) 0) :bounds))
         (y (+ (aref b 1) (/ (aref b 3) 2.0))))
    (cl-loop for i below n
             collect (vector (+ (aref b 0) (* (aref b 2) (/ (+ i 0.5) n))) y))))

(defun easel-agent--bench-hover (spec n)
  "Mean/max ms of N pointermove dispatches on a scratch view of SPEC."
  (let* ((easel-views (make-hash-table :test 'equal))
         (view (easel-view-open spec :id "bench"))
         (points (and (easel-view-interactive view)
                      (easel-agent--hover-points (easel-view-scene view) n))))
    (if (null points) :null
      (easel-agent--time n (lambda () (easel-dispatch view (list :type "pointermove" :px (pop points))))))))

(defun easel-agent-bench (pos opts)
  "Answer bench SOURCE (first of POS) under OPTS: latency per stage."
  (let* ((n (max 1 (or (easel-agent-arg-number opts :n) 10)))
         (src (easel-agent-resolve-source "bench" pos opts))
         (spec (plist-get src :spec))
         (svg-scene (easel-agent-compile spec opts "svg"))
         (text-scene (easel-agent-compile spec opts "text")))
    (easel-agent-ok
     (list :n n :emacs emacs-version :batch (if noninteractive t :false)
           :template (or (plist-get src :template) :null)
           :rows (length (plist-get (plist-get spec :data) :values))
           :items (apply #'+ (cl-loop for v across (plist-get svg-scene :views)
                                      append (cl-loop for m across (plist-get v :marks)
                                                      collect (length (plist-get m :items)))))
           :ms (list :resolve (if (plist-get src :template)
                                  (easel-agent--time n (lambda () (easel-agent-resolve-source "bench" pos opts)))
                                :null)
                     :compile-svg (easel-agent--time n (lambda () (easel-agent-compile spec opts "svg")))
                     :render-svg (easel-agent--time n (lambda () (easel-svg-render svg-scene)))
                     :compile-text (easel-agent--time n (lambda () (easel-agent-compile spec opts "text")))
                     :render-text (easel-agent--time n (lambda () (easel-text-render text-scene)))
                     :hover (easel-agent--bench-hover spec n)))
     (easel-agent-next "explain" src "--stage" "compile"))))

;;; doctor

(defun easel-agent--row (name status detail &optional remediation)
  "A doctor row: NAME, STATUS, DETAIL and REMEDIATION."
  (list :name name :status status :detail detail :remediation (or remediation :null)))

(defun easel-agent--template-row (name)
  "Doctor row for template NAME: its example resolves, compiles, renders."
  (condition-case err
      (let* ((spec (easel-resolve name (easel-template-example name)))
             (text (easel-text-render (easel-compile spec :target 'text))))
        (easel-agent--row (concat "template:" name) "pass"
                          (format "example renders (%d text lines)" (length (split-string text "\n")))))
    (error (let ((e (easel-error-plist err)))
             (easel-agent--row (concat "template:" name) "fail"
                               (format "%s: %s" (plist-get e :code) (plist-get e :message))
                               (easel-agent-cmd "check" name "--data"
                                                (or (ignore-errors (easel-template-example-file
                                                                    (easel-template-get name)))
                                                    "BINDINGS.json")))))))

(defun easel-agent-doctor-rows ()
  "Every health check, evaluated now."
  (append
   (list
    (if (version<= "29.1" emacs-version)
        (easel-agent--row "emacs" "pass" (format "GNU Emacs %s" emacs-version))
      (easel-agent--row "emacs" "fail" (format "GNU Emacs %s; easel needs 29.1" emacs-version)
                        "Install GNU Emacs 29.1 or newer"))
    (if (and (fboundp 'json-serialize) (fboundp 'json-parse-string))
        (easel-agent--row "json" "pass" "native JSON")
      (easel-agent--row "json" "fail" "this Emacs lacks native JSON" "Build Emacs with libjansson (29) or use 30+"))
    (let ((names (ignore-errors (easel-template-names))))
      (if names (easel-agent--row "templates" "pass"
                                  (format "%d: %s" (length names) (string-join names ", ")))
        (easel-agent--row "templates" "fail" (format "none in %s" (string-join easel-template-directories ", "))
                          "Point easel-template-directories at the repo's templates/"))))
   (mapcar #'easel-agent--template-row (ignore-errors (easel-template-names)))
   (list
    (if (file-exists-p easel-conformance-supported-file)
        (easel-agent--row "supported.json" "pass" easel-conformance-supported-file)
      (easel-agent--row "supported.json" "skip" "absent: every recognised feature counts as native"
                        "Generate it with (easel-conformance-generate)"))
    (cond (noninteractive (easel-agent--row "svg-display" "skip" "batch: no frame; the text backend is available"))
          ((image-type-available-p 'svg) (easel-agent--row "svg-display" "pass" "SVG images display"))
          (t (easel-agent--row "svg-display" "skip" "this Emacs cannot display SVG; views use the text backend"
                               "Build Emacs with librsvg for SVG views")))
    (if (easel-chart-available-p)
        (easel-agent--row "bin/chart" "pass" (executable-find easel-chart-program))
      (easel-agent--row "bin/chart" "skip" (easel-chart-missing-reason)
                        "Put bin/chart on PATH for export, static fallback and conformance"))
    (if (executable-find easel-chart-rsvg-program)
        (easel-agent--row "rsvg-convert" "pass" (executable-find easel-chart-rsvg-program))
      (easel-agent--row "rsvg-convert" "skip" (format "%s not on PATH" easel-chart-rsvg-program)
                        "Install librsvg2-bin for conformance diffs")))))

(defun easel-agent-doctor (_pos _opts)
  "Answer doctor: health rows; fail when any row fails."
  (let* ((rows (easel-agent-doctor-rows))
         (failed (seq-filter (lambda (r) (equal (plist-get r :status) "fail")) rows))
         (data (list :rows (vconcat rows)
                     :pass (seq-count (lambda (r) (equal (plist-get r :status) "pass")) rows)
                     :fail (length failed)
                     :skip (seq-count (lambda (r) (equal (plist-get r :status) "skip")) rows))))
    (if failed
        (apply #'easel-agent-fail "ENGINE_FAILED"
               (list :message (format "%d health check(s) failed: %s" (length failed)
                                      (mapconcat (lambda (r) (plist-get r :name)) failed ", ")))
               data
               (append (seq-filter (lambda (c) (and (stringp c) (string-prefix-p easel-agent-shell-program c)))
                                   (mapcar (lambda (r) (plist-get r :remediation)) failed))
                       (list (easel-agent-cmd "doctor"))))
      (easel-agent-ok data (easel-agent-cmd "describe")))))

(easel-agent-register-verb
 "bench" #'easel-agent-bench :doc "Measured resolve, compile, render and hover latency (ms)"
 :usage "bench SOURCE [--data B] [--n N] [--width W --height H]"
 :options (cons :n easel-agent--source-options))
(easel-agent-register-verb
 "doctor" #'easel-agent-doctor :doc "Eager health rows (:name :status pass|fail|skip :detail :remediation)"
 :usage "doctor")

(provide 'easel-agent-health)
;;; easel-agent-health.el ends here
