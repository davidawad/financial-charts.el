;;; easel-chart.el --- the static door: bin/chart build and diff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; bin/chart (chart-runtime) renders any resolved Vega-Lite
;; spec statically and compares images.  easel uses it twice: as the
;; conformance oracle, and to display specs whose features the native
;; engine does not support.  The command lines are variables because
;; they are an assumption here: adjust `easel-chart-build-args' and
;; `easel-chart-diff-args' if bin/chart's flags differ.

;;; Code:

(require 'easel-core)

(defvar easel-chart-program "chart"
  "The bin/chart executable (name on PATH or absolute file).")

(defvar easel-chart-build-args
  (lambda (spec-file out-file) (list "build" spec-file "--out" out-file "--force"))
  "Function (SPEC-FILE OUT-FILE) -> argument list rendering a spec statically.
The output format follows OUT-FILE's extension (.svg or .png).  OUT-FILE
already exists (`make-temp-file' creates it), hence --force.")

(defvar easel-chart-diff-args
  (lambda (a b threshold) (list "diff" a b "--threshold" (format "%s" threshold)))
  "Function (A B THRESHOLD) -> argument list comparing two PNGs.
Exit status 0 means the images agree within THRESHOLD.")

(defvar easel-chart-rsvg-program "rsvg-convert"
  "Rasterizer turning native SVG into PNG for oracle diffs.")

(defun easel-chart-available-p ()
  "Non-nil when bin/chart can be run."
  (and (executable-find easel-chart-program) t))

(defun easel-chart-missing-reason ()
  "Why the static door is unavailable, or nil."
  (cond ((not (executable-find easel-chart-program))
         (format "bin/chart (%s) is not on PATH; install bin/chart" easel-chart-program))
        (t nil)))

(defun easel-chart--run (args)
  "Run bin/chart with ARGS; return (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((exit (apply #'call-process easel-chart-program nil t nil args)))
      (cons exit (buffer-string)))))

(defun easel-chart-build (spec format)
  "Render resolved SPEC statically with bin/chart as FORMAT (\"svg\" or \"png\").
Return the image data as a unibyte string.  Signals NOT_FOUND when
bin/chart is absent and ENGINE_FAILED when it fails."
  (when-let* ((reason (easel-chart-missing-reason)))
    (easel-signal "NOT_FOUND" reason :program easel-chart-program))
  (let ((in (make-temp-file "easel-spec" nil ".vl.json"))
        (out (make-temp-file "easel-static" nil (concat "." format))))
    (unwind-protect
        (progn
          (with-temp-file in (insert (easel-json-encode spec)))
          (let ((result (easel-chart--run (funcall easel-chart-build-args in out))))
            (unless (and (zerop (car result)) (> (file-attribute-size (file-attributes out)) 0))
              (easel-signal "ENGINE_FAILED" (format "bin/chart build failed: %s" (string-trim (cdr result)))
                            :exit (car result)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally out)
              (buffer-string))))
      (delete-file in)
      (delete-file out))))

(defun easel-chart-diff (png-a png-b threshold)
  "Compare PNG files PNG-A and PNG-B with bin/chart; return (PASS . OUTPUT)."
  (let ((result (easel-chart--run (funcall easel-chart-diff-args png-a png-b threshold))))
    (cons (zerop (car result)) (string-trim (cdr result)))))

(defun easel-chart-rasterize (svg png-file)
  "Rasterize SVG (a string) to PNG-FILE with `easel-chart-rsvg-program'."
  (let ((in (make-temp-file "easel-native" nil ".svg" svg)))
    (unwind-protect
        (unless (zerop (call-process easel-chart-rsvg-program nil nil nil "-o" png-file in))
          (easel-signal "ENGINE_FAILED" (format "%s failed on native SVG" easel-chart-rsvg-program)))
      (delete-file in))))

(provide 'easel-chart)
;;; easel-chart.el ends here
