;;; easel-conformance.el --- the gallery, supported.json and the oracle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A feature is supported only when a conformance spec proves it.  The
;; gallery is test/conformance/NAME.vl.json, pure Vega-Lite, each with
;; usermeta.easel.threshold.  For every spec:
;;
;;   native  compile for both targets, render SVG and text; the text must
;;           equal test/conformance/NAME.txt exactly
;;   oracle  native SVG -> PNG (rsvg-convert) vs bin/chart's PNG, compared
;;           with bin/chart diff at the spec's threshold (only when
;;           bin/chart is installed; otherwise "unverified")
;;
;; `easel-conformance-generate' writes supported.json next to this file
;; from the passing gallery; `easel-spec-check', compile and describe
;; read it through `easel-spec-supported-function'.

;;; Code:

(require 'easel-core)
(require 'easel-spec)
(require 'easel-template)
(require 'easel-compile)
(require 'easel-svg)
(require 'easel-text)
(require 'easel-chart)

(defvar easel-conformance-directory
  (expand-file-name "test/conformance" easel-template--root)
  "Directory holding the conformance gallery.")

(defconst easel-conformance-supported-file
  (expand-file-name "supported.json"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "The generated list of supported features, shipped with the engine.")

(defconst easel-conformance-text-size '(:cols 60 :rows 16)
  "Size of the exact text goldens.")

(defconst easel-conformance-extensions
  '(:transform/x-easel "Domain transforms are materialized by resolve (ERT: easel-resolve-materializes-domain-transforms)."
    :mark/slot "Template slot placeholders are substituted by resolve (ERT: easel-template-enum-and-array-items).")
  "Features outside Vega-Lite proper, proven by ERT rather than the gallery.")

(defun easel-conformance-gallery ()
  "The gallery as plists (:name :file :spec :threshold), sorted by name."
  (mapcar (lambda (file)
            (let ((spec (easel-json-read-file file)))
              (list :name (string-remove-suffix ".vl.json" (file-name-nondirectory file))
                    :file file :spec spec
                    :threshold (or (plist-get (plist-get (plist-get spec :usermeta) :easel) :threshold) 0.05))))
          (directory-files easel-conformance-directory t "\\.vl\\.json\\'")))

(defun easel-conformance-text-file (entry)
  "The text golden of gallery ENTRY."
  (expand-file-name (concat (plist-get entry :name) ".txt") easel-conformance-directory))

(defun easel-conformance-native (entry)
  "Compile and render gallery ENTRY natively, unrestricted by supported.json.
Return (:name :ok :features :svg :text :error)."
  (let ((easel-spec-supported-function nil)
        (spec (plist-get entry :spec)))
    (condition-case err
        (let* ((svg (easel-svg-render (easel-compile spec)))
               (text (concat (substring-no-properties
                              (easel-text-render (easel-compile spec :target 'text :size easel-conformance-text-size)))
                             "\n"))
               (golden (easel-conformance-text-file entry))
               (matches (and (file-exists-p golden)
                             (equal text (with-temp-buffer (insert-file-contents golden) (buffer-string))))))
          (list :name (plist-get entry :name) :ok matches
                :features (delete-dups (mapcar (lambda (f) (plist-get f :feature))
                                               (seq-remove (lambda (f) (plist-get f :invalid))
                                                           (easel-spec-features (easel-spec-parse spec)))))
                :svg svg :text text
                :error (unless matches (list :code "GOLDEN_MISMATCH" :message "text differs from its golden"))))
      (easel-error (list :name (plist-get entry :name) :ok nil :features nil :error (easel-error-plist err))))))

(defun easel-conformance-oracle (entry native)
  "Compare NATIVE's SVG for ENTRY with bin/chart.  Return (:status :detail).
STATUS is \"pass\", \"fail\" or \"unverified\" (with the reason)."
  (cond
   ((easel-chart-missing-reason) (list :status "unverified" :detail (easel-chart-missing-reason)))
   ((not (executable-find easel-chart-rsvg-program))
    (list :status "unverified" :detail (format "%s is not on PATH" easel-chart-rsvg-program)))
   ((not (plist-get native :ok)) (list :status "fail" :detail "native rendering failed"))
   (t (let ((mine (make-temp-file "easel-native" nil ".png"))
            (ref (make-temp-file "easel-ref" nil ".png")))
        (unwind-protect
            (condition-case err
                (progn
                  (easel-chart-rasterize (plist-get native :svg) mine)
                  (with-temp-file ref
                    (set-buffer-multibyte nil)
                    (insert (easel-chart-build (plist-get entry :spec) "png")))
                  (let ((result (easel-chart-diff mine ref (plist-get entry :threshold))))
                    (list :status (if (car result) "pass" "fail") :detail (cdr result))))
              (easel-error (list :status "fail" :detail (plist-get (easel-error-plist err) :message))))
          (delete-file mine) (delete-file ref))))))

(defun easel-conformance-run (&optional oracle)
  "Run the gallery natively (and against bin/chart when ORACLE is non-nil)."
  (mapcar (lambda (entry)
            (let ((native (easel-conformance-native entry)))
              (append native
                      (list :oracle (if oracle (easel-conformance-oracle entry native)
                                      (list :status "unverified" :detail "oracle not run"))))))
          (easel-conformance-gallery)))

(defun easel-conformance-supported-data (results)
  "supported.json content for gallery RESULTS."
  (let ((features (make-hash-table :test 'equal)))
    (dolist (r results)
      (when (and (plist-get r :ok) (not (equal (plist-get (plist-get r :oracle) :status) "fail")))
        (dolist (f (plist-get r :features))
          (puthash f (cons r (gethash f features)) features))))
    (list :contract "easel-supported/v1"
          :vega-lite easel-spec-vega-lite-version
          :gallery "test/conformance"
          :features
          (let (out)
            (dolist (f (sort (hash-table-keys features) #'string<))
              (let ((rs (reverse (gethash f features))))
                (setq out (append out (list (easel-key f)
                                            (list :specs (vconcat (mapcar (lambda (r) (plist-get r :name)) rs))
                                                  :oracle (if (seq-some (lambda (r) (equal (plist-get (plist-get r :oracle) :status) "pass")) rs)
                                                              "verified" "unverified")))))))
            out)
          :extensions easel-conformance-extensions)))

(defun easel-conformance-generate (&optional oracle file)
  "Run the gallery (with ORACLE when non-nil) and write supported.json to FILE."
  (let ((data (easel-conformance-supported-data (easel-conformance-run oracle))))
    (with-temp-file (or file easel-conformance-supported-file)
      (insert (easel-json-pretty data)))
    data))

;;; Reading supported.json

(defvar easel-conformance--cache nil "(MTIME . FEATURES) of the last read.")

(defun easel-conformance-supported ()
  "Parsed supported.json, or nil when it does not exist."
  (when (file-exists-p easel-conformance-supported-file)
    (easel-json-read-file easel-conformance-supported-file)))

(defun easel-conformance-supported-features ()
  "Feature ids supported.json proves, plus extensions; t when the file is absent."
  (let ((mtime (file-attribute-modification-time (file-attributes easel-conformance-supported-file))))
    (if (null mtime) t
      (unless (equal (car easel-conformance--cache) mtime)
        (let ((data (easel-conformance-supported)))
          (setq easel-conformance--cache
                (cons mtime (mapcar #'easel-key-name
                                    (append (easel-plist-keys (plist-get data :features))
                                            (easel-plist-keys (plist-get data :extensions))))))))
      (cdr easel-conformance--cache))))

(setq easel-spec-supported-function #'easel-conformance-supported-features)

(defun easel-conformance-describe ()
  "The describe section for supported features."
  (let ((data (easel-conformance-supported)))
    (list :supported (if data
                         (list :file easel-conformance-supported-file
                               :features (vconcat (mapcar #'easel-key-name (easel-plist-keys (plist-get data :features))))
                               :verified (vconcat (cl-loop for (k v) on (plist-get data :features) by #'cddr
                                                           when (equal (plist-get v :oracle) "verified")
                                                           collect (easel-key-name k)))
                               :extensions (vconcat (mapcar #'easel-key-name (easel-plist-keys (plist-get data :extensions))))
                               :oracle (or (easel-chart-missing-reason) "bin/chart available"))
                       :null))))

(with-eval-after-load 'easel-describe
  (add-hook 'easel-describe-functions #'easel-conformance-describe))

(provide 'easel-conformance)
;;; easel-conformance.el ends here
