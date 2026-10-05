;;; easel-conformance-oracle.el --- reference images and the image oracle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; bin/chart is the oracle, but it is not installed everywhere, so its
;; output is committed: test/conformance/ref/NAME.png is `bin/chart
;; build' of NAME.vl.json, and ref/manifest.json records, per spec, the
;; hash of the spec it was built from and of the PNG.  The spec hash is
;; `easel-content-hash' of the spec without "usermeta" (which Vega-Lite
;; ignores), so thresholds and their justifications can change without
;; invalidating a reference.
;;
;; The oracle rasterizes the native SVG (rsvg-convert) and compares it
;; with the reference using `easel-png-compare': canvases are aligned
;; and padded, the size delta is reported and bounded separately, and
;; the spec passes when the differing-pixel ratio is within its
;; usermeta.easel.threshold.  A reference whose manifest hash no longer
;; matches its spec is stale and fails.  When bin/chart is on PATH the
;; reference is rebuilt from the spec instead of read from ref/, and
;; `easel-conformance-update-refs' rewrites ref/ and the manifest.
;;
;; Vega draws "time" scales and timeUnits in local time, so references
;; depend on the zone they were built in.  The manifest records it
;; ("tz"); native SVG is compiled with `easel-time-zone' bound to it and
;; bin/chart rebuilds run with TZ set to it.

;;; Code:

(require 'easel-core)
(require 'easel-chart)
(require 'easel-png)
(require 'easel-time)

(defvar easel-conformance-directory)
(declare-function easel-conformance-gallery "easel-conformance")

(defvar easel-conformance-size-tolerance 8
  "Default largest canvas size difference, in pixels per axis, that passes.
Vega pads the canvas for marks and labels overhanging the plot (1-7px
in the gallery), which native layout reproduces only approximately.")

(defun easel-conformance-ref-directory ()
  "Directory of the committed bin/chart reference images."
  (expand-file-name "ref" easel-conformance-directory))

(defun easel-conformance-manifest-file ()
  "The reference manifest."
  (expand-file-name "manifest.json" (easel-conformance-ref-directory)))

(defun easel-conformance-manifest ()
  "Parsed ref/manifest.json, or nil when absent."
  (let ((file (easel-conformance-manifest-file)))
    (and (file-exists-p file) (easel-json-read-file file))))

(defconst easel-conformance-default-zone "UTC"
  "Zone for references when the manifest names none.")

(defun easel-conformance-ref-zone ()
  "The time zone the committed references were built in."
  (or (plist-get (easel-conformance-manifest) :tz) easel-conformance-default-zone))

(defun easel-conformance-spec-hash (spec)
  "Hex sha256 identifying what bin/chart renders for SPEC (usermeta excluded)."
  (string-remove-prefix "sha256:" (easel-content-hash (easel--plist-without spec :usermeta))))

(defun easel-conformance--easel-meta (entry key)
  "usermeta.easel KEY of gallery ENTRY's spec."
  (plist-get (plist-get (plist-get (plist-get entry :spec) :usermeta) :easel) key))

(defun easel-conformance--file-hash (file)
  "Hex sha256 of FILE's bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun easel-conformance-ref-problem (entry)
  "Why ENTRY's committed reference cannot be trusted, or nil."
  (let* ((name (plist-get entry :name))
         (file (expand-file-name (concat name ".png") (easel-conformance-ref-directory)))
         (ref (plist-get (plist-get (easel-conformance-manifest) :refs) (easel-key name)))
         (fix "rebuild with bin/chart: M-x easel-conformance-update-refs"))
    (cond ((not (file-exists-p file)) (format "STALE_REF: ref/%s.png is missing; %s" name fix))
          ((null ref) (format "STALE_REF: ref/manifest.json has no entry for %s; %s" name fix))
          ((not (equal (plist-get ref :spec_sha256) (easel-conformance-spec-hash (plist-get entry :spec))))
           (format "STALE_REF: ref/%s.png was built from an older %s.vl.json; %s" name name fix))
          ((not (equal (plist-get ref :png_sha256) (easel-conformance--file-hash file)))
           (format "STALE_REF: ref/%s.png does not match its manifest hash; %s" name fix)))))

(defun easel-conformance--write-png (spec file)
  "Build SPEC with bin/chart into PNG FILE, in the references' time zone."
  (let ((process-environment (cons (concat "TZ=" (easel-conformance-ref-zone)) process-environment)))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (easel-chart-build spec "png")))))

(defun easel-conformance--judge (entry mine ref source)
  "Compare native PNG MINE with reference PNG REF for ENTRY; SOURCE names REF."
  (let* ((cmp (easel-png-compare (easel-png-read mine) (easel-png-read ref)))
         (threshold (plist-get entry :threshold))
         (tolerance (or (easel-conformance--easel-meta entry :size_tolerance) easel-conformance-size-tolerance))
         (delta (plist-get cmp :size-delta))
         (size-ok (and (<= (abs (aref delta 0)) tolerance) (<= (abs (aref delta 1)) tolerance)))
         (ratio-ok (<= (plist-get cmp :ratio) threshold)))
    (append
     (list :status (if (and size-ok ratio-ok) "pass" "fail")
           :detail (format "ratio %.4f %s %s; size %+d,%+d px (native %dx%d, %s %dx%d) %s %d; offset %d,%d"
                           (plist-get cmp :ratio) (if ratio-ok "<=" ">") threshold
                           (aref delta 0) (aref delta 1)
                           (aref (plist-get cmp :native) 0) (aref (plist-get cmp :native) 1)
                           source (aref (plist-get cmp :reference) 0) (aref (plist-get cmp :reference) 1)
                           (if size-ok "within" "beyond") tolerance
                           (aref (plist-get cmp :offset) 0) (aref (plist-get cmp :offset) 1))
           :threshold threshold :source source)
     cmp)))

(defun easel-conformance-oracle (entry native)
  "Compare NATIVE's SVG for gallery ENTRY with bin/chart's image.
Return (:status :detail ...): STATUS is \"pass\", \"fail\" or
\"unverified\" (when the native SVG cannot be rasterized here)."
  (cond
   ((not (zlib-available-p)) (list :status "unverified" :detail "this Emacs lacks zlib, needed to decode PNGs"))
   ((not (executable-find easel-chart-rsvg-program))
    (list :status "unverified"
          :detail (format "%s is not on PATH; needed to rasterize native SVG" easel-chart-rsvg-program)))
   ((not (plist-get native :svg)) (list :status "fail" :detail "native rendering failed"))
   (t
    (let ((stale (easel-conformance-ref-problem entry))
          (mine (make-temp-file "easel-native" nil ".png"))
          (fresh (and (easel-chart-available-p) (make-temp-file "easel-ref" nil ".png"))))
      (unwind-protect
          (condition-case err
              (progn
                (easel-chart-rasterize (plist-get native :svg) mine)
                (when fresh (easel-conformance--write-png (plist-get entry :spec) fresh))
                (cond
                 (fresh (let ((r (easel-conformance--judge entry mine fresh "bin/chart")))
                          (if stale (append (list :status "fail" :detail (concat stale "; " (plist-get r :detail))) r)
                            r)))
                 ((and stale (string-match-p "missing\\|no entry" stale)) (list :status "fail" :detail stale))
                 (t (let ((r (easel-conformance--judge
                              entry mine (expand-file-name (concat (plist-get entry :name) ".png")
                                                           (easel-conformance-ref-directory))
                              "ref")))
                      (if stale (append (list :status "fail" :detail (concat stale "; " (plist-get r :detail))) r)
                        r)))))
            (easel-error (list :status "fail" :detail (plist-get (easel-error-plist err) :message))))
        (delete-file mine)
        (when fresh (delete-file fresh)))))))

(defun easel-conformance-update-refs (&optional entries)
  "Rebuild ref/NAME.png with bin/chart for ENTRIES (default: the gallery).
Rewrite ref/manifest.json.  Signals NOT_FOUND without bin/chart."
  (interactive)
  (let ((entries (or entries (easel-conformance-gallery)))
        (zone (easel-conformance-ref-zone))
        (refs (plist-get (easel-conformance-manifest) :refs)))
    (dolist (entry entries)
      (let ((file (expand-file-name (concat (plist-get entry :name) ".png") (easel-conformance-ref-directory))))
        (easel-conformance--write-png (plist-get entry :spec) file)
        (setq refs (easel-plist-put refs (easel-key (plist-get entry :name))
                                    (list :png_sha256 (easel-conformance--file-hash file)
                                          :spec_sha256 (easel-conformance-spec-hash (plist-get entry :spec)))))))
    (with-temp-file (easel-conformance-manifest-file)
      (insert (easel-json-pretty
               (list :generator "bin/chart build SPEC --out ref/NAME.png (vl-convert, default theme), via easel-conformance-update-refs"
                     :spec_hash "easel-conformance-spec-hash: sha256 of the spec's canonical JSON without usermeta"
                     :tz zone
                     :refs (easel-json-canonical refs))))
      (insert "\n"))))


(provide 'easel-conformance-oracle)
;;; easel-conformance-oracle.el ends here
