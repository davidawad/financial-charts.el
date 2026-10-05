;;; easel-conformance-test.el --- conformance gallery, references, supported.json, fallback -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-conformance-test-unproven '("encoding/order")
  "Recognised features no gallery spec proves yet (so they fall back).")

(defun easel-conformance-test--rasterizer-p ()
  "Non-nil when native SVG can be rasterized and decoded here."
  (and (executable-find easel-chart-rsvg-program) (zlib-available-p)))

(defvar easel-conformance-test--results nil
  "Gallery results of this session, with the oracle when it can run.")

(defun easel-conformance-test--results ()
  "Run the gallery once per session (with the oracle when rsvg-convert exists)."
  (or easel-conformance-test--results
      (setq easel-conformance-test--results
            (easel-conformance-run (easel-conformance-test--rasterizer-p)))))

(defun easel-conformance-test--text (entry)
  "ENTRY's text rendering at the golden size."
  (let ((easel-spec-supported-function nil))
    (concat (substring-no-properties
             (easel-text-render (easel-compile (plist-get entry :spec) :target 'text
                                               :size easel-conformance-text-size)))
            "\n")))

(defun easel-conformance-test--write-text-goldens ()
  "Rewrite every gallery text golden (EASEL_UPDATE_GOLDEN)."
  (dolist (entry (easel-conformance-gallery))
    (with-temp-file (easel-conformance-text-file entry)
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert (easel-conformance-test--text entry)))))

(ert-deftest easel-conformance-text-goldens ()
  "Every gallery spec compiles natively and matches its exact text golden."
  (if (getenv "EASEL_UPDATE_GOLDEN")
      (easel-conformance-test--write-text-goldens)
    (dolist (entry (easel-conformance-gallery))
      (should (equal (cons (plist-get entry :name) (easel-conformance-test--text entry))
                     (cons (plist-get entry :name)
                           (with-temp-buffer (insert-file-contents (easel-conformance-text-file entry))
                                             (buffer-string))))))))

(ert-deftest easel-conformance-supported-json-is-current ()
  "supported.json lists exactly the features the passing gallery proves.
Its oracle verdicts are checked too wherever the oracle can run."
  (when (getenv "EASEL_UPDATE_GOLDEN")
    ;; supported.json counts only specs that match their text golden.
    (easel-conformance-test--write-text-goldens)
    (setq easel-conformance-test--results nil))
  (let* ((results (easel-conformance-test--results))
         (generated (easel-conformance-supported-data results))
         (pairs (lambda (data key) (cl-loop for (k v) on (plist-get data :features) by #'cddr
                                            collect (cons k (plist-get v key))))))
    (should (seq-every-p (lambda (r) (plist-get r :ok)) results))
    (if (getenv "EASEL_UPDATE_GOLDEN")
        (with-temp-file easel-conformance-supported-file (insert (easel-json-pretty generated)))
      (let ((committed (easel-conformance-supported)))
        (should committed)
        (should (equal (funcall pairs generated :specs) (funcall pairs committed :specs)))
        (when (easel-conformance-test--rasterizer-p)
          (should (equal (funcall pairs generated :oracle) (funcall pairs committed :oracle))))))))

(ert-deftest easel-conformance-every-native-feature-is-proven ()
  "Each mark, channel, scale type, transform and param form has a gallery spec."
  (let ((supported (easel-conformance-supported-features))
        (vocabulary (append (mapcar (lambda (m) (concat "mark/" m)) easel-spec--marks)
                            (mapcar (lambda (c) (concat "encoding/" (easel-key-name c))) easel-spec--channels)
                            (mapcar (lambda (s) (concat "scale/" s)) easel-spec--scale-types)
                            (mapcar (lambda (tr) (concat "transform/" (easel-key-name tr)))
                                    (remq :x-easel:transform easel-spec--transforms))
                            '("composition/layer" "composition/vconcat" "composition/hconcat"
                              "param/point" "param/interval" "param/value" "bind/scales" "bind/legend"
                              "encoding/condition" "encoding/aggregate" "encoding/bin" "encoding/timeUnit"))))
    (should (listp supported))
    (should (equal (seq-remove (lambda (f) (or (member f supported) (member f easel-conformance-test-unproven)))
                               vocabulary)
                   nil))
    (dolist (f easel-conformance-test-unproven) (should-not (member f supported)))))

(ert-deftest easel-conformance-check-reads-supported-json ()
  (let ((findings (easel-spec-check '(:data (:values [(:a 1 :b 2)]) :mark "line"
                                      :encoding (:x (:field "a") :y (:field "b") :order (:field "a"))))))
    (should (equal (mapcar (lambda (f) (list (plist-get f :code) (plist-get f :path))) findings)
                   '(("UNSUPPORTED_FEATURE" "/encoding/order")))))
  (should (plist-get (plist-get (easel-describe 'supported) :supported) :features)))

;;; The oracle

(ert-deftest easel-conformance-oracle-agrees-with-bin-chart ()
  "Native SVG vs bin/chart's image within each spec's threshold and size tolerance.
The images are the committed references, or fresh builds when bin/chart
is on PATH; only rsvg-convert is needed."
  (unless (zlib-available-p) (easel-test-skip "this Emacs lacks zlib, needed to decode PNGs"))
  (unless (easel-conformance-test--rasterizer-p)
    (easel-test-skip (format "%s not on PATH; needed to rasterize native SVG for the image oracle"
                             easel-chart-rsvg-program)))
  (let ((failures (seq-remove (lambda (r) (equal (plist-get (plist-get r :oracle) :status) "pass"))
                              (easel-conformance-test--results))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :name) (plist-get (plist-get r :oracle) :detail)))
                           failures)
                   nil))))

(ert-deftest easel-conformance-references-match-the-gallery ()
  "Every gallery spec has a reference built from its current text, and no other."
  (should (equal (mapcar (lambda (e) (list (plist-get e :name) (easel-conformance-ref-problem e)))
                         (seq-filter #'easel-conformance-ref-problem (easel-conformance-gallery)))
                 nil))
  (should (equal (sort (mapcar #'easel-key-name (easel-plist-keys (plist-get (easel-conformance-manifest) :refs)))
                       #'string<)
                 (sort (mapcar (lambda (e) (plist-get e :name)) (easel-conformance-gallery)) #'string<)))
  (should (stringp (easel-conformance-ref-zone))))

(ert-deftest easel-conformance-usermeta-does-not-stale-a-reference ()
  (let* ((entry (car (easel-conformance-gallery)))
         (spec (plist-get entry :spec)))
    (should (equal (easel-conformance-spec-hash spec)
                   (easel-conformance-spec-hash (plist-put (copy-sequence spec) :usermeta '(:easel (:threshold 0.5))))))
    (should-not (equal (easel-conformance-spec-hash spec)
                       (easel-conformance-spec-hash (plist-put (copy-sequence spec) :mark "point"))))))

(defconst easel-conformance-test--theme-file
  (easel-test-file "test/conformance/bin-chart-default-theme.json")
  "bin/chart's default theme, vendored.")

(ert-deftest easel-conformance-default-theme-is-bin-charts ()
  "The native default theme is exactly the vendored `chart theme --json'."
  (let ((vendored (easel-json-read-file easel-conformance-test--theme-file)))
    (should (equal (easel-json-canonical easel-theme-default) (easel-json-canonical (plist-get vendored :config))))
    (should (equal (easel-content-hash easel-theme-default) (plist-get vendored :hash)))))

(ert-deftest easel-conformance-vendored-theme-matches-bin-chart ()
  "The vendored theme is what the installed bin/chart reports."
  (easel-test-require-chart)
  (should (equal (plist-get (easel-chart-theme) :hash)
                 (plist-get (easel-json-read-file easel-conformance-test--theme-file) :hash))))

;;; Plumbing with stand-in programs (not an oracle: they only exercise the harness)

(defun easel-conformance-test--script (body)
  "An executable shell script running BODY."
  (let ((file (make-temp-file "easel-stub" nil ".sh" (concat "#!/bin/sh\n" body "\n"))))
    (set-file-modes file #o755)
    file))

(defun easel-conformance-test--ref (entry)
  "ENTRY's committed reference PNG."
  (expand-file-name (concat (plist-get entry :name) ".png") (easel-conformance-ref-directory)))

(defmacro easel-conformance-test--with-stubs (png chart &rest body)
  "Run BODY with an rsvg-convert stub writing PNG, and a bin/chart stub
writing PNG too when CHART is non-nil (otherwise no bin/chart)."
  (declare (indent 2))
  `(let* ((easel-chart-rsvg-program (easel-conformance-test--script (format "cp '%s' \"$2\"" ,png)))
          (easel-chart-program (if ,chart (easel-conformance-test--script (format "cp '%s' \"$4\"" ,png))
                                 "no-such-chart-program")))
     (unwind-protect (progn ,@body)
       (delete-file easel-chart-rsvg-program)
       (when ,chart (delete-file easel-chart-program)))))

(ert-deftest easel-conformance-oracle-plumbing ()
  (let* ((entry (car (easel-conformance-gallery)))
         (native (list :ok t :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"))
         (ref (easel-conformance-test--ref entry)))
    (easel-conformance-test--with-stubs ref nil
      (let ((r (easel-conformance-oracle entry native)))
        (should (equal (plist-get r :status) "pass"))
        (should (equal (plist-get r :source) "ref"))
        (should (= (plist-get r :ratio) 0))))
    (easel-conformance-test--with-stubs ref t
      (should (equal (plist-get (easel-conformance-oracle entry native) :source) "bin/chart")))
    (let ((easel-chart-rsvg-program "no-such-rsvg-convert"))
      (should (equal (plist-get (easel-conformance-oracle entry native) :status) "unverified")))))

(ert-deftest easel-conformance-oracle-fails-stale-references ()
  (let* ((entry (car (easel-conformance-gallery)))
         (stale (plist-put (copy-sequence entry) :spec
                           (plist-put (copy-sequence (plist-get entry :spec)) :description "edited")))
         (native (list :ok t :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"/>")))
    (should (string-prefix-p "STALE_REF" (easel-conformance-ref-problem stale)))
    (dolist (chart '(nil t))
      (easel-conformance-test--with-stubs (easel-conformance-test--ref entry) chart
        (let ((r (easel-conformance-oracle stale native)))
          (should (equal (plist-get r :status) "fail"))
          (should (string-prefix-p "STALE_REF" (plist-get r :detail))))))))

(ert-deftest easel-conformance-oracle-bounds-the-size-delta ()
  "A canvas size difference beyond the tolerance fails even when pixels agree."
  (let* ((entries (easel-conformance-gallery))
         (entry (seq-find (lambda (e) (equal (plist-get e :name) "mark-bar")) entries))
         (other (seq-find (lambda (e) (equal (plist-get e :name) "mark-tick")) entries))
         (native (list :ok t :svg "<svg xmlns=\"http://www.w3.org/2000/svg\"/>")))
    (easel-conformance-test--with-stubs (easel-conformance-test--ref other) nil
      (let ((r (easel-conformance-oracle entry native)))
        (should (equal (plist-get r :status) "fail"))
        (should (string-match-p "beyond" (plist-get r :detail)))))))

(ert-deftest easel-conformance-native-svg-uses-the-reference-zone ()
  "Native SVG is compiled in the references' time zone, the text in UTC."
  (let* ((entry (seq-find (lambda (e) (equal (plist-get e :name) "encoding-timeunit")) (easel-conformance-gallery)))
         (native (easel-conformance-native entry)))
    (should (equal (easel-conformance-ref-zone) "America/Chicago"))
    ;; 2026-03-02 is UTC midnight, still 1 March in Chicago, as Vega shows it.
    (should (string-match-p ">Mar 01, 2026<" (plist-get native :svg)))
    (should (string-match-p "Mar 02, 2026" (plist-get native :text)))))

;;; Static fallback

(defconst easel-conformance-test--arc
  '(:data (:values [(:k "a" :v 1) (:k "b" :v 2)]) :mark "arc"
    :encoding (:theta (:field "v" :type "quantitative") :color (:field "k" :type "nominal")))
  "A pie chart: outside the native subset.")

(ert-deftest easel-conformance-unsupported-specs-fall-back-to-static ()
  (let ((easel-views (make-hash-table :test 'equal))
        (easel-chart-program "no-such-chart-program"))
    (let* ((view (easel-view-open easel-conformance-test--arc :id "pie"))
           (inspect (easel-inspect view)))
      (should (eq (plist-get inspect :interactive) :false))
      (should (equal (mapcar (lambda (w) (plist-get w :path)) (plist-get inspect :warnings))
                     '("/mark" "/encoding/theta")))
      (should (equal (plist-get (plist-get (plist-get inspect :static) :error) :code) "NOT_FOUND"))
      (easel-dispatch view '(:type "key" :key "+"))
      (should (equal (plist-get (easel-inspect view) :last-event) "key +"))
      (let ((buffer (easel-show view 'text)))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "Static chart (not interactive)" (buffer-string)))
              (should (string-match-p "mark/arc at /mark" (buffer-string))))
          (kill-buffer buffer))))))

(ert-deftest easel-conformance-fallback-takes-bin-chart-image ()
  (easel-conformance-test--with-stubs (easel-conformance-test--ref (car (easel-conformance-gallery))) t
   (let ((easel-views (make-hash-table :test 'equal)))
     (let ((inspect (easel-inspect (easel-view-open easel-conformance-test--arc :id "pie"))))
       (should (equal (plist-get inspect :static) '(:source "bin/chart" :type "svg")))))))

(provide 'easel-conformance-test)
;;; easel-conformance-test.el ends here
