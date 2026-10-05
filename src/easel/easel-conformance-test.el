;;; easel-conformance-test.el --- conformance gallery, supported.json, fallback -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defconst easel-conformance-test-unproven '("encoding/order")
  "Recognised features no gallery spec proves yet (so they fall back).")

(ert-deftest easel-conformance-text-goldens ()
  "Every gallery spec compiles natively and matches its exact text golden."
  (dolist (entry (easel-conformance-gallery))
    (let* ((file (easel-conformance-text-file entry))
           (easel-spec-supported-function nil)
           (text (concat (substring-no-properties
                          (easel-text-render (easel-compile (plist-get entry :spec) :target 'text
                                                            :size easel-conformance-text-size)))
                         "\n")))
      (if (getenv "EASEL_UPDATE_GOLDEN")
          (with-temp-file file (set-buffer-file-coding-system 'utf-8-unix) (insert text))
        (should (equal (cons (plist-get entry :name) text)
                       (cons (plist-get entry :name)
                             (with-temp-buffer (insert-file-contents file) (buffer-string)))))))))

(ert-deftest easel-conformance-supported-json-is-current ()
  "supported.json lists exactly the features the passing gallery proves."
  (let* ((results (easel-conformance-run nil))
         (generated (easel-conformance-supported-data results)))
    (should (seq-every-p (lambda (r) (plist-get r :ok)) results))
    (if (getenv "EASEL_UPDATE_GOLDEN")
        (with-temp-file easel-conformance-supported-file (insert (easel-json-pretty generated)))
      (let ((committed (easel-conformance-supported)))
        (should committed)
        (should (equal (cl-loop for (k v) on (plist-get generated :features) by #'cddr
                                collect (cons k (plist-get v :specs)))
                       (cl-loop for (k v) on (plist-get committed :features) by #'cddr
                                collect (cons k (plist-get v :specs)))))))))

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

(ert-deftest easel-conformance-oracle-agrees-with-bin-chart ()
  "Native SVG vs bin/chart PNG within each spec's threshold."
  (easel-test-require-chart)
  (unless (executable-find easel-chart-rsvg-program)
    (ert-skip (format "%s not on PATH; needed to rasterize native SVG" easel-chart-rsvg-program)))
  (let ((failures (seq-remove (lambda (r) (equal (plist-get (plist-get r :oracle) :status) "pass"))
                              (easel-conformance-run t))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :name) (plist-get r :oracle))) failures) nil))))

;;; Plumbing with stand-in programs (not an oracle: they only exercise the harness)

(defun easel-conformance-test--script (body)
  "An executable shell script running BODY."
  (let ((file (make-temp-file "easel-stub" nil ".sh" (concat "#!/bin/sh\n" body "\n"))))
    (set-file-modes file #o755)
    file))

(defmacro easel-conformance-test--with-stub-chart (&rest body)
  "Run BODY with stub bin/chart and rsvg-convert programs."
  `(let* ((easel-chart-program
           (easel-conformance-test--script
            "case \"$1\" in build) printf '<svg xmlns=\"http://www.w3.org/2000/svg\"/>' > \"$4\";; diff) exit 0;; esac"))
          (easel-chart-rsvg-program (easel-conformance-test--script "printf PNG > \"$2\"")))
     (unwind-protect (progn ,@body)
       (delete-file easel-chart-program) (delete-file easel-chart-rsvg-program))))

(ert-deftest easel-conformance-oracle-plumbing ()
  (easel-conformance-test--with-stub-chart
   (let* ((entry (car (easel-conformance-gallery)))
          (native (easel-conformance-native entry)))
     (should (equal (plist-get (easel-conformance-oracle entry native) :status) "pass"))))
  (let ((easel-chart-program "no-such-chart-program"))
    (should (equal (plist-get (easel-conformance-oracle (car (easel-conformance-gallery)) '(:ok t)) :status)
                   "unverified"))))

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
  (easel-conformance-test--with-stub-chart
   (let ((easel-views (make-hash-table :test 'equal)))
     (let ((inspect (easel-inspect (easel-view-open easel-conformance-test--arc :id "pie"))))
       (should (equal (plist-get inspect :static) '(:source "bin/chart" :type "svg")))))))

(provide 'easel-conformance-test)
;;; easel-conformance-test.el ends here
