;;; easel-render-test.el --- tests for the SVG and text renderers -*- lexical-binding: t; -*-

;;; Code:

(require 'easel-test-support)
(require 'easel)
(require 'easel-svg)
(require 'easel-text)

(defconst easel-render-test--specs
  `(("line" . ,(lambda () (easel-resolve "line" (plist-put (easel-template-example "line") :points t))))
    ("bars" . ,(lambda () (easel-resolve "bars" (easel-template-example "bars"))))
    ("stacked" . ,(lambda ()
                    '(:data (:values [(:k "a" :c "x" :v 1) (:k "a" :c "y" :v 2) (:k "b" :c "x" :v 3) (:k "b" :c "y" :v 1)])
                      :vconcat [(:mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                                        :color (:field "c" :type "nominal")))
                                (:mark "point" :encoding (:x (:field "v" :type "quantitative") :y (:field "k" :type "nominal")))])))
    ("area" . ,(lambda () '(:data (:values [(:t 1 :v 3) (:t 2 :v 5) (:t 3 :v 4) (:t 4 :v 8) (:t 5 :v 6)])
                            :mark "area" :encoding (:x (:field "t" :type "quantitative") :y (:field "v" :type "quantitative")))))
    ("hbar" . ,(lambda () '(:data (:values [(:k "alpha" :v 3.3) (:k "beta" :v 7.9) (:k "gamma" :v 5.2)])
                            :mark "bar" :encoding (:y (:field "k" :type "nominal") :x (:field "v" :type "quantitative"))))))
  "Specs rendered by both backends for goldens.")

(ert-deftest easel-render-text-goldens ()
  (dolist (entry easel-render-test--specs)
    (easel-test-golden (format "text-%s.txt" (car entry))
                       (concat (substring-no-properties
                                (easel-text-render (easel-compile (funcall (cdr entry)) :target 'text
                                                                  :size '(:cols 60 :rows 16))))
                               "\n"))))

(ert-deftest easel-render-svg-goldens ()
  (dolist (entry easel-render-test--specs)
    (easel-test-golden (format "svg-%s.svg" (car entry))
                       (concat (easel-svg-render (easel-compile (funcall (cdr entry)))) "\n"))))

(ert-deftest easel-render-text-is-deterministic ()
  (let ((spec (funcall (cdr (assoc "bars" easel-render-test--specs)))))
    (should (equal (easel-text-render (easel-compile spec :target 'text :size '(:cols 40 :rows 12)))
                   (easel-text-render (easel-compile spec :target 'text :size '(:cols 40 :rows 12)))))))

(defun easel-render-test--find (text prop value)
  "Position of the first char of TEXT whose PROP equals VALUE."
  (cl-loop for i from 0 below (length text)
           when (equal (get-text-property i prop text) value) return i))

(ert-deftest easel-render-text-cells-carry-datum-and-help-echo ()
  (let* ((scene (easel-compile (funcall (cdr (assoc "bars" easel-render-test--specs)))
                               :target 'text :size '(:cols 50 :rows 14)))
         (text (easel-text-render scene))
         (pos (easel-render-test--find text 'easel-datum 4)))
    (should pos)
    (should (equal (get-text-property pos 'easel-view text) "main"))
    (should (equal (get-text-property pos 'easel-mark text) "main/0"))
    (should (equal (get-text-property pos 'help-echo text) "category: Fri\nvalue: 12050"))
    (should (equal (plist-get (get-text-property pos 'face text) :foreground) "#4c78a8"))))

(ert-deftest easel-render-text-line-cells-map-to-nearest-datum ()
  (let* ((scene (easel-compile (easel-resolve "line" (easel-template-example "line"))
                               :target 'text :size '(:cols 60 :rows 16)))
         (text (easel-text-render scene))
         (datums (delete-dups (cl-loop for i from 0 below (length text)
                                       for d = (get-text-property i 'easel-datum text)
                                       when d collect d))))
    (should (equal (sort datums #'<) '(0 1 2 3 4 5 6 7)))))

(ert-deftest easel-render-text-legend-entries-are-tagged ()
  (let* ((text (easel-text-render (easel-compile (funcall (cdr (assoc "stacked" easel-render-test--specs)))
                                                 :target 'text :size '(:cols 50 :rows 20)))))
    (should (easel-render-test--find text 'easel-legend "y"))))

(ert-deftest easel-render-svg-hot-spots-for-discrete-items-and-legends ()
  (let* ((scene (easel-compile (funcall (cdr (assoc "stacked" easel-render-test--specs)))))
         (image (easel-svg-image scene :scale 1))
         (map (plist-get (cdr image) :map)))
    (should (eq (car image) 'image))
    (should (eq (plist-get (cdr image) :type) 'svg))
    (should (= (length map) (+ 4 4 2)))
    (should (assq 'easel:vconcat_0|vconcat_0/0|0 (mapcar (lambda (a) (cons (nth 1 a) a)) map)))
    (should (seq-find (lambda (a) (string-prefix-p "easel-legend:" (symbol-name (nth 1 a)))) map))
    (let ((area (seq-find (lambda (a) (eq (car (car a)) 'rect)) map)))
      (should (plist-get (nth 2 area) 'pointer)))))

(ert-deftest easel-render-svg-series-are-single-paths ()
  (let* ((values (vconcat (mapcar (lambda (i) (list :x i :y (% (* i 7) 13))) (number-sequence 0 999))))
         (svg (easel-svg-render (easel-compile (list :data (list :values values) :mark "line"
                                                     :encoding '(:x (:field "x" :type "quantitative")
                                                                 :y (:field "y" :type "quantitative")))))))
    (should (= (cl-count-if (lambda (_) t) (split-string svg "<path" t)) 2))))

(ert-deftest easel-render-svg-theme-is-a-vega-config ()
  (let* ((scene (easel-compile (funcall (cdr (assoc "bars" easel-render-test--specs)))))
         (svg (easel-svg-render scene '(:background "#111111" :axis (:labelColor "#eeeeee")))))
    (should (string-match-p "fill=\"#111111\"" svg))
    (should (string-match-p "fill=\"#eeeeee\"" svg))
    (should (string-match-p "stroke=\"#888\"" svg))))

(ert-deftest easel-render-svg-escapes-text ()
  (let ((svg (easel-svg-render (easel-compile '(:title "a < b & c" :data (:values [(:x 1)]) :mark "point"
                                                :encoding (:x (:field "x" :type "quantitative")))))))
    (should (string-match-p "a &lt; b &amp; c" svg))))

(ert-deftest easel-renderers-read-only-the-scene ()
  "Renderers depend on no spec, template or compile layer."
  (dolist (file '("src/easel/easel-svg.el" "src/easel/easel-text.el"))
    (with-temp-buffer
      (insert-file-contents (easel-test-file file))
      (dolist (layer '("easel-compile" "easel-spec" "easel-template" "easel-resolve" "easel-marks"))
        (goto-char (point-min))
        (should-not (re-search-forward (format "(require '%s)" layer) nil t))))))

(provide 'easel-render-test)
;;; easel-render-test.el ends here
