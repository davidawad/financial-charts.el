;;; easel-crosshair-test.el --- tests for the crosshair and its readout -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.2: a point selection {on: pointermove, nearest: true} plus a
;; rule layer filtered by it, driven headlessly through `easel-dispatch'
;; and `easel-replay', in a text buffer through point motion.

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defmacro easel-crosshair-test--with-view (var source &rest body)
  "Open SOURCE (bindings for the line template, or a spec) as VAR; run BODY."
  (declare (indent 2))
  `(let ((easel-views (make-hash-table :test 'equal)))
     (let ((,var (if (plist-get ,source :data)
                     (if (vectorp (plist-get ,source :data))
                         (easel-view-open "line" :bindings ,source :id "c")
                       (easel-view-open ,source :id "c"))
                   (error "Bad source"))))
       ,@body)))

(defun easel-crosshair-test--bindings ()
  "The line template's example bindings with the crosshair on."
  (plist-put (copy-sequence (easel-template-example "line")) :crosshair t))

(defun easel-crosshair-test--view (view)
  "VIEW's first scene view."
  (aref (plist-get (easel-view-scene view) :views) 0))

(defun easel-crosshair-test--mark (view id)
  "Mark ID (\"crosshair-rule\", \"crosshair-point\") of VIEW's scene."
  (easel-scene-mark (easel-view-scene view) id))

(defun easel-crosshair-test--x (view value)
  "Scene x pixel of data VALUE in VIEW."
  (easel-scale-apply (plist-get (plist-get (easel-crosshair-test--view view) :scales) :x) value))

(defun easel-crosshair-test--top (view)
  "A y pixel just inside the top of VIEW's plot, far from most data."
  (+ 2 (aref (plist-get (easel-crosshair-test--view view) :bounds) 1)))

(defconst easel-crosshair-test--sibling-spec
  '(:data (:values [(:t 1 :p 3) (:t 2 :p 5) (:t 3 :p 4) (:t 4 :p 8)])
    :width 200 :height 100
    :layer [(:params [(:name "h" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
             :mark "point"
             :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
                        :opacity (:condition (:param "h" :empty :false :value 1) :value 0)))
            (:mark "line"
             :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
                        :tooltip [(:field "p" :type "quantitative" :title "Price") (:field "t" :type "quantitative")]))
            (:transform [(:filter (:param "h" :empty :false))]
             :mark "rule" :encoding (:x (:field "t" :type "quantitative")))])
  "The param on an invisible point layer; only the line has a tooltip.")

;;; Template and reducer

(ert-deftest easel-crosshair-template-slot-is-opt-in ()
  (let ((off (easel-resolve "line" (easel-template-example "line")))
        (on (easel-resolve "line" (easel-crosshair-test--bindings))))
    (should (= (length (plist-get off :layer)) 1))
    (should (equal (seq-map (lambda (l) (plist-get l :name)) (plist-get on :layer))
                   '(nil "crosshair-hit" "crosshair-rule" "crosshair-point")))
    ;; The param lives on invisible rules: Vega-Lite has no `nearest' for
    ;; line marks, and rules hit-test by bisect.
    (should (equal (plist-get (aref (plist-get (aref (plist-get on :layer) 1) :params) 0) :select)
                   '(:type "point" :on "pointermove" :nearest t :encodings ["x"])))
    ;; The static path (bin/chart) draws the initial state: no crosshair.
    (dolist (i '(2 3))
      (should (equal (plist-get (aref (plist-get on :layer) i) :transform)
                     [(:filter (:param "crosshair" :empty :false))])))))

(ert-deftest easel-crosshair-snaps-to-the-nearest-x-anywhere-in-the-plot ()
  (easel-crosshair-test--with-view v (easel-crosshair-test--bindings)
    (let ((rows (plist-get (easel-crosshair-test--bindings) :data)))
      (seq-do-indexed
       (lambda (row k)
         ;; Pointer at the top of the plot, a few pixels inward of datum K:
         ;; nearest is horizontal, so the y distance does not matter.
         (let* ((x (easel-crosshair-test--x v (plist-get row :date)))
                (inspect (easel-dispatch v (list :type "pointermove"
                                                 :px (vector (+ x (if (zerop k) 3 -3)) (easel-crosshair-test--top v)))))
                (rule (plist-get (easel-crosshair-test--mark v "crosshair-rule") :items)))
           (should (equal (plist-get (plist-get (plist-get inspect :hover) :row) :date) (plist-get row :date)))
           (should (equal (plist-get (seq-find (lambda (p) (equal (plist-get p :name) "crosshair"))
                                               (plist-get inspect :params))
                                     :summary)
                          "1 value of date"))
           (should (= (length rule) 1))
           (should (< (abs (- (plist-get (aref rule 0) :x1) x)) 0.5))
           (should (equal (seq-map (lambda (r) (plist-get r :date))
                                   (plist-get (easel-crosshair-test--mark v "crosshair-point") :rows))
                          (list (plist-get row :date))))))
       rows))))

(ert-deftest easel-crosshair-leave-and-escape-clear-it ()
  (easel-crosshair-test--with-view v (easel-crosshair-test--bindings)
    (dolist (clear '((:type "pointerleave") (:type "key" :key "escape")))
      (easel-dispatch v (list :type "pointermove" :px (vector (easel-crosshair-test--x v "2026-03-05") 100)))
      (should (= (length (plist-get (easel-crosshair-test--mark v "crosshair-rule") :items)) 1))
      (easel-dispatch v clear)
      (should (= (length (plist-get (easel-crosshair-test--mark v "crosshair-rule") :items)) 0))
      (should (eq (plist-get (easel-inspect v) :hover) :null))
      (should-not (easel-crosshair-view-readout v)))))

(ert-deftest easel-crosshair-replays-from-the-log ()
  (easel-crosshair-test--with-view v (easel-crosshair-test--bindings)
    (dolist (date '("2026-03-03" "2026-03-09" "2026-03-06"))
      (easel-dispatch v (list :type "pointermove" :px (vector (easel-crosshair-test--x v date) 60))))
    (let ((log (easel-view-log v)) (want (easel-inspect v))
          (rule (plist-get (easel-crosshair-test--mark v "crosshair-rule") :items)))
      (let ((w (easel-view-open "line" :bindings (easel-crosshair-test--bindings) :id "replay")))
        (easel-replay w log)
        (should (equal (plist-get (easel-inspect w) :hover) (plist-get want :hover)))
        (should (equal (plist-get (easel-crosshair-test--mark w "crosshair-rule") :items) rule))
        (should (equal (plist-get (plist-get (plist-get want :hover) :row) :date) "2026-03-06"))))))

(ert-deftest easel-crosshair-patched-scenes-equal-full-compiles ()
  (dolist (source (list (easel-crosshair-test--bindings) easel-crosshair-test--sibling-spec))
    (easel-crosshair-test--with-view v source
      (dolist (e '((:type "pointermove" :px [70 40]) (:type "pointermove" :px [190 90])
                   (:type "pointermove" :px [120 30]) (:type "pointerleave")))
        (easel-dispatch v e)
        (should (equal (easel-scene-to-json (easel-view-scene v))
                       (easel-scene-to-json
                        (easel-params-with-state (easel-view-state v)
                          (easel-compile (easel-view-spec v) :state (easel-view-state v))))))))))

(ert-deftest easel-crosshair-date-parse-cache-is-transparent ()
  (let ((easel-time--parse-cache (make-hash-table :test 'equal)))
    (dotimes (_ 2)
      (should (equal (easel-time-parse "2026-03-05") (easel-time--parse-string "2026-03-05")))
      (should (equal (easel-time-parse "2026-03-05T12:00:00+02:00") 1772704800000))
      (should-not (easel-time-parse "AAPL"))
      (should (= (easel-time-parse 42) 42)))
    (should (eq (gethash "AAPL" easel-time--parse-cache) :none))))

;;; Readout

(ert-deftest easel-crosshair-readout-lists-every-tooltip-field ()
  (easel-crosshair-test--with-view v (easel-crosshair-test--bindings)
    (easel-dispatch v (list :type "pointermove" :px (vector (easel-crosshair-test--x v "2026-03-05") 40)))
    (should (equal (easel-crosshair-view-readout v)
                   [(:title "date" :value "Mar 05, 2026") (:title "value" :value "106.3")]))
    (should (equal (easel-crosshair-format (easel-crosshair-view-readout v))
                   "date=Mar 05, 2026  value=106.3"))))

(ert-deftest easel-crosshair-readout-borrows-a-sibling-marks-tooltip ()
  (easel-crosshair-test--with-view v easel-crosshair-test--sibling-spec
    (let ((inspect (easel-dispatch v (list :type "pointermove" :px (vector (easel-crosshair-test--x v 3) (easel-crosshair-test--top v))))))
      ;; The point layer holding the param wins the hit and has no tooltip.
      (should (equal (plist-get (plist-get inspect :hover) :mark) "main/0"))
      (should (equal (easel-crosshair-view-readout v) [(:title "Price" :value "4") (:title "t" :value "3")])))))

(ert-deftest easel-crosshair-readout-falls-back-to-the-row ()
  (easel-crosshair-test--with-view v '(:data (:values [(:t 1 :p 3) (:t 2 :p 5)])
                                       :mark "line"
                                       :encoding (:x (:field "t" :type "quantitative")
                                                  :y (:field "p" :type "quantitative")))
    (easel-dispatch v (list :type "pointermove"
                            :px (vector (easel-crosshair-test--x v 2)
                                        (easel-scale-apply (plist-get (plist-get (easel-crosshair-test--view v) :scales) :y) 5))))
    (should (equal (easel-crosshair-view-readout v) [(:title "t" :value "2") (:title "p" :value "5")]))))

;;; Glue

(defun easel-crosshair-test--rule-column (view)
  "Text column of VIEW's crosshair rule, or nil."
  (when-let* ((items (plist-get (easel-crosshair-test--mark view "crosshair-rule") :items))
              ((> (length items) 0)))
    (floor (easel-text--inside (plist-get (easel-crosshair-test--view view) :bounds) (plist-get (aref items 0) :x1))
           (aref (plist-get (plist-get (easel-view-scene view) :size) :cell) 0))))

(ert-deftest easel-crosshair-point-motion-moves-the-column ()
  (let ((easel-views (make-hash-table :test 'equal)))
    (let* ((view (easel-view-open "line" :bindings (easel-crosshair-test--bindings) :id "tty"))
           (buffer (easel-show view 'text))
           (written 0))
      (unwind-protect
          (with-current-buffer buffer
            (add-hook 'after-change-functions (lambda (beg end _) (setq written (+ written (- end beg)))) nil t)
            (let ((first (text-property-any (point-min) (point-max) 'easel-datum 1))
                  (last (text-property-any (point-min) (point-max) 'easel-datum 7))
                  columns)
              (dolist (pos (list first last))
                (goto-char pos)
                (setq written 0)
                (easel-mode--post-command)
                (let ((col (easel-crosshair-test--rule-column view))
                      (tooltip (plist-get (plist-get (easel-inspect view) :hover) :tooltip)))
                  (push col columns)
                  ;; The buffer is exactly the new grid, point did not move,
                  ;; and only the crosshair's cells were rewritten.
                  (should (equal-including-properties (buffer-string) (easel-text-render (easel-view-scene view))))
                  (should (= (point) pos))
                  ;; The default theme has no frame, so lines are right-trimmed
                  ;; and a rule past a line's end also writes the blank run to it
                  ;; (fc-qx1.21): still a fraction of the grid, never a rewrite.
                  (should (< 0 written (max (* 3 (count-lines (point-min) (point-max)))
                                            (/ (buffer-size) 5))))
                  (should (= (current-column) (save-excursion (goto-char pos) (current-column))))
                  (should (= (length tooltip) 2))
                  (should (string-suffix-p (easel-crosshair-format tooltip) (format "%s" header-line-format)))
                  ;; A rule glyph sits in the crosshair's column.
                  (should (save-excursion
                            (goto-char (point-min))
                            (cl-loop until (eobp)
                                     thereis (progn (move-to-column col)
                                                                    (and (= (current-column) col) (eq (char-after) ?│)))
                                     do (forward-line 1))))))
              (should (> (nth 0 columns) (nth 1 columns)))))
        (kill-buffer buffer)))))

(ert-deftest easel-crosshair-gui-readout-updates-before-the-redraw ()
  (let ((easel-views (make-hash-table :test 'equal)) (redraws 0))
    (let ((view (easel-view-open "line" :bindings (easel-crosshair-test--bindings) :id "gui" :target 'svg)))
      (with-temp-buffer
        (easel-view-mode)
        (setq easel-mode--view view)
        (setf (easel-view-buffer view) (current-buffer))
        (cl-letf (((symbol-function 'easel-mode-redraw) (lambda (&rest _) (setq redraws (1+ redraws)))))
          (let* ((x (round (easel-crosshair-test--x view "2026-03-10")))
                 (image '(image :type svg :data "" :scale 1))
                 (posn (list (selected-window) 1 (cons x 40) 0 nil 1 (cons x 40) image (cons x 40) '(400 . 300))))
            (easel-mode-pointer (list 'mouse-movement posn))
            (should (string-match-p "date=Mar 10, 2026  value=110.4" (format "%s" header-line-format)))
            (should (= redraws 1))))))))

(provide 'easel-crosshair-test)
;;; easel-crosshair-test.el ends here
