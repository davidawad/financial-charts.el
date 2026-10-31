;;; financial-chart-3d-vol-surface-test.el --- The IV surface, in 2D and in 3D -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these beside financial-chart-3d-test.el, against
;; the graphics.el and eas.el checkouts of `make deps-3d'.  The rows come
;; from the SPY chain fixture (test/fixtures/3d/vol-surface/); the 2D
;; template validates and draws in a subprocess without graphics.el;
;; the 3D variant replays the session and draws its golden
;; (test/golden/3d/vol-surface.{frame,txt}, 60x24, dark).

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(require 'graphics-frame)
(financial-chart-3d--ensure)

(defun financial-chart-3d-vol-test--chain ()
  "The SPY chain fixture."
  (eas-json-read-file (expand-file-name financial-chart-3d-vol-surface-fixture financial-chart-test-root)))

(defun financial-chart-3d-vol-test--prim (prims id)
  "The primitive of PRIMS whose id is ID."
  (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))

(defconst financial-chart-3d-vol-test--grid
  '((:time "t1" :spot 100 :strike 90 :dte 7 :iv 0.30) (:time "t1" :spot 100 :strike 100 :dte 7 :iv 0.20)
    (:time "t1" :spot 100 :strike 110 :dte 7 :iv 0.24)
    (:time "t1" :spot 100 :strike 90 :dte 30 :iv 0.22) (:time "t1" :spot 100 :strike 110 :dte 30 :iv 0.18))
  "Two expiries by three strikes, the 30-day 100 strike missing.")

;;; Rows

(ert-deftest financial-chart-3d-vol-surface-rows-from-the-fixture ()
  (let* ((chain (financial-chart-3d-vol-test--chain))
         (rows (append (financial-chart-3d-vol-surface-rows chain) nil))
         (times (delete-dups (mapcar (lambda (r) (plist-get r :time)) rows))))
    (should (equal (plist-get chain :symbol) "SPY"))
    (should (= (length times) (length (plist-get chain :snapshots))))
    (should (equal times (sort (copy-sequence times) #'string<)))
    (should (= (length rows) (* (length times) 8 17)))
    (dolist (r rows)
      (should (stringp (plist-get r :expiry)))
      (should (< 0.05 (plist-get r :iv) 1.0))
      ;; dte is the calendar days from the quote to the expiry.
      (should (= (plist-get r :dte) (financial-chart-3d--vol-days (plist-get r :time) (plist-get r :expiry)))))
    ;; The front week's put skew: IV falls from the lowest strike to the money.
    (let ((front (seq-filter (lambda (r) (and (equal (plist-get r :time) (car times)) (= (plist-get r :dte) 7)))
                             rows)))
      (should (> (plist-get (car front) :iv) (plist-get (nth 8 front) :iv))))))

(ert-deftest financial-chart-3d-vol-surface-rows-shapes-and-errors ()
  ;; Snapshots alone; quotes under `quotes'; dte from expiry; no IV dropped.
  (let ((rows (financial-chart-3d-vol-surface-rows
               '((:time "2026-10-30T10:00:00" :spot 100
                        :quotes [(:strike 100 :expiry "2026-11-06" :iv 0.2)
                                 (:strike 105 :expiry "2026-11-06" :iv :null)])))))
    (should (equal (append rows nil)
                   '((:time "2026-10-30T10:00:00" :spot 100 :strike 100 :dte 7 :expiry "2026-11-06" :iv 0.2)))))
  (dolist (case '(((:spot 1 :chain [(:strike 1 :dte 1 :iv 0.1)]) "missing_time")
                  ((:time "2026-10-30" :chain [(:strike 1 :dte 1 :iv 0.1)]) "missing_spot")
                  ((:time "2026-10-30" :spot 1 :chain [(:dte 1 :iv 0.1)]) "missing_strike")
                  ((:time "2026-10-30" :spot 1 :chain [(:strike 1 :iv 0.1)]) "missing_dte")
                  ((:time "2026-10-30" :spot 1 :chain [(:strike 1 :dte 1 :iv -0.1)]) "bad_iv")
                  ((:time "2026-10-30" :spot 1 :chain []) "empty_chain")))
    (let ((err (should-error (financial-chart-3d-vol-surface-rows (list (car case)))
                             :type 'financial-chart-invalid-data)))
      (should (equal (plist-get (cddr err) :code) (cadr case))))))

(ert-deftest financial-chart-3d-vol-surface-example-is-generated ()
  (let ((example (eas-json-read-file (expand-file-name "examples/vol-surface.data.json" financial-chart-test-root)))
        (file (make-temp-file "fc3d-vol" nil ".json")))
    (unwind-protect
        (progn
          (financial-chart-3d-vol-surface-write-example file)
          (should (equal (eas-json-read-file file) example)))
      (delete-file file))
    (should (equal (eas-template-example "vol-surface") example))
    (should (equal (eas-template-example "fc3d/vol-surface") example))))

;;; The mark

(ert-deftest financial-chart-3d-vol-surface-mark-wire-and-surface ()
  (let* ((wire (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :strike_every 1)
                                                     financial-chart-3d-vol-test--grid))
         (mesh (financial-chart-3d-vol-test--prim wire "surface"))
         (atm (financial-chart-3d-vol-test--prim wire "atm"))
         (surface (financial-chart-3d-vol-test--prim
                   (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "surface")
                                                         financial-chart-3d-vol-test--grid)
                   "surface"))
         (heights (plist-get surface :heights)))
    ;; Two smiles of two segments, three strikes' term lines of one.
    (should (equal (plist-get mesh :type) "segments"))
    (should (= (length (plist-get mesh :segments)) 7))
    (should (= (length (plist-get mesh :colors)) 7))
    ;; The at-the-money term structure: one segment between two expiries.
    (should (= (length (plist-get atm :segments)) 1))
    (should (equal (plist-get surface :type) "heightfield"))
    ;; Rows are expiries, nearest first; the missing cell is interpolated.
    (should (equal heights [[0.3 0.2 0.24] [0.22 0.2 0.18]]))
    (should (equal (plist-get surface :color_range) [0.18 0.3]))
    ;; No rows, or a single strike: an empty surface, not an error.
    (should (equal (plist-get (financial-chart-3d-vol-test--prim
                               (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d") nil)
                               "surface")
                              :segments)
                   []))))

(ert-deftest financial-chart-3d-vol-surface-mark-spans-the-session ()
  ;; Under a time encoding the scales are every snapshot's: a quieter
  ;; snapshot draws lower on the same axes instead of filling them.
  (let* ((later (mapcar (lambda (r) (plist-put (copy-sequence r) :iv (- (plist-get r :iv) 0.05)))
                        financial-chart-3d-vol-test--grid))
         (alone (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "surface") later))
         (spanned (let ((graphics-eas-domain-rows financial-chart-3d-vol-test--grid))
                    (financial-chart-3d--vol-surface-mark '(:mark "fc-vol-surface3d" :style "surface") later))))
    (cl-flet ((range (prims) (mapcar (lambda (v) (/ (round (* v 1000)) 1000.0))
                                     (plist-get (financial-chart-3d-vol-test--prim prims "surface") :color_range))))
      (should (equal (range alone) '(0.13 0.25)))
      (should (equal (range spanned) '(0.13 0.3))))))

;;; The 3D view

(ert-deftest financial-chart-3d-vol-surface-replays-the-session ()
  (let ((view (financial-chart-3d-vol-surface-open (financial-chart-3d-vol-test--chain) :id "fc3d-test-vol")))
    (unwind-protect
        (let ((timeline (graphics-eas-timeline view)))
          (should (member "fc-vol-surface3d" (graphics-eas-all-marks)))
          (should (>= (plist-get timeline :keyframes) 8))
          (should (plist-get timeline :loop))
          (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
          (let ((open (graphics-eas-frame view 60 24)))
            (financial-chart-3d-seek-end view)
            (should-not (equal open (graphics-eas-frame view 60 24)))))
      (financial-chart-3d--close view))))

(ert-deftest financial-chart-3d-vol-surface-golden ()
  (graphics-ensure-core 'rs)
  (let* ((dir (expand-file-name "test/golden/3d" financial-chart-test-root))
         (frame (financial-chart-3d-frame "vol-surface"))
         (tmp (make-temp-file "fc3d-vol" nil ".frame")))
    (unwind-protect
        (progn
          (graphics-frame-write-file tmp frame 60 24)
          (should (equal (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally tmp) (buffer-string))
                         (with-temp-buffer (set-buffer-multibyte nil)
                                           (insert-file-contents-literally (expand-file-name "vol-surface.frame" dir))
                                           (buffer-string)))))
      (delete-file tmp))
    (let ((text (graphics-eas-braille-text frame 60)))
      (should (equal text (with-temp-buffer (let ((coding-system-for-read 'utf-8))
                                              (insert-file-contents (expand-file-name "vol-surface.txt" dir)))
                                            (buffer-string))))
      ;; Something drawn across most of the frame, not a blob or a speck.
      (let ((lines (seq-filter (lambda (l) (string-match-p "[^⠀]" l)) (split-string text "\n" t))))
        (should (>= (length lines) 14))))))

;;; Without graphics.el

(ert-deftest financial-chart-3d-vol-surface-validates-without-graphics ()
  (let* ((eas (file-name-directory (locate-library "eas")))
         (root financial-chart-test-root)
         (dirs (mapcar (lambda (d) (expand-file-name d root))
                       '("src" "src/core" "src/indicators" "src/charts" "src/integrations")))
         (form `(progn
                  (require 'financial-chart-3d)
                  (when (or (featurep 'graphics-eas) (financial-chart-3d-available-p))
                    (error "graphics.el was found"))
                  (dolist (name '("vol-surface" "fc3d/vol-surface"))
                    (let ((spec (eas-resolve name (eas-template-example name))))
                      (when (eas-spec-check spec) (error "%s: %S" name (eas-spec-check spec)))
                      (unless (> (length (eas-text-render (eas-compile spec :target 'text :size '(:cols 60 :rows 14))))
                                 0)
                        (error "%s did not draw in 2D" name))))
                  ;; The 2D heatmap is the latest quote's: one cell per strike and expiry.
                  (let* ((rows (financial-chart-3d-vol-surface-rows
                                (eas-json-read-file ,(expand-file-name financial-chart-3d-vol-surface-fixture root))))
                         (svg (eas-svg-render (eas-compile (eas-resolve "vol-surface" (list :data rows))
                                                           :target 'svg :size '(640 . 360))))
                         (cells (with-temp-buffer (insert svg) (count-matches "<rect" (point-min) (point-max))))
                         (all (with-temp-buffer
                                (insert (eas-svg-render (eas-compile (eas-resolve "vol-surface" (list :data (seq-take rows 136)))
                                                                     :target 'svg :size '(640 . 360))))
                                (count-matches "<rect" (point-min) (point-max)))))
                    (unless (= cells all) (error "2D drew %d rects, one snapshot draws %d" cells all))
                    (condition-case err (progn (financial-chart-3d-vol-surface-open rows) (error "no error"))
                      (financial-chart-3d-unavailable (princ (format "OK %s" (plist-get (cddr err) :code))))))))
         (output (with-temp-buffer
                   (list (apply #'call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" eas
                                (append (mapcan (lambda (d) (list "-L" d)) dirs)
                                        (list "--eval" (prin1-to-string form))))
                         (buffer-string)))))
    (should (equal output (list 0 "OK GRAPHICS_MISSING")))))

(provide 'financial-chart-3d-vol-surface-test)
;;; financial-chart-3d-vol-surface-test.el ends here
