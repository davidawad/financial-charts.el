;;; financial-chart-3d-test.el --- 3D variants of the templates via graphics.el -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these against the graphics.el and eas.el
;; checkouts `make deps-3d' prepares in test/deps/ (graphics.el's Rust
;; module built).  Every template has a 3D variant whose body is the
;; template's, which eas still draws in 2D and graphics.el draws in 3D
;; to its goldens (test/golden/3d/NAME.{frame,txt} at the end of its
;; replay and t0.5/NAME.{frame,txt} halfway, 60x24, dark, the Rust core;
;; EAS_UPDATE_GOLDEN=1 rewrites them, `make golden-3d' also rewrites
;; docs/3d.md).  financial-chart-3d-framing-test.el checks every frame
;; reads, at five sizes and five phases.  Without graphics.el the 3D commands signal
;; `financial-chart-3d-unavailable' and 2D is untouched (a subprocess
;; without graphics.el on its load path).

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-3d)
(require 'graphics-eas)
(require 'graphics-frame)
(financial-chart-3d--ensure)

(defun financial-chart-3d-test--json (file)
  "FILE under the repository root, parsed as eas parses JSON."
  (eas-json-read-file (expand-file-name file financial-chart-test-root)))

(defun financial-chart-3d-test--json-rows (rows)
  "ROWS as they read back from the JSON they are written as."
  (append (eas-json-parse (json-encode (vconcat rows))) nil))

(defun financial-chart-3d-test--templates ()
  "Every 2D template file: (NAME . FILE) for templates/ and templates/financial/."
  (mapcar (lambda (f) (cons (file-name-base f) f))
          (append (directory-files (expand-file-name "templates" financial-chart-test-root) t "\\.json\\'")
                  (directory-files (expand-file-name "templates/financial" financial-chart-test-root) t "\\.json\\'"))))

(defun financial-chart-3d-test--without (plist key)
  "PLIST without KEY."
  (cl-loop for (k v) on plist by #'cddr unless (eq k key) append (list k v)))

(defmacro financial-chart-3d-test--with-view (spec &rest body)
  "Bind `view' to 3D variant SPEC = (NAME . OPEN-ARGS) around BODY, then close it."
  (declare (indent 1))
  `(let ((view (apply #'financial-chart-3d-open (car ,spec) (cdr ,spec))))
     (unwind-protect (progn ,@body)
       (financial-chart-3d--close view))))

;;; Coverage and shape

(ert-deftest financial-chart-3d-every-template-has-a-variant ()
  (should (equal (sort (mapcar #'car (financial-chart-3d-test--templates)) #'string<)
                 (sort (financial-chart-3d-variants) #'string<))))

(ert-deftest financial-chart-3d-variants-keep-their-template-body ()
  (dolist (entry (financial-chart-3d-test--templates))
    (let* ((name (car entry))
           (base (with-temp-buffer (insert-file-contents (cdr entry)) (eas-json-parse (buffer-string))))
           (variant (financial-chart-3d-test--json (format "templates3d/%s.json" name)))
           (meta (plist-get variant :x-eas))
           (base-meta (plist-get base :x-eas)))
      (ert-info ((format "variant %s" name))
        (should (equal (financial-chart-3d-test--without variant :x-eas)
                       (financial-chart-3d-test--without base :x-eas)))
        (should (equal (plist-get meta :template) (plist-get base-meta :template)))
        (should (equal (plist-get meta :base) name))
        ;; Its slots are the template's, plus any the 3D reading adds.
        (cl-loop for (slot def) on (plist-get base-meta :slots) by #'cddr
                 do (should (equal (plist-get (plist-get meta :slots) slot) def)))
        (should (member (plist-get (plist-get meta :graphics) :mark) (graphics-eas-all-marks)))
        (should (equal (eas-template-get (financial-chart-3d--name name))
                       (eas-template-get (financial-chart-3d--name name))))))))

(ert-deftest financial-chart-3d-variants-still-draw-in-2d ()
  (dolist (name (financial-chart-3d-variants))
    (ert-info ((format "variant %s" name))
      (let* ((qualified (financial-chart-3d--name name))
             (resolved (eas-resolve qualified (eas-template-example qualified))))
        (should-not (eas-spec-unsupported resolved))
        (should (> (length (eas-text-render (eas-compile resolved :target 'text :size '(:cols 60 :rows 14)))) 0))))))

(ert-deftest financial-chart-3d-series-variants-replay ()
  (dolist (name '("ohlc" "panes" "volume-profile" "drawdown" "payoff" "payoff-curves"))
    (ert-info ((format "variant %s" name))
      (financial-chart-3d-test--with-view (list name :id (concat "fc3d-test-replay:" name))
        (let ((timeline (graphics-eas-timeline view)))
          (should timeline)
          (should (> (plist-get timeline :duration) 0))
          (should (plist-get timeline :loop))
          ;; Its start, middle and end differ: the replay draws.
          (let ((frames (mapcar (lambda (phase)
                                  (financial-chart-3d-seek view phase)
                                  (graphics-eas-frame view 60 24))
                                '(0.0 0.5 1.0))))
            (should (= (length (delete-dups frames)) 3))))))))

(ert-deftest financial-chart-3d-replays-draw-every-row ()
  ;; A replay colours the rows so far and ghosts the rest: every frame
  ;; has every bar, from a third of them in colour at the start.
  (let* ((bars (financial-chart-3d--rows (plist-get (financial-chart-3d-test--json "examples/panes.data.json") :bars)))
         (graphics '(:mark "fc-candle3d" :timeline (:duration 8)))
         (colors (lambda (seconds)
                   (let ((graphics-eas-mark-params (list :time seconds)))
                     (append (plist-get (seq-find (lambda (p) (equal (plist-get p :id) "bodies"))
                                                  (financial-chart-3d--candle-mark graphics bars))
                                        :colors)
                             nil))))
         (ghosts (lambda (seconds) (seq-count (lambda (c) (equal c financial-chart-3d-ghost-color))
                                              (funcall colors seconds)))))
    (dolist (seconds '(0 4 8))
      (should (= (length (funcall colors seconds)) (length bars))))
    (should (= (funcall ghosts 0) (- (length bars) (max 8 (ceiling (length bars) 3)))))
    (should (< 0 (funcall ghosts 4) (funcall ghosts 0)))
    (should (= (funcall ghosts 8) 0))
    ;; Without a position (a mark lowered alone), all of it.
    (should (= (seq-count (lambda (c) (equal c financial-chart-3d-ghost-color))
                          (plist-get (aref (financial-chart-3d--candle-mark graphics bars) 2) :colors))
               0))))

(ert-deftest financial-chart-3d-payoff-surface-stays-whole ()
  ;; The surface has every row at every phase; the trace moves across it.
  (let* ((rows (append (plist-get (financial-chart-3d-test--json "examples/3d/payoff.data.json") :surface) nil))
         (graphics '(:mark "fc-payoff3d" :timeline (:duration 6)))
         (at (lambda (seconds)
               (let ((graphics-eas-mark-params (list :time seconds)))
                 (financial-chart-3d--payoff-mark graphics rows))))
         (find (lambda (prims id) (seq-find (lambda (p) (equal (plist-get p :id) id)) prims))))
    (dolist (seconds '(0 3 6))
      (let ((surface (funcall find (funcall at seconds) "surface")))
        (should (equal (plist-get surface :type) "heightfield"))
        (should (= (length (plist-get surface :heights)) 8))))
    (let ((z (lambda (seconds) (aref (aref (plist-get (funcall find (funcall at seconds) "front") :segments) 0) 2))))
      (should (< (funcall z 0) (funcall z 3) (funcall z 6))))))

;;; Goldens

(defun financial-chart-3d-test--golden (name frame)
  "Compare FRAME with test/golden/3d/NAME.{frame,txt} (EAS_UPDATE_GOLDEN rewrites)."
  (let* ((dir (expand-file-name "test/golden/3d" financial-chart-test-root))
         (file (expand-file-name (concat name ".frame") dir))
         (txt (expand-file-name (concat name ".txt") dir))
         (tmp (make-temp-file "fc3d-golden" nil ".frame")))
    (unwind-protect
        (progn
          (graphics-frame-write-file tmp frame 60 24)
          (if (or (getenv "EAS_UPDATE_GOLDEN") (not (file-exists-p file)))
              (progn (make-directory dir t)
                     (copy-file tmp file t)
                     (let ((coding-system-for-write 'utf-8-unix))
                       (write-region (graphics-eas-braille-text frame 60) nil txt nil 'silent))
                     (unless (getenv "EAS_UPDATE_GOLDEN")
                       (ert-fail (format "Golden %s was missing and has been written; review and rerun" name))))
            (should (equal (cons name (with-temp-buffer (set-buffer-multibyte nil)
                                                        (insert-file-contents-literally tmp) (buffer-string)))
                           (cons name (with-temp-buffer (set-buffer-multibyte nil)
                                                        (insert-file-contents-literally file) (buffer-string)))))
            (should (equal (graphics-eas-braille-text frame 60)
                           (with-temp-buffer (let ((coding-system-for-read 'utf-8))
                                               (insert-file-contents txt))
                                             (buffer-string))))))
      (delete-file tmp))))

(ert-deftest financial-chart-3d-goldens ()
  (graphics-ensure-core 'rs)
  (pcase-dolist (`(,phase . ,sub) financial-chart-3d-golden-phases)
    (dolist (name (financial-chart-3d-variants))
      (financial-chart-3d-test--golden (if (string-empty-p sub) name (concat sub "/" name))
                                       (financial-chart-3d-frame name nil nil nil phase)))))

(ert-deftest financial-chart-3d-gallery-page-shows-every-golden ()
  (let ((page (with-temp-buffer
                (insert-file-contents (expand-file-name "docs/3d.md" financial-chart-test-root))
                (buffer-string)))
        (generated (make-temp-file "fc3d-page" nil ".md")))
    (unwind-protect
        (progn
          (financial-chart-3d-write-gallery generated)
          (should (equal page (with-temp-buffer (insert-file-contents generated) (buffer-string)))))
      (delete-file generated))
    (dolist (name (financial-chart-3d-variants))
      (should (string-search (format "\n## %s\n" name) page))
      (dolist (file (cons (format "test/golden/3d/%s.txt" name)
                          (and (plist-get (financial-chart-3d--describe name) :replay)
                               (list (format "test/golden/3d/t0.5/%s.txt" name)))))
        (should (string-search (with-temp-buffer
                                 (insert-file-contents (expand-file-name file financial-chart-test-root))
                                 (buffer-string))
                               page))))))

;;; Framing and the gallery

(ert-deftest financial-chart-3d-camera-fits-the-data ()
  ;; A flat box seen from the default camera: the fit centres it and
  ;; comes closer than the core's bounding-sphere fit; twice the box,
  ;; twice as far.
  (let* ((box (lambda (k) (cl-loop for x in '(-6.0 6.0)
                                   append (cl-loop for y in '(0.0 1.0)
                                                   append (cl-loop for z in '(-4.5 4.5)
                                                                   collect (vector (* k x) (* k y) (* k z)))))))
         (fit (financial-chart-3d-camera-fit (funcall box 1) '(:yaw 30 :pitch 24)))
         (sphere (* 1.05 (/ (sqrt (+ 36 0.25 20.25)) (sin (degrees-to-radians 20))))))
    (should (< (abs (aref (plist-get fit :target) 0)) 0.5))
    (should (< (plist-get fit :distance) sphere))
    ;; Projected as the core projects (raster.rs), the corners fill
    ;; `financial-chart-3d-fill' of the shorter side, or of the width
    ;; allowed across, and no more.
    (let* ((yaw (degrees-to-radians 30)) (pitch (degrees-to-radians 24)) (tan (tan (degrees-to-radians 20)))
           (offset (vector (* (cos pitch) (sin yaw)) (sin pitch) (* (cos pitch) (cos yaw))))
           (eye (cl-map 'vector (lambda (t0 o) (+ t0 (* (plist-get fit :distance) o))) (plist-get fit :target) offset))
           (forward (cl-map 'vector #'- offset))
           (right (let ((r (financial-chart-3d--cross forward [0.0 1.0 0.0])))
                    (cl-map 'vector (lambda (v) (/ v (sqrt (financial-chart-3d--dot r r)))) r)))
           (up (financial-chart-3d--cross right forward))
           (extent (cl-loop for p in (funcall box 1)
                            maximize (let* ((v (cl-map 'vector #'- p eye))
                                            (depth (financial-chart-3d--dot v forward)))
                                       (max (/ (abs (financial-chart-3d--dot v right))
                                               (* depth tan financial-chart-3d-pane-aspect))
                                            (/ (abs (financial-chart-3d--dot v up)) (* depth tan)))))))
      (should (< (abs (- extent financial-chart-3d-fill)) 0.01)))
    (should (< (abs (- (plist-get (financial-chart-3d-camera-fit (funcall box 2) '(:yaw 30 :pitch 24)) :distance)
                       (* 2 (plist-get fit :distance))))
               0.01)))
  ;; Every variant's view gets one, its own camera's keys kept.
  (financial-chart-3d-test--with-view '("ohlc" :id "fc3d-test-fit")
    (let ((camera (plist-get (graphics-eas-view-graphics (graphics-eas-get view)) :camera)))
      (should (plist-get camera :distance))
      (should (plist-get camera :target)))))

(ert-deftest financial-chart-3d-gallery-grid-tiles-evenly ()
  ;; Ten panes: two rows of five, not three of four with the last row
  ;; two panes wide.
  (should (equal (financial-chart-3d--grid 10 216 60) '(5 5)))
  (dolist (n '(1 2 7 10 11 13 17))
    (dolist (size '((216 . 60) (80 . 24) (300 . 80)))
      (let ((counts (financial-chart-3d--grid n (car size) (cdr size))))
        (should (= (apply #'+ counts) n))
        (should (<= (- (apply #'max counts) (apply #'min counts)) 1))))))

(ert-deftest financial-chart-3d-gallery-tiles-its-frame ()
  (let ((names (financial-chart-3d-variants)))
    (save-window-excursion
      (unwind-protect
          (progn
            (financial-chart-3d-gallery names)
            (let* ((panes (window-list nil 'nomini))
                   (rows (seq-group-by (lambda (w) (cadr (window-edges w))) panes)))
              (should (= (length panes) (length names)))
              ;; Each row's panes are as wide as each other, to a cell.
              (dolist (row rows)
                (let ((widths (mapcar #'window-total-width (cdr row))))
                  (should (<= (- (apply #'max widths) (apply #'min widths)) 1))))))
        (dolist (name names)
          (when-let* ((view (gethash (financial-chart-3d--name name) eas-views)))
            (financial-chart-3d--close view)))))))

;;; The readings compute what the 2D charts compute

(ert-deftest financial-chart-3d-orbits-through-eas-params ()
  (financial-chart-3d-test--with-view '("ladder" :id "fc3d-test-orbit")
    (let ((before (graphics-eas-frame view 60 24)))
      (eas-dispatch view '(:type "params" :values (:yaw 120)))
      (should (equal (plist-get (graphics-eas-params view) :yaw) 120))
      (should-not (equal before (graphics-eas-frame view 60 24))))))

(ert-deftest financial-chart-3d-payoff-surface ()
  (let* ((payoff (plist-get (financial-chart-3d-test--json "examples/payoff.data.json") :data))
         (rows (financial-chart-3d-payoff-surface payoff :vol 0.3 :days [30 0]))
         (at (lambda (dte price) (plist-get (seq-find (lambda (r) (and (= (plist-get r :dte) dte)
                                                                        (= (plist-get r :price) price)))
                                                       rows)
                                            :pnl))))
    (should (= (length rows) (* 2 (length payoff))))
    ;; Day 0 is the expiry payoff itself.
    (seq-doseq (p payoff)
      (should (= (funcall at 0 (plist-get p :price)) (plist-get p :pnl))))
    ;; A long straddle 30 days out is worth more than at expiry at the
    ;; strike (time value), about 0.8 * S * vol * sqrt(t) of premium.
    (let ((time-value (- (funcall at 30 100.0) (funcall at 0 100.0))))
      (should (< (abs (- time-value (* 0.8 100 0.3 (sqrt (/ 30 365.0))))) 0.3)))
    (should (equal (mapcar (lambda (r) (plist-get r :day)) (seq-take rows 1)) '(0)))
    (should-error (financial-chart-3d-payoff-surface nil) :type 'financial-chart-invalid-data)))

(ert-deftest financial-chart-3d-payoff-example-is-generated ()
  (let ((example (financial-chart-3d-test--json "examples/3d/payoff.data.json")))
    (should (equal (append (plist-get example :surface) nil)
                   (financial-chart-3d-test--json-rows
                    (financial-chart-3d-payoff-surface (plist-get example :data)))))
    ;; Bindings without a surface get one, so they replay.
    (let ((bindings (financial-chart-3d-bindings "payoff" (list :data (plist-get example :data)))))
      (should (equal (length (plist-get bindings :surface)) (length (plist-get example :surface)))))))

(ert-deftest financial-chart-3d-payoff-mark-prices-expiry-rows ()
  ;; Opened with expiry rows only (no surface), the mark prices them itself.
  (let* ((payoff (plist-get (financial-chart-3d-test--json "examples/payoff.data.json") :data))
         (prims (financial-chart-3d--payoff-mark '(:mark "fc-payoff3d" :vol 0.3 :days [30 10 0])
                                                 (append payoff nil)))
         (surface (seq-find (lambda (p) (equal (plist-get p :id) "surface")) prims)))
    (should (equal (plist-get surface :type) "heightfield"))
    (should (= (length (plist-get surface :heights)) 3))
    (should (= (length (aref (plist-get surface :heights) 0)) (length payoff)))))

(ert-deftest financial-chart-3d-volume-profile-matches-2d ()
  (let* ((bars (financial-chart-3d--rows (plist-get (financial-chart-3d-test--json "examples/volume-profile.data.json")
                                                    :bars)))
         (profile (financial-chart-volume-profile bars 24))
         (volumes (financial-chart-3d-profile-volumes bars (plist-get profile :low) (plist-get profile :high) 24)))
    (should (= (length volumes) 24))
    (cl-loop for a across volumes for b in (plist-get profile :volumes)
             do (should (< (abs (- a b)) 1e-6)))))

(ert-deftest financial-chart-3d-panes-rsi-is-financial-charts ()
  (let* ((bars (financial-chart-3d--rows (plist-get (financial-chart-3d-test--json "examples/panes.data.json") :bars)))
         (prims (financial-chart-3d--panes-mark '(:mark "fc-panes3d") bars))
         (rsi (seq-find (lambda (p) (equal (plist-get p :id) "rsi")) prims))
         (valid (seq-count #'identity (financial-chart-rsi bars 14))))
    (should (vectorp prims))
    (should (= (length (plist-get rsi :segments)) (1- valid)))
    (should (seq-find (lambda (p) (equal (plist-get p :id) "price-bodies")) prims))
    (should (seq-find (lambda (p) (equal (plist-get p :id) "volume-bars")) prims))))

(ert-deftest financial-chart-3d-drawdown-and-depth-rows ()
  (should (equal (financial-chart-3d-drawdown-rows '((:x 1 :y 100) (:x 2 :y 80) (:x 3 :y 120) (:x 4 :y 90)) :x :y)
                 '((:x 1 :depth 0.0) (:x 2 :depth 20.0) (:x 3 :depth 0.0) (:x 4 :depth 25.0))))
  (should (equal (financial-chart-3d-cumulative-depth
                  '((:side "bid" :price 99 :size 1) (:side "bid" :price 98 :size 2)
                    (:side "ask" :price 101 :size 3) (:side "ask" :price 102 :size 4)))
                 '((:price 98.0 :cumulative 3.0 :side "bid") (:price 99.0 :cumulative 1.0 :side "bid")
                   (:price 101.0 :cumulative 3.0 :side "ask") (:price 102.0 :cumulative 7.0 :side "ask")))))

;;; Live books

(ert-deftest financial-chart-3d-book-streams-a-surface ()
  (let* ((eas-stream-use-timers nil)
         ;; A second passes between pushes: each frame clears the cap.
         (clock 0)
         (eas-stream-clock (lambda () (cl-incf clock)))
         (snapshot (financial-chart-3d--example-snapshot))
         (batches (financial-chart-3d-book-deltas snapshot 3))
         (view (financial-chart-3d-book-open snapshot :levels 8 :id "fc3d-test-book")))
    (unwind-protect
        (let ((first (graphics-eas-frame view 60 24)))
          (dolist (batch batches)
            (should (plist-get (financial-chart-3d-book-push view batch 0) :step)))
          (let ((rows (plist-get (eas-view-data view) :rows)))
            (should (equal (delete-dups (mapcar (lambda (r) (plist-get r :step)) rows)) '(0 1 2 3))))
          (should-not (equal first (graphics-eas-frame view 60 24))))
      (financial-chart-3d-book-close view))
    (should-error (financial-chart-3d-book-push view (car batches)) :type 'financial-chart-invalid-book)))

(ert-deftest financial-chart-3d-ladder-book-replaces-its-bars ()
  (let* ((eas-stream-use-timers nil)
         (snapshot (financial-chart-3d--example-snapshot))
         (view (financial-chart-3d-book-open snapshot :template "ladder" :levels 8 :id "fc3d-test-ladder")))
    (unwind-protect
        (let ((n (length (plist-get (eas-view-data view) :rows))))
          (financial-chart-3d-book-push view (car (financial-chart-3d-book-deltas snapshot 1)) 0)
          (should (= (length (plist-get (eas-view-data view) :rows)) n)))
      (financial-chart-3d-book-close view)))
  (should-error (financial-chart-3d-book-open nil :template "nope") :type 'financial-chart-invalid-book))

(ert-deftest financial-chart-3d-depth-live-example-is-generated ()
  (should (equal (append (plist-get (financial-chart-3d-test--json "examples/3d/depth-live.data.json") :data) nil)
                 (financial-chart-3d-test--json-rows
                  (financial-chart-3d-book-history (financial-chart-3d--example-snapshot) 39)))))

;;; Without graphics.el

(ert-deftest financial-chart-3d-unavailable-is-a-clear-error ()
  (let ((financial-chart-3d-graphics-feature 'financial-chart-3d-test-no-such-feature))
    (should-not (financial-chart-3d-available-p))
    (let ((err (should-error (financial-chart-3d-open "ohlc") :type 'financial-chart-3d-unavailable)))
      (should (string-match-p "graphics-el" (cadr err)))
      (should (equal (plist-get (cddr err) :code) "GRAPHICS_MISSING")))
    (should-error (financial-chart-3d-gallery) :type 'financial-chart-3d-unavailable)
    (should-error (financial-chart-3d-book-open (financial-chart-3d--example-snapshot))
                  :type 'financial-chart-3d-unavailable)))

(ert-deftest financial-chart-3d-loads-and-draws-2d-without-graphics ()
  (let* ((eas (file-name-directory (locate-library "eas")))
         (root financial-chart-test-root)
         (dirs (mapcar (lambda (d) (expand-file-name d root))
                       '("src" "src/core" "src/indicators" "src/charts" "src/integrations")))
         (form `(progn
                  (require 'financial-chart-3d)
                  (unless (and (not (featurep 'graphics-eas)) (not (financial-chart-3d-available-p)))
                    (error "graphics.el was found"))
                  (unless (> (length (eas-text-render (eas-compile (eas-resolve "fc3d/ohlc" (eas-template-example "fc3d/ohlc"))
                                                              :target 'text :size '(:cols 60 :rows 14))))
                             0)
                    (error "fc3d/ohlc did not draw in 2D"))
                  (unless (equal (financial-chart-plot 'payoff '((90 -5) (100 0) (110 5)) :backend 'text
                                                       :width 30 :height 6)
                                 (financial-chart-eas-render 'payoff '((90 -5) (100 0) (110 5)) 'text
                                                             :width 30 :height 6))
                    (error "2D plot changed"))
                  (condition-case err (progn (financial-chart-3d-open "ohlc") (error "no error"))
                    (financial-chart-3d-unavailable (princ (format "OK %s" (plist-get (cddr err) :code)))))))
         (output (with-temp-buffer
                   (list (apply #'call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" eas
                                (append (mapcan (lambda (d) (list "-L" d)) dirs)
                                        (list "--eval" (prin1-to-string form))))
                         (buffer-string)))))
    (should (equal (car output) 0))
    (should (string-match-p "OK GRAPHICS_MISSING" (cadr output)))))

(provide 'financial-chart-3d-test)
;;; financial-chart-3d-test.el ends here
