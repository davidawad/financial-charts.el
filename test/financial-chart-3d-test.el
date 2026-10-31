;;; financial-chart-3d-test.el --- 3D variants of the templates via graphics.el -*- lexical-binding: t; -*-

;;; Commentary:

;; `make test-3d' runs these against the graphics.el and eas.el
;; checkouts `make deps-3d' prepares in test/deps/ (graphics.el's Rust
;; module built).  Every template has a 3D variant whose body is the
;; template's, which eas still draws in 2D and graphics.el draws in 3D
;; to its golden (test/golden/3d/NAME.{frame,txt}, 60x24, dark, the
;; Rust core; EAS_UPDATE_GOLDEN=1 rewrites them, `make golden-3d' also
;; rewrites docs/3d.md).  Without graphics.el the 3D commands signal
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
          (should (> (plist-get timeline :keyframes) 2))
          (should (plist-get timeline :loop))
          ;; Its start and its end differ: the replay draws.
          (graphics-eas-timeline-set view :seconds 0.0 :playing nil)
          (let ((start (graphics-eas-frame view 60 24)))
            (financial-chart-3d-seek-end view)
            (should-not (equal start (graphics-eas-frame view 60 24)))))))))

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
  (dolist (name (financial-chart-3d-variants))
    (financial-chart-3d-test--golden name (financial-chart-3d-frame name))))

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
      (should (string-search (with-temp-buffer
                               (insert-file-contents
                                (expand-file-name (format "test/golden/3d/%s.txt" name) financial-chart-test-root))
                               (buffer-string))
                             page)))))

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
