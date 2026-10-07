;;; financial-chart-eas-book-test.el --- live order books on eas streaming -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.4: order-book snapshots and delta batches (validation,
;; atomicity), the rows the ladder and depth-live templates draw, and
;; live views fed by deltas through eas streaming: frame cap, hover
;; pause, the newest book winning a queued frame, flash.  Text and SVG
;; goldens of both templates live under test/golden/order-book/;
;; EAS_UPDATE_GOLDEN=1 (or FINANCIAL_CHART_UPDATE_GOLDEN=1) rewrites
;; them.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart-eas-book)
(require 'financial-chart-eas-book-bench)

(defun financial-chart-book-test--file (name)
  "The parsed JSON file NAME under examples/order-book/."
  (eas-json-read-file (expand-file-name (concat "examples/order-book/" name) financial-chart-test-root)))

(defun financial-chart-book-test--golden (name actual)
  "Compare ACTUAL with golden NAME under test/golden/order-book/."
  (let ((file (expand-file-name name (expand-file-name "test/golden/order-book"
                                                       financial-chart-test-root)))
        (update (or (getenv "EAS_UPDATE_GOLDEN") (getenv "FINANCIAL_CHART_UPDATE_GOLDEN"))))
    (if (or update (not (file-exists-p file)))
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert actual))
          (unless update
            (ert-fail (format "Golden %s was missing and has been written; review and rerun" name))))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) actual)))))

(defmacro financial-chart-book-test--should-code (code &rest body)
  "Assert BODY signals `financial-chart-invalid-book' with CODE; return its props."
  (declare (indent 1))
  `(let ((err (should-error (progn ,@body) :type 'financial-chart-invalid-book)))
     (should (equal (plist-get (cddr err) :code) ,code))
     (cddr err)))

(defun financial-chart-book-test--book ()
  "A small book: bids 100 (2), 99 (3); asks 101 (1), 102 (4)."
  (financial-chart-book-make '(:bids [[100 2] [99 3]] :asks [[101 1] [102 4]])))

(defun financial-chart-book-test--levels (book side)
  "BOOK's SIDE as ((PRICE . SIZE) ...), best first."
  (financial-chart-book--sorted book side nil))

;;; Snapshots

(ert-deftest financial-chart-book-make-accepts-every-level-form ()
  (dolist (snapshot '((:bids [[100 2] [99 3]] :asks [[101 1]])
                      (:bids ((100 2) (99 3)) :asks ((101 1)))
                      (:bids [(:price 100 :size 2) (:price 99 :size 3)] :asks [(:price 101 :size 1)])))
    (let ((book (financial-chart-book-make snapshot)))
      (should (equal (financial-chart-book-test--levels book :bids) '((100.0 . 2) (99.0 . 3))))
      (should (equal (financial-chart-book-test--levels book :asks) '((101.0 . 1))))))
  (let ((summary (financial-chart-book-summary (financial-chart-book-test--book))))
    (should (equal (plist-get summary :mid) 100.5))
    (should (equal (plist-get summary :spread) 1.0)))
  ;; One side, or none, is a book too.
  (should (eq (plist-get (financial-chart-book-summary (financial-chart-book-make '(:bids [[5 1]])))
                         :spread)
              :null)))

(ert-deftest financial-chart-book-make-names-the-bad-level ()
  (let ((props (financial-chart-book-test--should-code "INVALID_BOOK"
                 (financial-chart-book-make '(:bids [[100 2] [99 -3]] :asks [])))))
    (should (equal (plist-get props :index) 1))
    (should (equal (plist-get props :path) "/bids/1")))
  (should (equal (plist-get (financial-chart-book-test--should-code "INVALID_BOOK"
                              (financial-chart-book-make '(:bids [] :asks [[101 1] [101 2]])))
                            :path)
                 "/asks/1"))
  (financial-chart-book-test--should-code "INVALID_BOOK" (financial-chart-book-make [1 2]))
  (financial-chart-book-test--should-code "INVALID_BOOK" (financial-chart-book-make '(:levels [])))
  (financial-chart-book-test--should-code "CROSSED_BOOK"
    (financial-chart-book-make '(:bids [[102 1]] :asks [[101 1]])))
  ;; A financial-chart error, with a message that says how to fix it.
  (should (get 'financial-chart-invalid-book 'error-conditions))
  (should (memq 'financial-chart-error (get 'financial-chart-invalid-book 'error-conditions))))

;;; Deltas

(ert-deftest financial-chart-book-apply-inserts-updates-and-deletes ()
  (let ((book (financial-chart-book-test--book)))
    (financial-chart-book-apply book [(:op "insert" :side "bid" :price 99.5 :size 7)
                                      (:op "update" :side "ask" :price 101 :size 5)
                                      (:op "delete" :side "ask" :price 102)
                                      (:op "set" :side "ask" :price 103 :size 1)]
                                0)
    (should (equal (financial-chart-book-test--levels book :bids) '((100.0 . 2) (99.5 . 7) (99.0 . 3))))
    (should (equal (financial-chart-book-test--levels book :asks) '((101.0 . 5) (103.0 . 1))))
    ;; Without op: size 0 deletes, any other size sets.
    (financial-chart-book-apply book '((:side "bid" :price 99 :size 0) (:side "ask" :price 101 :size 6)) 1)
    (should (equal (financial-chart-book-test--levels book :bids) '((100.0 . 2) (99.5 . 7))))
    (should (equal (financial-chart-book-test--levels book :asks) '((101.0 . 6) (103.0 . 1))))
    (should (equal (financial-chart-book-deltas book) 6))))

(ert-deftest financial-chart-book-apply-rejects-out-of-sync-deltas-atomically ()
  (let* ((book (financial-chart-book-test--book))
         (before (list (financial-chart-book-test--levels book :bids)
                       (financial-chart-book-test--levels book :asks))))
    (dolist (case '(("DUPLICATE_LEVEL" 1 "/1" [(:op "update" :side "bid" :price 100 :size 9)
                                               (:op "insert" :side "bid" :price 99 :size 1)])
                    ("UNKNOWN_LEVEL" 0 "/0" [(:op "delete" :side "ask" :price 105)])
                    ("UNKNOWN_LEVEL" 1 "/1" [(:op "set" :side "bid" :price 98 :size 1)
                                             (:op "update" :side "bid" :price 97 :size 1)])
                    ("INVALID_DELTA" 0 "/0/op" [(:op "replace" :side "bid" :price 100 :size 1)])
                    ("INVALID_DELTA" 1 "/1/side" [(:side "bid" :price 100 :size 1)
                                                  (:side "buy" :price 100 :size 1)])
                    ("INVALID_DELTA" 0 "/0/price" [(:side "bid" :price "100" :size 1)])
                    ("INVALID_DELTA" 0 "/0/size" [(:op "insert" :side "bid" :price 98 :size -1)])
                    ("INVALID_DELTA" 0 "/0" [7])
                    ("CROSSED_BOOK" 1 "/1" [(:side "bid" :price 100.5 :size 1)
                                            (:side "bid" :price 101.5 :size 1)])))
      (let ((props (financial-chart-book-test--should-code (car case)
                     (financial-chart-book-apply book (nth 3 case) 0))))
        (should (equal (list (car case) (plist-get props :index) (plist-get props :path))
                       (list (car case) (nth 1 case) (nth 2 case)))))
      ;; Nothing of a failed batch sticks, not even its valid deltas.
      (should (equal (list (financial-chart-book-test--levels book :bids)
                           (financial-chart-book-test--levels book :asks))
                     before))
      (should (zerop (financial-chart-book-deltas book)))
      (should (zerop (hash-table-count (financial-chart-book-changed book)))))))

;;; Rows

(ert-deftest financial-chart-book-rows-are-levels-around-a-mid-row ()
  (let* ((book (financial-chart-book-test--book))
         (rows (financial-chart-book-rows book :now 0)))
    (should (equal (mapcar (lambda (r) (list (plist-get r :side) (plist-get r :price)
                                             (plist-get r :cumulative) (plist-get r :level)))
                           rows)
                   '(("ask" 102.0 5 1) ("ask" 101.0 1 0) ("mid" 100.5 0 -1)
                     ("bid" 100.0 2 0) ("bid" 99.0 5 1))))
    (should (equal (plist-get (aref rows 2) :label) "mid 100.5  spread 1"))
    ;; Every row has the same fields, so the stream's schema holds.
    (should (seq-every-p (lambda (r) (equal (eas-plist-keys r) (eas-plist-keys (aref rows 0)))) rows))
    ;; LEVELS caps each side, nearest first.
    (should (equal (mapcar (lambda (r) (plist-get r :price)) (financial-chart-book-rows book :levels 1 :now 0))
                   '(101.0 100.5 100.0)))
    ;; An empty side leaves the mid row at the other side's best.
    (let ((rows (financial-chart-book-rows (financial-chart-book-make '(:bids [[5 1]])) :now 0)))
      (should (equal (plist-get (aref rows 0) :price) 5.0))
      (should (equal (plist-get (aref rows 0) :label) "spread -")))
    (should (equal (length (financial-chart-book-rows (financial-chart-book-make nil) :now 0)) 1))))

;;; Tick size

(defun financial-chart-book-test--noisy (n)
  "A book whose N levels per side are computed as a feed would: 100.95 - 0.1 * i."
  (list :bids (vconcat (cl-loop for i below n collect (vector (- 100.95 (* 0.1 i)) (1+ i))))
        :asks (vconcat (cl-loop for i below n collect (vector (+ 101.05 (* 0.1 i)) (1+ i))))))

(ert-deftest financial-chart-book-infers-the-tick-from-price-steps ()
  ;; The feed's arithmetic leaves float noise: 101.05 + 0.1 * 11 is 102.14999999999999.
  (should (equal (number-to-string (+ 101.05 (* 0.1 11))) "102.14999999999999"))
  (let* ((book (financial-chart-book-make (financial-chart-book-test--noisy 12)))
         (summary (financial-chart-book-summary book))
         (rows (financial-chart-book-rows book :now 0)))
    (should (equal (plist-get summary :tick) 0.1))
    (should (equal (plist-get summary :decimals) 2))
    (should (equal (plist-get summary :mid) 101.0))
    (should (equal (plist-get summary :spread) 0.1))
    (should (equal (mapcar (lambda (r) (plist-get r :price_label)) (seq-take rows 3))
                   '("102.15" "102.05" "101.95")))
    (should (equal (plist-get (aref rows 12) :label) "mid 101.00  spread 0.10"))
    (should (equal (plist-get (aref rows 12) :price_label) "101.00"))
    ;; No label, price, mid or spread carries the noise.
    (seq-doseq (row rows)
      (dolist (key '(:price :mid :spread))
        (should (equal (number-to-string (plist-get row key))
                       (number-to-string (string-to-number (format "%.6f" (plist-get row key)))))))
      (should-not (string-match-p "[0-9]\{7,\}" (concat (plist-get row :price_label) (plist-get row :label))))))
  ;; A delta whose price is 102.15 exactly finds the snapshot's 102.14999999999999.
  (let ((book (financial-chart-book-make (financial-chart-book-test--noisy 12))))
    (financial-chart-book-apply book (vector (list :op "update" :side "ask" :price (+ 101.15 (* 0.1 10)) :size 9)) 0)
    (should (equal (cdr (assoc 102.15 (financial-chart-book-test--levels book :asks))) 9))
    ;; A finer price from a delta refines the precision.
    (financial-chart-book-apply book [(:op "insert" :side "ask" :price 101.075 :size 1)] 0)
    (should (equal (plist-get (financial-chart-book-summary book) :tick) 0.025))
    (should (equal (plist-get (aref (financial-chart-book-rows book :levels 1 :now 0) 0) :price_label)
                   "101.050"))))

(ert-deftest financial-chart-book-tick-option-sets-the-precision ()
  (let ((book (financial-chart-book-make '(:bids [[100 2] [99 3]] :asks [[101 1]] :tick 0.25))))
    (should (equal (plist-get (financial-chart-book-summary book) :tick) 0.25))
    (should (equal (mapcar (lambda (r) (plist-get r :price_label)) (financial-chart-book-rows book :now 0))
                   '("101.00" "100.50" "100.00" "99.00")))
    (should (equal (plist-get (aref (financial-chart-book-rows book :now 0) 1) :label)
                   "mid 100.50  spread 1.00")))
  ;; Whole-number prices print whole; a mid between them takes one place more.
  (let ((rows (financial-chart-book-rows (financial-chart-book-test--book) :now 0)))
    (should (equal (mapcar (lambda (r) (plist-get r :price_label)) rows) '("102" "101" "100.5" "100" "99"))))
  (financial-chart-book-test--should-code "INVALID_BOOK"
    (financial-chart-book-make '(:bids [[100 2]] :tick -1))))

(ert-deftest financial-chart-book-ladder-labels-noisy-prices-at-the-tick ()
  (let* ((rows (financial-chart-book-rows (financial-chart-book-make (financial-chart-book-test--noisy 6)) :now 0))
         (text (substring-no-properties
                (eas-text-render (eas-compile (eas-resolve "ladder" (list :data rows)) :target 'text
                                              :size '(:cols 80 :rows 30)))))
         (svg (eas-svg-render (eas-compile (eas-resolve "depth-live" (list :data rows)) :target 'svg
                                           :size '(640 . 360)))))
    (should (string-match-p "101\.55" text))
    (should (string-match-p "mid 101\.00  spread 0\.10" text))
    ;; Asks above bids: the band order is numeric, not the labels' text order.
    (should (< (string-match "101\.55" text) (string-match "100\.45" text)))
    (dolist (out (list text svg))
      (should-not (string-match-p "[0-9]\.[0-9]\{7,\}" out)))))

(ert-deftest financial-chart-book-summary-mid-and-spread-are-decimal ()
  "Cent prices give a cent mid and spread, not float noise on the axis."
  (let ((s (financial-chart-book-summary
            (financial-chart-book-make '(:bids [[99.89 1]] :asks [[100.09 1]])))))
    (should (equal (plist-get s :mid) 99.99))
    (should (equal (plist-get s :spread) 0.2))))

(ert-deftest financial-chart-book-rows-flash-changed-levels-for-a-while ()
  (let ((book (financial-chart-book-test--book))
        (changed (lambda (rows) (delq nil (mapcar (lambda (r) (and (eql (plist-get r :changed) 1)
                                                                   (plist-get r :price)))
                                                  rows)))))
    (financial-chart-book-apply book [(:side "bid" :price 99 :size 4) (:side "ask" :price 101.5 :size 1)] 10)
    (should (equal (funcall changed (financial-chart-book-rows book :flash 0.5 :now 10.2)) '(101.5 99.0)))
    (should-not (funcall changed (financial-chart-book-rows book :flash nil :now 10.2)))
    (should-not (funcall changed (financial-chart-book-rows book :flash 0.5 :now 10.6)))
    ;; Lapsed stamps are forgotten.
    (should (zerop (hash-table-count (financial-chart-book-changed book))))))

(ert-deftest financial-chart-book-examples-are-the-rows-of-the-example-deltas ()
  (let ((book (financial-chart-book-make (financial-chart-book-test--file "book.json"))))
    (financial-chart-book-apply book (financial-chart-book-test--file "deltas.json") 10)
    (dolist (template financial-chart-book-templates)
      (should (equal (eas-json-canonical (financial-chart-book-rows book :now 10.1))
                     (eas-json-canonical (plist-get (financial-chart-book-test--file
                                                  (format "%s.data.json" template))
                                                 :data)))))))

;;; Templates

(ert-deftest financial-chart-book-template-goldens ()
  (dolist (template financial-chart-book-templates)
    (let ((example (eas-template-example template)))
      (should (plist-get example :data))
      (financial-chart-book-test--golden
       (format "%s.txt" template)
       (substring-no-properties
        (eas-text-render (eas-compile (eas-resolve template example) :target 'text
                                      :size '(:cols 80 :rows 26)))))
      (let ((svg (eas-svg-render (eas-compile (eas-resolve template example) :target 'svg
                                              :size '(640 . 360)))))
        (should (string-prefix-p "<svg" svg))
        (financial-chart-book-test--golden
         (format "%s.svg" template) (concat (replace-regexp-in-string "><" ">\n<" svg) "\n"))))))

(ert-deftest financial-chart-book-frame-cap-follows-depth ()
  (should (equal (mapcar #'financial-chart-book-default-fps '(20 50 51 100 200)) '(10 10 8 8 5)))
  (financial-chart-book-test--live ((deep (financial-chart-book-bench-snapshot 150) :levels 150)
                                    (capped '(:bids [[1 1]]) :max-fps 3))
    (should (equal (plist-get (eas-stream-inspect deep) :max-fps) 5))
    (should (equal (plist-get (eas-stream-inspect deep) :window) 301))
    (should (equal (plist-get (eas-stream-inspect capped) :max-fps) 3))))

(ert-deftest financial-chart-book-bench-measures-every-part ()
  (let ((row (financial-chart-book-bench-one 5 'text "depth-live" :frames 2 :deltas 3)))
    (should (equal (plist-get row :rows) 11))
    (dolist (key '(:apply :push :draw :frame :worst))
      (should (and (numberp (plist-get row key)) (>= (plist-get row key) 0))))))

(ert-deftest financial-chart-book-templates-declare-streams ()
  (dolist (template financial-chart-book-templates)
    (should (equal (eas-stream-check (eas-stream-config template)) '(:max-fps 10 :window nil)))))

;;; Live views

(defmacro financial-chart-book-test--live (bindings &rest body)
  "Run BODY with a clock at 0, no timers, and BINDINGS of live book views.
Each binding is (VAR SNAPSHOT . OPEN-ARGS); views are closed after."
  (declare (indent 1))
  `(let* ((clock 0.0)
          (eas-stream-clock (lambda () clock))
          (eas-stream-use-timers nil)
          (eas-stream-hover-hold 2.0)
          ,@(mapcar (lambda (b) `(,(car b) (financial-chart-book-open ,(cadr b) ,@(cddr b) :target 'text)))
                    bindings))
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (b) `(ignore-errors (financial-chart-book-close ,(car b)))) bindings))))

(defun financial-chart-book-test--shown (view)
  "VIEW's data rows as (SIDE PRICE SIZE)."
  (mapcar (lambda (r) (list (plist-get r :side) (plist-get r :price) (plist-get r :size)))
          (plist-get (eas-view-data (eas-view-get view)) :rows)))

(ert-deftest financial-chart-book-live-frame-replaces-the-book ()
  (financial-chart-book-test--live
      ((view '(:bids [[100 2] [99 3]] :asks [[101 1] [102 4]]) :levels 5 :flash nil))
    (financial-chart-book-push view [(:op "delete" :side "ask" :price 102)
                                     (:op "insert" :side "bid" :price 98 :size 1)])
    (should (equal (financial-chart-book-test--shown view)
                   '(("ask" 101.0 1) ("mid" 100.5 0) ("bid" 100.0 2) ("bid" 99.0 3) ("bid" 98.0 1))))
    (setq clock 1.0)
    (financial-chart-book-push view [(:side "ask" :price 101 :size 0) (:side "ask" :price 100.5 :size 2)])
    ;; The view holds one book, never the history of frames.
    (should (equal (financial-chart-book-test--shown view)
                   '(("ask" 100.5 2) ("mid" 100.25 0) ("bid" 100.0 2) ("bid" 99.0 3) ("bid" 98.0 1))))
    (let ((inspect (financial-chart-book-inspect view)))
      (should (equal (plist-get inspect :spread) 0.5))
      (should (equal (plist-get inspect :deltas) 4))
      (should (equal (plist-get (plist-get inspect :stream) :frames) 2)))))

(ert-deftest financial-chart-book-live-caps-frames-and-folds-deltas ()
  (financial-chart-book-test--live ((view '(:bids [[100 2]] :asks [[101 1]]) :max-fps 5 :flash nil))
    (financial-chart-book-push view [(:side "bid" :price 100 :size 3)])
    (should (equal (plist-get (eas-stream-inspect view) :frames) 1))
    ;; Within 1/5 s the next frame waits; later deltas fold into the
    ;; book instead of queueing more snapshots.
    (setq clock 0.05)
    (financial-chart-book-push view [(:side "bid" :price 100 :size 4)])
    (setq clock 0.1)
    (financial-chart-book-push view [(:side "bid" :price 100 :size 5)])
    (financial-chart-book-push view [(:side "bid" :price 99 :size 1)])
    (should (equal (plist-get (eas-stream-inspect view) :pushes) 2))
    (should (eq (plist-get (financial-chart-book-inspect view) :dirty) t))
    (should (equal (cadr (assoc "bid" (financial-chart-book-test--shown view))) 100.0))
    (should (equal (nth 2 (assoc "bid" (financial-chart-book-test--shown view))) 3))
    ;; The capped frame lands, and the folded book is queued behind it.
    (setq clock 0.2)
    (should (eas-stream-tick view))
    (should (eq (plist-get (financial-chart-book-inspect view) :dirty) :false))
    (setq clock 0.4)
    (should (eas-stream-tick view))
    (should (equal (financial-chart-book-test--shown view)
                   '(("ask" 101.0 1) ("mid" 100.5 0) ("bid" 100.0 5) ("bid" 99.0 1))))
    (should (equal (plist-get (eas-stream-inspect view) :frames) 3))))

(ert-deftest financial-chart-book-live-pauses-while-hovering ()
  (financial-chart-book-test--live ((view '(:bids [[100 2]] :asks [[101 1]]) :flash nil))
    (eas-dispatch view '(:type "pointermove" :px [50 50]))
    (financial-chart-book-push view [(:side "ask" :price 101 :size 9)])
    (should (equal (plist-get (eas-stream-inspect view) :held) "pointer"))
    (should (equal (nth 2 (assoc "ask" (financial-chart-book-test--shown view))) 1))
    (setq clock 1.0)
    (should-not (eas-stream-tick view))
    ;; Leaving the chart catches up at once.
    (eas-dispatch view '(:type "pointerleave"))
    (should (equal (nth 2 (assoc "ask" (financial-chart-book-test--shown view))) 9))))

(ert-deftest financial-chart-book-live-flashes-and-unflashes ()
  (financial-chart-book-test--live ((view '(:bids [[100 2]] :asks [[101 1]]) :flash 0.5 :max-fps 10))
    (financial-chart-book-push view [(:side "bid" :price 100 :size 3)])
    (let ((flashed (lambda () (mapcar (lambda (r) (plist-get r :changed))
                                      (plist-get (eas-view-data (eas-view-get view)) :rows)))))
      (should (equal (funcall flashed) '(0 0 1)))
      ;; A later frame, once the flash has lapsed, draws it plain.
      (setq clock 1.0)
      (financial-chart-book--offer (financial-chart-book--live view))
      (should (equal (funcall flashed) '(0 0 0))))))

(ert-deftest financial-chart-book-live-reset-close-and-errors ()
  (financial-chart-book-test--live ((view '(:bids [[100 2]] :asks [[101 1]]) :flash nil :template "depth-live"))
    (should (equal (eas-view-template (eas-view-get view)) "depth-live"))
    ;; A failed batch leaves the drawn book alone.
    (financial-chart-book-test--should-code "UNKNOWN_LEVEL"
      (financial-chart-book-push view [(:op "update" :side "ask" :price 105 :size 1)]))
    (setq clock 1.0)
    (financial-chart-book-reset view '(:bids [[50 1]] :asks [[51 1] [52 2]]))
    (should (equal (financial-chart-book-test--shown view)
                   '(("ask" 52.0 2) ("ask" 51.0 1) ("mid" 50.5 0) ("bid" 50.0 1))))
    (financial-chart-book-close view)
    (should-not (member (eas-view-id view) (eas-view-ids)))
    (financial-chart-book-test--should-code "NO_BOOK" (financial-chart-book-push view [])))
  (financial-chart-book-test--should-code "UNKNOWN_TEMPLATE"
    (financial-chart-book-open '(:bids [[1 1]]) :template "candles")))

(provide 'financial-chart-eas-book-test)
;;; financial-chart-eas-book-test.el ends here
