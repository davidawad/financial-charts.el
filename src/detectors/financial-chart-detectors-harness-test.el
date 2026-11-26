;;; financial-chart-detectors-harness-test.el --- tests every registered detector gets -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty.6: the generic harness of docs/design/markups.md, keyed off
;; `financial-chart-detector-registry', so a new detector is tested the
;; moment it registers:
;;
;;   lint         every entry has its citation, lag, typed params,
;;                invariance and an example that compiles
;;   schema       events are event/v1, confirmed no earlier than they
;;                happen, a fixed lag where one is declared
;;   truncation   detecting on bars 0..k gives exactly the full run's
;;                events confirmed by k (no look-ahead)
;;   invariance   scaling prices (and shifting them, where declared)
;;                changes no event: same ids, strength and evidence,
;;                geometry transformed
;;
;; Truncation and invariance run on the defaults and on each of an
;; entry's :variants, with the invariance each declares.
;;   determinism  two runs encode to the same JSON
;;   oracle       with :oracle-params an entry equals its TA-Lib
;;                function bar for bar on the oracle bars; with its
;;                defaults it fires on a subset of them
;;
;; The oracle goldens are test/fixtures/detectors/talib.json, written by
;; test/oracles/talib-goldens; the tests skip when they are absent.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defconst financial-chart-harness-test--fixtures
  (expand-file-name "test/fixtures/detectors" financial-chart-test-root)
  "The oracle bars and goldens.")

(defvar financial-chart-harness-test--bars-cache nil)

(defun financial-chart-harness-test--bars (&optional n)
  "The first N (default all) oracle bars as a vector."
  (unless financial-chart-harness-test--bars-cache
    (setq financial-chart-harness-test--bars-cache
          (vconcat (eas-json-read-file (expand-file-name "oracle-bars.json" financial-chart-harness-test--fixtures)))))
  (if n (seq-subseq financial-chart-harness-test--bars-cache 0 n) financial-chart-harness-test--bars-cache))

(defun financial-chart-harness-test--close (a b)
  "Non-nil when numbers A and B agree to a relative 1e-6."
  (<= (abs (- a b)) (* 1e-6 (max 1.0 (abs a) (abs b)))))

(defun financial-chart-harness-test--map-y (geometry fn)
  "GEOMETRY with FN applied to every price in it."
  (let ((point (lambda (p) (vector (aref p 0) (funcall fn (aref p 1))))))
    (cl-loop for (k v) on geometry by #'cddr
             append (list k (pcase k
                              (:y (funcall fn v))
                              (:levels (vconcat (mapcar (lambda (l) (list :ratio (plist-get l :ratio)
                                                                          :y (funcall fn (plist-get l :y))))
                                                        v)))
                              (:points (vconcat (mapcar point v)))
                              ((or :anchor (guard (and (memq k '(:from :to)) (vectorp v)))) (funcall point v))
                              (_ v))))))

(defun financial-chart-harness-test--same (a b)
  "Non-nil when A and B are equal, numbers to a relative 1e-6."
  (cond ((and (numberp a) (numberp b)) (financial-chart-harness-test--close a b))
        ((and (vectorp a) (vectorp b)) (and (= (length a) (length b)) (cl-every #'financial-chart-harness-test--same a b)))
        ((and (consp a) (consp b)) (and (= (length a) (length b)) (cl-every #'financial-chart-harness-test--same a b)))
        (t (equal a b))))

(ert-deftest financial-chart-harness-registry-lint-is-clean ()
  (should (> (length (financial-chart-detector-names)) 20))
  (should (equal (financial-chart-detector-lint) nil))
  ;; Each entry's example compiles.
  (dolist (example (delete-dups (mapcar (lambda (e) (plist-get (cdr e) :example)) financial-chart-detector-registry)))
    (should (financial-chart-compose (expand-file-name example financial-chart-root)))))

(ert-deftest financial-chart-harness-events-are-event-v1 ()
  (let ((bars (financial-chart-harness-test--bars 800)))
    (dolist (name (financial-chart-detector-names))
      (let* ((entry (financial-chart-detector name))
             (params (financial-chart-detector-params name nil))
             (lag (plist-get entry :lag))
             (lag (cond ((integerp lag) lag)
                        ((and (symbolp lag) (not (eq lag 'variable)))
                         (plist-get params (intern (concat ":" (symbol-name lag))))))))
        (dolist (e (financial-chart-detect name bars))
          (should (equal (list name (cl-loop for k in '(:id :detector :params :index :time :confirmed_index
                                                          :confirmed_time :direction :strength :geometry :evidence)
                                             always (plist-member e k)))
                         (list name t)))
          (should (string-prefix-p (concat name "@") (plist-get e :id)))
          (should (member (plist-get e :direction) '("bullish" "bearish" "neutral")))
          (should (<= 0 (plist-get e :strength) 1))
          (should (>= (plist-get e :confirmed_index) (plist-get e :index)))
          (when lag (should (= (- (plist-get e :confirmed_index) (plist-get e :index)) lag)))
          ;; Evidence is data: numbers, strings or booleans.
          (cl-loop for (_ v) on (plist-get e :evidence) by #'cddr
                   do (should (or (numberp v) (stringp v) (memq v '(t :false))))))
        ;; Ids are unique.
        (let ((ids (mapcar (lambda (e) (plist-get e :id)) (financial-chart-detect name bars))))
          (should (= (length ids) (length (delete-dups (copy-sequence ids))))))))))

(defun financial-chart-harness-test--cases (name)
  "Detector NAME's parameter sets to test: ((PARAMS . INVARIANCE) ...).
Its defaults with its :invariance, then each of its :variants."
  (let ((e (financial-chart-detector name)))
    (cons (cons nil (plist-get e :invariance)) (plist-get e :variants))))

(ert-deftest financial-chart-harness-no-look-ahead ()
  (let* ((bars (financial-chart-harness-test--bars 500))
         (cuts (number-sequence 15 499 41)))
    (dolist (name (financial-chart-detector-names))
      (pcase-dolist (`(,params . ,_) (financial-chart-harness-test--cases name))
        (let ((full (financial-chart-detect name bars params)))
          (dolist (k cuts)
            (should (equal (list name params k (financial-chart-detect name (seq-subseq bars 0 (1+ k)) params))
                           (list name params k (seq-filter (lambda (e) (<= (plist-get e :confirmed_index) k)) full))))))))))

(ert-deftest financial-chart-harness-price-invariance ()
  (let ((bars (financial-chart-harness-test--bars 600)))
    (dolist (name (financial-chart-detector-names))
     (pcase-dolist (`(,params . ,invariance) (financial-chart-harness-test--cases name))
      (dolist (transform (list (cons 'scale (lambda (y) (* 4.0 y)))
                               (cons 'scale (lambda (y) (* 0.37 y)))
                               (cons 'shift (lambda (y) (+ y 250.0)))))
        (when (memq (car transform) invariance)
          (let* ((fn (cdr transform))
                 (moved (vconcat (mapcar (lambda (b) (cl-loop for (k v) on b by #'cddr
                                                              append (list k (if (memq k '(:open :high :low :close))
                                                                                 (funcall fn v) v))))
                                         bars)))
                 (a (financial-chart-detect name bars params))
                 (b (financial-chart-detect name moved params)))
            (should (equal (list name (car transform) (mapcar (lambda (e) (plist-get e :id)) b))
                           (list name (car transform) (mapcar (lambda (e) (plist-get e :id)) a))))
            (cl-loop for x in a for y in b
                     do (should (equal (list name (plist-get x :id) t)
                                       (list name (plist-get x :id)
                                             (and (equal (plist-get x :direction) (plist-get y :direction))
                                                  (= (plist-get x :confirmed_index) (plist-get y :confirmed_index))
                                                  (financial-chart-harness-test--close (plist-get x :strength) (plist-get y :strength))
                                                  (financial-chart-harness-test--same (plist-get x :evidence) (plist-get y :evidence))
                                                  (financial-chart-harness-test--same
                                                   (financial-chart-harness-test--map-y (plist-get x :geometry) fn)
                                                   (plist-get y :geometry))))))))))))))

(ert-deftest financial-chart-harness-determinism ()
  (let ((bars (financial-chart-harness-test--bars 400)))
    (dolist (name (financial-chart-detector-names))
      (should (equal (eas-json-encode (vconcat (financial-chart-detect name bars)))
                     (eas-json-encode (vconcat (financial-chart-detect name (copy-sequence bars)))))))))

(defun financial-chart-harness-test--oracle ()
  "The TA-Lib goldens, or nil when not generated."
  (let ((file (expand-file-name "talib.json" financial-chart-harness-test--fixtures)))
    (and (file-readable-p file) (eas-json-read-file file))))

(defun financial-chart-harness-test--hits (names params-of)
  "(INDEX . VALUE) of detectors NAMES on the oracle bars as TA-Lib scores them.
PARAMS-OF gives a detector's parameters."
  (sort (cl-loop for n in names
                 for entry = (financial-chart-detector n)
                 append (mapcar (lambda (e)
                                  (cons (plist-get e :index)
                                        (* (if (and (plist-get entry :oracle-sign)
                                                    (equal (plist-get e :direction) "bearish"))
                                               -1 1)
                                           (if (eq (plist-get (plist-get e :evidence) :strict) :false) 80 100))))
                                (financial-chart-detect n (financial-chart-harness-test--bars) (funcall params-of n))))
        (lambda (a b) (< (car a) (car b)))))

(ert-deftest financial-chart-harness-talib-oracle ()
  (let ((gold (financial-chart-harness-test--oracle)) (by (make-hash-table :test #'equal)))
    (skip-unless gold)
    (dolist (name (financial-chart-detector-names))
      (when-let* ((o (plist-get (financial-chart-detector name) :oracle)))
        (push name (gethash (string-remove-prefix "talib:" o) by))))
    (should (> (hash-table-count by) 15))
    (maphash
     (lambda (fn names)
       (let* ((g (plist-get (plist-get gold :functions) (intern (concat ":" fn))))
              (theirs (mapcar (lambda (h) (cons (aref h 0) (aref h 1))) (plist-get g :hits))))
         (should g)
         (should (equal (list fn (plist-get g :lookback))
                        (list fn (plist-get (financial-chart-detector (car names)) :lookback))))
         ;; Under the oracle's parameters: bar for bar.
         (should (equal (list fn (financial-chart-harness-test--hits
                                  names (lambda (n) (plist-get (financial-chart-detector n) :oracle-params))))
                        (list fn theirs)))
         ;; With the defaults (Nison's trend): a subset, as each entry's :deviations says.
         (let ((ours (financial-chart-harness-test--hits names (lambda (_) nil))))
           (should (equal (list fn (cl-set-difference ours theirs :test #'equal)) (list fn nil)))
           (when (cl-some (lambda (n) (plist-get (financial-chart-detector n) :oracle-params)) names)
             (should (cl-every (lambda (n) (plist-get (financial-chart-detector n) :deviations)) names))))))
     by)))

(ert-deftest financial-chart-harness-detect-validates-and-filters ()
  (let ((bars (financial-chart-harness-test--bars 300)))
    (let ((err (should-error (financial-chart-detect "pattern.hamer" bars) :type 'financial-chart-invalid-detection)))
      (should (equal (plist-get (cddr err) :code) "UNKNOWN_DETECTOR"))
      (should (string-match-p "pattern\\.hammer" (cadr err))))
    (let ((err (should-error (financial-chart-detect "pattern.hammer" bars '(:trnd 3))
                             :type 'financial-chart-invalid-detection)))
      (should (equal (list (plist-get (cddr err) :code) (plist-get (cddr err) :path)) '("INVALID_PARAM" "/params/trnd"))))
    (let ((err (should-error (financial-chart-detect "swing.pivot" bars '(:left 0))
                             :type 'financial-chart-invalid-detection)))
      (should (equal (plist-get (cddr err) :path) "/params/left")))
    (let ((all (financial-chart-detect "swing.pivot" bars)))
      (should (equal (financial-chart-detect "swing.pivot" bars nil :limit 2) (last all 2)))
      (should (seq-every-p (lambda (e) (>= (plist-get e :confirmed_index) 250))
                           (financial-chart-detect "swing.pivot" bars nil :since 50))))
    (let ((answer (financial-chart-detect-verb "swing.pivot" bars nil nil 1)))
      (should (eq (plist-get answer :ok) t))
      (should (= (length (plist-get (plist-get answer :data) :events)) 1))
      (should (stringp (eas-json-encode answer))))
    (should (equal (plist-get (financial-chart-detect-verb "nope" bars) :reason) "UNKNOWN_DETECTOR"))))

(provide 'financial-chart-detectors-harness-test)
;;; financial-chart-detectors-harness-test.el ends here
