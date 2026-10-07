;;; financial-chart-test.el --- candlestick entry points, indicators, cohorts -*- lexical-binding: t; -*-

(require 'ert)
(require 'financial-chart)

(defmacro financial-chart-test--with-defaults (&rest body)
  "Run BODY with every financial-chart-* defcustom reset to its
standard value, so tests don't leak customizations across each other."
  `(let ((financial-chart-height 20)
         (financial-chart-max-bars 80)
         (financial-chart-show-volume t)
         (financial-chart-indicators nil)
         (financial-chart-oscillators nil)
         (financial-chart-png-converter nil))
     ,@body))

(defun financial-chart-test--bars (n)
  "Return N synthetic bars with distinct OHLCV/time values."
  (cl-loop for i from 0 below n
           collect (list :open (+ 100 i) :high (+ 105 i) :low (+ 95 i)
                         :close (+ 100 (if (cl-evenp i) 2 -2) i)
                         :volume (* 1000 (1+ i))
                         :time (* i 86400000))))

(ert-deftest financial-chart-version-matches-header ()
  "`financial-chart-version' is the Version header releases bump."
  (require 'lisp-mnt)
  (let ((file (locate-library "financial-chart.el" t)))
    (should file)
    (should (stringp financial-chart-version))
    (should (equal financial-chart-version (lm-version file)))))

(ert-deftest financial-chart-render-errors-on-no-bars ()
  (financial-chart-test--with-defaults
   (let ((err (should-error (financial-chart-render nil) :type 'financial-chart-invalid-data)))
     (should (equal (plist-get (cddr err) :code) "no_data")))))

(ert-deftest financial-chart-render-tolerates-zero-range-bars ()
  (financial-chart-test--with-defaults
   (let ((bars (list '(:open 100 :high 100 :low 100 :close 100))))
     (should (stringp (financial-chart-render bars 3))))))

;; -- bar-count windowing --

(ert-deftest financial-chart-max-bars-trims-to-most-recent ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-max-bars 3)
          (bars (financial-chart-test--bars 10))
          (bindings (financial-chart-eas-bindings 'ohlc bars)))
     (should (equal (plist-get bindings :bars) (last bars 3)))
     (should (stringp (financial-chart-render bars 12))))))

(ert-deftest financial-chart-max-bars-nil-disables-windowing ()
  (financial-chart-test--with-defaults
   (let* ((financial-chart-max-bars nil)
          (bars (financial-chart-test--bars 10)))
     (should (equal (plist-get (financial-chart-eas-bindings 'ohlc bars) :bars) bars)))))

(ert-deftest financial-chart-render-svg-errors-on-no-bars ()
  (financial-chart-test--with-defaults
   (should-error (financial-chart-render-svg nil) :type 'financial-chart-invalid-data)))

(ert-deftest financial-chart-render-svg-produces-well-formed-xml ()
  (financial-chart-test--with-defaults
   (let* ((bars (financial-chart-test--bars 5))
          (svg (financial-chart-render-svg bars "TEST")))
     (should (string-prefix-p "<svg" (string-trim svg)))
     (should (string-suffix-p "</svg>" (string-trim svg)))
     (should (string-match-p "<rect" svg))
     (should (string-match-p "<text" svg))
     (should (string-match-p "TEST" svg)))))

(ert-deftest financial-chart-export-svg-writes-file ()
  (financial-chart-test--with-defaults
   (let ((file (make-temp-file "financial-chart-test" nil ".svg")))
     (unwind-protect
         (progn
           (financial-chart-export-svg (financial-chart-test--bars 5) file "AAPL")
           (should (file-exists-p file))
           (with-temp-buffer
             (insert-file-contents file)
             (should (string-match-p "AAPL" (buffer-string)))
             (should (string-match-p "<svg" (buffer-string)))))
       (delete-file file)))))

(ert-deftest financial-chart-export-png-invokes-function-converter ()
  (financial-chart-test--with-defaults
   (let* ((calls nil)
          (svg-existed-at-call-time nil)
          (financial-chart-png-converter
           (lambda (svg-file png-file)
             (push (cons svg-file png-file) calls)
             (setq svg-existed-at-call-time (file-exists-p svg-file))
             (with-temp-file png-file (insert "fake-png-bytes"))))
          (out-file (make-temp-file "financial-chart-test" nil ".png")))
     (unwind-protect
         (progn
           (financial-chart-export-png (financial-chart-test--bars 3) out-file "AAPL")
           (should (= (length calls) 1))
           ;; the intermediate .svg existed at call time (it's deleted by
           ;; export-png's own unwind-protect right after this returns)
           (should svg-existed-at-call-time)
           (should (equal (cdar calls) out-file))
           (with-temp-buffer
             (insert-file-contents out-file)
             (should (equal (buffer-string) "fake-png-bytes"))))
       (delete-file out-file)))))

(ert-deftest financial-chart-export-png-cleans-up-intermediate-svg ()
  (financial-chart-test--with-defaults
   (let* ((captured-svg-file nil)
          (financial-chart-png-converter
           (lambda (svg-file png-file)
             (setq captured-svg-file svg-file)
             (with-temp-file png-file (insert "x"))))
          (out-file (make-temp-file "financial-chart-test" nil ".png")))
     (unwind-protect
         (progn
           (financial-chart-export-png (financial-chart-test--bars 3) out-file)
           (should-not (file-exists-p captured-svg-file)))
       (delete-file out-file)))))

(ert-deftest financial-chart-resolve-png-converter-errors-when-none-found ()
  (financial-chart-test--with-defaults
   (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
     (let ((err (should-error (financial-chart--resolve-png-converter) :type 'financial-chart-error)))
       (should (equal (plist-get (cddr err) :code) "no_png_converter"))))))

(ert-deftest financial-chart-resolve-png-converter-auto-detect-uses-real-executable-find ()
  ;; Regression test: `executable-find' takes a STRING, and the auto-detect
  ;; list is a list of SYMBOLS -- a prior version passed symbols straight
  ;; through and errored with "wrong-type-argument stringp" the moment a
  ;; converter actually needed to be found (every mocked test above hid
  ;; this, since none of them exercised the real `executable-find').
  ;; Deliberately does NOT mock `executable-find', so this only proves
  ;; anything on a machine with at least one of rsvg-convert/convert/magick
  ;; installed -- skip rather than false-negative if none are present.
  (financial-chart-test--with-defaults
   (if (cl-some #'executable-find '("rsvg-convert" "convert" "magick"))
       (should (memq (financial-chart--resolve-png-converter)
                     '(rsvg-convert convert magick)))
     (ert-skip "no rsvg-convert/convert/magick on this machine"))))

(ert-deftest financial-chart-resolve-png-converter-prefers-explicit-setting ()
  (financial-chart-test--with-defaults
   (let ((financial-chart-png-converter 'magick))
     (should (eq (financial-chart--resolve-png-converter) 'magick)))))

;; -- provider-agnostic SVG/PNG export + doctor hook --

(ert-deftest financial-chart-doctor-checks-shape-and-loadable ()
  (financial-chart-test--with-defaults
   (let ((checks (financial-chart-doctor-checks)))
     ;; every row is the eager doctor shape
     (dolist (c checks)
       (should (stringp (plist-get c :name)))
       (should (memq (plist-get c :status) '(pass fail skip)))
       (should (stringp (plist-get c :detail))))
     ;; every registered kind renders its example
     (should (cl-every (lambda (c) (eq (plist-get c :status) 'pass))
                       (cl-remove-if-not
                        (lambda (c) (string-prefix-p "kind " (plist-get c :name)))
                        checks))))))

;; -- built-in indicator functions --

(defun financial-chart-test--close-bars (closes)
  "Minimal bars with only :close set, for SMA/EMA/RSI tests."
  (mapcar (lambda (c) (list :close c)) closes))

(ert-deftest financial-chart-sma-nil-during-warmup-then-trailing-mean ()
  (let ((sma (financial-chart-sma
              (financial-chart-test--close-bars '(1 2 3 4 5)) 3)))
    (should (equal (nth 0 sma) nil))
    (should (equal (nth 1 sma) nil))
    (should (= (nth 2 sma) 2.0))
    (should (= (nth 3 sma) 3.0))
    (should (= (nth 4 sma) 4.0))))

(ert-deftest financial-chart-sma-respects-custom-field ()
  (let* ((bars (list '(:close 1 :open 10) '(:close 2 :open 20) '(:close 3 :open 30)))
         (sma (financial-chart-sma bars 2 :open)))
    (should (equal (nth 0 sma) nil))
    (should (= (nth 1 sma) 15.0))
    (should (= (nth 2 sma) 25.0))))

(ert-deftest financial-chart-ema-seeds-with-sma-then-smooths ()
  (let ((ema (financial-chart-ema
              (financial-chart-test--close-bars '(1 2 3 4 5)) 3)))
    (should (equal (nth 0 ema) nil))
    (should (equal (nth 1 ema) nil))
    (should (= (nth 2 ema) 2.0))
    (should (= (nth 3 ema) 3.0))
    (should (= (nth 4 ema) 4.0))))

(ert-deftest financial-chart-rsi-matches-hand-computed-example ()
  ;; closes 10,11,12,11,13 -> changes +1,+1,-1,+2 ; period 3
  (let ((rsi (financial-chart-rsi
              (financial-chart-test--close-bars '(10 11 12 11 13)) 3)))
    (should (equal (nth 0 rsi) nil))
    (should (equal (nth 1 rsi) nil))
    (should (equal (nth 2 rsi) nil))
    ;; window [+1,+1,-1]: avg-gain=2/3, avg-loss=1/3, RS=2 -> RSI=66.667
    (should (< (abs (- (nth 3 rsi) 66.6667)) 0.01))
    ;; window [+1,-1,+2]: avg-gain=1.0, avg-loss=1/3, RS=3 -> RSI=75.0
    (should (< (abs (- (nth 4 rsi) 75.0)) 0.01))))

(ert-deftest financial-chart-rsi-is-100-when-no-losses-in-window ()
  (let ((rsi (financial-chart-rsi
              (financial-chart-test--close-bars '(10 11 12 13)) 2)))
    (should (= (nth 3 rsi) 100.0))))

(ert-deftest financial-chart-rsi-stays-within-0-100-bounds ()
  (let ((rsi (financial-chart-rsi
              (financial-chart-test--close-bars
               '(100 95 110 88 120 80 130 70 140 60 150))
              3)))
    (dolist (v rsi)
      (when v
        (should (>= v 0.0))
        (should (<= v 100.0))))))

(ert-deftest financial-chart-vwap-matches-hand-computed-example ()
  (let* ((bars (list '(:high 10 :low 8 :close 9 :volume 100)
                     '(:high 12 :low 10 :close 11 :volume 200)))
         (vwap (financial-chart-vwap bars)))
    ;; bar1: typical=(10+8+9)/3=9, cum_pv=900, cum_vol=100 -> 9.0
    (should (< (abs (- (nth 0 vwap) 9.0)) 0.001))
    ;; bar2: typical=11, cum_pv=900+2200=3100, cum_vol=300 -> 10.3333
    (should (< (abs (- (nth 1 vwap) 10.3333)) 0.001))))

(ert-deftest financial-chart-vwap-nil-for-bars-without-volume ()
  (let* ((bars (list '(:high 10 :low 8 :close 9)
                     '(:high 12 :low 10 :close 11 :volume 200)))
         (vwap (financial-chart-vwap bars)))
    (should (equal (nth 0 vwap) nil))
    (should (numberp (nth 1 vwap)))))

(ert-deftest financial-chart-sma-usable-directly-as-indicator-overlay ()
  ;; Confirms the documented usage pattern actually works end to end.
  (financial-chart-test--with-defaults
   (let* ((financial-chart-show-volume nil)
          (financial-chart-indicators
           (list (list :fn (lambda (bars) (financial-chart-sma bars 3))
                       :label "SMA 3")))
          (bars (financial-chart-test--bars 10))
          (bindings (financial-chart-eas-bindings 'ohlc bars))
          (item (aref (plist-get bindings :indicators) 0)))
     (should (eq (plist-get bindings :volume) :false))
     (should (equal (plist-get item :as) "SMA 3"))
     (should (equal (append (plist-get item :values) nil) (financial-chart-sma bars 3)))
     (should (string-match-p "<path" (financial-chart-render-svg bars))))))

;; -- indicator cohorts --

(defconst financial-chart-test--rsi-record
  '((ref . ((kind . "indicator") (id . "finance.market.rsi-14")))
    (name . "Observed 14-period relative strength index")
    (revision . "1.0.0")
    (attributes
     . ((description . "RSI")
        (value . ((type . "array") (unit . "1") (nullable . t)
                  (item_type . "number") (scale . "linear")
                  (bounds . ((min . 0) (max . 100))))))))
  "Mock catalog record for a bounded (oscillator) indicator.")

(defconst financial-chart-test--sma-record
  '((ref . ((kind . "indicator") (id . "finance.market.sma")))
    (name . "Observed close simple moving average")
    (revision . "1.0.0")
    (attributes
     . ((description . "SMA")
        (value . ((type . "array") (unit . "usd/share") (nullable . t)
                  (item_type . "number") (scale . "linear"))))))
  "Mock catalog record for a price-scale indicator.")

(ert-deftest financial-chart-cohort-builtin-only-resolves-without-catalog ()
  "A builtin-only cohort resolves purely, never touching the catalog."
  (let ((called nil))
    (let ((financial-chart-indicator-catalog-function
           (lambda (&rest _) (setq called t) nil)))
      (let ((specs (financial-chart-resolve-cohort 'trend-following)))
        (should (= (length specs) 3))
        (should (cl-every (lambda (s) (functionp (plist-get s :fn))) specs))
        (should-not called)))))

(ert-deftest financial-chart-cohort-builtin-args-thread-into-closure ()
  "A builtin member's :args curry into the resolved one-arg overlay fn."
  (let* ((financial-chart-indicator-cohorts
          '((c :doc "d" :members ((:fn financial-chart-sma :args (5))))))
         (spec (car (financial-chart-resolve-cohort 'c)))
         (bars (financial-chart-test--bars 10)))
    (should (equal (funcall (plist-get spec :fn) bars)
                   (financial-chart-sma bars 5)))))

(ert-deftest financial-chart-cohort-catalog-member-resolves-with-param ()
  "A catalog member threads its :params through the evaluator table into
the builtin, purely (no bridge call needed for resolve)."
  (let ((financial-chart-indicator-cohorts
         '((cat :doc "catalog sma"
                :members ((:indicator "finance.market.sma" :params (:window 30)))))))
    (let* ((specs (financial-chart-resolve-cohort 'cat))
           (fn (plist-get (car specs) :fn))
           (bars (financial-chart-test--bars 40))
           (series (funcall fn bars)))
      (should (= (length specs) 1))
      (should (= (length series) 40))
      (should-not (nth 28 series))
      (should (numberp (nth 29 series)))
      (should (equal series (financial-chart-sma bars 30))))))

(ert-deftest financial-chart-cohort-unresolvable-member-signals-typed-error ()
  "A catalog member with no evaluator entry signals the typed error,
naming the member and the fix."
  (let ((financial-chart-indicator-cohorts
         '((bad :doc "no evaluator"
                :members ((:indicator "finance.market.bar-return" :params nil))))))
    (let ((err (should-error (financial-chart-resolve-cohort 'bad)
                             :type 'financial-chart-unresolvable-cohort)))
      (should (string-match-p "finance.market.bar-return" (cadr err)))
      (should (string-match-p "financial-chart-recipe-evaluators" (cadr err))))))

(ert-deftest financial-chart-cohort-unknown-name-signals ()
  "Resolving an undefined cohort name signals the typed error."
  (should-error (financial-chart-resolve-cohort 'does-not-exist)
                :type 'financial-chart-unresolvable-cohort))

(ert-deftest financial-chart-cohort-all-oscillator-resolves-to-panel ()
  "An all-oscillator cohort resolves to one oscillator-panel spec."
  (let ((specs (financial-chart-resolve-cohort 'momentum)))
    (should (= (length specs) 1))
    (should (eq (plist-get (car specs) :panel) 'oscillator))))

(ert-deftest financial-chart-cohort-describe-reports-oscillator-panel ()
  "Describe reports where a resolved oscillator member draws."
  (let* ((desc (financial-chart-describe-cohort 'mean-reversion))
         (members (plist-get desc :members))
         (rsi (cl-find-if
               (lambda (m) (equal (plist-get (plist-get m :member) :indicator)
                                  "finance.market.rsi-14"))
               members)))
    (should rsi)
    (should (eq (plist-get rsi :status) 'resolved))
    (should (eq (plist-get rsi :panel) 'oscillator))))

(ert-deftest financial-chart-cohort-overlay-safety-probed-from-value ()
  "Overlay-safety is derived from the record's `attributes.value' (bounds
/ unit), not a hardcoded symbol list; describe surfaces the live probe."
  (should (financial-chart--catalog-value-oscillator-p
           (alist-get 'value (alist-get 'attributes
                                        financial-chart-test--rsi-record))))
  (should-not (financial-chart--catalog-value-oscillator-p
               (alist-get 'value (alist-get 'attributes
                                            financial-chart-test--sma-record))))
  (let ((financial-chart-indicator-cohorts
         '((c :doc "d"
              :members ((:indicator "finance.market.rsi-14" :params (:period 14)))))))
    (let ((financial-chart-indicator-catalog-function
               (lambda (&rest _) financial-chart-test--rsi-record)))
      (let* ((desc (financial-chart-describe-cohort 'c))
             (m (car (plist-get desc :members))))
        (should (eq (plist-get m :catalog-live) t))
        (should (eq (plist-get m :probed-oscillator) t))))))

(ert-deftest financial-chart-cohort-describe-soft-fails-without-bridge ()
  "Describe of a catalog member downgrades to :unknown when no catalog
function is configured, never erroring."
  (let ((financial-chart-indicator-cohorts
         '((c :doc "d"
              :members ((:indicator "finance.market.sma" :params (:window 20)))))))
    (let ((financial-chart-indicator-catalog-function nil))
      (let* ((desc (financial-chart-describe-cohort 'c))
             (m (car (plist-get desc :members))))
        (should (eq (plist-get m :catalog-live) :unknown))))))

(ert-deftest financial-chart-cohort-list-summarizes ()
  "`financial-chart-list-cohorts' returns a pure per-cohort summary."
  (let ((rows (financial-chart-list-cohorts)))
    (let ((tf (assq 'trend-following rows)))
      (should tf)
      (should (= (plist-get (cdr tf) :members) 3))
      (should (= (plist-get (cdr tf) :resolvable) 3))
      (should (= (plist-get (cdr tf) :excluded) 0))
      (should (= (plist-get (cdr tf) :oscillators) 0)))
    (let ((mr (cdr (assq 'mean-reversion rows))))
      (should (= (plist-get mr :resolvable) 2))
      (should (= (plist-get mr :oscillators) 1))
      (should (= (plist-get mr :excluded) 0)))))

(ert-deftest financial-chart-cohort-doctor-checks-pass-and-fail ()
  "The cohort doctor rows pass for resolvable cohorts and fails (with a
remediation) for a broken one."
  (let ((checks (financial-chart-cohort-doctor-checks)))
    (should (cl-every (lambda (c) (eq (plist-get c :status) 'pass)) checks))
    )
  (let ((financial-chart-indicator-cohorts
         '((broken :doc "d" :members ((:indicator "nope.no.eval" :params nil))))))
    (let ((c (car (financial-chart-cohort-doctor-checks))))
      (should (eq (plist-get c :status) 'fail))
      (should (string-match-p "nope.no.eval" (plist-get c :detail)))
      (should (> (length (plist-get c :remediation)) 0)))))

(provide 'financial-chart-test)
;;; financial-chart-test.el ends here
