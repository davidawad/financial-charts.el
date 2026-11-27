;;; financial-chart-presets-test.el --- every shipped preset expands and renders (fc-nyt) -*- lexical-binding: t; -*-

;;; Commentary:

;; The named technical-analysis presets of presets/ on 230 synthetic
;; daily bars (no market data is committed), and the detectors and
;; annotations they need.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'financial-chart-test-support)
(require 'financial-chart)

(defconst financial-chart-presets-test--named
  '("ichimoku" "ichimoku-rsi-vwap" "squeeze" "turtle" "elder-triple-screen" "minervini-trend-template"
    "weinstein-stage" "connors-rsi2" "golden-cross" "supertrend-adx" "volume-auction")
  "The named setups, each with a cited doc.")

(defun financial-chart-presets-test--bars (&optional n)
  "N (230) deterministic daily bars: a trending wave with volume."
  (cl-loop for i below (or n 230)
           for c = (+ 100 (* 0.12 i) (* 12 (sin (* i 0.09))) (* 2.5 (sin (* i 1.3))))
           for o = (+ c (* 1.2 (sin (* i 2.1))))
           collect (list :time (format-time-string "%F" (+ 1735689600 (* i 86400)) t)
                         :open o :close c
                         :high (+ (max o c) 1.0 (abs (sin i))) :low (- (min o c) 1.0 (abs (cos i)))
                         :volume (+ 1000000 (* 50000 (% (* i 7) 13)) (if (zerop (% i 37)) 900000 0)))))

(defun financial-chart-presets-test--close (a b) (< (abs (- a b)) 1e-6))

(defun financial-chart-presets-test--chart (name)
  "A chart of the synthetic bars with preset NAME."
  (list :title name :bars (vconcat (financial-chart-presets-test--bars)) :preset name))

(ert-deftest financial-chart-presets-every-shipped-preset-expands-and-renders ()
  (dolist (name (financial-chart-presets-list))
    (let* ((chart (financial-chart-presets-test--chart name))
           (x (financial-chart-expand chart))
           (text (financial-chart-compose-render chart :backend 'text :width 120 :height 40))
           (svg (if (member name financial-chart-presets-test--named)
                    (financial-chart-compose-render chart :backend 'svg :width 900 :height 600)
                  "<svg></svg>")))
      ;; Provenance: every entry the preset contributed says so.
      (dolist (entry (append (plist-get (plist-get x :price) :series) (plist-get (plist-get x :price) :annotations)
                             (plist-get x :panes) nil))
        (should (equal (list name (plist-get entry :origin)) (list name (concat "preset:" name)))))
      ;; Text: drawn, deterministic, and free of unprinted values.
      (should (> (length text) 500))
      (when (member name financial-chart-presets-test--named)
        (should (equal text (financial-chart-compose-render chart :backend 'text :width 120 :height 40))))
      (should-not (string-match-p "nil\\|:null\\|NaN\\|Inf\\|Error" text))
      ;; SVG: a document with a drawn price pane.
      (should (string-prefix-p "<svg" (string-trim-left (replace-regexp-in-string "\\`<\\?xml[^>]*>\\s-*" "" svg))))
      (should (string-match-p "</svg>" svg)))))

(ert-deftest financial-chart-presets-lint-is-clean-and-named-ones-cite-a-source ()
  (should (equal (financial-chart-presets-lint) nil))
  (dolist (name financial-chart-presets-test--named)
    (should (member name (financial-chart-presets-list)))
    (let ((doc (plist-get (financial-chart-preset-get name) :doc)))
      ;; A book or paper and its year, and the canonical parameters as numbers.
      (should (string-match-p "\\b\\(19\\|20\\)[0-9][0-9]\\b" doc))
      (should (string-match-p "[0-9]" doc))
      (should (> (length doc) 300)))))

(ert-deftest financial-chart-presets-named-setups-draw-what-their-doc-says ()
  (let ((series-of (lambda (name)
                     (mapcar (lambda (s) (list (plist-get s :indicator) (append (plist-get s :params) nil)
                                               (plist-get s :output)))
                             (append (plist-get (plist-get (financial-chart-expand (financial-chart-presets-test--chart name)) :price)
                                                :series) nil))))
        (panes-of (lambda (name)
                    (mapcar (lambda (p) (plist-get p :study))
                            (append (plist-get (financial-chart-expand (financial-chart-presets-test--chart name)) :panes) nil))))
        (price-of (lambda (name key)
                    (append (plist-get (plist-get (financial-chart-expand (financial-chart-presets-test--chart name)) :price)
                                       key)
                            nil))))
    (ignore series-of)
    (should (equal (mapcar (lambda (x) (plist-get x :study)) (funcall price-of "ichimoku" :studies)) '("ichimoku")))
    (should (equal (funcall panes-of "ichimoku-rsi-vwap") '("wilder-rsi")))
    (should (equal (mapcar (lambda (a) (plist-get a :type)) (funcall price-of "ichimoku-rsi-vwap" :annotations))
                   '("anchored-vwap" "anchored-vwap")))
    (should (equal (funcall panes-of "squeeze") '("squeeze")))
    (should (equal (mapcar #'caddr (funcall series-of "squeeze"))
                   '("squeeze-bb-upper" "squeeze-bb-lower" "squeeze-kc-upper" "squeeze-kc-lower")))
    (should (equal (cl-remove-duplicates (mapcar #'cadr (funcall series-of "turtle")) :test #'equal) '((55) (20))))
    (should (equal (funcall panes-of "turtle") '("atr")))
    (should (equal (funcall panes-of "elder-triple-screen") '("macd" "force-index" "elder-impulse")))
    (should (equal (mapcar #'cadr (funcall series-of "elder-triple-screen")) '((13) (26))))
    (should (equal (mapcar #'cadr (funcall series-of "minervini-trend-template")) '((50) (150) (200))))
    (should (equal (mapcar (lambda (x) (plist-get x :study)) (funcall price-of "minervini-trend-template" :studies)) '("range-52w")))
    (should (equal (mapcar #'cadr (funcall series-of "weinstein-stage")) '((150))))
    (should (equal (mapcar #'cadr (funcall series-of "connors-rsi2")) '((200) (5))))
    (should (equal (funcall panes-of "connors-rsi2") '("wilder-rsi")))
    (should (equal (mapcar #'cadr (funcall series-of "golden-cross")) '((50) (200))))
    (should (equal (mapcar (lambda (a) (list (plist-get a :type) (plist-get a :fast) (plist-get a :slow)))
                           (funcall price-of "golden-cross" :annotations))
                   '(("ma-cross" 50 200))))
    (should (equal (mapcar (lambda (x) (plist-get x :study)) (funcall price-of "supertrend-adx" :studies)) '("supertrend")))
    (should (equal (funcall panes-of "supertrend-adx") '("adx")))
    (should (member "volume-profile" (mapcar (lambda (a) (plist-get a :type)) (funcall price-of "volume-auction" :annotations))))))

;;; Studies

(ert-deftest financial-chart-presets-new-studies-compute-over-the-bars ()
  (let ((spec (financial-chart-compose (financial-chart-presets-test--chart "elder-triple-screen"))))
    (should (= (length (plist-get spec :vconcat)) 4)))
  ;; The 52-week lines, on fewer than 252 bars, are the bars so far.
  (let* ((bars (financial-chart-presets-test--bars 100))
         (rows (financial-chart-indicator-evaluate 'range-52w bars 252)))
    (should (= (car (plist-get (car rows) :values)) (plist-get (car bars) :high)))
    (should (= (car (last (plist-get (car rows) :values))) (cl-loop for b in bars maximize (plist-get b :high)))))
  ;; A bad study parameter says which.
  (let ((err (should-error (financial-chart-compose
                            (list :bars (vconcat (financial-chart-presets-test--bars))
                                  :panes (vector (list :study "squeeze" :params [0])))))))
    (should (equal (plist-get (cddr err) :code) "INDICATOR_FAILED"))))

;;; cross.ma

(defun financial-chart-presets-test--closes (closes)
  "Bars with CLOSES, each a point either side."
  (vconcat (mapcar (lambda (c) (list :open c :high (+ c 1) :low (- c 1) :close c)) closes)))

(ert-deftest financial-chart-presets-cross-ma-finds-golden-and-death-crosses ()
  (let ((events (financial-chart-detect "cross.ma" (financial-chart-presets-test--closes '(10 10 10 10 10 12 14 16 14 10 6))
                                        '(:fast 2 :slow 4))))
    (should (equal (mapcar (lambda (e) (list (plist-get e :index) (plist-get e :direction))) events)
                   '((5 "bullish") (9 "bearish"))))
    (should (equal (plist-get (car events) :id) "cross.ma@5"))
    (should (> (plist-get (plist-get (car events) :evidence) :gap) 0))
    (should (< (plist-get (plist-get (cadr events) :evidence) :gap) 0))))

(ert-deftest financial-chart-presets-cross-ma-needs-fast-below-slow ()
  (should (equal (plist-get (cddr (should-error (financial-chart-detect "cross.ma" (financial-chart-presets-test--closes '(1 2 3 4 5 6)) '(:fast 4 :slow 3))
                                               :type 'financial-chart-invalid-detection))
                            :code)
                 "INVALID_PARAM"))
  (should (equal (financial-chart-detect "cross.ma" (financial-chart-presets-test--closes '(1 2 3)) nil) nil)))

(ert-deftest financial-chart-presets-ma-cross-annotation-marks-the-crossing-bars ()
  (let* ((closes '(10 10 10 10 10 12 14 16 14 10 6))
         (spec (financial-chart-compose
                (list :bars (vconcat (cl-loop for c in closes for i from 1
                                              collect (list :time (format "2026-03-%02d" i) :open c :high (+ c 1) :low (- c 1) :close c))) 
                      :price (list :annotations (vector (list :type "ma-cross" :fast 2 :slow 4))))))
         (layers (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil))
         (up (cl-find-if (lambda (l) (string-suffix-p "-bullish" (or (plist-get l :name) ""))) layers))
         (down (cl-find-if (lambda (l) (string-suffix-p "-bearish" (or (plist-get l :name) ""))) layers)))
    (should (= (length (plist-get (plist-get up :data) :values)) 1))
    (should (= (length (plist-get (plist-get down :data) :values)) 1))
    (should (equal (plist-get (aref (plist-get (plist-get up :data) :values) 0) :label) "cross up"))
    ;; Under the canonical 50/200 the labels are the golden and the death cross.
    (let* ((closes (append (cl-loop for i below 200 collect (- 100 (* 0.1 i)))
                           (cl-loop for i below 90 collect (+ 80 (* 1.0 i)))))
           (spec (financial-chart-compose
                  (list :bars (vconcat (cl-loop for c in closes for i from 0
                                                collect (list :time (format-time-string "%F" (+ 1735689600 (* i 86400)) t)
                                                              :open c :high (+ c 1) :low (- c 1) :close c))) 
                        :price (list :annotations (vector (list :type "ma-cross"))))))
           (layer (cl-find-if (lambda (l) (string-suffix-p "-bullish" (or (plist-get l :name) "")))
                              (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil))))
      (should (equal (plist-get (aref (plist-get (plist-get layer :data) :values) 0) :label) "golden cross")))))

(defun financial-chart-presets-test--annotation-layer (annotation n suffix)
  "The price-layer of ANNOTATION on N synthetic bars whose name ends in SUFFIX."
  (let* ((spec (financial-chart-compose (list :bars (vconcat (financial-chart-presets-test--bars n))
                                              :price (list :annotations (vector annotation)))))
         (layers (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil)))
    (cl-find-if (lambda (l) (string-suffix-p suffix (or (plist-get l :name) ""))) layers)))

(ert-deftest financial-chart-presets-ma-cross-rejects-bad-arguments ()
  (let ((err (should-error (financial-chart-compose
                            (list :bars (vconcat (financial-chart-presets-test--bars 60))
                                  :price (list :annotations (vector (list :type "ma-cross" :fast 30 :slow 10)))))
                           :type 'financial-chart-invalid-chart)))
    (should (equal (plist-get (cddr err) :code) "INVALID_ANNOTATION"))
    (should (string-match-p "/price/annotations/0" (plist-get (cddr err) :path)))))

;;; profile.volume

(ert-deftest financial-chart-presets-volume-profile-finds-poc-and-value-area ()
  ;; One bar per price bin 0..10; volume in bins 0, 4, 5 and 9.
  (let* ((bars (vconcat (cl-mapcar (lambda (k v) (list :open k :high (1+ k) :low k :close (+ k 0.5) :volume v))
                                   '(0 4 5 9) '(10 100 50 20))))
         (p (financial-chart-setups-volume-profile bars 0 3 10 0.7)))
    (should (= (plist-get p :lo) 0))
    (should (= (plist-get p :hi) 10))
    (should (financial-chart-presets-test--close (plist-get p :poc) 4.5))
    ;; 100 of 180 is short of 70% (126): add the two bins above (50) -> 150.
    (should (financial-chart-presets-test--close (plist-get p :val) 4.0))
    (should (financial-chart-presets-test--close (plist-get p :vah) 7.0))
    (should (financial-chart-presets-test--close (plist-get p :poc-share) (/ 100.0 180)))))

(ert-deftest financial-chart-presets-volume-profile-spreads-a-bar-over-its-range ()
  (let* ((bars (vector (list :high 4.0 :low 0.0 :close 2.0 :open 2.0 :volume 400.0)))
         (vols (financial-chart-setups--profile-bins bars 0 0 4 0.0 4.0)))
    (should (equal (append vols nil) '(100.0 100.0 100.0 100.0)))))

(ert-deftest financial-chart-presets-volume-profile-events-and-annotation ()
  (let* ((bars (vconcat (financial-chart-presets-test--bars 120)))
         (events (financial-chart-detect "profile.volume" bars '(:window 60 :bins 12)))
         (e (car (last events))))
    (should (= (length events) 119))
    (should (= (length (plist-get (plist-get e :geometry) :levels)) 3))
    (let ((r (mapcar (lambda (l) (plist-get l :ratio)) (plist-get (plist-get e :geometry) :levels))))
      (should (apply #'<= r))
      (should (cl-every (lambda (x) (<= 0 x 1)) r))))
  (let ((layer (financial-chart-presets-test--annotation-layer (list :type "volume-profile" :bins 12) 120 "-bins")))
    (should (> (length (plist-get (plist-get layer :data) :values)) 3)))
  ;; Without volume there is no profile.
  (let ((err (should-error (financial-chart-compose
                            (list :bars (vconcat (mapcar (lambda (b) (cl-loop for (k v) on b by #'cddr unless (eq k :volume) append (list k v)))
                                                         (financial-chart-presets-test--bars 30)))
                                  :price (list :annotations (vector (list :type "volume-profile"))))))))
    (should (equal (plist-get (cddr err) :code) "NO_VOLUME"))))

(provide 'financial-chart-presets-test)
;;; financial-chart-presets-test.el ends here
