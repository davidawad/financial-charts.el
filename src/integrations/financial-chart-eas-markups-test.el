;;; financial-chart-eas-markups-test.el --- automatic levels, patterns, drawing tools and presets -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gty: the markup annotations of `financial-chart-eas-markups' and
;; the presets of `financial-chart-eas-presets' on composed charts.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'financial-chart-detectors-harness-test)

(defun financial-chart-markups-test--bars ()
  "The catalog examples' 100 bars."
  (plist-get (financial-chart-catalog-example "fibonacci") :bars))

(defun financial-chart-markups-test--layers (annotations &optional chart)
  "Price-pane layers named after an annotation for ANNOTATIONS on the example bars.
CHART adds top-level keys."
  (let ((spec (financial-chart-compose
               (append chart (list :bars (financial-chart-markups-test--bars)
                                   :price (list :annotations annotations))))))
    (cl-remove-if-not (lambda (l) (string-match-p "annotation" (or (plist-get l :name) "")))
                      (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil))))

(defun financial-chart-markups-test--named (layers name)
  "The layer of LAYERS called NAME."
  (cl-find name layers :key (lambda (l) (plist-get l :name)) :test #'equal))

(defun financial-chart-markups-test--rows (layer)
  "LAYER's own data rows."
  (append (plist-get (plist-get layer :data) :values) nil))

(defmacro financial-chart-markups-test--fails (code path &rest body)
  "Assert BODY signals `financial-chart-invalid-chart' with CODE and PATH."
  (declare (indent 2))
  `(let ((err (should-error (progn ,@body) :type 'financial-chart-invalid-chart)))
     (should (equal (list (plist-get (cddr err) :code) (plist-get (cddr err) :path)) (list ,code ,path)))))

(ert-deftest financial-chart-markups-every-type-is-registered-and-described ()
  (dolist (type (mapcar #'car financial-chart-markup-types))
    (should (member type financial-chart-annotation-types))
    (should (seq-contains-p (plist-get (plist-get (financial-chart-compose-describe) :annotations) :types)
                            type)))
  (should (= (length (plist-get (financial-chart-compose-describe) :candle-patterns))
             (length (financial-chart-candle-pattern-list)))))

(ert-deftest financial-chart-markups-support-resistance-draws-ranked-levels ()
  (let* ((layers (financial-chart-markups-test--layers [(:type "support-resistance" :max 3 :bands t)]))
         (levels (financial-chart-support-resistance
                  (car (financial-chart-compose--bars (list :bars (financial-chart-markups-test--bars))))
                  :max 3))
         (rows (financial-chart-markups-test--rows
                (financial-chart-markups-test--named layers "price-annotation-0-support"))))
    (should levels)
    (should (= (length rows) (length (seq-filter (lambda (l) (eq (plist-get l :kind) 'support)) levels))))
    (should (string-match-p "\\`S [0-9.]+ x[0-9]+\\'" (plist-get (car rows) :label)))
    (should (financial-chart-markups-test--named layers "price-annotation-0-support-bands"))
    (should (equal (plist-get (plist-get (financial-chart-markups-test--named layers "price-annotation-0-support") :mark)
                              :color)
                   financial-chart-palette-up))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/max"
      (financial-chart-markups-test--layers [(:type "support-resistance" :max 0)]))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/atr"
      (financial-chart-markups-test--layers [(:type "support-resistance" :atr -1)]))
    ;; Each drawn level is the newest event of a level.auto-sr cluster.
    (should (string-prefix-p "level.auto-sr@" (plist-get (car rows) :id)))))

(ert-deftest financial-chart-markups-patterns-mark-bars-by-direction ()
  (let* ((layers (financial-chart-markups-test--layers [(:type "patterns")]))
         (bull (financial-chart-markups-test--named layers "price-annotation-0-bullish"))
         (bear (financial-chart-markups-test--named layers "price-annotation-0-bearish"))
         (bars (vconcat (financial-chart-markups-test--bars))))
    (should (equal (plist-get (plist-get bull :mark) :shape) "triangle-up"))
    (should (equal (plist-get (plist-get bear :mark) :shape) "triangle-down"))
    ;; Bullish marks sit under their bar's low.
    (dolist (row (financial-chart-markups-test--rows bull))
      (should (< (plist-get row :y) (plist-get (aref bars (plist-get row :time)) :low)))
      (should (member (plist-get row :name) (financial-chart-candle-pattern-list)))
      (should (string-prefix-p "pattern." (plist-get row :id))))
    (let ((only (financial-chart-markups-test--layers [(:type "patterns" :only ["doji"] :labels "name")])))
      (should (seq-every-p (lambda (r) (equal (plist-get r :label) "doji"))
                           (financial-chart-markups-test--rows
                            (financial-chart-markups-test--named only "price-annotation-0-neutral"))))
      (should-not (financial-chart-markups-test--named only "price-annotation-0-bullish")))
    (should-not (financial-chart-markups-test--named
                 (financial-chart-markups-test--layers [(:type "patterns" :labels "none")])
                 "price-annotation-0-bullish-labels"))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/only/1"
      (financial-chart-markups-test--layers [(:type "patterns" :only ["hammer" "rising-hammer"])]))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/labels"
      (financial-chart-markups-test--layers [(:type "patterns" :labels "long")]))))

(ert-deftest financial-chart-markups-zigzag-and-auto-trendlines ()
  (let* ((layers (financial-chart-markups-test--layers [(:type "zigzag" :deviation 4 :labels t)
                                                        (:type "auto-trendlines" :left 2)]))
         (zig (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-0")))
         (support (car (financial-chart-markups-test--rows
                        (financial-chart-markups-test--named layers "price-annotation-1-support")))))
    (should (>= (length zig) 2))
    (should (financial-chart-markups-test--named layers "price-annotation-0-running"))
    ;; Legs join end to start.
    (cl-loop for (a b) on zig while b do (should (equal (plist-get a :time2) (plist-get b :time))))
    (should (seq-every-p (lambda (r) (member (plist-get r :label) '("HH" "HL" "LH" "LL")))
                         (financial-chart-markups-test--rows
                          (financial-chart-markups-test--named layers "price-annotation-0-labels"))))
    ;; Extended right to the last bar.
    (should (= (plist-get support :time2) 99))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/deviation"
      (financial-chart-markups-test--layers [(:type "zigzag" :deviation -1)]))))

(ert-deftest financial-chart-markups-fib-extension-and-time-zones ()
  (let* ((layers (financial-chart-markups-test--layers
                  [(:type "fib-extension" :from ["2026-03-18" 128.72] :to ["2026-05-08" 104.89]
                    :then ["2026-05-22" 111.95] :levels [1 1.618] :extend t)
                   (:type "fib-timezones" :at "2026-02-02")]))
         (ext (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-0")))
         (zones (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-1"))))
    (should (equal (mapcar (lambda (r) (/ (round (* 100 (plist-get r :y))) 100.0)) ext)
                   (list (/ (round (* 100 (+ 111.95 (- 104.89 128.72)))) 100.0)
                         (/ (round (* 100 (+ 111.95 (* 1.618 (- 104.89 128.72))))) 100.0))))
    (should (equal (mapcar (lambda (r) (plist-get r :time)) zones) '(0 1 2 3 5 8 13 21 34 55 89)))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/then"
      (financial-chart-markups-test--layers [(:type "fib-extension" :from ["2026-03-18" 1] :to ["2026-05-08" 2])]))
    ;; Without points it uses the last confirmed zigzag pivots.
    (should (financial-chart-markups-test--layers [(:type "fib-extension" :deviation 4)]))))

(ert-deftest financial-chart-markups-channel-and-pitchfork ()
  (let* ((layers (financial-chart-markups-test--layers
                  [(:type "channel" :from ["2026-05-08" 104] :to ["2026-06-01" 117] :offset 5 :extend "none")
                   (:type "channel" :window 30)
                   (:type "pitchfork" :points [["2026-04-17" 114] ["2026-05-07" 105] ["2026-05-22" 112]])]))
         (fixed (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-0")))
         (fork (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-2"))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :y) (plist-get r :y2))) fixed)
                   '((104.0 117.0) (109.0 122.0))))
    (should (= (length (financial-chart-markups-test--rows
                        (financial-chart-markups-test--named layers "price-annotation-1"))) 2))
    ;; Median line from the first point through the middle of the others.
    (should (= (length fork) 3))
    (should (= (plist-get (car fork) :y) 114))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/through"
      (financial-chart-markups-test--layers [(:type "channel" :from ["2026-05-08" 104] :to ["2026-06-01" 117])]))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/points"
      (financial-chart-markups-test--layers [(:type "pitchfork" :points [["2026-04-17" 114]])]))))

(ert-deftest financial-chart-markups-anchored-vwap-measure-measured-move ()
  (let* ((layers (financial-chart-markups-test--layers
                  [(:type "anchored-vwap" :at "2026-05-01")
                   (:type "measure" :from ["2026-03-18" 128.72] :to ["2026-05-08" 104.89])
                   (:type "measured-move" :from ["2026-05-08" 104.89] :to ["2026-05-22" 111.95]
                    :then ["2026-05-21" 109.6])]))
         (vwap (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-0")))
         (label (car (financial-chart-markups-test--rows
                      (financial-chart-markups-test--named layers "price-annotation-1-label"))))
         (target (car (financial-chart-markups-test--rows
                       (financial-chart-markups-test--named layers "price-annotation-2-target")))))
    (should (= (plist-get (car vwap) :time) 64))
    (should (= (length vwap) 36))
    (should (string-match-p "\\`-23\\.83 (-18\\.5%) 37 bars\\'" (plist-get label :label)))
    (should (< (abs (- (plist-get target :y) (+ 109.6 (- 111.95 104.89)))) 1e-9))
    (financial-chart-markups-test--fails "NO_VOLUME" "/price/annotations/0"
      (financial-chart-compose
       (list :bars (vconcat (mapcar (lambda (b) (cl-loop for (k v) on b by #'cddr unless (eq k :volume) append (list k v)))
                                    (financial-chart-markups-test--bars)))
             :price '(:annotations [(:type "anchored-vwap")]))))))

(ert-deftest financial-chart-presets-merge-files-inline-and-the-chart ()
  (let* ((chart (financial-chart-presets-apply
                 (list :bars [] :preset ["swing-trader" "mine"]
                       :presets '(:mine (:price (:annotations [(:type "zigzag")] :style "line")
                                         :panes [(:study "macd")]))
                       :price '(:style "hollow" :series [(:indicator "ema" :params [9])]))))
         (price (plist-get chart :price)))
    ;; The chart's own scalar keys win; lists append in order.
    (should (equal (plist-get price :style) "hollow"))
    (should (equal (mapcar (lambda (s) (aref (plist-get s :params) 0)) (plist-get price :series))
                   '(7 20 50 9)))
    (should (equal (mapcar (lambda (a) (plist-get a :type)) (plist-get price :annotations))
                   '("fibonacci" "support-resistance" "patterns" "zigzag")))
    (should (equal (mapcar (lambda (p) (plist-get p :study)) (plist-get chart :panes))
                   '("volume" "rsi" "macd")))
    (should-not (plist-get chart :doc))
    ;; Every shipped preset compiles.
    (dolist (name (financial-chart-presets-list))
      (should (financial-chart-compose (list :bars (financial-chart-markups-test--bars) :preset name))))
    (should (member "swing-trader" (financial-chart-presets-list)))))

(ert-deftest financial-chart-presets-fail-with-their-path ()
  (financial-chart-markups-test--fails "UNKNOWN_PRESET" "/preset/1"
    (financial-chart-compose (list :bars (financial-chart-markups-test--bars) :preset ["trend" "nope"])))
  (financial-chart-markups-test--fails "UNKNOWN_PRESET" "/preset"
    (financial-chart-compose (list :bars (financial-chart-markups-test--bars) :preset "../trend")))
  (financial-chart-markups-test--fails "INVALID_PRESET" "/preset"
    (financial-chart-compose (list :bars (financial-chart-markups-test--bars) :preset 3)))
  (financial-chart-markups-test--fails "PRESET_CYCLE" "/preset/preset"
    (financial-chart-compose (list :bars (financial-chart-markups-test--bars) :preset "a"
                                   :presets '(:a (:preset "a"))))))

(ert-deftest financial-chart-presets-merge-laws-and-provenance ()
  (let* ((chart (list :bars (financial-chart-markups-test--bars) :preset ["swing-trader" "fibonacci"]
                      :colors '(:up "#00ff00")
                      :price '(:series [(:indicator "sma" :params [20] :color "#000000")]
                               :annotations [(:type "fibonacci")])))
         (x (financial-chart-expand chart))
         (price (plist-get x :price)))
    ;; A repeated id replaces the earlier entry in its place.
    (should (equal (mapcar (lambda (s) (list (aref (plist-get s :params) 0) (plist-get s :origin)))
                           (plist-get price :series))
                   '((7 "preset:swing-trader") (20 "chart") (50 "preset:swing-trader"))))
    (should (equal (plist-get (nth 1 (append (plist-get price :series) nil)) :color) "#000000"))
    ;; Two presets' fibonacci and the chart's are one entry, the chart's.
    (should (equal (seq-count (lambda (a) (equal (plist-get a :type) "fibonacci")) (plist-get price :annotations)) 1))
    (should (equal (plist-get (seq-find (lambda (a) (equal (plist-get a :type) "fibonacci")) (plist-get price :annotations))
                              :origin)
                   "chart"))
    (should (seq-every-p (lambda (p) (equal (plist-get p :origin) "preset:swing-trader")) (plist-get x :panes)))
    (should (equal (plist-get (plist-get x :colors) :up) "#00ff00"))
    ;; The expansion composes like the chart.
    (should (equal (financial-chart-compose-render x) (financial-chart-compose-render chart)))
    (should (eq (plist-get (financial-chart-expand-verb chart) :ok) t))
    (should (equal (plist-get (financial-chart-expand-verb (list :bars [] :preset "nope")) :reason) "UNKNOWN_PRESET"))))

(ert-deftest financial-chart-presets-owner-directories-and-lint ()
  (should (equal (financial-chart-presets-lint) nil))
  (let* ((dir (make-temp-file "fc-presets" t))
         (financial-chart-preset-directories (list dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "trend.json" dir)
            (insert "{\"preset/v1\": \"trend\", \"doc\": \"mine\", \"price\": {\"studies\": [\"psar\"]}}"))
          (with-temp-file (expand-file-name "bad.json" dir)
            (insert "{\"price\": {}}"))
          ;; The owner's file shadows the package's.
          (should (equal (plist-get (financial-chart-preset-get "trend") :doc) "mine"))
          (should (equal (plist-get (financial-chart-presets-apply (list :preset "trend")) :price)
                         '(:studies ["psar"])))
          (should (assoc "bad" (financial-chart-presets-lint))))
      (delete-directory dir t))))

(ert-deftest financial-chart-markups-chart-events-are-data ()
  (let ((events (financial-chart-chart-events (expand-file-name "examples/indicators/annotations.json"
                                                                financial-chart-root))))
    (should (equal (mapcar (lambda (e) (plist-get e :detector)) events)
                   '("constant.level" "constant.level" "constant.trendline" "constant.event"
                     "constant.box" "constant.text" "constant.event")))
    (should (equal (plist-get (car events) :geometry) '(:y 127.3 :from "2026-03-09" :to "2026-04-17"))))
  (let* ((events (financial-chart-chart-events
                  (list :bars (financial-chart-markups-test--bars) :preset "swing-trader")))
         (detectors (delete-dups (mapcar (lambda (e) (plist-get e :detector)) events))))
    (should (member "level.auto-sr" detectors))
    (should (member "fib.retracement" detectors))
    (should (seq-some (lambda (d) (string-prefix-p "pattern." d)) detectors))
    ;; The drawn S/R ids are among them.
    (let* ((layers (financial-chart-markups-test--layers [(:type "support-resistance")]))
           (ids (mapcar (lambda (r) (plist-get r :id))
                        (append (financial-chart-markups-test--rows
                                 (financial-chart-markups-test--named layers "price-annotation-0-support"))
                                (financial-chart-markups-test--rows
                                 (financial-chart-markups-test--named layers "price-annotation-0-resistance"))))))
      (should ids)
      (dolist (id ids) (should (member id (mapcar (lambda (e) (plist-get e :id)) events)))))))

(ert-deftest financial-chart-cohorts-are-presets ()
  (let ((cohorts (financial-chart--cohorts)))
    (dolist (name '(trend-following mean-reversion momentum))
      (should (string-prefix-p "preset " (plist-get (cdr (assq name cohorts)) :provenance))))
    (should (equal (plist-get (cdr (assq 'trend-following cohorts)) :members)
                   '((:fn financial-chart-sma :args (20) :face font-lock-keyword-face)
                     (:fn financial-chart-sma :args (50) :face font-lock-type-face)
                     (:fn financial-chart-vwap :args nil :face font-lock-constant-face))))
    (should (equal (plist-get (cdr (assq 'momentum cohorts)) :members)
                   '((:indicator "finance.market.rsi-14" :params (:period 14)))))
    ;; A cohort preset is an ordinary preset too.
    (should (financial-chart-compose (list :bars (financial-chart-markups-test--bars) :preset "mean-reversion")))
    (should (eq (plist-get (car (last (financial-chart-resolve-cohort 'mean-reversion))) :panel) 'oscillator))
    ;; The obsolete defcustom still adds and shadows.
    (let ((financial-chart-indicator-cohorts '((momentum :doc "mine" :members ((:fn financial-chart-sma :args (3)))))))
      (should (equal (plist-get (financial-chart--cohort 'momentum) :doc) "mine")))))

(ert-deftest financial-chart-summarize-reads-the-chart ()
  (let* ((chart (financial-chart-catalog-example "preset-swing-trader"))
         (d (financial-chart-summarize chart))
         (bars (financial-chart-markups-test--bars)))
    (should (equal (plist-get d :bars-id) (financial-chart-bars-id (car (financial-chart-compose--bars chart)))))
    (should (= (length (plist-get d :bars-id)) 16))
    (should (equal (plist-get (plist-get d :last) :time) "2026-06-19"))
    ;; Levels nearest first, with the drawn ids.
    (let ((levels (append (plist-get d :levels) nil)))
      (should levels)
      (should (cl-every (lambda (l) (string-prefix-p "level.auto-sr@" (plist-get l :id))) levels))
      (should (apply #'<= (mapcar (lambda (l) (abs (plist-get l :distance_pct))) levels))))
    (should (equal (plist-get (plist-get (plist-get d :series_state) :rsi) :state) "overbought"))
    (should (member (plist-get (plist-get (plist-get d :series_state) :sma-7) :state) '("up" "down" "flat")))
    (should (cl-every (lambda (e) (<= (plist-get e :bars_ago) 20)) (plist-get d :recent_events)))
    (should (string-match-p "\\`Close 134.76" (plist-get d :text)))
    ;; A few hundred tokens.
    (should (< (length (eas-json-encode d)) 2400))
    ;; Same bars, same id; other bars, another.
    (should-not (equal (financial-chart-bars-id (car (financial-chart-compose--bars (list :bars bars))))
                       (financial-chart-bars-id (cdr (car (financial-chart-compose--bars (list :bars bars)))))))
    (should (eq (plist-get (financial-chart-summarize-verb chart) :ok) t))
    (should (equal (plist-get (financial-chart-summarize-verb (list :bars [])) :reason) "NO_BARS"))))

(ert-deftest financial-chart-markups-tool-detectors ()
  (let* ((bars (car (financial-chart-compose--bars (list :bars (financial-chart-markups-test--bars)))))
         (crosses (financial-chart-detect "vwap.anchored" bars))
         (vwap (financial-chart-anchored-vwap bars 64)))
    (should crosses)
    ;; Each cross flips the close's side of the VWAP.
    (should (cl-every (lambda (e) (equal (plist-get e :direction)
                                         (if (> (plist-get (plist-get e :evidence) :distance) 0) "bullish" "bearish")))
                      crosses))
    ;; With the first bar as anchor the VWAP is the plain anchored VWAP.
    (let ((e (car (financial-chart-detect "vwap.anchored" bars '(:anchor "first")))))
      (should (financial-chart-harness-test--close
               (plist-get (plist-get e :geometry) :y) (nth (plist-get e :index) (financial-chart-anchored-vwap bars 0)))))
    (should (numberp (car (last vwap))))
    (let ((c (car (last (financial-chart-detect "channel.regression" bars '(:window 30))))))
      (should (= (plist-get c :index) 99))
      (should (= (length (plist-get (plist-get c :geometry) :points)) 4))
      (should (<= 0 (plist-get (plist-get c :evidence) :position) 1))))
  (let ((m (seq-find (lambda (e) (equal (plist-get e :detector) "constant.measure"))
                     (financial-chart-chart-events
                      (list :bars (financial-chart-markups-test--bars)
                            :price '(:annotations [(:type "measure" :from ["2026-03-18" 128.72] :to ["2026-05-08" 104.89])]))))))
    (should (= (plist-get (plist-get m :evidence) :bars) 37))
    (should (< (abs (- (plist-get (plist-get m :evidence) :change_pct) -18.513)) 0.01))))

(ert-deftest financial-chart-detect-memo-and-check ()
  (let* ((bars (car (financial-chart-compose--bars (list :bars (financial-chart-markups-test--bars)))))
         (financial-chart-detect-memo t))
    (clrhash financial-chart-detect--memo)
    (let ((a (financial-chart-detect "swing.pivot" bars)))
      (should-not financial-chart-detect--last-cached)
      (should (eq (financial-chart-detect "swing.pivot" (copy-sequence bars)) a))
      (should financial-chart-detect--last-cached)
      (should (eq (plist-get (plist-get (financial-chart-detect-verb "swing.pivot" bars) :cost) :cached) t))
      ;; Other params, other bars: computed.
      (financial-chart-detect "swing.pivot" bars '(:left 2))
      (should-not financial-chart-detect--last-cached)
      (financial-chart-detect "swing.pivot" (seq-take bars 50))
      (should-not financial-chart-detect--last-cached)
      ;; Re-registering a detector forgets its memo.
      (let ((entry (financial-chart-detector 'swing.pivot)))
        (apply #'financial-chart-register-detector 'swing.pivot (plist-get entry :fn)
               (cl-loop for (k v) on entry by #'cddr unless (memq k '(:fn :generation)) append (list k v))))
      (financial-chart-detect "swing.pivot" bars)
      (should-not financial-chart-detect--last-cached)))
  ;; check: bars, presets, annotations, before computing.
  (should (eq (plist-get (financial-chart-check-chart (financial-chart-catalog-example "preset-swing-trader")) :ok) t))
  (let* ((bars (vconcat (financial-chart-markups-test--bars)))
         (bad (vconcat (list (plist-put (copy-sequence (aref bars 0)) :high 1.0)) (seq-drop bars 1)))
         (answer (financial-chart-check-chart (list :bars bad :preset "trend"
                                              :price '(:annotations [(:type "zigzag" :deviation -1) (:type "nope")])))))
    (should (eq (plist-get answer :ok) :false))
    (should (equal (mapcar (lambda (p) (plist-get p :path)) (plist-get (plist-get answer :data) :problems))
                   '("/bars/0/high" "/bars/0/open" "/bars/0/close"
                     "/price/annotations/2/deviation" "/price/annotations/3/type"))))
  (should (equal (plist-get (financial-chart-check-chart (list :bars [] :preset "nope")) :reason) "UNKNOWN_PRESET"))
  (should (equal (plist-get (plist-get (financial-chart-check-bars-verb
                                         (list '(:open 1 :high 2 :low 0 :close 1 :time "2026-01-02")
                                               '(:open 1 :high 2 :low 0 :close 1 :time "2026-01-01")))
                                        :data)
                             :problems)
                 [(:index 1 :field "time" :problem "earlier than the bar before")])))

(ert-deftest financial-chart-markups-fib-extension-clips-to-the-bars ()
  (let* ((bars (financial-chart-markups-test--bars))
         (range (let ((v (append bars nil)))
                  (cons (apply #'min (mapcar (lambda (b) (plist-get b :low)) v))
                        (apply #'max (mapcar (lambda (b) (plist-get b :high)) v)))))
         (a '(:type "fib-extension" :from ["2026-03-18" 128.72] :to ["2026-05-08" 104.89] :then ["2026-05-22" 111.95]))
         (ys (lambda (layers) (mapcar (lambda (r) (plist-get r :y))
                                      (financial-chart-markups-test--rows (financial-chart-markups-test--named layers "price-annotation-0")))))
         (clipped (funcall ys (financial-chart-markups-test--layers (vector a))))
         (all (funcall ys (financial-chart-markups-test--layers (vector (append a '(:extend t)))))))
    ;; 1.618 and beyond fall below the bars: left out unless extend.
    (should (cl-every (lambda (y) (<= (car range) y (cdr range))) clipped))
    (should (< (length clipped) (length all)))
    (should (= (length all) 7))
    (financial-chart-markups-test--fails "INVALID_ANNOTATION" "/price/annotations/0/extend"
      (financial-chart-markups-test--layers (vector (append a '(:extend "yes")))))))

(ert-deftest financial-chart-markups-channel-is-cut-to-the-bars ()
  (should (equal (financial-chart-markup--clip-row '(:time 0 :time2 10 :y 0.0 :y2 20.0) 5.0 15.0)
                 '(:time 2.5 :time2 7.5 :y 5.0 :y2 15.0)))
  (should-not (financial-chart-markup--clip-row '(:time 0 :time2 10 :y 30.0 :y2 40.0) 5.0 15.0))
  (let* ((bars (financial-chart-markups-test--bars))
         (v (append bars nil))
         (lo (apply #'min (mapcar (lambda (b) (plist-get b :low)) v)))
         (hi (apply #'max (mapcar (lambda (b) (plist-get b :high)) v)))
         ;; A steep hand-drawn channel running far off the bars.
         (a '(:type "channel" :from ["2026-02-02" 60] :to ["2026-06-19" 200] :offset 30))
         (ys (lambda (layers) (cl-loop for r in (financial-chart-markups-test--rows
                                                 (financial-chart-markups-test--named layers "price-annotation-0"))
                                       append (list (plist-get r :y) (plist-get r :y2))))))
    (should (cl-every (lambda (y) (<= lo y hi)) (funcall ys (financial-chart-markups-test--layers (vector a)))))
    (should (cl-some (lambda (y) (> y hi))
                     (funcall ys (financial-chart-markups-test--layers (vector (append a '(:clip :false)))))))))

(provide 'financial-chart-eas-markups-test)
;;; financial-chart-eas-markups-test.el ends here
