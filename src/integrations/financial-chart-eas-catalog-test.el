;;; financial-chart-eas-catalog-test.el --- the study catalog and zones -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.3: studies expand into the composition DSL (series, fills,
;; rules, zones) and every one compiles to a spec eas draws natively.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-agent)

(defun financial-chart-catalog-test--bars ()
  "The 80 bars of the composed-chart examples, times in epoch ms."
  (mapcar (lambda (b) (plist-put (copy-sequence b) :time (eas-time-parse (plist-get b :time))))
          (append (plist-get (financial-chart-compose-example "candles") :bars) nil)))

(defun financial-chart-catalog-test--expand (chart)
  "CHART (with the example bars) expanded."
  (let ((bars (financial-chart-catalog-test--bars)))
    (financial-chart-catalog-expand (append (list :bars (vconcat bars)) chart) bars)))

(defun financial-chart-catalog-test--layers (spec pane)
  "Layer names of SPEC's PANE."
  (mapcar (lambda (l) (plist-get l :name)) (plist-get (aref (plist-get spec :vconcat) pane) :layer)))

(defmacro financial-chart-catalog-test--fails (code path &rest body)
  "Assert BODY signals `financial-chart-invalid-chart' with CODE and PATH."
  (declare (indent 2))
  `(let ((err (should-error (progn ,@body) :type 'financial-chart-invalid-chart)))
     (should (equal (list (plist-get (cddr err) :code) (plist-get (cddr err) :path)) (list ,code ,path)))
     (should (stringp (cadr err)))))

(ert-deftest financial-chart-catalog-every-study-draws-natively ()
  (let ((bars (vconcat (financial-chart-catalog-test--bars))))
    (pcase-dolist (`(,name ,place . ,_) financial-chart-studies)
      (let* ((chart (if (eq place 'price)
                        (list :bars bars :price (list :studies (vector name)))
                      (list :bars bars :panes (vector (list :study name)))))
             (spec (financial-chart-compose chart)))
        (should (equal (list name (eas-spec-unsupported spec)) (list name nil)))
        (should (equal (list name (plist-get (eas-agent "render" (eas-json-encode spec) :backend "text"
                                                        :cols 60 :rows 20)
                                             :ok))
                       (list name t)))))))

(ert-deftest financial-chart-catalog-channel-studies-shade-between-their-bands ()
  (let* ((price (plist-get (financial-chart-catalog-test--expand
                            '(:price (:studies [(:study "bollinger" :params [10 1.5])])))
                           :price))
         (series (append (plist-get price :series) nil)))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) series)
                   '("bollinger-10-1.5.upper" "bollinger-10-1.5.middle" "bollinger-10-1.5.lower")))
    (should (equal (plist-get (car series) :params) [10 1.5]))
    (should (equal (plist-get (aref (plist-get price :fills) 0) :between)
                   ["bollinger-10-1.5.upper" "bollinger-10-1.5.lower"]))
    (should (equal (plist-get (nth 1 series) :dash) [4 2])))
  (dolist (name '("keltner" "donchian" "envelopes"))
    (let ((spec (financial-chart-compose (list :bars (vconcat (financial-chart-catalog-test--bars))
                                               :price (list :studies (vector name))))))
      (should (member "price-fill-0-band" (financial-chart-catalog-test--layers spec 0))))))

(ert-deftest financial-chart-catalog-ichimoku-cloud-is-coloured-by-which-span-leads ()
  (let* ((bars (vconcat (financial-chart-catalog-test--bars)))
         (spec (financial-chart-compose (list :bars bars :colors '(:up "#00ff00" :down "#ff0000")
                                              :price '(:studies ["ichimoku"]))))
         (layers (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil))
         (above (cl-find "price-fill-0-above" layers :key (lambda (l) (plist-get l :name)) :test #'equal))
         (below (cl-find "price-fill-0-below" layers :key (lambda (l) (plist-get l :name)) :test #'equal))
         (xs (vconcat (mapcar (lambda (b) (plist-get b :time)) bars)))
         (cloud (append (plist-get (plist-get above :data) :values) nil)))
    (should (equal (plist-get (plist-get above :mark) :color) "#00ff00"))
    (should (equal (plist-get (plist-get below :mark) :color) "#ff0000"))
    ;; The cloud runs 26 (week)days past the last bar.
    (should (equal (plist-get (car (last cloud)) :time)
                   (car (last (financial-chart-shift-future-xs xs "temporal" 26)))))
    (should (equal (seq-take (seq-drop (financial-chart-catalog-test--layers spec 0) 4) 5)
                   '("series-ichimoku.tenkan" "series-ichimoku.kijun" "series-ichimoku.chikou"
                     "series-ichimoku.senkou-a" "series-ichimoku.senkou-b")))))

(ert-deftest financial-chart-catalog-oscillator-panes-have-levels-zones-and-excursions ()
  (let* ((pane (aref (plist-get (financial-chart-catalog-test--expand '(:panes [(:study "rsi" :params [21])]))
                                :panes)
                     0))
         (fills (append (plist-get pane :fills) nil)))
    (should (equal (plist-get pane :title) "RSI 21"))
    (should (equal (mapcar (lambda (r) (plist-get r :y)) (plist-get pane :rules)) '(70 30 50)))
    (should (equal (mapcar (lambda (f) (plist-get f :between)) fills) '([70 30] ["rsi-21" 70] ["rsi-21" 30])))
    (should (equal (plist-get (nth 1 fills) :below) "none"))
    (should (equal (plist-get (nth 2 fills) :above) "none")))
  ;; levels move the rules; "none" leaves one side of a fill undrawn.
  (let* ((spec (financial-chart-compose (list :bars (vconcat (financial-chart-catalog-test--bars))
                                              :panes [(:study "stochastic" :levels [90 10] :title "Slow")])))
         (names (financial-chart-catalog-test--layers spec 1))
         (rules (cl-remove-if-not (lambda (l) (plist-get l :data))
                                  (append (plist-get (aref (plist-get spec :vconcat) 1) :layer) nil))))
    (should (equal (seq-take names 3) '("pane-1-fill-0-band" "pane-1-fill-1-above" "pane-1-fill-2-below")))
    (should (member [(:level 90)] (mapcar (lambda (l) (plist-get (plist-get l :data) :values)) rules)))
    (should (equal (plist-get (plist-get (plist-get (aref (plist-get (aref (plist-get spec :vconcat) 1) :layer) 0)
                                                    :encoding) :y) :title)
                   "Slow")))
  ;; MACD: histogram coloured by sign under its line and signal.
  (let ((pane (aref (plist-get (financial-chart-catalog-test--expand
                                '(:colors (:up "#0000ff") :panes [(:study "macd")]))
                               :panes)
                    0)))
    (should (equal (mapcar (lambda (s) (plist-get s :output)) (plist-get pane :series))
                   '("macd-histogram" "macd" "macd-signal")))
    (should (equal (plist-get (aref (plist-get pane :series) 0) :above) "#0000ff"))))

(ert-deftest financial-chart-catalog-psar-ribbon-pivots-and-volume ()
  (let* ((bars (financial-chart-catalog-test--bars))
         (price (plist-get (financial-chart-catalog-test--expand
                            '(:price (:studies ["psar" (:study "ma-ribbon" :params ["sma" 5 10 15])])))
                           :price))
         (series (append (plist-get price :series) nil))
         (sar (financial-chart-parabolic-sar bars)))
    ;; Every SAR dot is in exactly one of the two series, by its side of the close.
    (cl-loop for v in sar for b in bars for i from 0
             for below = (aref (plist-get (nth 0 series) :values) i)
             for above = (aref (plist-get (nth 1 series) :values) i)
             do (should (equal (if v 1 0) (+ (if (numberp below) 1 0) (if (numberp above) 1 0))))
             do (when (numberp below) (should (< below (plist-get b :close)))))
    (should (equal (mapcar (lambda (s) (plist-get s :params)) (seq-drop series 2)) '([5] [10] [15])))
    (should (equal (plist-get (nth 2 series) :color) (car financial-chart-studies-ribbon-colors)))
    (should (equal (plist-get (nth 4 series) :color) (cadr financial-chart-studies-ribbon-colors))))
  (let ((price (plist-get (financial-chart-catalog-test--expand
                           '(:price (:studies [(:study "pivots" :params ["fibonacci" "week"])])))
                          :price)))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) (plist-get price :series))
                   '("pivots-fibonacci-week.pp" "pivots-fibonacci-week.r1" "pivots-fibonacci-week.r2"
                     "pivots-fibonacci-week.r3" "pivots-fibonacci-week.s1" "pivots-fibonacci-week.s2"
                     "pivots-fibonacci-week.s3")))
    (should (cl-every (lambda (s) (equal (plist-get s :style) "step")) (plist-get price :series))))
  (let ((spec (financial-chart-compose (list :bars (vconcat (financial-chart-catalog-test--bars))
                                             :panes [(:study "volume")]))))
    (should (equal (plist-get (aref (plist-get spec :vconcat) 1) :name) "volume"))))

(ert-deftest financial-chart-catalog-pane-keys-merge-after-the-study ()
  (let ((pane (aref (plist-get (financial-chart-catalog-test--expand
                                '(:panes [(:studies ["obv" "atr"] :series ["rsi"] :rules [5]
                                           :zones [(:from 1 :to 2 :color "#123456")] :height 40)]))
                               :panes)
                    0)))
    (should (equal (mapcar (lambda (s) (or (plist-get s :id) s)) (plist-get pane :series)) '("obv" "atr" "rsi")))
    (should (equal (plist-get pane :rules) [5]))
    (should (equal (plist-get pane :fills) [(:between [1 2] :color "#123456" :opacity 0.1)]))
    (should (equal (plist-get pane :height) 40))
    ;; Two studies in one pane: no single study title.
    (should-not (plist-get pane :title)))
  ;; Zones work in the price pane too, with no study.
  (let ((spec (financial-chart-compose (list :bars (vconcat (financial-chart-catalog-test--bars))
                                             :price '(:zones [(:from 100 :to 102)])))))
    (should (member "price-fill-0-band" (financial-chart-catalog-test--layers spec 0)))))

(ert-deftest financial-chart-catalog-failures-name-code-and-path ()
  (let ((bars (vconcat (financial-chart-catalog-test--bars))))
    (financial-chart-catalog-test--fails "UNKNOWN_STUDY" "/price/studies/0"
      (financial-chart-compose (list :bars bars :price '(:studies ["renko"]))))
    (financial-chart-catalog-test--fails "UNKNOWN_STUDY" "/panes/0/study"
      (financial-chart-compose (list :bars bars :panes [(:study "nope")])))
    (financial-chart-catalog-test--fails "STUDY_MISPLACED" "/price/studies/1"
      (financial-chart-compose (list :bars bars :price '(:studies ["bollinger" "rsi"]))))
    (financial-chart-catalog-test--fails "STUDY_MISPLACED" "/panes/1"
      (financial-chart-compose (list :bars bars :panes [(:study "rsi") (:study "ichimoku")])))
    (financial-chart-catalog-test--fails "INDICATOR_FAILED" "/panes/0/params"
      (financial-chart-compose (list :bars bars :panes [(:study "rsi" :params ["x"])])))
    (financial-chart-catalog-test--fails "INDICATOR_FAILED" "/price/studies/0/params"
      (financial-chart-compose (list :bars bars :price '(:studies [(:study "pivots" :params ["mystery"])]))))
    (financial-chart-catalog-test--fails "INVALID_STUDY" "/panes/0/levels"
      (financial-chart-compose (list :bars bars :panes [(:study "rsi" :levels [1 2 3])])))
    (financial-chart-catalog-test--fails "INVALID_STUDY" "/price/studies/0"
      (financial-chart-compose (list :bars bars :price '(:studies [7]))))
    (financial-chart-catalog-test--fails "UNKNOWN_INDICATOR" "/price/studies/0/params/0"
      (financial-chart-compose (list :bars bars :price '(:studies [(:study "ma-ribbon" :params ["zma" 5])]))))
    (financial-chart-catalog-test--fails "INDICATOR_FAILED" "/price/studies/0/params/1"
      (financial-chart-compose (list :bars bars :price '(:studies [(:study "ma-ribbon" :params [5 -1])]))))
    (financial-chart-catalog-test--fails "INVALID_ZONE" "/panes/0/zones/0"
      (financial-chart-compose (list :bars bars :panes [(:series ["rsi"] :zones [(:from 30)])])))))

(ert-deftest financial-chart-catalog-is-described ()
  (let ((studies (plist-get (financial-chart-compose-describe) :studies)))
    (should (= (length studies) (length financial-chart-studies)))
    (should (equal (plist-get (seq-find (lambda (s) (equal (plist-get s :name) "adx")) studies) :indicator)
                   "dmi"))
    (should (stringp (eas-json-encode (financial-chart-compose-describe))))))

(provide 'financial-chart-eas-catalog-test)
;;; financial-chart-eas-catalog-test.el ends here
