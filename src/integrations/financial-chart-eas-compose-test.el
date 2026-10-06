;;; financial-chart-eas-compose-test.el --- the composition DSL -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.2: a declarative chart (price style, overlays, fills, panes)
;; compiles to one plain eas spec.  Every price style has an example
;; (examples/compose/STYLE.json) with text and SVG goldens under
;; test/golden/compose/; EAS_UPDATE_GOLDEN=1 (or
;; FINANCIAL_CHART_UPDATE_GOLDEN=1) rewrites them.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-agent)

(defun financial-chart-compose-test--golden (name actual)
  "Compare ACTUAL with golden NAME under test/golden/compose/."
  (let ((file (expand-file-name name (expand-file-name "test/golden/compose"
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

(defun financial-chart-compose-test--bars ()
  "The ohlc shape's example bars (48, epoch-ms times)."
  (plist-get (alist-get 'ohlc financial-chart-shapes) :example))

(defun financial-chart-compose-test--layers (spec pane)
  "Layers of SPEC's PANE (an index) as a list."
  (append (plist-get (aref (plist-get spec :vconcat) pane) :layer) nil))

(defun financial-chart-compose-test--layer (spec pane name)
  "The layer NAME of SPEC's PANE."
  (cl-find name (financial-chart-compose-test--layers spec pane)
           :key (lambda (l) (plist-get l :name)) :test #'equal))

(defmacro financial-chart-compose-test--fails (code path &rest body)
  "Assert BODY signals `financial-chart-invalid-chart' with CODE and PATH."
  (declare (indent 2))
  `(let ((err (should-error (progn ,@body) :type 'financial-chart-invalid-chart)))
     (should (equal (plist-get (cddr err) :code) ,code))
     (should (equal (plist-get (cddr err) :path) ,path))
     (should (stringp (cadr err)))
     err))

;;; Every style

(ert-deftest financial-chart-compose-every-style-has-an-example-that-draws-natively ()
  (should (= (length financial-chart-styles) 8))
  (dolist (style (mapcar #'car financial-chart-styles))
    (let* ((chart (financial-chart-compose-example style))
           (spec (financial-chart-compose chart)))
      (should (equal (list style (financial-chart-series-get (plist-get chart :price) :style))
                     (list style style)))
      (should (equal (list style (eas-spec-unsupported spec)) (list style nil)))
      ;; eas alone renders the compiled spec: the verb envelope is ok.
      (should (eq (plist-get (eas-agent "render" (eas-json-encode spec) :backend "text"
                                        :cols 60 :rows 20)
                             :ok)
                  t)))))

(ert-deftest financial-chart-compose-text-goldens ()
  (dolist (style (mapcar #'car financial-chart-styles))
    (financial-chart-compose-test--golden
     (format "%s.txt" style)
     (substring-no-properties
      (financial-chart-compose-render (financial-chart-compose-example style)
                                      :width 90 :height 30)))))

(ert-deftest financial-chart-compose-svg-goldens ()
  (dolist (style (mapcar #'car financial-chart-styles))
    (let ((svg (financial-chart-compose-render (financial-chart-compose-example style)
                                               :backend 'svg :width 640 :height 420)))
      (should (string-prefix-p "<svg" svg))
      (financial-chart-compose-test--golden
       (format "%s.svg" style) (concat (replace-regexp-in-string "><" ">\n<" svg) "\n")))))

;;; Structure

(ert-deftest financial-chart-compose-panes-share-x-crosshair-and-zoom ()
  (let* ((spec (financial-chart-compose (financial-chart-compose-example "candles")))
         (panes (plist-get spec :vconcat))
         (params (plist-get spec :params))
         (names (mapcar (lambda (p) (plist-get p :name)) panes)))
    (should (equal names '("price" "volume" "pane-2" "pane-3")))
    (should (equal (plist-get (plist-get spec :resolve) :scale)
                   '(:x "shared" :y "independent" :color "independent")))
    (should (equal (mapcar (lambda (p) (plist-get p :name)) params) '("crosshair" "zoom")))
    (dolist (param (append params nil))
      (should (equal (plist-get param :views)
                     ["price-hit" "volume-hit" "pane-2-hit" "pane-3-hit"])))
    ;; Each pane has its hit layer and a crosshair rule; only the bottom
    ;; pane labels the x axis.
    (seq-doseq (pane panes)
      (let ((layers (append (plist-get pane :layer) nil)))
        (should (cl-find (format "%s-hit" (plist-get pane :name)) layers
                         :key (lambda (l) (plist-get l :name)) :test #'equal))
        (should (equal (plist-get (car (last layers)) :transform)
                       [(:filter (:param "crosshair" :empty :false))]))))
    (should (equal (plist-get (plist-get (plist-get (financial-chart-compose-test--layer spec 0 "wicks")
                                                    :encoding)
                                         :x)
                              :axis)
                   '(:labels :false)))
    (should (equal (plist-get (plist-get (plist-get (financial-chart-compose-test--layer spec 3 "pane-3-hit")
                                                    :encoding)
                                         :x)
                              :title)
                   "date"))
    (should-not (plist-get (financial-chart-compose
                            (list :bars (financial-chart-compose-test--bars) :crosshair :false))
                           :params))
    (should-not (plist-member (financial-chart-compose
                               (list :bars (financial-chart-compose-test--bars) :crosshair :false))
                              :params))
    (should (equal (plist-get (financial-chart-compose (list :bars (financial-chart-compose-test--bars)))
                              :params)
                   (financial-chart-compose--params ["price-hit"])))))

(ert-deftest financial-chart-compose-overlays-carry-the-indicator-numbers ()
  (let* ((bars (financial-chart-compose-test--bars))
         (spec (financial-chart-compose
                (list :bars bars
                      :price '(:series [(:indicator "sma" :params [5]) (:indicator "bollinger-bands")])
                      :panes [(:series ["rsi"])])))
         (rows (append (plist-get (plist-get spec :data) :values) nil))
         (sma (plist-get (financial-chart-indicator-evaluate 'sma bars 5) :values))
         (upper (plist-get (car (financial-chart-indicator-evaluate 'bollinger-bands bars)) :values))
         (rsi (plist-get (financial-chart-indicator-evaluate 'rsi bars) :values)))
    (should (= (length rows) 48))
    (should (equal (mapcar (lambda (r) (plist-get r :time)) rows)
                   (mapcar (lambda (b) (plist-get b :time)) bars)))
    (cl-loop for (col . expected) in `((:s0 . ,sma) (:s1 . ,upper) (:s4 . ,rsi))
             do (should (equal (mapcar (lambda (r) (let ((v (plist-get r col))) (if (eq v :null) nil v)))
                                       rows)
                               expected)))
    ;; Bollinger's three outputs are three series, ids with .OUTPUT.
    (should (equal (mapcar (lambda (l) (plist-get l :name))
                           (cl-remove-if-not (lambda (l) (string-prefix-p "series-" (plist-get l :name)))
                                             (financial-chart-compose-test--layers spec 0)))
                   '("series-sma-5" "series-bollinger-bands.bollinger-upper"
                     "series-bollinger-bands.bollinger-middle" "series-bollinger-bands.bollinger-lower")))
    ;; RSI's bounds become the pane's domain.
    (should (equal (plist-get (plist-get (plist-get (plist-get (financial-chart-compose-test--layer
                                                                spec 1 "series-rsi")
                                                               :encoding)
                                                    :y)
                                         :scale)
                              :domain)
                   [0 100]))))

(ert-deftest financial-chart-compose-series-styling ()
  (let* ((spec (financial-chart-compose
                (list :bars (financial-chart-compose-test--bars)
                      :price '(:style "line" :color "#111111" :width 3 :dash [5 1]
                               :series [(:indicator "ema" :params [10] :color "#abcdef" :width 2.5 :dash [4 2])
                                        (:indicator "sma" :params [5] :style "step")
                                        (:field "high" :style "dots")])
                      :panes [(:series [(:indicator "macd" :output "macd-histogram"
                                                      :above "#00ff00" :below "#ff0000")])])))
         (ema (financial-chart-compose-test--layer spec 0 "series-ema-10"))
         (price (financial-chart-compose-test--layer spec 0 "price-line"))
         (hist (financial-chart-compose-test--layer spec 1 "series-macd.macd-histogram")))
    (should (equal (plist-get price :mark)
                   '(:type "line" :color "#111111" :strokeWidth 3 :strokeDash [5 1])))
    (should (equal (plist-get (plist-get ema :mark) :strokeWidth) 2.5))
    (should (equal (plist-get (plist-get ema :mark) :strokeDash) [4 2]))
    (should (equal (plist-get (plist-get (plist-get ema :encoding) :color) :datum) "EMA 10"))
    (should (member "#abcdef" (append (plist-get (plist-get (plist-get (plist-get ema :encoding) :color)
                                                            :scale)
                                                 :range)
                                      nil)))
    (should (equal (plist-get (plist-get (financial-chart-compose-test--layer spec 0 "series-sma-5") :mark)
                              :interpolate)
                   "step-after"))
    (should (equal (plist-get (plist-get (financial-chart-compose-test--layer spec 0 "series-high") :mark)
                              :type)
                   "point"))
    (should (equal (plist-get (plist-get hist :mark) :type) "bar"))
    (should (equal (plist-get (plist-get hist :encoding) :color)
                   '(:condition (:test "datum.s3 >= 0" :value "#00ff00") :value "#ff0000")))))

;;; Price styles

(ert-deftest financial-chart-compose-heikin-ashi-math ()
  (let ((ha (financial-chart-styles-heikin-ashi
             '((:open 10 :high 14 :low 9 :close 12) (:open 12 :high 13 :low 8 :close 9)))))
    (should (equal (cdr (assoc "ha_close" ha)) [11.25 10.5]))
    (should (equal (cdr (assoc "ha_open" ha)) [11.0 11.125]))
    (should (equal (cdr (assoc "ha_high" ha)) [14 13]))
    (should (equal (cdr (assoc "ha_low" ha)) [9 8])))
  (let* ((spec (financial-chart-compose (financial-chart-compose-example "heikin-ashi")))
         (row (aref (plist-get (plist-get spec :data) :values) 1)))
    (should (numberp (plist-get row :ha_open)))
    (should (equal (plist-get (plist-get (plist-get (financial-chart-compose-test--layer spec 0 "bodies")
                                                    :encoding)
                                         :y)
                              :field)
                   "ha_open"))))

(ert-deftest financial-chart-compose-price-styles-and-up-down-colours ()
  (let ((bars (financial-chart-compose-test--bars)))
    (cl-flet ((names (style &rest more)
                (mapcar (lambda (l) (plist-get l :name))
                        (financial-chart-compose-test--layers
                         (financial-chart-compose (list :bars bars :price (append (list :style style) more)))
                         0))))
      (should (equal (names "candles") '("wicks" "bodies" "price-hit" nil)))
      (should (equal (names "hollow") '("wicks" "bodies-up" "bodies-down" "price-hit" nil)))
      (should (equal (names "ohlc") '("ranges" "opens" "closes" "price-hit" nil)))
      (should (equal (names "area") '("price-area" "price-line" "price-hit" nil)))
      (should (equal (names "baseline" :baseline 101)
                     '("baseline-above" "baseline-below" "baseline" "price-line" "price-hit" nil))))
    (let* ((spec (financial-chart-compose (list :bars bars :colors '(:up "#0000ff" :down "#ffa500")
                                                :price '(:style "hollow"))))
           (up (financial-chart-compose-test--layer spec 0 "bodies-up")))
      (should (equal (plist-get (plist-get up :mark) :stroke) "#0000ff"))
      (should (equal (plist-get (plist-get up :mark) :fill) "white"))
      (should (equal (plist-get (plist-get (financial-chart-compose-test--layer spec 0 "bodies-down") :mark)
                                :color)
                     "#ffa500")))
    ;; The baseline's level defaults to the first close.
    (let ((rule (financial-chart-compose-test--layer
                 (financial-chart-compose (list :bars bars :price '(:style "baseline"))) 0 "baseline")))
      (should (equal (plist-get (plist-get rule :data) :values)
                     (vector (list :level (plist-get (car bars) :close))))))))

(ert-deftest financial-chart-compose-bars-without-time-use-their-index ()
  (let* ((spec (financial-chart-compose
                '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5) (:open 1.5 :high 3 :low 1 :close 2.5)])))
         (rows (plist-get (plist-get spec :data) :values)))
    (should (equal (mapcar (lambda (r) (plist-get r :time)) rows) '(0 1)))
    (should (equal (plist-get (plist-get (plist-get (financial-chart-compose-test--layer spec 0 "wicks")
                                                    :encoding)
                                         :x)
                              :type)
                   "quantitative"))
    (should (stringp (financial-chart-compose-render
                      '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5) (:open 1.5 :high 3 :low 1 :close 2.5)])
                      :width 40 :height 10)))))

;;; Fills

(ert-deftest financial-chart-compose-fill-rows-switch-where-the-lines-cross ()
  (let ((rows (financial-chart-styles-fill-rows [0 10 20 30] [1 3 nil 5] [2 2 2 2])))
    ;; 1<2 then 3>2: a crossing row at x=5 where both are 2; the nil
    ;; at x=20 starts segment 1.
    (should (equal rows '((:time 0 :a 1 :b 2 :lo 1 :seg 0)
                          (:time 5.0 :a 2.0 :b 2.0 :lo 2.0 :seg 0)
                          (:time 10 :a 3 :b 2 :lo 2 :seg 0)
                          (:time 30 :a 5 :b 2 :lo 2 :seg 1)))))
  (let* ((spec (financial-chart-compose (financial-chart-compose-example "candles")))
         (above (financial-chart-compose-test--layer spec 0 "price-fill-0-above"))
         (below (financial-chart-compose-test--layer spec 0 "price-fill-0-below"))
         (sma10 (plist-get (financial-chart-indicator-evaluate
                            'sma (append (plist-get (financial-chart-compose-example "candles") :bars) nil) 10)
                           :values))
         (rows (append (plist-get (plist-get above :data) :values) nil)))
    (should (equal (plist-get (plist-get above :mark) :color) "#26a69a"))
    (should (equal (plist-get (plist-get below :mark) :color) "#ef5350"))
    (should (equal (plist-get (plist-get above :encoding) :y2) '(:field "lo")))
    ;; Every bar where both SMAs exist, plus the crossings.
    (should (> (length rows) (- 80 29)))
    (should (cl-some (lambda (r) (= (plist-get r :a) (plist-get r :b))) rows))
    (should (equal (plist-get (car rows) :a) (nth 29 sma10))))
  ;; One colour: a single band between A and B; numbers are levels.
  (let ((spec (financial-chart-compose
               (list :bars (financial-chart-compose-test--bars)
                     :panes [(:series ["rsi"] :fills [(:between ["rsi" 70] :color "#ff0000")])]))))
    (should (equal (plist-get (plist-get (financial-chart-compose-test--layer spec 1 "pane-1-fill-0-band") :mark)
                              :color)
                   "#ff0000"))))

;;; Palette

(ert-deftest financial-chart-compose-palette-is-stable-and-distinct ()
  (let ((financial-chart-palette '("c0" "c1" "c2" "c3"))
        (financial-chart-palette-homes '(("sma" . 0) ("ema" . 1))))
    (should (equal (financial-chart-palette-assign '((a . "sma") (b . "sma") (c . "ema") (a . "sma")))
                   '((a . "c0") (b . "c2") (c . "c1"))))
    ;; EMA keeps its home whatever comes before it, unless taken.
    (should (equal (financial-chart-palette-assign '((c . "ema"))) '((c . "c1"))))
    (should (equal (financial-chart-palette-home "unlisted") (financial-chart-palette-home "unlisted"))))
  (let* ((spec (financial-chart-compose
                (list :bars (financial-chart-compose-test--bars)
                      :price '(:series [(:indicator "sma" :params [20]) (:indicator "sma" :params [5])])
                      :panes [(:series [(:indicator "sma" :params [20])])])))
         (colour (lambda (pane name)
                   (let* ((enc (plist-get (financial-chart-compose-test--layer spec pane name) :encoding))
                          (c (plist-get enc :color))
                          (scale (plist-get c :scale)))
                     (aref (plist-get scale :range)
                           (seq-position (plist-get scale :domain) (plist-get c :datum)))))))
    (should (equal (funcall colour 0 "series-sma-20") (car financial-chart-palette)))
    (should-not (equal (funcall colour 0 "series-sma-5") (funcall colour 0 "series-sma-20")))
    ;; The same series in another pane keeps its colour and its column.
    (should (equal (funcall colour 1 "series-sma-20") (funcall colour 0 "series-sma-20")))
    (should (= (length (cl-remove-if-not (lambda (k) (string-match-p "\\`:s[0-9]" (symbol-name k)))
                                         (eas-plist-keys (aref (plist-get (plist-get spec :data) :values) 0))))
               2))))

;;; Doors and failures

(ert-deftest financial-chart-compose-reads-json-text-and-files ()
  (let* ((file (expand-file-name "step.json" financial-chart-compose-examples-directory))
         (json (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (should (equal (financial-chart-compose file) (financial-chart-compose json)))
    (should (equal (financial-chart-compose file)
                   (financial-chart-compose (financial-chart-compose-example "step"))))))

(ert-deftest financial-chart-compose-shell-door ()
  (let* ((file (expand-file-name "ohlc.json" financial-chart-compose-examples-directory))
         (spec (with-output-to-string
                 (let ((command-line-args-left (list "--" file)))
                   (financial-chart-compose-main))))
         (text (with-output-to-string
                 (let ((command-line-args-left (list file "--backend" "text" "--cols" "50" "--rows" "12")))
                   (financial-chart-compose-main)))))
    (should (equal (eas-json-canonical (eas-json-parse spec))
                   (eas-json-canonical (eas-json-parse (eas-json-encode (financial-chart-compose file))))))
    (should (string-match-p "OHLC bars" text))
    (should (string-match-p "%K" text)))
  (let ((failure (financial-chart-compose-failure
                  (should-error (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5)]
                                                           :price (:style "renko")))))))
    (should (equal (plist-get failure :reason) "UNKNOWN_STYLE"))
    (should (equal (plist-get failure :path) "/price/style"))
    (should (eq (plist-get failure :ok) :false))))

(ert-deftest financial-chart-compose-describe-names-every-style-and-indicator ()
  (let ((d (financial-chart-compose-describe)))
    (should (equal (mapcar (lambda (s) (plist-get s :name)) (plist-get d :styles))
                   (mapcar #'car financial-chart-styles)))
    (should (seq-contains-p (plist-get d :indicators) "rsi"))
    (should (stringp (eas-json-encode d)))
    (dolist (style (mapcar #'car financial-chart-styles))
      (should (file-exists-p (expand-file-name (concat style ".json")
                                               financial-chart-compose-examples-directory))))))

(ert-deftest financial-chart-compose-failures-name-code-and-path ()
  (let ((bars (financial-chart-compose-test--bars)))
    (financial-chart-compose-test--fails "NO_BARS" "/bars" (financial-chart-compose '(:bars [])))
    (financial-chart-compose-test--fails "UNKNOWN_STYLE" "/price/style"
      (financial-chart-compose (list :bars bars :price '(:style "renko"))))
    (financial-chart-compose-test--fails "UNKNOWN_INDICATOR" "/panes/0/series/1/indicator"
      (financial-chart-compose (list :bars bars :panes [(:series ["rsi" "nope"])])))
    (financial-chart-compose-test--fails "UNKNOWN_OUTPUT" "/price/series/0/output"
      (financial-chart-compose (list :bars bars :price '(:series [(:indicator "macd" :output "x")]))))
    (financial-chart-compose-test--fails "INDICATOR_FAILED" "/price/series/0/params"
      (financial-chart-compose (list :bars bars :price '(:series [(:indicator "sma" :params ["x"])]))))
    (financial-chart-compose-test--fails "LENGTH_MISMATCH" "/price/series/0/values"
      (financial-chart-compose (list :bars bars :price '(:series [(:values [1 2] :label "v")]))))
    (financial-chart-compose-test--fails "UNKNOWN_FIELD" "/price/series/0/field"
      (financial-chart-compose (list :bars bars :price '(:series [(:field "vwap")]))))
    (financial-chart-compose-test--fails "UNKNOWN_STYLE" "/price/series/0/style"
      (financial-chart-compose (list :bars bars :price '(:series [(:indicator "sma" :style "ribbon")]))))
    (financial-chart-compose-test--fails "UNKNOWN_SERIES" "/price/fills/0/between/1"
      (financial-chart-compose (list :bars bars :price '(:series ["sma"] :fills [(:between ["sma" "ema"])]))))
    (financial-chart-compose-test--fails "INVALID_FILL" "/price/fills/0/between"
      (financial-chart-compose (list :bars bars :price '(:fills [(:between ["close"])]))))
    (financial-chart-compose-test--fails "INVALID_PANE" "/panes/0"
      (financial-chart-compose (list :bars bars :panes ["rsi"])))
    (financial-chart-compose-test--fails "EMPTY_PANE" "/panes/1"
      (financial-chart-compose (list :bars bars :panes [(:rules [1]) ()])))
    (financial-chart-compose-test--fails "NO_VOLUME" "/panes/0/volume"
      (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5)] :panes [(:volume t)])))
    (financial-chart-compose-test--fails "INVALID_TIME" "/bars/1/time"
      (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5 :time "2026-01-02")
                                        (:open 1 :high 2 :low 0.5 :close 1.5)])))
    (let ((err (financial-chart-compose-test--fails "INVALID_BAR" "/bars/1"
                 (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5)
                                                   (:open 1 :high 2 :low 0.5)])))))
      (should (equal (plist-get (cddr err) :index) 1)))
    ;; Every failure is a financial-chart-error.
    (should-error (financial-chart-compose '(:bars [])) :type 'financial-chart-error)))

(provide 'financial-chart-eas-compose-test)
;;; financial-chart-eas-compose-test.el ends here
