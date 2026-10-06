;;; financial-chart-eas-annotations-test.el --- markers and annotations -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.3: buy/sell arrows, levels, trend lines, events, text, boxes
;; and Fibonacci retracements on any pane of a composed chart.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-agent)

(defun financial-chart-annotation-test--chart (annotations &optional pane)
  "The candles example's bars with ANNOTATIONS on the price pane (or PANE)."
  (let ((bars (plist-get (financial-chart-compose-example "candles") :bars)))
    (if pane
        (list :bars bars :panes (vector (list :series ["rsi"] :annotations annotations)))
      (list :bars bars :price (list :annotations annotations)))))

(defun financial-chart-annotation-test--layers (chart &optional pane)
  "Layers of CHART's PANE (default 0) whose name mentions an annotation."
  (cl-remove-if-not (lambda (l) (string-match-p "annotation" (or (plist-get l :name) "")))
                    (append (plist-get (aref (plist-get (financial-chart-compose chart) :vconcat) (or pane 0))
                                       :layer)
                            nil)))

(defun financial-chart-annotation-test--rows (layer)
  "LAYER's own data rows."
  (append (plist-get (plist-get layer :data) :values) nil))

(defmacro financial-chart-annotation-test--fails (code path &rest body)
  "Assert BODY signals `financial-chart-invalid-chart' with CODE and PATH."
  (declare (indent 2))
  `(let ((err (should-error (progn ,@body) :type 'financial-chart-invalid-chart)))
     (should (equal (list (plist-get (cddr err) :code) (plist-get (cddr err) :path)) (list ,code ,path)))
     (should (stringp (cadr err)))))

(ert-deftest financial-chart-annotation-buy-and-sell-arrows-sit-off-the-bar ()
  (let* ((bars (append (plist-get (financial-chart-compose-example "candles") :bars) nil))
         (bar (lambda (date) (seq-find (lambda (b) (equal (plist-get b :time) date)) bars)))
         (layers (financial-chart-annotation-test--layers
                  (financial-chart-annotation-test--chart
                   [(:type "buy" :at ["2026-03-13" "2026-05-26"] :label "B")
                    (:type "sell" :at "2026-04-02" :color "#000000")
                    (:type "buy" :at "2026-03-16" :y 50)])))
         (buys (financial-chart-annotation-test--rows (car layers)))
         (sell (car (financial-chart-annotation-test--rows (nth 2 layers)))))
    (should (equal (mapcar (lambda (l) (plist-get l :name)) layers)
                   '("price-annotation-0" "price-annotation-0-label" "price-annotation-1" "price-annotation-2")))
    (should (equal (plist-get (plist-get (car layers) :mark) :shape) "triangle-up"))
    (should (equal (plist-get (plist-get (nth 2 layers) :mark) :shape) "triangle-down"))
    (should (equal (plist-get (plist-get (car layers) :mark) :color) financial-chart-palette-up))
    (should (equal (plist-get (plist-get (nth 2 layers) :mark) :color) "#000000"))
    (should (= (length buys) 2))
    (should (equal (plist-get (car buys) :time) (eas-time-parse "2026-03-13")))
    (should (< (plist-get (car buys) :y) (plist-get (funcall bar "2026-03-13") :low)))
    (should (> (plist-get sell :y) (plist-get (funcall bar "2026-04-02") :high)))
    (should (equal (plist-get (car buys) :label) "B"))
    (should (equal (plist-get sell :label) "sell"))
    (should (= (plist-get (car (financial-chart-annotation-test--rows (nth 3 layers))) :y) 50))))

(ert-deftest financial-chart-annotation-levels-trendlines-events-text-boxes ()
  (let* ((chart (financial-chart-annotation-test--chart
                 [(:type "level" :y 105 :label "R")
                  (:type "level" :y 100 :from "2026-04-01" :to "2026-05-01")
                  (:type "trendline" :from ["2026-05-01" 101] :to ["2026-05-08" 100] :extend "right")
                  (:type "event" :at "2026-04-15" :label "Earnings")
                  (:type "text" :at "2026-03-20" :y 96 :label "note")
                  (:type "box" :from ["2026-05-05" 106] :to ["2026-04-20" 103] :label "range")]))
         (spec (financial-chart-compose chart))
         (layers (financial-chart-annotation-test--layers chart))
         (named (lambda (n) (cl-find n layers :key (lambda (l) (plist-get l :name)) :test #'equal)))
         (rows (lambda (n) (financial-chart-annotation-test--rows (funcall named n))))
         (xs (mapcar (lambda (r) (plist-get r :time)) (plist-get (plist-get spec :data) :values))))
    ;; A level spans the chart unless given from/to; its label sits at the end.
    (should (equal (car (funcall rows "price-annotation-0"))
                   (list :time (car xs) :time2 (car (last xs)) :y 105)))
    (should (equal (plist-get (car (funcall rows "price-annotation-0-label")) :label) "R"))
    (should (equal (plist-get (car (funcall rows "price-annotation-1")) :time) (eas-time-parse "2026-04-01")))
    ;; A trend line extended right keeps its slope to the last bar.
    (let ((r (car (funcall rows "price-annotation-2")))
          (slope (/ -1.0 (- (eas-time-parse "2026-05-08") (eas-time-parse "2026-05-01")))))
      (should (equal (plist-get r :time2) (car (last xs))))
      (should (< (abs (- (plist-get r :y2) (+ 100 (* slope (- (car (last xs)) (eas-time-parse "2026-05-08"))))))
                 1e-9))
      (should (equal (plist-get (plist-get (plist-get (funcall named "price-annotation-2") :encoding) :y2) :field)
                     "y2")))
    ;; An event is a vertical rule with its label at the top of the pane.
    (should-not (plist-get (plist-get (funcall named "price-annotation-3") :encoding) :y))
    (should (equal (plist-get (plist-get (funcall named "price-annotation-3-label") :encoding) :y) '(:value 0)))
    (should (equal (plist-get (car (funcall rows "price-annotation-4")) :label) "note"))
    ;; A box is normalised to low/high corners.
    (should (equal (car (funcall rows "price-annotation-5"))
                   (list :time (eas-time-parse "2026-04-20") :time2 (eas-time-parse "2026-05-05") :y 103 :y2 106)))
    (should-not (eas-spec-unsupported spec))
    (should (eq (plist-get (eas-agent "render" (eas-json-encode spec) :backend "text" :cols 70 :rows 20) :ok) t)))
  ;; Annotations work in any pane.
  (let ((layers (financial-chart-annotation-test--layers
                 (financial-chart-annotation-test--chart [(:type "level" :y 50)] t)
                 1)))
    (should (equal (mapcar (lambda (l) (plist-get l :name)) layers) '("pane-1-annotation-0")))))

(ert-deftest financial-chart-annotation-trendline-keeps-its-segment-whatever-the-order ()
  (dolist (extend '("right" "left" "both" "none"))
    (let* ((chart (list :bars (vconcat (cl-loop for i below 10 collect (list :open 100 :high 101 :low 99 :close 100)))
                        :price (list :annotations (vector (list :type "trendline" :from [6 106] :to [2 102]
                                                                :extend extend)))))
           (row (car (financial-chart-annotation-test--rows (car (financial-chart-annotation-test--layers chart))))))
      (should (<= (plist-get row :time) 2))
      (should (>= (plist-get row :time2) 6))
      ;; Slope 1: y = x + 100 all along.
      (should (= (plist-get row :y) (+ 100 (plist-get row :time))))
      (should (= (plist-get row :y2) (+ 100 (plist-get row :time2)))))))

(ert-deftest financial-chart-annotation-fibonacci-retraces-the-swing ()
  (should (equal (financial-chart-fibonacci-levels 100 200 [0 0.5 1]) '((0 . 200) (0.5 . 150.0) (1 . 100))))
  ;; The swing runs from the earlier extreme to the later one.
  (let ((bars '((:high 5 :low 4) (:high 9 :low 6) (:high 7 :low 1) (:high 6 :low 2))))
    (should (equal (financial-chart-fibonacci-swing bars [0 1 2 3]) '((1 . 9) (2 . 1)))))
  (let ((bars '((:high 5 :low 4) (:high 6 :low 1) (:high 9 :low 6) (:high 7 :low 5))))
    (should (equal (financial-chart-fibonacci-swing bars [0 1 2 3]) '((1 . 1) (2 . 9))))
    (should (equal (financial-chart-fibonacci-swing bars [0 1 2 3] 2) '((2 . 9) (3 . 5)))))
  (let* ((chart (financial-chart-annotation-test--chart [(:type "fibonacci")]))
         (layers (financial-chart-annotation-test--layers chart))
         (levels (financial-chart-annotation-test--rows
                  (cl-find "price-annotation-0" layers :key (lambda (l) (plist-get l :name)) :test #'equal)))
         (bars (append (plist-get chart :bars) nil))
         (hi (apply #'max (mapcar (lambda (b) (plist-get b :high)) bars)))
         (lo (apply #'min (mapcar (lambda (b) (plist-get b :low)) bars))))
    (should (= (length levels) 7))
    (should (equal (sort (mapcar (lambda (r) (plist-get r :y)) levels) #'<)
                   (sort (mapcar #'cdr (financial-chart-fibonacci-levels lo hi financial-chart-fibonacci-ratios)) #'<)))
    (should (member "61.8%" (mapcar (lambda (r) (car (split-string (plist-get r :label)))) levels)))
    (should (equal (mapcar (lambda (l) (plist-get l :name)) layers)
                   '("price-annotation-0-bands-0" "price-annotation-0-bands-1" "price-annotation-0"
                     "price-annotation-0-swing" "price-annotation-0-labels"))))
  (let* ((layers (financial-chart-annotation-test--layers
                  (financial-chart-annotation-test--chart
                   [(:type "fibonacci" :from ["2026-03-02" 100] :to ["2026-04-02" 110] :levels [0.5 1.618])])))
         (levels (financial-chart-annotation-test--rows (car (cl-remove-if-not
                                                              (lambda (l) (equal (plist-get l :name) "price-annotation-0"))
                                                              layers)))))
    (should (equal (mapcar (lambda (r) (plist-get r :y)) levels) (list 105.0 (- 110 (* 1.618 10)))))))

(ert-deftest financial-chart-annotation-failures-name-code-and-path ()
  (financial-chart-annotation-test--fails "NO_SUCH_BAR" "/price/annotations/0/at"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "buy" :at "2026-03-01")])))
  (financial-chart-annotation-test--fails "NO_SUCH_BAR" "/price/annotations/0/at/1"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "sell" :at ["2026-03-02" "1999-01-01"])])))
  (financial-chart-annotation-test--fails "INVALID_TIME" "/price/annotations/0/at"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "event" :at "soon")])))
  (financial-chart-annotation-test--fails "UNKNOWN_ANNOTATION" "/panes/0/annotations/0/type"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "arrow")] t)))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/1"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "level" :y 1) "level"])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/y"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "level")])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/to"
    (financial-chart-compose (financial-chart-annotation-test--chart
                              [(:type "trendline" :from ["2026-03-02" 1] :to [1])])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/extend"
    (financial-chart-compose (financial-chart-annotation-test--chart
                              [(:type "trendline" :from ["2026-03-02" 1] :to ["2026-03-03" 2] :extend "up")])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/label"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "text" :at "2026-03-02" :y 1)])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/levels"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "fibonacci" :levels ["a"])])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/window"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "fibonacci" :window 1)])))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations"
    (financial-chart-compose (financial-chart-annotation-test--chart "level")))
  ;; Without bar times, "at" is a bar index.
  (let ((spec (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5) (:open 1.5 :high 3 :low 1 :close 2.5)]
                                         :price (:annotations [(:type "buy" :at 1)])))))
    (should spec))
  (financial-chart-annotation-test--fails "INVALID_TIME" "/price/annotations/0/at"
    (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5)]
                               :price (:annotations [(:type "buy" :at "2026-01-01")]))))
  ;; Off the price pane, markers need y and Fibonacci needs its ends.
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/panes/0/annotations/0/y"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "buy" :at "2026-03-02")] t)))
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/panes/0/annotations/0/from"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "fibonacci")] t)))
  (should (financial-chart-compose (financial-chart-annotation-test--chart
                                    [(:type "sell" :at "2026-03-02" :y 80)
                                     (:type "fibonacci" :from ["2026-03-02" 20] :to ["2026-04-01" 80])]
                                    t)))
  ;; Malformed values fail as annotations, never as Lisp type errors.
  (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/panes/0/annotations/0/y"
    (financial-chart-compose (financial-chart-annotation-test--chart [(:type "buy" :at "2026-03-02" :y "a")] t)))
  (dolist (type '("trendline" "box" "fibonacci"))
    (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/from"
      (financial-chart-compose (financial-chart-annotation-test--chart
                                (vector (list :type type :from 3 :to ["2026-03-03" 2]))))))
  (dolist (levels '("abc" 5))
    (financial-chart-annotation-test--fails "INVALID_ANNOTATION" "/price/annotations/0/levels"
      (financial-chart-compose (financial-chart-annotation-test--chart
                                (vector (list :type "fibonacci" :levels levels))))))
  ;; A pane of annotations alone draws.
  (should (financial-chart-compose (financial-chart-annotation-test--chart [(:type "level" :y 1)] t))))

(provide 'financial-chart-eas-annotations-test)
;;; financial-chart-eas-annotations-test.el ends here
