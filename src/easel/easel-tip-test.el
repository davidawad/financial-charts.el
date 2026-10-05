;;; easel-tip-test.el --- tests for tooltips, click targets and actions -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.1: encoding.tooltip and encoding.href on the runtime, driven
;; headlessly through `easel-dispatch' and `easel-replay'.

;;; Code:

(require 'easel-test-support)
(require 'easel)

(defmacro easel-tip-test--with-view (var source &rest body)
  "Open SOURCE (a template name or spec) as VAR in a fresh registry; run BODY.
A template name opens with its example bindings."
  (declare (indent 2))
  `(let ((easel-views (make-hash-table :test 'equal))
         (easel-action-inhibit nil))
     (let ((,var (if (stringp ,source)
                     (easel-view-open ,source :bindings (easel-template-example ,source) :id "t")
                   (easel-view-open ,source :id "t"))))
       ,@body)))

(defun easel-tip-test--item (view mark-id i)
  "Item I of MARK-ID in VIEW's scene."
  (aref (plist-get (easel-scene-mark (easel-view-scene view) mark-id) :items) i))

(defun easel-tip-test--centre (view mark-id i)
  "Pixel centre of item I of MARK-ID in VIEW's scene."
  (let ((item (easel-tip-test--item view mark-id i)))
    (if (plist-member item :w)
        (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))
      (vector (plist-get item :x) (plist-get item :y)))))

(defun easel-tip-test--anchor (view mark-id k)
  "Pixel of datum K on the first series of MARK-ID in VIEW's scene."
  (let ((p (aref (plist-get (easel-tip-test--item view mark-id 0) :points) k)))
    (vector (aref p 0) (aref p 1))))

(defun easel-tip-test--titles (tooltip)
  "TOOLTIP as an alist of (TITLE . VALUE)."
  (mapcar (lambda (p) (cons (plist-get p :title) (plist-get p :value))) tooltip))

(defconst easel-tip-test--href-spec
  '(:data (:values [(:k "a" :v 3 :url "https://example.com/a") (:k "b" :v 5 :url "https://example.com/b")])
    :width 200 :height 100
    :layer [(:mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                    :href (:field "url")))
            (:mark "line" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                     :href (:field "url")
                                     :tooltip [(:field "k" :type "nominal") (:field "v" :type "quantitative")]))])
  "Bars and a line, both linking each datum to its url.")

;;; The ohlc template (the demo's candlesticks)

(ert-deftest easel-tip-ohlc-template-golden ()
  (easel-test-golden "resolve-ohlc.json"
                     (easel-json-pretty (easel-resolve "ohlc" (easel-template-example "ohlc"))))
  (should (equal (plist-get (plist-get (plist-get (easel-template-get "ohlc") :meta) :actions) :candles) "echo")))

;;; Tooltips

(ert-deftest easel-tip-bars-hover-reports-the-bar-tooltip ()
  (easel-tip-test--with-view v "bars"
    (let ((inspect (easel-dispatch v (list :type "pointermove" :px (easel-tip-test--centre v "main/0" 1)))))
      (should (equal (easel-tip-test--titles (plist-get (plist-get inspect :hover) :tooltip))
                     '(("category" . "Tue") ("value" . "10400"))))
      (easel-dispatch v '(:type "pointerleave"))
      (should (eq (plist-get (easel-inspect v) :hover) :null)))))

(ert-deftest easel-tip-series-tooltip-comes-from-the-hit-index ()
  ;; One path per series: the datum is found by bisecting x, and its
  ;; tooltip is encoded from the plan, not read off the item.
  (easel-tip-test--with-view v "line"
    (should-not (plist-get (easel-tip-test--item v "main/0" 0) :tooltip))
    (let* ((px (easel-tip-test--anchor v "main/0" 4))
           (hover (plist-get (easel-dispatch v (list :type "pointermove" :px (vector (+ 2 (aref px 0)) (aref px 1))))
                             :hover)))
      (should (equal (plist-get hover :datum) 4))
      (should (equal (easel-tip-test--titles (plist-get hover :tooltip))
                     '(("date" . "Mar 06, 2026") ("value" . "108.9")))))))

(ert-deftest easel-tip-hot-spots-and-text-cells-carry-help-echo ()
  (easel-tip-test--with-view v "bars"
    (let ((areas (easel-svg-hot-spots (easel-view-scene v))))
      (should (= (length areas) 5))
      (should (equal (plist-get (nth 2 (car areas)) 'help-echo) "category: Mon\nvalue: 8200"))))
  (easel-tip-test--with-view v "ohlc"
    (easel-view-resize v '(:cols 60 :rows 16) 'text)
    (let ((text (easel-text-render (easel-view-scene v))))
      (should (seq-some (lambda (i) (let ((h (get-text-property i 'help-echo text)))
                                      (and h (string-match-p "close: 416" h))))
                        (number-sequence 0 (1- (length text))))))))

;;; Click targets and actions

(ert-deftest easel-tip-click-records-the-target ()
  (easel-tip-test--with-view v "bars"
    (let* ((click (plist-get (easel-dispatch v (list :type "click" :px (easel-tip-test--centre v "main/0" 2))) :click)))
      (should (equal (plist-get click :mark) "main/0"))
      (should (equal (plist-get click :row) '(:category "Wed" :value 6100)))
      (should (equal (cdr (assoc "value" (easel-tip-test--titles (plist-get click :tooltip)))) "6100"))
      (should (eq (plist-get click :href) :null))
      ;; Nothing is bound and there is no href: recorded, nothing runs.
      (should (eq (plist-get click :action) :null)))
    ;; A click on empty plot space clears it; other events leave it.
    (easel-dispatch v (list :type "click" :px (easel-tip-test--centre v "main/0" 2)))
    (easel-dispatch v (list :type "pointermove" :px [5 5]))
    (should-not (eq (plist-get (easel-inspect v) :click) :null))
    (let* ((b (plist-get (aref (plist-get (easel-view-scene v) :views) 0) :bounds)))
      (easel-dispatch v (list :type "click" :px (vector (+ (aref b 0) 2) (+ (aref b 1) 2)))))
    (should (eq (plist-get (easel-inspect v) :click) :null))))

(ert-deftest easel-tip-ohlc-template-actions-run-on-mouse-1 ()
  ;; mouse-1 is pointerdown + pointerup; the template binds candles to echo.
  (easel-tip-test--with-view v "ohlc"
    (let* ((px (easel-tip-test--centre v "candles" 0)) (inhibit-message t))
      (easel-dispatch v (list :type "pointerdown" :px px))
      (let ((click (plist-get (easel-dispatch v (list :type "pointerup" :px px)) :click)))
        (should (equal (plist-get click :mark) "candles"))
        (should (equal (plist-get click :action) "echo"))
        (should (eq (plist-get click :ran) t))
        (should (string-match-p "date: Aug 20, 2026\nopen: 408.6\nhigh: 417.96\nlow: 407.72\nclose: 416"
                                (plist-get click :result))))
      ;; A press that moves past the click slop is a drag, not a click.
      (easel-dispatch v '(:type "click" :px [0 0]))
      (easel-dispatch v (list :type "pointerdown" :px px))
      (easel-dispatch v (list :type "pointerup" :px (vector (+ 10 (aref px 0)) (aref px 1))))
      (should (eq (plist-get (easel-inspect v) :click) :null)))))

(ert-deftest easel-tip-href-opens-for-bars-and-series ()
  (easel-tip-test--with-view v easel-tip-test--href-spec
    (let* ((opened nil) (easel-action-browse-function (lambda (url) (push url opened))))
      (let ((click (plist-get (easel-dispatch v (list :type "click" :px (easel-tip-test--centre v "main/0" 1))) :click)))
        (should (equal (plist-get click :href) "https://example.com/b"))
        (should (equal (plist-get click :action) "open-href"))
        (should (equal (plist-get click :result) "https://example.com/b")))
      ;; Off the bars but near the line's first datum: the series hit.
      (let* ((px (easel-tip-test--anchor v "main/1" 0))
             (click (plist-get (easel-dispatch v (list :type "click" :px (vector (aref px 0) (- (aref px 1) 8))))
                               :click)))
        (should (equal (plist-get click :mark) "main/1"))
        (should (equal (plist-get click :href) "https://example.com/a"))
        (should (equal (easel-tip-test--titles (plist-get click :tooltip)) '(("k" . "a") ("v" . "3")))))
      (should (equal opened '("https://example.com/a" "https://example.com/b"))))))

(ert-deftest easel-tip-actions-bind-by-mark-param-and-wildcard ()
  (easel-tip-test--with-view v '(:data (:values [(:k "a" :v 3) (:k "b" :v 5)]) :width 200 :height 100
                                 :params [(:name "pick" :select (:type "point" :on "click"))]
                                 :mark "bar"
                                 :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
    (let ((px (easel-tip-test--centre v "main/0" 0)) (calls nil))
      (easel-action-define "test-note" (lambda (target _view) (push (plist-get target :row) calls) "noted"))
      (easel-action-bind v "*" "copy-row")
      (let ((kill-ring nil))
        (should (equal (plist-get (plist-get (easel-dispatch v (list :type "click" :px px)) :click) :action) "copy-row"))
        (should (equal (car kill-ring) "{\"k\":\"a\",\"v\":3}")))
      (easel-action-bind v "pick" "test-note")
      (should (equal (plist-get (plist-get (easel-dispatch v (list :type "click" :px px)) :click) :result) "noted"))
      (easel-action-bind v "main/0" "echo")
      (let ((inhibit-message t))
        (should (equal (plist-get (plist-get (easel-dispatch v (list :type "click" :px px)) :click) :action) "echo")))
      (should (equal calls '((:k "a" :v 3))))
      (should (equal (plist-get (easel-test-should-code "NOT_FOUND" (easel-action-bind v "*" "nope")) :action)
                     "nope"))
      (should (member "open-href" (mapcar (lambda (a) (plist-get a :name)) (easel-action-describe)))))))

(ert-deftest easel-tip-failing-action-is-data-not-an-error ()
  (easel-tip-test--with-view v "bars"
    (easel-action-define "test-boom" (lambda (_target _view) (error "Kaboom")))
    (easel-action-bind v "*" "test-boom")
    (let ((click (plist-get (easel-dispatch v (list :type "click" :px (easel-tip-test--centre v "main/0" 0))) :click)))
      (should (eq (plist-get click :ran) :false))
      (should (equal (plist-get (plist-get click :error) :code) "ENGINE_FAILED"))
      (should (string-match-p "Kaboom" (plist-get (plist-get click :error) :message))))))

(ert-deftest easel-tip-replay-and-inhibit-record-without-running ()
  (easel-tip-test--with-view v "bars"
    (let ((runs 0) (px (easel-tip-test--centre v "main/0" 3)))
      (easel-action-define "test-count" (lambda (_target _view) (cl-incf runs)))
      (easel-action-bind v "*" "test-count")
      (easel-dispatch v (list :type "click" :px px))
      (should (= runs 1))
      (let ((easel-action-inhibit t))
        (should (eq (plist-get (plist-get (easel-dispatch v (list :type "click" :px px)) :click) :ran) :false)))
      (let ((log (easel-view-log v)))
        (easel-tip-test--with-view w "bars"
          (easel-action-bind w "*" "test-count")
          (let ((click (plist-get (easel-replay w log) :click)))
            (should (equal (plist-get click :action) "test-count"))
            (should (eq (plist-get click :ran) :false))
            (should (equal (plist-get click :row) '(:category "Thu" :value 9300))))))
      (should (= runs 1)))))

;;; Glue: echo area and RET in buffers

(ert-deftest easel-tip-hover-echoes-series-tooltips-in-buffers ()
  (easel-tip-test--with-view v "line"
    (let* ((shown nil)
           (easel-mode-tip-display-function (lambda (text _view) (push text shown))))
      (with-temp-buffer
        (setf (easel-view-buffer v) (current-buffer))
        (easel-dispatch v (list :type "pointermove" :px (easel-tip-test--anchor v "main/0" 1)))
        (easel-dispatch v (list :type "pointermove" :px (easel-tip-test--anchor v "main/0" 1)))
        (easel-dispatch v '(:type "pointerleave"))
        (should (equal (reverse shown) '("date: Mar 03, 2026\nvalue: 104.1" nil))))))
  ;; GUI discrete marks: the :map area's help-echo is Emacs's job.
  (easel-tip-test--with-view v "bars"
    (let* ((shown nil)
           (easel-mode-tip-display-function (lambda (text _view) (push text shown))))
      (with-temp-buffer
        (setf (easel-view-buffer v) (current-buffer))
        (easel-dispatch v (list :type "pointermove" :px (easel-tip-test--centre v "main/0" 0)))
        (should-not shown)
        (easel-view-resize v '(:cols 60 :rows 16) 'text)
        (easel-dispatch v '(:type "pointerleave"))
        (easel-dispatch v (list :type "pointermove" :px (easel-tip-test--centre v "main/0" 0)))
        (should (equal shown '("category: Mon\nvalue: 8200"))))))
  ;; The default display falls back to the echo area off a GUI frame.
  (easel-tip-test--with-view v "line"
    (with-temp-buffer
      (let ((inhibit-message t))
        (easel-mode-tip-display "date: x" v))
      (should (equal (current-message) nil))
      (should easel-mode-tip--shown))))

(ert-deftest easel-tip-ret-at-point-clicks-in-a-text-buffer ()
  (easel-tip-test--with-view v "bars"
    (easel-view-resize v '(:cols 60 :rows 16) 'text)
    (easel-action-bind v "*" "copy-row")
    (with-temp-buffer
      (easel-view-mode)
      (setq easel-mode--view v)
      (setf (easel-view-buffer v) (current-buffer))
      (let ((inhibit-read-only t)) (insert (easel-text-render (easel-view-scene v))))
      (goto-char (point-min))
      (while (not (equal (get-text-property (point) 'easel-datum) 4)) (forward-char 1))
      (let ((kill-ring nil))
        (easel-mode-click-at-point)
        (should (equal (car kill-ring) "{\"category\":\"Fri\",\"value\":12050}")))
      (should (equal (plist-get (plist-get (easel-inspect v) :click) :mark) "main/0")))))

(provide 'easel-tip-test)
;;; easel-tip-test.el ends here
