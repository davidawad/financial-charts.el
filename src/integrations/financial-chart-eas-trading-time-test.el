;;; financial-chart-eas-trading-time-test.el --- the trading-time x axis -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-gbo.5: composed charts put bars on trading time (one slot per
;; bar, no weekend, holiday or overnight gaps) with date ticks at
;; sensible boundaries; "x": "calendar" keeps calendar time.

;;; Code:

(require 'ert)
(require 'financial-chart-test-support)
(require 'financial-chart)
(require 'eas-agent)

(defun financial-chart-trading-test--times (&rest dates)
  "DATES (ISO strings) as a vector of epoch ms."
  (vconcat (mapcar #'eas-time-parse dates)))

(defun financial-chart-trading-test--weekdays (from count)
  "COUNT weekday ISO dates from FROM (an ISO date) on."
  (let ((ms (eas-time-parse from)) out)
    (while (< (length out) count)
      (unless (memq (decoded-time-weekday (decode-time (/ ms 1000) t)) '(0 6))
        (push (format-time-string "%F" (/ ms 1000) t) out))
      (setq ms (+ ms 86400000)))
    (nreverse out)))

(defun financial-chart-trading-test--bars (dates)
  "Flat bars at DATES (ISO strings or epoch ms)."
  (vconcat (seq-map-indexed (lambda (d i) (list :time d :open (+ 100 i) :high (+ 101 i)
                                                :low (+ 99 i) :close (+ 100.5 i)))
                            dates)))

(defun financial-chart-trading-test--x (spec &optional pane layer)
  "The x encoding of SPEC's LAYER (index, default 0) in PANE (default 0)."
  (plist-get (plist-get (aref (plist-get (aref (plist-get spec :vconcat) (or pane 0)) :layer) (or layer 0))
                        :encoding)
             :x))

(defmacro financial-chart-trading-test--fails (code path &rest body)
  "Assert BODY signals `financial-chart-invalid-chart' with CODE and PATH."
  (declare (indent 2))
  `(let ((err (should-error (progn ,@body) :type 'financial-chart-invalid-chart)))
     (should (equal (list (plist-get (cddr err) :code) (plist-get (cddr err) :path)) (list ,code ,path)))
     (should (stringp (cadr err)))))

;;; Ticks

(ert-deftest financial-chart-trading-ticks-pick-the-finest-unit-that-fits ()
  ;; 80 weekdays from March: weeks are too many, months fit.
  (let ((times (apply #'financial-chart-trading-test--times
                      (financial-chart-trading-test--weekdays "2026-03-02" 80))))
    (should (equal (financial-chart-trading-ticks times) '((22 . "Apr") (44 . "May") (65 . "Jun"))))
    ;; With room for 16, weeks: each Monday, labelled with its date.
    (let ((weeks (financial-chart-trading-ticks times 16)))
      (should (= (length weeks) 15))
      (should (equal (car weeks) '(5 . "Mar 9")))))
  ;; Twenty weekdays: weeks.
  (should (equal (mapcar #'cdr (financial-chart-trading-ticks
                                (apply #'financial-chart-trading-test--times
                                       (financial-chart-trading-test--weekdays "2026-03-02" 20))))
                 '("Mar 9" "Mar 16" "Mar 23")))
  ;; Five days: each day.
  (should (equal (mapcar #'cdr (financial-chart-trading-ticks
                                (financial-chart-trading-test--times "2026-03-02" "2026-03-03" "2026-03-04")))
                 '("Mar 3" "Mar 4")))
  ;; A new year labels the year.
  (should (equal (financial-chart-trading-ticks
                  (apply #'financial-chart-trading-test--times
                         (financial-chart-trading-test--weekdays "2026-11-02" 90)))
                 '((21 . "Dec") (44 . "2027") (65 . "Feb") (85 . "Mar"))))
  ;; Five years of month-end bars: years.
  (should (equal (mapcar #'cdr (financial-chart-trading-ticks
                                (vconcat (cl-loop for y from 2020 to 2024
                                                  append (cl-loop for m from 1 to 12
                                                                  collect (eas-time-parse (format "%d-%02d-28" y m)))))))
                 '("2021" "2022" "2023" "2024")))
  ;; One bar: its date.
  (should (equal (financial-chart-trading-ticks (financial-chart-trading-test--times "2026-03-02"))
                 '((0 . "Mar 2")))))

(ert-deftest financial-chart-trading-ticks-on-intraday-bars ()
  ;; Hourly bars over three sessions: each day opens with its date.
  (let ((times (vconcat (cl-loop for day in '("2026-03-02" "2026-03-03" "2026-03-04")
                                 append (cl-loop for h from 14 to 20
                                                 collect (+ (eas-time-parse day) (* h 3600000)))))))
    (should (equal (financial-chart-trading-ticks times) '((7 . "Mar 3") (14 . "Mar 4")))))
  ;; Five-minute bars from 14:30 to 17:25: half hours.
  (let ((times (vconcat (cl-loop for i below 36
                                 collect (+ (eas-time-parse "2026-03-02") (* 14 3600000) (* 30 60000)
                                            (* i 300000))))))
    (should (equal (mapcar #'cdr (financial-chart-trading-ticks times))
                   '("15:00" "15:30" "16:00" "16:30" "17:00")))))

(ert-deftest financial-chart-trading-axis-labels-only-its-ticks ()
  (should (equal (financial-chart-trading-axis '((3 . "Apr") (9 . "May")))
                 '(:values [3 9] :labelExpr "datum.value == 3 ? 'Apr' : datum.value == 9 ? 'May' : ''"))))

;;; Dates to bars

(ert-deftest financial-chart-trading-index-maps-a-date-to-its-bar-or-the-nearest ()
  (let ((times (financial-chart-trading-test--times "2026-03-05" "2026-03-06" "2026-03-09" "2026-03-10")))
    (should (= (financial-chart-trading-index times (eas-time-parse "2026-03-06")) 1))
    ;; Saturday is nearer Friday, Sunday nearer Monday.
    (should (= (financial-chart-trading-index times (eas-time-parse "2026-03-07")) 1))
    (should (= (financial-chart-trading-index times (eas-time-parse "2026-03-08")) 2))
    ;; Saturday noon is 1.5 days from Friday and from Monday: a tie
    ;; goes to the later bar.
    (should (= (financial-chart-trading-index times (+ (eas-time-parse "2026-03-07") 43200000)) 2))
    (should-not (financial-chart-trading-index times (eas-time-parse "2026-03-04")))
    (should-not (financial-chart-trading-index times (eas-time-parse "2026-03-11")))))

;;; Composed charts

(ert-deftest financial-chart-trading-compose-closes-weekend-and-overnight-gaps ()
  (let* ((dates '("2026-03-05" "2026-03-06" "2026-03-09" "2026-03-10"))
         (spec (financial-chart-compose (list :bars (financial-chart-trading-test--bars dates))))
         (rows (append (plist-get (plist-get spec :data) :values) nil))
         (x (financial-chart-trading-test--x spec)))
    ;; Friday and Monday sit one slot apart; the date rides along.
    (should (equal (mapcar (lambda (r) (plist-get r :time)) rows) '(0 1 2 3)))
    (should (equal (mapcar (lambda (r) (plist-get r :date)) rows) (mapcar #'eas-time-parse dates)))
    (should (equal (plist-get x :type) "quantitative"))
    (should (equal (plist-get x :scale) '(:domain [-0.5 3.5] :nice :false :zero :false)))
    (should (equal (plist-get (plist-get x :axis) :values) [1 2 3]))
    ;; The crosshair reads the date.
    (should (member '(:field "date" :type "temporal" :title "date")
                    (append (plist-get (plist-get (cl-find "price-hit" (plist-get (aref (plist-get spec :vconcat) 0) :layer)
                                                           :key (lambda (l) (plist-get l :name)) :test #'equal)
                                                  :encoding)
                                       :tooltip)
                            nil)))
    (should-not (eas-spec-unsupported spec))
    (should (eq (plist-get (eas-agent "render" (eas-json-encode spec) :backend "text" :cols 50 :rows 12) :ok) t)))
  ;; Intraday: the last bar of one session and the first of the next are adjacent.
  (let* ((times (cl-loop for day in '("2026-03-02" "2026-03-03")
                         append (cl-loop for h from 14 to 20 collect (+ (eas-time-parse day) (* h 3600000)))))
         (spec (financial-chart-compose (list :bars (financial-chart-trading-test--bars times))))
         (rows (append (plist-get (plist-get spec :data) :values) nil)))
    (should (equal (mapcar (lambda (r) (plist-get r :time)) rows) (number-sequence 0 13)))
    (should (equal (plist-get (plist-get (financial-chart-trading-test--x spec) :axis) :labelExpr)
                   "datum.value == 7 ? 'Mar 3' : ''"))))

(ert-deftest financial-chart-trading-calendar-keeps-calendar-time ()
  (let* ((dates '("2026-03-05" "2026-03-06" "2026-03-09"))
         (bars (financial-chart-trading-test--bars dates)))
    (dolist (x '("calendar" (:scale "calendar")))
      (let* ((spec (financial-chart-compose (list :bars bars :x x)))
             (rows (append (plist-get (plist-get spec :data) :values) nil))
             (enc (financial-chart-trading-test--x spec)))
        (should (equal (mapcar (lambda (r) (plist-get r :time)) rows) (mapcar #'eas-time-parse dates)))
        (should-not (plist-member (car rows) :date))
        (should (equal (plist-get enc :type) "temporal"))
        (should-not (plist-get enc :scale))))
    ;; Ticks cap the date labels.
    (let ((spec (financial-chart-compose
                 (list :bars (financial-chart-trading-test--bars
                              (financial-chart-trading-test--weekdays "2026-03-02" 80))
                       :x '(:ticks 16)))))
      (should (= (length (plist-get (plist-get (financial-chart-trading-test--x spec) :axis) :values)) 15))))
  ;; Bars without times stay on their indices.
  (let ((spec (financial-chart-compose '(:bars [(:open 1 :high 2 :low 0.5 :close 1.5)
                                                (:open 1.5 :high 3 :low 1 :close 2.5)]))))
    (should-not (plist-get (financial-chart-trading-test--x spec) :axis))
    (should (equal (plist-get (financial-chart-trading-test--x spec) :type) "quantitative")))
  (let ((bars (financial-chart-trading-test--bars '("2026-03-05"))))
    (financial-chart-trading-test--fails "INVALID_X" "/x"
      (financial-chart-compose (list :bars bars :x "lunar")))
    (financial-chart-trading-test--fails "INVALID_X" "/x/scale"
      (financial-chart-compose (list :bars bars :x '(:scale "log"))))
    (financial-chart-trading-test--fails "INVALID_X" "/x/ticks"
      (financial-chart-compose (list :bars bars :x '(:ticks 0))))))

(ert-deftest financial-chart-trading-annotations-land-on-bars ()
  (let* ((dates (financial-chart-trading-test--weekdays "2026-03-02" 10))
         (spec (financial-chart-compose
                (list :bars (financial-chart-trading-test--bars dates)
                      :price '(:annotations [(:type "event" :at "2026-03-07" :label "Sat")
                                             (:type "event" :at "2026-03-08")
                                             (:type "sell" :at "2026-03-09")
                                             (:type "box" :from ["2026-03-03" 1] :to ["2026-03-10" 2])]))))
         (layers (append (plist-get (aref (plist-get spec :vconcat) 0) :layer) nil))
         (row (lambda (name) (aref (plist-get (plist-get (cl-find name layers :key (lambda (l) (plist-get l :name))
                                                                    :test #'equal)
                                                          :data)
                                                :values)
                                   0))))
    ;; Saturday lands on Friday (slot 4), Sunday on Monday (slot 5).
    (should (= (plist-get (funcall row "price-annotation-0") :time) 4))
    (should (= (plist-get (funcall row "price-annotation-1") :time) 5))
    (should (= (plist-get (funcall row "price-annotation-2") :time) 5))
    (should (equal (list (plist-get (funcall row "price-annotation-3") :time)
                         (plist-get (funcall row "price-annotation-3") :time2))
                   '(1 6))))
  (financial-chart-trading-test--fails "NO_SUCH_BAR" "/price/annotations/0/at"
    (financial-chart-compose (list :bars (financial-chart-trading-test--bars '("2026-03-05" "2026-03-06"))
                                   :price '(:annotations [(:type "event" :at "2026-04-01")])))))

(ert-deftest financial-chart-trading-forward-shifts-add-labelled-slots ()
  (let* ((dates (financial-chart-trading-test--weekdays "2026-03-02" 60))
         (spec (financial-chart-compose
                (list :bars (financial-chart-trading-test--bars dates)
                      :price '(:series [(:indicator "sma" :params [5] :shift 26 :id "ahead")]
                               :annotations [(:type "event" :at "2026-06-08")]))))
         (x (financial-chart-trading-test--x spec))
         (event (cl-find "price-annotation-0" (plist-get (aref (plist-get spec :vconcat) 0) :layer)
                         :key (lambda (l) (plist-get l :name)) :test #'equal)))
    ;; 60 bars and 26 future slots, the last a weekday 26 sessions on.
    (should (equal (plist-get (plist-get x :scale) :domain) [-0.5 85.5]))
    (should (string-match-p "'Jun'" (plist-get (plist-get x :axis) :labelExpr)))
    ;; A date among the future slots maps to its slot.
    (should (= (plist-get (aref (plist-get (plist-get event :data) :values) 0) :time) 70))))

(provide 'financial-chart-eas-trading-time-test)
;;; financial-chart-eas-trading-time-test.el ends here
