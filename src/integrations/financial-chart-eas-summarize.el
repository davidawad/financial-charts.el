;;; financial-chart-eas-summarize.el --- a composed chart's scene digest for agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `financial-chart-summarize' (fc-gty.8, docs/design/markups.md
;; "Reading a chart without looking at it") reads what a composed chart
;; shows as a few hundred tokens of data:
;;
;;   {"bars-id": "...", "last": {"time", "close"},
;;    "levels": [{"id", "y", "side", "distance_pct", "touches"}],
;;    "fib": {"nearest": {"ratio", "y"}, "swing": [FROM, TO]},
;;    "recent_events": [{"id", "bars_ago", "direction", "strength"}],
;;    "series_state": {"sma-7": {"value", "state": "up"}, "rsi": {"value", "state": "overbought"}},
;;    "evidence": {"pattern.hammer": {"status": "untested"}},  (recent events' detectors)
;;    "text": "Close 134.76 ..."}
;;
;; Levels come from the chart's support-resistance annotations, the
;; Fibonacci swing from its fibonacci annotations, events from every
;; detector-backed annotation (`financial-chart-chart-events') confirmed
;; in the last SINCE bars, series states from
;; `financial-chart-series-state-functions' (by indicator) or the
;; defaults: a bounded oscillator against its reference levels, anything
;; else by its slope over the last 5 bars.  Evidence comes from
;; `financial-chart-evidence-verdict-function' when set (fc-gty.9),
;; else "untested".

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-detector-api)
(require 'financial-chart-patterns)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-catalog)
(require 'financial-chart-eas-presets)
(require 'financial-chart-eas-markups)

(declare-function financial-chart-compose-read "financial-chart-eas-compose" (chart))
(declare-function financial-chart-compose--bars "financial-chart-eas-compose" (chart))
(declare-function financial-chart-compose--pane-series "financial-chart-eas-compose" (pane bars path))

(defvar financial-chart-series-reference-levels
  '(("rsi" 70 30) ("money-flow-index" 80 20) ("stochastic" 80 20) ("williams-r" -20 -80)
    ("ultimate-oscillator" 70 30))
  "Overbought and oversold levels of bounded oscillators, by indicator.")

(defvar financial-chart-series-state-functions
  '(("adx" . financial-chart-summarize--adx-state)
    ("dmi" . financial-chart-summarize--adx-state))
  "Indicator name to a function of (VALUES BARS) returning a state string.
An indicator not listed uses `financial-chart-series-reference-levels'
or its slope.")

(defvar financial-chart-evidence-verdict-function nil
  "Function of a detector name returning its evidence plist, or nil.
Set by the evaluate layer (fc-gty.9); nil means every detector is untested.")

(defun financial-chart-summarize--adx-state (values _bars)
  "Trending when the last of VALUES is 25 or more, else ranging."
  (let ((v (car (last (delq nil (append values nil))))))
    (and v (if (>= v 25) "trending" "ranging"))))

(defun financial-chart-summarize--slope-state (values bars)
  "up, down or flat: the last of VALUES against 5 bars before, in average ranges."
  (let* ((v (vconcat values)) (n (length v)) (k 5))
    (when (and (> n k) (aref v (1- n)) (aref v (- n 1 k)))
      (let* ((range (/ (cl-loop for i from (- n k) below n
                                for b = (nth i bars)
                                sum (- (plist-get b :high) (plist-get b :low)))
                       (float k)))
             (slope (/ (- (aref v (1- n)) (aref v (- n 1 k))) (* k (if (> range 0) range 1.0)))))
        (cond ((> slope 0.05) "up") ((< slope -0.05) "down") (t "flat"))))))

(defun financial-chart-summarize--state (indicator values bars)
  "The state of series VALUES of INDICATOR over BARS."
  (let ((fn (cdr (assoc indicator financial-chart-series-state-functions)))
        (levels (cdr (assoc indicator financial-chart-series-reference-levels)))
        (last (car (last (delq nil (append values nil))))))
    (cond (fn (funcall fn values bars))
          ((and levels last) (cond ((>= last (car levels)) "overbought")
                                   ((<= last (cadr levels)) "oversold")
                                   (t "neutral")))
          (t (financial-chart-summarize--slope-state values bars)))))

(defun financial-chart-summarize--annotations (chart type)
  "Every annotation of TYPE in CHART's panes."
  (cl-loop for pane in (cons (financial-chart-series-get chart :price)
                             (append (financial-chart-series-get chart :panes) nil))
           append (seq-filter (lambda (a) (equal (financial-chart-series-get a :type) type))
                              (append (financial-chart-series-get pane :annotations) nil))))

(defun financial-chart-summarize--round (x)
  "X to 2 decimals."
  (and (numberp x) (/ (fround (* (float x) 100)) 100.0)))

(cl-defun financial-chart-summarize (chart &key (since 20) (max-events 8))
  "A scene digest of CHART (a plist, JSON text or file): what it shows.
SINCE is how many bars back counts as recent (20); MAX-EVENTS caps the
recent events (8).  Pure."
  (let* ((chart (financial-chart-presets-apply (financial-chart-compose-read chart)))
         (bars (car (financial-chart-compose--bars chart)))
         (v (vconcat bars)) (n (length v))
         (last-bar (aref v (1- n)))
         (close (plist-get last-bar :close))
         (expanded (financial-chart-catalog-expand chart bars))
         ;; Levels.
         (levels (cl-loop for a in (financial-chart-summarize--annotations chart "support-resistance")
                          append (financial-chart-support-resistance
                                  bars :left (financial-chart-series-get a :left)
                                  :right (financial-chart-series-get a :right)
                                  :atr (financial-chart-series-get a :atr)
                                  :tolerance (financial-chart-series-get a :tolerance)
                                  :max (or (financial-chart-series-get a :max) 4))))
         (levels (mapcar (lambda (l)
                           (list :id (plist-get l :id)
                                 :y (financial-chart-summarize--round (plist-get l :price))
                                 :side (if (< (plist-get l :price) close) "below" "above")
                                 :distance_pct (financial-chart-summarize--round
                                                (* 100 (/ (- (plist-get l :price) close) close)))
                                 :touches (plist-get l :touches)))
                         (sort levels (lambda (a b) (< (abs (- (plist-get a :price) close))
                                                       (abs (- (plist-get b :price) close)))))))
         ;; Fibonacci.
         (fib-a (car (financial-chart-summarize--annotations chart "fibonacci")))
         (fib (when fib-a
                (let* ((e (car (last (financial-chart-detect
                                      "fib.retracement" v
                                      (cl-loop for k in '(:window :levels) when (plist-member fib-a k)
                                               append (list k (plist-get fib-a k)))))))
                       (g (plist-get e :geometry))
                       (nearest (car (sort (append (plist-get g :levels) nil)
                                           (lambda (a b) (< (abs (- (plist-get a :y) close))
                                                            (abs (- (plist-get b :y) close))))))))
                  (when e
                    (list :nearest (list :ratio (plist-get nearest :ratio)
                                         :y (financial-chart-summarize--round (plist-get nearest :y)))
                          :swing (vector (aref (plist-get g :from) 0) (aref (plist-get g :to) 0)))))))
         ;; Events.
         (events (seq-remove (lambda (e) (or (string-prefix-p "constant." (plist-get e :detector))
                                             (member (plist-get e :detector) '("level.auto-sr" "fib.retracement"))))
                             (financial-chart-chart-events chart)))
         (recent (seq-filter (lambda (e) (>= (plist-get e :confirmed_index) (- n since))) events))
         (recent (seq-take (sort recent (lambda (a b) (> (plist-get a :index) (plist-get b :index)))) max-events))
         (recent (mapcar (lambda (e) (list :id (plist-get e :id) :bars_ago (- n 1 (plist-get e :index))
                                           :direction (plist-get e :direction)
                                           :strength (financial-chart-summarize--round (plist-get e :strength))))
                         recent))
         ;; Series.
         (series (cl-loop for pane in (cons (financial-chart-series-get expanded :price)
                                            (append (financial-chart-series-get expanded :panes) nil))
                          for i from 0
                          append (financial-chart-compose--pane-series pane bars (format "/s/%d" i))))
         (states (cl-loop for s in series
                          for values = (plist-get s :values)
                          for value = (car (last (delq nil (append values nil))))
                          when value
                          append (list (intern (concat ":" (plist-get s :id)))
                                       (list :value (financial-chart-summarize--round value)
                                             :state (or (financial-chart-summarize--state
                                                         (car (plist-get s :key)) values bars)
                                                        :null)))))
         ;; Evidence.
         (detectors (delete-dups (mapcar (lambda (e) (car (split-string (plist-get e :id) "@"))) recent)))
         (evidence (cl-loop for d in detectors
                            append (list (intern (concat ":" d))
                                         (or (and financial-chart-evidence-verdict-function
                                                  (funcall financial-chart-evidence-verdict-function d))
                                             (list :status "untested")))))
         (time (financial-chart-detector--time v (1- n))))
    (list :bars-id (financial-chart-bars-id bars)
          :last (list :time time :close close)
          :levels (vconcat levels)
          :fib (or fib :null)
          :recent_events (vconcat recent)
          :series_state states
          :evidence evidence
          :text (financial-chart-summarize--text close levels fib recent states evidence))))

(defun financial-chart-summarize--text (close levels fib recent states evidence)
  "One paragraph of the digest: CLOSE, LEVELS, FIB, RECENT, STATES, EVIDENCE."
  (let* ((below (seq-find (lambda (l) (equal (plist-get l :side) "below")) levels))
         (above (seq-find (lambda (l) (equal (plist-get l :side) "above")) levels))
         (osc (cl-loop for (k v) on states by #'cddr
                       when (member (plist-get v :state) '("overbought" "oversold"))
                       collect (format "%s %s %s" (substring (symbol-name k) 1) (plist-get v :value)
                                       (plist-get v :state))))
         (trend (cl-loop for (k v) on states by #'cddr
                         when (member (plist-get v :state) '("up" "down"))
                         collect (format "%s %s" (substring (symbol-name k) 1) (plist-get v :state))))
         (event (car recent)))
    (string-join
     (delq nil
           (list (format "Close %s%s." close
                         (cond ((and below above) (format " between support %s (%d touches, %s%%) and resistance %s (%d, +%s%%)"
                                                          (plist-get below :y) (plist-get below :touches) (plist-get below :distance_pct)
                                                          (plist-get above :y) (plist-get above :touches) (plist-get above :distance_pct)))
                               (below (format ", above every level; nearest support %s (%d touches, %s%%)"
                                              (plist-get below :y) (plist-get below :touches) (plist-get below :distance_pct)))
                               (above (format ", below every level; nearest resistance %s (%d touches, +%s%%)"
                                              (plist-get above :y) (plist-get above :touches) (plist-get above :distance_pct)))
                               (t "")))
                 (when fib (format "Nearest Fibonacci level %s%% at %s."
                                   (financial-chart-summarize--round (* 100 (plist-get (plist-get fib :nearest) :ratio)))
                                   (plist-get (plist-get fib :nearest) :y)))
                 (when osc (concat (string-join osc ", ") "."))
                 (when trend (concat (string-join trend ", ") "."))
                 (when event
                   (let ((d (car (split-string (plist-get event :id) "@"))))
                     (format "Last event: %s %d bars ago (%s)." d (plist-get event :bars_ago)
                             (plist-get (plist-get evidence (intern (concat ":" d))) :status))))))
     " ")))

(defun financial-chart-summarize-verb (chart &optional since)
  "`financial-chart-summarize' of CHART in the chart/v1 envelope with its cost."
  (let ((start (float-time)))
    (condition-case err
        (let ((digest (financial-chart-summarize chart :since (or since 20))))
          (list :contract "chart/v1" :ok t :data digest
                :next ["(financial-chart-chart-events CHART)" "(financial-chart-expand CHART)"]
                :cost (list :ms (round (* 1000 (- (float-time) start)))
                            :events (length (plist-get digest :recent_events)))))
      (error (list :contract "chart/v1" :ok :false
                   :reason (or (plist-get (cddr err) :code) (symbol-name (car err)))
                   :message (if (stringp (cadr err)) (cadr err) (error-message-string err))
                   :evidence (list :path (or (plist-get (cddr err) :path) :null)))))))

(provide 'financial-chart-eas-summarize)
;;; financial-chart-eas-summarize.el ends here
