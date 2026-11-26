;;; financial-chart-detector-api.el --- event/v1, the detector registry and the detect verb -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Layer M2 of docs/design/markups.md (fc-gty.5).  A detector is a pure
;; function of bars and parameters that returns events; drawing is a
;; view of them (`financial-chart-eas-markups').  An event (event/v1)
;; is a plist that encodes as JSON:
;;
;;   (:id "pattern.hammer@2026-05-26" :detector "pattern.hammer"
;;    :params (:trend 10) :index 81 :time "2026-05-26"
;;    :confirmed_index 81 :confirmed_time "2026-05-26"
;;    :direction "bullish" :strength 0.74
;;    :geometry (:at "2026-05-26" :y 111.14)
;;    :evidence (:lower_shadow 1.9 ...))
;;
;; :confirmed_index is the first bar on which the event is knowable:
;; the event depends only on bars at or before it (no look-ahead).
;; :strength is in [0, 1].  :evidence holds the dimensionless predicate
;; values that made the event fire.  :geometry is (:at :y), a level
;; (:y :from :to), (:points [[T Y] ...]) or Fibonacci (:levels
;; [(:ratio :y) ...] :from [T Y] :to [T Y] [:anchor [T Y]]); T is the
;; bar's time (ISO),
;; or its index when the bars have no times.
;;
;; `financial-chart-register-detector' registers one with metadata
;; (:params typed, :lag, :definition with its citation, :oracle,
;; :invariance, :mark, :example); `financial-chart-detect' runs one,
;; `financial-chart-detectors-describe' lists them and
;; `financial-chart-detector-lint' checks every entry carries what the
;; generic harness (fc-gty.6) needs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-core)

(define-error 'financial-chart-invalid-detection
  "financial-chart: invalid detection request" 'financial-chart-error)

(defvar financial-chart-detector-registry nil
  "Registered detectors as (NAME . PLIST), NAME a symbol such as `pattern.hammer'.
PLIST holds :fn and the metadata of `financial-chart-register-detector'.")

(defconst financial-chart-detector-param-types
  '(integer number boolean string choice numbers)
  "Parameter types a detector may declare.")

(defun financial-chart-detection-fail (path code format-string &rest args)
  "Signal `financial-chart-invalid-detection' at PATH with CODE."
  (signal 'financial-chart-invalid-detection
          (list (apply #'format format-string args) :code code :path path)))

(defun financial-chart-register-detector (name fn &rest metadata)
  "Register detector NAME (a symbol) computed by FN with METADATA.
FN is called with the bars (a vector of bar/v1 plists, oldest first) and
the parameters (a plist with every declared parameter, defaults filled)
and returns raw events: plists with :index, :confirmed (the confirming
bar index), :direction (`bullish', `bearish' or `neutral'), :strength,
:geometry (bar indices for times, see `financial-chart-detector--geometry')
and :evidence (a plist of dimensionless numbers or strings).

METADATA keys:
  :kind        pattern, swing, level, fib, trend
  :direction   the direction it signals, or `both'
  :params      ((:name SYM :type TYPE :default D [:min :max :choices :doc]) ...)
               TYPE one of `financial-chart-detector-param-types'
  :lag         confirmation bars after the event bar: an integer, a
               parameter name whose value it is, or `variable'
  :lookback    bars of history before the first possible event
  :needs       bar fields read, e.g. (open high low close)
  :definition  the primary definition it implements, with its citation
  :oracle      \"talib:CDLHAMMER\" when an external reference exists
  :oracle-params  parameters under which it must equal the oracle exactly
  :oracle-sign    non-nil when the oracle's sign is the event's direction
  :deviations  strings: where and why it differs from the oracle
  :invariance  (scale shift): price transforms that leave events unchanged
  :mark        how a view draws it, e.g. (:type marker :below t :label \"H\")
  :example     a chart under the package root that draws it
  :doc         one line"
  (unless (and (symbolp name) (functionp fn) (zerop (% (length metadata) 2)))
    (financial-chart-detection-fail "" "INVALID_DETECTOR"
                                    "register needs a symbol, a function and a keyword plist"))
  (setf (alist-get name financial-chart-detector-registry) (append (list :fn fn) metadata))
  name)

(defun financial-chart-detector (name)
  "Detector NAME's registry plist, or nil.  NAME is a symbol or string."
  (alist-get (if (stringp name) (intern name) name) financial-chart-detector-registry))

(defun financial-chart-detector-names ()
  "Every registered detector's name as a string, sorted."
  (sort (mapcar (lambda (e) (symbol-name (car e))) financial-chart-detector-registry) #'string<))

(defun financial-chart-detector--did-you-mean (name names)
  "The NAMES closest to NAME, at most three."
  (let ((name (format "%s" name)))
    (seq-take (sort (seq-filter (lambda (n) (or (string-search name n) (string-search n name)
                                                (<= (string-distance name n) (max 3 (/ (length name) 3)))))
                                names)
                    (lambda (a b) (< (string-distance name a) (string-distance name b))))
              3)))

(defun financial-chart-detector--entry (name path)
  "Detector NAME's plist; UNKNOWN_DETECTOR at PATH when there is none."
  (or (and (or (symbolp name) (stringp name)) (financial-chart-detector name))
      (let ((close (financial-chart-detector--did-you-mean name (financial-chart-detector-names))))
        (financial-chart-detection-fail path "UNKNOWN_DETECTOR" "Unknown detector %S%s" name
                                        (if close (format "; did you mean %s?" (string-join close ", "))
                                          (format "; detectors: %s"
                                                  (string-join (financial-chart-detector-names) ", ")))))))

;;; Parameters

(defun financial-chart-detector--check-param (spec value path)
  "VALUE checked against parameter SPEC; INVALID_PARAM at PATH."
  (let ((type (plist-get spec :type)) (min (plist-get spec :min)) (max (plist-get spec :max))
        (fail (lambda (what) (financial-chart-detection-fail path "INVALID_PARAM" "%s is %s, got %S"
                                                             (plist-get spec :name) what value))))
    (pcase type
      ('integer (unless (integerp value) (funcall fail "an integer")))
      ('number (unless (numberp value) (funcall fail "a number")))
      ('boolean (unless (memq value '(t nil :false)) (funcall fail "true or false")))
      ('string (unless (stringp value) (funcall fail "a string")))
      ('choice (unless (member value (plist-get spec :choices))
                 (funcall fail (format "one of %s" (string-join (plist-get spec :choices) ", ")))))
      ('numbers (unless (and (or (vectorp value) (listp value)) (seq-every-p #'numberp value))
                  (funcall fail "an array of numbers"))))
    (when (and (numberp value) min (< value min)) (funcall fail (format "at least %s" min)))
    (when (and (numberp value) max (> value max)) (funcall fail (format "at most %s" max)))
    (if (eq value :false) nil value)))

(defun financial-chart-detector-params (name params &optional path)
  "PARAMS (a plist or parsed JSON object, keyword keys) for detector NAME,
checked and with every declared parameter's default filled, in
declaration order.  PATH locates PARAMS for errors (default /params)."
  (let* ((path (or path "/params"))
         (entry (financial-chart-detector--entry name "/detector"))
         (specs (plist-get entry :params))
         (known (mapcar (lambda (s) (symbol-name (plist-get s :name))) specs)))
    (cl-loop for (k _) on params by #'cddr
             for key = (substring (symbol-name k) 1)
             unless (member key known)
             do (let ((close (financial-chart-detector--did-you-mean key known)))
                  (financial-chart-detection-fail (format "%s/%s" path key) "INVALID_PARAM"
                                                  "%s takes no parameter %S%s" name key
                                                  (if close (format "; did you mean %s?" (string-join close ", "))
                                                    (format "; parameters: %s"
                                                            (if known (string-join known ", ") "none"))))))
    (cl-loop for spec in specs
             for key = (intern (concat ":" (symbol-name (plist-get spec :name))))
             for given = (plist-member params key)
             for value = (if given (cadr given) (plist-get spec :default))
             append (list key (if (and given (not (eq value :null)))
                                  (financial-chart-detector--check-param
                                   spec value (format "%s/%s" path (plist-get spec :name)))
                                (if (eq value :null) nil value))))))

;;; Times and geometry

(defun financial-chart-detector--time (bars i)
  "Bar I of BARS as a time for events: its ISO time, else I."
  (let ((time (plist-get (aref bars i) :time)))
    (cond ((stringp time) time)
          ((numberp time)
           (let ((secs (floor time 1000)))
             (if (zerop (% time 86400000))
                 (format-time-string "%F" secs t)
               (format-time-string "%FT%TZ" secs t))))
          (t i))))

(defun financial-chart-detector--point (bars point)
  "POINT, (INDEX . Y), as [TIME Y]."
  (vector (financial-chart-detector--time bars (car point)) (cdr point)))

(defun financial-chart-detector--geometry (bars g)
  "Raw geometry G (bar indices) as event/v1 geometry over BARS."
  (cond ((plist-member g :at)
         (list :at (financial-chart-detector--time bars (plist-get g :at)) :y (plist-get g :y)))
        ((plist-member g :levels)
         (append (list :levels (vconcat (mapcar (lambda (l) (list :ratio (car l) :y (cdr l))) (plist-get g :levels)))
                       :from (financial-chart-detector--point bars (plist-get g :from))
                       :to (financial-chart-detector--point bars (plist-get g :to)))
                 (when (plist-get g :anchor)
                   (list :anchor (financial-chart-detector--point bars (plist-get g :anchor))))))
        ((plist-member g :points)
         (list :points (vconcat (mapcar (lambda (p) (financial-chart-detector--point bars p))
                                        (plist-get g :points)))))
        (t (list :y (plist-get g :y)
                 :from (financial-chart-detector--time bars (plist-get g :from))
                 :to (financial-chart-detector--time bars (plist-get g :to))))))

(defun financial-chart-detector--finish (name params bars raw)
  "RAW events of detector NAME with PARAMS over BARS as event/v1 plists.
Ids are NAME@TIME, with #2, #3 ... for a second event on one bar."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar
     (lambda (e)
       (let* ((i (plist-get e :index)) (c (plist-get e :confirmed))
              (base (format "%s@%s" name (financial-chart-detector--time bars i)))
              (k (cl-incf (gethash base seen 0))))
         (list :id (if (> k 1) (format "%s#%d" base k) base)
               :detector (symbol-name name)
               :params (cl-loop for (k v) on params by #'cddr
                                append (list k (cond ((null v) :null) ((listp v) (vconcat v)) (t v))))
               :index i :time (financial-chart-detector--time bars i)
               :confirmed_index c :confirmed_time (financial-chart-detector--time bars c)
               :direction (symbol-name (plist-get e :direction))
               :strength (float (max 0 (min 1 (plist-get e :strength))))
               :geometry (financial-chart-detector--geometry bars (plist-get e :geometry))
               :evidence (plist-get e :evidence))))
     (sort (copy-sequence raw)
           (lambda (a b) (or (< (plist-get a :confirmed) (plist-get b :confirmed))
                             (and (= (plist-get a :confirmed) (plist-get b :confirmed))
                                  (< (plist-get a :index) (plist-get b :index)))))))))

;;; Bars

(defun financial-chart-detector--bars (bars needs)
  "BARS (a list or vector of bar/v1 plists) as a vector.
INVALID_BAR when a bar lacks a number for a field of NEEDS (symbols,
default open high low close)."
  (let ((v (vconcat bars))
        (keys (mapcar (lambda (f) (intern (concat ":" (symbol-name f)))) (or needs '(open high low close)))))
    (seq-do-indexed
     (lambda (b i)
       (dolist (k keys)
         (unless (numberp (plist-get b k))
           (financial-chart-detection-fail (format "/bars/%d/%s" i (substring (symbol-name k) 1))
                                           "INVALID_BAR" "Bar %d needs a number %s" i
                                           (substring (symbol-name k) 1)))))
     v)
    v))

;;; Bars identity

(defun financial-chart-bars-id (bars)
  "A content hash of BARS: the first 16 hex digits of the SHA-256 of their
canonical rows (time, open, high, low, close, volume), so the same bars
have the same id however they were read."
  (substring
   (secure-hash 'sha256
                (mapconcat (lambda (b)
                             (mapconcat (lambda (k) (format "%S" (plist-get b k)))
                                        '(:time :open :high :low :close :volume) ","))
                           (append bars nil) "\n"))
   0 16))

;;; The verb

(cl-defun financial-chart-detect (name bars &optional params &key since limit)
  "Events of detector NAME over BARS with PARAMS, oldest confirmation first.
NAME is a symbol or string; BARS a list or vector of bar/v1 plists;
PARAMS a plist (or parsed JSON object) of the detector's parameters.
SINCE keeps events confirmed in the last SINCE bars (an integer) or at
or after a time (a string); LIMIT keeps the last LIMIT.  Signal
`financial-chart-invalid-detection' with :code and :path."
  (let* ((entry (financial-chart-detector--entry name "/detector"))
         (name (if (stringp name) (intern name) name))
         (params (financial-chart-detector-params name params))
         (v (financial-chart-detector--bars bars (plist-get entry :needs)))
         (events (if (zerop (length v)) nil
                   (financial-chart-detector--finish name params v (funcall (plist-get entry :fn) v params))))
         (n (length v)))
    (when since
      (setq events (seq-filter (lambda (e)
                                 (if (integerp since) (>= (plist-get e :confirmed_index) (- n since))
                                   (not (string< (format "%s" (plist-get e :confirmed_time)) since))))
                               events)))
    (when (and limit (> (length events) limit))
      (setq events (last events limit)))
    events))

(defun financial-chart-detect-verb (name bars &optional params since limit)
  "`financial-chart-detect' answered in the chart/v1 envelope.
Return (:contract \"chart/v1\" :ok t :data (:events [...]) :next [...]
:cost (:ms :bars :events)), or :ok :false with :reason and :evidence."
  (let ((start (float-time)))
    (condition-case err
        (let ((events (financial-chart-detect name bars params :since since :limit limit)))
          (list :contract "chart/v1" :ok t
                :data (list :detector (format "%s" name) :events (vconcat events))
                :next (vector (format "(financial-chart-detector-describe '%s)" name))
                :cost (list :ms (round (* 1000 (- (float-time) start))) :bars (length bars)
                            :events (length events))))
      (financial-chart-invalid-detection
       (list :contract "chart/v1" :ok :false :reason (plist-get (cddr err) :code)
             :message (cadr err) :evidence (list :path (plist-get (cddr err) :path))
             :next (vector "(financial-chart-detector-names)"))))))

;;; Describe and lint

(defun financial-chart-detector-describe (name)
  "Detector NAME as JSON-ready data: its metadata without the function."
  (let ((entry (financial-chart-detector--entry name "/detector")))
    (append (list :name (format "%s" name))
            (cl-loop for (k v) on entry by #'cddr
                     unless (eq k :fn)
                     append (list k (pcase k
                                      (:params (vconcat (mapcar (lambda (s)
                                                                  (cl-loop for (pk pv) on s by #'cddr
                                                                           append (list pk (if (symbolp pv) (format "%s" pv)
                                                                                             (if (listp pv) (vconcat pv) pv)))))
                                                                v)))
                                      ((or :needs :invariance :deviations :oracle-params)
                                       (if (and (listp v) (keywordp (car v))) v (vconcat (mapcar (lambda (x) (format "%s" x)) v))))
                                      (_ (if (and v (symbolp v) (not (keywordp v))) (symbol-name v) v))))))))

(defun financial-chart-detectors-describe ()
  "Every detector as JSON-ready data, by name."
  (vconcat (mapcar #'financial-chart-detector-describe (financial-chart-detector-names))))

(defun financial-chart-detector-lint (&optional root)
  "Problems with the registry as (NAME . MESSAGE), empty when clean.
Every entry needs :kind, a :definition citing its source (a year or
TradingView), a :lag, typed :params, an explicit :invariance and an
:example file under ROOT (default `financial-chart-root')."
  (let ((root (or root financial-chart-root)))
    (cl-loop
     for (name . e) in financial-chart-detector-registry
     append
     (delq nil
           (list
            (unless (plist-get e :kind) (cons name "no :kind"))
            (unless (and (stringp (plist-get e :definition))
                         (string-match-p "\\(?:(?[12][0-9]\\{3\\}\\)\\|TradingView\\|Pine" (plist-get e :definition)))
              (cons name ":definition does not cite a source (a year or TradingView)"))
            (let ((lag (plist-get e :lag)))
              (unless (or (integerp lag) (eq lag 'variable)
                          (and (symbolp lag) (cl-find lag (plist-get e :params) :key (lambda (s) (plist-get s :name)))))
                (cons name ":lag is not an integer, a parameter or variable")))
            (cl-loop for s in (plist-get e :params)
                     unless (memq (plist-get s :type) financial-chart-detector-param-types)
                     return (cons name (format "parameter %s has no valid :type" (plist-get s :name))))
            (unless (and (plist-member e :invariance)
                         (cl-subsetp (plist-get e :invariance) '(scale shift)))
              (cons name ":invariance is not a subset of (scale shift)"))
            (unless (and (stringp (plist-get e :example))
                         (file-readable-p (expand-file-name (plist-get e :example) root)))
              (cons name (format ":example %S is not a readable file" (plist-get e :example)))))))))

(provide 'financial-chart-detector-api)
;;; financial-chart-detector-api.el ends here
