;;; financial-chart-eas-series.el --- series, fills and colours of a composed chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The middle of `financial-chart-compose' (fc-gbo.2): turn each pane's
;; "series" entries into value vectors aligned with the bars, and give
;; every series an id, a label, a column and a colour.  A series entry
;; is one of
;;
;;   "sma"                                an indicator by name
;;   {"indicator": "sma", "params": [20]} an indicator with parameters;
;;                                        "output" keeps one output of a
;;                                        multi-output indicator, else
;;                                        each output is its own series
;;   {"values": [...], "label": "model"}  a precomputed series, one value
;;                                        (or null) per bar
;;   {"field": "close"}                   a column of the bars
;;
;; plus "id", "label", "color", "width", "dash", "style" (line, step,
;; histogram, area, dots), "shift" (bars later, or earlier when
;; negative) and, for histograms, "above"/"below".
;; Indicator math runs on the supplied bars through
;; `financial-chart-indicator-evaluate'; nothing is fetched.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-indicator-api)
(require 'financial-chart-eas-palette)
(require 'financial-chart-eas-styles)

(defconst financial-chart-series-styles '("line" "step" "histogram" "area" "dots")
  "How a series of a composed chart may be drawn.")

(defconst financial-chart-series-bar-fields '("open" "high" "low" "close" "volume")
  "Bar columns a series or a fill may name.")

(defconst financial-chart-series-max-shift 500
  "Most bars a series may be shifted (\"shift\") either way.")

(defun financial-chart-series-get (object key)
  "KEY of parsed JSON OBJECT, with JSON null and false as nil."
  (let ((value (and (listp object) (plist-get object key))))
    (if (memq value '(:null :false)) nil value)))

(defun financial-chart-series-fail (path code format-string &rest args)
  "Signal `financial-chart-invalid-chart' at PATH with CODE.
The message is FORMAT-STRING applied to ARGS."
  (signal 'financial-chart-invalid-chart
          (list (apply #'format format-string args) :code code :path path)))

(defun financial-chart-series--number-list (seq)
  "SEQ (a list or vector) as a list of numbers and nils."
  (mapcar (lambda (v) (if (numberp v) v nil)) (append seq nil)))

(defun financial-chart-series--slug (value)
  "VALUE (a number or string) as an id fragment."
  (replace-regexp-in-string "[^[:alnum:].]+" "-" (format "%s" value)))

;;; Resolving one entry

(defun financial-chart-series--indicator (item bars path)
  "Series of indicator ITEM evaluated on BARS; PATH locates ITEM."
  (let* ((name (format "%s" (or (financial-chart-series-get item :indicator)
                                (financial-chart-series-get item :name))))
         (symbol (intern name))
         (params (append (financial-chart-series-get item :params) nil))
         (entry (assq symbol financial-chart-indicator-registry))
         (wanted (financial-chart-series-get item :output))
         (outputs
          (progn
            (unless entry
              (financial-chart-series-fail
               (concat path "/indicator") "UNKNOWN_INDICATOR"
               "Unknown indicator %s; indicators: %s" name
               (mapconcat (lambda (e) (symbol-name (car e)))
                          (reverse financial-chart-indicator-registry) ", ")))
            (condition-case err
                (let ((r (apply #'financial-chart-indicator-evaluate symbol bars params)))
                  (if (keywordp (car r)) (list r) r))
              (error
               (financial-chart-series-fail (concat path "/params") "INDICATOR_FAILED"
                                            "Indicator %s%S failed: %s" name params
                                            (error-message-string err))))))
         (outputs (if (not wanted) outputs
                    (or (cl-remove-if-not (lambda (o) (equal (format "%s" (plist-get o :name)) wanted))
                                          outputs)
                        (financial-chart-series-fail
                         (concat path "/output") "UNKNOWN_OUTPUT"
                         "Indicator %s has no output %s; outputs: %s" name wanted
                         (mapconcat (lambda (o) (format "%s" (plist-get o :name))) outputs ", ")))))
         (suffix (if params (concat " " (mapconcat (lambda (p) (format "%s" p)) params ",")) ""))
         (base-id (or (financial-chart-series-get item :id)
                      (mapconcat #'financial-chart-series--slug (cons name params) "-")))
         (base-label (financial-chart-series-get item :label))
         (several (or (cdr outputs) (and wanted (not (financial-chart-series-get item :id))))))
    (mapcar
     (lambda (out)
       (let ((output (format "%s" (plist-get out :name))))
         (list :id (if several (concat base-id "." output) base-id)
               :label (cond ((and base-label (cdr outputs)) (concat base-label " " output))
                            (base-label)
                            (t (concat (plist-get out :label) suffix)))
               :key (list name params output)
               :palette (if (assoc output financial-chart-palette-homes) output name)
               :bounds (plist-get out :bounds)
               :shift (plist-get out :shift)
               :default-style (if (string-suffix-p "histogram" output) "histogram"
                                (if (eq symbol 'parabolic-sar) "dots" "line"))
               :values (vconcat (financial-chart-series--number-list (plist-get out :values))))))
     outputs)))

(defun financial-chart-series--values (item bars path)
  "The precomputed series of ITEM, one value per bar of BARS; PATH locates it."
  (let ((values (financial-chart-series-get item :values))
        (label (or (financial-chart-series-get item :label)
                   (financial-chart-series-get item :id) "values")))
    (unless (= (length values) (length bars))
      (financial-chart-series-fail (concat path "/values") "LENGTH_MISMATCH"
                                   "values has %d entries for %d bars; give one per bar (null for none)"
                                   (length values) (length bars)))
    (seq-do-indexed (lambda (v i)
                      (unless (or (numberp v) (memq v '(nil :null)))
                        (financial-chart-series-fail (format "%s/values/%d" path i) "NOT_A_NUMBER"
                                                     "values[%d] is %S; give a number or null" i v)))
                    values)
    (list (list :id (or (financial-chart-series-get item :id) (financial-chart-series--slug label))
                :label label :key (list "values" label) :palette label :default-style "line"
                :values (vconcat (financial-chart-series--number-list values))))))

(defun financial-chart-series--field (item bars path)
  "The bar column series of ITEM over BARS; PATH locates it."
  (let ((field (financial-chart-series-get item :field)))
    (unless (member field financial-chart-series-bar-fields)
      (financial-chart-series-fail (concat path "/field") "UNKNOWN_FIELD"
                                   "field %S is not a bar column; columns: %s" field
                                   (string-join financial-chart-series-bar-fields ", ")))
    (list (list :id (or (financial-chart-series-get item :id) field)
                :label (or (financial-chart-series-get item :label) field)
                :key (list "field" field) :palette field :default-style "line"
                :values (vconcat (mapcar (lambda (b) (let ((v (plist-get b (intern (concat ":" field)))))
                                                       (and (numberp v) v)))
                                         bars))))))

(defun financial-chart-series-resolve (item bars path)
  "Series plists of series entry ITEM over BARS; PATH locates ITEM.
Each carries :id :label :key :palette :values and ITEM's styling
:style :color :width :dash :above :below, and :shift (bars, 0 for none)."
  (let* ((item (if (stringp item) (list :indicator item) item))
         (series (cond ((not (and (listp item) (keywordp (car item))))
                        (financial-chart-series-fail path "INVALID_SERIES"
                                                     "A series is an indicator name or an object, got %S" item))
                       ((or (financial-chart-series-get item :indicator)
                            (financial-chart-series-get item :name))
                        (financial-chart-series--indicator item bars path))
                       ((plist-member item :values) (financial-chart-series--values item bars path))
                       ((financial-chart-series-get item :field) (financial-chart-series--field item bars path))
                       (t (financial-chart-series-fail
                           path "INVALID_SERIES"
                           "A series needs \"indicator\", \"values\" or \"field\"; got keys %S"
                           (cl-loop for (k _) on item by #'cddr collect k)))))
         (style (financial-chart-series-get item :style))
         (shift (financial-chart-series-get item :shift)))
    (unless (or (null shift) (and (integerp shift) (<= (abs shift) financial-chart-series-max-shift)))
      (financial-chart-series-fail (concat path "/shift") "INVALID_SHIFT"
                                   "shift %S; give a whole number of bars within +/-%d (positive draws later)"
                                   shift financial-chart-series-max-shift))
    (when (and style (not (member style financial-chart-series-styles)))
      (financial-chart-series-fail (concat path "/style") "UNKNOWN_STYLE"
                                   "Series style %S; styles: %s" style
                                   (string-join financial-chart-series-styles ", ")))
    (mapcar (lambda (s)
              (let ((shift (or shift (plist-get s :shift) 0)))
                (unless (zerop shift)
                  (setq s (plist-put (copy-sequence s) :key (append (plist-get s :key) (list :shift shift)))))
                (setq s (plist-put (copy-sequence s) :shift shift)))
              (append (list :style (or style (plist-get s :default-style))
                            :color (financial-chart-series-get item :color)
                            :width (financial-chart-series-get item :width)
                            :dash (financial-chart-series-get item :dash)
                            :above (financial-chart-series-get item :above)
                            :below (financial-chart-series-get item :below)
                            :path path)
                      s))
            series)))

;;; The whole chart's series

(defun financial-chart-series-finish (series)
  "Give each of SERIES (in chart order) a :column, unique :label and :color.
Series with equal :key share a column and a colour (an indicator in two
panes); distinct series get distinct labels and, unless given a :color,
palette colours by `financial-chart-palette-assign'."
  (let* ((colours (financial-chart-palette-assign
                   (cl-loop for s in series unless (plist-get s :color)
                            collect (cons (plist-get s :key) (plist-get s :palette)))))
         (columns nil) (labels nil))
    (mapcar
     (lambda (s)
       (let* ((key (plist-get s :key))
              (column (or (cdr (assoc key columns))
                          (let ((c (format "s%d" (length columns))))
                            (push (cons key c) columns) c)))
              (label (plist-get s :label))
              (owner (cdr (assoc label labels)))
              (label (if (or (null owner) (equal owner key)) label
                       (cl-loop for n from 2
                                for l = (format "%s (%d)" label n)
                                unless (assoc l labels) return l))))
         (unless (assoc label labels) (push (cons label key) labels))
         (append (list :column column :label label
                       :color (or (plist-get s :color) (cdr (assoc key colours))))
                 s)))
     series)))

(defun financial-chart-series-ref (ref series bars path)
  "Values of fill end REF: a number, a series id or label, or a bar field.
SERIES are the chart's finished series, BARS its bars; PATH locates REF."
  (cond ((numberp ref) (make-vector (length bars) ref))
        ((and (stringp ref)
              (cl-find-if (lambda (s) (or (equal (plist-get s :id) ref) (equal (plist-get s :label) ref)))
                          series))
         (plist-get (cl-find-if (lambda (s) (or (equal (plist-get s :id) ref)
                                                (equal (plist-get s :label) ref)))
                                series)
                    :values))
        ((member ref financial-chart-series-bar-fields)
         (plist-get (car (financial-chart-series--field (list :field ref) bars path)) :values))
        (t (financial-chart-series-fail
            path "UNKNOWN_SERIES" "Fill end %S is not a number, bar field or series; series ids: %s"
            ref (mapconcat (lambda (s) (plist-get s :id)) series ", ")))))

(defun financial-chart-series-fill (fill series bars path)
  "FILL (a \"fills\" entry) as (:a VALUES :b VALUES . STYLE).
SERIES and BARS resolve its \"between\" ends; PATH locates FILL."
  (let ((between (append (financial-chart-series-get fill :between) nil)))
    (unless (= (length between) 2)
      (financial-chart-series-fail (concat path "/between") "INVALID_FILL"
                                   "A fill needs \"between\": [A, B], two series ids, fields or numbers"))
    (list :a (financial-chart-series-ref (car between) series bars (concat path "/between/0"))
          :b (financial-chart-series-ref (cadr between) series bars (concat path "/between/1"))
          :color (financial-chart-series-get fill :color)
          :above (financial-chart-series-get fill :above)
          :below (financial-chart-series-get fill :below)
          :opacity (financial-chart-series-get fill :opacity))))

(provide 'financial-chart-eas-series)
;;; financial-chart-eas-series.el ends here
