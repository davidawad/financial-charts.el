;;; financial-chart-eas-catalog.el --- studies and zones expanded into the composition DSL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `financial-chart-catalog-expand' rewrites a chart description's
;; shorthands into the plain composition DSL before it compiles
;; (fc-gbo.3):
;;
;;   "price": {"studies": ["ichimoku", {"study": "bollinger", "params": [20, 2]}]}
;;   "panes": [{"study": "rsi", "levels": [80, 20]},
;;             {"studies": ["macd"], "height": 90},
;;             {"series": ["cci"], "zones": [{"from": -100, "to": 100}]}]
;;
;; A study (`financial-chart-studies') becomes series, fills, rules, a
;; title and a domain merged into its pane; the pane's own keys stay
;; and come after.  A study entry is a name or {"study", "params",
;; "id", "levels", "values"}; a pane may itself be one study ({"study":
;; ...} with those keys beside it).  "values" draws the caller's own
;; series in the study's dress instead of computing them.  "zones" ({"from", "to", "color", "opacity"})
;; become fills between two levels.  Bad entries signal
;; `financial-chart-invalid-chart' with :code and the JSON :path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-palette)
(require 'financial-chart-eas-studies)
(require 'eas)

(defconst financial-chart-catalog-zone-color "#90a4ae"
  "Default colour of a zone.")

(defun financial-chart-catalog--names (place)
  "Names of the studies that go in PLACE, comma separated."
  (mapconcat #'car (seq-filter (lambda (s) (eq (nth 1 s) place)) financial-chart-studies) ", "))

(defun financial-chart-catalog--entry (item path)
  "Study ITEM (a name or object) at PATH as (ENTRY . OBJECT)."
  (let* ((object (cond ((stringp item) (list :study item))
                       ((and (listp item) (keywordp (car item))) item)
                       (t (financial-chart-series-fail path "INVALID_STUDY"
                                                       "A study is a name or {\"study\", \"params\", \"id\", \"levels\"}, got %S"
                                                       item))))
         (name (financial-chart-series-get object :study))
         (entry (assoc name financial-chart-studies)))
    (unless entry
      (financial-chart-series-fail (if (stringp item) path (concat path "/study")) "UNKNOWN_STUDY"
                                   "Unknown study %S; overlays (price \"studies\"): %s; panes: %s" name
                                   (financial-chart-catalog--names 'price) (financial-chart-catalog--names 'pane)))
    (cons entry object)))

(defun financial-chart-catalog--check-params (entry params bars path)
  "Signal at PATH unless study ENTRY computes with PARAMS over BARS."
  (let ((indicator (nth 2 entry)))
    (if (equal (car entry) "ma-ribbon")
        (let ((kind (if (stringp (car params)) (car params) "ema")))
          (unless (assq (intern kind) financial-chart-indicator-registry)
            (financial-chart-series-fail (concat path "/params/0") "UNKNOWN_INDICATOR"
                                         "Ribbon average %S is not an indicator; try sma, ema, wma, hma" kind))
          (seq-do-indexed (lambda (p i)
                            (unless (and (integerp p) (> p 0))
                              (financial-chart-series-fail (format "%s/params/%d" path i) "INDICATOR_FAILED"
                                                           "A ribbon period must be a positive integer, got %S" p)))
                          (if (stringp (car params)) (cdr params) params)))
      (when indicator
        (condition-case err
            (apply #'financial-chart-indicator-evaluate indicator bars params)
          (error (financial-chart-series-fail (concat path "/params") "INDICATOR_FAILED"
                                              "Study %s%S failed: %s" (car entry) params
                                              (error-message-string err))))))))

;; A study draws the caller's numbers instead of computing them when
;; given "values": {PART: [one value or null per bar], ...}; PART is what
;; follows the study id in a series id ("upper" of bollinger.upper), or
;; "value" for a one-line study such as rsi.
(defun financial-chart-catalog--supplied (fragment values id bars path)
  "FRAGMENT with its series replaced by VALUES, the study's \"values\".
ID is the study id; BARS the chart's bars; PATH locates the study."
  (let* ((part (lambda (s) (let ((sid (plist-get s :id)))
                             (if (equal sid id) "value" (string-remove-prefix (concat id ".") sid)))))
         (parts (mapcar part (plist-get fragment :series))))
    (cl-loop for (key vs) on values by #'cddr
             for name = (substring (symbol-name key) 1)
             for at = (format "%s/values/%s" path name)
             do (unless (member name parts)
                  (financial-chart-series-fail at "INVALID_STUDY" "This study has no series %S; its parts: %s"
                                               name (string-join parts ", ")))
             do (unless (and (or (vectorp vs) (consp vs)) (= (length vs) (length bars)))
                  (financial-chart-series-fail at "LENGTH_MISMATCH"
                                               "%s has %s values for %d bars; give one per bar (null for none)"
                                               name (if (sequencep vs) (length vs) "no") (length bars))))
    (plist-put (copy-sequence fragment) :series
               (mapcar (lambda (s)
                         (let ((vs (plist-get values (intern (concat ":" (funcall part s))))))
                           (if (not vs) s
                             (append (list :values vs :label (format "%s %s" id (funcall part s)))
                                     (cl-loop for (k v) on s by #'cddr
                                              unless (memq k '(:indicator :params :output)) append (list k v))))))
                       (plist-get fragment :series)))))

(defun financial-chart-catalog--array (object key path)
  "OBJECT's KEY as a list; signal INVALID_STUDY at PATH unless an array."
  (let ((value (financial-chart-series-get object key)))
    (unless (or (vectorp value) (and (listp value) (not (keywordp (car value)))))
      (financial-chart-series-fail (format "%s/%s" path (substring (symbol-name key) 1)) "INVALID_STUDY"
                                   "%s is an array, got %S" (substring (symbol-name key) 1) value))
    (append value nil)))

(defun financial-chart-catalog--study (item place bars colours path)
  "The pane fragment of study ITEM in PLACE over BARS; PATH locates ITEM.
COLOURS is (UP . DOWN)."
  (pcase-let* ((`(,entry . ,object) (financial-chart-catalog--entry item path))
               (`(,name ,where ,indicator ,fn . ,_) entry)
               (params (financial-chart-catalog--array object :params path))
               (levels (financial-chart-catalog--array object :levels path))
               (values (financial-chart-series-get object :values))
               (id (or (financial-chart-series-get object :id)
                       (mapconcat #'financial-chart-series--slug (cons name params) "-"))))
    (unless (stringp id)
      (financial-chart-series-fail (concat path "/id") "INVALID_STUDY" "id is a string, got %S" id))
    (unless (eq where place)
      (financial-chart-series-fail path "STUDY_MISPLACED"
                                   (if (eq where 'price)
                                       "%s is a price overlay; put it in \"price\": {\"studies\": [...]}"
                                     "%s draws its own pane; put it in \"panes\": [{\"study\": ...}]")
                                   name))
    (let ((count (if (equal name "adx") 1 2)))
      (unless (and (memq (length levels) (list 0 count)) (cl-every #'numberp levels))
        (financial-chart-series-fail (concat path "/levels") "INVALID_STUDY"
                                     "%s levels are %s, got %S" name
                                     (if (= count 1) "[one number]" "[upper, lower]") levels)))
    (financial-chart-catalog--check-params entry params bars path)
    (unless (or (null values) (and (listp values) (keywordp (car values))))
      (financial-chart-series-fail (concat path "/values") "INVALID_STUDY"
                                   "values is an object {PART: [one value or null per bar]}, got %S" values))
    (append (financial-chart-catalog--supplied
             (condition-case err
                 (funcall fn (list :id id :indicator indicator :params params :levels levels :bars bars
                                   :up (car colours) :down (cdr colours)))
               ((financial-chart-invalid-chart) (signal (car err) (cdr err)))
               (error (financial-chart-series-fail (concat path "/params") "INDICATOR_FAILED"
                                                   "Study %s%S failed: %s" name params
                                                   (error-message-string err))))
             values id bars path)
            (list :title (concat (or (plist-get (cdr (assq indicator financial-chart-indicator-registry)) :label)
                                     name)
                                 (if params (concat " " (mapconcat (lambda (p) (format "%s" p)) params ","))
                                   ""))))))

(defun financial-chart-catalog--zones (pane path)
  "Fills for PANE's \"zones\"; PATH locates PANE."
  (cl-loop for zone in (append (financial-chart-series-get pane :zones) nil) for i from 0
           for at = (format "%s/zones/%d" path i)
           collect (let ((from (financial-chart-series-get zone :from))
                         (to (financial-chart-series-get zone :to)))
                     (unless (and (numberp from) (numberp to))
                       (financial-chart-series-fail at "INVALID_ZONE"
                                                    "A zone is {\"from\": LEVEL, \"to\": LEVEL, \"color\", \"opacity\"}, got %S"
                                                    zone))
                     (list :between (vector from to)
                           :color (or (financial-chart-series-get zone :color) financial-chart-catalog-zone-color)
                           :opacity (or (financial-chart-series-get zone :opacity) 0.1)))))

(defun financial-chart-catalog--pane (pane place bars colours path)
  "PANE (at PATH, in PLACE) with its studies and zones expanded over BARS."
  (let* ((single (and (eq place 'pane) (financial-chart-series-get pane :study)))
         (items (if single (list pane)
                  (let ((studies (financial-chart-series-get pane :studies)))
                    (unless (or (vectorp studies) (null studies))
                      (financial-chart-series-fail (concat path "/studies") "INVALID_STUDY"
                                                   "studies is an array of study names or objects, got %S"
                                                   studies))
                    (append studies nil))))
         (fragments (cl-loop for item in items for i from 0
                             collect (financial-chart-catalog--study
                                      item place bars colours
                                      (if single path (format "%s/studies/%d" path i)))))
         (gather (lambda (key) (vconcat (append (mapcan (lambda (f) (copy-sequence (plist-get f key))) fragments)
                                                (append (financial-chart-series-get pane key) nil))))))
    (if (not (or fragments (financial-chart-series-get pane :zones))) pane
      (let ((out (cl-loop for (k v) on pane by #'cddr
                          unless (memq k '(:study :studies :params :levels :values :zones :series :fills :rules))
                          append (list k v))))
        (append (list :series (funcall gather :series)
                      :fills (vconcat (financial-chart-catalog--zones pane path) (funcall gather :fills))
                      :rules (funcall gather :rules))
                (when (and (eq place 'pane) (not (plist-member out :title)) (= (length fragments) 1))
                  (list :title (plist-get (car fragments) :title)))
                (when (and (cl-some (lambda (f) (plist-get f :volume)) fragments)
                           (not (plist-member out :volume)))
                  (list :volume t))
                out)))))

(defun financial-chart-catalog-expand (chart bars)
  "CHART with every study and zone written out in the composition DSL.
BARS are its validated bars (some studies compute from them)."
  (let* ((colors (financial-chart-series-get chart :colors))
         (colours (cons (or (financial-chart-series-get colors :up) financial-chart-palette-up)
                        (or (financial-chart-series-get colors :down) financial-chart-palette-down)))
         (price (financial-chart-series-get chart :price))
         (panes (financial-chart-series-get chart :panes))
         (chart (copy-sequence chart)))
    (when price
      (setq chart (plist-put chart :price (financial-chart-catalog--pane price 'price bars colours "/price"))))
    (when (and panes (or (vectorp panes) (consp panes)))
      (setq chart (plist-put chart :panes
                             (vconcat (seq-map-indexed
                                       (lambda (pane i)
                                         (if (and (listp pane) (keywordp (car pane)))
                                             (financial-chart-catalog--pane pane 'pane bars colours
                                                                            (format "/panes/%d" i))
                                           pane))
                                       panes)))))
    chart))

(defconst financial-chart-catalog-examples-directory
  (expand-file-name "../../examples/indicators"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Example charts, one per study and kind of annotation (NAME.json).")

(defun financial-chart-catalog-examples ()
  "Names of the catalog's example charts."
  (mapcar #'file-name-sans-extension
          (directory-files financial-chart-catalog-examples-directory nil "\\.json\\'")))

(defun financial-chart-catalog-example (name)
  "The example chart NAME (a study, markers, annotations or fibonacci), parsed."
  (let ((file (expand-file-name (format "%s.json" name) financial-chart-catalog-examples-directory)))
    (unless (file-readable-p file)
      (financial-chart-series-fail "" "NOT_FOUND" "No indicator example %s; examples: %s" name
                                   (string-join (financial-chart-catalog-examples) ", ")))
    (eas-json-read-file file)))

(defun financial-chart-catalog-describe ()
  "The study catalog as JSON-ready data."
  (vconcat (mapcar (pcase-lambda (`(,name ,place ,indicator ,_ ,doc))
                     (append (list :name name :place (symbol-name place) :doc doc)
                             (when indicator (list :indicator (symbol-name indicator)))))
                   financial-chart-studies)))

(provide 'financial-chart-eas-catalog)
;;; financial-chart-eas-catalog.el ends here
