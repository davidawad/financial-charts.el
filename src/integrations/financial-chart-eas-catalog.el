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
;; "id", "levels"}; a pane may itself be one study ({"study": ...} with
;; those keys beside it).  "zones" ({"from", "to", "color", "opacity"})
;; become fills between two levels.  Bad entries signal
;; `financial-chart-invalid-chart' with :code and the JSON :path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'financial-chart-eas-series)
(require 'financial-chart-eas-palette)
(require 'financial-chart-eas-studies)

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

(defun financial-chart-catalog--study (item place bars colours path)
  "The pane fragment of study ITEM in PLACE over BARS; PATH locates ITEM.
COLOURS is (UP . DOWN)."
  (pcase-let* ((`(,entry . ,object) (financial-chart-catalog--entry item path))
               (`(,name ,where ,indicator ,fn . ,_) entry)
               (params (append (financial-chart-series-get object :params) nil))
               (levels (append (financial-chart-series-get object :levels) nil)))
    (unless (eq where place)
      (financial-chart-series-fail path "STUDY_MISPLACED"
                                   (if (eq where 'price)
                                       "%s is a price overlay; put it in \"price\": {\"studies\": [...]}"
                                     "%s draws its own pane; put it in \"panes\": [{\"study\": ...}]")
                                   name))
    (unless (and (<= (length levels) 2) (cl-every #'numberp levels))
      (financial-chart-series-fail (concat path "/levels") "INVALID_STUDY"
                                   "levels is one or two numbers (upper first), got %S" levels))
    (financial-chart-catalog--check-params entry params bars path)
    (append (funcall fn (list :id (or (financial-chart-series-get object :id)
                                      (mapconcat #'financial-chart-series--slug (cons name params) "-"))
                              :indicator indicator :params params :levels levels :bars bars
                              :up (car colours) :down (cdr colours)))
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
         (items (if single (list pane) (append (financial-chart-series-get pane :studies) nil)))
         (fragments (cl-loop for item in items for i from 0
                             collect (financial-chart-catalog--study
                                      item place bars colours
                                      (if single path (format "%s/studies/%d" path i)))))
         (gather (lambda (key) (vconcat (append (mapcan (lambda (f) (copy-sequence (plist-get f key))) fragments)
                                                (append (financial-chart-series-get pane key) nil))))))
    (if (not (or fragments (financial-chart-series-get pane :zones))) pane
      (let ((out (cl-loop for (k v) on pane by #'cddr
                          unless (memq k '(:study :studies :params :levels :zones :series :fills :rules))
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

(defun financial-chart-catalog-describe ()
  "The study catalog as JSON-ready data."
  (vconcat (mapcar (pcase-lambda (`(,name ,place ,indicator ,_ ,doc))
                     (append (list :name name :place (symbol-name place) :doc doc)
                             (when indicator (list :indicator (symbol-name indicator)))))
                   financial-chart-studies)))

(provide 'financial-chart-eas-catalog)
;;; financial-chart-eas-catalog.el ends here
