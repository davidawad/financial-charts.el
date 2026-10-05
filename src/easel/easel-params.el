;;; easel-params.el --- Vega-Lite selection params: stores and tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  Interactions are Vega-Lite params, not an invented API.
;; A selection's value in view state is a plain store:
;;
;;   point     (:type "point" :fields ["t"] :values [["2026-03-02"] ...])
;;   interval  (:type "interval" :fields (:x "t") :x [LO HI] [:y [LO HI]])
;;
;; nil is the empty selection.  `easel-params-contains' decides
;; membership the way Vega-Lite does; `easel-params-with-state' installs
;; it as the compile and filter hooks so conditional encodings and
;; {"param": ...} filters follow the state.  Point selections without
;; fields or encodings key on the row identity (:_easel_row), like
;; Vega-Lite's _vgsid_.

;;; Code:

(require 'easel-core)
(require 'easel-time)
(require 'easel-encode)
(require 'easel-transform)
(require 'easel-params-index)

(defun easel-params-normalize (param)
  "Return PARAM's selection definition with Vega-Lite defaults filled."
  (let* ((select (plist-get param :select))
         (select (if (stringp select) (list :type select) select))
         (type (plist-get select :type)))
    (when select
      (append (list :type type
                    :on (or (plist-get select :on) (if (equal type "point") "click" "drag"))
                    :encodings (or (plist-get select :encodings) (if (equal type "interval") ["x" "y"]))
                    :fields (plist-get select :fields)
                    :nearest (eq (plist-get select :nearest) t)
                    :toggle (if (plist-member select :toggle) (plist-get select :toggle) t)
                    :clear (if (plist-member select :clear) (plist-get select :clear) "dblclick"))))))

(defun easel-params-of (scene &optional type)
  "SCENE's selection params (plists with :name :view :def :bind), of TYPE if given."
  (seq-filter (lambda (p) (and (plist-get p :def) (or (null type) (equal type (plist-get (plist-get p :def) :type)))))
              (mapcar (lambda (p) (list :name (plist-get p :name) :view (plist-get p :view)
                                        :bind (plist-get p :bind) :def (easel-params-normalize p)))
                      (append (plist-get scene :params) nil))))

(defun easel-params-channel-field (scene view-id channel)
  "Field mapped to CHANNEL (\"x\") in SCENE's view VIEW-ID."
  (when-let* ((view (seq-find (lambda (v) (equal (plist-get v :id) view-id)) (plist-get scene :views)))
              (scale (plist-get (plist-get view :scales) (easel-key channel))))
    (plist-get scale :field)))

(defun easel-params-point-fields (scene param)
  "The fields a point selection PARAM stores in SCENE."
  (let ((def (plist-get param :def)))
    (cond ((plist-get def :fields) (append (plist-get def :fields) nil))
          ((plist-get def :encodings)
           (delq nil (mapcar (lambda (ch) (easel-params-channel-field scene (plist-get param :view) ch))
                             (plist-get def :encodings))))
          (t (list (easel-key-name easel-params-row-key))))))

(defconst easel-params-row-key :_easel_row "Row identity key, as tagged by compile.")

(defun easel-params-point-store (fields rows)
  "A point selection store over FIELDS holding the tuples of ROWS."
  (when rows
    (list :type "point" :fields (vconcat fields)
          :values (vconcat (mapcar (lambda (row) (vconcat (mapcar (lambda (f) (plist-get row (easel-key f))) fields)))
                                   rows)))))

(defun easel-params-toggle (store fields row)
  "STORE with ROW's tuple over FIELDS toggled in or out."
  (let* ((tuple (vconcat (mapcar (lambda (f) (plist-get row (easel-key f))) fields)))
         (values (append (plist-get store :values) nil))
         (values (if (member tuple values) (delete tuple values) (append values (list tuple)))))
    (when values (list :type "point" :fields (vconcat fields) :values (vconcat values)))))

(defun easel-params--same (a b)
  "Equality for stored values: dates compare as instants."
  (or (equal a b)
      (and (numberp a) (numberp b) (= a b))
      (let ((ta (easel-time-parse a)) (tb (easel-time-parse b)))
        (and ta tb (stringp (if (stringp a) a b)) (= ta tb)))))

(defun easel-params--number (v)
  "V as a number for interval tests (dates become epoch ms)."
  (if (numberp v) v (easel-time-parse v)))

(defun easel-params-contains (store row)
  "Non-nil when ROW is in selection STORE (a non-empty store)."
  (pcase (plist-get store :type)
    ("point"
     (let ((fields (append (plist-get store :fields) nil)))
       (seq-some (lambda (tuple)
                   (cl-loop for f in fields for v across tuple
                            always (easel-params--same (plist-get row (easel-key f)) v)))
                 (plist-get store :values))))
    ("interval"
     (cl-loop for (channel field) on (plist-get store :fields) by #'cddr
              for range = (plist-get store channel)
              for v = (easel-params--number (plist-get row (easel-key field)))
              always (or (null range)
                         (and v (<= (min (aref range 0) (aref range 1)) v (max (aref range 0) (aref range 1)))))))
    (_ nil)))

(defun easel-params-test (state name row empty)
  "Whether ROW is in selection NAME under view STATE; EMPTY for empty selections."
  (let ((store (plist-get (plist-get state :params) (easel-key name))))
    (if (null store) empty (easel-params-contains store row))))

(defmacro easel-params-with-state (state &rest body)
  "Run BODY with selection hooks answering from view STATE."
  (declare (indent 1))
  (let ((s (make-symbol "state")))
    `(let* ((,s ,state)
            (easel-encode-param-test-function
             (lambda (name row empty) (easel-params-test ,s name row empty)))
            (easel-transform-param-predicate
             (lambda (name row _env empty) (easel-params-test ,s name row empty)))
            (easel-transform-param-filter-function
             (lambda (name rows empty)
               (easel-params-index-filter (plist-get (plist-get ,s :params) (easel-key name)) rows empty))))
       ,@body)))

(defun easel-params-summary (store)
  "A short human summary of selection STORE, or \"empty\"."
  (pcase (plist-get store :type)
    ("point" (format "%d value%s of %s" (length (plist-get store :values))
                     (if (= 1 (length (plist-get store :values))) "" "s")
                     (mapconcat #'identity (plist-get store :fields) ",")))
    ("interval" (mapconcat (lambda (ch)
                             (let ((r (plist-get store ch)))
                               (format "%s %s..%s" (plist-get (plist-get store :fields) ch)
                                       (easel-params--fmt (aref r 0)) (easel-params--fmt (aref r 1)))))
                           (seq-filter (lambda (ch) (plist-get store ch)) '(:x :y)) "; "))
    (_ "empty")))

(defun easel-params--fmt (v)
  "Format interval bound V (ISO date for epoch ms that look like dates)."
  (if (and (numberp v) (> (abs v) 1e11)) (easel-time-iso v) (format "%s" v)))

(provide 'easel-params)
;;; easel-params.el ends here
