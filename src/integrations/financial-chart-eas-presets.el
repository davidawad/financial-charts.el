;;; financial-chart-eas-presets.el --- named chart markup presets merged into composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A composed chart may name presets (fc-gty.4):
;;
;;   {"bars": [...], "preset": ["swing-trader", "trend"],
;;    "price": {"series": [{"indicator": "sma", "params": [7]}]}}
;;
;; A preset is a fragment of a chart description (usually "price" and
;; "panes") in NAME.json under `financial-chart-presets-directory', or
;; inline in the chart's own "presets": {NAME: FRAGMENT}, which wins
;; over a file of the same name.  A fragment may itself name a
;; "preset".  `financial-chart-presets-apply' merges the presets in
;; order and the chart last: in "price" (and in each pane) the lists
;; "studies", "series", "annotations", "fills", "rules" and "zones" are
;; appended, "panes" are appended, and any other key takes the later
;; value, so the chart's own keys win.  An unknown preset is
;; UNKNOWN_PRESET, a bad one INVALID_PRESET, with the JSON :path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-core)
(require 'financial-chart-eas-series)

(defcustom financial-chart-presets-directory
  (expand-file-name "presets" financial-chart-root)
  "Directory of chart markup presets, one NAME.json each."
  :type 'directory
  :group 'financial-chart)

(defconst financial-chart-presets--list-keys
  '(:studies :series :annotations :fills :rules :zones)
  "Pane keys whose lists a preset appends to.")

(defun financial-chart-presets-list ()
  "Names of the presets in `financial-chart-presets-directory', sorted."
  (when (file-directory-p financial-chart-presets-directory)
    (sort (mapcar #'file-name-sans-extension
                  (directory-files financial-chart-presets-directory nil "\\`[^.].*\\.json\\'"))
          #'string<)))

(defun financial-chart-preset-get (name)
  "Preset NAME's fragment from `financial-chart-presets-directory', or nil."
  (when (and (stringp name) (string-match-p "\\`[[:alnum:]_-]+\\'" name))
    (let ((file (expand-file-name (concat name ".json") financial-chart-presets-directory)))
      (when (file-readable-p file) (eas-json-read-file file)))))

(defun financial-chart-presets--object-p (value)
  "Non-nil when VALUE is a parsed JSON object."
  (and (listp value) (keywordp (car value))))

(defun financial-chart-presets--names (value path)
  "The \"preset\" VALUE as ((NAME . PATH) ...); PATH locates it."
  (cond ((null value) nil)
        ((stringp value) (list (cons value path)))
        ((or (vectorp value) (and (consp value) (not (keywordp (car value)))))
         (seq-map-indexed (lambda (name i)
                            (unless (stringp name)
                              (financial-chart-series-fail (format "%s/%d" path i) "INVALID_PRESET"
                                                           "A preset is named by a string, got %S" name))
                            (cons name (format "%s/%d" path i)))
                          value))
        (t (financial-chart-series-fail path "INVALID_PRESET"
                                        "preset is a name or an array of names, got %S" value))))

(defun financial-chart-presets--merge-pane (base over)
  "Pane OVER merged onto pane BASE: list keys appended, others OVER's."
  (let ((out (copy-sequence base)))
    (cl-loop for (k v) on over by #'cddr
             do (setq out (plist-put out k
                                     (if (memq k financial-chart-presets--list-keys)
                                         (vconcat (plist-get out k) (and (sequencep v) (not (stringp v)) v))
                                       v))))
    out))

(defun financial-chart-presets--merge (base over)
  "Chart fragment OVER merged onto BASE (see the commentary)."
  (let ((out (copy-sequence base)))
    (cl-loop for (k v) on over by #'cddr
             unless (memq k '(:preset :presets :doc))
             do (setq out (plist-put out k
                                     (pcase k
                                       (:price (financial-chart-presets--merge-pane (plist-get out :price) v))
                                       (:panes (vconcat (plist-get out :panes) v))
                                       (_ v)))))
    out))

(defun financial-chart-presets--resolve (name path inline seen)
  "Preset NAME (at PATH) expanded: its own presets merged under it.
INLINE is the chart's \"presets\" object; SEEN the names being expanded."
  (when (member name seen)
    (financial-chart-series-fail path "INVALID_PRESET" "Preset %s includes itself (%s)"
                                 name (string-join (reverse (cons name seen)) " -> ")))
  (let ((fragment (or (and inline (plist-get inline (intern (concat ":" name))))
                      (financial-chart-preset-get name))))
    (unless fragment
      (financial-chart-series-fail path "UNKNOWN_PRESET" "Unknown preset %S; presets: %s" name
                                   (string-join (append (cl-loop for (k _) on inline by #'cddr
                                                                 collect (substring (symbol-name k) 1))
                                                        (financial-chart-presets-list))
                                                ", ")))
    (unless (financial-chart-presets--object-p fragment)
      (financial-chart-series-fail path "INVALID_PRESET" "Preset %s is not an object" name))
    (let ((under (financial-chart-presets--names (plist-get fragment :preset) (concat path "/preset"))))
      (financial-chart-presets--merge
       (cl-reduce (lambda (acc p)
                    (financial-chart-presets--merge
                     acc (financial-chart-presets--resolve (car p) (cdr p) inline (cons name seen))))
                  under :initial-value nil)
       fragment))))

(defun financial-chart-presets-apply (chart)
  "CHART with its \"preset\" names merged in; CHART itself if it names none."
  (let ((inline (financial-chart-series-get chart :presets))
        (names (financial-chart-presets--names (financial-chart-series-get chart :preset) "/preset")))
    (when (and inline (not (financial-chart-presets--object-p inline)))
      (financial-chart-series-fail "/presets" "INVALID_PRESET"
                                   "presets is an object of NAME: chart fragment, got %S" inline))
    (if (not names) chart
      (financial-chart-presets--merge
       (cl-reduce (lambda (acc p)
                    (financial-chart-presets--merge
                     acc (financial-chart-presets--resolve (car p) (cdr p) inline nil)))
                  names :initial-value nil)
       chart))))

(defun financial-chart-presets-describe ()
  "The presets as JSON-ready data: directory, names and each one's \"doc\"."
  (list :directory financial-chart-presets-directory
        :presets (vconcat (mapcar (lambda (name)
                                    (list :name name
                                          :doc (or (financial-chart-series-get
                                                    (financial-chart-preset-get name) :doc)
                                                   :null)))
                                  (financial-chart-presets-list)))))

(provide 'financial-chart-eas-presets)
;;; financial-chart-eas-presets.el ends here
