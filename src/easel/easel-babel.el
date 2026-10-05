;;; easel-babel.el --- org-babel: #+begin_src easel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L7 (fc-qx1.13).  One org chart block type:
;;
;;   #+begin_src easel :template ohlc :data tbl
;;   {"title": "TSM"}
;;   #+end_src
;;
;; :template names a template; the body, when present, is a JSON
;; object of extra bindings (or, without :template, a whole chart/v1
;; spec).  :data binds the template's data slot from an org table, a
;; named table or babel result (through the org-table adapter), or a
;; .json/.csv/.tsv file.  Any other header named like a slot binds it
;; (:title "TSM", :points true), and :var NAME=REF binds slot NAME.
;;
;; What the block yields:
;;
;;   :file F :results file   F by extension: .svg native SVG, .json
;;                           resolved pure Vega-Lite, .txt the text
;;                           chart, .png/.pdf through bin/chart.
;;   :as view (default)      the text chart as the result, and a live
;;                           view inline (easel-babel-inline.el): the
;;                           SVG over it in GUI frames, the text itself
;;                           in terminals.  Clicking a mark jumps to its
;;                           source row (easel-babel-source.el).
;;   :as text                the text chart only.
;;   :as vl                  resolved pure Vega-Lite; with
;;                           :wrap src vega-lite it feeds ob-vega, and it
;;                           is what render's ::: {.chart} and bin/chart
;;                           take.
;;
;; While exporting no view opens, and :as defaults to
;; `easel-babel-export-as'.  :cols/:rows size the text chart and
;; :width/:height the SVG; both default to the spec's own size.

;;; Code:

(require 'easel)
(require 'ob-core)

(declare-function org-babel-ref-resolve "ob-ref" (ref))

(defvar org-export-current-backend)

(defvar org-babel-default-header-args:easel '((:results . "verbatim"))
  "Default header arguments for easel blocks.")

(defvar easel-babel-export-as "text"
  "What an easel block without :file yields while exporting: text or vl.")

(defvar easel-babel-target nil
  "Target of block views: nil picks svg in graphic frames, else text.")

(defconst easel-babel--reserved
  '(:data :template :as :cols :rows :width :height :id :var :file :results :exports
    :result-params :result-type :session :cache :noweb :tangle :hlines :colnames
    :rownames :wrap :post :output-dir :file-desc :file-ext :file-mode :eval :dir :sep)
  "Header arguments that never bind a template slot.")

(defvar easel-babel-after-open-functions nil
  "Hook run with VIEW, SOURCE and BLOCK after a block opens a live view.
SOURCE is (:name REF :buffer B) when :data named an org table or
result, else nil; BLOCK is a marker at the block, or nil.")

;;; Data

(defun easel-babel--file-adapter (file)
  "The adapter name for data FILE by extension, or nil."
  (pcase (downcase (or (file-name-extension file) ""))
    ("json" "json") ("csv" "csv") ("tsv" "tsv")))

(defun easel-babel--resolve-ref (ref)
  "The value of org REF (a table, named table, result or src block name)."
  (require 'ob-ref)
  (condition-case err
      (org-babel-ref-resolve ref)
    (error (easel-signal "NOT_FOUND"
                         (format "No org table, result or block named %s here (%s); add #+name: %s above it"
                                 ref (error-message-string err) ref)
                         :table ref))))

(defun easel-babel-data (value)
  "VALUE (a :data or :var value) as data/v1.
A string names a .json/.csv/.tsv file or an org reference; a list of
rows (header first, `hline' allowed) is an org table or babel result;
a list of plists is plist rows."
  (cond
   ((easel-data-p value) value)
   ((and (stringp value) (easel-babel--file-adapter value))
    (unless (file-exists-p value)
      (easel-signal "NOT_FOUND" (format "No data file %s (from %s); fix the :data path"
                                        value default-directory)
                    :file value))
    (easel-data-from (easel-babel--file-adapter value) (list :file (expand-file-name value))))
   ((stringp value) (easel-babel-data (easel-babel--resolve-ref value)))
   ((and (consp value) (easel-object-p (car value))) (easel-data-from "plist" value))
   ((and (consp value) (or (listp (car value)) (eq (car value) 'hline)))
    (easel-data-from "org-table" value))
   (t (easel-signal "INVALID_INPUT"
                    (format "Cannot read chart data from %S; give :data a table name or a .json/.csv file"
                            value)
                    :header "data"))))

(defun easel-babel--slot-value (def value)
  "VALUE for a data slot with DEF: rows, or the raw table for org-table slots."
  (if (equal (plist-get def :shape) "org-table") value
    (plist-get (easel-babel-data value) :rows)))

(defun easel-babel-data-slot (template)
  "Name (a keyword) of TEMPLATE's data slot: the first required shape slot."
  (let ((slots (easel-template-slots template)) first)
    (or (cl-loop for (slot def) on slots by #'cddr
                 when (plist-get def :shape)
                 do (setq first (or first slot))
                 and when (easel-true-p (plist-get def :required)) return slot)
        first
        (easel-signal "INVALID_INPUT"
                      (format "Template %s has no data slot for :data" (plist-get template :name))
                      :header "data"))))

;;; Bindings

(defun easel-babel--scalar (def value)
  "Header VALUE (as org read it) coerced to slot DEF's type."
  (pcase (plist-get def :type)
    ("boolean" (cond ((memq value '(t :false)) value)
                     ((member (format "%s" value) '("true" "yes" "t" "1")) t)
                     (t :false)))
    ("string" (format "%s" value))
    (_ value)))

(defun easel-babel--body (body)
  "BODY parsed as JSON, or nil when blank."
  (let ((text (string-trim (or body ""))))
    (unless (string-empty-p text)
      (condition-case err
          (easel-json-parse text)
        (easel-error (easel-signal "PARSE_ERROR"
                                   (format "The block body is not JSON (%s); give bindings as a JSON object"
                                           (plist-get (easel-error-plist err) :message))
                                   :path "body"))))))

(defun easel-babel-bindings (template params body)
  "Bindings for TEMPLATE (a plist) from org PARAMS and the JSON BODY."
  (let* ((slots (easel-template-slots template))
         (bindings (easel-babel--body body)))
    (unless (easel-object-p bindings)
      (easel-signal "INVALID_INPUT" "With :template, the block body is a JSON object of bindings"
                    :path "body"))
    (pcase-dolist (`(,key . ,value) params)
      (cond
       ((and (eq key :var) (consp value) (plist-member slots (easel-key (car value))))
        (let* ((slot (easel-key (car value))) (def (plist-get slots slot)))
          (setq bindings (easel-plist-put bindings slot
                                          (if (plist-get def :shape)
                                              (easel-babel--slot-value def (cdr value))
                                            (cdr value))))))
       ((and (not (memq key easel-babel--reserved)) (plist-member slots key))
        (setq bindings (easel-plist-put bindings key (easel-babel--scalar (plist-get slots key) value))))))
    (when-let* ((data (cdr (assq :data params))))
      (let ((slot (easel-babel-data-slot template)))
        (setq bindings (easel-plist-put bindings slot
                                        (easel-babel--slot-value (plist-get slots slot) data)))))
    bindings))

(defun easel-babel-spec (params body)
  "The block's resolved Vega-Lite and template name: (SPEC . NAME).
NAME is nil when BODY is a whole chart/v1 spec."
  (let ((name (cdr (assq :template params))))
    (if name
        (let* ((name (format "%s" name)) (template (easel-template-get name)))
          (cons (easel-resolve template (easel-babel-bindings template params body)) name))
      (let ((spec (easel-babel--body body)))
        (unless spec
          (easel-signal "INVALID_INPUT"
                        (format "Give :template (one of %s) or a chart/v1 spec as the body"
                                (string-join (easel-template-names) ", "))
                        :header "template"))
        (cons (easel-resolve-spec spec) nil)))))

(defun easel-babel--rows (params)
  "Root rows from :data for a plain spec, or nil."
  (when-let* ((data (cdr (assq :data params))))
    (plist-get (easel-babel-data data) :rows)))

;;; Output

(defun easel-babel--number (params key)
  "Header KEY of PARAMS as a number, or nil."
  (let ((v (cdr (assq key params))))
    (cond ((numberp v) v)
          ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v)))))

(defun easel-babel-size (params target)
  "Compile size PARAMS ask for under TARGET (text or svg), or nil."
  (let ((c (easel-babel--number params :cols)) (r (easel-babel--number params :rows))
        (w (easel-babel--number params :width)) (h (easel-babel--number params :height)))
    (if (eq target 'text)
        (and c r (list :cols c :rows r))
      (and w h (cons w h)))))

(defun easel-babel-text (spec params &optional rows)
  "SPEC (with root ROWS) drawn as the deterministic text chart."
  (substring-no-properties
   (easel-text-render (easel-compile spec :rows rows :target 'text
                                     :size (easel-babel-size params 'text)))))

(defun easel-babel--write (file data &optional binary)
  "Write DATA to FILE (unibyte when BINARY)."
  (let ((coding-system-for-write (if binary 'binary 'utf-8)))
    (write-region data nil file nil 'silent)))

(defun easel-babel-write-file (file spec params &optional rows)
  "Write SPEC (with root ROWS) to FILE in the format its extension names."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (pcase ext
      ("svg" (easel-babel--write file (easel-svg-render
                                       (easel-compile spec :rows rows
                                                      :size (easel-babel-size params 'svg)))))
      ("json" (easel-babel--write file (concat (easel-json-pretty spec) "\n")))
      ("txt" (easel-babel--write file (easel-babel-text spec params rows)))
      ((or "png" "pdf") (easel-babel--write file (easel-chart-build spec ext) t))
      (_ (easel-signal "INVALID_INPUT"
                       (format "Cannot write %s; use a .svg, .json, .txt, .png or .pdf :file" file)
                       :header "file")))))

(defun easel-babel--exporting-p ()
  "Non-nil while org exports."
  (and (boundp 'org-export-current-backend) org-export-current-backend t))

(defun easel-babel-as (params)
  "What the block yields: \"view\", \"text\" or \"vl\"."
  (let ((as (format "%s" (or (cdr (assq :as params))
                             (if (easel-babel--exporting-p) easel-babel-export-as "view")))))
    (unless (member as '("view" "text" "vl"))
      (easel-signal "INVALID_INPUT" (format ":as %s is not one of view, text, vl" as) :header "as"))
    (if (and (equal as "view") (easel-babel--exporting-p)) "text" as)))

(defun easel-babel-source (params)
  "Where the block's :data came from: (:name REF :buffer B), or nil."
  (let ((data (cdr (assq :data params))))
    (when (and (stringp data) (not (easel-babel--file-adapter data)))
      (list :name data :buffer (current-buffer)))))

(defun easel-babel--block ()
  "Marker at the block being executed, or nil."
  (let ((loc (and (boundp 'org-babel-current-src-block-location)
                  org-babel-current-src-block-location)))
    (cond ((markerp loc) loc)
          ((integerp loc) (copy-marker loc)))))

(defun easel-babel-view-id (template params)
  "Id of a block's view: TEMPLATE:NAME, NAME its :id, #+name or :data."
  (format "%s:%s" (or template "chart")
          (or (cdr (assq :id params))
              (when-let* ((block (easel-babel--block)))
                (with-current-buffer (marker-buffer block)
                  (save-excursion
                    (goto-char block)
                    (nth 4 (org-babel-get-src-block-info 'no-eval)))))
              (let ((d (cdr (assq :data params)))) (and (stringp d) (file-name-base d)))
              "org")))

(defun easel-babel-open (spec template params rows)
  "Open a live view of resolved SPEC (with root ROWS) for a block.
Re-running the block replaces its view.  Return the view."
  (let ((id (easel-babel-view-id template params)))
    (when (gethash id easel-views) (easel-view-close id))
    (let* ((target (or easel-babel-target
                       (if (and (display-graphic-p) (image-type-available-p 'svg)) 'svg 'text)))
           (view (easel-view-open spec :id id :rows rows :target target
                                  :size (easel-babel-size params target))))
      (setf (easel-view-template view) template)
      (run-hook-with-args 'easel-babel-after-open-functions view (easel-babel-source params)
                          (easel-babel--block))
      view)))

;;;###autoload
(defun org-babel-execute:easel (body params)
  "Execute an easel block: JSON BODY under header PARAMS.
See the commentary of easel-babel.el for the header arguments."
  (let ((file (cdr (assq :file params))))
    (when (and file (not (member "file" (cdr (assq :result-params params)))))
      (easel-signal "INVALID_INPUT" ":file needs :results file (as with ob-dot)" :header "file"))
    (easel-babel--execute body params file)))

(defun easel-babel--execute (body params file)
  "Run a block (BODY, PARAMS) writing FILE when non-nil; return its result."
  (pcase-let* ((as (easel-babel-as params))
               (`(,spec . ,template) (easel-babel-spec params body))
               (rows (and (null template) (easel-babel--rows params))))
    (cond
     (file
      (easel-babel-write-file file spec params rows)
      nil)
     (t
      (pcase as
        ("vl" (easel-json-pretty spec))
        ("text" (easel-babel-text spec params rows))
        (_ (let ((view (easel-babel-open spec template params rows)))
             (cond ((not (easel-view-interactive view))
                    (mapconcat (lambda (w) (format "Static chart: %s unsupported at %s"
                                                   (or (plist-get w :feature) (plist-get w :code))
                                                   (plist-get w :path)))
                               (easel-view-warnings view) "\n"))
                   ((eq (easel-view-target view) 'text)
                    (substring-no-properties (easel-text-render (easel-view-scene view))))
                   (t (easel-babel-text spec params rows))))))))))

(provide 'easel-babel)
;;; easel-babel.el ends here
