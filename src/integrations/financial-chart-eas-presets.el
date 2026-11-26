;;; financial-chart-eas-presets.el --- named chart markup presets merged into composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Layer M4 of docs/design/markups.md (fc-gty.4).  A composed chart may
;; name presets:
;;
;;   {"bars": [...], "preset": ["trend", "swing-trader"],
;;    "price": {"series": [{"indicator": "sma", "params": [7]}]}}
;;
;; A preset is preset/v1: a fragment of a chart description (usually
;; "price" and "panes") with "preset/v1": NAME and a "doc".  It is found
;; in the chart's own "presets": {NAME: FRAGMENT}, else NAME.json in a
;; directory of `financial-chart-preset-directories' (owner data), else
;; in `financial-chart-presets-directory' (this package's presets/).  A
;; fragment may name its own "preset".
;;
;; The merge laws: presets merge in order and the chart last.  Lists
;; ("series", "studies", "annotations", "fills", "rules", "zones" in a
;; pane, and "panes") append; an entry whose id already appears replaces
;; the earlier one in its place instead of repeating (the id is its
;; "id", else for a series its indicator and params, for a study or a
;; pane its study and params, else the entry itself).  Other keys take
;; the later value, so the chart's own win; objects ("price", "colors")
;; merge by the same laws.  A preset that includes itself is
;; PRESET_CYCLE, an unknown one UNKNOWN_PRESET, a bad one
;; INVALID_PRESET, each with its JSON path.
;;
;; `financial-chart-expand' returns the merged description with every
;; list entry's provenance in "origin" ("preset:NAME" or "chart"): the
;; first thing to look at when a chart is not what you expected
;; ("origin", not "from": annotations already use "from" for a time).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-core)
(require 'financial-chart-eas-series)

(defcustom financial-chart-presets-directory
  (expand-file-name "presets" financial-chart-root)
  "This package's directory of chart markup presets, one NAME.json each."
  :type 'directory
  :group 'financial-chart)

(defcustom financial-chart-preset-directories nil
  "Owner directories of presets, searched in order before the package's.
A NAME.json here shadows the package's preset of that name."
  :type '(repeat directory)
  :group 'financial-chart)

(defconst financial-chart-presets--list-keys
  '(:studies :series :annotations :fills :rules :zones :panes)
  "Keys whose lists a preset appends to.")

(defconst financial-chart-presets--meta-keys
  '(:preset :presets :doc :preset/v1 :cohort)
  "Keys of a preset that describe it and are not merged.")

(defun financial-chart-presets--directories ()
  "Preset directories in search order."
  (append financial-chart-preset-directories (list financial-chart-presets-directory)))

(defun financial-chart-presets-list ()
  "Names of the presets in every preset directory, sorted, without repeats."
  (sort (delete-dups
         (cl-loop for dir in (financial-chart-presets--directories)
                  when (file-directory-p dir)
                  append (mapcar #'file-name-sans-extension
                                 (directory-files dir nil "\\`[^.].*\\.json\\'"))))
        #'string<))

(defun financial-chart-preset-file (name)
  "The file of preset NAME, or nil."
  (when (and (stringp name) (string-match-p "\\`[[:alnum:]_-]+\\'" name))
    (cl-loop for dir in (financial-chart-presets--directories)
             for file = (expand-file-name (concat name ".json") dir)
             when (file-readable-p file) return file)))

(defun financial-chart-preset-get (name)
  "Preset NAME's fragment from the preset directories, or nil."
  (when-let* ((file (financial-chart-preset-file name)))
    (eas-json-read-file file)))

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

;;; Merge laws

(defun financial-chart-presets--strip (entry)
  "ENTRY without its \"origin\"."
  (if (financial-chart-presets--object-p entry)
      (cl-loop for (k v) on entry by #'cddr unless (eq k :origin) append (list k v))
    entry))

(defun financial-chart-presets--entry-id (key entry)
  "The id under which list KEY's ENTRY replaces an earlier one, or nil."
  (let* ((e (financial-chart-presets--strip entry))
         (obj (financial-chart-presets--object-p e))
         (canon (lambda (x) (eas-json-encode (eas-json-canonical x)))))
    (cond ((and obj (plist-get e :id)) (format "%s#%s" key (plist-get e :id)))
          ((and (eq key :series) (stringp e)) (format "series:%s" e))
          ((and (eq key :series) obj (plist-get e :indicator))
           (format "series:%s%s%s" (plist-get e :indicator)
                   (funcall canon (or (plist-get e :params) []))
                   (if (plist-get e :output) (concat "." (plist-get e :output)) "")))
          ((eq key :series) nil)
          ((memq key '(:studies :panes))
           (cond ((stringp e) (format "study:%s[]" e))
                 ((and obj (stringp (plist-get e :study)))
                  (format "study:%s%s" (plist-get e :study) (funcall canon (or (plist-get e :params) []))))
                 (t (format "%s:%s" key (funcall canon e)))))
          (t (format "%s:%s" key (funcall canon e))))))

(defun financial-chart-presets--append (key base over)
  "List KEY's BASE with OVER appended; a repeated id replaces in place."
  (let ((out (append base nil)))
    (dolist (e (append over nil))
      (let* ((id (financial-chart-presets--entry-id key e))
             (pos (and id (cl-position id out :test #'equal
                                       :key (lambda (x) (financial-chart-presets--entry-id key x))))))
        (if pos (setf (nth pos out) e) (setq out (append out (list e))))))
    (vconcat out)))

(defun financial-chart-presets--merge (base over)
  "OVER merged onto BASE by the merge laws (see the commentary)."
  (let ((out (copy-sequence base)))
    (cl-loop for (k v) on over by #'cddr
             unless (memq k financial-chart-presets--meta-keys)
             do (let ((old (plist-get out k)))
                  (setq out (plist-put out k
                                       (cond ((and (memq k financial-chart-presets--list-keys)
                                                   (or (vectorp v) (null v) (and (consp v) (not (keywordp (car v))))))
                                              (financial-chart-presets--append k old v))
                                             ((and (financial-chart-presets--object-p v)
                                                   (or (null old) (financial-chart-presets--object-p old)))
                                              (financial-chart-presets--merge old v))
                                             (t v))))))
    out))

(defun financial-chart-presets--tag (fragment origin)
  "FRAGMENT with every list entry's \"origin\" set to ORIGIN (a string).
A bare string entry (a study or series name) becomes an object."
  (cl-loop for (k v) on fragment by #'cddr
           append (list k
                        (cond ((and (memq k financial-chart-presets--list-keys)
                                    (or (vectorp v) (and (consp v) (not (keywordp (car v))))))
                               (vconcat
                                (mapcar (lambda (e)
                                          (let ((e (cond ((financial-chart-presets--object-p e) e)
                                                         ((and (stringp e) (memq k '(:studies :panes))) (list :study e))
                                                         ((and (stringp e) (eq k :series)) (list :indicator e))
                                                         (t nil))))
                                            (if e (append (financial-chart-presets--tag (financial-chart-presets--strip e) origin)
                                                          (list :origin origin))
                                              e)))
                                        v)))
                              ((and (financial-chart-presets--object-p v) (not (memq k financial-chart-presets--meta-keys)))
                               (financial-chart-presets--tag v origin))
                              (t v)))))

;;; Resolution

(defun financial-chart-presets--resolve (name path inline seen tag)
  "Preset NAME (at PATH) with its own presets merged under it.
INLINE is the chart's \"presets\" object; SEEN the names being
expanded; TAG non-nil sets each entry's origin."
  (when (member name seen)
    (financial-chart-series-fail path "PRESET_CYCLE" "Preset %s includes itself (%s)"
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
                     acc (financial-chart-presets--resolve (car p) (cdr p) inline (cons name seen) tag)))
                  under :initial-value nil)
       (if tag (financial-chart-presets--tag fragment (concat "preset:" name)) fragment)))))

(defun financial-chart-presets--apply (chart tag)
  "CHART with its presets merged in; TAG non-nil records provenance."
  (let ((inline (financial-chart-series-get chart :presets))
        (names (financial-chart-presets--names (financial-chart-series-get chart :preset) "/preset")))
    (when (and inline (not (financial-chart-presets--object-p inline)))
      (financial-chart-series-fail "/presets" "INVALID_PRESET"
                                   "presets is an object of NAME: chart fragment, got %S" inline))
    (if (and (not names) (not tag)) chart
      (let ((merged (financial-chart-presets--merge
                     (cl-reduce (lambda (acc p)
                                  (financial-chart-presets--merge
                                   acc (financial-chart-presets--resolve (car p) (cdr p) inline nil tag)))
                                names :initial-value nil)
                     (if tag (financial-chart-presets--tag chart "chart") chart))))
        ;; Bars and other scalars of the chart pass through untouched.
        (if (plist-member chart :bars) (plist-put merged :bars (plist-get chart :bars)) merged)))))

(defun financial-chart-presets-apply (chart)
  "CHART with its \"preset\" names merged in; CHART itself if it names none."
  (financial-chart-presets--apply chart nil))

(declare-function financial-chart-compose-read "financial-chart-eas-compose" (chart))

(defun financial-chart-expand (chart)
  "CHART (a plist, JSON text or file) with its presets merged by the merge
laws and every list entry's provenance in \"origin\": \"preset:NAME\"
or \"chart\".  Pure; the result composes like CHART."
  (financial-chart-presets--apply (financial-chart-compose-read chart) t))

(defun financial-chart-expand-verb (chart)
  "`financial-chart-expand' in the chart/v1 envelope, without the bars."
  (condition-case err
      (let ((expanded (financial-chart-expand chart)))
        (list :contract "chart/v1" :ok t
              :data (cl-loop for (k v) on expanded by #'cddr unless (eq k :bars) append (list k v))
              :next ["(financial-chart-compose CHART)"]))
    (financial-chart-invalid-chart
     (list :contract "chart/v1" :ok :false :reason (plist-get (cddr err) :code) :message (cadr err)
           :evidence (list :path (plist-get (cddr err) :path))
           :next ["(financial-chart-presets-describe)"]))))

;;; Describe and lint

(defun financial-chart-presets-describe ()
  "The presets as JSON-ready data: directories, names and each one's \"doc\"."
  (list :directories (vconcat (financial-chart-presets--directories))
        :presets (vconcat (mapcar (lambda (name)
                                    (list :name name :file (financial-chart-preset-file name)
                                          :doc (or (financial-chart-series-get (financial-chart-preset-get name) :doc)
                                                   :null)))
                                  (financial-chart-presets-list)))))

(defun financial-chart-presets-lint ()
  "Problems with the preset files as (NAME . MESSAGE), empty when clean.
Each is preset/v1 naming its file, has a \"doc\" and expands."
  (cl-loop for name in (financial-chart-presets-list)
           for p = (condition-case err (financial-chart-preset-get name) (error err))
           append
           (delq nil
                 (list (unless (equal (and (financial-chart-presets--object-p p) (plist-get p :preset/v1)) name)
                         (cons name "\"preset/v1\" does not name the file"))
                       (unless (and (financial-chart-presets--object-p p) (stringp (plist-get p :doc)))
                         (cons name "no \"doc\""))
                       (condition-case err
                           (progn (financial-chart-presets-apply (list :preset name)) nil)
                         (financial-chart-invalid-chart (cons name (cadr err))))))))

(provide 'financial-chart-eas-presets)
;;; financial-chart-eas-presets.el ends here
