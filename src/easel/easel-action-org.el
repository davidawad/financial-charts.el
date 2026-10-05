;;; easel-action-org.el --- actions that jump into org: source rows, notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.5).  Two built-in actions for charts whose data
;; lives in org:
;;
;;   goto-source  jump to where the clicked datum came from:
;;                1. the row's link column (args "field", default
;;                   "source"): an org link or URL, opened as open-href
;;                2. the view's source (`easel-action-set-source', or
;;                   args "file"/"buffer" with "table"/"heading"): the
;;                   datum's line in the named org table, or the
;;                   heading whose title holds the row's HEADING field
;;   open-notes   open the notes for the datum's date: DIRECTORY/DATE.org
;;                (`easel-action-notes-directory', args "directory"),
;;                or the heading naming the date in one file
;;                (`easel-action-notes-file', args "file").  The date
;;                comes from args "field", default the view's x field.
;;
;; Both show the buffer with `easel-action-display-function' and
;; answer "FILE-OR-BUFFER:LINE"; what they cannot find is NOT_FOUND,
;; recorded on the click like any action failure.

;;; Code:

(require 'easel-core)
(require 'easel-time)
(require 'easel-params)
(require 'easel-view)
(require 'easel-action)

(declare-function org-at-table-p "org-table" (&optional table-type))

(defvar easel-action-display-function #'pop-to-buffer
  "Function the org actions call with the buffer they jumped in.")

(defvar easel-action-notes-directory nil
  "Directory of daily notes files, named by `easel-action-notes-file-format'.")

(defvar easel-action-notes-file nil
  "One org file whose headings name the dates they hold notes for.")

(defvar easel-action-notes-file-format "%Y-%m-%d.org"
  "`format-time-string' format (UTC) of a day's notes file name.")

(defvar easel-action-notes-date-format "%Y-%m-%d"
  "`format-time-string' format (UTC) of the date a notes heading contains.")

(defvar easel-action--sources (make-hash-table :test 'eq :weakness 'key)
  "Per-view source of the rows: view -> (:file F | :buffer B ...).")

(defun easel-action-set-source (view source)
  "Record where VIEW's rows came from, for goto-source; return SOURCE.
SOURCE is a plist: :file F or :buffer B, then :table NAME (the
#+name of the org table the rows were read from, row N being its Nth
data line; nil means the first table) or :heading FIELD (each row
names its heading in FIELD).  nil forgets it."
  (let ((view (easel-view-get view)))
    (if source (puthash view source easel-action--sources) (remhash view easel-action--sources))
    source))

(defun easel-action--arg (target key)
  "TARGET's binding argument KEY (a keyword), or nil."
  (plist-get (plist-get target :args) key))

(defun easel-action--show (buffer pos)
  "Show BUFFER with point at POS; return \"FILE-OR-BUFFER:LINE\"."
  (with-current-buffer buffer
    (goto-char pos)
    (funcall easel-action-display-function buffer)
    (when-let* ((window (get-buffer-window buffer)))
      (set-window-point window pos))
    (format "%s:%d" (or (buffer-file-name buffer) (buffer-name buffer))
            (with-current-buffer buffer (line-number-at-pos pos)))))

(defun easel-action--source-buffer (source)
  "The buffer SOURCE's :file or :buffer names."
  (let ((file (plist-get source :file)) (buffer (plist-get source :buffer)))
    (cond (file (find-file-noselect (expand-file-name file)))
          ((and buffer (buffer-live-p (get-buffer buffer))) (get-buffer buffer))
          (t (easel-signal "NOT_FOUND" (format "Source %s is neither a file nor a live buffer; pass :file or :buffer"
                                               (or file buffer))
                           :action "goto-source")))))

(defun easel-action--table-row (name n)
  "Position of data row N of the org table named NAME (nil: the first table).
The first non-hline line is the header, as the org-table adapter reads it."
  (require 'org-table)
  (save-excursion
    (goto-char (point-min))
    (unless (if name
                (re-search-forward (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
              (re-search-forward "^[ \t]*|" nil t))
      (easel-signal "NOT_FOUND" (format "No org table %s in %s" (or name "at all") (buffer-name))
                    :action "goto-source" :table name))
    (forward-line (if name 1 0))
    (let ((k -2))                       ; the header line is row -1
      (while (and (< k n) (org-at-table-p))
        (unless (looking-at-p "^[ \t]*|-") (setq k (1+ k)))
        (when (< k n) (forward-line 1)))
      (unless (and (= k n) (org-at-table-p))
        (easel-signal "NOT_FOUND" (format "Table %s has no data row %d; was it edited since the chart opened?"
                                          (or name "") n)
                      :action "goto-source" :index n))
      (skip-chars-forward " \t|")
      (point))))

(defun easel-action--heading (title action)
  "Position of the first heading whose title contains TITLE, for ACTION."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*+[ \t]+.*%s" (regexp-quote title)) nil t)
        (line-beginning-position)
      (easel-signal "NOT_FOUND" (format "No heading mentions %s in %s; add one" title (buffer-name))
                    :action action :heading title))))

(defun easel-action-goto-source (target view)
  "Jump to the org row, heading or link TARGET's datum in VIEW came from."
  (let* ((row (plist-get target :row))
         (link (plist-get row (easel-key (or (easel-action--arg target :field) "source"))))
         (args (plist-get target :args))
         (source (if (or (plist-get args :file) (plist-get args :buffer)) args
                   (gethash view easel-action--sources))))
    (cond
     ((and (stringp link) (not (string-empty-p link))) (easel-action-open-link link))
     (source
      (let ((buffer (easel-action--source-buffer source)))
        (easel-action--show
         buffer
         (with-current-buffer buffer
           (if-let* ((field (plist-get source :heading)))
               (easel-action--heading (format "%s" (plist-get row (easel-key field))) "goto-source")
             (let ((n (plist-get target :source-row)))
               (unless (integerp n)
                 (easel-signal "NOT_FOUND" "This datum has no source row (an aggregate?); use :heading or a link column"
                               :action "goto-source"))
               (easel-action--table-row (plist-get source :table) n)))))))
     (t (easel-signal "NOT_FOUND"
                      "This datum has no source: give rows a source column (an org link) or call `easel-action-set-source'"
                      :action "goto-source")))))

(defun easel-action--date (target view)
  "TARGET's date in VIEW as epoch ms, from args field or the x field."
  (let* ((given (easel-action--arg target :field))
         (scene (easel-view-scene view))
         (field (or given (easel-params-channel-field scene (plist-get target :view) "x")))
         (value (and field (plist-get (plist-get target :row) (easel-key field))))
         (x (plist-get (plist-get (seq-find (lambda (v) (equal (plist-get v :id) (plist-get target :view)))
                                            (plist-get scene :views))
                                  :scales)
                       :x)))
    (or (and (stringp value) (easel-time-parse value))
        (and (numberp value) (or given (member (plist-get x :type) '("time" "utc"))) value)
        (easel-signal "NOT_FOUND" (format "Datum has no date in field %s; bind open-notes with a \"field\"" field)
                      :action "open-notes" :field field))))

(defun easel-action-open-notes (target view)
  "Open the notes for TARGET's date in VIEW."
  (let* ((time (/ (easel-action--date target view) 1000.0))
         (directory (or (easel-action--arg target :directory) easel-action-notes-directory))
         (file (or (easel-action--arg target :file) easel-action-notes-file)))
    (cond
     (directory
      (let ((buffer (find-file-noselect
                     (expand-file-name (format-time-string (or (easel-action--arg target :format)
                                                               easel-action-notes-file-format)
                                                           time t)
                                       directory))))
        (easel-action--show buffer (with-current-buffer buffer (point-min)))))
     (file
      (let ((buffer (find-file-noselect (expand-file-name file))))
        (easel-action--show buffer (with-current-buffer buffer
                                     (easel-action--heading (format-time-string easel-action-notes-date-format time t)
                                                            "open-notes")))))
     (t (easel-signal "NOT_FOUND"
                      "No notes location: set `easel-action-notes-directory' or `easel-action-notes-file', or bind args"
                      :action "open-notes")))))

(easel-register-action
 "goto-source" :fn #'easel-action-goto-source
 :doc "Jump to the datum's source: its link column, or its org table row or heading.")

(easel-register-action
 "open-notes" :fn #'easel-action-open-notes
 :doc "Open the notes for the datum's date (a file per day, or a heading in one file).")

(provide 'easel-action-org)
;;; easel-action-org.el ends here
