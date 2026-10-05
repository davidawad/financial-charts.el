;;; easel-babel-source.el --- org-babel: click a mark, land on its table row -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L7 (fc-qx1.13).  A view opened by an easel block whose
;; :data names an org table (or a named result or block) remembers
;; that name, and its clicks run the "org-source-row" action: the
;; clicked datum's row index (the _easel_row tag compile puts on every
;; source row) is counted down the table, skipping the header and
;; hlines, and point lands on that row.  The binding is per view
;; (`easel-action-bind' on "*" and on every key the template's own
;; actions name), so the template still decides for views opened any
;; other way.  The table is looked up by name at click time, so edits
;; above it do not break the jump.

;;; Code:

(require 'easel-babel)
(require 'easel-action)

(declare-function org-at-table-p "org-table" (&optional table-type))
(declare-function org-babel-find-named-result "ob-core" (name))
(declare-function org-babel-where-is-src-block-result "ob-core" (&optional insert info hash))

(defvar easel-babel-sources (make-hash-table :test 'equal)
  "Org data source of each babel view: view id -> (:name REF :buffer B).")

(defun easel-babel-source-of (view)
  "The org source of VIEW (an id or view), or nil."
  (gethash (easel-view-id (easel-view-get view)) easel-babel-sources))

(defun easel-babel-source--table-at (pos)
  "Start of the table at POS, or of the result table of the block at POS."
  (save-excursion
    (goto-char pos)
    (let ((case-fold-search t))
      (while (looking-at-p "^[ \t]*#\\+\\(name\\|tblname\\|caption\\|header\\|attr_[a-z]+\\|plot\\|results\\):")
        (forward-line 1)))
    (cond
     ((org-at-table-p) (line-beginning-position))
     ((looking-at-p "^[ \t]*#\\+begin_src")
      (when-let* ((res (org-babel-where-is-src-block-result)))
        (goto-char res)
        (forward-line 1)
        (and (org-at-table-p) (line-beginning-position)))))))

(defun easel-babel-source-table (source)
  "Marker at the first line of SOURCE's table; signal NOT_FOUND when gone."
  (let ((name (plist-get source :name)) (buffer (plist-get source :buffer)))
    (unless (buffer-live-p buffer)
      (easel-signal "NOT_FOUND" (format "The org buffer holding %s was killed; re-run the block" name)
                    :table name))
    (with-current-buffer buffer
      (require 'org-table)
      (save-excursion
        (goto-char (point-min))
        (let* ((case-fold-search t)
               (start (or (and (re-search-forward
                                (format "^[ \t]*#\\+\\(?:tbl\\)?name:[ \t]*%s[ \t]*$" (regexp-quote name))
                                nil t)
                               (easel-babel-source--table-at (line-beginning-position)))
                          (when-let* ((res (org-babel-find-named-result name)))
                            (easel-babel-source--table-at res)))))
          (unless start
            (easel-signal "NOT_FOUND" (format "No org table named %s in %s; name it with #+name: %s"
                                              name (buffer-name) name)
                          :table name))
          (copy-marker start))))))

(defun easel-babel-source-row (source index)
  "Marker at data row INDEX (0-based, after the header) of SOURCE's table."
  (let ((table (easel-babel-source-table source)) (n -2)) ; the header row is -1
    (with-current-buffer (marker-buffer table)
      (save-excursion
        (goto-char table)
        (while (and (< n index) (org-at-table-p))
          (unless (looking-at-p "^[ \t]*|-") (setq n (1+ n)))
          (when (< n index) (forward-line 1)))
        (unless (and (= n index) (org-at-table-p))
          (easel-signal "NOT_FOUND" (format "Table %s has no data row %d; re-run the block"
                                            (plist-get source :name) (1+ index))
                        :table (plist-get source :name) :index index))
        (skip-chars-forward " \t|")
        (point-marker)))))

(defun easel-babel-source--index (view target)
  "Source row index of click TARGET in VIEW, or nil."
  (let* ((scene-view (seq-find (lambda (v) (equal (plist-get v :id) (plist-get target :view)))
                               (plist-get (easel-view-scene view) :views)))
         (mark (and scene-view (seq-find (lambda (m) (equal (plist-get m :id) (plist-get target :mark)))
                                         (plist-get scene-view :marks))))
         (rows (and mark (plist-get mark :rows)))
         (datum (plist-get target :datum)))
    (or (and rows (integerp datum) (< datum (length rows))
             (plist-get (aref rows datum) easel-compile-row-key))
        (seq-position (plist-get (easel-view-data view) :rows) (plist-get target :row)))))

(defun easel-babel-source-goto (target view)
  "Move to the org table row behind click TARGET in VIEW; return NAME:ROW."
  (let* ((source (or (easel-babel-source-of view)
                     (easel-signal "NOT_FOUND" (format "View %s has no org table source" (easel-view-id view))
                                   :view (easel-view-id view))))
         (index (or (easel-babel-source--index view target)
                    (easel-signal "ENGINE_FAILED"
                                  "This mark draws derived rows (aggregated), not a table row"
                                  :mark (plist-get target :mark))))
         (pos (easel-babel-source-row source index)))
    (with-current-buffer (marker-buffer pos) (push-mark nil t))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    (format "%s:%d" (plist-get source :name) (1+ index))))

(easel-action-define "org-source-row" #'easel-babel-source-goto
                     :doc "Jump to the org table row behind the clicked datum (views opened by org-babel).")

(defun easel-babel-source--on-open (view source _block)
  "Remember VIEW's org SOURCE and make its clicks jump to the source row."
  (remhash (easel-view-id view) easel-babel-sources)
  (when source
    (puthash (easel-view-id view) source easel-babel-sources)
    (dolist (key (cons "*" (when-let* ((name (easel-view-template view)))
                             (mapcar #'easel-key-name
                                     (easel-plist-keys (plist-get (plist-get (easel-template-get name) :meta)
                                                                  :actions))))))
      (easel-action-bind view key "org-source-row"))))

(add-hook 'easel-babel-after-open-functions #'easel-babel-source--on-open)

(provide 'easel-babel-source)
;;; easel-babel-source.el ends here
