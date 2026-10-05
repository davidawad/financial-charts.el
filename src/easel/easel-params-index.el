;;; easel-params-index.el --- indexed {"param": ...} filters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1/L6 (fc-qx1.9).  The Vega-Lite crosshair idiom is a rule
;; layer filtered by a point selection, {"filter": {"param": "hover"}},
;; so every pointermove re-runs that filter over all rows.  Testing each
;; row was 77% of a 100k-row hover.  `easel-params-index-filter' answers
;; the same filter from a hash of each row's tuple over the store's
;; fields, built once per rows vector and kept in a weak table, so a
;; hover costs the selected rows, not all of them.
;;
;; Keys normalise values the way `easel-params--same' compares them:
;; numbers as floats, date strings as epoch ms, anything else as itself.
;; Interval stores, and rows holding integers past 2^53 (where floats
;; would merge distinct values), fall back to the row-by-row test.

;;; Code:

(require 'easel-core)
(require 'easel-time)

(defvar easel-params-index--tables (make-hash-table :test 'eq :weakness 'key)
  "Rows vector -> alist of (FIELDS . HASH of normalised tuple -> indices).")

(defconst easel-params-index--exact (expt 2 53)
  "Largest integer magnitude a float holds exactly.")

(defun easel-params-index--norm (v)
  "V normalised so that `equal' on results is `easel-params--same' on values.
Throw `easel-params-index-skip' for integers floats cannot hold."
  (cond ((integerp v)
         (when (> (abs v) easel-params-index--exact) (throw 'easel-params-index-skip nil))
         (float v))
        ((floatp v) (+ 0.0 v))
        ((stringp v) (let ((ms (easel-time-parse v))) (if ms (easel-params-index--norm ms) v)))
        (t v)))

(defun easel-params-index--table (rows fields)
  "The tuple index of ROWS over FIELDS (strings), built on first use."
  (let ((cached (assoc fields (gethash rows easel-params-index--tables))))
    (or (cdr cached)
        (let ((table (make-hash-table :test 'equal :size (max 1 (length rows))))
              (keys (mapcar #'easel-key fields)))
          (dotimes (i (length rows))
            (let* ((row (aref rows i))
                   (tuple (mapcar (lambda (k) (easel-params-index--norm (plist-get row k))) keys)))
              (puthash tuple (cons i (gethash tuple table)) table)))
          (push (cons fields table) (gethash rows easel-params-index--tables))
          table))))

(defun easel-params-index--matches (store rows)
  "Indices of ROWS in point STORE, unsorted; throws when no index applies."
  (let ((table (easel-params-index--table rows (append (plist-get store :fields) nil))))
    (cl-loop for tuple across (plist-get store :values)
             append (copy-sequence (gethash (mapcar #'easel-params-index--norm tuple) table)))))

(defun easel-params-index-changed (rows pairs old new)
  "Sorted indices of ROWS whose membership may differ between OLD and NEW.
OLD and NEW are view states; PAIRS are (PARAM . EMPTY).  Returns `all'
when an index cannot bound the change (interval stores, or an empty
selection that means everything)."
  (or (catch 'easel-params-index-skip
        (let (hits)
          (dolist (p pairs)
            (dolist (state (list old new))
              (let ((store (plist-get (plist-get state :params) (easel-key (car p)))))
                (cond ((and (null store) (not (cdr p))))
                      ((equal (plist-get store :type) "point")
                       (setq hits (nconc (easel-params-index--matches store rows) hits)))
                      (t (throw 'easel-params-index-skip nil))))))
          (sort (delete-dups hits) #'<)))
      'all))

(defun easel-params-index-filter (store rows empty)
  "ROWS (a vector) in selection STORE, in order, or nil when no index applies.
A nil STORE is the empty selection: all ROWS when EMPTY, else none."
  (cond
   ((null store) (if empty rows []))
   ((equal (plist-get store :type) "point")
    (catch 'easel-params-index-skip
      (vconcat (mapcar (lambda (i) (aref rows i))
                       (sort (delete-dups (easel-params-index--matches store rows)) #'<)))))))

(provide 'easel-params-index)
;;; easel-params-index.el ends here
