;;; financial-chart-matrix.el --- Matrix and volume-profile charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@protonmail.com>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Heatmaps for labeled numeric matrices, and OHLCV volume profiles.
;; The latter spreads each bar's volume evenly across the price bins touched
;; by its low-high range; it is an OHLCV approximation, not trade-level data.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'financial-chart-plot)

(defconst financial-chart-matrix--negative-color "#2166ac")
(defconst financial-chart-matrix--neutral-color "#f7f7f7")
(defconst financial-chart-matrix--positive-color "#b2182b")
(defconst financial-chart-matrix-max-bins 512
  "Maximum number of volume-profile price bins per render.")

(defun financial-chart-matrix--sequence-p (value)
  "Whether VALUE is a finite list or a non-string vector."
  (or (proper-list-p value)
      (and (vectorp value) (not (stringp value)))))

(defun financial-chart-matrix--validate (data)
  "Signal unless DATA is a labeled rectangular numeric matrix."
  (unless (and (proper-list-p data) (cl-evenp (length data))
               (plist-member data :labels) (plist-member data :rows))
    (financial-chart--invalid 0 "expected (:labels LABELS :rows ROWS)"))
  (let ((labels (plist-get data :labels))
        (column-labels (plist-get data :column-labels))
        (rows (plist-get data :rows)))
    (unless (and (financial-chart-matrix--sequence-p labels)
                 (> (length labels) 0))
      (financial-chart--invalid 0 ":labels must be a non-empty list or vector"))
    (cl-loop for label in (append labels nil)
             for label-index from 0
             unless (or (stringp label) (symbolp label) (numberp label))
             do (financial-chart--invalid label-index
                                          "matrix labels must be strings, symbols or numbers"))
    (unless (and (financial-chart-matrix--sequence-p rows)
                 (= (length rows) (length labels)))
      (financial-chart--invalid 0 ":rows must contain one row per label"))
    (unless (and (financial-chart-matrix--sequence-p (car (append rows nil)))
                 (> (length (car (append rows nil))) 0))
      (financial-chart--invalid 0 "matrix rows must contain at least one numeric value"))
    (let ((column-count (length (car (append rows nil)))))
      (when column-labels
        (unless (and (financial-chart-matrix--sequence-p column-labels)
                     (= (length column-labels) column-count))
          (financial-chart--invalid 0 ":column-labels must contain one label per column"))
        (cl-loop for label in (append column-labels nil)
                 for label-index from 0
                 unless (or (stringp label) (symbolp label) (numberp label))
                 do (financial-chart--invalid label-index
                                              "column labels must be strings, symbols or numbers")))
    (cl-loop for row in (append rows nil)
             for row-index from 0
             do
      (unless (and (financial-chart-matrix--sequence-p row)
                   (= (length row) column-count))
        (financial-chart--invalid row-index "row must contain %d values" column-count))
      (cl-loop for value in (append row nil)
               for column-index from 0
               do
        (unless (numberp value)
          (financial-chart--invalid row-index "cell %d must be numeric, got %S"
                                    column-index value)))))))

(defun financial-chart-matrix--values (data _props)
  "Every cell value in matrix DATA, for summaries."
  (apply #'append (mapcar (lambda (row) (append row nil))
                          (append (plist-get data :rows) nil))))

(defun financial-chart-matrix--from-json (data)
  "JSON-parsed matrix DATA (labels, rows, column_labels) as a matrix plist."
  (append (list :labels (alist-get 'labels data))
          (when (alist-get 'column_labels data)
            (list :column-labels (alist-get 'column_labels data)))
          (list :rows (alist-get 'rows data))))

(defun financial-chart-matrix--to-json (data)
  "Matrix DATA as a JSON object."
  (append `((labels . ,(vconcat (plist-get data :labels)))
            (rows . ,(vconcat (mapcar #'vconcat (plist-get data :rows)))))
          (when (plist-get data :column-labels)
            `((column_labels . ,(vconcat (plist-get data :column-labels)))))))

(setf (alist-get 'matrix financial-chart-shapes)
      '(:doc "Numeric matrix: (:labels (ROW-LABEL ...) :rows ((VALUE ...) ...));
optional :column-labels names columns, otherwise square matrices reuse :labels.
JSON: {\"labels\": [...], \"rows\": [[...], ...], \"column_labels\": [...]}."
        :example (:labels ("SPY" "QQQ" "TLT")
                  :rows ((1.0 0.82 -0.12) (0.82 1.0 -0.08) (-0.12 -0.08 1.0)))
        :validator financial-chart-matrix--validate
        :values financial-chart-matrix--values
        :from-json financial-chart-matrix--from-json
        :to-json financial-chart-matrix--to-json))

(defun financial-chart-matrix--label (label)
  "LABEL as a display string with terminal and XML control chars removed."
  (replace-regexp-in-string "[[:cntrl:]]" "" (format "%s" label) t t))

(defun financial-chart-matrix--range (values)
  "Return the display scale (LOW . HIGH) for matrix VALUES."
  (let ((low (apply #'min values))
        (high (apply #'max values)))
    (cond
     ((and (>= low -1) (<= high 1)) (cons -1.0 1.0))
     ((= low high) (cons (- low 0.5) (+ high 0.5)))
     (t (cons low high)))))

(defun financial-chart-matrix--center (range)
  "Center value of RANGE."
  (/ (+ (car range) (cdr range)) 2.0))

(defun financial-chart-matrix--glyph (value range)
  "A shaded block for VALUE on RANGE, with intensity by distance from center."
  (let* ((center (financial-chart-matrix--center range))
         (extent (max (- center (car range)) (- (cdr range) center)))
         (magnitude (if (= value center) 0
                      (min 1.0 (/ (abs (- value center)) extent))))
         (level (if (= magnitude 0) 0 (min 4 (max 1 (round (* magnitude 4))))))
         (glyph (aref "·░▒▓█" level)))
    (cons glyph (cond ((< value center) 'down)
                      ((> value center) 'up)
                      (t 'dim)))))

(defun financial-chart-matrix--pad (text width &optional center)
  "Pad TEXT to display WIDTH columns, optionally centered."
  (let* ((text (truncate-string-to-width text width nil nil "…"))
         (padding (max 0 (- width (string-width text)))))
    (if center
        (concat (make-string (/ padding 2) ?\s) text
                (make-string (- padding (/ padding 2)) ?\s))
      (concat text (make-string padding ?\s)))))

(cl-defun financial-chart-matrix-text
    (data &key (width 54) title (up-face 'financial-chart-up)
          (down-face 'financial-chart-down) (dim-face 'financial-chart-dim)
          (accent-face 'financial-chart-accent) &allow-other-keys)
  "Render labeled numeric matrix DATA as a shaded text heatmap.
WIDTH is the approximate cell-grid width.  Cell shade shows distance
from the scale center; DOWN-FACE and UP-FACE distinguish its sides."
  (when data
    (let* ((title (and title (financial-chart-matrix--label title)))
           (labels (mapcar #'financial-chart-matrix--label
                           (append (plist-get data :labels) nil)))
           (rows (append (plist-get data :rows) nil))
           (column-count (length (car rows)))
           (column-labels
            (mapcar #'financial-chart-matrix--label
                    (append (or (plist-get data :column-labels)
                                (if (= (length labels) column-count)
                                    labels
                                  (number-sequence 1 column-count))) nil)))
           (values (apply #'append (mapcar (lambda (r) (append r nil)) rows)))
           (range (financial-chart-matrix--range values))
           (center (financial-chart-matrix--center range))
           (n (length column-labels))
           (label-width (apply #'max (mapcar #'string-width labels)))
           (cell-width (max 1 (min 8 (/ (max n width) n))))
           (prefix (concat (make-string label-width ?\s) "  "))
           (header (concat prefix
                           (mapconcat (lambda (label)
                                        (financial-chart-matrix--pad label cell-width t))
                                      column-labels " ")))
           (lines
            (cl-loop for label in labels
                     for row in rows
                     collect
                     (concat (financial-chart-matrix--pad label label-width) "  "
                             (mapconcat
                              (lambda (value)
                                (let* ((cell (financial-chart-matrix--glyph value range))
                                       (number (financial-chart-fmt value))
                                       (label (if (<= (+ 1 (string-width number)) cell-width)
                                                  (concat (string (car cell)) number)
                                                (string (car cell))))
                                       (face (pcase (cdr cell)
                                               ('down down-face) ('up up-face) (_ dim-face))))
                                  (propertize (financial-chart-matrix--pad
                                               label cell-width t)
                                              'face face)))
                              (append row nil) " "))))
           (legend (concat
                    "Legend: "
                    (propertize "−" 'face down-face) " below "
                    (propertize "·" 'face dim-face) " " (financial-chart-fmt center) " "
                    (propertize "+" 'face up-face) " above; shades show distance from center"
                    " (" (financial-chart-fmt (car range)) "…"
                    (financial-chart-fmt (cdr range)) ")"))
           (body (mapconcat #'string-trim-right (cons header lines) "\n")))
      (concat (when title (concat title "\n")) body "\n"
              (propertize legend 'face accent-face)))))

(defun financial-chart-matrix--hex-rgb (color)
  "COLOR as three 0..255 RGB components."
  (list (string-to-number (substring color 1 3) 16)
        (string-to-number (substring color 3 5) 16)
        (string-to-number (substring color 5 7) 16)))

(defun financial-chart-matrix--mix-color (a b fraction)
  "Interpolate hexadecimal colors A to B by FRACTION in [0,1]."
  (let ((left (financial-chart-matrix--hex-rgb a))
        (right (financial-chart-matrix--hex-rgb b)))
    (concat "#" (cl-loop for x in left
                         for y in right
                         concat (format "%02x" (round (+ x (* fraction (- y x)))))))))

(defun financial-chart-matrix--color (value range)
  "Diverging palette color for VALUE on RANGE."
  (let ((center (financial-chart-matrix--center range)))
    (if (<= value center)
        (financial-chart-matrix--mix-color
         financial-chart-matrix--negative-color financial-chart-matrix--neutral-color
         (/ (float (- value (car range))) (- center (car range))))
      (financial-chart-matrix--mix-color
       financial-chart-matrix--neutral-color financial-chart-matrix--positive-color
       (/ (float (- value center)) (- (cdr range) center))))))

(defun financial-chart-matrix--contrast (color)
  "A readable foreground color for COLOR."
  (let* ((rgb (financial-chart-matrix--hex-rgb color))
         (luminance (+ (* 0.299 (nth 0 rgb))
                       (* 0.587 (nth 1 rgb))
                       (* 0.114 (nth 2 rgb)))))
    (if (< luminance 145) "#ffffff" "#222222")))

(cl-defun financial-chart-matrix-svg
    (data &key (width 600) (height 360) title &allow-other-keys)
  "Render labeled numeric matrix DATA as an SVG heatmap, WIDTH x HEIGHT pixels.
Values within -1..1 use that fixed scale; other matrices use their
observed minimum and maximum."
  (when data
    (let* ((title (and title (financial-chart-matrix--label title)))
           (labels (mapcar #'financial-chart-matrix--label
                           (append (plist-get data :labels) nil)))
           (rows (append (plist-get data :rows) nil))
           (column-count (length (car rows)))
           (column-labels
            (mapcar #'financial-chart-matrix--label
                    (append (or (plist-get data :column-labels)
                                (if (= (length labels) column-count)
                                    labels
                                  (number-sequence 1 column-count))) nil)))
           (values (apply #'append (mapcar (lambda (r) (append r nil)) rows)))
           (range (financial-chart-matrix--range values))
           (row-count (length labels))
           (left (min (max 0 (1- width))
                      (+ 18 (* 7 (apply #'max (mapcar #'string-width labels))))))
           (top 48)
           (legend-h 46)
           (cell-width (min 140 (/ (float (max 1 (- width left 12))) column-count)))
           (cell-height (min 48 (/ (float (max 1 (- height top legend-h 10))) row-count)))
           (grid-width (* column-count cell-width))
           (grid-height (* row-count cell-height))
           (svg (financial-chart-svg--canvas width height title))
           (grid-x left)
           (grid-y top))
      (cl-loop for label in column-labels
               for column from 0
               do (financial-chart-svg--text
                   svg (truncate-string-to-width label 12 nil nil "…")
                   (+ grid-x (* column cell-width) (/ cell-width 2.0)) (- grid-y 7) "middle"))
      (cl-loop for label in labels
               for row-index from 0
               do (financial-chart-svg--text
                   svg label (- grid-x 8)
                   (+ grid-y (* row-index cell-height) (/ cell-height 2.0) 4) "end"))
      (cl-loop for row in rows
               for row-index from 0
               do (cl-loop for value in (append row nil)
                           for column-index from 0
                           for color = (financial-chart-matrix--color value range)
                           for x = (+ grid-x (* column-index cell-width))
                           for y = (+ grid-y (* row-index cell-height))
                           do (svg-rectangle svg x y cell-width cell-height
                                             :fill color :stroke "#ffffff" :stroke-width 1)
                           when (>= cell-width 38)
                           do (financial-chart-svg--text
                               svg (financial-chart-fmt value)
                               (+ x (/ cell-width 2.0))
                               (+ y (/ cell-height 2.0) 4) "middle"
                               (financial-chart-matrix--contrast color))))
      (let* ((legend-y (+ grid-y grid-height 28))
             (legend-x grid-x)
             (legend-width (max 60 (min 180 grid-width))))
        (cl-loop for i from 0 below 48
                 for fraction = (/ i 47.0)
                 for value = (+ (car range) (* fraction (- (cdr range) (car range))))
                 do (svg-rectangle svg (+ legend-x (* legend-width fraction)) legend-y
                                  (/ legend-width 48.0) 8
                                  :fill (financial-chart-matrix--color value range)))
        (financial-chart-svg--text svg (financial-chart-fmt (car range))
                                   legend-x (+ legend-y 23) "start")
        (financial-chart-svg--text svg (financial-chart-fmt (financial-chart-matrix--center range))
                                   (+ legend-x (/ legend-width 2.0)) (+ legend-y 23) "middle")
        (financial-chart-svg--text svg (financial-chart-fmt (cdr range))
                                   (+ legend-x legend-width) (+ legend-y 23) "end"))
      (financial-chart-svg--string svg))))

(defun financial-chart-matrix--volume-data (bars bins)
  "Aggregate OHLCV BARS into BINS price intervals."
  (unless (and (integerp bins) (> bins 0)
               (<= bins financial-chart-matrix-max-bins))
    (signal 'financial-chart-error
            (list (format ":bins must be an integer from 1 to %d"
                          financial-chart-matrix-max-bins)
                  :code "invalid_bins")))
  (when bars
    (let* ((raw-low (apply #'min (mapcar (lambda (bar) (plist-get bar :low)) bars)))
           (raw-high (apply #'max (mapcar (lambda (bar) (plist-get bar :high)) bars)))
           (flat (= raw-low raw-high))
           (low (if flat (- raw-low 0.5) raw-low))
           (high (if flat (+ raw-high 0.5) raw-high))
           (step (/ (- high low) (float bins)))
           (volumes (make-vector bins 0.0)))
      (cl-loop for bar in bars
               for bar-index from 0
               for bar-low = (min (plist-get bar :low) (plist-get bar :high))
               for bar-high = (max (plist-get bar :low) (plist-get bar :high))
               for volume = (or (plist-get bar :volume) 0)
               do (unless (and (numberp volume) (>= volume 0))
                    (financial-chart--invalid bar-index
                                              ":volume must be a non-negative number when present"))
               do (if (= bar-low bar-high)
                      (let ((index (min (1- bins)
                                        (max 0 (floor (/ (- bar-low low) step))))))
                        (aset volumes index (+ (aref volumes index) volume)))
                    (cl-loop for index from 0 below bins
                             for bin-low = (+ low (* index step))
                             for bin-high = (+ bin-low step)
                             for overlap = (max 0 (- (min bar-high bin-high)
                                                    (max bar-low bin-low)))
                             when (> overlap 0)
                             do (aset volumes index
                                      (+ (aref volumes index)
                                         (* volume (/ overlap (- bar-high bar-low))))))))
      (list :low low :high high :step step :volumes (append volumes nil)
            :poc (and (> (apply #'+ (append volumes nil)) 0)
                      (cl-position (apply #'max (append volumes nil)) volumes))
            :last-close (plist-get (car (last bars)) :close)))))

(defun financial-chart-matrix--volume-bin (value low step bins)
  "Bin index for VALUE given LOW, STEP and BINS."
  (min (1- bins) (max 0 (floor (/ (- value low) step)))))

(cl-defun financial-chart-volume-profile-text
    (bars &key (bins 24) (width 24) (unit "") title
          (up-face 'financial-chart-up) (down-face 'financial-chart-down)
          (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
          &allow-other-keys)
  "Render OHLCV BARS as a horizontal volume profile.
BINS is the number of price levels, WIDTH is the maximum bar width, and
UNIT is appended to price labels.  BINS ranges from 1 to
`financial-chart-matrix-max-bins'.  P marks point of control; C marks the
last close's bin.  This OHLCV estimate spreads each bar's volume uniformly
across its low-high range; it does not use trade-level volume data."
  (when bars
    (let* ((title (and title (financial-chart-matrix--label title)))
           (unit (financial-chart-matrix--label unit))
           (profile (financial-chart-matrix--volume-data bars bins))
           (low (plist-get profile :low))
           (step (plist-get profile :step))
           (volumes (plist-get profile :volumes))
           (max-volume (apply #'max volumes))
           (poc (plist-get profile :poc))
           (last-close (plist-get profile :last-close))
           (close-bin (and (numberp last-close)
                           (financial-chart-matrix--volume-bin last-close low step bins)))
           (label-width (apply #'max
                               (mapcar (lambda (i)
                                         (string-width
                                          (concat unit
                                                  (financial-chart-fmt
                                                   (+ low (* (+ i 0.5) step))))))
                                       (number-sequence 0 (1- bins)))))
           (body
            (cl-loop for index downfrom (1- bins) to 0
                     for volume = (nth index volumes)
                     for count = (if (= max-volume 0) 0
                                   (round (* width (/ volume max-volume))))
                     for marker = (concat (if (and poc (= index poc)) "P" " ")
                                          (if (and close-bin (= index close-bin)) "C" " "))
                     for price = (+ low (* (+ index 0.5) step))
                     for face = (if (and poc (= index poc)) accent-face up-face)
                     collect
                     (concat (propertize (financial-chart-matrix--pad marker 2)
                                         'face (if (string-match-p "P" marker)
                                                   accent-face
                                                 (if (string-match-p "C" marker)
                                                     down-face dim-face)))
                             (financial-chart-matrix--pad (concat unit
                                                                  (financial-chart-fmt price))
                                                          label-width)
                             " │" (propertize (make-string count ?█) 'face face)))))
      (concat (when title (concat title "\n"))
              (mapconcat #'identity body "\n") "\n"
              (propertize (concat (if poc "P = point of control"
                                    "POC unavailable: no positive volume")
                                  "   C = last close\n"
                                  "OHLCV estimate: volume is spread uniformly across each bar's low-high range")
                          'face dim-face)))))

(cl-defun financial-chart-volume-profile-svg
    (bars &key (bins 24) (width 600) (height 420) (unit "") title
          &allow-other-keys)
  "Render OHLCV BARS as a volume-profile SVG, WIDTH x HEIGHT pixels.
BINS is 1 to `financial-chart-matrix-max-bins'; UNIT is appended to
price labels. The displayed profile is estimated from OHLCV bars."
  (when bars
    (let* ((title (and title (financial-chart-matrix--label title)))
           (unit (financial-chart-matrix--label unit))
           (profile (financial-chart-matrix--volume-data bars bins))
           (low (plist-get profile :low))
           (high (plist-get profile :high))
           (step (plist-get profile :step))
           (volumes (plist-get profile :volumes))
           (max-volume (max 1.0 (apply #'max volumes)))
           (poc (plist-get profile :poc))
           (last-close (plist-get profile :last-close))
           (last-close-y (and (numberp last-close)
                              (+ 34 (* (- height 58)
                                       (- 1.0 (max 0.0
                                                  (min 1.0
                                                       (/ (float (- last-close low))
                                                          (- high low)))))))))
           (left 82)
           (right 100)
           (top 34)
           (bottom 24)
           (plot-width (max 10 (- width left right)))
           (plot-height (max 10 (- height top bottom)))
           (row-height (/ plot-height (float bins)))
           (svg (financial-chart-svg--canvas width height title)))
      (financial-chart-svg--text
       svg "OHLCV estimate: volume is spread uniformly across each bar's low-high range"
       8 30 "start")
      (cl-loop for index downfrom (1- bins) to 0
               for volume = (nth index volumes)
               for row = (- (1- bins) index)
               for bar-width = (* plot-width (/ volume max-volume))
               for y = (+ top (* row row-height))
               for price = (+ low (* (+ index 0.5) step))
               for label-step = (max 1 (ceiling (/ 14.0 row-height)))
               when (= (% index label-step) 0)
               do (financial-chart-svg--text
                   svg (concat unit (financial-chart-fmt price)) 5 (+ y (/ row-height 2.0) 4) "start")
               do (svg-rectangle svg left y bar-width (max 1 (- row-height 1))
                                 :fill (if (and poc (= index poc))
                                           "#7b3294" "#4393c3"))
               when (and poc (= index poc))
               do (financial-chart-svg--text svg "POC" (+ left bar-width 6)
                                             (+ y (/ row-height 2.0) 4) "start" "#7b3294"))
      (when last-close-y
        (svg-line svg left last-close-y (+ left plot-width) last-close-y
                  :stroke "#c62828" :stroke-width 1.5)
        (financial-chart-svg--text svg
                                   (concat "Close " unit
                                           (financial-chart-fmt last-close))
                                   (+ left plot-width 8) last-close-y "start" "#c62828"))
      (financial-chart-matrix--svg-legend svg left (- height 18)
                                          (if poc "POC" "POC unavailable") "#7b3294"
                                          "Last close" "#c62828")
      (financial-chart-svg--string svg))))

(defun financial-chart-matrix--svg-legend (svg x y first first-color second second-color)
  "Draw two labeled color swatches into SVG at X,Y."
  (svg-rectangle svg x y 10 10 :fill first-color)
  (financial-chart-svg--text svg first (+ x 15) (+ y 9) "start")
  (svg-line svg (+ x 70) (+ y 5) (+ x 86) (+ y 5)
            :stroke second-color :stroke-width 2)
  (financial-chart-svg--text svg second (+ x 91) (+ y 9) "start"))

(financial-chart-register-kind
 'heatmap :shape 'matrix :text 'financial-chart-matrix-text
 :svg 'financial-chart-matrix-svg
 :doc "Diverging heatmap for a labeled numeric matrix.")
(financial-chart-register-kind
 'volume-profile :shape 'ohlc :text 'financial-chart-volume-profile-text
 :svg 'financial-chart-volume-profile-svg
 :doc "Estimated OHLCV volume by price level, with point of control and last close.")

(provide 'financial-chart-matrix)
;;; financial-chart-matrix.el ends here
