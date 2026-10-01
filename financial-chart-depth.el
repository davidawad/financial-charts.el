;;; financial-chart-depth.el --- Order-book depth charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;;; Commentary:

;; Order-book depth as a text ladder, cumulative text bars, or a classic
;; cumulative SVG plot.  This module owns the order-book shape and kind.

;;; Code:

(require 'cl-lib)
(require 'financial-chart-plot)

(defun financial-chart-depth--invalid-level (side index fmt &rest args)
  "Signal invalid order-book level INDEX on SIDE, with reason FMT/ARGS."
  (financial-chart--invalid index "%s level: %s" side (apply #'format fmt args)))

(defun financial-chart-depth--validate-order-book (book)
  "Signal unless BOOK has positive levels and a non-crossed best bid/ask."
  (unless (and (listp book) (proper-list-p book)
               (cl-evenp (length book))
               (plist-member book :bids) (plist-member book :asks))
    (signal 'financial-chart-invalid-data
            (list "order book must be a plist with :bids and :asks lists"
                  :code "invalid_data")))
  (dolist (side '(:bids :asks))
    (let ((levels (plist-get book side)))
      (unless (and (listp levels) (proper-list-p levels))
        (signal 'financial-chart-invalid-data
                (list (format "%s must be a list of (PRICE SIZE) pairs"
                              (substring (symbol-name side) 1))
                      :code "invalid_data")))
      (cl-loop for level in levels for index from 0
               do (unless (and (listp level) (proper-list-p level)
                               (= (length level) 2))
                    (financial-chart-depth--invalid-level
                     (substring (symbol-name side) 1) index
                     "expected (PRICE SIZE), got %S" level))
               do (let ((price (car level))
                        (size (cadr level)))
                    (unless (and (numberp price) (> price 0))
                      (financial-chart-depth--invalid-level
                       (substring (symbol-name side) 1) index
                       "price must be a positive number, got %S" price))
                    (unless (and (numberp size) (> size 0))
                      (financial-chart-depth--invalid-level
                       (substring (symbol-name side) 1) index
                       "size must be a positive number, got %S" size))))))
  (let* ((bids (plist-get book :bids))
         (best-bid (car (financial-chart-depth--sorted-levels book :bids 1)))
         (best-ask (car (financial-chart-depth--sorted-levels book :asks 1))))
    (when (and best-bid best-ask (> (car best-bid) (car best-ask)))
      (financial-chart-depth--invalid-level
       "bids" (cl-position best-bid bids :test #'eq)
       "best bid %s exceeds best ask %s; correct the crossed order-book prices"
       (car best-bid) (car best-ask)))
    t))

(defun financial-chart-depth--sorted-levels (book side &optional limit)
  "Nearest levels on SIDE from BOOK, capped at LIMIT when non-nil."
  (let* ((descending (eq side :bids))
         (levels (sort (copy-sequence (plist-get book side))
                       (lambda (a b)
                         (if descending (> (car a) (car b))
                           (< (car a) (car b)))))))
    (if limit
        (cl-subseq levels 0 (min limit (length levels)))
      levels)))

(defun financial-chart-depth--cumulative-levels (levels)
  "LEVELS in nearest-first order as (PRICE SIZE CUMULATIVE-SIZE) records."
  (let ((total 0))
    (mapcar (lambda (level)
              (setq total (+ total (cadr level)))
              (list (car level) (cadr level) total))
            levels)))

(defun financial-chart-depth--number (number)
  "NUMBER as a compact decimal string without changing its precision."
  (let ((text (number-to-string number)))
    (if (string-match "\\([0-9]+\\)\\.0\\'" text)
        (match-string 1 text)
      text)))

(defun financial-chart-depth--price (price unit)
  "PRICE prefixed by UNIT, if present; discard control characters."
  (let ((prefix (or unit "")))
    (concat (if (stringp prefix)
                (replace-regexp-in-string "[[:cntrl:]]" "" prefix)
              prefix)
            (financial-chart-depth--number price))))

(defun financial-chart-depth--summary (book unit)
  "Mid-price or one-sided quote and spread label plist for BOOK."
  (let* ((bids (financial-chart-depth--sorted-levels book :bids 1))
         (asks (financial-chart-depth--sorted-levels book :asks 1))
         (bid (caar bids))
         (ask (caar asks)))
    (when (or bid ask)
      (if (and bid ask)
          (let* ((mid (/ (+ bid ask) 2.0))
                 (spread (- ask bid))
                 (percent (/ (* 100.0 spread) mid)))
            (list :mid mid :spread spread
                  :text (format "mid %s | spread %s (%.2f%%)"
                                (financial-chart-depth--price mid unit)
                                (financial-chart-depth--price spread unit)
                                percent)))
        (let ((side (if bid "best bid" "best ask"))
              (price (or bid ask)))
          (list :mid nil :reference price :spread nil
                :text (format "%s %s | mid n/a | spread n/a"
                              side (financial-chart-depth--price price unit))))))))

(defun financial-chart-depth--pad (text width &optional left)
  "Pad TEXT to display WIDTH, on the right or on the LEFT when LEFT."
  (let ((padding (make-string (max 0 (- width (string-width text))) ?\s)))
    (if left (concat padding text) (concat text padding))))

(defun financial-chart-depth--midline (text width face)
  "A WIDTH-column separator around TEXT, colored with FACE."
  (let* ((remaining (max 2 (- width (string-width text))))
         (left (/ remaining 2))
         (right (- remaining left)))
    (propertize (concat (make-string left ?─) " " text " "
                        (make-string right ?─))
                'face face)))

(defun financial-chart-depth--text-header (bar-width price-width size-width size-title accent-face)
  "The table header for BAR-WIDTH, PRICE-WIDTH and SIZE-WIDTH columns."
  (propertize
   (concat "  " (financial-chart-depth--pad "DEPTH" bar-width)
           "  " (financial-chart-depth--pad "PRICE" price-width t)
           "  " (financial-chart-depth--pad size-title size-width t) "\n")
   'face accent-face))

(defun financial-chart-depth--text-ladder-row
    (level max-size bar-width price-width size-width unit face dim-face)
  "Render one ladder LEVEL with sqrt-scaled depth and fixed columns."
  (let* ((price (financial-chart-depth--price (car level) unit))
         (size (financial-chart-depth--number (cadr level)))
         (bar (financial-chart-depth-bar (cadr level) max-size bar-width face)))
    (concat "  " bar (make-string (max 0 (- bar-width (string-width bar))) ?\s)
            "  " (propertize (financial-chart-depth--pad price price-width t)
                             'face dim-face)
            "  " (propertize (financial-chart-depth--pad size size-width t)
                             'face dim-face) "\n")))

(defun financial-chart-depth--text-cumulative-row
    (level max-cumulative bar-width price-width size-width unit face dim-face)
  "Render cumulative LEVEL as a linear horizontal bar."
  (let* ((price (financial-chart-depth--price (car level) unit))
         (total (nth 2 level))
         (quantity (financial-chart-depth--number total))
         (columns (max 1 (round (* bar-width (/ total (float max-cumulative))))))
         (bar (propertize (make-string columns ?█) 'face face)))
    (concat "  " (propertize (financial-chart-depth--pad price price-width t)
                             'face dim-face)
            "  " bar (make-string (max 0 (- bar-width columns)) ?\s)
            "  " (propertize (financial-chart-depth--pad quantity size-width t)
                             'face dim-face) "\n")))

(defun financial-chart-depth--text-ladder
    (book asks bids width unit up-face down-face dim-face accent-face)
  "Render the BOOK ladder with visible ASKS and BIDS."
  (let* ((price-values (append (mapcar #'car asks) (mapcar #'car bids)))
         (prices (mapcar (lambda (price)
                           (financial-chart-depth--price price unit))
                         price-values))
         (sizes (mapcar (lambda (level)
                          (financial-chart-depth--number (cadr level)))
                        (append asks bids)))
         (summary (plist-get (financial-chart-depth--summary book unit) :text))
         (price-width (apply #'max (cons 5 (mapcar #'string-width prices))))
         (size-width (apply #'max (cons 4 (mapcar #'string-width sizes))))
         (bar-width (max 1 (- width price-width size-width 10)))
         (max-size (apply #'max (cons 1 (mapcar #'cadr (append asks bids))))))
    (concat
     (propertize "ASKS\n" 'face down-face)
     (financial-chart-depth--text-header bar-width price-width size-width "SIZE" accent-face)
     (if asks
         (mapconcat (lambda (level)
                      (financial-chart-depth--text-ladder-row
                       level max-size bar-width price-width size-width unit down-face dim-face))
                    (reverse asks) "")
       (propertize "  (no asks)\n" 'face dim-face))
     (financial-chart-depth--midline summary width dim-face) "\n"
     (propertize "BIDS\n" 'face up-face)
     (financial-chart-depth--text-header bar-width price-width size-width "SIZE" accent-face)
     (if bids
         (mapconcat (lambda (level)
                      (financial-chart-depth--text-ladder-row
                       level max-size bar-width price-width size-width unit up-face dim-face))
                    bids "")
       (propertize "  (no bids)\n" 'face dim-face)))))

(defun financial-chart-depth--text-cumulative
    (book asks bids width unit up-face down-face dim-face accent-face)
  "Render cumulative text depth rows, with ASKS above and BIDS below mid."
  (let* ((ask-data (reverse (financial-chart-depth--cumulative-levels asks)))
         (bid-data (financial-chart-depth--cumulative-levels bids))
         (data (append ask-data bid-data))
         (prices (mapcar (lambda (level)
                           (financial-chart-depth--price (car level) unit)) data))
         (quantities (mapcar (lambda (level)
                              (financial-chart-depth--number (nth 2 level))) data))
         (price-width (apply #'max (cons 5 (mapcar #'string-width prices))))
         (size-width (apply #'max (cons 8 (mapcar #'string-width quantities))))
         (bar-width (max 1 (- width price-width size-width 10)))
         (max-cumulative (apply #'max (cons 1 (mapcar (lambda (level) (nth 2 level)) data))))
         (summary (plist-get (financial-chart-depth--summary book unit) :text)))
    (concat
     (propertize "ASK CUMULATIVE DEPTH\n" 'face down-face)
     (propertize
      (concat "  " (financial-chart-depth--pad "PRICE" price-width t)
              "  " (financial-chart-depth--pad "DEPTH" bar-width)
              "  " (financial-chart-depth--pad "CUM SIZE" size-width t) "\n")
      'face accent-face)
     (if ask-data
         (mapconcat (lambda (level)
                      (financial-chart-depth--text-cumulative-row
                       level max-cumulative bar-width price-width size-width unit
                       down-face dim-face))
                    ask-data "")
       (propertize "  (no asks)\n" 'face dim-face))
     (financial-chart-depth--midline summary width dim-face) "\n"
     (propertize "BID CUMULATIVE DEPTH\n" 'face up-face)
     (propertize
      (concat "  " (financial-chart-depth--pad "PRICE" price-width t)
              "  " (financial-chart-depth--pad "DEPTH" bar-width)
              "  " (financial-chart-depth--pad "CUM SIZE" size-width t) "\n")
      'face accent-face)
     (if bid-data
         (mapconcat (lambda (level)
                      (financial-chart-depth--text-cumulative-row
                       level max-cumulative bar-width price-width size-width unit
                       up-face dim-face))
                    bid-data "")
       (propertize "  (no bids)\n" 'face dim-face)))))

(cl-defun financial-chart-text-depth
    (book &key (style 'ladder) (width 60) (height 10) (unit "")
          (up-face 'financial-chart-up) (down-face 'financial-chart-down)
          (dim-face 'financial-chart-dim) (accent-face 'financial-chart-accent)
          &allow-other-keys)
  "Render order BOOK as a depth ladder or cumulative-depth text chart.
STYLE is ladder by default, with asks above a mid/spread line and bids
below; cumulative draws linear cumulative-size bars.  WIDTH is the
target line width.  HEIGHT limits visible levels on each side to half
that many rows.  UNIT prefixes prices and spreads; control characters
in UNIT are discarded.  Ladder bars use financial-chart-depth-bar
square-root scaling."
  (let* ((limit (max 1 (/ height 2)))
         (asks (financial-chart-depth--sorted-levels book :asks limit))
         (bids (financial-chart-depth--sorted-levels book :bids limit)))
    (when (or asks bids)
      (pcase style
        ('ladder (financial-chart-depth--text-ladder
                  book asks bids width unit up-face down-face dim-face accent-face))
        ('cumulative (financial-chart-depth--text-cumulative
                      book asks bids width unit up-face down-face dim-face accent-face))
        (_ (signal 'financial-chart-error
                   (list (format "depth style must be ladder or cumulative, got %S" style)
                         :code "invalid_style" :style style)))))))

(defun financial-chart-depth--svg-side-points
    (levels side center-x center-y half-width max-cumulative price-y)
  "Return a stepped curve for LEVELS on SIDE around the chart center."
  (when levels
    (let ((x center-x)
          (curve (list (cons center-x center-y))))
      (dolist (level levels)
        (let* ((y (funcall price-y (car level)))
               (fraction (/ (nth 2 level) (float max-cumulative)))
               (next-x (if (eq side :bids)
                           (- center-x (* half-width fraction))
                         (+ center-x (* half-width fraction)))))
          (setq curve (append curve (list (cons x y) (cons next-x y)))
                x next-x)))
      curve)))

(cl-defun financial-chart-svg-depth
    (book &key (width 600) (height 240) (unit "$") title &allow-other-keys)
  "Render BOOK as a classic cumulative SVG depth chart.
Bid levels step down-left in the up color; ask levels step up-right in
the down color.  The annotation reports the mid price and spread."
  (let* ((bids (financial-chart-depth--cumulative-levels
                (financial-chart-depth--sorted-levels book :bids)))
         (asks (financial-chart-depth--cumulative-levels
                (financial-chart-depth--sorted-levels book :asks)))
         (all (append bids asks)))
    (when all
      (let* ((summary (financial-chart-depth--summary book unit))
             (reference (or (plist-get summary :mid)
                            (plist-get summary :reference)))
             (raw-prices (mapcar #'car all))
             (raw-low (min reference (apply #'min raw-prices)))
             (raw-high (max reference (apply #'max raw-prices)))
             (raw-span (- raw-high raw-low))
             (padding (if (> raw-span 0)
                          (* raw-span 0.05)
                        (max 0.01 (* (abs reference) 0.01))))
             (price-low (- raw-low padding))
             (price-high (+ raw-high padding))
             (price-span (- price-high price-low))
             (max-cumulative (apply #'max (mapcar (lambda (level) (nth 2 level)) all)))
             (top-label (financial-chart-depth--price price-high unit))
             (bottom-label (financial-chart-depth--price price-low unit))
             (axis-label-width (max (string-width top-label) (string-width bottom-label)))
             (frame (financial-chart-svg--frame width height title))
             (x0 (max (nth 0 frame)
                      (+ 12 (* axis-label-width (* 0.6 financial-chart-svg-font-size)))))
             (y0 (nth 1 frame))
             (plot-width (max 2 (- width x0 financial-chart-svg-margin-right)))
             (plot-height (max 20 (- (nth 3 frame) 30)))
             (center-x (+ x0 (/ plot-width 2.0)))
             (center-y (+ y0 (* plot-height
                                (- 1.0 (/ (- reference price-low) price-span)))))
             (price-y (lambda (price)
                        (financial-chart-svg--n
                         (+ y0 (* plot-height
                                  (- 1.0 (/ (- price price-low) price-span)))))))
             (bid-points (financial-chart-depth--svg-side-points
                          bids :bids center-x center-y (/ plot-width 2.0)
                          max-cumulative price-y))
             (ask-points (financial-chart-depth--svg-side-points
                          asks :asks center-x center-y (/ plot-width 2.0)
                          max-cumulative price-y))
             (svg (financial-chart-svg--canvas width height title))
             (bottom (+ y0 plot-height))
             (bid-color (financial-chart-svg--color 'up))
             (ask-color (financial-chart-svg--color 'down))
             (grid-color (financial-chart-svg--color 'grid)))
        (svg-line svg x0 y0 x0 bottom :stroke grid-color)
        (svg-line svg x0 bottom (+ x0 plot-width) bottom :stroke grid-color)
        (svg-line svg center-x y0 center-x bottom :stroke grid-color)
        (svg-line svg x0 center-y (+ x0 plot-width) center-y
                  :stroke grid-color :stroke-dasharray "4 3")
        (financial-chart-svg--horizontal-ticks
         svg (mapcar (lambda (price)
                       (list (funcall price-y price)
                             (financial-chart-depth--price price unit)))
                     (financial-chart--axis-label-values price-low price-high 5))
         x0 (+ x0 plot-width))
        (financial-chart-svg--vertical-ticks
         svg `((0 ,(financial-chart-depth--number max-cumulative))
               (0.5 "0")
               (1 ,(financial-chart-depth--number max-cumulative)))
         x0 y0 plot-width plot-height)
        (when bid-points
          (svg-polygon svg (append bid-points
                                   (list (cons (caar (last bid-points)) center-y)))
                       :fill bid-color :fill-opacity 0.2 :stroke "none")
          (svg-polyline svg bid-points :fill "none" :stroke bid-color :stroke-width 2))
        (when ask-points
          (svg-polygon svg (append ask-points
                                   (list (cons (caar (last ask-points)) center-y)))
                       :fill ask-color :fill-opacity 0.2 :stroke "none")
          (svg-polyline svg ask-points :fill "none" :stroke ask-color :stroke-width 2))
        (dolist (level bids)
          (let ((x (- center-x (* (/ plot-width 2.0)
                                 (/ (nth 2 level) (float max-cumulative)))))
                (y (funcall price-y (car level))))
            (financial-chart-svg--point-target
             svg x y (format "Bid %s: size %s, cumulative %s"
                             (financial-chart-fmt (car level))
                             (financial-chart-fmt (cadr level))
                             (financial-chart-fmt (nth 2 level))))))
        (dolist (level asks)
          (let ((x (+ center-x (* (/ plot-width 2.0)
                                 (/ (nth 2 level) (float max-cumulative)))))
                (y (funcall price-y (car level))))
            (financial-chart-svg--point-target
             svg x y (format "Ask %s: size %s, cumulative %s"
                             (financial-chart-fmt (car level))
                             (financial-chart-fmt (cadr level))
                             (financial-chart-fmt (nth 2 level))))))
        (financial-chart-svg--text svg (plist-get summary :text)
                                   center-x (- y0 8) "middle")
        (financial-chart-svg--text svg "cumulative size" center-x (+ bottom 29) "middle")
        (financial-chart-svg--text svg "BIDS" x0 (+ y0 14) "start" bid-color)
        (financial-chart-svg--text svg "ASKS" (+ x0 plot-width) (+ y0 14) "end" ask-color)
        (financial-chart-svg--string svg)))))

(defun financial-chart-depth--values (data _props)
  "Every level price in order-book DATA, for summaries."
  (mapcar #'car (append (plist-get data :bids) (plist-get data :asks))))

(defun financial-chart-depth--from-json (data)
  "JSON-parsed {\"bids\": [[P, S] ...], \"asks\": [...]} as an order-book plist."
  (append (when (assq 'bids data) (list :bids (alist-get 'bids data)))
          (when (assq 'asks data) (list :asks (alist-get 'asks data)))))

(defun financial-chart-depth--to-json (data)
  "Order-book DATA as a JSON object {\"bids\": [[P, S] ...], \"asks\": [...]}."
  (cl-flet ((levels (key) (apply #'vector (mapcar (lambda (l) (apply #'vector l))
                                                  (plist-get data key)))))
    (list (cons 'bids (levels :bids)) (cons 'asks (levels :asks)))))

(add-to-list 'financial-chart-shapes
             '(order-book
               :doc "Plist (:bids ((PRICE SIZE) ...) :asks ((PRICE SIZE) ...)); positive levels, with best bid no higher than best ask when both exist.  JSON: {\"bids\": [[price, size], ...], \"asks\": [...]}."
               :example (:bids ((100.0 2.0) (99.5 4.0) (99.0 6.0))
                        :asks ((100.5 1.0) (101.0 3.0) (101.5 5.0)))
               :validator financial-chart-depth--validate-order-book
               :values financial-chart-depth--values
               :from-json financial-chart-depth--from-json
               :to-json financial-chart-depth--to-json))

(financial-chart-register-kind
 'depth :shape 'order-book
 :text #'financial-chart-text-depth
 :svg #'financial-chart-svg-depth
 :doc "Order-book ladder or cumulative bid/ask depth chart.")

(provide 'financial-chart-depth)
;;; financial-chart-depth.el ends here
