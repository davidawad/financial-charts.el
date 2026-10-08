;;; financial-chart-eas-readout.el --- financial hover readouts as eas components -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The hover readout of every financial chart (fc-gy6).  eas reserves
;; one line under the plot and draws it from a component tree, the
;; chart's x-eas.readout (eas-readout, eas-component).  This file
;; registers the financial components, so a chart's JSON says:
;;
;;   "x-eas": {"readout": {"component": "row", "children": [
;;     {"component": "ohlc-readout", "props": {"style": "candles", "basis": "open"}},
;;     {"component": "indicator-values"}]}}
;;
;;   ohlc-readout      date, open, high, low, close, change, change %,
;;                     volume; close and the change coloured up or down
;;                     against the open or the previous close; volume
;;                     abbreviated (1.2M); open, high and low hidden on
;;                     line, step, area and baseline styles
;;   indicator-values  each indicator's value at the bar, in its series
;;                     colour, bold when its pane is under the cursor
;;   order-book-level  a book level: side, price and size in the side's
;;                     colour, cumulative size, level
;;   signed-value      a number coloured up when >= 0 and down below
;;                     (P/L, drawdown, a position's value)
;;   quantity          a labelled number (abbreviated, percent, ...) or
;;                     date, hidden when the datum lacks the field
;;
;; Up, down, bid and ask colours are props; unset, they follow the
;; chart's theme (its Vega config "financial": {"up", "down", "bid",
;; "ask"}), then `financial-chart-palette-up' and `-down'.  A component
;; whose datum is missing (nothing under the cursor and no latest row)
;; shows the view's values strip instead.  Users swap or compose these
;; like any eas component, and add their own with
;; `eas-define-component'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eas)
(require 'financial-chart-eas-palette)

(defconst financial-chart-readout-line-styles '("line" "step" "area" "baseline" "mountain")
  "Price styles that draw only the close: open, high and low do not apply.")

(defconst financial-chart-readout-ohlc-fields
  ["date" "open" "high" "low" "close" "change" "change_pct" "volume"]
  "Fields `ohlc-readout' shows by default, in order.")

;;; Helpers

(defun financial-chart-readout--fixed (n decimals)
  "Number N with DECIMALS places."
  (format (format "%%.%df" decimals) n))

(defun financial-chart-readout--whole-p (n)
  "Non-nil when number N is a whole number (of usual size)."
  (or (integerp n) (and (< (abs n) 1e15) (= n (truncate n)))))

(defun financial-chart-readout--spaced (atoms &optional sep)
  "ATOMS, each after the first separated from the one before by SEP.
SEP defaults to two spaces."
  (let ((sep (list (eas-component-span (or sep "  ")))))
    (cl-loop for a in atoms for i from 0
             collect (if (> i 0) (plist-put (copy-sequence a) :sep sep) a))))

(defun financial-chart-readout--get (datum field)
  "DATUM's FIELD (a string), nil when absent or null."
  (let ((v (and datum field (plist-get datum (eas-key field)))))
    (unless (memq v '(nil :null)) v)))

(defun financial-chart-readout--number (datum field)
  "DATUM's FIELD as a number, or nil."
  (let ((v (financial-chart-readout--get datum field)))
    (cond ((numberp v) v)
          ((and (stringp v) (string-match-p "\\`-?[0-9.]+\\'" v)) (string-to-number v)))))

(defun financial-chart-readout-color (ctx key prop)
  "PROP, else CTX's theme colour KEY (:up :down :bid :ask), else the palette's.
A bid without a theme colour of its own is up, an ask down."
  (let ((theme (plist-get ctx :theme)) (up (memq key '(:up :bid))))
    (or prop
        (eas-theme-get theme :financial key)
        (eas-theme-get theme :financial (if up :up :down))
        (if up financial-chart-palette-up financial-chart-palette-down))))

(defun financial-chart-readout-abbreviate (n &optional decimals)
  "Number N abbreviated with K, M, B or T: 1234567 is \"1.2M\".
DECIMALS (default 1) below 100 of the unit, none above it."
  (let* ((a (abs n))
         (unit (cl-find-if (lambda (u) (>= a (car u)))
                           '((1e12 . "T") (1e9 . "B") (1e6 . "M") (1e3 . "K")))))
    (if (null unit)
        (if (financial-chart-readout--whole-p n) (format "%d" (truncate n)) (financial-chart-readout--fixed n (or decimals 1)))
      (let ((x (/ n (car unit))))
        (concat (if (>= (abs x) 100) (format "%d" (round x)) (financial-chart-readout--fixed x (or decimals 1)))
                (cdr unit))))))

(defun financial-chart-readout--fmt (n kind decimals)
  "Number N as text per KIND (abbrev, plain, number, percent), DECIMALS places."
  (pcase kind
    ("abbrev" (financial-chart-readout-abbreviate n))
    ("plain" (if (financial-chart-readout--whole-p n) (format "%d" (truncate n)) (format "%s" n)))
    ("percent" (concat (financial-chart-readout--fixed (* 100 n) decimals) "%"))
    ;; d3's minus sign is U+2212; the readout keeps one minus, ASCII.
    (_ (string-replace "\u2212" "-" (eas-format-number (format ",.%df" decimals) n)))))

(defun financial-chart-readout--atom (label value style &rest keys)
  "An atom of LABEL then VALUE (strings), VALUE in STYLE.
KEYS are :priority, :keep and :number (VALUE as a number).  The
short form cuts the label to its first letter; the shorter one also
abbreviates the :number."
  (let* ((n (plist-get keys :number))
         (dim '(:dim t))
         (mk (lambda (l v) (append (and l (not (string-empty-p l)) (list (eas-component-span (concat l " ") dim)))
                                   (list (eas-component-span v style))))))
    (eas-component-atom (funcall mk label value)
                        :short (lambda () (funcall mk (and label (substring label 0 (min 1 (length label)))) value))
                        :shorter (lambda ()
                                   (funcall mk (and label (substring label 0 (min 1 (length label))))
                                            (if n (financial-chart-readout-abbreviate n 2) value)))
                        :priority (or (plist-get keys :priority) 50)
                        :keep (plist-get keys :keep))))

(defun financial-chart-readout--fallback (ctx)
  "The view's values strip: what a component shows without a datum."
  (if (plist-get ctx :strip-fields)
      (eas-component-render '(:component "fields" :props (:source "strip")) ctx)
    nil))

(defun financial-chart-readout-pane (ctx)
  "Id of the pane (scene view) under the pointer in CTX's view, or nil."
  (when-let* ((view (plist-get ctx :view)) (state (eas-view-state view)))
    (or (plist-get (plist-get state :hover) :view)
        (when-let* ((p (plist-get state :pointer)) (scene (eas-view-scene view)))
          (cl-loop for v across (vconcat (plist-get scene :views))
                   for b = (plist-get v :bounds)
                   thereis (and (vectorp b) (= (length b) 4)
                                (<= (aref b 0) (aref p 0) (+ (aref b 0) (aref b 2)))
                                (<= (aref b 1) (aref p 1) (+ (aref b 1) (aref b 3)))
                                (plist-get v :id)))))))

(defun financial-chart-readout--bold (style on)
  "STYLE, bold when ON."
  (if on (append '(:bold t) style) style))

;;; ohlc-readout

(defun financial-chart-readout--bar (ctx)
  "CTX's datum when it is a bar (it has a close), else the chart's last bar.
Nothing hovered, the strip's row may come from a layer that is not the
bars (a fill, a rule): the latest bar is what the readout means."
  (let ((d (plist-get ctx :datum)))
    (if (financial-chart-readout--number d "close") d
      (when-let* ((view (plist-get ctx :view))
                  (rows (ignore-errors (eas-data-rows (eas-view-data view))))
                  ((> (length rows) 0))
                  (last (elt rows (1- (length rows))))
                  ((financial-chart-readout--number last "close")))
        last))))

(defun financial-chart-readout--previous-close (ctx bar date-field close-field)
  "The close of the bar before BAR in CTX's view, matched on DATE-FIELD, or nil."
  (when-let* ((view (plist-get ctx :view))
              (key (financial-chart-readout--get bar date-field))
              (rows (ignore-errors (eas-data-rows (eas-view-data view)))))
    (cl-loop for prev = nil then row
             for row across (vconcat rows)
             when (equal (financial-chart-readout--get row date-field) key)
             return (and prev (financial-chart-readout--number prev close-field)))))

(defun financial-chart-readout--ohlc-atoms (props ctx)
  "The atoms of an `ohlc-readout' with PROPS in CTX."
  (let* ((d (financial-chart-readout--bar ctx))
         (fields (append (plist-get props :fields) nil))
         (line-style (member (plist-get props :style) financial-chart-readout-line-styles))
         (dec (plist-get props :decimals))
         (date-field (plist-get props :date_field))
         (close (financial-chart-readout--number d "close"))
         (open (financial-chart-readout--number d "open"))
         (basis (pcase (plist-get props :basis)
                  ("auto" (if (or line-style (null open)) "previous-close" "open"))
                  (b b)))
         (ref (if (equal basis "open") open
                (or (financial-chart-readout--number d (plist-get props :previous_close_field))
                    (financial-chart-readout--previous-close ctx d date-field "close"))))
         (change (and close ref (- close ref)))
         (dir (and change (if (>= change 0) :up :down)))
         (color (and dir (list :color (financial-chart-readout-color ctx dir (plist-get props dir)))))
         (pane (financial-chart-readout-pane ctx))
         (bold-close (equal pane (plist-get props :price_pane)))
         (bold-volume (equal pane (plist-get props :volume_pane))))
    (delq nil
          (mapcar
           (lambda (f)
             (pcase f
               ("date"
                (when-let* ((v (financial-chart-readout--get d date-field)))
                  (eas-component-atom
                   (list (eas-component-span (eas-component-format v (list :type "time" :pattern (plist-get props :date_format)))))
                   :shorter (list (eas-component-span (eas-component-format v '(:type "time") t)))
                   :priority 100 :role 'date)))
               ((or "open" "high" "low")
                (unless line-style
                  (when-let* ((n (financial-chart-readout--number d f)))
                    (financial-chart-readout--atom (upcase (substring f 0 1)) (financial-chart-readout--fmt n "number" dec)
                                                   nil :number n :priority (- 60 (cl-position f '("open" "high" "low") :test #'equal))))))
               ("close"
                (when close
                  (financial-chart-readout--atom "C" (financial-chart-readout--fmt close "number" dec)
                                                 (financial-chart-readout--bold color bold-close)
                                                 :number close :priority 95 :keep t)))
               ("change"
                (when change
                  (financial-chart-readout--atom nil (concat (if (>= change 0) "+" "") (financial-chart-readout--fmt change "number" dec))
                                                 color :priority 80)))
               ("change_pct"
                (when (and change (not (zerop ref)))
                  (let ((pct (/ change (float ref))))
                    (financial-chart-readout--atom nil (concat "(" (if (>= pct 0) "+" "")
                                                               (financial-chart-readout--fmt pct "percent" 2) ")")
                                                   color :priority 85))))
               ("volume"
                (when-let* ((n (financial-chart-readout--number d "volume")))
                  (financial-chart-readout--atom "Vol" (financial-chart-readout--fmt n (plist-get props :volume_format) 0)
                                                 (financial-chart-readout--bold nil bold-volume)
                                                 :priority 40)))
               (_ (when-let* ((v (financial-chart-readout--get d f)))
                    (financial-chart-readout--atom f (eas-component-format v nil) nil :priority 30)))))
           fields))))

(eas-define-component
 "ohlc-readout"
 :doc "A bar's date, open, high, low, close, change, change % and volume.
Close and the change are coloured UP or DOWN against BASIS (the open,
or the previous bar's close); open, high and low are hidden on line
styles; volume is abbreviated (1.2M)."
 :props `((fields :type array :default ,financial-chart-readout-ohlc-fields
                  :doc "Which of date open high low close change change_pct volume to show, in order; other names show that datum field.")
          (style :type string :default "candles" :doc "The price style; line, step, area and baseline hide open, high and low.")
          (basis :type string :default "auto" :enum ("auto" "open" "previous-close")
                 :doc "What close is compared with; auto is the open, or the previous close on line styles.")
          (up :type color :doc "Colour of a rise; default the theme's financial.up.")
          (down :type color :doc "Colour of a fall; default the theme's financial.down.")
          (volume_format :type string :default "abbrev" :enum ("abbrev" "number" "plain"))
          (decimals :type integer :default 2 :doc "Price decimals.")
          (previous_close_field :type string :default "prev_close"
                                :doc "A datum field holding the previous close; else the bar before is looked up.")
          (date_field :type string :default "time")
          (date_format :type string :default "%b %d, %Y")
          (price_pane :type string :default "price" :doc "Pane id; close is bold while the cursor is in it.")
          (volume_pane :type string :default "volume" :doc "Pane id; volume is bold while the cursor is in it."))
 :render (lambda (props ctx)
           (if (financial-chart-readout--bar ctx)
               (let ((atoms (financial-chart-readout--spaced (financial-chart-readout--ohlc-atoms props ctx) " ")))
                 ;; Two spaces after the date, one between the prices.
                 (when (and (cdr atoms) (eq (plist-get (car atoms) :role) 'date))
                   (setcar (cdr atoms) (plist-put (cadr atoms) :sep (list (eas-component-span "  ")))))
                 atoms)
             (financial-chart-readout--fallback ctx))))

;;; indicator-values

(defun financial-chart-readout--scene-series (view)
  "Folded indicator series VIEW's scene draws: ((NAME PANE COLOR) ...)."
  (let (out)
    (when-let* ((scene (and view (eas-view-scene view))))
      (seq-doseq (v (plist-get scene :views))
        (seq-doseq (m (plist-get v :marks))
          (let ((rows (plist-get m :rows)) (items (plist-get m :items)))
            (seq-doseq (row rows)
              (let ((name (plist-get row :indicator)))
                (when (and (stringp name) (not (assoc name out)))
                  (push (list name (plist-get v :id) nil) out))))
            (seq-doseq (item items)
              ;; A line's item is its whole path: :datum holds its rows.
              (let* ((i (plist-get item :datum)) (i (if (and (vectorp i) (> (length i) 0)) (aref i 0) i))
                     (row (and (integerp i) (< i (length rows)) (elt rows i)))
                     (entry (and row (assoc (plist-get row :indicator) out))))
                (when (and entry (null (nth 2 entry)) (stringp (plist-get item :stroke)))
                  (setf (nth 2 entry) (plist-get item :stroke)))))))))
    (nreverse out)))

(defun financial-chart-readout--indicator-atoms (props ctx)
  "The atoms of an `indicator-values' with PROPS in CTX."
  (let* ((d (financial-chart-readout--bar ctx))
         (pane (financial-chart-readout-pane ctx))
         (dec (plist-get props :decimals))
         (given (append (plist-get props :series) nil))
         (series (if given
                     (mapcar (lambda (s) (list (plist-get s :field) (plist-get s :pane) (plist-get s :color)
                                               (plist-get s :label)))
                             given)
                   (financial-chart-readout--scene-series (plist-get ctx :view)))))
    (cl-loop for (field spane color label) in series for i from 0
             for n = (financial-chart-readout--number d field)
             when n
             collect (financial-chart-readout--atom
                      (or label field) (financial-chart-readout--fmt n "number" dec)
                      (financial-chart-readout--bold (and color (list :color color))
                                                     (and spane pane (equal spane pane)))
                      :number n :priority (- (plist-get props :priority) (* i 0.01))))))

(eas-define-component
 "indicator-values"
 :doc "Each indicator's value on the datum, in its series colour.
The ones drawn in the pane under the cursor are bold.  Without SERIES,
the folded indicator lines of the chart (an \"indicator\" field) are
found in its scene, with their pane and colour."
 :props '((series :type array :doc "[{field, label, color, pane}]; default: found in the scene.")
          (decimals :type integer :default 2)
          (priority :type number :default 30 :doc "The first value's; each next is 0.01 lower."))
 :render (lambda (props ctx)
           (financial-chart-readout--spaced (financial-chart-readout--indicator-atoms props ctx))))

;;; order-book-level

(defun financial-chart-readout--book-atoms (props ctx)
  "The atoms of an `order-book-level' with PROPS in CTX."
  (let* ((d (plist-get ctx :datum))
         (side (financial-chart-readout--get d "side"))
         (color (pcase side
                  ("bid" (list :color (financial-chart-readout-color ctx :bid (plist-get props :bid))))
                  ("ask" (list :color (financial-chart-readout-color ctx :ask (plist-get props :ask))))))
         (dec (plist-get props :decimals))
         (price-text (lambda (field)
                       (let ((v (financial-chart-readout--get d field)))
                         (cond ((stringp v) v)
                               ((numberp v) (financial-chart-readout--fmt v "number" dec))))))
         (size-fmt (plist-get props :size_format)))
    (delq nil
          (mapcar
           (lambda (f)
             (pcase f
               ("side" (when side
                         (eas-component-atom (list (eas-component-span (upcase side) (append '(:bold t) color)))
                                             :priority 90 :keep t)))
               ("price" (when-let* ((p (or (and (equal side "mid") (funcall price-text "mid"))
                                            (funcall price-text (plist-get props :price_field))
                                            (funcall price-text "price"))))
                          (financial-chart-readout--atom nil p color :priority 95)))
               ("size" (when-let* (((not (equal side "mid"))) (n (financial-chart-readout--number d "size")))
                         (financial-chart-readout--atom "×" (financial-chart-readout--fmt n size-fmt 2) color
                                                        :number n :priority 85)))
               ("cumulative" (when-let* (((not (equal side "mid"))) (n (financial-chart-readout--number d "cumulative")))
                               (financial-chart-readout--atom "cum" (financial-chart-readout--fmt n size-fmt 2) nil
                                                              :number n :priority 60)))
               ("level" (when-let* ((n (financial-chart-readout--number d "level")) ((not (equal side "mid"))))
                          (financial-chart-readout--atom "L" (format "%d" n) '(:dim t) :priority 40)))
               ("spread" (when-let* ((n (financial-chart-readout--number d "spread")))
                           (financial-chart-readout--atom "spread" (financial-chart-readout--fmt n "plain" dec) nil
                                                          :number n :priority (if (equal side "mid") 80 20))))
               (_ (when-let* ((v (financial-chart-readout--get d f)))
                    (financial-chart-readout--atom f (eas-component-format v nil) nil :priority 30)))))
           (append (plist-get props :fields) nil)))))

(eas-define-component
 "order-book-level"
 :doc "One order-book level: side, price and size in BID or ASK colour.
On the mid row: the mid price and the spread."
 :props '((fields :type array :default ["side" "price" "size" "cumulative" "level" "spread"]
                  :doc "Which of side price size cumulative level spread to show, in order.")
          (bid :type color :doc "Colour of bids; default the theme's financial.bid, then up.")
          (ask :type color :doc "Colour of asks; default the theme's financial.ask, then down.")
          (price_field :type string :default "price_label" :doc "The price as drawn; price when absent.")
          (size_format :type string :default "abbrev" :enum ("abbrev" "number" "plain"))
          (decimals :type integer :default 2))
 :render (lambda (props ctx)
           (if (plist-get ctx :datum)
               (financial-chart-readout--spaced (financial-chart-readout--book-atoms props ctx))
             (financial-chart-readout--fallback ctx))))

;;; signed-value

(eas-define-component
 "signed-value"
 :doc "A datum FIELD coloured UP when >= 0 and DOWN below (P/L, drawdown)."
 :props '((field :type string :required t)
          (label :type string)
          (format :type string :default "number" :enum ("number" "percent" "abbrev" "plain"))
          (decimals :type integer :default 2)
          (sign :type boolean :default t :doc "Show + on positive values.")
          (up :type color) (down :type color)
          (priority :type number :default 80))
 :render (lambda (props ctx)
           (when-let* ((n (financial-chart-readout--number (plist-get ctx :datum) (plist-get props :field))))
             (let ((dir (if (>= n 0) :up :down)))
               (list (financial-chart-readout--atom
                      (or (plist-get props :label) (plist-get props :field))
                      (concat (if (and (> n 0) (not (memq (plist-get props :sign) (quote (nil :false :json-false :null))))) "+" "")
                              (financial-chart-readout--fmt n (plist-get props :format) (plist-get props :decimals)))
                      (list :color (financial-chart-readout-color ctx dir (plist-get props dir)))
                      :number n :priority (plist-get props :priority)))))))

(eas-define-component
 "quantity"
 :doc "A datum FIELD as LABEL and its value, hidden when the datum lacks it.
FORMAT abbrev writes 1234567 as 1.2M; time formats a date."
 :props '((field :type string :required t)
          (label :type string :doc "Default the field name; \"\" for none.")
          (format :type string :default "number" :enum ("number" "percent" "abbrev" "plain" "time"))
          (decimals :type integer :default 2)
          (style :type style)
          (priority :type number :default 50))
 :render (lambda (props ctx)
           (let* ((d (plist-get ctx :datum)) (field (plist-get props :field))
                  (label (or (plist-get props :label) field))
                  (fmt (plist-get props :format)))
             (if (equal fmt "time")
                 (when-let* ((v (financial-chart-readout--get d field)))
                   (list (eas-component-atom
                          (list (eas-component-span (eas-component-format v '(:type "time")) (plist-get props :style)))
                          :shorter (list (eas-component-span (eas-component-format v '(:type "time") t)
                                                             (plist-get props :style)))
                          :priority (plist-get props :priority))))
               (let ((n (financial-chart-readout--number d field)) (v (financial-chart-readout--get d field)))
                 (cond (n (list (financial-chart-readout--atom
                                 label (financial-chart-readout--fmt n fmt (plist-get props :decimals))
                                 (plist-get props :style) :number n :priority (plist-get props :priority))))
                       (v (list (financial-chart-readout--atom label (eas-component-format v nil)
                                                               (plist-get props :style)
                                                               :priority (plist-get props :priority))))))))))

(defconst financial-chart-readout-components
  '("ohlc-readout" "indicator-values" "order-book-level" "signed-value" "quantity")
  "The readout components this package registers with eas.")

(provide 'financial-chart-eas-readout)
;;; financial-chart-eas-readout.el ends here
