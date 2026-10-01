;;; financial-chart-plot.el --- One entry point for every chart kind: text or SVG -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `financial-chart-plot' KIND DATA &rest PROPS returns a chart as a string
;; (propertized unicode in a terminal, an SVG document in a GUI);
;; `-plot-insert' puts it at point and `-plot-view' shows it in a
;; `financial-chart-plot-mode' buffer.  KIND is a key of
;; `financial-chart-kinds'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'financial-chart-series)
(require 'financial-chart-text)
(require 'financial-chart-svg)

(defcustom financial-chart-backend 'auto
  "Rendering backend: `text', `svg', or `auto'.
`auto' uses SVG images when the selected frame can display them, else
unicode text -- so the same call looks right in a GUI and a terminal."
  :type '(choice (const auto) (const text) (const svg))
  :group 'financial-chart)

(defcustom financial-chart-empty-text "no data"
  "Text `financial-chart-plot-insert' shows when a chart has no data."
  :type 'string
  :group 'financial-chart)

;; -----------------------------------------------------------------------
;; Errors -- same convention as market-data.el: (MESSAGE . PLIST), where
;; the message names the fix and the plist carries :code plus locators.
;; -----------------------------------------------------------------------

(define-error 'financial-chart-unknown-kind
  "financial-chart: unknown chart kind" 'financial-chart-error)
(define-error 'financial-chart-invalid-data
  "financial-chart: invalid chart data" 'financial-chart-error)

;; -----------------------------------------------------------------------
;; Shapes and kinds -- registries, so the whole surface is enumerable.
;; A kind names a data shape and one renderer per backend; adding a kind
;; is `financial-chart-register-kind', no dispatch code changes.
;; -----------------------------------------------------------------------

(declare-function market-data-validate-bars "market-data" (bars))

(defvar financial-chart-shapes
  '((series
     :doc "Numbers, (X Y) lists or (X . Y) conses, oldest first.  A nil Y is skipped."
     :example ((1 40.0) (2 45.0) (3 50.0) (4 42.0))
     :validator financial-chart--validate-series)
    (payoff
     :doc "(PRICE PNL) pairs sorted by ascending price; both numeric."
     :example ((90 50) (95 0) (100 -100) (105 0) (110 50))
     :validator financial-chart--validate-payoff)
    (labeled
     :doc "(LABEL . VALUE) conses, e.g. P/L per position; VALUE numeric."
     :example (("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950))
     :validator financial-chart--validate-labeled)
    (ohlc
     :doc "bar/v1 plists (:open :high :low :close [:volume] [:time]), oldest
first; :time is epoch milliseconds.  Validated by market-data.el when it
is loaded (it owns the shape), else by the same required-key rule here."
     :example ((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
               (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000))
     :validator financial-chart--validate-ohlc))
  "Data shapes chart kinds accept: (SHAPE :doc :example :validator).")

(defvar financial-chart-kinds
  '((area :shape series :text financial-chart-text-area :svg financial-chart-svg-area
          :doc "Eighth-block area chart of a price or value history.")
    (line :shape series :text financial-chart-text-line :svg financial-chart-svg-area
          :doc "Braille line chart (2x4 dots per cell); SVG draws it as an area.")
    (sparkline :shape series :text financial-chart-text-sparkline
               :svg financial-chart-svg-area
               :doc "One-row sparkline for tables and mode lines.")
    (payoff :shape payoff :text financial-chart-text-payoff
            :svg financial-chart-svg-payoff
            :doc "Zero-anchored P/L-vs-price diagram with breakevens.")
    (bars :shape labeled :text financial-chart-text-bars :svg financial-chart-svg-bars
          :doc "Diverging horizontal bars, e.g. P/L per position.")
    (ohlc :shape ohlc :text financial-chart-text-ohlc :svg financial-chart-svg-ohlc
          :doc "Candlesticks with optional volume panel, X-axis and indicators."))
  "Chart kinds: (KIND :shape SHAPE :text FN :svg FN :doc STRING).
Each renderer is called as (FN DATA &rest PROPS) and returns a string.")

(defun financial-chart-register-kind (kind &rest spec)
  "Register (or replace) chart KIND with SPEC (:shape :text :svg :doc).
:shape must name an entry of `financial-chart-shapes'."
  (unless (assq (plist-get spec :shape) financial-chart-shapes)
    (signal 'financial-chart-error
            (list (format "unknown shape %S; known: %S" (plist-get spec :shape)
                          (mapcar #'car financial-chart-shapes)))))
  (setf (alist-get kind financial-chart-kinds) spec)
  kind)

(defun financial-chart--kind (kind)
  "KIND's registry plist, or signal `financial-chart-unknown-kind'."
  (or (alist-get kind financial-chart-kinds)
      (signal 'financial-chart-unknown-kind
              (list (format "%S is not a chart kind; use one of %s (see `financial-chart-list-kinds')"
                            kind (mapconcat #'symbol-name (mapcar #'car financial-chart-kinds) ", "))
                    :code "unknown_kind" :kind kind))))

;; -- validation --

(defun financial-chart--invalid (index fmt &rest args)
  "Signal `financial-chart-invalid-data' at element INDEX; FMT/ARGS the reason."
  (signal 'financial-chart-invalid-data
          (list (format "element %d: %s" index (apply #'format fmt args))
                :code "invalid_data" :index index)))

(defun financial-chart--validate-series (data)
  "Signal unless DATA is a SERIES."
  (unless (or (listp data) (vectorp data))
    (signal 'financial-chart-invalid-data
            (list (format "series must be a list or vector, got %S" data) :code "invalid_data")))
  (let ((i 0))
    (seq-doseq (p data)
      (unless (or (numberp p)
                  (and (consp p) (let ((y (financial-chart-series--point-y p)))
                                   (or (null y) (numberp y)))))
        (financial-chart--invalid i "expected a number, (X Y) or (X . Y) with numeric Y, got %S" p))
      (cl-incf i))))

(defun financial-chart--validate-payoff (data)
  "Signal unless DATA is a PAYOFF: numeric (PRICE PNL), ascending price."
  (financial-chart--validate-series data)
  (let ((i 0) prev)
    (seq-doseq (p data)
      (let ((x (financial-chart-series--point-x p)))
        (unless (and (numberp x) (numberp (financial-chart-series--point-y p)))
          (financial-chart--invalid i "payoff points need numeric (PRICE PNL), got %S" p))
        (when (and prev (< x prev))
          (financial-chart--invalid i "prices must ascend; %s follows %s (sort by price)" x prev))
        (setq prev x))
      (cl-incf i))))

(defun financial-chart--validate-labeled (data)
  "Signal unless DATA is a list of (LABEL . NUMBER)."
  (unless (listp data)
    (signal 'financial-chart-invalid-data
            (list (format "labeled data must be a list, got %S" data) :code "invalid_data")))
  (cl-loop for p in data for i from 0
           unless (and (consp p) (numberp (cdr p)))
           do (financial-chart--invalid i "expected (LABEL . NUMBER), got %S" p)))

(defun financial-chart--validate-ohlc (data)
  "Signal unless DATA is a list of bar/v1 plists."
  (if (fboundp 'market-data-validate-bars)
      (market-data-validate-bars data)
    (unless (listp data)
      (signal 'financial-chart-invalid-data
              (list (format "bars must be a list, got %S" data) :code "invalid_data")))
    (cl-loop for bar in data for i from 0
             do (unless (and (listp bar) (cl-evenp (length bar)))
                  (financial-chart--invalid i "bar is not a plist: %S" bar))
             (dolist (key '(:open :high :low :close))
               (unless (numberp (plist-get bar key))
                 (financial-chart--invalid i "required key %s missing or non-number" key))))))

;;;###autoload
(defun financial-chart-validate (kind data)
  "Return t when DATA fits KIND's shape, else signal a typed error.
`financial-chart-invalid-data' carries the offending element's :index.
Empty DATA is valid: it renders as \"no data\"."
  (let ((shape (plist-get (financial-chart--kind kind) :shape)))
    (when data
      (funcall (plist-get (alist-get shape financial-chart-shapes) :validator) data))
    t))

;; -- backend --

(defun financial-chart-plot--plist-drop (plist &rest keys)
  "PLIST without KEYS."
  (cl-loop for (k v) on plist by #'cddr
           unless (memq k keys) append (list k v)))

(defun financial-chart--backend-decision (backend)
  "(CONCRETE-BACKEND . REASON) for BACKEND (nil = `financial-chart-backend')."
  (let ((requested (or backend financial-chart-backend)))
    (pcase requested
      ('svg '(svg . "requested svg"))
      ('text '(text . "requested text"))
      (_ (if (and (display-images-p) (image-type-available-p 'svg))
             '(svg . "auto: this frame displays SVG images")
           '(text . "auto: this frame cannot display SVG images"))))))

(defun financial-chart-plot--resolve-backend (backend)
  "Concrete backend (`text' or `svg') for BACKEND.
BACKEND nil means `financial-chart-backend'."
  (car (financial-chart--backend-decision backend)))

(defun financial-chart--renderer-args (backend props)
  "The keyword args the BACKEND renderer receives for caller PROPS."
  (if (eq backend 'svg)
      (append (list :width (or (plist-get props :pixel-width) 600)
                    :height (or (plist-get props :pixel-height) 240))
              (financial-chart-plot--plist-drop props :backend :width :height
                                                :pixel-width :pixel-height))
    (financial-chart-plot--plist-drop props :backend)))

;; -- explain: the pure plan --

(defun financial-chart--data-summary (shape data)
  "Point count and value range of DATA in SHAPE, for explain and provenance."
  (let ((ys (pcase shape
              ('labeled (mapcar #'cdr data))
              ('ohlc (append (delq nil (mapcar (lambda (b) (plist-get b :low)) data))
                             (delq nil (mapcar (lambda (b) (plist-get b :high)) data))))
              (_ (financial-chart-series-values data)))))
    (append (list :points (length data))
            (when ys (list :min (apply #'min ys) :max (apply #'max ys))))))

;;;###autoload
(defun financial-chart-explain (kind data &rest props)
  "Return the plan `financial-chart-plot' would follow, without rendering.
A plist: :kind :shape :valid (t, or the error message) :backend and
:backend-reason :renderer :args (what the renderer receives) :points
:min :max.  Pure: no I/O, never signals for bad DATA."
  (let* ((entry (financial-chart--kind kind))
         (shape (plist-get entry :shape))
         (decision (financial-chart--backend-decision (plist-get props :backend)))
         (valid (condition-case err (financial-chart-validate kind data)
                  (error (error-message-string err)))))
    (append
     (list :kind kind :shape shape :valid valid
           :backend (car decision) :backend-reason (cdr decision)
           :renderer (plist-get entry (if (eq (car decision) 'svg) :svg :text))
           :args (financial-chart--renderer-args (car decision) props))
     (when (eq valid t) (financial-chart--data-summary shape data)))))

;; -- render --

(defun financial-chart--svg-provenance (svg kind data props)
  "SVG with a <title> and <desc> naming KIND, the data and PROPS' :title.
An agent reading the file alone knows what it shows."
  (let* ((summary (financial-chart--data-summary
                   (plist-get (financial-chart--kind kind) :shape) data))
         (title (or (plist-get props :title) (format "%s chart" kind)))
         (desc (format "financial-chart %s: %d points%s" kind (plist-get summary :points)
                       (if (plist-get summary :min)
                           (format ", range %s to %s"
                                   (financial-chart-fmt (plist-get summary :min))
                                   (financial-chart-fmt (plist-get summary :max)))
                         "")))
         (end (and (string-prefix-p "<svg" svg) (string-search ">" svg))))
    (if (not end)
        svg
      (concat (substring svg 0 (1+ end))
              "<title>" (xml-escape-string title) "</title>"
              "<desc>" (xml-escape-string desc) "</desc>"
              (substring svg (1+ end))))))

;;;###autoload
(defun financial-chart-plot (kind data &rest props)
  "Render DATA as a KIND chart and return it as a string.
KIND is a key of `financial-chart-kinds' (`financial-chart-list-kinds');
DATA must fit its shape (`financial-chart-validate').  PROPS: :backend
\(text, svg or auto -- see `financial-chart-backend'), :width/:height
\(text columns/rows), :pixel-width/:pixel-height (SVG), :unit, :title,
and per-renderer options such as :up-face/:down-face.  With `svg' the
string is an SVG document carrying a <title>/<desc> provenance block;
with `text' it is propertized unicode.  Returns nil when DATA is empty
\(sparkline: \"\").  `financial-chart-explain' shows the plan first."
  (let* ((entry (financial-chart--kind kind))
         (backend (financial-chart-plot--resolve-backend (plist-get props :backend))))
    (financial-chart-validate kind data)
    (let ((out (apply (plist-get entry (if (eq backend 'svg) :svg :text)) data
                      (financial-chart--renderer-args backend props))))
      (if (and out (eq backend 'svg))
          (financial-chart--svg-provenance out kind data props)
        out))))

;;;###autoload
(defun financial-chart-plot-spec (spec)
  "Render chart SPEC, a plist (:kind KIND :data DATA . PROPS).
The same plain-data form the batch CLI reads as JSON."
  (apply #'financial-chart-plot (plist-get spec :kind) (plist-get spec :data)
         (financial-chart-plot--plist-drop spec :kind :data)))

;; -- enumerate --

;;;###autoload
(defun financial-chart-list-kinds ()
  "Every chart kind as (KIND :shape SHAPE :doc DOC), registry order."
  (mapcar (lambda (e) (list (car e) :shape (plist-get (cdr e) :shape)
                            :doc (plist-get (cdr e) :doc)))
          financial-chart-kinds))

;;;###autoload
(defun financial-chart-describe-kind (kind)
  "KIND's full description: shape doc, example data, renderers, and
whether both renderers are defined right now."
  (let* ((entry (financial-chart--kind kind))
         (shape (alist-get (plist-get entry :shape) financial-chart-shapes)))
    (list :kind kind :doc (plist-get entry :doc)
          :shape (plist-get entry :shape) :shape-doc (plist-get shape :doc)
          :example (plist-get shape :example)
          :text (plist-get entry :text) :svg (plist-get entry :svg)
          :renderers-defined (and (fboundp (plist-get entry :text))
                                  (fboundp (plist-get entry :svg)) t))))

;;;###autoload
(defun financial-chart-sparkline (series &rest props)
  "One-row unicode sparkline string for SERIES (PROPS: :width :face)."
  (apply #'financial-chart-text-sparkline series props))

;;;###autoload
(defun financial-chart-plot-insert (kind data &rest props)
  "Insert DATA as a KIND chart at point: an SVG image or unicode text.
PROPS as in `financial-chart-plot'.
Inserts `financial-chart-empty-text' for no data."
  (let* ((backend (financial-chart-plot--resolve-backend (plist-get props :backend)))
         (out (apply #'financial-chart-plot kind data :backend backend props)))
    (cond
     ((or (null out) (equal out "")) (insert (propertize financial-chart-empty-text 'face 'financial-chart-dim)))
     ((eq backend 'svg) (insert-image (create-image out 'svg t :ascent 'center) "[chart]"))
     (t (insert out)))))

;; --- the *financial-chart* buffer ----------------------------------------------------

(defvar-local financial-chart-plot--spec nil
  "(KIND DATA PROPS) of the chart shown in this `financial-chart-plot-mode' buffer.")

(defvar financial-chart-plot-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'financial-chart-plot-refresh)
    (define-key m (kbd "t") #'financial-chart-plot-toggle-backend)
    m)
  "Keymap for `financial-chart-plot-mode'.")

(define-derived-mode financial-chart-plot-mode special-mode "Finchart"
  "Major mode for a buffer showing one financial-chart chart.
\\<financial-chart-plot-mode-map>\\[financial-chart-plot-refresh] re-renders to the window size; \
\\[financial-chart-plot-toggle-backend] flips text/SVG.")

(defun financial-chart-plot--fit-props (props)
  "PROPS with :width/:height filled in from the selected window if absent."
  (let* ((win (get-buffer-window (current-buffer) t))
         (cols (if win (window-body-width win) 80))
         (rows (if win (window-body-height win) 24)))
    (append props
            (unless (plist-member props :width)
              (list :width (or financial-chart-plot-width (max 20 (- cols 10)))))
            (unless (plist-member props :height)
              (list :height (max 6 (min 20 (- rows 6)))))
            (unless (plist-member props :pixel-width)
              (list :pixel-width (if win (window-body-width win t) 600))))))

(defun financial-chart-plot-refresh ()
  "Re-render this buffer's chart to fit its window."
  (interactive)
  (pcase-let ((`(,kind ,data ,props) financial-chart-plot--spec))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when-let* ((title (plist-get props :title)))
        (insert (propertize title 'face 'bold) "\n\n"))
      (apply #'financial-chart-plot-insert kind data (financial-chart-plot--fit-props props)))
    (goto-char (point-min))))

(defun financial-chart-plot-toggle-backend ()
  "Flip this buffer's chart between text and SVG."
  (interactive)
  (let* ((props (nth 2 financial-chart-plot--spec))
         (now (financial-chart-plot--resolve-backend (plist-get props :backend))))
    (setf (nth 2 financial-chart-plot--spec)
          (plist-put (copy-sequence props) :backend (if (eq now 'svg) 'text 'svg)))
    (financial-chart-plot-refresh)))

;;;###autoload
(defun financial-chart-plot-view (kind data &rest props)
  "Show DATA as a KIND chart in a `financial-chart-plot-mode' buffer.
Return the buffer.  PROPS as in `financial-chart-plot', plus :buffer
(name, default \"*financial-chart*\")."
  (let ((buf (get-buffer-create (or (plist-get props :buffer) "*financial-chart*"))))
    (with-current-buffer buf
      (financial-chart-plot-mode)
      (setq financial-chart-plot--spec (list kind data (financial-chart-plot--plist-drop props :buffer))))
    (unless noninteractive (pop-to-buffer buf))
    (with-current-buffer buf (financial-chart-plot-refresh))
    buf))

;;;###autoload
(defun financial-chart-demo ()
  "Show every financial-chart kind over built-in sample data."
  (interactive)
  (let ((series (cl-loop for i from 0 below 120
                         collect (list i (+ 100 (* 8 (sin (/ i 9.0))) (* 0.05 i)))))
        (payoff (cl-loop for p from 80 to 120
                         collect (list p (- (* 100 (max 0 (- p 100))) 250))))
        (buf (get-buffer-create "*financial-chart demo*")))
    (with-current-buffer buf
      (financial-chart-plot-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (spec `(("area" area ,series :unit "$")
                        ("line" line ,series :unit "$" :backend text)
                        ("payoff (long 100 call, $2.50)" payoff ,payoff)
                        ("bars" bars (("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950)))))
          (insert (propertize (car spec) 'face 'bold) "\n")
          (apply #'financial-chart-plot-insert (nth 1 spec) (nth 2 spec) :height 8 (nthcdr 3 spec))
          (insert "\n\n"))
        (insert "sparkline " (financial-chart-sparkline series :width 40) "\n")
        (goto-char (point-min))))
    (unless noninteractive (pop-to-buffer buf))
    buf))

;; --- health ---------------------------------------------------------------------

(defun financial-chart--check (name ok detail &optional remediation)
  "One eager doctor row: (:name NAME :status pass|fail :detail :remediation)."
  (list :name name :status (if ok 'pass 'fail) :detail detail
        :remediation (unless ok remediation)))

(defun financial-chart-plot-doctor-checks ()
  "Eager doctor rows: every registered kind renders its example in both
backends, and whether this frame can show SVG inline.  No I/O."
  (append
   (mapcar
    (lambda (entry)
      (let* ((kind (car entry))
             (example (plist-get (alist-get (plist-get (cdr entry) :shape)
                                            financial-chart-shapes)
                                 :example))
             (err (condition-case e
                      (progn
                        (financial-chart-plot kind example :backend 'text :width 20 :height 4)
                        (financial-chart-plot kind example :backend 'svg)
                        nil)
                    (error (error-message-string e)))))
        (financial-chart--check
         (format "kind %s renders" kind) (null err)
         (or err "example renders as text and svg")
         (format "(financial-chart-describe-kind '%s) and fix its renderers" kind))))
    financial-chart-kinds)
   (list (list :name "inline svg display"
               :status (if (image-type-available-p 'svg) 'pass 'skip)
               :detail (if (image-type-available-p 'svg)
                           "svg images available; auto backend draws SVG in GUI frames"
                         "no svg image support: auto backend draws text")
               :remediation nil))))

(provide 'financial-chart-plot)
;;; financial-chart-plot.el ends here
