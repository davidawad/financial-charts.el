;;; financial-chart-plot.el --- One entry point for every chart kind: text or SVG -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-chart.el

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

(defconst financial-chart-kinds
  '((area      financial-chart-text-area      financial-chart-svg-area)
    (line      financial-chart-text-line      financial-chart-svg-area)
    (sparkline financial-chart-text-sparkline financial-chart-svg-area)
    (payoff    financial-chart-text-payoff    financial-chart-svg-payoff)
    (bars      financial-chart-text-bars      financial-chart-svg-bars)
    (ohlc      financial-chart-text-ohlc      financial-chart-svg-ohlc))
  "KIND -> (TEXT-RENDERER SVG-RENDERER).")

(defun financial-chart-plot--plist-drop (plist &rest keys)
  "PLIST without KEYS."
  (cl-loop for (k v) on plist by #'cddr
           unless (memq k keys) append (list k v)))

(defun financial-chart-plot--resolve-backend (backend)
  "Concrete backend (`text' or `svg') for BACKEND.
BACKEND nil means `financial-chart-backend'."
  (pcase (or backend financial-chart-backend)
    ('svg 'svg)
    ('text 'text)
    (_ (if (and (display-images-p) (image-type-available-p 'svg)) 'svg 'text))))

;;;###autoload
(defun financial-chart-plot (kind data &rest props)
  "Render DATA as a KIND chart and return it as a string.
See the Commentary of financial-chart.el for KIND and PROPS.  With the `svg'
backend the string is an SVG document; with `text' it is propertized
unicode.  Returns nil when DATA is empty (sparkline: \"\")."
  (let ((entry (or (alist-get kind financial-chart-kinds)
                   (error "financial-chart: unknown chart kind %S" kind))))
    (if (eq (financial-chart-plot--resolve-backend (plist-get props :backend)) 'svg)
        (apply (nth 1 entry) data
               (append (list :width (or (plist-get props :pixel-width) 600)
                             :height (or (plist-get props :pixel-height) 240))
                       (financial-chart-plot--plist-drop props :backend :width :height
                                             :pixel-width :pixel-height)))
      (apply (nth 0 entry) data (financial-chart-plot--plist-drop props :backend)))))

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

(defun financial-chart-plot-doctor-checks ()
  "Lazy (LABEL . CHECK-FN) health checks; CHECK-FN -> (:ok :detail :remediation)."
  (list
   (cons "financial-chart text render"
         (lambda ()
           (let ((s (financial-chart-plot 'area '(1 3 2 5) :backend 'text :width 4 :height 2)))
             (list :ok (and (stringp s) (string-match-p "last 5" s))
                   :detail "area chart renders over sample data"))))
   (cons "financial-chart svg backend"
         (lambda ()
           (list :ok t
                 :detail (if (image-type-available-p 'svg)
                             "svg images available"
                           "text only here (no svg image support in this frame)"))))
   (cons "financial-chart candles"
         (lambda ()
           (list :ok t
                 :detail (if (fboundp 'financial-chart-render)
                             "ohlc delegates to financial-chart"
                           "financial-chart not loaded: ohlc draws closes")
                 :remediation (unless (fboundp 'financial-chart-render)
                                "(require 'financial-chart)"))))))

(provide 'financial-chart-plot)
;;; financial-chart-plot.el ends here
