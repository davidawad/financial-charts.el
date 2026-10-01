;;; financial-chart-svg.el --- SVG renderers and SVG/PNG export for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-chart.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vector candlesticks built with Emacs's own svg.el, plus SVG and PNG
;; export (PNG shells out to rsvg-convert or ImageMagick).

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-indicators)

;; -----------------------------------------------------------------------
;; SVG rendering -- a real vector chart, sharing this file's data-prep
;; code (windowing, scale conversion, price range, indicator series) and
;; configuration surface with the text renderer above, so they can never
;; drift out of sync with each other or need separate customization.
;; -----------------------------------------------------------------------

(require 'svg)

(defcustom financial-chart-svg-candle-width 6
  "Pixel width of each candle's body in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-candle-gap 3
  "Pixel gap between adjacent candles in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-wick-width 1
  "Pixel stroke width of a candle's wick line in the SVG renderer."
  :type 'number
  :group 'financial-chart)

(defcustom financial-chart-svg-price-height 400
  "Pixel height of the price panel in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-volume-height 100
  "Pixel height of the volume panel in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-left 55
  "Left margin (pixels) reserved for price/volume axis labels."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-right 20
  "Right margin (pixels) in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-top 40
  "Top margin (pixels) reserved for the title in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-margin-bottom 30
  "Bottom margin (pixels) reserved for X-axis labels in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-font-size 12
  "Font size (pixels) for all text in the SVG renderer."
  :type 'integer
  :group 'financial-chart)

(defcustom financial-chart-svg-font-family
  "DejaVu Sans Mono, Menlo, Consolas, monospace"
  "CSS font-family value for all text in the SVG renderer.
This package's own default is a sensible, widely-available open-source
monospace stack (DejaVu Sans Mono, with common platform fallbacks and a
generic `monospace' as the last resort) -- it doesn't assume any one
specific font is installed on whatever machine ends up rasterizing the
SVG. Set this to your own preferred font machine-wide (e.g. `(setq
financial-chart-svg-font-family \"Hack\")'), or pass FONT-FAMILY to
`financial-chart-render-svg'/`-export-svg'/`-export-png' to override it
for one call."
  :type 'string
  :group 'financial-chart)

(defcustom financial-chart-svg-background nil
  "Background color for the SVG renderer, or nil to use the current
`default' face's background (theme-aware)."
  :type '(choice (const :tag "Theme background" nil) color)
  :group 'financial-chart)

(defcustom financial-chart-svg-text-color nil
  "Text color for the SVG renderer, or nil to use `financial-chart-axis-face'
\(or the `default' face, if that's also nil) -- theme-aware either way."
  :type '(choice (const :tag "Theme-derived" nil) color)
  :group 'financial-chart)

(defcustom financial-chart-export-directory "~/Desktop"
  "Default directory the `financial-chart-schwab-export-*' commands
suggest/save into."
  :type 'directory
  :group 'financial-chart)

(defcustom financial-chart-png-converter nil
  "How `financial-chart-export-png' rasterizes SVG to PNG: nil
auto-detects the first available of `rsvg-convert'/`convert'/`magick'
via `executable-find'; a symbol names one of those explicitly; a
function is called as (FN SVG-FILE PNG-FILE) and does the conversion
itself (e.g. to shell out to some other tool, or convert in-process)."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const rsvg-convert) (const convert) (const magick)
                 function)
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-foreground "#333333"
  "Foreground used by the SVG renderer when a face's color can't be
resolved to a real color -- happens in a themeless session (e.g.
`emacs -Q --batch'), where Emacs returns its literal \"unspecified\"
placeholder instead of an actual color. Never used when a real theme
color is available."
  :type 'color
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-background "#ffffff"
  "Background fallback -- see `financial-chart-svg-fallback-foreground'."
  :type 'color
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-up-color "#2e7d32"
  "Foreground fallback specifically for `financial-chart-up-face' -- see
`financial-chart-svg-fallback-foreground'."
  :type 'color
  :group 'financial-chart)

(defcustom financial-chart-svg-fallback-down-color "#c62828"
  "Foreground fallback specifically for `financial-chart-down-face' -- see
`financial-chart-svg-fallback-foreground'."
  :type 'color
  :group 'financial-chart)

(defun financial-chart--color-unspecified-p (value)
  "Return non-nil when VALUE is Emacs's \"no real color\" placeholder
\(nil, the symbol `unspecified', or the strings \"unspecified-fg\"/
\"unspecified-bg\" -- `face-attribute' returns one of these forms
depending on the attribute and Emacs version when a face has no theme
color set, e.g. in a themeless `emacs -Q --batch' session)."
  (or (null value) (eq value 'unspecified)
      (member value '("unspecified-fg" "unspecified-bg"))))

(defun financial-chart--face-color (face &optional attr fallback)
  "Return FACE's resolved ATTR (default :foreground) as a color string.
Falls back to the `default' face's ATTR when FACE is nil or has none
of its own, and finally to FALLBACK (default
`financial-chart-svg-fallback-foreground'/`-background', by ATTR) when
even that is unresolvable -- always returns a real, renderable color,
regardless of whether a theme is loaded."
  (let* ((attr (or attr :foreground))
         (value
          (let ((v (and face (face-attribute face attr nil t))))
            (if (financial-chart--color-unspecified-p v)
                (face-attribute 'default attr nil t)
              v))))
    (if (financial-chart--color-unspecified-p value)
        (or fallback
            (if (eq attr :background)
                financial-chart-svg-fallback-background
              financial-chart-svg-fallback-foreground))
      (format "%s" value))))

(defun financial-chart--axis-label-values (min max count)
  "Return COUNT scale-space values evenly spaced from MIN to MAX inclusive."
  (if (<= count 1)
      (list min)
    (let ((step (/ (- max min) (float (1- count)))))
      (mapcar (lambda (i) (+ min (* i step))) (number-sequence 0 (1- count))))))

(defun financial-chart--svg-x (index)
  "Pixel X of bar INDEX's left edge in the SVG renderer."
  (+ financial-chart-svg-margin-left
     (* index (+ financial-chart-svg-candle-width financial-chart-svg-candle-gap))))

(defun financial-chart--svg-y (value min max panel-y panel-height)
  "Map scale-space VALUE in [MIN,MAX] to a pixel Y within a panel
spanning [PANEL-Y, PANEL-Y+PANEL-HEIGHT), Y growing downward."
  ;; (float ...) on the numerator is load-bearing: MIN/MAX/VALUE are
  ;; frequently plain integers (any bar data using whole-number prices),
  ;; and Elisp's `/' truncates on all-integer operands -- (/ 6 33) is 0,
  ;; not 0.18 -- which collapsed every candle but the topmost to the
  ;; panel's bottom pixel until this was caught by testing with integer
  ;; OHLC values.
  (+ panel-y (* panel-height (- 1.0 (/ (float (- value min)) (- max min))))))

(defun financial-chart--svg-price-panel (svg bars min max panel-y panel-h
                                             text-color indicator-series)
  "Draw the price panel (axis labels, gridlines, candles, indicator
overlays) into SVG."
  (dolist (value (financial-chart--axis-label-values
                  min max financial-chart-axis-label-count))
    (let ((y (financial-chart--svg-y value min max panel-y panel-h)))
      (svg-text svg (string-trim (format financial-chart-axis-format
                                        (financial-chart--from-scale value)))
               :x 5 :y (+ y 4) :fill text-color
               :font-family financial-chart-svg-font-family
               :font-size financial-chart-svg-font-size)))
  (cl-loop
   for i from 0
   for bar in bars
   do
   (let* ((x (financial-chart--svg-x i))
          (cx (+ x (/ financial-chart-svg-candle-width 2.0)))
          (open (financial-chart--to-scale (plist-get bar :open)))
          (close (financial-chart--to-scale (plist-get bar :close)))
          (low (financial-chart--to-scale (plist-get bar :low)))
          (high (financial-chart--to-scale (plist-get bar :high)))
          (up (>= (plist-get bar :close) (plist-get bar :open)))
          (color
           (financial-chart--face-color
            (if up financial-chart-up-face financial-chart-down-face)
            :foreground
            (if up financial-chart-svg-fallback-up-color
              financial-chart-svg-fallback-down-color)))
          (wick-color
           (if financial-chart-wick-face
               (financial-chart--face-color financial-chart-wick-face)
             color))
          (y-open (financial-chart--svg-y open min max panel-y panel-h))
          (y-close (financial-chart--svg-y close min max panel-y panel-h))
          (y-high (financial-chart--svg-y high min max panel-y panel-h))
          (y-low (financial-chart--svg-y low min max panel-y panel-h))
          (body-top (min y-open y-close))
          (body-h (max 1.0 (abs (- y-close y-open)))))
     (svg-line svg cx y-high cx y-low
              :stroke wick-color :stroke-width financial-chart-svg-wick-width)
     (svg-rectangle svg x body-top financial-chart-svg-candle-width body-h
                    :fill color)))
  (dolist (spec indicator-series)
    (let ((points
           (cl-loop
            for i from 0
            for value in (plist-get spec :series)
            when value
            collect
            (cons
             (+ (financial-chart--svg-x i) (/ financial-chart-svg-candle-width 2.0))
             (financial-chart--svg-y
              (financial-chart--to-scale value) min max panel-y panel-h)))))
      (when (>= (length points) 2)
        (svg-polyline svg points
                      :stroke (financial-chart--face-color (plist-get spec :face))
                      :fill "none" :stroke-width 1.5)))))

(defun financial-chart--svg-volume-panel (svg bars panel-y panel-h text-color)
  "Draw the volume panel (axis labels, bars) into SVG."
  (let* ((volumes (mapcar (lambda (b) (float (or (plist-get b :volume) 0))) bars))
         (max-vol (max 1.0 (apply #'max volumes))))
    (dolist (value (financial-chart--axis-label-values
                    0.0 max-vol financial-chart-volume-axis-label-count))
      (let ((y (financial-chart--svg-y value 0.0 max-vol panel-y panel-h)))
        (svg-text svg (string-trim (format financial-chart-volume-axis-format value))
                 :x 5 :y (+ y 4) :fill text-color
                 :font-size financial-chart-svg-font-size
                 :font-family financial-chart-svg-font-family)))
    (cl-loop
     for i from 0
     for bar in bars
     do
     (let* ((x (financial-chart--svg-x i))
            (vol (float (or (plist-get bar :volume) 0)))
            (up (>= (plist-get bar :close) (plist-get bar :open)))
            (color
             (financial-chart--face-color
              (if up
                  (or financial-chart-volume-up-face financial-chart-up-face)
                (or financial-chart-volume-down-face financial-chart-down-face))
              :foreground
              (if up financial-chart-svg-fallback-up-color
                financial-chart-svg-fallback-down-color)))
            (y (financial-chart--svg-y vol 0.0 max-vol panel-y panel-h))
            (h (max 1.0 (- (+ panel-y panel-h) y))))
       (svg-rectangle svg x y financial-chart-svg-candle-width h :fill color)))))

(defun financial-chart--svg-x-axis (svg bars axis-y text-color)
  "Draw evenly-spaced date/time labels into SVG below the chart."
  (let* ((n (length bars))
         (rows
          (financial-chart--axis-label-rows n financial-chart-x-axis-label-count)))
    (dolist (i rows)
      (let ((time (plist-get (nth i bars) :time)))
        (when time
          (svg-text svg (format-time-string financial-chart-x-axis-format
                                            (/ time 1000.0))
                   :x (financial-chart--svg-x i) :y (+ axis-y 15)
                   :fill text-color :font-size financial-chart-svg-font-size
                   :font-family financial-chart-svg-font-family))))))

;;;###autoload
(defun financial-chart-render-svg (bars &optional title font-family)
  "Render BARS as a real vector SVG candlestick chart, returned as an
XML string. Shares `financial-chart-render''s configuration surface
\(bar windowing, colors, scale, volume panel, X-axis, indicators) plus
its own `financial-chart-svg-*' size/margin/color knobs. FONT-FAMILY
overrides `financial-chart-svg-font-family' for this call only."
  (unless bars
    (user-error "financial-chart-render-svg: no bars to render"))
  (let* ((financial-chart-svg-font-family
          (or font-family financial-chart-svg-font-family))
         (bars (financial-chart--window-bars bars))
         (n (length bars))
         (range (financial-chart--bars-range bars))
         (min (car range))
         (max (if (= (car range) (cdr range)) (+ (cdr range) 0.0001) (cdr range)))
         (show-volume
          (and financial-chart-show-volume
               (cl-some (lambda (b) (plist-get b :volume)) bars)))
         (show-x-axis
          (and financial-chart-show-x-axis
               (cl-some (lambda (b) (plist-get b :time)) bars)))
         (plot-width
          (+ financial-chart-svg-margin-left financial-chart-svg-margin-right
             (* n (+ financial-chart-svg-candle-width financial-chart-svg-candle-gap))))
         (price-y financial-chart-svg-margin-top)
         (price-h financial-chart-svg-price-height)
         (volume-y (+ price-y price-h 10))
         (volume-h (if show-volume financial-chart-svg-volume-height 0))
         (xaxis-y (+ volume-y volume-h (if show-volume 10 0)))
         (total-height
          (+ xaxis-y (if show-x-axis financial-chart-svg-margin-bottom 10)))
         (bg (or financial-chart-svg-background
                (financial-chart--face-color 'default :background)))
         (text-color
          (or financial-chart-svg-text-color
              (and financial-chart-axis-face
                   (financial-chart--face-color financial-chart-axis-face))
              (financial-chart--face-color 'default :foreground)))
         (indicator-series (financial-chart--compute-indicator-series bars))
         (svg (svg-create plot-width total-height)))
    (svg-rectangle svg 0 0 plot-width total-height :fill bg)
    (when title
      (svg-text svg title :x financial-chart-svg-margin-left :y 20
               :fill text-color
               :font-size (+ 2 financial-chart-svg-font-size)
               :font-family financial-chart-svg-font-family
               :font-weight "bold"))
    (financial-chart--svg-price-panel
     svg bars min max price-y price-h text-color indicator-series)
    (when show-volume
      (financial-chart--svg-volume-panel svg bars volume-y volume-h text-color))
    (when show-x-axis
      (financial-chart--svg-x-axis svg bars xaxis-y text-color))
    (with-temp-buffer
      (svg-print svg)
      (buffer-string))))

;;;###autoload
(defun financial-chart-export-svg (bars file &optional title font-family)
  "Write BARS as an SVG candlestick chart to FILE. Returns FILE.
FONT-FAMILY overrides `financial-chart-svg-font-family' for this call."
  (with-temp-file file
    (insert (financial-chart-render-svg bars title font-family)))
  file)

(defun financial-chart--resolve-png-converter ()
  "Return the converter `financial-chart-export-png' should use."
  (or financial-chart-png-converter
      (cl-find-if (lambda (name) (executable-find (symbol-name name)))
                  '(rsvg-convert convert magick))
      (user-error
       "No SVG->PNG converter found (looked for rsvg-convert/convert/magick) -- install one (e.g. brew install librsvg) or set financial-chart-png-converter")))

(defun financial-chart--run-png-converter (converter svg-file png-file width height)
  "Invoke external CONVERTER to rasterize SVG-FILE to PNG-FILE."
  (let ((args
         (pcase converter
           ('rsvg-convert
            (append (list "-o" png-file)
                    (when width (list "-w" (number-to-string width)))
                    (when height (list "-h" (number-to-string height)))
                    (list svg-file)))
           ((or 'convert 'magick)
            (list svg-file png-file)))))
    (let ((status (apply #'call-process (symbol-name converter) nil nil nil args)))
      (unless (zerop status)
        (user-error "financial-chart-export-png: %s exited %s" converter status)))))

;;;###autoload
(defun financial-chart-export-png (bars file &optional title width height
                                        font-family)
  "Write BARS as a PNG candlestick chart to FILE.
Renders to SVG first (`financial-chart-export-svg') then rasterizes via
`financial-chart-png-converter' -- the one place this file shells out
to an external process, because rasterizing vector graphics isn't
something Elisp can do on its own. WIDTH/HEIGHT (pixels) are passed to
the converter when it supports them (currently: rsvg-convert).
FONT-FAMILY overrides `financial-chart-svg-font-family' for this call.
Returns FILE."
  (let ((svg-file (make-temp-file "financial-chart" nil ".svg"))
        (converter (financial-chart--resolve-png-converter)))
    (unwind-protect
        (progn
          (financial-chart-export-svg bars svg-file title font-family)
          (if (functionp converter)
              (funcall converter svg-file file)
            (financial-chart--run-png-converter
             converter svg-file file width height)))
      (delete-file svg-file))
    file))

(provide 'financial-chart-svg)
;;; financial-chart-svg.el ends here
