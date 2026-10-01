;;; financial-chart-svg.el --- SVG renderers and SVG/PNG export for financial-chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vector candlesticks built with Emacs's own svg.el, plus SVG and PNG
;; export (PNG shells out to rsvg-convert or ImageMagick).

;;; Code:

(require 'financial-chart-series)
(require 'subr-x)
(require 'financial-chart-core)
(require 'financial-chart-indicators)

;; -----------------------------------------------------------------------
;; SVG rendering -- a real vector chart, sharing this file's data-prep
;; code (windowing, scale conversion, price range, indicator series) and
;; configuration surface with the text renderer above, so they can never
;; drift out of sync with each other or need separate customization.
;; -----------------------------------------------------------------------

(require 'svg)
(require 'color)

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

(defcustom financial-chart-svg-oscillator-height 100
  "Pixel height of the oscillator panel in the SVG renderer."
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
  "Default directory the interactive export commands suggest."
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
                                             plot-width indicator-series)
  "Draw the price panel (axis labels, gridlines, candles, indicator
overlays) into SVG."
  (financial-chart-svg--horizontal-ticks
   svg (mapcar (lambda (value)
                 (list (financial-chart--svg-y value min max panel-y panel-h)
                       (string-trim
                        (format financial-chart-axis-format
                                (financial-chart--from-scale value)))))
   (financial-chart--axis-label-values
                min max financial-chart-axis-label-count))
   financial-chart-svg-margin-left
   (- plot-width financial-chart-svg-margin-right))
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
          (color (financial-chart-svg--color (if up 'up 'down)))
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
     (let ((group (financial-chart-svg--element-title
                   (svg-node svg 'g)
                   (format "Open %s, high %s, low %s, close %s"
                           (plist-get bar :open) (plist-get bar :high)
                           (plist-get bar :low) (plist-get bar :close)))))
       (svg-line group cx y-high cx y-low
                 :stroke wick-color :stroke-width financial-chart-svg-wick-width)
       (svg-rectangle group x body-top financial-chart-svg-candle-width body-h
                      :fill color))))
  (cl-loop for spec in indicator-series for series-index from 0
           do (let ((points
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
                      :stroke (financial-chart-svg--series-face-color spec series-index)
                      :fill "none" :stroke-width 1.5))
      (cl-loop for value in (plist-get spec :series) for i from 0
               when (numberp value)
               do (financial-chart-svg--point-target
                   svg (+ (financial-chart--svg-x i)
                          (/ financial-chart-svg-candle-width 2.0))
                   (financial-chart--svg-y (financial-chart--to-scale value)
                                           min max panel-y panel-h)
                   (format "%s: %s" (plist-get spec :label) value))))))

(defun financial-chart--svg-volume-panel (svg bars panel-y panel-h plot-width)
  "Draw the volume panel (axis labels, bars) into SVG."
  (let* ((volumes (mapcar (lambda (b) (float (or (plist-get b :volume) 0))) bars))
         (max-vol (max 1.0 (apply #'max volumes))))
    (financial-chart-svg--horizontal-ticks
     svg (mapcar (lambda (value)
                   (list (financial-chart--svg-y value 0.0 max-vol panel-y panel-h)
                         (string-trim (format financial-chart-volume-axis-format value))))
     (financial-chart--axis-label-values
                  0.0 max-vol financial-chart-volume-axis-label-count))
     financial-chart-svg-margin-left
     (- plot-width financial-chart-svg-margin-right))
    (cl-loop
     for i from 0
     for bar in bars
     do
     (let* ((x (financial-chart--svg-x i))
            (vol (float (or (plist-get bar :volume) 0)))
            (up (>= (plist-get bar :close) (plist-get bar :open)))
            (color (financial-chart-svg--color (if up 'up 'down)))
            (y (financial-chart--svg-y vol 0.0 max-vol panel-y panel-h))
            (h (max 1.0 (- (+ panel-y panel-h) y)))
            (group (financial-chart-svg--element-title
                    (svg-node svg 'g)
                    (format "Volume %s" (or (plist-get bar :volume) 0)))))
       (svg-rectangle group x y financial-chart-svg-candle-width h :fill color)))))

(defun financial-chart--svg-oscillator-panel
    (svg panel-y panel-h plot-width series-list)
  "Draw fixed 0-100 oscillator series and 30/70 guides into SVG."
  (let ((axis-values '(100 70 30 0))
        (x1 financial-chart-svg-margin-left)
        (x2 (- plot-width financial-chart-svg-margin-right)))
    (financial-chart-svg--horizontal-ticks
     svg (mapcar (lambda (value)
                   (list (financial-chart--svg-y value 0.0 100.0 panel-y panel-h)
                         (number-to-string value)
                         (and (memq value '(70 30)) 'guide)))
                 axis-values)
     x1 x2)
    (cl-loop for spec in series-list for series-index from 0
             do
      (let ((points
             (cl-loop
              for i from 0
              for value in (plist-get spec :series)
              when (numberp value)
              collect
              (cons (+ (financial-chart--svg-x i)
                       (/ financial-chart-svg-candle-width 2.0))
                    (financial-chart--svg-y
                     (max 0.0 (min 100.0 (float value)))
                     0.0 100.0 panel-y panel-h)))))
        (when (>= (length points) 2)
          (svg-polyline svg points
                        :stroke (financial-chart-svg--series-face-color
                                 spec series-index)
                        :fill "none" :stroke-width 1.5))
        (cl-loop for value in (plist-get spec :series)
                 for index from 0
                 when (numberp value)
                 do (financial-chart-svg--point-target
                     svg (+ (financial-chart--svg-x index)
                            (/ financial-chart-svg-candle-width 2.0))
                     (financial-chart--svg-y
                      (max 0.0 (min 100.0 (float value)))
                      0.0 100.0 panel-y panel-h)
                     (format "%s: %s" (plist-get spec :label) value)))))))

(defun financial-chart--svg-x-axis (svg bars axis-y text-color)
  "Draw evenly-spaced date/time labels into SVG below the chart."
  (let* ((n (length bars))
         (rows
          (financial-chart--axis-label-rows n financial-chart-x-axis-label-count)))
    (dolist (i rows)
      (let ((time (plist-get (nth i bars) :time)))
        (when time
          (svg-line svg (financial-chart--svg-x i) axis-y
                    (financial-chart--svg-x i) (+ axis-y 5)
                    :stroke (financial-chart-svg--color 'grid))
          (financial-chart-svg--text
           svg (format-time-string financial-chart-x-axis-format (/ time 1000.0))
           (financial-chart--svg-x i) (+ axis-y 15)
           (if (= i 0) "start"
             (if (= i (1- n)) "end" "middle")) text-color))))))

;;;###autoload
(defun financial-chart-render-svg (bars &optional title font-family)
  "Render BARS as a real vector SVG candlestick chart, returned as an
XML string. Shares `financial-chart-render''s configuration surface
\(bar windowing, colors, scale, oscillator and volume panels, X-axis,
indicators) plus
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
         (show-oscillators (and financial-chart-oscillators t))
         (show-x-axis
          (and financial-chart-show-x-axis
               (cl-some (lambda (b) (plist-get b :time)) bars)))
         (plot-width
          (+ financial-chart-svg-margin-left financial-chart-svg-margin-right
             (* n (+ financial-chart-svg-candle-width financial-chart-svg-candle-gap))))
         (price-y financial-chart-svg-margin-top)
         (price-h financial-chart-svg-price-height)
         (oscillator-y (+ price-y price-h 10))
         (oscillator-h (if show-oscillators
                           financial-chart-svg-oscillator-height 0))
         (volume-y (+ price-y price-h 10
                      (if show-oscillators (+ oscillator-h 10) 0)))
         (volume-h (if show-volume financial-chart-svg-volume-height 0))
         (xaxis-y (+ volume-y volume-h (if show-volume 10 0)))
         (total-height
          (+ xaxis-y (if show-x-axis financial-chart-svg-margin-bottom 10)))
         (indicator-series (financial-chart--compute-indicator-series bars))
         (oscillator-series (financial-chart--compute-oscillator-series bars))
         (svg (financial-chart-svg--canvas plot-width total-height title))
         (text-color (financial-chart-svg--color 'text)))
    (financial-chart--svg-price-panel
     svg bars min max price-y price-h plot-width indicator-series)
    (when (> (length indicator-series) 1)
      (financial-chart-svg--legend svg indicator-series
                                   financial-chart-svg-margin-left
                                   (- price-y 4)))
    (when show-oscillators
      (financial-chart--svg-oscillator-panel
       svg oscillator-y oscillator-h plot-width oscillator-series)
      (when (> (length oscillator-series) 1)
        (financial-chart-svg--legend svg oscillator-series
                                     financial-chart-svg-margin-left
                                     (- oscillator-y 4))))
    (when show-volume
      (financial-chart--svg-volume-panel
       svg bars volume-y volume-h plot-width))
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

;; -----------------------------------------------------------------------
;; Generic chart kinds (area, payoff, bars) as SVG
;; -----------------------------------------------------------------------

(defcustom financial-chart-svg-palette nil
  "Alist overriding SVG colours: keys up, down, text, grid, background.
A missing key falls back to the matching financial-chart face, then to
`financial-chart-svg--fallback-palette'."
  :type '(alist :key-type symbol :value-type string)
  :group 'financial-chart)

(defconst financial-chart-svg--fallback-palette
  '((up . "#2e7d32") (down . "#c62828") (text . "#333333")
    (grid . "#9e9e9e") (background . "#ffffff"))
  "Colours used when neither the palette nor a face resolves one.")

(defconst financial-chart-svg--faces
  '((up . financial-chart-up) (down . financial-chart-down) (text . default)
    (grid . financial-chart-dim) (background . default))
  "Face consulted for each palette key.")

(defun financial-chart-svg--color (key)
  "Resolve palette KEY through explicit, semantic and theme colors."
  (let* ((face (pcase key
                 ('up financial-chart-up-face)
                 ('down financial-chart-down-face)
                 ('text (or financial-chart-axis-face 'default))
                 (_ (alist-get key financial-chart-svg--faces))))
         (attribute (if (eq key 'background) :background :foreground))
         (theme-color (and (display-graphic-p) face
                           (face-attribute face attribute nil t)))
         (fallback (pcase key
                     ('up financial-chart-svg-fallback-up-color)
                     ('down financial-chart-svg-fallback-down-color)
                     ('background (or financial-chart-svg-background
                                      financial-chart-svg-fallback-background))
                     ('text (or financial-chart-svg-text-color
                                financial-chart-svg-fallback-foreground))
                     (_ (alist-get key financial-chart-svg--fallback-palette)))))
    (or (alist-get key financial-chart-svg-palette)
        (when (and (eq financial-chart-color-palette 'colorblind-safe)
                   (memq key '(up down)))
          (if (eq key 'up) "#0072B2" "#D55E00"))
        (and (not (financial-chart--color-unspecified-p theme-color))
             (format "%s" theme-color))
        fallback)))

(defconst financial-chart-svg--safe-series-colors
  '("#0072B2" "#D55E00" "#009E73" "#CC79A7"
    "#56B4E9" "#E69F00" "#F0E442" "#000000")
  "Distinct colors used by the colorblind-safe palette for series.")

(defun financial-chart-svg--series-color (index)
  "Return the shared SVG color for zero-based series INDEX."
  (let* ((base (if (eq financial-chart-color-palette 'colorblind-safe)
                   financial-chart-svg--safe-series-colors
                 (list "#2e7d32" "#c62828"
                       "#1565c0" "#8e24aa" "#17becf" "#8c564b"
                       "#e377c2" "#7f7f7f" "#bcbd22")))
         (colors (copy-sequence base))
         (candidate-index 0))
    (while (<= (length colors) index)
      (let* ((hue (mod (* candidate-index 0.618033988749895) 1.0))
             (rgb (color-hsl-to-rgb hue 0.72 0.45))
             (candidate (apply #'color-rgb-to-hex (append rgb '(2)))))
        (setq candidate-index (1+ candidate-index))
        (unless (member candidate colors)
          (setq colors (append colors (list candidate))))))
    (nth index colors)))

(defun financial-chart-svg--series-face-color (spec index)
  "Resolve SPEC's face or shared palette color at series INDEX."
  (if (or (eq financial-chart-color-palette 'colorblind-safe)
          (memq (plist-get spec :face) '(nil default)))
      (financial-chart-svg--series-color index)
    (financial-chart--face-color (plist-get spec :face))))

(defun financial-chart-svg--legend (svg series x y)
  "Draw SERIES labels in SVG at X,Y using the shared fonts and colors."
  (let ((cursor x))
    (cl-loop for spec in series for index from 0
             for color = (financial-chart-svg--series-face-color spec index)
             for label = (format "%s" (or (plist-get spec :label)
                                           (format "Series %d" (1+ index))))
             do (svg-line svg cursor (- y 4) (+ cursor 14) (- y 4)
                          :stroke color :stroke-width 2)
             (financial-chart-svg--text svg label (+ cursor 19) y "start" color)
             (setq cursor (+ cursor 28 (* 7 (string-width label)))))))

(defun financial-chart-svg--n (x)
  "X rounded to two decimals, for stable SVG coordinates."
  (/ (round (* x 100)) 100.0))

(defun financial-chart-svg--element-title (node text)
  "Append a hover TITLE child to SVG DOM NODE and return NODE."
  (when (and (consp node) (consp (cdr node)))
    (setcdr (cdr node)
            (append (cddr node) (list (list 'title nil (format "%s" text))))))
  node)

(defun financial-chart-svg--titled-group (svg title)
  "Add an SVG group to SVG whose hover title is TITLE, and return it."
  (financial-chart-svg--element-title (svg-node svg 'g) title))

(defun financial-chart-svg--point-target (svg x y title &optional radius)
  "Add an invisible circular hover target with TITLE at X,Y in SVG."
  (let ((group (financial-chart-svg--titled-group svg title)))
    (svg-circle group x y (or radius 4)
                :fill "transparent" :stroke "none" :pointer-events "all")
    group))

(defun financial-chart-svg--canvas (width height title)
  "A background-filled WIDTH x HEIGHT svg with optional TITLE text."
  (let ((svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill (financial-chart-svg--color 'background))
    (when title
      (financial-chart-svg--text
       svg title financial-chart-svg-margin-left
       (max (+ financial-chart-svg-font-size 4)
            (/ financial-chart-svg-margin-top 2.0))
       "start" nil (+ financial-chart-svg-font-size 2) "bold"))
    svg))

(defun financial-chart-svg--text (svg text x y anchor &optional color size weight)
  "Add TEXT to SVG at X,Y with ANCHOR in COLOR (default: text colour)."
  (let ((properties
         (list :x (financial-chart-svg--n x)
               :y (financial-chart-svg--n y)
               :font-family financial-chart-svg-font-family
               :font-size (or size financial-chart-svg-font-size)
               :text-anchor anchor
               :fill (or color (financial-chart-svg--color 'text)))))
    (when weight
      (setq properties (append properties (list :font-weight weight))))
    (apply #'svg-text svg text properties)))

(defun financial-chart-svg--string (svg)
  "SVG serialized to a string."
  (with-temp-buffer (svg-print svg) (buffer-string)))

(defun financial-chart-svg--frame (width height title)
  "Plot box (X0 Y0 W H) inside a WIDTH x HEIGHT canvas with TITLE."
  (let* ((top (if title financial-chart-svg-margin-top 10))
         (left financial-chart-svg-margin-left)
         (right financial-chart-svg-margin-right)
         (bottom financial-chart-svg-margin-bottom))
    (list left top (max 1 (- width left right))
          (max 1 (- height top bottom)))))

(defun financial-chart-svg--horizontal-ticks (svg ticks x0 x1)
  "Draw horizontal SVG grid TICKS, each (Y LABEL), with shared styling."
  (dolist (tick ticks)
    (let ((y (car tick))
          (label (cadr tick)))
      (if (eq (nth 2 tick) 'guide)
          (svg-line svg x0 y x1 y :stroke (financial-chart-svg--color 'grid)
                    :stroke-width 0.7 :stroke-dasharray "3 3")
        (svg-line svg x0 y x1 y :stroke (financial-chart-svg--color 'grid)
                  :stroke-width 0.6 :stroke-opacity 0.55))
      (when label
        (financial-chart-svg--text svg label (- x0 6) (+ y 4) "end")))))

(defun financial-chart-svg--vertical-ticks (svg ticks x0 y0 width height)
  "Draw normalized X TICKS (POSITION LABEL) along an SVG plot."
  (dolist (tick ticks)
    (let* ((position (car tick))
           (x (+ x0 (* width position)))
           (anchor (cond ((<= position 0) "start")
                         ((>= position 1) "end")
                         (t "middle"))))
      (svg-line svg x y0 x (+ y0 height)
                :stroke (financial-chart-svg--color 'grid)
                :stroke-width 0.5 :stroke-opacity 0.25)
      (svg-line svg x (+ y0 height) x (+ y0 height 5)
                :stroke (financial-chart-svg--color 'grid) :stroke-width 0.6)
      (financial-chart-svg--text svg (cadr tick) x (+ y0 height 17) anchor))))

(defun financial-chart-svg--series-x-ticks (series width)
  "Return date or coordinate ticks for SERIES across plot WIDTH."
  (or (financial-chart-series-x-axis-labels
       series (max 2 (floor (/ width (* financial-chart-svg-font-size 1.2)))))
      (let* ((points (append series nil))
             (count (length points))
             (indexes (delete-dups (list 0 (/ (1- count) 2) (1- count)))))
        (when (> count 0)
          (mapcar
           (lambda (index)
             (let* ((point (nth index points))
                    (x (financial-chart-series--point-x point)))
               (list (/ index (float (max 1 (1- count))))
                     (cond
                      ((and (numberp x) (> x 1e11))
                       (format-time-string financial-chart-x-axis-format (/ x 1000.0)))
                      ((numberp x) (financial-chart-fmt x))
                      (x (format "%s" x))
                      (t (number-to-string (1+ index)))))))
           indexes)))))

(defun financial-chart-svg--series-point-label (point index)
  "Return the X-coordinate label for POINT at source INDEX."
  (let ((x (financial-chart-series--point-x point)))
    (cond
     ((and (numberp x) (> x 1e11))
      (format-time-string financial-chart-x-axis-format (/ x 1000.0)))
     (x (format "%s" x))
     (t (format "Point %d" (1+ index))))))

(defun financial-chart-svg--series-x-axis (svg ticks x0 y0 width height text-color)
  "Draw date TICKS below an SVG series plot."
  (ignore text-color)
  (financial-chart-svg--vertical-ticks svg ticks x0 y0 width height))

(defun financial-chart-svg--series (series style width height unit title scale)
  "Render SERIES with STYLE (`area' or `line') in a shared SVG frame."
  (let ((values (financial-chart-series-values series)))
    (when values
      (financial-chart-series-validate-scale series scale)
      (pcase-let* ((`(,x0 ,y0 ,w ,frame-h)
                    (financial-chart-svg--frame width height title))
                   (x-ticks (financial-chart-svg--series-x-ticks series w))
                   (h (if x-ticks (max 1 (- frame-h 18)) frame-h))
                   (raw-cols (financial-chart-series-resample
                              series (max 2 (floor w 2))))
                   (range-values
                    (if (financial-chart-series-x-aware-p series) values raw-cols))
                   (cols (mapcar (lambda (value)
                                   (financial-chart-series-scale-value value scale))
                                 raw-cols))
                   (`(,lo . ,hi)
                    (financial-chart-range
                     (mapcar (lambda (value)
                               (financial-chart-series-scale-value value scale))
                             range-values)))
                   (span (financial-chart-series-scale-span lo hi scale))
                   (n (max 1 (1- (length cols))))
                   (source-xs (financial-chart-series-xs series))
                   (source-x0 (car source-xs))
                   (source-x-span (and (financial-chart-series-x-aware-p series)
                                       (- (car (last source-xs)) source-x0)))
                   (color (financial-chart-svg--color
                           (if (eq (financial-chart-direction-face cols 'up 'down) 'up)
                               'up 'down)))
                   (points
                    (cl-loop for value in cols for index from 0
                             collect
                             (cons (financial-chart-svg--n
                                    (+ x0 (* w (/ index (float n)))))
                                   (financial-chart-svg--n
                                    (+ y0 (* h (- 1 (/ (- value lo) span))))))))
                   (y-ticks
                    (mapcar (lambda (value)
                              (list (+ y0 (* h (- 1 (/ (- value lo) span))))
                                    (concat
                                     (financial-chart-fmt
                                      (financial-chart-series-unscale-value value scale))
                                     unit)))
                            (financial-chart--axis-label-values lo hi 3)))
                   (svg (financial-chart-svg--canvas width height title)))
        (financial-chart-svg--horizontal-ticks svg y-ticks x0 (+ x0 w))
        (when (eq style 'area)
          (svg-polygon
           svg (append points
                       (list (cons (car (car (last points))) (+ y0 h))
                             (cons (car (car points)) (+ y0 h))))
           :fill color :fill-opacity 0.15 :stroke "none"))
        (svg-polyline svg points :fill "none" :stroke color :stroke-width 1.5)
        (cl-loop for point in (append series nil) for index from 0
                 for point-x = (financial-chart-series--point-x point)
                 for value = (financial-chart-series--point-y point)
                 when (numberp value)
                 do (let* ((x (financial-chart-svg--n
                               (+ x0 (* w (if (and source-x-span (numberp point-x))
                                              (/ (float (- point-x source-x0))
                                                 source-x-span)
                                            (/ index
                                               (float (max 1 (1- (length series))))))))))
                           (scaled (financial-chart-series-scale-value value scale))
                           (y (+ y0 (* h (- 1 (/ (- scaled lo) span))))))
                      (financial-chart-svg--point-target
                       svg x y
                       (format "%s: %s%s"
                               (financial-chart-svg--series-point-label point index)
                               (financial-chart-fmt value) unit))))
        (when x-ticks
          (financial-chart-svg--series-x-axis svg x-ticks x0 y0 w h nil))
        (financial-chart-svg--text
         svg (format "last %s%s   %d pts"
                     (financial-chart-fmt (car (last values))) unit (length values))
         (+ x0 w) (+ y0 h (if x-ticks 34 18)) "end")
        (financial-chart-svg--string svg)))))

(cl-defun financial-chart-svg-area
    (series &key (width 600) (height 240) (unit "") title (scale 'linear)
            &allow-other-keys)
  "SVG of SERIES as a filled area chart with a shared axis and grid."
  (financial-chart-svg--series series 'area width height unit title scale))

(cl-defun financial-chart-svg-line
    (series &key (width 600) (height 240) (unit "") title (scale 'linear)
            &allow-other-keys)
  "SVG of SERIES as an unfilled polyline with a shared axis and grid."
  (financial-chart-svg--series series 'line width height unit title scale))

(cl-defun financial-chart-svg-sparkline
    (series &key (width 600) (height 240) (unit "") title (scale 'linear)
            &allow-other-keys)
  "SVG of SERIES as an unfilled polyline with point hover values."
  (financial-chart-svg--series series 'line width height unit title scale))

(cl-defun financial-chart-svg-payoff
    (payoff &key (width 600) (height 260) (unit "$") title &allow-other-keys)
  "SVG string of PAYOFF ((PRICE PNL) ...) against a zero line, with breakevens."
  (let ((xs (delq nil (financial-chart-series-xs payoff)))
        (ys (financial-chart-series-values payoff)))
    (when (and ys (= (length xs) (length ys)) (cdr xs))
      (pcase-let* ((`(,x0 ,y0 ,w ,h) (financial-chart-svg--frame width height title))
                   (`(,lo . ,hi) (financial-chart-payoff-range payoff))
                   (span (float (max 0.001 (- hi lo))))
                   (`(,plo . ,phi) (financial-chart-range xs))
                   (pspan (float (max 0.001 (- phi plo))))
                   (sx (lambda (p) (financial-chart-svg--n (+ x0 (* w (/ (- p plo) pspan))))))
                   (sy (lambda (v) (financial-chart-svg--n (+ y0 (* h (- 1 (/ (- v lo) span)))))))
                   (zy (funcall sy 0))
                   (y-ticks (mapcar (lambda (value)
                                      (list (funcall sy value)
                                            (financial-chart-fmt-money value unit)))
                                    (list hi 0 lo)))
                   (svg (financial-chart-svg--canvas width height title)))
        (financial-chart-svg--horizontal-ticks svg y-ticks x0 (+ x0 w))
        (cl-loop for (p0 p1) on xs for (v0 v1) on ys while p1
                 do (let ((segs (if (< (* v0 v1) 0)
                                    (let ((pc (+ p0 (* (- p1 p0) (/ (float (- v0)) (- v1 v0))))))
                                      (list (list p0 v0 pc 0) (list pc 0 p1 v1)))
                                  (list (list p0 v0 p1 v1)))))
                      (dolist (s segs)
                        (pcase-let ((`(,a ,va ,b ,vb) s))
                          (svg-polygon svg (list (cons (funcall sx a) zy)
                                                 (cons (funcall sx a) (funcall sy va))
                                                 (cons (funcall sx b) (funcall sy vb))
                                                 (cons (funcall sx b) zy))
                                       :fill (financial-chart-svg--color (if (>= (+ va vb) 0) 'up 'down))
                                       :fill-opacity 0.25 :stroke "none")))))
        (svg-line svg x0 zy (+ x0 w) zy :stroke (financial-chart-svg--color 'grid)
                  :stroke-dasharray "4 3")
        (svg-polyline svg (cl-mapcar (lambda (p v) (cons (funcall sx p) (funcall sy v))) xs ys)
                      :fill "none" :stroke (financial-chart-svg--color 'text) :stroke-width 1.5)
        (cl-loop for p in xs for v in ys
                 do (financial-chart-svg--point-target
                     svg (funcall sx p) (funcall sy v)
                     (format "Price %s, P/L %s"
                             (financial-chart-fmt p)
                             (financial-chart-fmt-money v unit))))
        (dolist (be (financial-chart-payoff-breakevens payoff))
          (svg-line svg (funcall sx be) y0 (funcall sx be) (+ y0 h)
                    :stroke (financial-chart-svg--color 'grid) :stroke-dasharray "2 3")
          (financial-chart-svg--text svg (concat unit (financial-chart-fmt be)) (funcall sx be) (+ y0 h 14)
                              "middle"))
        (financial-chart-svg--string svg)))))

(cl-defun financial-chart-svg-bars
    (bars &key (width 600) (row-height 20) (unit "") title &allow-other-keys)
  "SVG string of BARS ((LABEL . VALUE) ...) as diverging horizontal bars."
  (when bars
    (pcase-let* ((height (+ (if title financial-chart-svg-margin-top 10)
                            financial-chart-svg-margin-bottom
                            (* row-height (length bars))))
                 (`(,x0 ,y0 ,w ,_h) (financial-chart-svg--frame width height title))
                 (x0 (+ x0 40))
                 (w (- w 80))
                 (mid (+ x0 (/ w 2.0)))
                 (plot-height (* row-height (length bars)))
                 (peak (max 1e-9 (apply #'max (mapcar (lambda (b) (abs (cdr b))) bars))))
                 (svg (financial-chart-svg--canvas width height title)))
      (financial-chart-svg--vertical-ticks
      svg `((0 ,(financial-chart-fmt-money (- peak) unit))
             (0.5 "0")
             (1 ,(financial-chart-fmt-money peak unit)))
       x0 y0 w plot-height)
      (cl-loop for (label . v) in bars for i from 0
               for y = (+ y0 (* i row-height))
               for len = (financial-chart-svg--n (* (/ w 2.0) (/ (abs v) (float peak))))
               do (let ((group
                         (financial-chart-svg--element-title
                          (svg-node svg 'g)
                          (format "%s: %s" label (financial-chart-fmt-money v unit)))))
                    (svg-rectangle group (if (< v 0) (- mid len) mid) (+ y 3) len
                                   (- row-height 6)
                                   :fill (financial-chart-svg--color
                                          (if (< v 0) 'down 'up)))
                    (financial-chart-svg--text group (format "%s" label)
                                               (- x0 6) (+ y (* 0.7 row-height)) "end")
                    (financial-chart-svg--text group (financial-chart-fmt-money v unit)
                                               (+ x0 w 6) (+ y (* 0.7 row-height)) "start")))
      (svg-line svg mid y0 mid (+ y0 (* row-height (length bars)))
                :stroke (financial-chart-svg--color 'grid))
      (financial-chart-svg--string svg))))

(cl-defun financial-chart-svg-ohlc (bars &key title &allow-other-keys)
  "SVG string of OHLC BARS as candlesticks with TITLE.
The `ohlc' kind's adapter over `financial-chart-render-svg'."
  (financial-chart-render-svg bars title))

(provide 'financial-chart-svg)
;;; financial-chart-svg.el ends here
