;;; financial-chart-eas-themes.el --- declarative colour themes for composed charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; URL: https://github.com/davidawad/financial-charts.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A chart's "theme" (fc-gty.31, fc-gty.34) names a theme or is an
;; object of theme keys, over the theme its "base" names:
;;
;;   "theme": "gruvbox"
;;   "theme": {"base": "dark", "up": "#00c853", "palette": ["#ffd600"]}
;;
;; Keys: background, foreground, grid, muted (levels, zones, study
;; rules), up, down, price, palette (series colours, in order) and halo
;; (under pinned labels; default the background, "none" when it is
;; "transparent").  A key a theme leaves out keeps the package's value
;; (`financial-chart-palette' and friends); the chart's "colors" still
;; win over the theme.  Presets may set "theme" too; an object theme
;; over a preset's named one keeps that name as its base.
;;
;; With no "theme", an image (SVG, kitty) takes
;; `financial-chart-image-theme' and text takes
;; `financial-chart-text-theme'.  The text default, "terminal", draws
;; in the terminal's own colours: up, down and the series are ANSI
;; colour names ("green", "brightblue"), which a tty frame maps through
;; the terminal's palette and a graphic frame through the ansi-color
;; faces, so the chart follows whatever theme the terminal has.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'financial-chart-eas-palette)
(require 'financial-chart-eas-series)

(defvar financial-chart-label-halo-color)
(defvar financial-chart-annotation-color)
(defvar financial-chart-catalog-zone-color)
(defvar financial-chart-studies-rule-color)
(defvar financial-chart-studies-ribbon-colors)

(defconst financial-chart-themes
  '(("default"
     :doc "The package palette on eas's default (light) background.")
    ("dark"
     :doc "TradingView-like dark: light ink on a near-black pane."
     :background "#131722" :foreground "#d1d4dc" :grid "#2a2e39" :muted "#787b86"
     :up "#26a69a" :down "#ef5350" :price "#b2b5be"
     :palette ("#5b8cff" "#ffb74d" "#ce93d8" "#4db6ac" "#f06292" "#bcaaa4"
               "#4dd0e1" "#d4e157" "#7986cb" "#ff8a65" "#90a4ae" "#aed581"))
    ("transparent"
     :doc "The package palette with no background, for a page's own."
     :background "transparent")
    ("gruvbox"
     :doc "Gruvbox dark (https://github.com/morhetz/gruvbox)."
     :background "#282828" :foreground "#ebdbb2" :grid "#3c3836" :muted "#928374"
     :up "#b8bb26" :down "#fb4934" :price "#d5c4a1"
     :palette ("#83a598" "#fe8019" "#d3869b" "#8ec07c" "#fabd2f" "#bdae93"
               "#458588" "#d65d0e" "#b16286" "#689d6a" "#d79921" "#a89984"))
    ("gruvbox-light"
     :doc "Gruvbox light."
     :background "#fbf1c7" :foreground "#3c3836" :grid "#ebdbb2" :muted "#7c6f64"
     :up "#79740e" :down "#9d0006" :price "#504945"
     :palette ("#076678" "#af3a03" "#8f3f71" "#427b58" "#b57614" "#7c6f64"
               "#458588" "#d65d0e" "#b16286" "#689d6a" "#d79921" "#928374"))
    ("terminal"
     :doc "The terminal's own colours: ANSI names, no background."
     :up "green" :down "red" :price "unspecified-fg" :muted "brightblack"
     :palette ("blue" "yellow" "magenta" "cyan" "brightblue" "brightyellow"
               "brightmagenta" "brightcyan")))
  "Shipped themes: (NAME . KEYS); see the commentary for the keys.")

(defconst financial-chart-theme-keys
  '(:base :background :foreground :grid :muted :up :down :price :palette :halo :doc)
  "Keys a theme object may have.")

(defcustom financial-chart-image-theme "default"
  "Theme of image renders (SVG, kitty) of a chart that names none."
  :type `(choice ,@(mapcar (lambda (e) (list 'const (car e))) financial-chart-themes) string)
  :group 'financial-chart-palette)

(defcustom financial-chart-text-theme "terminal"
  "Theme of text renders of a chart that names none."
  :type `(choice ,@(mapcar (lambda (e) (list 'const (car e))) financial-chart-themes) string)
  :group 'financial-chart-palette)

(defvar financial-chart-theme-backend nil
  "The backend a chart is being composed for: `text', else an image.
`financial-chart-compose-render' binds it; it picks the default theme.")

(defun financial-chart-theme-names ()
  "Names of the shipped themes."
  (mapcar #'car financial-chart-themes))

(defun financial-chart-theme--object (theme path seen)
  "THEME (a name or object, at PATH) as a plist of theme keys.
SEEN holds the names being resolved, to refuse a cycle."
  (cond
   ((stringp theme)
    (when (member theme seen)
      (financial-chart-series-fail path "THEME_CYCLE" "Theme %s is its own base" theme))
    (let ((entry (assoc theme financial-chart-themes)))
      (unless entry
        (financial-chart-series-fail path "UNKNOWN_THEME" "Unknown theme %S; themes: %s" theme
                                     (mapconcat #'identity (financial-chart-theme-names) ", ")))
      (financial-chart-theme--object (cdr entry) path (cons theme seen))))
   ((and (listp theme) (keywordp (car theme)))
    (cl-loop for (k _) on theme by #'cddr
             unless (memq k financial-chart-theme-keys)
             do (financial-chart-series-fail (format "%s/%s" path (substring (symbol-name k) 1))
                                             "INVALID_THEME" "Unknown theme key %s; keys: %s"
                                             (substring (symbol-name k) 1)
                                             (mapconcat (lambda (k) (substring (symbol-name k) 1))
                                                        financial-chart-theme-keys ", ")))
    (let ((palette (plist-get theme :palette)))
      (when (and palette (not (and (sequencep palette) (not (stringp palette))
                                   (seq-every-p #'stringp palette))))
        (financial-chart-series-fail (concat path "/palette") "INVALID_THEME"
                                     "A theme's palette is an array of colours, got %S" palette)))
    (let ((base (plist-get theme :base)))
      (append (cl-loop for (k v) on theme by #'cddr
                       unless (memq k '(:base :doc)) append (list k (if (eq k :palette) (append v nil) v)))
              (and base (financial-chart-theme--object base (concat path "/base") seen)))))
   (t (financial-chart-series-fail path "INVALID_THEME"
                                   "A theme is a name (%s) or an object, got %S"
                                   (mapconcat #'identity (financial-chart-theme-names) ", ") theme))))

(defun financial-chart-theme-resolve (theme &optional backend)
  "THEME (a name, an object or nil) as a plist of theme keys.
Nil takes the default theme of BACKEND (`text' or an image)."
  (financial-chart-theme--object
   (or theme (if (eq backend 'text) financial-chart-text-theme financial-chart-image-theme))
   "/theme" nil))

(defun financial-chart-theme--ansi (colour)
  "COLOUR, an ANSI colour name, as the frame shows it.
A graphic frame reads the ansi-color face of the name (themes set
those); a tty frame and batch keep the name, which the terminal maps."
  (or (and (stringp colour) (not noninteractive) (display-graphic-p)
           (let* ((name (if (string-prefix-p "bright" colour)
                            (concat "bright-" (substring colour 6)) colour))
                  (face (intern-soft (concat "ansi-color-" name))))
             (cond ((equal colour "unspecified-fg") (face-foreground 'default nil t))
                   ((and face (facep face))
                    (let ((c (face-foreground face nil t)))
                      (and (stringp c) (not (string-prefix-p "unspecified" c)) c))))))
      colour))

(defun financial-chart-theme--colours (theme)
  "THEME's colours with ANSI names mapped by `financial-chart-theme--ansi'."
  (cl-loop for (k v) on theme by #'cddr
           append (list k (cond ((stringp v) (financial-chart-theme--ansi v))
                                ((consp v) (mapcar #'financial-chart-theme--ansi v))
                                (t v)))))

(defun financial-chart-theme-halo (theme)
  "The halo colour under pinned labels in THEME: \"none\" on transparent."
  (let ((bg (plist-get theme :background)))
    (or (plist-get theme :halo)
        (cond ((equal bg "transparent") "none")
              ((and (stringp bg) (string-prefix-p "#" bg)) bg)
              ((plist-member theme :background) "none")
              (t "#fcfcfb")))))

(defun financial-chart-theme-config (theme)
  "The eas spec \"config\" of THEME, or nil when it sets no frame colours."
  (let ((bg (plist-get theme :background)) (fg (plist-get theme :foreground))
        (grid (plist-get theme :grid)) (muted (or (plist-get theme :muted) (plist-get theme :foreground))))
    (when (or bg fg)
      (append
       (when bg (list :background bg))
       (when fg
         (list :axis (append (list :labelColor fg :titleColor fg)
                             (when muted (list :domainColor muted :tickColor muted))
                             (when grid (list :gridColor grid)))
               :legend (list :labelColor fg :titleColor fg)
               :title (list :color fg)))))))

(defun financial-chart-theme-call (theme fn)
  "Call FN with THEME's colours in force (see the commentary).
Bind the palette, up, down and price colours, the levels' and zones'
colour and the halo, for the duration of FN."
  (let* ((theme (financial-chart-theme--colours theme))
         (muted (plist-get theme :muted))
         (palette (plist-get theme :palette))
         (financial-chart-palette (or palette financial-chart-palette))
         (financial-chart-palette-up (or (plist-get theme :up) financial-chart-palette-up))
         (financial-chart-palette-down (or (plist-get theme :down) financial-chart-palette-down))
         (financial-chart-palette-price (or (plist-get theme :price) financial-chart-palette-price))
         (financial-chart-label-halo-color (financial-chart-theme-halo theme))
         (financial-chart-annotation-color (or muted financial-chart-annotation-color))
         (financial-chart-catalog-zone-color (or muted financial-chart-catalog-zone-color))
         (financial-chart-studies-rule-color (or muted financial-chart-studies-rule-color))
         (financial-chart-studies-ribbon-colors
          ;; The ribbon blends between two hex colours.
          (let ((ends (and palette (list (car palette) (or (nth 4 palette) (nth 1 palette))))))
            (if (and ends (cl-every (lambda (c) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" c)) ends))
                ends
              financial-chart-studies-ribbon-colors))))
    (funcall fn theme)))

(defun financial-chart-theme-describe ()
  "The shipped themes as JSON-ready data."
  (vconcat (mapcar (lambda (e) (append (list :name (car e))
                                       (cl-loop for (k v) on (cdr e) by #'cddr
                                                append (list k (if (consp v) (vconcat v) v)))))
                   financial-chart-themes)))

(provide 'financial-chart-eas-themes)
;;; financial-chart-eas-themes.el ends here
