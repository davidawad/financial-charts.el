;;; financial-chart.el --- OHLC candlestick charts, rendered in-buffer -*- lexical-binding: t; -*-

;; Author: David Awad
;; Keywords: comm, tools, finance

;;; Commentary:

;; Pure-Elisp candlestick chart renderer, two output forms:
;;
;; 1. `financial-chart-render'/`-view' -- a unicode candlestick chart
;;    (with an optional volume pane and X-axis) directly in an Emacs
;;    buffer. No gnuplot, no external process.
;; 2. `financial-chart-render-svg'/`-export-svg' -- a real vector SVG
;;    candlestick chart (actual rectangles/lines, not glyphs), built
;;    with Emacs's own `svg.el' -- still no external process.
;;    `-export-png' rasterizes that SVG to a PNG file via whichever of
;;    rsvg-convert/ImageMagick is available -- the one place in this
;;    file that shells out, because rasterizing vector graphics is not
;;    something Elisp can do on its own.
;;
;; Both renderers share the same configuration surface and the same
;; data-prep code (bar windowing, scale conversion, price range) so
;; they can never drift out of sync with each other.
;;
;; Every visual/behavioral knob is a `defcustom' -- height, bar-count
;; window, candle width/gap, colors, glyph characters, linear/log price
;; scale, axis label counts/formats, volume pane, X-axis, and overlay
;; indicators. Nothing about the chart's appearance is hardcoded into
;; the rendering functions; a caller who wants a one-off override
;; `let'-binds the relevant variable(s) around a `financial-chart-render'
;; call rather than passing a long positional argument list -- standard
;; Emacs idiom for widely-configurable rendering code.
;;
;; Data-source agnostic: `financial-chart-render'/`financial-chart-view'
;; take a plain list of (:open :high :low :close &optional :volume :time)
;; plists, oldest first. :volume feeds the optional volume pane; :time
;; (epoch milliseconds) feeds the optional X-axis; both are omittable,
;; and the chart degrades gracefully (no volume pane without :volume
;; data present in ANY bar; no X-axis without :time data).
;;
;; The Schwab bridge at the bottom is soft-wired via `fboundp' so this
;; file never hard-requires schwab-broker.el -- callers who never load
;; it get a renderer that still works against any other bar source
;; (Alpaca, a CSV import, synthetic data for testing, ...).

;;; Code:

(require 'financial-chart-core)
(require 'financial-chart-indicators)
(require 'financial-chart-text)
(require 'financial-chart-svg)
(require 'financial-chart-symbol)
(require 'financial-chart-presets)

(provide 'financial-chart)
;;; financial-chart.el ends here
