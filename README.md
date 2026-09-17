# financial-chart.el

A pure-Elisp OHLC candlestick chart renderer, two output forms:

1. **In-buffer** — unicode box-drawing/block characters at half-block
   vertical resolution, `propertize` faces for up/down coloring. No
   gnuplot, no external process.
2. **SVG/PNG** — a real vector chart (actual rectangles and lines, not
   glyphs), built with Emacs's own `svg.el`. Still no external process
   for the SVG itself; PNG export shells out to whichever of
   `rsvg-convert`/ImageMagick is installed, because rasterizing vector
   graphics genuinely isn't something Elisp can do on its own.

Both share the same configuration surface and the same data-prep code
(bar windowing, scale conversion, price range), so they can't drift out
of sync with each other. Requires Emacs 27.1+.

Data-source agnostic: the renderer takes a plain list of
`(:open :high :low :close &optional :volume :time)` plists, oldest bar
first. It doesn't know or care where those bars came from. `:volume`
and `:time` are both optional and independently gate an optional
volume panel and X-axis — omit either (or both) and the chart degrades
gracefully rather than erroring or drawing an empty panel.

Every visual and behavioral aspect of the chart is a `defcustom` — see
**Configuration** below. Nothing is hardcoded into the rendering
functions; override anything for one call by `let`-binding the
relevant variable(s) around it.

## Install

Copy `financial-chart.el` somewhere on your `load-path`, then:

```elisp
(require 'financial-chart)
```

## Use

```elisp
(financial-chart-view
 '((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
   (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000)
   (:open 101.5 :high 102 :low 96 :close 97 :volume 21000 :time 1700172800000))
 "MY SYMBOL")
```

pops a `*financial-chart*` buffer with a candlestick render (volume
panel and X-axis included automatically, since these bars carry
`:volume`/`:time`). Call `financial-chart-render` directly instead if
you want the chart as a plain string (e.g. to embed elsewhere, such as
in an org-mode `#+begin_example` block) rather than a popped buffer.

## Configuration

All `financial-chart-*` custom variables (`M-x customize-group
financial-chart`):

**Size** — `financial-chart-height` (price panel rows, default 20),
`financial-chart-max-bars` (window to the most recent N bars, default
80; nil/0 = unlimited — this is what keeps a 1000-bar pull from
rendering as 1000 unreadable columns), `financial-chart-candle-width`
(columns per candle, default 1), `financial-chart-candle-gap` (blank
columns between candles, default 1).

**Colors** — `financial-chart-up-face`/`-down-face` (default
`success`/`error`), `financial-chart-wick-face` (default nil = reuse
the candle's own face), `financial-chart-axis-face` (default nil =
default face).

**Glyphs** — `financial-chart-glyph-full-block`/`-upper-half`/
`-lower-half`/`-wick`/`-empty`/`-indicator`/`-volume-bar` (all
characters, defaulting to unicode block/box-drawing glyphs — override
any of these for an ASCII-only terminal).

**Price scale** — `financial-chart-scale` (`linear` or `log`, default
`linear`), `financial-chart-axis-label-count` (default 3),
`financial-chart-axis-format` (default `"%7.2f "`).

**Volume panel** — `financial-chart-show-volume` (default t; only
actually renders when at least one bar has `:volume`),
`financial-chart-volume-height` (default 5),
`financial-chart-volume-up-face`/`-down-face` (default nil = reuse the
price panel's up/down faces), `financial-chart-volume-axis-label-count`
(default 2), `financial-chart-volume-axis-format` (default `"%7.0f "`).

**X-axis** — `financial-chart-show-x-axis` (default t; only actually
renders when at least one bar has `:time`),
`financial-chart-x-axis-label-count` (default 4),
`financial-chart-x-axis-format` (a `format-time-string` string, default
`"%m/%d"`).

**Overlay indicators** — `financial-chart-indicators`, a list of plists
`(:fn FN :face FACE :glyph CHAR)`. `FN` takes the (already-windowed)
bars list and returns a same-length list of numbers-or-nil on the same
price scale as the candles; each non-nil value is drawn at its row.
Empty by default (no overlays drawn unless you configure one):

```elisp
(setq financial-chart-indicators
      (list (list :fn (lambda (bars) (financial-chart-sma bars 20))
                  :face 'font-lock-keyword-face)))
```

## Built-in indicators

- `financial-chart-sma`/`-ema` `(bars &optional window field)` — moving
  average of `:close` (or `FIELD`) over `WINDOW` bars (default 20). On
  the price scale — use directly as a `financial-chart-indicators` `:fn`.
- `financial-chart-vwap` `(bars)` — cumulative volume-weighted average
  price (typical price × volume). Also on the price scale, also a
  direct `:fn`. VWAP conventionally resets daily — pass one session's
  bars, not a multi-day history, unless you deliberately want a running
  VWAP across the whole window.
- `financial-chart-rsi` `(bars &optional period field)` — simple-average
  RSI (default period 14), values in [0,100]. **Not** on the price
  scale — do not pass it straight to `financial-chart-indicators`, it
  will render invisible or nonsensical against the price panel's own
  axis. Call it directly for a table/memo, or build a separate
  oscillator sub-panel with its own 0-100 scale (analogous to the
  volume panel) if you want it charted.

None of these are native to Schwab's or Alpaca's APIs — both give raw
OHLCV only (Alpaca's bars do include a native `vw` VWAP field per bar,
`(alist-get 'vw bar)`, if you'd rather use the broker's own number than
recompute it). RSI is never native to any broker API; it's always
computed from closes, here or anywhere else.

## SVG / PNG export

```elisp
(financial-chart-export-svg bars "chart.svg" "MY SYMBOL")   ; pure Elisp
(financial-chart-export-png bars "chart.png" "MY SYMBOL")   ; shells out to rasterize

;; override the font for one call:
(financial-chart-export-png bars "chart.png" "MY SYMBOL" nil nil "Hack")

;; or set it machine-wide, e.g. in your init file:
(setq financial-chart-svg-font-family "Hack")
```

`financial-chart-render-svg` returns the SVG as a string, if you want it
without writing a file (e.g. to embed in HTML). The SVG renderer shares
`financial-chart-render`'s configuration (bar window, colors, scale,
volume panel, X-axis, indicators) plus its own size/margin knobs
(`financial-chart-svg-candle-width`/`-gap`, `-price-height`,
`-volume-height`, `-margin-left`/`-right`/`-top`/`-bottom`,
`-font-size`) and background/text color overrides
(`financial-chart-svg-background`/`-text-color`, both nil by default —
derived from your current theme).

**Font:** `financial-chart-svg-font-family` defaults to a widely-available
open-source monospace stack (`"DejaVu Sans Mono, Menlo, Consolas,
monospace"`) that doesn't assume any one specific font is installed on
whatever machine ends up rasterizing the SVG. Set it to your own
preferred font for every chart (`(setq financial-chart-svg-font-family
"Hack")`), or pass a `FONT-FAMILY` argument to `financial-chart-render-svg`/
`-export-svg`/`-export-png` to override it for one call only, e.g.
`(financial-chart-export-png bars file title nil nil "Hack")`.

`financial-chart-export-png` renders to SVG first, then rasterizes via
`financial-chart-png-converter` (nil auto-detects `rsvg-convert` /
ImageMagick's `convert`/`magick` via `executable-find`, in that order;
set it to a symbol to force one, or to a function of `(SVG-FILE
PNG-FILE)` to convert some other way entirely). This is the one place
in this file that shells out to an external process.

**Themeless-session fallback colors:** if a face's color can't be
resolved to anything real (Emacs's literal `"unspecified"` placeholder
— happens in `emacs -Q --batch`, CI, or any session with no theme
loaded), the SVG/PNG renderer falls back to
`financial-chart-svg-fallback-foreground`/`-background`/`-up-color`/
`-down-color` instead of silently passing garbage into the SVG (which
rasterizes as a solid black image). A themed GUI session never hits
this path — your actual theme colors are used.

## Schwab bridge

If [schwab-broker.el](https://github.com/davidawad/schwab-broker.el)
is loaded:

```elisp
(financial-chart-schwab-view
 "AAPL" :period-type "day" :period 5 :frequency-type "minute" :frequency 5)
(financial-chart-schwab-export-svg "AAPL" "aapl.svg")
(financial-chart-schwab-export-png "AAPL" "aapl.png")
```

fetch real price history and render/export it directly.
`M-x financial-chart-schwab-view`/`-export-svg`/`-export-png` (a symbol
prompt, plus a file prompt for the export commands, defaulting under
`financial-chart-export-directory` — `~/Desktop` by default) use
`financial-chart-schwab-default-period-type`/`-period`/
`-frequency-type`/`-frequency` (default: 1 month of daily bars) for
whichever keys you don't supply — each explicit key you pass overrides
only that one default, the rest still apply.

This is a soft dependency (checked via `fboundp` at call time, not
`require`d) — `financial-chart.el` never needs schwab-broker.el loaded
to do anything else.

## Tests

ERT tests live in `test/financial-chart-test.el`, pure logic (no
network, no Emacs display needed). Run standalone:

```sh
emacs -Q --batch -L . -l financial-chart.el -l test/financial-chart-test.el \
  -f ert-run-tests-batch-and-exit
```

## License

MIT — see [LICENSE](LICENSE).
