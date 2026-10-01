# financial-chart.el

Financial charts in Emacs from plain Lisp data. One call draws any chart
kind (candlesticks, area, braille line, sparkline, option payoff,
diverging P/L bars) as unicode text in a terminal frame or as an SVG
image in a GUI frame. Pure Elisp on Emacs's built-in `svg.el`; the only
external process is optional PNG export.

```
   +$50 █▅▂               ▂▅█
      0 ███▆▃───────────▃▆███
              █████████      
               ▀█████▀       
                ▀███▀        
  -$100          ▔█▔         
        $90              $110

        breakeven $95, $105   max +$50 / -$100   5 pts
```

Requires Emacs 29.1+. No dependencies. Optional:
market-data.el (companion package) for
charting ticker symbols from a broker
([schwab-broker.el](https://github.com/davidawad/schwab-broker.el),
[alpaca-broker.el](https://github.com/davidawad/alpaca-broker.el)).

## Install

With Emacs 30's `use-package :vc`:

```elisp
(use-package financial-chart
  :vc (:url "https://github.com/davidawad/financial-chart.el"))
```

or put the directory on `load-path` and `(require 'financial-chart)`.

## Use

```elisp
;; a string: SVG in a GUI frame, unicode text in a terminal
(financial-chart-plot 'area '((1 40.0) (2 45.0) (3 50.0) (4 42.0)) :unit "$")

;; at point, or in a buffer (g re-renders to the window, t flips text/SVG)
(financial-chart-plot-insert 'payoff '((90 50) (95 0) (100 -100) (105 0) (110 50)))
(financial-chart-plot-view 'bars '(("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950))
                           :title "open P/L" :unit "$")

;; candlesticks with volume panel, X-axis and indicator overlays
(financial-chart-plot-view 'ohlc
  '((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
    (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000)))

;; every kind over sample data
(financial-chart-demo)
```

| Kind | Data shape | Example |
|---|---|---|
| `area`, `line`, `sparkline` | series: numbers, `(X Y)` or `(X . Y)`, oldest first | `'((1 40.0) (2 45.0))` |
| `payoff` | `(PRICE PNL)` sorted by price | `'((90 50) (100 -100) (110 50))` |
| `bars` | `(LABEL . VALUE)` | `'(("AAPL" . 1200) ("TSLA" . -950))` |
| `ohlc` | `(:open :high :low :close [:volume] [:time])` plists, `:time` in epoch ms | see above |

Common props: `:backend` (`text`, `svg`, `auto`; default
`financial-chart-backend`), `:width`/`:height` (text columns/rows),
`:pixel-width`/`:pixel-height` (SVG), `:unit`, `:title`, and
`:up-face`/`:down-face`/`:dim-face`/`:accent-face` for the text kinds.
A chart is also plain data, `(:kind area :data ... :unit "$")`, which
`financial-chart-plot-spec` renders.

## For programs and agents

Every part of the package can be listed, checked and planned before
anything is drawn:

```elisp
(financial-chart-list-kinds)            ; kinds with shape and doc
(financial-chart-describe-kind 'payoff) ; shape doc, example data, renderers
(financial-chart-validate 'payoff data) ; t, or financial-chart-invalid-data with :index
(financial-chart-explain 'area data :backend 'svg) ; renderer, args, backend and why; draws nothing
(financial-chart-describe)              ; the whole package as JSON-ready data
(financial-chart-doctor)                ; M-x: does every kind render here?
(financial-chart-register-kind 'my-kind :shape 'series :text #'my-text :svg #'my-svg :doc "...")
```

Errors are typed (`financial-chart-unknown-kind`,
`financial-chart-invalid-data`, parent `financial-chart-error`); their
message says what to do and their data carries `:code` and, for bad
data, the offending `:index`. SVG output carries a `<title>` and a
`<desc>` stating the kind, point count and value range.

From outside Emacs, `bin/financial-chart` takes the same spec as JSON:

```sh
bin/financial-chart example payoff                 # a spec to start from
bin/financial-chart example payoff | bin/financial-chart render -
echo '{"kind":"bars","data":{"AAPL":1200,"TSLA":-950},"unit":"$","backend":"svg"}' \
  | bin/financial-chart render - > pnl.svg
bin/financial-chart explain spec.json   # the plan, as JSON
bin/financial-chart kinds | describe | doctor
```

Failures print `{"ok":false,"error":{"code":...,"message":...}}` and exit 1.

## Candlesticks

`financial-chart-render` (text) and `financial-chart-render-svg` take a
list of OHLC plists directly, and `financial-chart-view` pops a buffer.
Every visual aspect of a candlestick chart is a `defcustom`;
`let`-bind one for a one-off override.

### Configuration

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

### Built-in indicators

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

### SVG / PNG export

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

## Ticker symbols, presets and cohorts

With market-data.el and a
broker package loaded, charts can be fetched by symbol. market-data
picks the provider; nothing here names a broker.

```elisp
(financial-chart-view-symbol "AAPL" :period-type "day" :period 5
                             :frequency-type "minute" :frequency 5)
(financial-chart-explain-symbol "AAPL")          ; provider decision + render config, no fetch
(financial-chart-export-symbol-svg "AAPL" "aapl.svg")

(financial-chart-list-presets)                   ; daytrade, swing, options-memo
(financial-chart-resolve-preset 'swing "AAPL")   ; the full plan, no network
(financial-chart-view-preset "AAPL" 'swing)

(financial-chart-list-cohorts)                   ; named indicator sets
(financial-chart-describe-cohort 'trend-following)
```

Presets (`financial-chart-presets`) and cohorts
(`financial-chart-indicator-cohorts`) are data: add an entry to add
one. Titles of symbol charts state the symbol, provider, period, bar
count and fetch time. Cohort members may name recipes in an external
indicator catalog; set `financial-chart-indicator-catalog-function` to
a lookup function and `financial-chart-describe-cohort` will check them
against it.

## Bringing data from other packages

This package draws; it never fetches or talks to a broker. The
boundary is the data shapes above, so anything that produces them can
be charted:

- **OHLC bars** are the bar/v1 plist `(:open :high :low :close
  [:volume] [:time])`, `:time` in epoch milliseconds. market-data.el
  defines that shape and converts broker responses into it; when
  market-data is loaded, `financial-chart-validate` uses its validator,
  so the two packages cannot disagree about what a bar is.
- **By ticker**, `financial-chart-view-symbol` and the presets ask
  market-data for bars. Provider choice, request defaults
  (`market-data-default-period` etc.) and authentication belong to
  market-data and the broker packages, and none of them are duplicated
  here.
- **Anything else** (positions, P/L, payoff curves, a CSV, a CLI's
  JSON) needs only a small converter in the package that owns that data,
  producing a series, payoff or labeled list for `financial-chart-plot`.
  From outside Emacs, emit a JSON spec for `bin/financial-chart`.
- **New chart types** register with `financial-chart-register-kind`; the
  doctor, `describe` and the CLI pick them up.

## Layout

| File | What |
|---|---|
| `financial-chart.el` | entry: requires everything, `describe`, `doctor` |
| `financial-chart-core.el` | customization group, candle defcustoms, scale and windowing |
| `financial-chart-series.el` | data shapes, faces, resampling, formatting, breakevens |
| `financial-chart-indicators.el` | SMA/EMA/RSI/VWAP, overlays, cohorts |
| `financial-chart-text.el` | text renderers |
| `financial-chart-svg.el` | SVG renderers, SVG/PNG export |
| `financial-chart-plot.el` | kind and shape registries, `plot`, `validate`, `explain`, plot buffers |
| `financial-chart-symbol.el` | charts by ticker through market-data.el |
| `financial-chart-presets.el` | named presets |
| `financial-chart-batch.el`, `bin/financial-chart` | JSON command line |

## Tests

```sh
make test      # every test/*-test.el, offline, no display
make compile   # byte-compile with warnings as errors
make test MARKET_DATA=../market-data.el   # same suite with market-data loaded
```

Golden text and SVG fixtures are in `test/fixtures/`. After an intended
visual change, regenerate them with `FINANCIAL_CHART_UPDATE_GOLDEN=1
make test` and review the diff. Trailing spaces in fixtures are data;
`.gitattributes` and `.editorconfig` keep tools from stripping them.

## License

MIT — see [LICENSE](LICENSE).
