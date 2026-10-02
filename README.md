# financial-charts.el

The Emacs package is `financial-chart` (files and functions are
`financial-chart-*`); this repository is financial-charts.el.

Financial charts in Emacs from plain Lisp data. One call draws any chart
kind (candlesticks, area, braille line, sparkline, option payoff,
diverging P/L bars) as unicode text in a terminal frame or as an SVG
image in a GUI frame. Pure Elisp on Emacs's built-in `svg.el`; the only
external process is optional PNG export.

![TSMC daily candlestick chart rendered by financial-charts.el](images/tsmc-candlestick.png)

[See the indicator chart samples](docs/indicator-examples.md).

Daily NYSE: TSM candles, August 20–October 1, 2026. [Source data](examples/tsmc-daily.csv)
and [regeneration script](src/examples/render-tsmc-chart.el); source: [Nasdaq historical
data](https://api.nasdaq.com/api/quote/TSM/historical?assetclass=stocks&fromdate=2026-08-01&todate=2026-10-02&limit=30).

Requires Emacs 29.1+. No dependencies. Optional:
market-data.el (companion package) for
charting ticker symbols from a broker
([schwab-broker.el](https://github.com/davidawad/schwab-broker.el),
[alpaca-broker.el](https://github.com/davidawad/alpaca-broker.el)).

## Install

With Emacs 30's `use-package :vc`:

```elisp
(use-package financial-chart
  :vc (:url "https://github.com/davidawad/financial-charts.el" :lisp-dir "src"))
```

or add the repository's `src/` directory to `load-path` and `(require 'financial-chart)`.

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

| Kind | What it draws | Data shape |
|---|---|---|
| `area`, `line`, `sparkline` | price or value history; numeric X is spaced by value, epoch-ms X gets a date axis, `:scale 'log` | series: numbers, `(X Y)` or `(X . Y)`, oldest first |
| `ohlc` | candlesticks with volume panel, X-axis, indicator overlays and an oscillator sub-panel (RSI) | `(:open :high :low :close [:volume] [:time])` plists, `:time` in epoch ms |
| `multi` | several named series on one scale, `:normalize 100` to rebase (ticker vs benchmark) | `(("AAPL" . SERIES) ("SPY" . SERIES))` |
| `payoff` | option P/L vs price with breakevens and true max gain/loss | `(PRICE PNL)` sorted by price |
| `payoff-curves` | T+n payoff curves on one price grid | `(("T+0" . PAYOFF) ("T+30" . PAYOFF))` |
| `bars` | diverging bars, e.g. P/L per position | `(LABEL . VALUE)` |
| `drawdown` | running % decline from the high-water mark, max drawdown | series (equity or price) |
| `histogram` | distribution of period returns, mean and stdev (`:bins`) | series |
| `depth` | order book as a ladder or cumulative depth (`:style 'cumulative`) | `(:bids ((P S) ...) :asks ((P S) ...))` |
| `heatmap` | labeled matrix, e.g. correlations, diverging colors | `(:labels (...) :rows ((...) ...))` |
| `volume-profile` | volume by price level with point of control | ohlc |

`bin/financial-chart kinds` lists them with docs; `bin/financial-chart example KIND`
prints a ready-to-render spec for any of them.

Looks: every SVG shares one style (gridlines, tick labels, legends, a
hover `<title>` on each point or bar). `financial-chart-color-palette`
(or `:palette` per call) switches to a colorblind-safe blue/orange scheme
in text and SVG. Text candlesticks can trade the default half-block glyphs
for `braille` (4x vertical resolution) or `eighths` (8x) via
`financial-chart-candle-style`. Oscillators such as RSI draw in their own
0-100 panel under the price chart (`financial-chart-oscillators`, or a
cohort that contains them).

Common props: `:backend` (`text`, `svg`, `auto`; default
`financial-chart-backend`), `:width`/`:height` (text columns/rows),
`:pixel-width`/`:pixel-height` (SVG), `:unit`, `:title`, and
`:up-face`/`:down-face`/`:dim-face`/`:accent-face` for the text kinds.
A chart is also plain data, `(:kind area :data ... :unit "$")`, which
`financial-chart-plot-spec` renders.

### The chart buffer

`financial-chart-plot-view` shows a chart in a `financial-chart-plot-mode`
buffer: `g` re-renders to the window, `t` flips text/SVG, `+`/`-` zoom a
series around point or the latest data and `0` resets, and moving point
over a text chart shows that column's X and Y in the echo area. Pass
`:refresh-fn` (returns fresh data) and `:refresh-interval` (seconds) for a
live chart; `r` toggles the timer.

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

### Provider-neutral indicator series

The calculation boundary is normalized `bar/v1` input and
`indicator-series/v1` output. Indicator functions have no broker
dependency. Any provider or external calculator can return the same
series shape: aligned `:values` (numbers or `nil`), optional epoch-ms
`:timestamps`, plus `:name`, `:label`, `:unit`, `:panel`, `:scale`,
`:bounds`, `:params` and `:source` metadata.

```elisp
;; Compute locally from normalized bars; output retains the bar timestamps.
(setq rsi (financial-chart-indicator-evaluate 'rsi bars 14))

;; Or normalize a result computed by another library or service.
(setq vendor-rsi
      (financial-chart-normalize-indicator-series
       '(:name vendor.rsi-14 :label "RSI 14" :unit :percent
         :panel :oscillator :values [nil 42.1 55.8]
         :timestamps [1728000000000 1728086400000 1728172800000]
         :source vendor)))

;; Any normalized result is directly plottable as a line series.
(financial-chart-plot-spec (financial-chart-indicator-chart-spec vendor-rsi))
```

`financial-chart-register-indicator` adds a local calculator to the
registry. `financial-chart-list-indicators` reports its display
metadata. Multi-output indicators (for example, a MACD line, signal
line and histogram) return a list of named series with the same
timestamps. The built-ins favor simple, inspectable pure-Elisp
calculations for ordinary chart windows; expensive or specialized
calculations can be supplied externally through `indicator-series/v1`.

### Built-in indicators

- `financial-chart-indicator-evaluate` exposes the built-ins through one
  registry. Call `financial-chart-list-indicators` to discover them:
  moving averages (SMA, EMA, WMA, DEMA, TEMA, HMA, KAMA); momentum and
  oscillators (Momentum, ROC, CCI, MACD, RSI, Stochastic, Williams %R,
  Ultimate Oscillator); volatility and range (ATR, Bollinger Bands,
  Keltner Channels, Donchian Channels); trend (DMI/ADX, Aroon, Parabolic
  SAR); and volume flow (OBV, A/D, MFI, CMF, Chaikin Oscillator).
  Parameterized functions use documented period defaults. Warm-up and
  unavailable points remain `nil`, preserving alignment with input bars.
- [Indicator examples](docs/indicator-examples.md) shows eight rendered
  TSMC charts and includes the script to regenerate their PNG captures.
- `financial-chart-sma`/`-ema` `(bars &optional window field)` — moving
  average of `:close` (or `FIELD`) over `WINDOW` bars (default 20). On
  the price scale — use directly as a `financial-chart-indicators` `:fn`.
- `financial-chart-vwap` `(bars)` — cumulative volume-weighted average
  price (typical price × volume). Also on the price scale, also a
  direct `:fn`. VWAP conventionally resets daily — pass one session's
  bars, not a multi-day history, unless you deliberately want a running
  VWAP across the whole window.
- Indicator overlay and oscillator specs accept per-series `:face`
  values for text and SVG, or an SVG `:color` string. Customize fallback
  SVG series colors with `financial-chart-svg-series-colors`; customize
  generic text series faces with `financial-chart-multi-text-faces`.
- Bollinger regions from close to the upper and lower bands can be shaded
  independently with `financial-chart-indicator-bands` and
  `financial-chart-bollinger-band-spec`. Each region has a configurable
  color and opacity; SVG defaults are
  `financial-chart-svg-band-upper-fill`,
  `financial-chart-svg-band-lower-fill`, and
  `financial-chart-svg-band-fill-opacity`.
- `financial-chart-rsi` `(bars &optional period field)` — simple-average
  RSI (default period 14), values in [0,100]. **Not** on the price
  scale — do not pass it straight to `financial-chart-indicators`, it
  will render invisible or nonsensical against the price panel's own
  axis. Call it directly for a table/memo, or build a separate
  oscillator sub-panel with its own 0-100 scale (analogous to the
  volume panel) if you want it charted.

The calculators work from normalized bars, independent of which provider
supplied them. Alpaca's bars also carry a native per-bar VWAP field;
Schwab and Alpaca do not supply the broader technical indicators above
as market-data fields.

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
whatever machine ends up rasterizing the SVG. Set `financial-chart-svg-google-font`
to a Google Fonts family name such as `"Inter"` to load it in SVG viewers
with network access. For offline/self-contained SVGs, set
`financial-chart-svg-font-file` to a local `.ttf`, `.otf`, `.woff`, or
`.woff2` file and set `financial-chart-svg-font-family` to its family name:

```elisp
(setq financial-chart-svg-google-font "Inter"
      financial-chart-svg-font-size 14)

;; Or embed a local font file in each SVG:
(setq financial-chart-svg-font-file "~/fonts/Inter-Regular.woff2")
;; The filename supplies the family by default. Override when needed:
(setq financial-chart-svg-font-file-family "Inter")
```

`financial-chart-svg-font-size` controls text size. A per-call
`FONT-FAMILY` argument to `financial-chart-render-svg`/`-export-svg`/
`-export-png` overrides the configured family for that call.

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
  doctor, `describe` and the CLI pick them up. A new data shape is one
  `financial-chart-shapes` entry: `:doc`, `:example`, `:validator`, and
  optionally `:values` (the numbers explain/provenance summarize),
  `:from-json`/`:to-json` (the CLI's JSON form); a kind may add `:check`
  for props that change how data is read. Each built-in kind beyond the
  core ones lives in its own module this way, so adding one never edits
  a core file.

## Layout

Source is grouped by responsibility under src/:

- src/financial-chart.el — package entry point and package discovery.
- src/core/ — configuration, data shapes, faces and series helpers.
- src/indicators/ — normalized indicator API, registry and built-in families.
- src/renderers/ — terminal and SVG rendering.
- src/charts/ — plot interface, chart kinds and multi-series charts.
- src/integrations/ — ticker and preset bridges.
- src/cli/ — JSON command-line interface.
- src/examples/ — Elisp scripts that regenerate chart examples.
- test/ — ERT tests; examples/ — sample TSMC bars; docs/ — guide and captures.

## Tests

```sh
make test      # every src/**/*-test.el, offline, no display
make compile   # byte-compile with warnings as errors
make test MARKET_DATA=../market-data.el   # same suite with market-data loaded
```

ERT tests live beside the source modules they cover. Golden text and SVG
fixtures are in `test/fixtures/`. After an intended
visual change, regenerate them with `FINANCIAL_CHART_UPDATE_GOLDEN=1
make test` and review the diff. Trailing spaces in fixtures are data;
`.gitattributes` and `.editorconfig` keep tools from stripping them.

## License

MIT — see [LICENSE](LICENSE).
