# financial-charts.el

The Emacs package is `financial-chart` (files and functions are
`financial-chart-*`); this repository is financial-charts.el.

Financial charts in Emacs from data you supply. financial-chart never
fetches anything: you hand it plain Lisp data (or JSON through eas's
shell door), it validates that data strictly and draws it however you
declare, through the [eas.el](https://github.com/davidawad/eas.el) chart
engine. One call draws any chart kind (candlesticks, area, line,
sparkline, option payoff, diverging P/L bars, depth, heatmap, ...) as
text in a terminal frame or as an SVG image in a GUI frame. Pure Elisp;
the only external process is optional PNG export.

![TSMC daily candlestick chart rendered by financial-charts.el](images/tsmc-candlestick.png)

[See the indicator chart samples](docs/indicator-examples.md).

Daily NYSE: TSM candles, August 20–October 1, 2026. [Source data](examples/tsmc-daily.csv)
and [regeneration script](src/examples/render-tsmc-chart.el); source: [Nasdaq historical
data](https://api.nasdaq.com/api/quote/TSM/historical?assetclass=stocks&fromdate=2026-08-01&todate=2026-10-02&limit=30).

Requires Emacs 30.1+ and [eas.el](https://github.com/davidawad/eas.el).
Data comes from you: a broker package, a CSV, a tool's JSON. See
"Bringing data from other packages".

## Install

With Emacs 30's `use-package :vc`:

```elisp
(use-package financial-chart
  :vc (:url "https://github.com/davidawad/financial-charts.el" :lisp-dir "src"))
```

or add eas.el's and this repository's `src/` directories to `load-path` and
`(require 'financial-chart)`.

## Use

```elisp
;; a string: SVG in a GUI frame, unicode text in a terminal
(financial-chart-plot 'area '((1 40.0) (2 45.0) (3 50.0) (4 42.0)) :unit "$")

;; at point, or in a buffer (g re-renders to the window, t flips text/SVG)
(financial-chart-plot-insert 'payoff '((90 50) (95 0) (100 -100) (105 0) (110 50)))
(financial-chart-plot-view 'bars '(("AAPL" . 1200) ("VTI" . 8000) ("TSLA" . -950))
                           :title "open P/L" :unit "$")

;; candlesticks with a volume pane (overlays: financial-chart-indicators)
(financial-chart-plot-view 'ohlc
  '((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
    (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000)))

;; every kind over sample data
(financial-chart-demo)
```

| Kind | What it draws | Data shape |
|---|---|---|
| `area`, `line`, `sparkline` | price or value history; numeric X is spaced by value, epoch-ms X gets a date axis, `:scale 'log` | series: numbers, `(X Y)` or `(X . Y)`, oldest first |
| `ohlc` | candlesticks with volume pane, indicator overlays and oscillator panes (RSI) | `(:open :high :low :close [:volume] [:time])` plists, `:time` in epoch ms |
| `multi` | several named series on one scale, `:normalize 100` to rebase (ticker vs benchmark) | `(("AAPL" . SERIES) ("SPY" . SERIES))` |
| `payoff` | option P/L vs price with breakevens | `(PRICE PNL)` sorted by price |
| `payoff-curves` | T+n payoff curves on one price grid | `(("T+0" . PAYOFF) ("T+30" . PAYOFF))` |
| `bars` | diverging bars, e.g. P/L per position | `(LABEL . VALUE)` |
| `drawdown` | running % decline from the high-water mark, max drawdown | series (equity or price) |
| `histogram` | distribution of period returns, mean and stdev (`:bins`) | series |
| `depth` | cumulative bid/ask order-book depth | `(:bids ((P S) ...) :asks ((P S) ...))` |
| `heatmap` | labeled matrix, e.g. correlations, diverging colors | `(:labels (...) :rows ((...) ...))` |
| `volume-profile` | volume by price level with point of control | ohlc |

`(financial-chart-list-kinds)` lists them with docs;
`(financial-chart-describe-kind 'KIND)` shows a kind's data shape and example.

Every kind is drawn by an eas template (`financial-chart-describe-kind`
names it), so text and SVG share one layout, hover tooltips and the
datum behind each cell. Oscillators such as RSI draw in their own pane
under the candles (`financial-chart-oscillators`, or a cohort that
contains them).

Common props: `:backend` (`text`, `svg`, `auto`; default
`financial-chart-backend`), `:width`/`:height` (text columns/rows),
`:pixel-width`/`:pixel-height` (SVG), `:unit`, `:title`, `:font` (SVG
font family) and `:scale 'log` for area and line. A chart is also plain
data, `(:kind area :data ... :unit "$")`, which `financial-chart-plot-spec`
renders.

### The chart buffer

`financial-chart-plot-view` shows a chart in a `financial-chart-plot-mode`
buffer: `g` re-renders to the window, `t` flips text/SVG, `+`/`-` zoom a
series around point or the latest data and `0` resets, and moving point
over a text chart shows that datum's tooltip in the echo area. Pass
`:refresh-fn` (returns fresh data) and `:refresh-interval` (seconds) for a
live chart; `r` toggles the timer.

## For programs and agents

Every part of the package can be listed, checked and planned before
anything is drawn:

```elisp
(financial-chart-list-kinds)            ; kinds with shape, template and doc
(financial-chart-describe-kind 'payoff) ; shape doc, example data, its eas template
(financial-chart-validate 'payoff data) ; t, or financial-chart-invalid-data
(financial-chart-check 'payoff data)    ; t, or (:code :index :field :message)
(financial-chart-explain 'area data :backend 'svg) ; template, backend and why, args; draws nothing
(financial-chart-describe)              ; the whole package as JSON-ready data
(financial-chart-doctor)                ; M-x: does every kind render here, at parity?
(financial-chart-register-kind 'my-kind :shape 'series :template "my-template"
                               :adapter "series" :doc "...")
```

Errors are typed (`financial-chart-unknown-kind`,
`financial-chart-invalid-data`, parent `financial-chart-error`); their
message says what to do and their data is `(MESSAGE :code CODE :index
INDEX :field FIELD)`, naming the offending row and field
(`financial-chart-error-data` returns it as a plist). SVG output
carries a `<title>` and a `<desc>` stating the kind, point count and
value range.

### Validation

Nothing is drawn until the data passes its shape's validator. Codes:

| Shape | Checked | Codes |
|---|---|---|
| bar/v1 (`ohlc`, `volume-profile`) | a list of plists with finite `:open :high :low :close`; high >= max(open, close) >= min(open, close) >= low; `:volume` non-negative; `:time` (epoch ms) on every bar or none, strictly increasing | `not_a_list`, `not_a_plist`, `missing_field`, `not_a_number`, `high_below_body`, `low_above_body`, `negative_volume`, `time_not_increasing` |
| indicators | every overlay and oscillator `:fn` returns one number-or-nil per bar; indicator-series/v1 `:timestamps` equal the bars' `:time` (`financial-chart-validate-indicator-series`) | `indicator_length`, `indicator_misaligned`, `not_a_number` |
| series | numbers, `(X Y)` or `(X . Y)` with finite Y (nil Y skips a point); `:scale 'log` needs Y > 0 | `invalid_point`, `not_a_number`, `nonpositive_log` |
| payoff | numeric `(PRICE PNL)`, ascending price | `not_a_number`, `price_not_ascending` |
| labeled | `(LABEL . NUMBER)` | `invalid_label`, `not_a_number` |
| order-book | `:bids`/`:asks` of positive `(PRICE SIZE)`, best bid <= best ask | `not_an_order_book`, `invalid_level`, `not_positive`, `crossed_book` |
| matrix | one numeric row per label, rectangular | `not_a_matrix`, `row_count`, `column_count`, `not_a_number` |
| multi-series, payoff-curves | each entry as its inner shape (nested rows read `series[2].y`), payoff curves on one price grid | the inner codes, `zero_base`, `grid_mismatch` |

Drawdown and histogram also check their math (`negative_price`,
`nonpositive_start`, `zero_price`). Data handed to eas's adapters (the
shell door) fails as eas's `SHAPE_INVALID` with the same `index` and
`field`.

## Charts from the shell

The shell door is eas.el's `bin/eas` with this package's templates
registered. `bin/eas` alone does not load them (it has no templates flag
or env var), so run the same entry point with `financial-chart-eas`
loaded first. Put it in a shell function:

```sh
EAS=~/projects/Personal/emacs/eas.el   # your eas.el checkout
FC=~/projects/Investing/financial-chart.el
fc-eas() {
  emacs -Q --batch -L "$EAS/src" -L "$FC/src" -l financial-chart -l financial-chart-eas \
    -l eas-agent-cli -f eas-agent-cli-main -- "$@"
}

fc-eas describe templates                     # ohlc, panes, payoff, depth, ... plus eas's own
fc-eas example ohlc --raw > bars.json         # bindings that render as-is
fc-eas render ohlc --data bars.json --backend text --raw          # candlesticks as text
fc-eas render ohlc --data bars.json --backend svg --raw > ohlc.svg
fc-eas check payoff --data payoff.json        # validate before drawing
```

`bars.json` is `{"bars": [{"time": "2026-08-20", "open": ..., "high": ..., "low": ...,
"close": ..., "volume": ...}, ...]}`. Every verb answers a `chart/v1`
envelope and exits 1 on failure; `--raw` prints only the data. The
`indicators` and `oscillators` bindings of `ohlc` need `financial-chart-eas`
loaded, which the function above does. The verbs and options are
documented in eas.el's README and `bin/eas describe verbs`.

## Candlesticks

`financial-chart-render` (text), `financial-chart-render-svg` and
`financial-chart-view` take a list of bar/v1 plists directly; they are
the `ohlc` kind drawn by eas's `ohlc` template. `let`-bind a defcustom
for a one-off override.

### Configuration

All `financial-chart-*` custom variables (`M-x customize-group
financial-chart`):

- `financial-chart-height` — text rows of `financial-chart-render` (default 20).
- `financial-chart-max-bars` — window to the most recent N bars (default
  80; nil/0 = unlimited).
- `financial-chart-show-volume` — draw the volume pane (default t; only
  when at least one bar has `:volume`).
- `financial-chart-indicators` — overlays on the price pane, a list of
  plists `(:fn FN :label LABEL)`. `FN` takes the (already-windowed) bars
  and returns one number-or-nil per bar on the price scale; a result of
  any other length is a validation error.
- `financial-chart-oscillators` — the same shape, each drawn in its own
  pane under the price pane.

```elisp
(setq financial-chart-indicators
      (list (list :fn (lambda (bars) (financial-chart-sma bars 20)) :label "SMA 20")))
```

Colors, fonts, axes and glyphs are eas's (its theme follows yours).

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
- `financial-chart-rsi` `(bars &optional period field)` — simple-average
  RSI (default period 14), values in [0,100]. **Not** on the price
  scale — put it in `financial-chart-oscillators`, which draws it in its
  own pane, not in `financial-chart-indicators`.

The calculators work from the bars you supply, independent of where they
came from.

### SVG / PNG export

```elisp
(financial-chart-export-svg bars "chart.svg" "MY SYMBOL")   ; pure Elisp
(financial-chart-export-png bars "chart.png" "MY SYMBOL")   ; shells out to rasterize
(financial-chart-export-png bars "chart.png" "MY SYMBOL" 1280 760 "Hack") ; size, font
```

`financial-chart-render-svg` returns the SVG as a string. Any kind
exports the same way: `(financial-chart-plot KIND DATA :backend 'svg)`.
eas's own export door (`bin/eas export ... --vl`) hands pure Vega-Lite
to other renderers.

`financial-chart-export-png` renders to SVG first, then rasterizes via
`financial-chart-png-converter` (nil auto-detects `rsvg-convert` /
ImageMagick's `convert`/`magick` via `executable-find`, in that order;
set it to a symbol to force one, or to a function of `(SVG-FILE
PNG-FILE)` to convert some other way entirely). This is the one place
the package runs an external process.

## Indicator cohorts

```elisp
(financial-chart-list-cohorts)                   ; named indicator sets
(financial-chart-describe-cohort 'trend-following)
(financial-chart-resolve-cohort 'mean-reversion) ; overlay and oscillator specs
```

Cohorts (`financial-chart-indicator-cohorts`) are data: add an entry to
add one. Cohort members may name recipes in an external indicator
catalog; set `financial-chart-indicator-catalog-function` to a lookup
function and `financial-chart-describe-cohort` will check them against
it.

## Bringing data from other packages

This package draws; it never fetches, and it names no data source or
broker. The boundary is the data shapes above, so anything that
produces them can be charted:

- **OHLC bars** are the bar/v1 plist `(:open :high :low :close
  [:volume] [:time])`, `:time` in epoch milliseconds. A broker or
  market-data package converts its responses to that shape and calls
  `financial-chart-plot` (or `financial-chart-render`) with them;
  `financial-chart-validate` says exactly which bar is wrong if one is.
- **Anything else** (positions, P/L, payoff curves, a CSV, a tool's
  JSON) needs only a small converter in the package that owns that data,
  producing a series, payoff or labeled list for `financial-chart-plot`.
  From outside Emacs, emit bindings JSON for an eas template (see
  "Charts from the shell").
- **New chart types** register with `financial-chart-register-kind`,
  naming a shape and the eas template that draws it; the doctor and
  `describe` pick them up. A new data shape is one
  `financial-chart-shapes` entry: `:doc`, `:example`, `:validator`
  (signalling `financial-chart-invalid-data` with `:code`, `:index` and
  `:field`), and optionally `:values` (the numbers explain/provenance
  summarize), `:from-json`/`:to-json` (the JSON form of the data); a
  kind may add `:check` for props that change how data is read. Each
  built-in kind beyond the core ones lives in its own module this way,
  so adding one never edits a core file.

## Layout

Source is grouped by responsibility under src/:

- src/financial-chart.el — package entry point and package discovery.
- src/core/ — configuration, errors, data shapes and validation.
- src/indicators/ — normalized indicator API, registry, built-in families and cohorts.
- src/charts/ — the kind registry, `financial-chart-plot` and each chart kind.
- src/integrations/ — eas adapters, transforms, templates and parity checks.
- src/examples/ — Elisp scripts that regenerate chart examples.
- templates/ — financial-chart's eas templates; test/ — test support and template goldens;
  examples/ — sample TSMC bars and template bindings; docs/ — guide and captures.

## Tests

```sh
make test      # needs eas.el (EAS=/path/to/eas.el); every src module's *-test.el files, offline, no display (< 1 min)
make compile   # byte-compile with warnings as errors
```

ERT tests live beside the source modules they cover. The template text
goldens are in `test/golden/eas-templates/`; after an intended visual
change regenerate them with `EAS_UPDATE_GOLDEN=1 make test` and review
the diff. Trailing spaces in goldens are data; `.gitattributes` and
`.editorconfig` keep tools from stripping them.

## The eas engine

The interactive engine is its own package, [eas.el](https://github.com/davidawad/eas.el)
(design, latency budget and Vega-Lite gallery live there; see
`docs/design/engine.md`). financial-chart requires it (`eas`, Emacs 30.1)
and registers its adapters, transforms and financial templates on it.
The Makefile looks for eas.el at
`../../Personal/emacs/eas.el`; override with `EAS=/path/to/eas.el`.

### Chart kinds as eas templates

Every chart kind is an eas template: plain Vega-Lite over tidy rows.
The generic ones (`area`, `series-line`, `sparkline`, `multi`,
`histogram`, `heatmap`, `line`, `bars`) ship with eas.el;
financial-chart adds `ohlc`, `panes`, `payoff`, `diverging-bars`,
`payoff-curves`, `drawdown`, `depth` (in `templates/`) and
`financial/volume-profile`, all registered through
`eas-template-directories`. `ohlc` takes a `volume` pane, `indicators`
overlays and `oscillators` panes by indicator name, e.g. eas's
`bin/eas render ohlc --data b.json` with
`"indicators": [{"name": "sma", "params": [20], "as": "sma20"}]` (the
`indicator` transform needs financial-chart loaded).

`financial-chart-plot` validates the data, lowers it to the template's
bindings (`financial-chart-eas-bindings`) and draws it with eas;
`financial-chart-explain` names the template. Where a template computes
in Vega-Lite what this package computes in Lisp (drawdowns, breakevens,
return statistics, cumulative depth, volume per level, overlay values),
`(financial-chart-eas-parity 'drawdown)` checks, as data, that the two
agree; the doctor runs it for every kind.

### bin/chart is not a runtime dependency

eas draws every chart in Emacs Lisp, as SVG or text. The config
`bin/chart` (a Vega-Lite build door) is used in only two places, and
eas works without it:

- Test oracle (dev and CI), now in eas.el: its PNGs are the committed
  conformance references.
- Static export: eas's `bin/eas export ... --vl` hands pure Vega-Lite to
  `bin/chart build`, and org-babel `:file x.png`/`x.pdf` calls it.

A spec that uses Vega-Lite features outside the native subset opens as
a static view. That view shows its `UNSUPPORTED_FEATURE` findings as
text, and `render --backend svg` fails with the same reason code. If
you want a picture from `bin/chart` in that case, opt in with
`(setq eas-static-fallback t)`.

A valid Vega-Lite property that the native renderer draws without (an
axis `labelFont`, a legend `orient: "bottom"`, `config.locale`) does
not block anything. `check` lists it in `warnings` as
`UNSUPPORTED_FEATURE` with `"ignored": true`, its JSON path, and
`native: true`. The chart still opens natively and stays interactive.

## License

MIT — see [LICENSE](LICENSE).
