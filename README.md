# financial-chart.el

A pure-Elisp OHLC candlestick chart renderer, built entirely on Emacs's
own display engine — no gnuplot, no image/PNG generation, no external
process. Renders directly into a buffer using unicode box-drawing and
block characters at half-block vertical resolution, with `propertize`
faces for up/down coloring. Requires Emacs 27.1+ (for `propertize`
face support on unicode glyphs; no other version-specific features).

Data-source agnostic: the renderer takes a plain list of
`(:open :high :low :close)` plists, oldest bar first. It doesn't know
or care where those bars came from.

## Install

Copy `financial-chart.el` somewhere on your `load-path`, then:

```elisp
(require 'financial-chart)
```

## Use

```elisp
(financial-chart-view
 '((:open 100 :high 103 :low 99 :close 102)
   (:open 102 :high 104 :low 101 :close 101.5)
   (:open 101.5 :high 102 :low 96 :close 97))
 "MY SYMBOL")
```

pops a `*financial-chart*` buffer with a candlestick render. Call
`financial-chart-render` directly instead if you want the chart as a
plain string (e.g. to embed elsewhere) rather than a popped buffer.

`financial-chart-height` (default 20) controls the number of character
rows; `financial-chart-up-face`/`financial-chart-down-face` (default
`success`/`error`) control candle coloring.

## Schwab bridge

If [schwab-broker.el](https://github.com/davidawad/schwab-broker.el)
is loaded, `financial-chart-schwab-view` fetches real price history and
renders it directly:

```elisp
(financial-chart-schwab-view
 "AAPL" :period-type "day" :period 5 :frequency-type "minute" :frequency 5)
```

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
