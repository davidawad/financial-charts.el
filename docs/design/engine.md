# eas: the chart engine moved to eas.el

The `eas` chart engine (a Vega-Lite-subset interpreter in Emacs Lisp,
layers L0 data through L7 surfaces, prefix `eas-`) is its own package,
[eas.el](https://github.com/davidawad/eas.el). Its design, layer
contracts, spikes and gallery coverage live there:

- `docs/design/engine.md` (the layer tower and every contract)
- `docs/design/engine-spikes.md` (measured latency and spikes)
- `docs/design/gallery-coverage.md` (Vega-Lite gallery coverage)

It was extracted from this repository's `fc-qx1` branch with
`git filter-repo`; epic `fc-qx1` is still tracked in this repo's beads.

## What stays here

financial-chart.el is a client of eas, as eas's design intends: eas
never refers to financial-chart, dependencies point this way.

- `src/integrations/financial-chart-eas.el` registers the financial
  adapters (series, payoff, labeled, matrix, order-book,
  payoff-curves, multi-series), the `indicator`, `values` and
  `volume-profile` transforms, and adds `templates/` and
  `templates/financial/` to `eas-template-directories`.
- `templates/`: `ohlc`, `panes`, `depth`, `payoff`, `payoff-curves`,
  `drawdown`, `diverging-bars`, `financial/volume-profile`, each with
  example bindings in `examples/`.
- `src/charts/financial-chart-plot.el` draws every financial-chart kind
  through its template (the generic ones, `line`, `bars`, `area`,
  `heatmap`, `histogram`, `sparkline`, `multi`, `series-line`, come from
  eas.el); there is no other renderer.
- `src/integrations/financial-chart-eas-parity.el` checks that each
  template plots the numbers financial-chart computes.
- `examples/eas-demo-candles.el`, the candles demo.
- Tests of the above: `src/integrations/financial-chart-eas*-test.el`,
  `financial-chart-templates-test.el`, goldens in
  `test/golden/eas-templates/`.

## Developing against eas

`make test` and `make compile` find eas at `EAS` (default
`../../Personal/emacs/eas.el`, a checkout with `src/`):

```sh
make test EAS=/path/to/eas.el
```
