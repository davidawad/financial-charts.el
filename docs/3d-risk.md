# 3D portfolio risk: data contracts

Seven templates chart portfolio risk. Each has a 2D template in
`templates/financial/` (eas draws it, with or without graphics.el) and a
3D variant in `templates3d/` (graphics.el draws it; see
[3d.md](3d.md) for the renders). `src/financial-chart-3d-risk.el`
registers their marks and holds the helpers named below.

This package never fetches data. Each template takes rows lowered from
the JSON that an invest-metrics metric emits (dotfiles
`src/rust/crates/invest-metrics`, `invest metric run ID`), so the
invest.el boards can feed them directly. The examples are hand-derived
fixtures in `test/fixtures/3d/risk/`, written by
`scripts/fixtures-3d-risk.py`. Correlations and risk contributions in
those fixtures come from one small factor model, so every matrix is
positive semi-definite and every contribution row sums to 100.

```elisp
(require 'financial-chart-3d)
;; 3D when graphics.el is available, else the 2D template, same bindings:
(financial-chart-3d-risk-open "risk-correlation" :bindings (list :data ROWS) :show t)
(financial-chart-3d-open "risk-stress" :show t)     ; 3D only
M-x financial-chart-3d-gallery                       ; every variant, these included
```

Rows are JSON objects, or plists in Lisp. Dates are ISO strings
(`"2026-03-31"`) or numbers, oldest first. A replay plays its keyframes
in the order they first appear (numbers sorted). Field names are the
defaults; a variant's `encoding` can rename them.

| template | rows | source metric | 3D mark | replay |
|---|---|---|---|---|
| `risk-correlation` | `{window, row, column, value}` | `stats.rolling.correlation` | `fc-risk-grid3d` | window by window (frame mode, eased) |
| `risk-contribution` | `{date, asset, contribution}` | `stats.risk_contribution` per date | `fc-risk-ribbons3d` | cumulative by date |
| `risk-factor-exposure` | `{date, factor, beta}` | `stats.rolling.factor_regression`; `port.beta_weighted_delta` | `fc-risk-ribbons3d` | cumulative by date |
| `risk-stress` | `{scenario, asset, pnl}` | `port.stress` per scenario | `fc-risk-grid3d` | none (one snapshot) |
| `risk-underwater` | `{date, strategy, equity}` | `stats.max_drawdown` per strategy (its returns) | `fc-risk-ribbons3d` (`transform: underwater`) | cumulative by date |
| `risk-return-distribution` | `{horizon, return}` | `stats.var`, `stats.var_cvar.*` (their return samples) | `fc-risk-histogram3d` | horizon by horizon |
| `risk-frontier` | `{date, vol, ret, sharpe}` | `port.frontier` | `fc-risk-frontier3d` | cumulative by date |

## risk-correlation

One full correlation matrix per window, in long form:

```json
{"window": "2026-05-31", "row": "SPY", "column": "TLT", "value": 0.536}
```

- `window`: the window's end date (or an index). Every window carries
  every `row` x `column` cell, the diagonal included.
- `value`: the correlation, in [-1, 1].

`financial-chart-3d-risk-matrix-rows` lowers matrices to these rows. It
takes one matrix `{labels: [...], matrix: [[...], ...]}` (also accepted:
`assets`, `names` or `symbols` for the labels, and `correlation` or
`values` for the matrix). It also takes a series of matrices, either
`{windows: [M, ...]}` or a plain array, where each `M` carries its
`window` (or `date`, `end`, `asof`).

3D: one bar per cell on an asset by asset floor. Positive correlations
stand up in green, negative ones hang through the floor in red, and the
colour deepens with magnitude. The diagonal is a flat tile. The height
scale is fixed at [-1, 1] (`domain`). Frame mode draws one window at a
time and tweens each bar into the next window, so you watch the
stock-bond correlation flip. 2D: a heatmap of the latest window, values
printed.

`stats.rolling.correlation` emits `{windows: [{window: END_DATE,
labels, matrix}, ...]}` (input `{dates, series: [{label, values}],
window, step?}`); its adapter lowers every window.

## risk-contribution

```json
{"date": "2026-05-31", "asset": "TLT", "contribution": 21.37}
```

- `contribution`: the asset's share of portfolio volatility, in percent
  (`w_i (Sigma w)_i / w' Sigma w x 100`). Contributions sum to 100 on
  each date. A hedge can contribute a negative share; its ribbon then
  hangs below the zero plane.

3D: one ribbon per asset (a slab from zero up to its share), in the
asset colours eas uses in 2D. The first asset sits at the back. 2D: a
stacked area chart reaching 100%.

## risk-factor-exposure

```json
{"date": "2026-05-31", "factor": "HML", "beta": 0.164}
```

- `beta`: the factor loading from a rolling regression (Fama-French
  plus momentum and quality in the fixture). Any signed exposure works.
  For `port.beta_weighted_delta`, put the underlying in `factor` and the
  beta-weighted delta in `beta`.

3D: one ribbon per factor around a zero plane. Long exposure stands
above it and short exposure hangs below. 2D: one line per factor, with
a dashed zero rule.

`stats.rolling.factor_regression` emits `{windows: [{window, betas:
{FACTOR: beta}}]}`; its adapter makes one row per window and factor.
`port.beta_weighted_delta`'s adapter makes one row per position and date
from the positions its output echoes (`inputs.positions`):
`quantity x multiplier x delta x underlying_price x beta`, the terms its
value sums, in dollars.

## risk-stress

```json
{"scenario": "2022 Rates", "asset": "TLT", "pnl": -7.75}
```

- `pnl`: the position's P/L under the scenario, in percent of NAV or in
  currency (one unit per chart).

3D: a scenario by position grid of signed bars. Losses sink red through
the floor, gains rise green. A `net` column (the row's sum) stands apart
at the right edge. The camera looks up from below, because the losses
are the point. 2D: a heatmap, values printed.

## risk-underwater

```json
{"date": "2025-09-26", "strategy": "Vol premium", "equity": 103.12}
```

- `equity`: each strategy's NAV, on any scale. The template computes the
  drawdown itself: the percent below the running high, 0 at a new high
  (`financial-chart-3d-risk-underwater-rows`, the same as
  `stats.max_drawdown`'s underwater series). Order the rows by date,
  oldest first. Feeding drawdowns directly also works: give `equity` as
  `100 + drawdown_pct`, whose running high is 100.

3D: a ridgeline. Each strategy hangs below its own high-water line
(`stack` 0.9 lifts each lane's zero above the next lane's deepest
point), and a marker picks out each strategy's worst point. 2D: one
drawdown line per strategy.

## risk-return-distribution

```json
{"horizon": "21d", "return": -3.412}
```

- `return`: one return sample, in percent. Overlapping h-day returns
  work fine. List the horizons shortest first. Any number of samples per
  horizon works, because bars show each bin's share of its horizon.

3D: one histogram per horizon, in lanes from front (shortest) to back,
all on one set of return bins (`bins`, default 17). Loss bins are red
and gain bins green. Each lane's 95% historical VaR (`level`) stands as
an amber gate at its quantile. Each lane is scaled to its own peak, so
a long horizon's spread is not flattened; set `scale: "shared"` for one
scale. `financial-chart-3d-risk-histograms` returns the shares, VaR
and CVaR (minus the 1 - level quantile, and minus the mean beyond it,
the historical definitions). 2D: one step line per
horizon of the share of returns in each bin.

## risk-frontier

```json
{"date": "2026-03-31", "vol": 9.214, "ret": 7.6, "sharpe": 0.445}
```

- `vol`, `ret`: annualised volatility and expected return of one
  frontier portfolio, in percent.
- `sharpe`: its Sharpe ratio, `(ret - rf) / vol`.
- Every date needs the same number of points: the frontier sampled at
  fixed steps of target return. Consecutive dates are joined into the
  surface only when their counts match.

3D: frontiers of return (up) over volatility (across), one per date
from front to back. Each frontier is a ribbon covering 55% (`fill`) of
the way to the next, coloured blue to amber by Sharpe. The newest
frontier is drawn white, and an amber path joins each date's
maximum-Sharpe portfolio. 2D: one line per date.

`port.frontier` emits `{asof, rf, labels, points: [{vol, ret, sharpe,
weights}]}` for N target returns from the minimum-variance portfolio
upward, or its as-of series `{series: [...]}` (with `dates`, `series`
and `window` in its input). `vol` and `ret` are decimals unless the run
set `output_scale: 100`; the output does not say which, so pass
`:output-scale 100` to the adapter for such a run.

## From the metrics' output

`financial-chart-3d-bindings-from-metric TEMPLATE METRIC-ID OUTPUT &rest
ARGS` (`src/financial-chart-3d-risk.el` holds these adapters) takes one
run's output, or a series of entries `{output, ...}` each stamped as
below; ARGS stamps a single output (`:as-of`, `:scenario`, ...) and sets
the template's slots (`:title`):

| metric | template | entry | rows from |
|---|---|---|---|
| `stats.rolling.correlation` | `risk-correlation` | the output (or `{as_of, output}` per matrix) | every window's matrix |
| `stats.rolling.factor_regression` | `risk-factor-exposure` | the output | `windows[].betas` |
| `port.beta_weighted_delta` | `risk-factor-exposure` | `{as_of, output}` | `inputs.positions` |
| `stats.risk_contribution` | `risk-contribution` | `{as_of, output, labels?}` | `value.assets[].fraction_of_variance` x 100; `labels` (or `:labels`) name the asset indices |
| `port.stress` | `risk-stress` | `{scenario, output}` | `inputs.exposures`: `weight x scenario_return` x 100, percent of NAV |
| `stats.var`, `stats.var_cvar.historical`, `stats.var_cvar.parametric` | `risk-return-distribution` | `{horizon, output}` | `inputs.returns` x 100 |
| `stats.max_drawdown` | `risk-underwater` | `{strategy, output, dates?}` | its returns compounded from 100 (period 0 is the start) |
| `port.frontier` | `risk-frontier` | the output, or `{as_of, output}` | `points`, x 100 unless `:output-scale 100` |

Each metric's output echoes the input it was run on (`inputs`), so the
samples and exposures are read from the output itself.

## Without graphics.el

`financial-chart-3d-risk-open` opens the 2D template with the same
bindings and does not signal. `financial-chart-3d-open` signals
`financial-chart-3d-unavailable`, as it does for every variant. The
templates' 2D bodies are the variants' bodies, so eas draws the variants
in 2D too.
