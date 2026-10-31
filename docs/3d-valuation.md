# Valuation surfaces: data contracts

Three templates show valuation over a grid of assumptions:

| template | metric (`invest metric run ID`) | 2D fallback | 3D mark | replay |
|---|---|---|---|---|
| `dcf-sensitivity` | `eq.dcf --grid` over WACC x terminal growth, one run per as-of date; price from `eq.price_target` | heatmap, cells coloured against the price | `fc-dcf3d` | snapshots (`as_of`), one morphing into the next |
| `revenue-surface` | `eq.revenue.possibility_surface` | fan of quantile lines, the median orange | `fc-revenue3d` | year by year |
| `kelly-surface` | `stats.kelly.growth --grid` over edge x fraction | heatmap, diverging at zero growth | `fc-kelly3d` | none (not a time series) |

Each one is a 2D template in `templates/financial/NAME.json` plus its 3D
variant in `templates3d/NAME.json` (`fc3d/NAME`, written by
`scripts/templates3d-valuation.py`). `src/financial-chart-3d-valuation.el`
registers the marks. This package never fetches data: a template's
bindings are the JSON the metric emits, as below, so the invest.el boards
can bind it directly. The example of each is its fixture in
`test/fixtures/3d/valuation/`, and `docs/3d.md` shows its golden render.

```elisp
(financial-chart-3d-open "dcf-sensitivity" :bindings BINDINGS :show t)   ; 3D
(eas-resolve "dcf-sensitivity" BINDINGS)                                  ; 2D
M-x financial-chart-3d-gallery                                          ; all variants
```

The bindings are a JSON object. Field names are snake_case and rates are
decimals (`0.085` is 8.5%). Rows are an array of objects, in any order:
the grid axes are the distinct values, sorted. A grid with missing cells
still draws: an empty cell sits at the surface's lowest value.

## Grid runs (invest-metrics)

The DCF and Kelly metrics each return one value for one set of
assumptions; `invest metric run ID --grid` evaluates one over a grid and
emits one row per point, which is what these templates bind:

```text
invest metric run eq.dcf \
  --data '{"historical_fcf": [5.0, 5.4], "projection_years": 5, "growth_rate": 0.08,
           "shares_outstanding": 1.0, "cash_and_equivalents": 0.0, "total_debt": 12.0,
           "discount_rate": 0.0, "terminal_growth_rate": 0.0}' \
  --grid wacc@discount_rate=0.07:0.11:0.005 \
  --grid terminal_growth@terminal_growth_rate=0.01:0.04:0.005 \
  --pick value_per_share@dcf_intrinsic_value_per_share \
  --stamp price=141.2 --stamp as_of='"2026-09-30"'

invest metric run stats.kelly.growth --data '{"model": "continuous", "sigma": 0.2}' \
  --grid edge=0.01:0.12:0.01 --grid fraction=0:3:0.25 --pick growth
invest metric run stats.kelly.growth --data '{"model": "binary", "odds": 1}' \
  --grid edge=0.01:0.12:0.01 --grid fraction=0:0.95:0.05 --pick growth
```

- `--grid COLUMN[@FIELD]=FROM:TO:STEP` (inclusive) is an axis: the row
  column `COLUMN`, set on the input field `FIELD` (default `COLUMN`).
  Every point of the axes' cartesian product is one row, the first axis
  slowest. Other inputs stay as `--data` gives them (the DCF input's own
  `discount_rate` and `terminal_growth_rate` are overridden).
- `--pick COLUMN[@KEY]` puts the output value named `KEY` (a
  `MetricCalculation`'s `key`) in `COLUMN`; without `KEY`, a single
  output's `value`. A point the metric rejects (WACC at or below growth)
  gives null.
- `--stamp NAME=JSON` adds a constant to every row. A DCF history is
  one run per as-of date, each stamped with its `as_of` and that date's
  `price`; there is no `--as-of` flag.
- `stats.kelly.growth` takes `{model, edge, fraction, sigma | odds}` and
  returns the growth rate at that fraction (null for ruin).
  `stats.kelly.binary` and `stats.kelly.continuous` return the optimal
  fraction, not growth, so they do not feed this surface.

`financial-chart-3d-bindings-from-metric` turns these rows into
bindings (`(financial-chart-3d-metric-templates)` lists every metric and
the templates it feeds):

```elisp
(financial-chart-3d-bindings-from-metric "dcf-sensitivity" "eq.dcf" ROWS)
;; or the runs of a history, each stamped by its entry when the rows are not:
(financial-chart-3d-bindings-from-metric
 "dcf-sensitivity" "eq.dcf"
 (list (list :as_of "2026-06-30" :price 118.6 :output ROWS-JUNE)
       (list :as_of "2026-09-30" :price 141.2 :output ROWS-SEPT)))
(financial-chart-3d-bindings-from-metric "kelly-surface" "stats.kelly.growth" ROWS)
```

- `eq.dcf`: rows without a number `value_per_share` (the rejected
  points) are left out; the `price` slot is `:price` if given, else the
  newest row's `price`. Rows need `wacc` and `terminal_growth`
  (`missing_axis` otherwise).
- `stats.kelly.growth`: grid rows `{edge, fraction, growth}`, or one
  output per point (a list), whose `inputs` give edge and fraction and
  whose model, sigma or odds become the slots. Ruin (null) and growth
  below `:floor` (default -1) are drawn at the floor, as
  `financial-chart-3d-kelly-surface` draws them.

`financial-chart-3d-kelly-surface` still computes a Kelly grid locally
from the metric's parameters (see below). The revenue surface has no
adapter: `eq.revenue.possibility_surface` emits a confidence grid over
revenue levels (`{periods, revenues, z}`), not the quantiles by year this
template draws.

## dcf-sensitivity

```json
{
  "title": "DCF sensitivity: value per share by WACC and terminal growth",
  "price": 141.2,
  "data": [
    {"as_of": "2026-09-30", "wacc": 0.07, "terminal_growth": 0.01, "value_per_share": 118.76, "price": 141.2},
    ...
  ]
}
```

| field | type | meaning |
|---|---|---|
| `price` (slot) | number, required | current price per share (`eq.price_target`'s `price`): the 2D colour midpoint and the 3D plane |
| `data[].wacc` | number | discount rate (x) |
| `data[].terminal_growth` | number | perpetual growth after the explicit forecast (z) |
| `data[].value_per_share` | number | `eq.dcf`'s equity value per share at that point (height) |
| `data[].as_of` | string, optional | snapshot date (ISO). With several snapshots, 3D replays them in date order and 2D draws the latest |
| `data[].price` | number, optional | that snapshot's price. In 3D it overrides the slot, so the plane moves with each snapshot |

3D: a lit surface over WACC (across) and terminal growth (deep), with
value per share as the height. The colour is red below the price and
green above it, split evenly around the price. The amber wire plane sits
at the price. The white fair-value line marks where the surface crosses
the plane (marching squares over the grid): on one side the stock is
cheap under those assumptions, on the other it is dear. The snapshots
morph into one another (`encoding.time`, mode `frame`, cubic easing),
over a grid and scale that span all of them.

Fixture: four quarterly snapshots of a 9 x 7 grid (WACC 7% to 11% by
0.5%, terminal growth 1% to 4% by 0.5%). The values are derived by hand
from a five-year DCF on a per-share basis: free cash flow FCF1 in year 1,
growing 8% a year to year 5, then a Gordon terminal value
`FCF5 (1 + g) / (WACC - g)`, all discounted at WACC, less 12.00 of net
debt per share. FCF1 and price per snapshot:

| as_of | FCF1 | price |
|---|---|---|
| 2025-12-31 | 5.40 | 121.30 |
| 2026-03-31 | 5.65 | 134.80 |
| 2026-06-30 | 5.90 | 118.60 |
| 2026-09-30 | 6.20 | 141.20 |

`financial-chart-3d-valuation-dcf-fixture-is-derived` recomputes every
row from these inputs.

## revenue-surface

```json
{
  "title": "Revenue possibility surface, FY2027-FY2034 ($bn)",
  "data": [
    {"year": 2027, "quantile": 0.05, "revenue": 43.13},
    ...
  ]
}
```

| field | type | meaning |
|---|---|---|
| `data[].year` | number | fiscal year (x) |
| `data[].quantile` | number in (0, 1) | quantile of the simulated outcome distribution (z); 0.5 is the median |
| `data[].revenue` | number | revenue at that year and quantile, in the metric's unit (height); name the unit in `title` or `y_title` |

3D: revenue over fiscal year (across) and quantile (deep), coloured from
dark blue (low) to amber (high). The cone widens with the horizon. The
white line traces the median along its row, and the cyan line outlines
the newest year. The surface grows one year at a time (mode
`cumulative`), with the scale fixed to all the years.

Fixture: quantiles of a lognormal revenue path from FY2026 revenue of
48.2 ($bn), with drift log 1.09 a year and volatility 0.12:
`R(t, q) = 48.2 exp(t log 1.09 + z_q 0.12 sqrt t)`, where `t = year - 2026`
and `z_q` is the standard normal quantile. That gives 8 years x 7
quantiles (5, 10, 25, 50, 75, 90 and 95%).
`financial-chart-3d-valuation-revenue-fixture-is-derived` recomputes
every row.

## kelly-surface

```json
{
  "title": "Kelly growth rate by edge and leverage (sigma 20%)",
  "model": "continuous",
  "sigma": 0.2,
  "data": [
    {"edge": 0.01, "fraction": 0.0, "growth": 0.0},
    ...
  ]
}
```

| field | type | meaning |
|---|---|---|
| `model` (slot) | `"binary"` or `"continuous"` | which metric the growth came from (recorded; the chart draws `growth` as given) |
| `sigma` (slot) | number | continuous model: annualised volatility |
| `odds` (slot) | number | binary model: net odds `b` paid on a win |
| `data[].edge` | number | continuous: expected excess return `mu - r`; binary: expected return per unit staked, `p b - (1 - p)` (z) |
| `data[].fraction` | number | fraction of bankroll staked; above 1 is leverage (x) |
| `data[].growth` | number | expected log growth per period (height) |

The growth rates (`financial-chart-3d-kelly-growth`):

- continuous: `g = f edge - sigma^2 f^2 / 2`. Its peak is at the Kelly
  fraction `f* = edge / sigma^2`, and it falls back to zero at `2 f*`.
- binary: with `p = (1 + edge) / (1 + b)`,
  `g = p log(1 + b f) + (1 - p) log(1 - f)`. Its peak is at `f* = edge / b`;
  at `f >= 1` the bet is ruin.

`financial-chart-3d-kelly-surface` computes the whole grid
(`:model :edges :fractions :sigma :odds :floor`). Ruin and anything below
`:floor` are drawn at the floor.

3D: log growth over bet fraction (across) and edge (deep). The colour is
red where growth is negative and green where it is positive, split evenly
around zero. The grey wire plane is zero growth. The amber line is where
over-betting starts to lose money, and the white ridge is the Kelly
fraction for each edge. Static: the input is a grid, not a time series.

Fixture: the continuous model at sigma 0.20, edges 1% to 12% by 1% and
fractions 0 to 3 by 0.25. `financial-chart-3d-valuation-kelly-fixture-is-computed`
checks it against `financial-chart-3d-kelly-surface`.

## Marks

`fc-dcf3d`, `fc-revenue3d` and `fc-kelly3d` share one lowering,
`financial-chart-3d-valuation--surface-mark`. It takes channels `x`, `z`
and `y`, and these options in `x-eas.graphics`:

- `level` or `level_field`: the reference plane and the split point of
  the colour gradient.
- `contour`: default on.
- `trace`: `"max"` traces the ridge; a number traces that z row.
- `front`: outlines the newest column of a replay.
- `height`, `extent`, `low_color`, `high_color`, `plane_color`,
  `contour_color`, `trace_color`, `front_color`.

Each template sets its own `camera`, chosen so that its 60x24 golden
reads clearly.
