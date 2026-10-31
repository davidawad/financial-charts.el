# Rates and credit in 3D: data contracts

Four templates draw fixed-income and credit metrics from
[invest-metrics](https://gitlab.com/davidawad/dotfiles) (`src/rust/crates/invest-metrics`,
`invest metric run ID`). Each takes the JSON those metrics emit; this
package never fetches it. A caller (the dotfiles `invest.el` boards)
runs the metrics and passes their output, parsed as eas parses JSON, to
the template's `data` slot:

| template | metrics | 3D (`fc3d/NAME`) | replay |
|---|---|---|---|
| `yield-curve` | `fi.nss_fit`, `fi.curve_regime`, `fi.forward_path`, `fi.rolldown_carry` | `fc-yield-curve3d`: the curves over tenor and date, coloured by regime | one curve per frame, earlier ones left as a trail |
| `credit-spreads` | `fi.spread_suite`, `fi.spread_pctile` | `fc-credit3d`: a spread surface over tenor and rating, coloured by percentile | |
| `cds-term-structure` | `credit.cds_isda_implied_hazard_pd` | `fc-cds-term3d`: the term structure over tenor and date | one date per frame, with a trail |
| `key-rate-durations` | `fi.krd`, `fi.dv01` | `fc-krd3d`: signed bars over key rate and holding | |

```elisp
(require 'financial-chart-3d)            ; loads financial-chart-3d-rates
;; 3D with graphics.el, else the 2D template in an eas buffer:
(financial-chart-3d-rates-show "yield-curve" HISTORY :title "UST curve")
;; or explicitly:
(financial-chart-3d-open "yield-curve" :bindings (list :data HISTORY) :show t)
(eas-view-open "yield-curve" :bindings (list :data HISTORY))     ; 2D
```

Every template has a 2D form (`templates/financial/NAME.json`, a heatmap
with the values printed) and its 3D variant (`templates3d/NAME.json`,
the same body plus `x-eas.graphics`), both written by
`scripts/templates3d-rates.py`. The `data` slot's shape
(`invest/yield-curve`, `invest/credit-spreads`, `invest/cds-term`,
`invest/krd`, registered by `src/financial-chart-3d-rates.el`) lowers the
metric JSON to the flat rows below, so 2D and 3D draw the same rows.
Rows already flat pass through, so a caller may also send those. A
malformed entry is eas's `SHAPE_INVALID`, with the entry's `index`, the
`field` and a `reason` code; the Lisp adapters
(`financial-chart-3d-rates-curve-rows`, `-spread-rows`, `-cds-rows`,
`-krd-rows`) signal the same as `financial-chart-invalid-data`.

A metric output is `{key, value, calculation, inputs}`
(`MetricCalculation`); `inputs` echoes the input record, every field of
it, null or not. Only the inputs named below are read, so outputs with
the nulls left out (as in the fixtures) read the same. Rates are
decimals, spreads basis points, as the metrics define them.

The examples are `test/fixtures/3d/rates/NAME.data.json`: invest-metrics
output for hand-chosen inputs (approximate US Treasury quarter-end
curves 2022-2025, a rating by tenor spread grid, a BBB industrial's CDS
curve through a 2026 stress, seven holdings' key-rate durations), null
inputs dropped. `financial-chart-3d-rates-test.el` checks them against
each metric's rule.

`financial-chart-3d-bindings-from-metric TEMPLATE METRIC-ID OUTPUT &rest
ARGS` builds the `data` below from the runs themselves (the shapes then
pass the flat rows through), the same in 2D and 3D:

| metric | template | OUTPUT |
|---|---|---|
| `fi.nss_fit` | `yield-curve` | entries `{as_of, output, curve_regime?, forward_path?, rolldown_carry?}`, or one output with `:as-of`; each becomes `{as_of, nss_fit: output, ...}` |
| `fi.curve_regime` | `yield-curve` | entries `{as_of, output}`: the output's `inputs.curve_years` and `curve_rates` are the curve, its value the regime |
| `fi.spread_suite`, `fi.spread_pctile` | `credit-spreads` | one output, a list of them (both metrics mixed) or entries `{output}` |
| `fi.krd` | `key-rate-durations` | entries `{holding, output, dv01?, tenors?}`, or one output with `:holding` |
| `credit.cds_isda_implied_hazard_pd` | `cds-term-structure` | entries `{input, output}` by date, or outputs; `:measure` picks the field drawn |

A curve without its date is `missing_date`; the other errors are the
lowerings' below.

## yield-curve

`data`: the curve history, oldest first, one entry per date:

```json
[{"as_of": "2025-12-31",
  "nss_fit": {"key": "nss_fit", "value": 0.03899,
              "inputs": {"curve_years": [0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30],
                         "curve_rates": [0.037, 0.0362, 0.035, 0.0348, ...]}},
  "curve_regime": {"key": "curve_regime", "value": 1.0, "inputs": {...}},
  "forward_path": {"key": "forward_path", "value": 0.047931,
                   "inputs": {"forward_years": [1, 2, 4, 6, 9, 19, 29],
                              "forward_rates": [0.0346, 0.0353, ...]}},
  "rolldown_carry": [{"key": "rolldown_carry", "value": 103.0,
                      "inputs": {"years": 10, "ytm": 0.0412, "horizon_years": 0.25}}, ...]}]
```

- `as_of` (or `date`): the curve's date, required.
- `nss_fit`: the `fi.nss_fit` output; its `inputs.curve_years` (tenors in
  years) and `inputs.curve_rates` (the fitted curve, decimals, one per
  tenor) are the curve. An entry may carry `curve_years` and
  `curve_rates` itself instead (the metric's input).
- `curve_regime`, optional: the `fi.curve_regime` output; `value` -1 is
  inverted, 0 flat, 1 upward. Absent, the regime is decided from the
  curve as the metric decides it (last rate above the first by more
  than 1e-8: upward; below: inverted; else flat).
- `forward_path`, optional: the `fi.forward_path` output; its
  `inputs.forward_years` and `inputs.forward_rates` are a forward curve
  of that date, drawn in 3D beside the date's curve when it is the
  newest shown.
- `rolldown_carry`, optional: `fi.rolldown_carry` outputs, one per tenor
  (`inputs.years`); `value`, the carry in bps, lands on that tenor's row.

Rows: `{date, tenor, tenor_label, yield, regime, kind, carry_bps}`;
`yield` in percent, `regime` `"normal"`, `"flat"` or `"inverted"`, `kind`
`"spot"` or `"forward"`, `carry_bps` or null. 2D: yield by tenor across
and date down (spot rows). 3D: each date's curve as a line over tenor
(evenly spaced by rank) at its depth, oldest at the back, coloured by
regime (teal upward, amber flat, red inverted), the newest white, the
forward curve amber in front of it; it replays date by date
(`encoding.time` on `date`), scales spanning the whole history.
Options: `fill: true` adds the lit surface under the curves, `ribs: true`
lines along date at each tenor; `colors` maps regimes to colours.

## credit-spreads

`data`: `fi.spread_suite` and/or `fi.spread_pctile` outputs, any order:

```json
[{"key": "spread_pctile", "value": 8.33,
  "inputs": {"rating": "AAA", "years": 7.0, "spread_bps": 40.0,
             "spread_history_bps": [53.7, 53.3, ...]}},
 {"key": "spread_suite", "value": 72.5, "inputs": {"rating": "A", "years": 5.0, "spread_bps": 72.5}}]
```

- `inputs.rating` and `inputs.years` (the tenor) name the cell, both
  required. Outputs of one cell merge into one row.
- `spread_suite`'s `value` is the spread (bps); `spread_pctile`'s
  `value` is its percentile (0 to 100) in `inputs.spread_history_bps`,
  and its `inputs.spread_bps` the spread when no `spread_suite` gives one.
- Ratings rank as `fi.rating_notch` ranks them, D (0) to AAA (21);
  unknown ratings rank 0.

Rows: `{rating, notch, tenor, tenor_label, spread_bps, pctile}`, best
rating first, then by tenor; `pctile` null without `spread_pctile`. 2D:
spread by tenor across and rating down. 3D: each rating's curve over
tenor, AAA in front and the worst rating behind, height the spread on a
log scale (`scale: "linear"` for linear), lines along rating at each
tenor joining them into a surface; each segment coloured by its cells'
percentile (green tight to red wide), or by spread without one.
`fill: true` adds the lit surface.

## cds-term-structure

`data`: the `credit.cds_isda_implied_hazard_pd` outputs by date, oldest
first, each alone or with the input it was run on (which carries the
par spreads the output does not repeat):

```json
[{"input": {"reference_entity": "Northwind Industrials Inc", "as_of": "2026-04-20",
            "discount_rate": 0.038, "recovery": 0.4,
            "spreads": [{"maturity": "2027-06-20", "spread_bp": 210.0}, ...]},
  "output": {"reference_entity": "Northwind Industrials Inc", "as_of": "2026-04-20",
             "recovery": 0.4, "internal_only": true,
             "curve": [{"maturity": "2027-06-20", "hazard_rate": 0.0352,
                        "cumulative_default_probability": 0.0403}, ...]}}]
```

- `output.as_of` (`YYYY-MM-DD`) and `output.curve` are required; each
  node needs `maturity`, `hazard_rate` and
  `cumulative_default_probability`.
- `input.spreads`, optional: each maturity's `spread_bp`.
- A node's tenor is its whole years to maturity from `as_of`, rounded
  (standard CDS maturities, 20 Jun / 20 Dec, sit within a quarter of
  a whole year).

Rows: `{as_of, tenor, tenor_label, maturity, spread_bp, hazard_bps,
pd_pct, value}`; `hazard_bps` the hazard rate in bps, `pd_pct` the
cumulative default probability in percent, `spread_bp` null without the
input, `value` the par spread when every row has one, else the hazard
rate. The `measure` slot picks the field drawn: `value` (default),
`spread_bp`, `hazard_bps` or `pd_pct`. 2D: that field by tenor across
and date down. 3D: as yield-curve, coloured by level (blue low to red
high), replayed date by date. These outputs are `internal_only`: the
chart is for the analyst's own Emacs, not for publishing.

## key-rate-durations

`data`: one entry per holding:

```json
[{"holding": "UST 30Y 4.625% 2055",
  "krd": {"key": "krd", "value": 16.44,
          "inputs": {"key_rate_shocks": [0.01, 0.05, 0.18, 0.7, 4.9, 10.6],
                     "curve_years": [0.5, 2, 5, 10, 20, 30]}},
  "dv01": {"key": "dv01", "value": 0.1662, "inputs": {"duration": 16.44, "price": 101.1}}}]
```

- `holding`, required: its name.
- `krd`: the `fi.krd` output; `inputs.key_rate_shocks` is the duration
  at each key rate, `value` their sum (the duration).
- The key-rate tenors: the entry's `tenors`, else `krd.inputs.curve_years`
  when as many, else `financial-chart-3d-rates-key-rates` (0.5, 2, 5, 10,
  20, 30); another count is an error (`tenor_mismatch`).
- `dv01`, optional: the `fi.dv01` output (per 100 par).

Rows: `{holding, tenor, tenor_label, krd, duration, dv01}`, holdings in
order. 2D: duration by key rate across and holding down, on a diverging
scale (short exposure is negative). 3D: a bar per holding and key rate,
up for long exposure and down through the floor for short, each holding
its colour, first holding at the back; `total: true` adds each
holding's whole duration in a last column.
