# Valuation stadium (football field)

`valuation-stadium` draws `valuation.stadium/1`, the JSON valuation-rs
emits for `valuation stadium`: every valuation method enabled for an asset
class, run over one asset. This package never runs a method and never
fetches an input; the caller supplies the JSON (bare, or in valuation-rs's
`{contract, ok, data}` envelope). Its sibling surfaces over a grid of
assumptions are in [3d-valuation.md](3d-valuation.md).

```text
valuation stadium --request examples/stadium/long_equity.json --auto-assume > stadium.json
jq '{data: .}' stadium.json > bindings.json          # the template's one slot
fc-eas render valuation-stadium --data bindings.json --backend text --raw
```

```elisp
(eas-resolve "valuation-stadium" (list :data STADIUM))       ; STADIUM: the parsed JSON
(eas-view-open "valuation-stadium" :bindings (list :data STADIUM) :id "acme")
```

`fc-eas` is the shell function in the README ("Charts from the shell").
The example bindings are `examples/valuation-stadium.data.json`
(`fc-eas example valuation-stadium --raw`); real outputs of valuation-rs
are in `test/fixtures/valuation/` (`stock`, `cds`, `re-multifamily`).

## What it draws

Text (64 to 100 columns, deterministic) and SVG, from the same template:

```text
                                ACME Corp common (illustrative inputs)  (long_equity, default)
         Comparables: EV/EBITDA┤                      │    │            │               ▓▓▓│▓▓▓▌
      DCF: free cash flow to t…┤                      │    │            │▓▓▓▓▓▓▓│▓▓▓▓▓▓▓▓▓▓▎
      ...
                                ■ income  ■ market  ■ cost  ■ arbitrage  ■ contingent_claim
Net current asset value (net-n…┤needs current_assets, total_liabilities
Reverse DCF (market-implied gr…┤2.04% rate
             8 of 10 methods ok; dispersion 57.5%; weighted mean 101.36; median 85.34; market 140.00
```

(`test/golden/eas-templates/template-valuation-stadium.txt` is the full
golden.)

- Header: asset, class, profile.
- One bar per `ok` row with `role: value`: `low` to `high`, a tick at
  `value`, a point when the method has no range; coloured by `approach`
  (legend beneath). Highest value first, or `:group t` to group by
  approach (income, market, cost, arbitrage, contingent_claim,
  statistical, heuristic), highest first inside a group.
- Vertical rules: `reference` (market price, solid red), the summary's
  `weighted_mean` (dashed) and `median` (dotted). A missing one is left out.
- Strip beneath the axis, never on it: `role: diagnostic` rows (yields,
  spreads, implied growth: a `rate` prints as a percent), `skipped` rows
  with the inputs they `missing`, `error` rows with their `reason`, and
  `not_implemented` rows.
- Caption: methods ok, `dispersion`, weighted mean, median, market price.

All `value` rows share one price axis, so they must share one `unit`
(`mixed_units` otherwise).

## Drill-down

Selecting a bar (a click on the range or its tick, `RET` on it) runs the
action `valuation-drill`, which opens that method's tree in an outline
buffer `*valuation: METHOD*` (`outline-mode`; `TAB` folds, `q` quits):
value, its `lines` (formula, and the values of what each is `from`), its
`assumptions` with provenance, and `components`, each a sub-tree. A
component's line resolves names in its parents' assumptions.

```text
* DCF: free cash flow to the firm at WACC [eq.dcf_fcff] = 163.364  (range 140.419 to 194.382)
** lines
*** value_per_share = 163.364
   equity value / shares outstanding
   from equity_value = 16,336,402,080, shares_outstanding = 100,000,000
** assumptions
*** risk_free_rate = 0.0484 (fred DGS10 as of 2026-11-05)
   url: https://fred.stlouisfed.org/series/DGS10
   transform: percent / 100
** components
*** wacc [coc.wacc] = 0.0919924
***** cost_of_equity [coc.capm] = 0.0979
```

Why an outline buffer, not a template: eas templates are marks on axes.
The tree is prose (formulas, source notes) of unbounded depth, which wants
folding, search and copy; a chart of it would be a worse text buffer. The
same string is `(financial-chart-valuation-trace STADIUM "eq.dcf_fcff")`
and `(financial-chart-valuation-drill STADIUM METHOD t)` opens the buffer.
`valuation trace` in valuation-rs prints the same tree from the CLI.

## Agent verbs

The template speaks the usual `chart/v1` verbs (a failure names its reason
and evidence):

```sh
fc-eas describe templates                                  # lists valuation-stadium and its slots
fc-eas check valuation-stadium --data bindings.json         # SHAPE_INVALID names row index and field
fc-eas render valuation-stadium --data bindings.json --backend svg --raw > stadium.svg
```

```elisp
(eas-agent "open" "valuation-stadium" :data (list :data STADIUM) :id "acme")
(eas-agent "dispatch" "acme" "{\"type\":\"click\",\"px\":[600,60]}")   ; selects a bar: :click's :result
(eas-agent "inspect" "acme")                                          ; hover, click, domains
```

A click answers `:action "valuation-drill"` and `:result "trace *valuation:
METHOD*"`; the tree is then `(with-current-buffer "*valuation: METHOD*"
(buffer-string))`.

## Validation

`financial-chart-valuation-validate` is strict and signals
`financial-chart-invalid-data` with `:code`, `:index` (the row) and
`:field`; through eas it is `SHAPE_INVALID` with the same `field` and
`index`, the code as `reason`.

| code | when |
|---|---|
| `not_a_plist`, `missing_field` | not an object; no `class`, `rows`, or a row lacks `method`, `status`, `approach`, `unit` |
| `wrong_contract` | `contract` is not `valuation.stadium/1` |
| `invalid_status`, `invalid_approach`, `invalid_role` | outside the contract's enums |
| `not_a_number` | an `ok` row's `value`, `low`, `high` or the `reference` is not finite |
| `range_order` | not `low <= value <= high`, or `high` without `low` |
| `mixed_units` | value rows of two units on one axis |
| `invalid_detail` | a row's `detail` (or a component's) lacks a numeric `value` |
| `unknown_method`, `no_detail` | `financial-chart-valuation-trace` on a missing, skipped or failed row |

There is no 3D variant: the stadium is categorical (methods by value), not
a surface, so `templates3d/` has no entry for it.
