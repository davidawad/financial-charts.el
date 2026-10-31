# invest-metrics golden cases (copied)

Verbatim copies of the dotfiles repository's
`src/rust/crates/invest-metrics/tests/fixtures/golden/` cases at
ead6f883ea7, so the 3D templates' adapters can be tested against
metric output without the dotfiles checkout:

- options_grids.json: opt.greeks_grid, opt.payoff_grid, opt.pop_grid
- kelly_growth.json: stats.kelly.growth
- risk_rolling.json: stats.rolling.correlation, stats.rolling.factor_regression, port.frontier

- fi.json: fi.nss_fit, fi.krd, fi.spread_suite, fi.curve_regime and the other fi.* metrics
- opt.json: opt.gex, opt.chain_stats, opt.skew_rr25_bf25, opt.vrp, opt.max_pain, ...
- ta_v2.json: ta.rel_strength
- indicator_stats.json: stats.risk_contribution, stats.var, ...
- port.json: port.stress
- fut_term_structure.json: local golden-shaped cases for fut.term_structure (contango, backwardation, mixed, single, and dated contracts)
- es_curve.json: a representative invest-fetch Schwab /ES raw-curve shape fixture

The futures metric fixtures are retained locally because the source dotfiles checkout is not part of this standalone package checkout. Refresh the copied upstream goldens when that checkout is available. For upstream cases, refresh by copying them again; the invest lane owns their shape.

## runs.json (generated)

A golden pins a few JSON pointers of an output; the adapters
(`financial-chart-3d-bindings-from-metric`) read whole outputs. runs.json
is invest-metrics' own output at the same revision, written by
`scripts/invest-metrics-runs.py` (which runs
`scripts/invest-metrics-runall.rs` inside a dotfiles checkout; its
docstring says how):

- `cases`: every golden case above of a metric the adapters take, run
  on its input. `financial-chart-3d-metrics-test.el` checks each against
  the golden's pointers.
- `series`: hand-chosen inputs run as a board runs them, one entry per
  date, tenor, expiry, holding, scenario or strategy (with its input),
  plus the `--grid` runs of eq.dcf and stats.kelly.growth, and runs of
  metrics with no golden here (market.breadth_pct_above_200d,
  credit.cds_isda_implied_hazard_pd on the rates fixture's inputs,
  opt.iv_surface). Every metric/template pair draws from these.

Regenerate it after refreshing the goldens.
