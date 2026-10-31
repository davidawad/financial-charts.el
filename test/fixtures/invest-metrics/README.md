# invest-metrics golden cases (copied)

Copies of the dotfiles repository's golden cases, so the 3D templates'
adapters can be tested against metric fixtures without the dotfiles checkout:

- options_grids.json: opt.greeks_grid, opt.payoff_grid, opt.pop_grid
- options_forward.json: opt.implied_forward, opt.risk_neutral_density
- kelly_growth.json: stats.kelly.growth
- risk_rolling.json: stats.rolling.correlation, stats.rolling.factor_regression, port.frontier

- fi.json: fi.nss_fit, fi.krd, fi.spread_suite, fi.curve_regime and the other fi.* metrics
- opt.json: opt.gex, opt.chain_stats, opt.skew_rr25_bf25, opt.vrp, opt.max_pain, ...
- ta_v2.json: ta.rel_strength
- indicator_stats.json: stats.risk_contribution, stats.var, ...
- port.json: port.stress
- futures.json: copied fut.term_structure golden inputs and expected pointers (including valid and rejected cases)
- ../3d/futures/curve-es.json: a recorded invest-fetch Schwab /ES raw curve
- ../3d/futures/term-structure-es.json: its computed term-structure output

`options_forward.json` is from the `opt-forward` branch; the NVDA density
and forward outputs are in `test/fixtures/3d/density-surface/`. Refresh by
copying them again; the invest lane owns their shape.

`futures.json` is from dotfiles master (fut.term_structure goldens); the
recorded /ES curve and its term structure are in `test/fixtures/3d/futures/`.

## runs.json (generated)

A golden pins a few JSON pointers of an output; the adapters
(`financial-chart-3d-bindings-from-metric`) read whole outputs. runs.json
combines the older metrics outputs and the `opt-forward` cases, written by
`scripts/invest-metrics-runs.py` (which runs
`scripts/invest-metrics-runall.rs` inside a dotfiles checkout; its
docstring says how):

- `cases`: every golden case above of a metric the adapters take, run
  on its input. `financial-chart-3d-metrics-test.el` checks each against
  the golden's pointers. Credit curve cases carry pointer samples from
  `credit_curve.rs`; their complete recorded outputs are in `series`.
- `series`: hand-chosen inputs run as a board runs them, one entry per
  date, tenor, expiry, holding, scenario or strategy (with its input),
  plus the `--grid` runs of eq.dcf and stats.kelly.growth, and runs of
  metrics with no golden here (market.breadth_pct_above_200d,
  credit.cds_isda_implied_hazard_pd on the rates fixture's inputs,
  opt.iv_surface). Every metric/template pair draws from these.

Regenerate it after refreshing the goldens.

## Credit curve fixtures

The NVDA credit inputs and outputs are copied from `davidawad/dotfiles`
branch `credit-curve` at commit `5c16ea47caa1cc3f68911b7f32323189219eb64d`
(2026-10-31):

- `credit/term.json` and `credit/ladder.json` are the recorded outputs of
  `credit.cds_term_structure` and `bond_corp.cds_implied_ladder`.
- `credit/view.json`, `credit/treasury.json`, and `credit/edgar.json` are
  the recorded board inputs.
- `credit_curve.rs` and `golden/credit_curve.json` retain the metric tests
  and expected values. `credit_nvda_view.json` is the view golden.
- `test/fixtures/3d/rates/{term,ladder,view,treasury,edgar}.json` are
  byte-for-byte copies of those board fixtures. The 3D template's
  `cds-bond-ladder.data.json` wraps the real ladder output with its title.
- The credit metrics landed after the pinned runall revision above. Their
  `runs.json` series entries contain the full recorded NVDA outputs verbatim
  from `credit/term.json` and `credit/ladder.json`; these are recorded metric
  outputs, not runall runs. The cases retain the Rust golden inputs and the
  output pointers asserted by `credit_curve.rs`. The runs generator must keep
  both kinds of credit entries when it regenerates `runs.json`.
