# invest-metrics golden cases (copied)

Verbatim copies of the dotfiles repository's
`src/rust/crates/invest-metrics/tests/fixtures/golden/` cases at
ead6f883ea7, so the 3D templates' adapters can be tested against
real metric output without the dotfiles checkout:

- options_grids.json: opt.greeks_grid, opt.payoff_grid, opt.pop_grid
- kelly_growth.json: stats.kelly.growth
- risk_rolling.json: stats.rolling.correlation, stats.rolling.factor_regression, port.frontier

- fi.json: fi.nss_fit, fi.krd, fi.spread_suite, fi.curve_regime and the other fi.* metrics
- opt.json: opt.gex, opt.chain_stats, opt.skew_rr25_bf25, opt.vrp, opt.max_pain, ...
- ta_v2.json: ta.rel_strength
- indicator_stats.json: stats.risk_contribution, stats.var, ...
- port.json: port.stress

Refresh by copying them again; the invest lane owns their shape.
