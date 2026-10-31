# Options surfaces in 3D: data contracts

Three options charts, each a 2D template in `templates/financial/` (drawn
by eas, needs nothing else) and its 3D variant in `templates3d/` (drawn by
[graphics.el](https://gitlab.com/davidawad/graphics-el) when it is
installed):

| template | 3D mark | replay | reads |
|---|---|---|---|
| `greeks-surface` | `fc-greeks3d` | toward expiry | one greek over spot x days to expiry |
| `position-pnl-surface` | `fc-pnl3d` | toward expiry | a multi-leg position's P/L over spot x days to expiry, the expiry payoff its front edge |
| `pop-surface` | `fc-pop3d` | none | probability of profit (or of touch) over strike x expiry |

`src/financial-chart-3d-options.el` registers the marks;
`scripts/templates3d_options.py` writes the 3D variants from the 2D
templates; `scripts/options3d_fixtures.py` writes the fixtures in
`test/fixtures/3d/options/` and the same files as each template's example
in `examples/3d/` (which ships with the package); the 3D renders are in
`docs/3d.md`.

This package never fetches or computes market data. Each template takes
the rows an invest-metrics metric emits (dotfiles
`src/rust/crates/invest-metrics`, `invest metric run ID`), so the invest.el
boards can feed them directly:

```elisp
(require 'financial-chart-3d)
;; OUTPUT: the metric run's JSON, parsed (`json-parse-string' with
;; :object-type 'plist :array-type 'array, or `eas-json-read-file').
(financial-chart-3d-options-show
 "greeks-surface"
 (financial-chart-3d-options-bindings "greeks-surface" OUTPUT :greek "vega"))
;; 3D in a graphics.el buffer; without graphics.el, the 2D template
;; in an eas buffer.  Or explicitly:
(financial-chart-3d-open "position-pnl-surface" :bindings B :show t)
(eas-view-open "position-pnl-surface" :bindings B)        ; 2D
```

`financial-chart-3d-options-bindings TEMPLATE OUTPUT &rest SLOTS` maps a
metric run to bindings: `rows` to `data` (adding `day` where missing),
`params.spot` to `spot`, `title` to `title`, then SLOTS.

## Request for the invest-metrics lane

The scalar metrics this needs exist (`opt.bsm_greeks`,
`opt.greeks.second_order`, `opt.spread.payoff` / `opt.payoff_curve`,
`opt.probability_of_profit`, `opt.prob_touch`), but each evaluates one
point. The surfaces need them over a grid. Requested: three grid metrics
that evaluate those over the grid their params name and emit the shapes
below. Until they land the templates are driven from the fixtures, which
are exactly what they should emit for the params shown (closed-form
Black-Scholes-Merton, no dividends; `financial-chart-3d-options-test.el`
checks the fixtures against its own BSM).

Common envelope (what `financial-chart-3d-options-bindings` reads):

```json
{
  "metric": "opt.greeks_grid",
  "title": "AAPL 2026-12-18 100C",
  "as_of": "2026-10-31",
  "params": {"spot": 100.0, "...": "..."},
  "rows": [{"...": "..."}]
}
```

Conventions for every grid:

- Rows are tidy, one per grid point, ordered furthest `dte` first, then by
  the x field ascending. Every (x, dte) pair of the grid is present once.
- `dte` is calendar days to expiry (integer). `day` is calendar days from
  the grid's furthest date (`max(dte) - dte`), the replay's clock; the
  bindings helper adds it when absent.
- Numbers are JSON numbers; a value that cannot be computed is `null` (the
  surface draws it at the grid's minimum), never a string.
- Grid sizes that draw well: 15 to 40 x values, 5 to 10 dates.

### `opt.greeks_grid` -> `greeks-surface`

Params: `kind` (`call`/`put`, or `legs` as in `opt.spread.payoff` for a
position's net greeks), `strike`, `expiry`, `vol` (annualised), `rate`,
`div_yield`, `spot` (now), `spots` (`[lo, hi, step]`), `dtes` (list).

Rows:

| field | unit | meaning |
|---|---|---|
| `spot` | price | underlying price of the grid point |
| `dte` | days | days to expiry of the grid point |
| `day` | days | `max(dte) - dte` (optional) |
| `delta` | per 1.00 of spot | dV/dS |
| `gamma` | per 1.00 of spot | d2V/dS2 |
| `vega` | per vol point | dV/dsigma / 100 |
| `theta` | per calendar day | dV/dt / 365 (negative is decay) |
| `vanna` | per vol point | d(delta)/dsigma / 100 |
| `charm` | per calendar day | change in delta as one day passes, / 365 |

Slots: `data` (rows), `greek` (`delta` `gamma` `vega` `theta` `vanna`
`charm`; default `gamma`), `spot` (a rule in 2D, the traced slice in 3D;
omit for none), `title`, `description`.

Fixture `greeks-surface.data.json`: a long 100 call, vol 0.25, rate 0.04,
spots 70 to 130 by 2.5, dte 60 45 30 21 14 7 3.

3D: a heightfield of the chosen greek, spot across, dte from the furthest
(back) to the nearest (front), each date a strip with a seam behind it; a
cool to warm gradient over the greek's range; its zero level framed when
the greek changes sign (vanna, charm); the slice at `spot` traced across
the dates and dropped to the floor. It replays date by date toward expiry
(SPC in the 3D buffer), so gamma's ridge sharpens as you watch.

2D: the greek against spot, one curve per dte, a dashed zero line and the
spot rule.

### `opt.payoff_grid` -> `position-pnl-surface`

Params: `legs` (as `opt.spread.payoff`: `[{"kind": "call", "strike": 90,
"qty": 1, "premium": ...}, ...]`, same expiry), `multiplier` (100),
`opened` (`{"spot", "dte"}` the cost is struck at, or the legs' premiums),
`vol`, `rate`, `spot`, `spots`, `dtes` (must include 0).

Rows:

| field | unit | meaning |
|---|---|---|
| `spot` | price | underlying price |
| `dte` | days | days to expiry; 0 is the expiry payoff |
| `day` | days | `max(dte) - dte` (optional) |
| `pnl` | currency | position value at (spot, dte) minus its cost, times the multiplier |

Slots: `data`, `spot`, `y_title` (default `P/L`), `title`, `description`.

Fixture `position-pnl-surface.data.json`: a long 90/100/110 call butterfly
opened at spot 100 with 45 days left (vol 0.25, rate 0.04, multiplier
100), spots 80 to 120 by 2, dte 45 38 31 24 17 10 5 2 0.

3D: P/L as a heightfield, red (loss) to green (gain), the zero (breakeven)
level framed; the expiry payoff (dte 0) is the front edge, drawn dim from
the first frame. The replay fills the surface date by date, its newest
date traced bright, until it meets that edge: theta decay as the tent
rises toward expiry.

2D: P/L against spot, one curve per dte, the expiry payoff emphasised, a
dashed zero line and the spot rule.

### `opt.pop_grid` -> `pop-surface`

Params: `strategy` (`short_put`, `short_call`, `long_call`, ... as
`opt.probability_of_profit` takes, or `legs`), `vol`, `rate`, `spot`,
`strikes` (`[lo, hi, step]`), `dtes`.

Rows:

| field | unit | meaning |
|---|---|---|
| `strike` | price | the (short) strike |
| `dte` | days | days to expiry |
| `pop` | fraction 0..1 | probability the trade is profitable at expiry (`opt.probability_of_profit`) |
| `prob_touch` | fraction 0..1 | probability the strike trades before expiry (`opt.prob_touch`) |
| `credit` | price | premium received (or paid, negative) per share |
| `breakeven` | price | expiry breakeven |

Slots: `data`, `metric` (`pop` or `prob_touch`; default `pop`), `spot`,
`title`, `description`.

Fixture `pop-surface.data.json`: a short put, spot 100, vol 0.25, rate
0.04, strikes 70 to 110 by 2.5, dte 90 60 45 30 21 14 7; `pop` is the
risk-neutral probability of finishing above the breakeven, `prob_touch`
the lognormal first-passage probability (1 at and above spot).

3D: the chosen probability as a heightfield over strike and expiry,
coloured on the absolute scale (red at 0, green at 1, whatever the rows'
range), the even-odds (0.5) level framed. Strikes by expiry are not a
time series, so it does not replay.

2D: the probability against strike, one curve per dte, a dashed 0.5 line
and the spot rule.
