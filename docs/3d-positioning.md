# Options positioning in 3D: data contracts

Four charts of options positioning, each a 2D template in
`templates/financial/` (what eas draws when graphics.el is absent) and
its 3D variant in `templates3d/` (`fc3d/NAME`, drawn by graphics.el).
`src/financial-chart-3d-positioning.el` registers their marks;
`scripts/templates3d-positioning.py` writes both templates. Their goldens
are in [3d.md](3d.md).

| chart | metrics | 3D mark | replay |
|---|---|---|---|
| `options-oi` | `opt.chain_stats`, `opt.max_pain` | `fc-strikes3d` | |
| `options-gex` | `opt.gex` | `fc-strikes3d` | |
| `options-skew` | `opt.skew_rr25_bf25` (or `opt.positioning_skew`) | `fc-skew3d` | by snapshot date |
| `options-vrp` | `opt.vrp`, `stats.realized_vol`, `opt.iv_rank`, `opt.calendar_iv_ratio` | `fc-vrp3d` | by day |

This package never fetches data. Each template takes the rows the named
invest-metrics metric emits (`invest metric run ID`, dotfiles
`src/rust/crates/invest-metrics`), so an invest.el board binds a
metric's output to a slot as it is:

```elisp
(financial-chart-3d-open "options-oi" :show t
                         :bindings (list :data CHAIN-ROWS :max_pain MAX-PAIN-ROWS :spot 586.15))
(financial-chart-3d-open "options-vrp" :show t :bindings (list :data VRP-ROWS))
;; 2D, no graphics.el needed:
(eas-view-open "options-oi" :bindings (list :data CHAIN-ROWS :spot 586.15)) ; no max-pain marks
```

Rows are JSON objects (plists in Lisp), in a JSON array or a `data/v1`
object. Field names below are the defaults; a variant's
`x-eas.graphics.encoding` names others. Numbers may be integers or
floats; `null` and missing values are skipped. The example of each
template is its fixture in `test/fixtures/3d/options/`, a hand-derived
SPY-like snapshot as of 2026-10-30 (spot 586.15).

## options-oi: open interest by strike x expiry

Slots:

| slot | metric | shape |
|---|---|---|
| `data` (required) | `opt.chain_stats` | rows `{expiry, strike, call_oi, put_oi}`, one per strike per expiry |
| `spot` (required) | the underlying's price | number |
| `max_pain` | `opt.max_pain` | rows `{expiry, max_pain}`; computed from `data` when left out |
| `title`, `description` | | strings |

- `expiry`: an ISO date string (any value works; expiries are lanes in
  the order they first appear, so send them nearest first).
- `strike`: number. `call_oi`, `put_oi`: contracts (non-negative).
- Max pain is the strike, of those the expiry lists, minimising what its
  options pay holders at expiry: the sum over strikes K of
  `call_oi * max(0, S - K) + put_oi * max(0, K - S)`, least at S; ties go
  to the lower strike (`financial-chart-3d-positioning-max-pain`). The
  fixture's `max_pain` rows are exactly what its chain gives (590, 580,
  580, 585), which the tests check.

3D: a column per row on a strike (across) by expiry (back, nearest in
front) floor, calls rising above it and puts hanging below it, on one
scale (the tallest is `height`, default 3); spot a white plane through
the strike axis; each expiry's max pain a gold post above its lane, the
posts joined so max pain's drift across expiries reads at a glance.
2D: calls up and puts down summed over expiries, spot ruled, max-pain
strikes as gold triangles on the zero line.

## options-gex: dealer gamma exposure by strike x expiry

| slot | metric | shape |
|---|---|---|
| `data` (required) | `opt.gex` | rows `{expiry, strike, call_gex, put_gex[, net_gex]}` |
| `spot` (required) | the underlying's price | number |
| `title`, `description` | | strings |

- `call_gex`, `put_gex`: dealer gamma exposure in dollars per 1% move
  (the fixture uses $M: `gamma * OI * 100 * spot^2 * 0.01 / 1e6`, gamma
  from Black-Scholes at a skewed IV). Put gamma is negative (dealers
  short the puts customers buy); the chart draws `|call_gex|` up and
  `|put_gex|` down, so either sign convention for puts draws the same.
- `net_gex` is optional and not drawn; it is the tooltip's.
- The gamma flip is where net gamma by strike (summed over expiries,
  `|call| - |put|`) changes sign between neighbouring strikes,
  interpolated linearly; of several, the one nearest spot
  (`financial-chart-3d-positioning-gamma-flip`). The template computes it
  (`"flip": true` in its `x-eas.graphics`; the mark also takes a price
  there). In the fixture it is 582.29, below spot: positive gamma.

3D: as `options-oi`, with the flip a violet plane beside the spot one.
2D: call gamma up and put gamma down summed over expiries, spot ruled.

## options-skew: 25-delta risk reversal and butterfly over time

| slot | metric | shape |
|---|---|---|
| `data` (required) | `opt.skew_rr25_bf25` | rows `{date, tenor, rr25, bf25}`, one per tenor per snapshot |

- `date`: the snapshot's ISO date (sorted as strings, so ISO dates sort
  in time). `tenor`: days to expiry (numbers; sorted, evenly spaced as
  lanes). `rr25`: 25-delta call IV minus put IV, vol points (negative
  for equity index put skew). `bf25`: 25-delta butterfly, the wings'
  average IV minus ATM IV, vol points.
- The fixture: 13 weekly snapshots, 2026-08-07 to 2026-10-30, tenors 7,
  30, 60, 90 and 180 days, with a skew-steepening stress in early
  October that hits the short end hardest.
- `opt.positioning_skew` (put versus call positioning by tenor over
  time) draws the same way: bind its rows `{date, tenor, pos_skew}` and
  set the 3D variant's `panels` to `["pos_skew"]` (the mark takes any
  value fields, one panel each).

3D (replays snapshot by snapshot, `encoding.time` on `date`): one
panel per field (`panels`, default `rr25` above `bf25`), each with a
ribbon per tenor running across time (shortest tenor in front, warm;
longest at the back, cool), its zero level framed, and the newest
snapshot's term structure traced white across the tenors. Each panel's
scale spans all snapshots, so the replay grows ribbons in place.
2D: two facets, one line per snapshot over tenor.

## options-vrp: implied against realized volatility

| slot | metric | shape |
|---|---|---|
| `data` (required) | `opt.vrp` joined by date with `stats.realized_vol`, `opt.iv_rank`, `opt.calendar_iv_ratio` | rows `{date, iv, rv[, vrp, iv_rank, calendar_ratio]}` |

- `date`: ISO date, one row per day (oldest first). `iv`: 30-day ATM
  implied volatility, percent. `rv`: 20-day realized volatility,
  percent (`stats.realized_vol`). `vrp`: `iv - rv`, optional (the
  ribbon is drawn from `iv` and `rv`; `financial-chart-3d-positioning-vrp-rows`
  adds it to rows without one, for the 2D tooltip). `iv_rank`: 0 to 100
  (`opt.iv_rank`), optional. `calendar_ratio`: front over back implied
  volatility (`opt.calendar_iv_ratio`, above 1 is backwardation),
  optional.
- The fixture: 44 trading days, 2026-09-01 to 2026-10-30, a volatility
  spike in early October (IV to 24.5, the term structure inverting)
  that realized volatility catches a week later, the premium briefly
  negative in mid-October.

3D (replays day by day): the middle lane is one slab between IV (its
white edge) and RV (its light-blue edge), filled green where IV is
above RV (the premium option sellers earn) and red where RV overtakes
it, split exactly where they cross; behind it, a bar per day as tall as
the IV rank; in front, the IV ratio on its own scale with 1.0 ruled,
red above it. 2D: IV and RV lines with the band between them filled.
