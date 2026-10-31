# Market structure in 3D: data contracts

Four templates added by `src/financial-chart-3d-market.el` (loaded by
`financial-chart-3d.el`), each with a 3D variant in `templates3d/` and a
2D fallback in `templates/financial/`:

| template | 3D mark | replay | input metric |
|---|---|---|---|
| `rrg` | `fc-rrg3d` | date by date | `ta.rel_strength` |
| `sector-breadth` | `fc-breadth3d` | day by day | `breadth.rs` |
| `liquidity-heatmap` | `fc-liquidity3d` | step by step; streams live | order-book snapshots (as `depth-live`) |
| `session-profile` | `fc-session-profile3d` | session by session | volume profile per session, `ta.vwap` |

This package never fetches data. Each template takes the rows below, which
is what the named invest-metrics metric (`invest metric run ID`) is to
emit, so the invest.el boards can bind its JSON straight to the `data`
slot: an array of row objects, or a data/v1 object with `rows`. Callers
holding prices, bars or a book instead build the rows with the helper
named under each template.

```elisp
(financial-chart-3d-open "rrg" :bindings (list :data ROWS :tail 8) :show t)
(eas-show (eas-view-open "rrg" :bindings (list :data ROWS)))   ; 2D, no graphics.el
```

Without graphics.el the 2D fallback draws from the same bindings (`rrg`,
or `fc3d/rrg`, which eas draws flat); the 3D commands signal
`financial-chart-3d-unavailable`. Every variant is in
`M-x financial-chart-3d-gallery` and on the [gallery page](3d.md), with its
golden render. Fields not listed are ignored, so a metric can carry more.

The examples are `test/fixtures/3d/market/NAME.data.json`, written by
`(financial-chart-3d-market-write-fixtures)` from seeded generators
(`make test-3d` checks they still match).

## rrg: relative rotation graph

Rows, one per symbol per date:

| field | type | meaning |
|---|---|---|
| `date` | ISO date string (or a number) | the period; dates sort as strings |
| `symbol` | string | the sector or instrument |
| `rs_ratio` | number | JdK RS-Ratio: relative strength against the benchmark, 100 = in line |
| `rs_momentum` | number | JdK RS-Momentum: the rate of change of `rs_ratio`, 100 = flat |

Slots: `data` (required), `tail` (integer, default 8: dates of trail per
symbol), `title`, `description`.

Helper: `(financial-chart-3d-rrg-rows CLOSES BENCHMARK :dates D :ratio 10
:momentum 5)`, CLOSES an alist `(SYMBOL . PRICES)`. rs = close /
benchmark; rs_ratio = 100 * rs / mean(rs, last 10); rs_momentum = 100 *
rs_ratio / mean(rs_ratio, last 5): an open approximation of JdK's
measures that rotates clockwise like them. A metric's own values take
precedence.

3D: the RRG lies on the floor, RS-Ratio across and RS-Momentum away from
the viewer, 100 at the centre, each axis scaled to the farthest point of
the whole series (a replay never rescales). The floor's edges are coloured
by quadrant: leading (top right) green, weakening yellow, lagging red,
improving blue. Each symbol's trail of its last `tail` dates climbs to its
newest point, a cube with a stem down to where it stands now. The replay
(`encoding.time` on `date`) rotates the trails date by date.

2D: the classic RRG: trails over the last `tail` dates, a labelled dot at
each newest point, dashed rules at 100.

## sector-breadth: market breadth by sector over time

Rows, one per sector per date:

| field | type | meaning |
|---|---|---|
| `date` | ISO date string | the trading day |
| `sector` | string | the sector, drawn in order of first appearance |
| `pct` | number 0-100 | percent of the sector's members above their moving average (50-day in the example) |

Slots: `data` (required), `title`, `description`.

3D: one line per sector running from the oldest date (back) to the newest
(front), its height and colour the breadth (red at 0, green at 100), the
50% level framed and the newest date traced white across the sectors.
`"style": "surface"` in `x-eas.graphics` draws a lit heightfield instead.
The replay grows it day by day toward the viewer.

2D: a heatmap, date across and sector down, red-yellow-green around 50.

## liquidity-heatmap: order-book depth by price and time

Rows: one per book level per step, as `financial-chart-book-rows` gives
them (the `depth-live` contract) with the step added:

| field | type | meaning |
|---|---|---|
| `step` | integer | the snapshot, 0 first |
| `side` | `"bid"`, `"ask"` or `"mid"` | one `mid` row per step carries the mid price |
| `price` | number | the level's price |
| `size` | number | resting size at the level (not cumulative) |
| `mid` | number | the step's mid price (every row carries it) |

Slots: `data` (required), `title`, `description`.

Helper: `(financial-chart-3d-liquidity-rows BOOK LEVELS STEP)`, BOOK from
`financial-chart-book-make` on a snapshot `{bids: [[price, size], ...],
asks: [...], tick}`. `financial-chart-3d-market-snapshot-deltas` turns
consecutive snapshots into the deltas a live view takes.

Live, as `depth-live` streams:

```elisp
(setq v (financial-chart-3d-liquidity-open SNAPSHOT :levels 16 :history 60 :show t))
(financial-chart-3d-liquidity-push v DELTAS)   ; {op, side, price, size}, as financial-chart-book-push
(financial-chart-3d-liquidity-close v)
M-x financial-chart-3d-demo-liquidity           ; the example book, streaming
```

3D: time across, price away from the viewer (highest at the back, on the
book's tick grid), the newest `max_rows` (60) steps. Every level resting
at least `wall` (3) times the median size stands up as a block, dark blue
to amber with size: the walls Bookmap shows as bright bands. The mid
price runs across the floor between them in white. `"style": "surface"`
draws every level as a lit heightfield (Bookmap's heatmap raised), and
`"ridges"` as one line per price. The replay steps through the snapshots;
a live view drops it and shows the newest step.

2D: price up, step across, each level a square coloured by its size
(inferno), the mid traced white.

## session-profile: volume profile by session

Rows, one per price level per session, every session over the same levels:

| field | type | meaning |
|---|---|---|
| `session` | ISO date string | the session |
| `price` | number | the level's middle |
| `volume` | number | volume traded at the level in the session |
| `poc` | 0 or 1 | optional: 1 at the session's point of control |
| `vwap` | number | optional: the session's VWAP (`ta.vwap`), the same on each of its rows |
| `low`, `high` | number | optional: the level's bounds |

Slots: `data` (required), `title`, `description`.

Helper: `(financial-chart-3d-session-profile BARS :bins 24 :session FN)`
from OHLCV bars (oldest first): BINS equal levels over every bar's range,
shared by all sessions; each bar's volume spread over its low-high range
(as the `volume-profile` template estimates it); the session of a bar is
the date of its `time` (ISO string, or epoch milliseconds UTC) unless FN
says otherwise; the VWAP weights each bar's typical price by its volume.

3D: price across, sessions from the back (oldest) toward the viewer, one
slab per session whose top is its profile (one height scale for every
session, so a heavy day stands taller), coloured old blue to new amber.
Each point of control is capped white and the VWAPs are joined in amber
across the floor: value migrating session by session. The replay adds one
session at a time.

2D: session across, price up, each level a square shaded by volume, the
point of control ringed and the VWAP ticked.
