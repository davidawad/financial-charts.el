# vol-surface: implied volatility by strike and expiry

An option chain's implied volatility (IV) as a surface over strike and days
to expiry. Two templates share one data contract:

- `vol-surface` (`templates/financial/vol-surface.json`, 2D, eas): a heatmap
  of the **latest** quote time in the rows, strike across, days to expiry
  down, coloured by IV (viridis), hover readout `K`, `DTE`, `IV%`. Needs
  nothing beyond eas.
- `fc3d/vol-surface` (`templates3d/vol-surface.json`, 3D, graphics.el): the
  same template body plus `x-eas.graphics`, mark `fc-vol-surface3d`
  (`src/financial-chart-3d-vol-surface.el`). It replays the session one
  quote time at a time (`encoding.time` on `time`, mode `frame`, 1 s per
  snapshot, the surface tweening between them, looping) while the camera
  sways 15 degrees and back. `SPC` plays or pauses, `,` `.` step, arrows
  orbit. Without graphics.el it still draws in 2D and the 3D commands
  signal `financial-chart-3d-unavailable`.

## Rows (`data`)

One row per strike, expiry and quote time:

| field | type | meaning |
|---|---|---|
| `time` | string, ISO 8601 (`2026-10-30T10:30:00`) | quote time of the snapshot; the 3D replay's keyframes, sorted |
| `spot` | number | the underlying's price at `time` (the 3D at-the-money line) |
| `strike` | number | option strike |
| `dte` | number | calendar days from `time` to expiry |
| `expiry` | string `YYYY-MM-DD` or null | expiry date (tooltip) |
| `iv` | number > 0 | implied volatility as a fraction: `0.18` is 18% |

Missing strike/expiry cells are fine. In 3D each expiry's smile is
interpolated across a missing strike (nearest value past the ends). In 2D
the cell stays empty.

## From chain snapshots

`(financial-chart-3d-vol-surface-rows CHAIN)` builds the rows from chain
snapshots: a plist `(:snapshots [...])` or just the vector/list of
snapshots:

```json
{"symbol": "SPY", "date": "2026-10-30",
 "snapshots": [
   {"time": "2026-10-30T09:30:00", "spot": 680.10,
    "chain": [{"expiry": "2026-11-06", "dte": 7, "strike": 640, "iv": 0.266}, ...]},
   ...]}
```

- The quotes can be under `chain` or `quotes`.
- A quote without `dte` gets the calendar days from `time` to `expiry`.
- Quotes with a null or missing `iv` are dropped.
- Malformed input signals `financial-chart-invalid-data` with
  `:code` `missing_time`, `missing_spot`, `missing_strike`, `missing_dte`,
  `bad_iv` or `empty_chain`, and `:index` (the snapshot).

The fixture is `test/fixtures/3d/vol-surface/spy-chain.json`: a SPY chain on
2026-10-30, with 8 quote times (09:30 to 16:00), 8 expiries (7 to 120 DTE)
and 17 strikes (640 to 720). Over the session, a midday selloff lifts the
front of the term structure and steepens the put skew, then it recovers.
It is synthetic: the chain fixture of invest-vol-surface's ERT lives in
the dotfiles repository and was not available to this package. The
template example `examples/vol-surface.data.json` is this fixture's rows
(`financial-chart-3d-vol-surface-write-example` regenerates it).

## From `opt.iv_surface` (invest-vol-surface)

`financial-chart-3d-vol-surface-rows` and `financial-chart-3d-vol-surface-open`
take the output of `invest metric run opt.iv_surface` verbatim, so
invest-vol-surface can pass it straight to `fc3d/vol-surface`. One result is
one snapshot:

```json
{"underlying_price": 769.64,
 "strikes": [768.0, 769.0, 770.0],
 "moneyness": [0.9978691336209137, 0.9991684423886492, 1.0004677511563849],
 "days_to_expiry": [11, 32],
 "iv": [[null, 0.2, 0.19], [0.26, 0.24, 0.22]]}
```

| field | | |
|---|---|---|
| `strikes` | required | numbers, one per column of `iv` |
| `days_to_expiry` | required | numbers, one per row of `iv` |
| `iv` | required | one array per expiry, each with one IV (a fraction) or `null` per strike |
| `underlying_price` | optional | the spot. If it is absent, the spot is the mean of `strike / moneyness` |
| `moneyness` | optional | `strike / spot`, one per strike. Needed when `underlying_price` is absent |
| `time` | optional | the quote time (ISO 8601) |

- A single result is one keyframe. Its time is the `:time` argument, then
  its own `time`, then now.
- A replay is an array of `{time, surface}` pairs, one result per pair:

  ```json
  [{"time": "2026-10-30T10:00:00", "surface": {"strikes": [...], "days_to_expiry": [...], "iv": [...], ...}},
   {"time": "2026-10-30T12:00:00", "surface": {...}}]
  ```

- `null` cells are dropped. The mark interpolates them along each smile.
- Rows have `expiry` null, because the result carries only days to expiry.
- Errors use `financial-chart-invalid-data`, with `:index` set to the
  snapshot (the pair's position). The codes are `missing_strike`,
  `missing_dte`, `missing_spot` (no `underlying_price` and no usable
  `moneyness`), `bad_shape` (`iv` is not `days_to_expiry` x `strikes`),
  `missing_time` and `missing_surface` (a malformed pair), plus `bad_iv` and
  `empty_chain` as above.

The fixture `test/fixtures/3d/vol-surface/iv_surface.json` is a verbatim
copy of invest-vol-surface's `test/fixtures/vol-surface/iv_surface.json`
from the dotfiles repository. A live NVDA chain is 24 expiries x 125 strikes. The
wire mesh draws at most `max_strikes` (41) of the strikes, evenly spread,
so that it stays readable. The ATM line is computed from every strike. The
tests cover the fixture, a synthetic 24 x 125 surface with nulls, and a
3-snapshot replay.

## Opening it

```elisp
(require 'financial-chart-3d)
;; 3D, from chain snapshots or rows; SPC plays the session
(financial-chart-3d-vol-surface-open CHAIN :title "SPY IV" :show t)
;; 3D, from opt.iv_surface output (one result, or {time, surface} pairs)
(financial-chart-3d-vol-surface-open SURFACE-OR-SERIES :symbol "NVDA" :show t)
;; 2D, eas only
(eas-view-open "vol-surface" :bindings (list :data (financial-chart-3d-vol-surface-rows CHAIN)))
```

`financial-chart-3d-vol-surface-open (SURFACE-OR-SERIES &key symbol title
time id show)` returns the view. `symbol` titles it "SYMBOL implied
volatility", and `title` overrides that. `time` is the time of a single
`opt.iv_surface` result.

`financial-chart-3d-bindings-from-metric` routes the metric like every
other (README, "From invest metrics"):

```elisp
(financial-chart-3d-bindings-from-metric "vol-surface" "opt.iv_surface" SURFACE :symbol "NVDA")
;; a session: entries {time, output}, one per snapshot
(financial-chart-3d-bindings-from-metric "vol-surface" "opt.iv_surface"
                                         (list (list :time "2026-10-30T10:00:00" :output S1) ...))
```

It gives `(:data ROWS :title ...)`, the rows of
`financial-chart-3d-vol-surface-rows`; a single result is at `:time`,
else its own time, else now.

A caller such as invest-vol-surface can offer the 3D view when
`(financial-chart-3d-available-p)` is non-nil. It should keep its 2D
heatmap otherwise.

## The 3D mark: `fc-vol-surface3d`

Channels `x` (strike, default field `strike`), `z` (`dte`), and `y` (`iv`).
Options in `x-eas.graphics`:

| option | default | |
|---|---|---|
| `style` | `"wire"` | `"wire"`: each expiry's smile and every `strike_every`-th strike's term line as segments, each coloured by its IV; `"surface"`: one lit heightfield |
| `max_strikes` | 41 | most strikes the wire mesh draws, evenly spread (the ATM line uses all of them) |
| `strike_every` | drawn strikes / 6 | spacing of the wire mesh's term lines, counted over the drawn strikes |
| `height` | 4 (template: 5) | world height of the IV range |
| `extent` | `[12, 9]` | world width (strikes) and depth (expiries) |
| `low_color`, `mid_color`, `high_color` | viridis `#3b528b`, `#21918c`, `#fde725` | IV colour ramp (`surface` uses low and high) |
| `atm_color` | `#ffffff` | the at-the-money term-structure line |

Strikes and expiries are evenly spaced in sorted order. The nearest
expiry is at the back, so its skew stands up behind the flatter long
expiries. The grid and the IV scale span every snapshot
(`graphics-eas-domain-rows`), so the surface rises and falls in place
through the session instead of rescaling. The wire style is the default
because its mesh reads as a surface even in a 60x24 Braille render. A
solid heightfield shows only its silhouette there.

The golden (`test/golden/3d/vol-surface.{frame,txt}`, 60x24, dark, at the
end of the replay) is in `docs/3d.md`.
`test/financial-chart-3d-vol-surface-test.el` (run by `make test-3d`)
checks the rows, the mark, the replay, the golden, and that both templates
validate and draw in 2D without graphics.el.
