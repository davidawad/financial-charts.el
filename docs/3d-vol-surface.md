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
| `contracts` | string, optional | the contracts at this strike and expiry, one line each (call, then put); set when a chain is given (below) |

Missing strike/expiry cells are fine. In 3D each expiry's smile is
interpolated across a missing strike (nearest value past the ends), and an
expiry with no IV at a quote time along each strike's term structure. In 2D
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
from the dotfiles repository. A live NVDA chain is 24 expiries x 125 strikes.
The solid surface uses every strike. The wire mesh draws at most
`max_strikes` (41) of them, evenly spread, so that it stays readable. The
ATM line is computed from every strike. The tests cover the fixture, a
synthetic 24 x 125 surface with nulls, and a 3-snapshot replay.

## The contracts behind it (`:chain`)

`financial-chart-3d-vol-surface-open`, the `opt.iv_surface` adapter
(`financial-chart-3d-bindings-from-metric ... :chain CHAIN`) and
`financial-chart-3d-vol-surface-rows` (as `:contracts`) take the option
chain the surface came from. Each sample's tooltip then names its
contracts. The chain is either of these:

- a list or vector of contracts, each a plist (or alist, or hash table)
  with Schwab's contract fields;
- a raw Schwab chain, `{"callExpDateMap": {"2026-12-01:32": {"768.0":
  [contract]}}, "putExpDateMap": {...}}`. A contract lacking `putCall`,
  `strikePrice`, `expirationDate` or `daysToExpiration` takes it from
  where it sits in the map.

| field | | |
|---|---|---|
| `putCall` | required | `CALL` or `PUT` (any case) |
| `strike` or `strikePrice` | required | number |
| `daysToExpiration` and/or `expirationDate` | one required | days, or an ISO 8601 date (its first 10 characters are used) |
| `symbol` | shown | the OCC symbol |
| `bid`, `ask`, `mark`, `last` | shown | prices |
| `volatility` | shown | IV in percent, as Schwab gives it. A value under 3 is taken as a fraction, and Schwab's `-999` as none |
| `delta`, `gamma`, `theta`, `vega` | `delta` shown | Greeks |
| `openInterest`, `totalVolume` | shown | |

A contract belongs to the row with its strike and days to expiry, or,
failing that, its strike and expiry date (the row's `expiry`, or its
`time` plus `dte`). A row without an `expiry` takes the contracts'.
`financial-chart-3d-vol-surface-contracts` parses a chain and signals
`financial-chart-invalid-data` with `:code` `bad_chain` (neither shape) or
`bad_contract` (`:index` the contract, `:field` `putCall`, `strike` or
`daysToExpiration`).

The fixture `test/fixtures/3d/vol-surface/schwab-chain.json` is a raw
Schwab chain for `iv_surface.json`'s cells, NVDA on 2026-10-30. The
11-day 768 strike has no contract, as it has no IV. It is synthetic.

### Tooltips

Each sample of the solid surface has a tooltip:

```
K 768  2026-12-01 (32 DTE)
IV 26.0%  moneyness 0.998
CALL NVDA  261201C00768000  bid 25.19 ask 25.49 mark 25.34  IV 26.0%  Δ 0.53  OI 1411  vol 323
PUT NVDA  261201P00768000  bid 23.55 ask 23.85 mark 23.70  IV 26.0%  Δ -0.47  OI 2096  vol 528
```

Without a chain the tooltip is the first two lines (no date when the rows
have no `expiry` and `time` is not a date). A sample with no quote says
where its IV came from:

```
K 768  2026-11-10 (11 DTE)
IV 20.0% (interpolated)  moneyness 0.998
interpolated from strike 769 (the nearest): no quote here
```

(or `between strikes 765 and 770`, or `between 7 and 14 DTE` / `from 7 DTE
(the nearest expiry)` for an expiry filled along the term structures).

The heightfield carries both `:tooltips` and `:data`, flat vectors in
row-major order like `:heights` (row j is the j-th nearest expiry,
column i the i-th strike, so sample (j, i) is index `j * strikes + i`).
The tooltip names the contracts for quoted cells and the interpolation
source for filled cells. The datum records strike, expiry, days to expiry,
IV, spot, and whether the sample was interpolated. graphics.el shows the
tooltip on hover and describes the full datum on `i` or `RET`.

## Opening it

```elisp
(require 'financial-chart-3d)
;; 3D, from chain snapshots or rows; SPC plays the session
(financial-chart-3d-vol-surface-open CHAIN :title "SPY IV" :show t)
;; 3D, from opt.iv_surface output (one result, or {time, surface} pairs)
(financial-chart-3d-vol-surface-open SURFACE-OR-SERIES :symbol "NVDA" :show t)
;; with the Schwab chain behind it: each sample's tooltip names its contracts
(financial-chart-3d-vol-surface-open SURFACE :chain SCHWAB-CHAIN :symbol "NVDA" :show t)
;; 2D, eas only
(eas-view-open "vol-surface" :bindings (list :data (financial-chart-3d-vol-surface-rows CHAIN)))
```

`financial-chart-3d-vol-surface-open (SURFACE-OR-SERIES &key chain symbol
title time id show)` returns the view. `chain` is the contracts behind it
(above). `symbol` titles it "SYMBOL implied
volatility", and `title` overrides that. `time` is the time of a single
`opt.iv_surface` result.

`financial-chart-3d-bindings-from-metric` routes the metric like every
other (README, "From invest metrics"):

```elisp
(financial-chart-3d-bindings-from-metric "vol-surface" "opt.iv_surface" SURFACE :symbol "NVDA"
                                         :chain SCHWAB-CHAIN)
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
| `style` | `"surface"` | `"surface"`: one solid lit heightfield on every strike, with a light grid of smiles and term lines drawn on it and a tooltip per sample; `"wire"`: each expiry's smile and every `strike_every`-th strike's term line as segments, each coloured by its IV |
| `grid_expiries` | 5 | smiles the solid surface's grid draws, evenly spread over the expiries (first and last included) |
| `grid_strikes` | 7 | term lines the grid draws, evenly spread over the strikes |
| `grid_color` | `#c4ccd4` | the grid's colour |
| `max_strikes` | 41 | most strikes the wire mesh draws, evenly spread (the ATM line uses all of them) |
| `strike_every` | drawn strikes / 6 | spacing of the wire mesh's term lines, counted over the drawn strikes |
| `height` | 4 (template: 5) | world height of the IV range |
| `extent` | `[12, 9]` | world width (strikes) and depth (expiries) |
| `low_color`, `mid_color`, `high_color` | viridis `#3b528b`, `#21918c`, `#fde725` | IV colour ramp (the heightfield's gradient runs low to high: the core ramps two colours) |
| `atm_color` | `#ffffff` | the at-the-money term-structure line |

Strikes and expiries are evenly spaced in sorted order. The nearest
expiry is at the back, so its skew stands up behind the flatter long
expiries. The grid and the IV scale span every snapshot
(`graphics-eas-domain-rows`), so the surface rises and falls in place
through the session instead of rescaling. The solid surface is the
default: a wire mesh leaves gaps between its lines, and the surface
should read as a surface. The grid and the ATM line sit on the surface's
own vertices, lifted a hair (segments win depth ties, graphics.el's
scene.md). In Braille every cell of the surface is lit, so they show as
lighter cells in the cell colour, not as dots.

The golden (`test/golden/3d/vol-surface.{frame,txt}`, 60x24, dark, at the
end of the replay) is in `docs/3d.md`.
`test/financial-chart-3d-vol-surface-test.el` (run by `make test-3d`)
checks the rows, the mark, the contracts and tooltips, the replay, the
golden, that the default leaves no empty cell inside the surface's
footprint at the framing sizes and phases, and that both templates
validate and draw in 2D without graphics.el.
