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

## Opening it

```elisp
(require 'financial-chart-3d)
;; 3D, from chain snapshots or rows; SPC plays the session
(financial-chart-3d-vol-surface-open CHAIN :title "SPY IV" :show t)
;; 2D, eas only
(eas-view-open "vol-surface" :bindings (list :data (financial-chart-3d-vol-surface-rows CHAIN)))
```

A caller such as invest-vol-surface can offer the 3D view when
`(financial-chart-3d-available-p)` is non-nil. It should keep its 2D
heatmap otherwise.

## The 3D mark: `fc-vol-surface3d`

Channels `x` (strike, default field `strike`), `z` (`dte`), and `y` (`iv`).
Options in `x-eas.graphics`:

| option | default | |
|---|---|---|
| `style` | `"wire"` | `"wire"`: each expiry's smile and every `strike_every`-th strike's term line as segments, each coloured by its IV; `"surface"`: one lit heightfield |
| `strike_every` | strikes / 6 | spacing of the wire mesh's term lines |
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
