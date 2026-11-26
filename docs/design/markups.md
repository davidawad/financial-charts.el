# Markups: series, events, marks, presets, evidence

Design for epic `fc-gty` (TradingView-parity markups). It covers
indicators, automatic support/resistance, candlestick patterns, drawing
tools and presets. The reader it is written for is an agent driving the
package: the goal is that an agent can learn what the package can do,
say what it wants, read what a chart shows and know whether to believe
it. It should do all of that in a handful of calls and a few hundred
tokens, without reading source or pixels.

## The problem with the current shape

Markups today are three unrelated mechanisms:

- `studies` and `series` in compose JSON, computed by the indicator
  registry.
- `annotations`, where detection and drawing are one function. The
  Fibonacci swing is found inside `financial-chart-annotation--fibonacci`
  and exists only as pixels.
- Cohorts, an elisp defcustom of `:fn` symbols used by the old API and
  unreachable from compose JSON. `docs/architecture/financial-tower.md`
  in the dotfiles names an L5 preset layer and
  `financial-chart-resolve-preset`. Neither exists in this package.

An agent cannot ask "where are the support levels" or "which patterns
fired in the last 10 bars" without rendering a chart and reading it
back. It cannot tell whether a mark means anything. And it cannot add a
detector without also writing its validation, docs, example and tests
by hand.

## The tower

Each layer is a registry of named entries. Each has a pure verb that
returns data in the `chart/v1` envelope. Each depends only on the layers
below it.

| Layer | Entry | Pure verb | Output shape |
|---|---|---|---|
| M0 bars | bar/v1 (market-data) | `check` | bars plus `bars-id`, a content hash of the canonical bars |
| M1 series | indicator (`sma`, `rsi`, `vwap`, ...) | `series` | one value or null per bar, plus `warmup` |
| M2 events | detector (`pattern.hammer`, `level.auto-sr`, `swing.zigzag`, `fib.retracement`) | `detect` | event/v1 list |
| M3 marks | mark (`marker`, `level`, `band`, `line`, `label`, `box`) | (inside `compose`) | eas layers |
| M4 presets | preset (`swing-trader`, `trend`, `reversal`) | `expand` | a full compose description with provenance |
| M5 chart | chart/v1 compose (exists) | `compose`, `explain` | eas spec |
| M6 view | eas render, live view (exists) | `render`, `summarize`, `inspect` | text, svg, scene digest |
| E evidence | evidence/v1 record per (entry, bars set) | `evaluate` (writes one file) | forward-return statistics |

The key cut is between M2 and M3. **Detection is data and drawing is a
view of it.** Every automatic markup (S/R, patterns, swings, Fibonacci
anchors, automatic trendlines, zigzag, pitchfork pivots) is an M2
detector that returns events. M3 only says how an event or series looks.
A hand-placed `level` or `trendline` is a constant event, an event the
user wrote instead of one a detector found. So both paths meet at the
same drawing code and the same readout.

## event/v1

```json
{"id": "pattern.bullish-engulfing@2026-05-26",
 "detector": "pattern.bullish-engulfing", "params": {},
 "index": 81, "time": "2026-05-26",
 "confirmed_index": 81, "confirmed_time": "2026-05-26",
 "direction": "bullish", "strength": 0.74,
 "geometry": {"at": "2026-05-26", "y": 111.14},
 "evidence": {"body_ratio": 0.68, "engulf_margin": 0.0031,
              "prior_slope_pct": -0.41, "prior_window": 10}}
```

- `id` is deterministic (detector@time, with an index suffix if a
  detector fires twice on one bar). It is stable across calls, across
  presets and in the live view. That lets an agent and a human talk
  about the same object.
- `confirmed_index` is the first bar on which the event is knowable.
  For a swing high with 5 right-hand bars it is index + 5. This is the
  look-ahead contract: the event depends only on bars at or before
  `confirmed_index`.
- `strength` is in [0, 1]. Its meaning is documented per detector
  (touch count for a level, closeness to the definition's thresholds for
  a pattern). Marks may size or fade by it.
- `geometry` is one of `{at, y}`, `{y, from, to}` (a level),
  `{points: [[t, y], ...]}` (a line, zigzag or pitchfork) or
  `{levels: [{ratio, y}], from, to}` (Fibonacci).
- `evidence` holds the predicate values that made the event fire. It is
  the answer to "why is this here", in the readout and in `detect`.

## Registry entries

An indicator or detector is one file in `src/indicators/` or
`src/detectors/`, registered with metadata. Everything else comes from
that metadata.

```elisp
(financial-chart-register-detector
 'pattern.hammer #'financial-chart-detect-hammer
 :kind 'pattern :direction 'bullish
 :params '((:name trend-window :type integer :default 10 :min 3)
           (:name max-body :type number :default 0.35 :min 0 :max 1))
 :lag 0                                  ; confirmation bars after the event bar
 :needs '(open high low close)
 :definition "Nison 1991: small real body at the top of the range,
lower shadow >= 2x body, little or no upper shadow, after a decline."
 :oracle "talib:CDLHAMMER"
 :mark '(:type marker :below t :label "H")
 :example "examples/detectors/hammer.json")
```

What the harness derives from one entry, with no extra code:

1. `describe markups`: name, kind, params with types, defaults and
   ranges, lag, needs, definition, oracle, latest evidence and its age.
2. Validation of compose JSON params, with `UNKNOWN_DETECTOR` and
   `INVALID_PARAM` plus a did-you-mean over registry names.
3. Generic tests for every entry:
   - truncation: running on bars[0..k] reproduces exactly the events
     with `confirmed_index <= k` from the full run;
   - invariance: scaling all prices by c > 0 and shifting by a constant
     leaves events unchanged (geometry transformed, ids equal);
   - determinism: two runs produce byte-equal JSON;
   - example: the example renders, and its text golden matches.
4. Oracle tests: when `:oracle` is set, events must match the
   committed oracle goldens. The goldens come from TA-Lib, generated by
   `test/oracles/talib-goldens` from the same fixture bars; generation
   is a dev step and the tests read committed files. Each disagreement
   is either fixed or listed with a reason in the entry's `:deviations`.
5. A lint that fails the build if an entry lacks `:definition`, `:lag`,
   `:params` types or `:example`.

So adding a detector is one file and one citation, and the entry gets
its validation, docs, tests and catalog row automatically.

## Presets

A preset is a compose fragment in JSON with metadata. Presets come from
three places: the package's `presets/` directory, the directories in
`financial-chart-preset-directories` (owner data, for example the
dotfiles), and an inline `"presets": {NAME: FRAGMENT}` in a chart.

```json
{"preset/v1": "swing-trader",
 "doc": "Short-swing read: SMA 7/20/50, automatic S/R, recent candle
patterns, Fibonacci of the last swing, RSI pane.",
 "price": {"series": [{"indicator": "sma", "params": [7]},
                      {"indicator": "sma", "params": [20]},
                      {"indicator": "sma", "params": [50]}],
           "annotations": [{"type": "support-resistance", "max": 4},
                           {"type": "patterns", "since": 20},
                           {"type": "fibonacci", "window": 60}]},
 "panes": [{"study": "rsi"}]}
```

A chart uses one with `"preset": "swing-trader"` or
`["trend", "swing-trader"]`. The merge laws:

1. Lists (`series`, `studies`, `annotations`, `panes`) are appended in
   preset order, with the chart's own entries last. An entry whose id
   already appears is replaced by the later one, not duplicated.
2. Scalars (`style`, `title`, `colors.*`): the chart wins, then the
   later preset.
3. A preset may name presets. A cycle is `PRESET_CYCLE`, and an unknown
   name is `UNKNOWN_PRESET` at its JSON path.
4. `expand` returns the fully merged description. Every element carries
   `"from": "preset:swing-trader"` or `"from": "chart"`. It is pure and
   cheap, and it is the first thing to call when a chart looks wrong.

Cohorts become presets. Each `financial-chart-indicator-cohorts` entry
is converted to `presets/<name>.json`. `financial-chart-resolve-cohort`
stays as a thin reader over the preset that it replaces, and the
defcustom is marked obsolete. That leaves one concept for "named bundle
of markups".

## Reading a chart without looking at it

`summarize` takes a chart description, or an open view's name, and
returns a scene digest of about 200 tokens. It is the cheapest
accurate way for an agent to know what a chart shows:

```json
{"bars-id": "b3f1...", "last": {"time": "2026-06-19", "close": 134.76},
 "levels": [{"id": "level.auto-sr@127.3", "y": 127.3, "side": "below",
             "distance_pct": -5.5, "touches": 4},
            {"id": "level.auto-sr@106.2", "y": 106.2, "side": "below",
             "distance_pct": -21.2, "touches": 5}],
 "fib": {"nearest": {"ratio": 0.236, "y": 131.2}, "swing": ["2026-05-07", "2026-06-19"]},
 "recent_events": [{"id": "pattern.bullish-engulfing@2026-05-26", "bars_ago": 18}],
 "series_state": {"sma-7": {"value": 133.4, "slope": "up"},
                  "rsi-14": {"value": 71.8, "state": "overbought"}},
 "evidence": {"pattern.bullish-engulfing": {"status": "untested"}},
 "text": "Close 134.76 above every S/R level; nearest support 127.3
(4 touches, -5.5%). RSI 71.8 overbought. Last pattern: bullish
engulfing 18 bars ago (untested on this ticker)."}
```

Each series and detector entry may declare a `:state` function (RSI to
`overbought` or `oversold`, MA slope to `up`, `down` or `flat`), so the
digest grows with the registry. An agent reads `summarize`. It reads
the text render only to check layout, and SVG only for a human
deliverable.

## Evidence: is a mark worth drawing

`evaluate` runs a detector or preset over a bars set (tickers × range ×
interval) and writes one evidence/v1 record. Each record is its own
content-addressed file under `financial-chart-evidence-directory`
(owner data, outside the package). There is no shared ledger file, so
runs in parallel never contend.

```json
{"evidence/v1": "pattern.hammer", "params": {}, "code_version": "<sha>",
 "bars_ids": ["..."], "universe": ["TSM", "NVDA"], "range": ["2018-01-01", "2026-06-30"],
 "split": {"in_sample_end": "2023-12-31"},
 "n": 412,
 "forward": {"1": {"mean": 0.0011, "baseline_mean": 0.0004, "hit": 0.53},
             "5": {...}, "20": {...}},
 "costs_bps": 5, "baseline": "random bars, same count, same tickers, 1000 draws",
 "reality_check_p": 0.31, "reference": "Bulkowski 2008 hammer stats",
 "verdict": "noise"}
```

The rules:

- Events enter at `confirmed_index + 1`'s open, never at the event bar.
- Prices are adjusted point-in-time from unadjusted bars plus
  corporate actions.
- The out-of-sample split is fixed before the run.
- p-values are corrected across every entry evaluated on the same
  universe (White's Reality Check or Hansen's SPA).
- `verdict` is `edge`, `noise` or `untested`. `describe` and
  `summarize` show it next to each detector, and a preset member whose
  verdict is `noise` is drawn faded unless the preset sets
  `"show_noise": true`.

The package owns the verb and the schema. Bars, universes and records
are owner data (`financial-chart-evidence-directory`, set in the
dotfiles to a directory under the invest data root).

## Verbs, one envelope

Every verb answers `{"contract":"chart/v1","ok","data","reason"?,
"evidence"?,"next":[...],"cost":{"ms","bars","events"}}` through the
three doors (elisp, `bin/eas` with this package's templates,
`emacsclient` for live views).

| Verb | Pure | Typical use |
|---|---|---|
| `describe markups` | yes | what exists, with params, lag, definition, verdict |
| `check` | yes | validate a description before computing anything |
| `expand` | yes | presets merged, with provenance |
| `detect` | yes | events for one detector or all of a description's |
| `series` | yes | values for one indicator |
| `summarize` | yes | scene digest |
| `compose` / `explain` | yes | eas spec, staged plan |
| `render` | yes | text (deterministic) or svg |
| `evaluate` | writes one file | evidence record |
| `inspect`, `selection` (live) | read | event ids and evidence under the crosshair or brush |
| `dispatch VIEW focus ID` (live) | acts on view | scroll the human's view to an event |

`next` carries runnable follow-ups. A `detect` with an untested
detector suggests the `evaluate` call. A failed `check` suggests the
`expand` call.

Cost control:

- `detect` and `series` take `since` (bars or a time) and `limit`.
- Results are memoized on (bars-id, entry, params, code version), so a
  board redraw recomputes nothing unchanged.
- `cost` is reported on every answer, so an agent can budget.

## Live view closure

In the human's Emacs, hovering a mark shows its `id`, `strength`,
`evidence` and `verdict` in the readout line. `inspect` and `selection`
return the same ids. An agent asked "what is that triangle" reads
`selection`. It does not have to guess from coordinates.

## Doctor rows

`financial-chart-doctor-checks` gains these rows:

- registry lint (missing `:definition`, `:example`, `:lag`);
- oracle goldens present and matching;
- presets parse and expand without cycles;
- evidence directory readable;
- detectors whose newest evidence is older than the code version or 90
  days (`stale`).

## Build order

Each step is usable on its own:

1. event/v1, the detector registry and `detect`. Port the Fibonacci
   swing out of the annotation into `fib.retracement` and `swing.pivot`.
   Make `level`, `trendline` and `box` constant events.
2. The generic harness: lint, truncation, invariance, determinism and
   example tests keyed off the registry.
3. Detectors: `level.auto-sr` (`fc-gty.1`), the candle patterns
   (`fc-gty.2`, with the TA-Lib oracle), and the drawing tools
   (`fc-gty.3`, starting with zigzag, Fibonacci extension, anchored
   VWAP and automatic trendlines).
4. Presets with `expand` and provenance, and the cohort migration
   (`fc-gty.4`).
5. `summarize`, with `:state` functions on RSI, the MAs, MACD and ADX.
6. `evaluate` and evidence/v1, and verdicts shown in `describe`,
   `summarize` and the readout.
7. Live readout ids, `dispatch focus`, and the doctor rows.
