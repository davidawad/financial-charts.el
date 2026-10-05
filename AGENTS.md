# AGENTS.md — financial-chart.el

Standalone, publishable Emacs package: every financial chart kind
(candles, area, line, sparkline, payoff, bars) as text or SVG from plain
Lisp data. README.md is the full reference.

In progress: `easel`, the interactive chart engine growing in
`src/easel/` (epic `fc-qx1`). Design and layer contracts:
`docs/design/engine.md`. Tasks: `br ready` in this repo (`.beads/`).

## Driving it

Ask the package; don't read source to learn its state.

### easel charts: one verb set, one envelope

Every verb answers `{"contract":"chart/v1","ok","data","reason"?,
"evidence"?,"next":[...]}`; a failure names its reason code (design
section 5) and evidence (`path`, `index`, `field`), and `next` holds
runnable commands. Same verbs, three doors:
`(easel-agent "render" "line" :data B :backend "text")` after
`(require 'easel-agent)`; `bin/easel render line --data b.json` (batch,
stateless verbs; `--raw` prints only the data; exit 1 on failure); and
`emacsclient --eval '(easel-agent-json "inspect" "line:daily")' | jq -r .`
for live views.

1. `bin/easel describe` (or `describe verbs|templates|events|reasons`)
   and pick a template, or `check` a hand-written Vega-Lite spec.
2. `bin/easel example line --raw > b.json` gives bindings that render
   as-is. Edit them, then `bin/easel check line --data b.json`.
3. `bin/easel render line --data b.json --raw` to see it (text is
   deterministic). `explain ... --stage resolve|compile|scene` only
   when something looks wrong; `bench` for latency.
4. To show the human: `open line --data b.json --show` in their Emacs,
   then `inspect`, `log` and `selection` say what they are looking at
   and picked; `dispatch VIEW EVENT` drives the view with event/v1
   (an array replays a log).
5. For a deliverable: `export ... --vl` is pure Vega-Lite for
   `bin/chart build`. `doctor` checks the install.

### financial-chart kinds

1. Discover: `(financial-chart-describe)` — kinds, shapes, cohorts,
   presets, entry points by verb, whether market-data is loaded. From a
   shell: `bin/financial-chart describe`.
2. Learn a kind's input: `(financial-chart-describe-kind 'payoff)` or
   `bin/financial-chart example payoff` (a spec `render` accepts as-is).
3. Check data: `(financial-chart-validate KIND DATA)`; a failure names
   the bad element's `:index`.
4. Plan: `(financial-chart-explain KIND DATA &rest PROPS)` gives the exact
   renderer and args `financial-chart-plot` will use and why that
   backend. For tickers: `financial-chart-explain-symbol`,
   `financial-chart-resolve-preset`. None of these draw or fetch.
5. Render: `financial-chart-plot` / `-plot-spec`. Prefer `:backend 'text`
   to read a chart yourself; the text renderers are deterministic.
6. Health: `(financial-chart-doctor-checks)` — eager rows
   `(:name :status pass|fail|skip :detail :remediation)`.

## Changing it

- `make test` (offline, no display) and `make compile` (warnings are
  errors) must pass. Golden fixtures: regenerate with
  `FINANCIAL_CHART_UPDATE_GOLDEN=1 make test` and review the diff.
- New chart kind: renderers in -text/-svg, then one
  `financial-chart-kinds` entry (or `financial-chart-register-kind`).
  The doctor and `describe` pick it up; add a golden fixture.
- New data shape: one `financial-chart-shapes` entry with a validator
  that signals `financial-chart-invalid-data` with `:code` and `:index`.
- Errors: `define-error` under `financial-chart-error`, data
  `(MESSAGE :code CODE ...)`, message says how to fix it. Never
  message-and-return-nil.
- Data sources stay out. market-data.el is soft (`fboundp`, never
  `require`); broker- or owner-specific adapters belong to the caller.
- Zero references to the owner's personal configuration. This repo is canonical; the
  configuration loads it via `load-path` and carry no copy.
- Authorized: david, swe.
