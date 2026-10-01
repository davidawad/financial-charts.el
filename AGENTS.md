# AGENTS.md — financial-chart.el

Standalone, publishable Emacs package: every financial chart kind
(candles, area, line, sparkline, payoff, bars) as text or SVG from plain
Lisp data. README.md is the full reference.

## Driving it

Ask the package; don't read source to learn its state.

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
   `(:layer "L2" :name :status pass|fail|skip :detail :remediation)`.

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
