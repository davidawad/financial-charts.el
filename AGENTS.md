# AGENTS.md — financial-chart.el

Standalone, publishable Emacs package: every financial chart kind
(candles, area, line, sparkline, payoff, bars, ...) as text or SVG from
data the caller supplies, validated strictly and drawn by eas templates.
It never fetches data. README.md is the full reference.

`eas`, the interactive chart engine, is its own package, eas.el (default
checkout `../../Personal/emacs/eas.el`; `EAS=` overrides), which this
package requires. Its design, README and AGENTS.md live there;
`docs/design/engine.md` points to them. Here: the financial adapters,
transforms and templates on eas (epic `fc-qx1`). Tasks: `br ready` in this
repo (`.beads/`).

## Driving it

Ask the package; don't read source to learn its state.

### eas charts: one verb set, one envelope

Every verb answers `{"contract":"chart/v1","ok","data","reason"?,
"evidence"?,"next":[...]}`; a failure names its reason code (design
section 5) and evidence (`path`, `index`, `field`), and `next` holds
runnable commands. Same verbs, three doors:
`(eas-agent "render" "line" :data B :backend "text")` after
`(require 'eas-agent)`; eas.el's `bin/eas render line --data b.json` (batch,
stateless verbs; `--raw` prints only the data; exit 1 on failure); and
`emacsclient --eval '(eas-agent-json "inspect" "line:daily")' | jq -r .`
for live views.

1. eas.el's `bin/eas describe` (or `describe verbs|templates|events|reasons`)
   and pick a template, or `check` a hand-written Vega-Lite spec.
2. `bin/eas example line --raw > b.json` gives bindings that render
   as-is. Edit them, then `bin/eas check line --data b.json`.
3. `bin/eas render line --data b.json --raw` to see it (text is
   deterministic). `explain ... --stage resolve|compile|scene` only
   when something looks wrong; `bench` for latency.
4. To show the human: `open line --data b.json --show` in their Emacs,
   then `inspect`, `log` and `selection` say what they are looking at
   and picked; `dispatch VIEW EVENT` drives the view with event/v1
   (an array replays a log). `link VIEW BUS` shares hover, brush and
   zoom across views (`buses` lists them; `eas-link-demo` shows two
   tickers on the `panes` template).
5. For a deliverable: `export ... --vl` is pure Vega-Lite for
   `bin/chart build`. `doctor` checks the install. bin/chart is never
   a runtime dependency: it is the conformance oracle (dev/CI; refs
   are committed) and the static export door. A spec outside the
   native subset shows `UNSUPPORTED_FEATURE` text unless
   `eas-static-fallback` is t.
6. In org: `#+begin_src eas :template ohlc :data tbl` (load
   `ob-eas`) opens view `ohlc:BLOCKNAME` inline; `:as text|vl` or
   `:results file :file x.svg|x.vl.json|x.png` for documents.
   eas.el's `examples/eas.org` shows each.

### financial-chart kinds

1. Discover: `(financial-chart-describe)` — kinds (with their eas
   template), shapes, cohorts, indicators, entry points by verb. From a
   shell, the eas door with this package's templates loaded:
   `fc-eas describe` (README, "Charts from the shell").
2. Learn a kind's input: `(financial-chart-describe-kind 'payoff)` or
   `fc-eas example payoff --raw` (bindings `render` accepts as-is).
3. Check data: `(financial-chart-check KIND DATA)` answers t or
   `(:code :index :field :message)` naming the bad row and field;
   `financial-chart-validate` signals the same as
   `financial-chart-invalid-data`.
4. Plan: `(financial-chart-explain KIND DATA &rest PROPS)` gives the
   template, renderer and args `financial-chart-plot` will use and why
   that backend. It draws nothing.
5. Render: `financial-chart-plot` / `-plot-spec`. Prefer `:backend 'text`
   to read a chart yourself; the text renderers are deterministic.
6. Health: `(financial-chart-doctor-checks)` — eager rows
   `(:name :status pass|fail|skip :detail :remediation)`.

## Changing it

- `make test` (offline, no display, under a minute) and `make compile`
  (warnings are errors) must pass; both need eas.el (see top). Engine
  gallery, conformance and bench runs belong to eas.el. Template goldens
  (test/golden/eas-templates/): `EAS_UPDATE_GOLDEN=1 make test`, then
  review the diff.
- Rendering is eas's. There is no native renderer: a new chart kind is
  an eas template (financial ones in `templates/`, generic ones in
  eas.el) plus one `financial-chart-kinds` entry naming its shape and
  template (or `financial-chart-register-kind`). The doctor and
  `describe` pick it up; add a parity check
  (`financial-chart-eas-parity-checks`) and a template golden.
- New data shape: one `financial-chart-shapes` entry with a validator
  that signals `financial-chart-invalid-data` with `:code`, `:index`
  and `:field` (`financial-chart--invalid`).
- Errors: `define-error` under `financial-chart-error`, data
  `(MESSAGE :code CODE ...)`, message says how to fix it. Never
  message-and-return-nil.
- Data sources stay out: no fetching, no ticker lookups, no
  market-data or broker integration. Callers supply the data; adapters
  from a broker's format to bar/v1 belong to the caller.
- Zero references to the owner's personal configuration. This repo is canonical; the
  configuration loads it via `load-path` and carry no copy.
- Authorized: david, swe.
