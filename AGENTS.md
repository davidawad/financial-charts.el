# AGENTS.md — financial-chart.el

Standalone, publishable Emacs package: pure-elisp OHLC candlestick
chart renderer, in-buffer (unicode block/box-drawing glyphs, no
gnuplot/image dependency). Data-source agnostic core; a soft-wired
(fboundp-checked, not `require`d) bridge into schwab-broker.el.

## For agents

- Read `README.md` first; it is the complete function reference.
- Tests: ERT in `test/financial-chart-test.el`, pure logic (no
  network, no display) — run offline.
- Keep the core renderer (`financial-chart-render`/`-view`) data-source
  agnostic. Bridges into a specific broker package (schwab-broker.el,
  and any future ones) go at the bottom of the file, soft-wired via
  `fboundp`, never a hard `require` of the broker package.
- Zero references to the owner's personal configuration are allowed here — the repo
  must remain publishable as-is. THIS repo is canonical; the owner's
  Emacs configuration imports it via load-path and carries no copy of
  the source. All changes land here.
- Authorized: david, swe.
