# financial-chart.el

A pure-Elisp OHLC candlestick chart renderer, built entirely on Emacs's
own display engine — no gnuplot, no image/PNG generation, no external
process. Renders directly into a buffer using unicode box-drawing and
block characters at half-block vertical resolution, with `propertize`
faces for up/down coloring. Requires Emacs 27.1+ (for `propertize`
face support on unicode glyphs; no other version-specific features).

Data-source agnostic: the renderer takes a plain list of
`(:open :high :low :close &optional :volume :time)` plists, oldest bar
first. It doesn't know or care where those bars came from. `:volume`
and `:time` are both optional and independently gate an optional
volume panel and X-axis — omit either (or both) and the chart degrades
gracefully rather than erroring or drawing an empty panel.

Every visual and behavioral aspect of the chart is a `defcustom` — see
**Configuration** below. Nothing is hardcoded into the rendering
functions; override anything for one call by `let`-binding the
relevant variable(s) around it.

## Install

Copy `financial-chart.el` somewhere on your `load-path`, then:

```elisp
(require 'financial-chart)
```

## Use

```elisp
(financial-chart-view
 '((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
   (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000)
   (:open 101.5 :high 102 :low 96 :close 97 :volume 21000 :time 1700172800000))
 "MY SYMBOL")
```

pops a `*financial-chart*` buffer with a candlestick render (volume
panel and X-axis included automatically, since these bars carry
`:volume`/`:time`). Call `financial-chart-render` directly instead if
you want the chart as a plain string (e.g. to embed elsewhere, such as
in an org-mode `#+begin_example` block) rather than a popped buffer.

## Configuration

All `financial-chart-*` custom variables (`M-x customize-group
financial-chart`):

**Size** — `financial-chart-height` (price panel rows, default 20),
`financial-chart-max-bars` (window to the most recent N bars, default
80; nil/0 = unlimited — this is what keeps a 1000-bar pull from
rendering as 1000 unreadable columns), `financial-chart-candle-width`
(columns per candle, default 1), `financial-chart-candle-gap` (blank
columns between candles, default 1).

**Colors** — `financial-chart-up-face`/`-down-face` (default
`success`/`error`), `financial-chart-wick-face` (default nil = reuse
the candle's own face), `financial-chart-axis-face` (default nil =
default face).

**Glyphs** — `financial-chart-glyph-full-block`/`-upper-half`/
`-lower-half`/`-wick`/`-empty`/`-indicator`/`-volume-bar` (all
characters, defaulting to unicode block/box-drawing glyphs — override
any of these for an ASCII-only terminal).

**Price scale** — `financial-chart-scale` (`linear` or `log`, default
`linear`), `financial-chart-axis-label-count` (default 3),
`financial-chart-axis-format` (default `"%7.2f "`).

**Volume panel** — `financial-chart-show-volume` (default t; only
actually renders when at least one bar has `:volume`),
`financial-chart-volume-height` (default 5),
`financial-chart-volume-up-face`/`-down-face` (default nil = reuse the
price panel's up/down faces), `financial-chart-volume-axis-label-count`
(default 2), `financial-chart-volume-axis-format` (default `"%7.0f "`).

**X-axis** — `financial-chart-show-x-axis` (default t; only actually
renders when at least one bar has `:time`),
`financial-chart-x-axis-label-count` (default 4),
`financial-chart-x-axis-format` (a `format-time-string` string, default
`"%m/%d"`).

**Overlay indicators** — `financial-chart-indicators`, a list of plists
`(:fn FN :face FACE :glyph CHAR)`. `FN` takes the (already-windowed)
bars list and returns a same-length list of numbers-or-nil on the same
price scale as the candles; each non-nil value is drawn at its row.
Empty by default (no overlays drawn unless you configure one):

```elisp
(defun my-sma (bars &optional window)
  (let ((window (or window 20)) (closes (mapcar (lambda (b) (plist-get b :close)) bars)))
    (cl-loop for i from 0 below (length closes)
             collect (if (< i (1- window)) nil
                       (/ (apply #'+ (cl-subseq closes (- i window -1) (1+ i))) (float window))))))

(setq financial-chart-indicators
      (list (list :fn (lambda (bars) (my-sma bars 20))
                  :face 'font-lock-keyword-face :glyph ?•)))
```

## Schwab bridge

If [schwab-broker.el](https://github.com/davidawad/schwab-broker.el)
is loaded, `financial-chart-schwab-view` fetches real price history and
renders it directly:

```elisp
(financial-chart-schwab-view
 "AAPL" :period-type "day" :period 5 :frequency-type "minute" :frequency 5)
```

`M-x financial-chart-schwab-view` (just a symbol prompt) uses
`financial-chart-schwab-default-period-type`/`-period`/
`-frequency-type`/`-frequency` (default: 1 month of daily bars) for
whichever keys you don't supply — each explicit key you pass overrides
only that one default, the rest still apply.

This is a soft dependency (checked via `fboundp` at call time, not
`require`d) — `financial-chart.el` never needs schwab-broker.el loaded
to do anything else.

## Tests

ERT tests live in `test/financial-chart-test.el`, pure logic (no
network, no Emacs display needed). Run standalone:

```sh
emacs -Q --batch -L . -l financial-chart.el -l test/financial-chart-test.el \
  -f ert-run-tests-batch-and-exit
```

## License

MIT — see [LICENSE](LICENSE).
