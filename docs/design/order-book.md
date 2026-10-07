# Live order books (fc-gbo.4)

The caller supplies one snapshot and then a stream of deltas. Data
sources stay out of this package. `financial-chart-eas-book` keeps the
book, and eas draws it with the `ladder` and `depth-live` templates
(`templates/financial/`).

## Flow

1. `financial-chart-book-open SNAPSHOT :template :levels :tick :flash :max-fps`
   validates the snapshot (`INVALID_BOOK` names `/bids/3`; `CROSSED_BOOK`),
   opens an eas view and attaches an eas stream.
2. `financial-chart-book-push VIEW DELTAS` applies one batch to a copy of
   the book. If any delta fails, the batch signals and the book is
   unchanged. Codes: `INVALID_DELTA` (path `/i/op|side|price|size`),
   `DUPLICATE_LEVEL`, `UNKNOWN_LEVEL` (the feed is out of sync, so
   resend the book with `financial-chart-book-reset`) and `CROSSED_BOOK`.
   Each changed level is stamped with the stream clock so it can flash.
3. The book becomes rows (`financial-chart-book-rows`): the nearest
   LEVELS asks (high to low), one `mid` row, then the nearest LEVELS bids.
   Every row has `{side, price, price_label, size, cumulative, level,
   changed, mid, spread, label}`. All rows share one schema, so eas's
   push schema check holds. The mid row's label reads `mid M  spread S`.
   `price_label` is the price at the book's tick precision (the `:tick`
   option, else the smallest price step; `financial-chart-book-tick`),
   recomputed every frame, so a finer price from a delta refines the
   labels. The ladder's price axis is `price_label`, sorted by `price`;
   mid and spread are rounded to the same precision (the mid one place
   finer when it falls between ticks).
4. The rows go out through `eas-push`. eas-stream applies the frame cap
   and holds frames while a drag is in progress or for
   `eas-stream-hover-hold` seconds after a pointer event.

## Replacing the book with an append-only push

eas's push appends rows and its stream keeps the last `window` rows.
Before each push the stream is re-attached with `window` set to that
frame's row count. When queued pushes are concatenated, the last N rows
are therefore exactly the newest snapshot. No delta history accumulates
in the view.

While a frame is queued (the cap or a hover holds it), new deltas only
mark the book dirty and are not queued as more snapshots. A hook on
`eas-view-dispatch-functions` offers the dirty book as soon as the
queued frame lands. At most one snapshot is ever pending, and the
frame after a pause always shows the latest book.

When a level's flash lapses, a timer pushes one more frame so the
highlight clears.

This replaces the book through the stream window rather than a keyed
push, because eas has no keyed row replacement. That primitive is filed
against eas.el as **eas-kbh** ("Keyed row replacement (upsert/delete by
key) in push and eas-stream"). With it, a frame would carry only the
changed levels plus deletes, the log would hold deltas instead of
books, and the window trick could go.

## Frame cost

`financial-chart-eas-book-bench.el` measures 20 frames of 10 size
updates each, after one warm-up frame. Each frame is split into:

- **apply**: the deltas applied to the book.
- **push**: rows built and pushed, then the scene recompiled by eas.
- **draw**: the scene drawn to an SVG string or a text grid.

The SVG size is 640 by max(360, 6 x rows) pixels. Text is 100x40 cells.
Machine: GNU Emacs 30.1, AMD EPYC 9R14, one core, batch.

### Byte-compiled (eas.el and financial-chart `.elc`)

| levels/side | rows | backend | template | apply ms | push ms | draw ms | frame ms | worst ms | max fps |
|---|---|---|---|---|---|---|---|---|---|
| 50 | 101 | svg | ladder | 0.0 | 14.6 | 4.5 | 19.1 | 27.2 | 52 |
| 50 | 101 | svg | depth-live | 0.0 | 7.5 | 2.2 | 9.7 | 17.9 | 103 |
| 50 | 101 | text | ladder | 0.0 | 8.7 | 4.2 | 12.9 | 21.7 | 77 |
| 50 | 101 | text | depth-live | 0.0 | 5.2 | 14.2 | 19.4 | 24.9 | 51 |
| 100 | 201 | svg | ladder | 0.0 | 28.4 | 6.7 | 35.1 | 41.9 | 28 |
| 100 | 201 | svg | depth-live | 0.0 | 11.6 | 3.2 | 14.8 | 22.9 | 67 |
| 100 | 201 | text | ladder | 0.0 | 16.8 | 5.7 | 22.5 | 30.9 | 44 |
| 100 | 201 | text | depth-live | 0.0 | 8.9 | 14.4 | 23.3 | 31.2 | 42 |
| 200 | 401 | svg | ladder | 0.0 | 55.4 | 17.4 | 72.9 | 77.6 | 13 |
| 200 | 401 | svg | depth-live | 0.0 | 18.1 | 7.9 | 26.0 | 34.4 | 38 |
| 200 | 401 | text | ladder | 0.0 | 33.5 | 8.1 | 41.6 | 50.4 | 24 |
| 200 | 401 | text | depth-live | 0.0 | 15.6 | 14.4 | 30.1 | 36.1 | 33 |

### Interpreted (`.el` sources, as `make test` loads them)

| levels/side | backend | template | frame ms | max fps |
|---|---|---|---|---|
| 50 | svg | ladder | 89.4 | 11 |
| 50 | svg | depth-live | 52.9 | 18 |
| 50 | text | ladder | 84.6 | 11 |
| 50 | text | depth-live | 148.2 | 6 |
| 100 | svg | ladder | 161.9 | 6 |
| 100 | svg | depth-live | 77.3 | 12 |
| 100 | text | ladder | 140.0 | 7 |
| 100 | text | depth-live | 175.2 | 5 |
| 200 | svg | ladder | 306.0 | 3 |
| 200 | svg | depth-live | 137.9 | 7 |
| 200 | text | ladder | 249.4 | 4 |
| 200 | text | depth-live | 222.8 | 4 |

Install the package byte-compiled (or native-compiled) for live books.
Interpreted, a 200-level ladder cannot keep 5 fps.

### Where the time goes

- Applying deltas costs nothing measurable (under 0.1 ms).
- The eas recompile ("push") dominates. It grows with rows: the ladder
  has an ordinal band per level, plus three layers.
- Drawing a text grid costs about 14 ms whatever the depth, which is
  why text depth-live is slower than SVG depth-live at 50 levels.

A keyed push (eas-kbh) would not remove the recompile. A data-only
scene patch in eas would be the next big win.

### Soak: real timers, 50 delta batches a second

`financial-chart-book-bench-soak-report` runs each configuration for
3 s. The feed sends 50 batches of 5 deltas per second, the default cap
applies, and a 10 ms probe timer measures how late Emacs gets to it.
"busy" is the share of wall time spent feeding and drawing.

| levels/side | backend | template | cap fps | delta batches | frames | fps | busy | worst probe delay ms |
|---|---|---|---|---|---|---|---|---|
| 50 | svg | ladder | 10 | 151 | 30 | 10.0 | 0.28 | 30 |
| 50 | svg | depth-live | 10 | 151 | 30 | 10.0 | 0.15 | 21 |
| 50 | text | ladder | 10 | 151 | 30 | 10.0 | 0.21 | 24 |
| 50 | text | depth-live | 10 | 151 | 30 | 10.0 | 0.4 | 31 |
| 100 | svg | ladder | 8 | 151 | 24 | 8.0 | 0.37 | 46 |
| 100 | svg | depth-live | 8 | 151 | 24 | 8.0 | 0.17 | 30 |
| 100 | text | ladder | 8 | 151 | 24 | 8.0 | 0.25 | 37 |
| 100 | text | depth-live | 8 | 151 | 24 | 8.0 | 0.35 | 36 |
| 200 | svg | ladder | 5 | 151 | 15 | 5.0 | 0.46 | 78 |
| 200 | svg | depth-live | 5 | 151 | 15 | 5.0 | 0.19 | 39 |
| 200 | text | ladder | 5 | 151 | 15 | 5.0 | 0.29 | 53 |
| 200 | text | depth-live | 5 | 151 | 15 | 5.0 | 0.26 | 40 |

Every configuration holds its cap, from 150 delta batches folded into
15 to 30 frames. Emacs stays responsive: busy time is at most 46% of
wall time, and the probe is never more than one frame late (78 ms
worst, for the 200-level SVG ladder). Hence the default cap
(`financial-chart-book-default-fps`): 10 fps up to 50 levels per side,
8 up to 100, and 5 beyond. That is inside the 5-15 fps target.
`:max-fps` overrides it. In a GUI frame, the display's image decode
comes on top of these batch numbers.

Reproduce, byte-compiling eas.el and this package first:

    emacs -Q --batch -L $EAS/src -L src -L src/core -L src/indicators \
      -L src/renderers -L src/charts -L src/integrations \
      -l financial-chart-eas-book-bench -f financial-chart-book-bench-report
    # ... -f financial-chart-book-bench-soak-report
