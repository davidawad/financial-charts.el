#!/usr/bin/env python3
"""Write templates3d/: the 3D variant of every financial-chart template.

Each variant is the template from templates/ (or templates/financial/)
with its body unchanged and an x-eas.graphics added, naming a graphics.el
mark (graphics.el's docs/design/eas-integration.md).  Slots the 3D
reading needs (payoff's surface, volatility and days) are added beside
the template's own; the example is the 3D one (`example` below).
src/financial-chart-3d.el registers the directory with eas under the
namespace "fc3d", so "fc3d/ohlc" is the variant of "ohlc".

    python3 scripts/templates3d.py      # or: make templates3d

financial-chart-3d-test.el checks that every variant's body still equals
its template's, so a change to a template means running this again.
"""

import json
import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Replay: a looping timeline of DURATION seconds whose position the mark
# reads (its "time" param), drawing every row's scales at every phase:
# a series grows from a third of its bars to all of them, a payoff
# surface stays whole while a trace sweeps it to expiry.  Not
# graphics.el's encoding.time, which hands the mark only the rows so far
# (one bar, one row of the surface, at the start).
def replay(duration):
    return {"loop": True, "autoplay": True, "duration": duration}


SIDES = {"bid": "#26a69a", "ask": "#ef5350"}

PAYOFF_SLOTS = {
    "surface": {
        "shape": "plist",
        "doc": "rows {price, dte, day, pnl}: P/L at each price and days to expiry "
               "(financial-chart-3d-payoff-surface); computed from data, vol and "
               "days when left out",
    },
    "vol": {
        "type": "number",
        "default": 0.3,
        "doc": "annualised volatility the surface's earlier days are priced at "
               "(lognormal, zero rate)",
    },
    "days": {
        "type": "array",
        "items": "number",
        "default": [60, 45, 30, 21, 14, 7, 3, 0],
        "doc": "days to expiry of the surface's rows, furthest first; 0 is the "
               "expiry payoff itself",
    },
}

VARIANTS = {
    "ohlc": {
        "source": "templates/ohlc.json",
        "example": "examples/panes.data.json",
        "reading": "candles as boxes along time, price up, each body as deep as "
                   "its volume; replays from its first third bar by bar",
        "graphics": {
            "mark": "fc-candle3d",
            "encoding": {"x": {"field": "time"}, "depth": {"field": "volume"}},
            "timeline": replay(8),
        },
    },
    "panes": {
        "source": "templates/panes.json",
        "example": "examples/panes.data.json",
        "reading": "price, volume and RSI(14) as three stacked panels on one "
                   "time axis; replays from its first third bar by bar",
        "graphics": {
            "mark": "fc-panes3d",
            "encoding": {"x": {"field": "time"}},
            "rsi": 14,
            "timeline": replay(8),
        },
    },
    "volume-profile": {
        "source": "templates/financial/volume-profile.json",
        "example": "examples/volume-profile.data.json",
        "reading": "horizontal volume-at-price bars beside the candles, point "
                   "of control bright; the profile builds as the bars replay from "
                   "their first third",
        "graphics": {
            "mark": "fc-volume-profile3d",
            "data": {"x-eas:slot": "bars"},
            "bins": {"x-eas:slot": "bins"},
            "encoding": {"x": {"field": "time"}},
            "timeline": replay(8),
        },
    },
    "drawdown": {
        "source": "templates/drawdown.json",
        "example": "examples/drawdown.data.json",
        "reading": "the underwater curve as a slab hanging below the "
                   "high-water line, seen from below; replays from its first third "
                   "point by point",
        "graphics": {
            "mark": "fc-drawdown3d",
            "encoding": {"x": {"field": "x"}, "y": {"field": "y"}},
            "timeline": replay(8),
        },
    },
    "payoff": {
        "source": "templates/payoff.json",
        "example": "examples/3d/payoff.data.json",
        "slots": PAYOFF_SLOTS,
        "reading": "P/L surface over price and days to expiry, the earlier days "
                   "priced from the expiry payoff; a bright trace sweeps it to expiry",
        "graphics": {
            "mark": "fc-payoff3d",
            "data": {"x-eas:slot": "surface"},
            "vol": {"x-eas:slot": "vol"},
            "days": {"x-eas:slot": "days"},
            "encoding": {"x": {"field": "price"}, "z": {"field": "dte"},
                         "y": {"field": "pnl"}},
            "timeline": replay(6),
        },
    },
    "payoff-curves": {
        "source": "templates/payoff-curves.json",
        "example": "examples/3d/payoff-curves.data.json",
        "reading": "the curves as one P/L surface over price and curve (a "
                   "strategy at several dates: a surface toward expiry); "
                   "a bright trace sweeps it curve by curve",
        "graphics": {
            "mark": "fc-payoff3d",
            "encoding": {"x": {"field": "price"}, "z": {"field": "curve"},
                         "y": {"field": "pnl"}},
            "timeline": replay(6),
        },
    },
    "depth": {
        "source": "templates/depth.json",
        "example": "examples/depth.data.json",
        "reading": "cumulative depth as two walls stepping away from the mid, "
                   "bids in front, asks behind",
        "graphics": {
            "mark": "fc-depth3d",
            "encoding": {"x": {"field": "price"}, "y": {"field": "size"},
                         "color": {"field": "side"}},
            "colors": SIDES,
        },
    },
    "depth-live": {
        "source": "templates/financial/depth-live.json",
        "example": "examples/3d/depth-live.data.json",
        "reading": "order-book surface: cumulative depth by price (across) and "
                   "step (back), streaming live from financial-chart-3d-book-push",
        "graphics": {
            "mark": "depth3d",
            "filter": {"field": "side", "oneOf": ["bid", "ask"]},
            "encoding": {"x": {"field": "price"}, "z": {"field": "step"},
                         "y": {"field": "cumulative"}, "color": {"field": "side"}},
            "colors": SIDES,
            "max_rows": 60,
            "extent": [16, 12],
        },
    },
    "ladder": {
        "source": "templates/financial/ladder.json",
        "example": "examples/order-book/ladder.data.json",
        "reading": "the price ladder as 3D bars, one per level, bids and asks in "
                   "two lanes; live pushes grow and shrink them",
        "graphics": {
            "mark": "bar3d",
            "filter": {"field": "side", "oneOf": ["bid", "ask"]},
            "encoding": {"x": {"field": "price_label"}, "z": {"field": "side"},
                         "y": {"field": "size"}, "color": {"field": "side"}},
            "colors": SIDES,
            "band": 0.6,
        },
    },
    "diverging-bars": {
        "source": "templates/diverging-bars.json",
        "example": "examples/diverging-bars.data.json",
        "reading": "one 3D bar per label, up from the floor for gains and down "
                   "through it for losses",
        "graphics": {
            "mark": "fc-diverging3d",
            "encoding": {"x": {"field": "label"}, "y": {"field": "value"}},
        },
    },
}


def variant(name, cfg):
    with open(os.path.join(ROOT, cfg["source"])) as f:
        template = json.load(f)
    meta = template["x-eas"]
    out = {}
    for key, value in template.items():
        if key != "x-eas":
            out[key] = value
    x_eas = {}
    for key, value in meta.items():
        if key == "doc":
            x_eas["doc"] = (f"3D variant of financial-chart's {name} (graphics.el, mark "
                            f"{cfg['graphics']['mark']}): {cfg['reading']}.  2D: {value}")
        elif key == "slots":
            x_eas["slots"] = dict(value, **cfg.get("slots", {}))
        elif key == "example":
            x_eas["example"] = cfg["example"]
        else:
            x_eas[key] = value
    x_eas["base"] = name
    x_eas["graphics"] = cfg["graphics"]
    # x-eas first, as in graphics.el's templates3d/.
    result = {"$schema": out.pop("$schema")} if "$schema" in out else {}
    result["x-eas"] = x_eas
    result.update(out)
    return result


def main():
    directory = os.path.join(ROOT, "templates3d")
    os.makedirs(directory, exist_ok=True)
    for name, cfg in VARIANTS.items():
        path = os.path.join(directory, name + ".json")
        with open(path, "w") as f:
            json.dump(variant(name, cfg), f, indent=2)
            f.write("\n")
        print(path)


if __name__ == "__main__":
    sys.exit(main())
