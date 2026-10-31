#!/usr/bin/env python3
"""Write the options surfaces' 3D variants into templates3d/.

As scripts/templates3d.py does for the other templates (its `variant'
writes each: the 2D template from templates/financial/ with its body
unchanged and an x-eas.graphics added), for the marks
src/financial-chart-3d-options.el registers.  docs/3d-options.md is the
data contract of each.

    python3 scripts/templates3d_options.py
"""

import json
import os
import sys

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from templates3d import ROOT, replay_timeline, variant  # noqa: E402

EXAMPLES = "examples/3d/"

VARIANTS = {
    "greeks-surface": {
        "source": "templates/financial/greeks-surface.json",
        "example": EXAMPLES + "greeks-surface.data.json",
        "reading": "the greek as a surface over spot and days to expiry, the "
                   "slice at the current spot traced; a bright trace sweeps it toward expiry",
        "graphics": {
            "mark": "fc-greeks3d",
            "camera": {"yaw": -35, "pitch": 16},
            "spot": {"x-eas:slot": "spot"},
            "encoding": {"x": {"field": "spot"}, "z": {"field": "dte"},
                         "y": {"field": {"x-eas:slot": "greek"}}},
            "timeline": replay_timeline(5),
        },
    },
    "position-pnl-surface": {
        "source": "templates/financial/position-pnl-surface.json",
        "example": EXAMPLES + "position-pnl-surface.data.json",
        "reading": "P/L surface over spot and days to expiry with the expiry "
                   "payoff as its front edge; a bright trace sweeps it day by day toward it",
        "graphics": {
            "mark": "fc-pnl3d",
            "camera": {"yaw": -35, "pitch": 16},
            "spot": {"x-eas:slot": "spot"},
            "encoding": {"x": {"field": "spot"}, "z": {"field": "dte"},
                         "y": {"field": "pnl"}},
            "timeline": replay_timeline(6),
        },
    },
    "pop-surface": {
        "source": "templates/financial/pop-surface.json",
        "example": EXAMPLES + "pop-surface.data.json",
        "reading": "the probability as a surface over strike and expiry, red "
                   "at 0 to green at 1, the even-odds level framed",
        "graphics": {
            "mark": "fc-pop3d",
            "camera": {"yaw": 35, "pitch": 18, "zoom": 1.35, "target": [-0.6, 0.9, 0]},
            "spot": {"x-eas:slot": "spot"},
            "encoding": {"x": {"field": "strike"}, "z": {"field": "dte"},
                         "y": {"field": {"x-eas:slot": "metric"}}},
        },
    },
}


def main():
    for name, cfg in VARIANTS.items():
        path = os.path.join(ROOT, "templates3d", name + ".json")
        with open(path, "w") as f:
            json.dump(variant(name, cfg), f, indent=2)
            f.write("\n")
        print(path)


if __name__ == "__main__":
    sys.exit(main())
