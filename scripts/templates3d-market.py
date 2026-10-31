#!/usr/bin/env python3
"""Write the market-structure 3D variants into templates3d/.

rrg, sector-breadth, liquidity-heatmap and session-profile: each the
template from templates/financial/ with its body unchanged and an
x-eas.graphics added, exactly as scripts/templates3d.py writes the
others (its `variant').  Their marks are registered by
src/financial-chart-3d-market.el; their examples are
test/fixtures/3d/market/ (`financial-chart-3d-market-write-fixtures').

    python3 scripts/templates3d-market.py
"""

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from templates3d import ROOT, TIMELINE, replay, variant  # noqa: E402

FIXTURES = "test/fixtures/3d/market"

VARIANTS = {
    "rrg": {
        "source": "templates/financial/rrg.json",
        "example": f"{FIXTURES}/rrg.data.json",
        "reading": "the rotation graph on the floor (RS-Ratio across, "
                   "RS-Momentum away), each sector's trail of its last tail "
                   "dates climbing to its newest point, a stem down to where it "
                   "stands now; rotates date by date",
        "graphics": {
            "mark": "fc-rrg3d",
            "tail": {"x-eas:slot": "tail"},
            "encoding": {"x": {"field": "rs_ratio"}, "z": {"field": "rs_momentum"},
                         "color": {"field": "symbol"},
                         "time": replay("date", 0.5)},
            "timeline": TIMELINE,
            "camera": {"yaw": 15, "pitch": 45, "zoom": 1.15},
        },
    },
    "sector-breadth": {
        "source": "templates/financial/sector-breadth.json",
        "example": f"{FIXTURES}/sector-breadth.data.json",
        "reading": "breadth as a surface over sector (across) and date "
                   "(toward the viewer), red to green, the 50% level framed, "
                   "the newest date traced bright and the sectors' mean up the "
                   "right wall; grows day by day",
        "graphics": {
            "mark": "fc-breadth3d",
            "encoding": {"x": {"field": "sector"}, "z": {"field": "date"},
                         "y": {"field": "pct"}, "time": replay("date", 0.15)},
            "timeline": TIMELINE,
            "camera": {"yaw": -65, "pitch": 40, "zoom": 1.2},
        },
    },
    "liquidity-heatmap": {
        "source": "templates/financial/liquidity-heatmap.json",
        "example": f"{FIXTURES}/liquidity-heatmap.data.json",
        "reading": "Bookmap's heatmap raised: resting size by step (across) and "
                   "price (away), dark blue to amber, the mid price traced in "
                   "white; replays step by step, and streams live from "
                   "financial-chart-3d-liquidity-push",
        "graphics": {
            "mark": "fc-liquidity3d",
            "encoding": {"x": {"field": "step"}, "z": {"field": "price"},
                         "y": {"field": "size"}, "color": {"field": "side"},
                         "time": replay("step", 0.2)},
            "timeline": TIMELINE,
            "max_rows": 60,
            "camera": {"yaw": -20, "pitch": 45, "zoom": 1.2},
        },
    },
    "session-profile": {
        "source": "templates/financial/session-profile.json",
        "example": f"{FIXTURES}/session-profile.data.json",
        "reading": "one volume-at-price slab per session, price across and "
                   "sessions toward the viewer, each point of control capped "
                   "white and the VWAPs joined across the floor; builds session "
                   "by session",
        "graphics": {
            "mark": "fc-session-profile3d",
            "encoding": {"x": {"field": "price"}, "z": {"field": "session"},
                         "y": {"field": "volume"}, "time": replay("session", 0.8)},
            "timeline": TIMELINE,
            "camera": {"yaw": 5, "pitch": 45, "zoom": 1.2},
        },
    },
}


def main():
    directory = os.path.join(ROOT, "templates3d")
    for name, cfg in VARIANTS.items():
        path = os.path.join(directory, name + ".json")
        with open(path, "w") as f:
            json.dump(variant(name, cfg), f, indent=2)
            f.write("\n")
        print(path)


if __name__ == "__main__":
    sys.exit(main())
