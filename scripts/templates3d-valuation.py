#!/usr/bin/env python3
"""Write the valuation surfaces' 3D variants into templates3d/.

dcf-sensitivity, revenue-surface and kelly-surface (templates/financial/)
with an x-eas.graphics naming the marks src/financial-chart-3d-valuation.el
registers, as scripts/templates3d.py writes the others (its `variant').
Their examples are the fixtures in test/fixtures/3d/valuation/; the data
contracts are docs/3d-valuation.md.

    python3 scripts/templates3d-valuation.py
"""

import os
import sys

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from templates3d import ROOT, TIMELINE, variant  # noqa: E402
import json  # noqa: E402

FIXTURES = "test/fixtures/3d/valuation/"

VARIANTS = {
    "dcf-sensitivity": {
        "source": "templates/financial/dcf-sensitivity.json",
        "example": FIXTURES + "dcf-sensitivity.data.json",
        "reading": "value per share as a surface over WACC and terminal growth, "
                   "the current price a plane through it and the fair-value line "
                   "where they cross; snapshots morph one into the next",
        "graphics": {
            "mark": "fc-dcf3d",
            "encoding": {"x": {"field": "wacc"}, "z": {"field": "terminal_growth"},
                         "y": {"field": "value_per_share"},
                         "time": {"field": "as_of", "mode": "frame", "step": 1.2,
                                  "easing": "cubic-in-out"}},
            "level": {"x-eas:slot": "price"},
            "level_field": "price",
            "height": 6,
            "camera": {"yaw": 140, "pitch": 20, "zoom": 1.2},
            "timeline": TIMELINE,
        },
    },
    "revenue-surface": {
        "source": "templates/financial/revenue-surface.json",
        "example": FIXTURES + "revenue-surface.data.json",
        "reading": "revenue as a surface over fiscal year and outcome quantile, "
                   "the cone widening with the horizon and the median traced; "
                   "it grows year by year",
        "graphics": {
            "mark": "fc-revenue3d",
            "encoding": {"x": {"field": "year"}, "z": {"field": "quantile"},
                         "y": {"field": "revenue"},
                         "time": {"field": "year", "mode": "cumulative", "step": 0.4}},
            "trace": 0.5,
            "front": True,
            "low_color": "#2c3e7a",
            "high_color": "#ffb74d",
            "height": 6,
            "camera": {"yaw": -55, "pitch": 24, "zoom": 1.15},
            "timeline": TIMELINE,
        },
    },
    "kelly-surface": {
        "source": "templates/financial/kelly-surface.json",
        "example": FIXTURES + "kelly-surface.data.json",
        "reading": "log growth as a surface over bet fraction and edge, the "
                   "Kelly fraction traced along its ridge and the zero-growth "
                   "line where over-betting starts to lose",
        "graphics": {
            "mark": "fc-kelly3d",
            "encoding": {"x": {"field": "fraction"}, "z": {"field": "edge"},
                         "y": {"field": "growth"}},
            "level": 0,
            "trace": "max",
            "plane_color": "#5b6b86",
            "contour_color": "#ffb74d",
            "height": 5,
            "camera": {"yaw": -135, "pitch": 25, "zoom": 1.15},
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
