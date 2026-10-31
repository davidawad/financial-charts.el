#!/usr/bin/env python3
"""Write templates3d/vol-surface.json, the 3D variant of vol-surface.

The variant is templates/financial/vol-surface.json with its body
unchanged and an x-eas.graphics naming the fc-vol-surface3d mark
(src/financial-chart-3d-vol-surface.el), as scripts/templates3d.py
writes the others.

    python3 scripts/templates3d_vol_surface.py
"""

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from templates3d import ROOT, TIMELINE, variant  # noqa: E402

VOL_SURFACE = {
    "source": "templates/financial/vol-surface.json",
    "example": "examples/vol-surface.data.json",
    "reading": "IV surface over strike and days to expiry as a mesh of each "
               "expiry's smile and the strikes' term structures, coloured by IV, "
               "the at-the-money term structure traced across it; replays the "
               "session quote time by quote time while the camera sways",
    "graphics": {
        "mark": "fc-vol-surface3d",
        "encoding": {"x": {"field": "strike"}, "z": {"field": "dte"},
                     "y": {"field": "iv"},
                     "time": {"field": "time", "mode": "frame", "step": 1.0,
                              "easing": "linear"}},
        "height": 5,
        "style": "wire",
        # The nearest expiry's skew stands at the back left; the target
        # sits left of and below the box's centre so the mesh fills 60x24.
        "camera": {"yaw": -25, "pitch": 30, "zoom": 1.3, "target": [-1.0, 1.0, 0.5]},
        # Sways 15 degrees right and back over the 8 snapshots.
        "camera_path": {"keyframes": [{"t": 0, "yaw": 0}, {"t": 4, "yaw": 15},
                                      {"t": 8, "yaw": 0}]},
        "timeline": TIMELINE,
    },
}


def main():
    path = os.path.join(ROOT, "templates3d", "vol-surface.json")
    with open(path, "w") as f:
        json.dump(variant("vol-surface", VOL_SURFACE), f, indent=2)
        f.write("\n")
    print(path)


if __name__ == "__main__":
    sys.exit(main())
