#!/usr/bin/env python3
"""Write the options-positioning templates: 2D and their 3D variants.

Each chart is a 2D template in templates/financial/ (what eas draws
when graphics.el is absent) and its 3D variant in templates3d/: the
same body plus an x-eas.graphics naming a mark that
src/financial-chart-3d-positioning.el registers.  The examples are the
fixtures in test/fixtures/3d/options/; the data contracts are in
docs/3d-positioning.md.

    python3 scripts/templates3d-positioning.py

financial-chart-3d-test.el checks that every variant's body equals its
template's, so a change here means running this again.
"""

import json
import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FIXTURES = "test/fixtures/3d/options"
CALL = "#26a69a"
PUT = "#ef5350"
GOLD = "#ffb74d"
TIMELINE = {"loop": True, "autoplay": True}


def replay(field, step):
    return {"field": field, "mode": "cumulative", "step": step}


def slot(name):
    return {"x-eas:slot": name}


def text_slots(description):
    return {
        "title": {"type": "string", "default": ""},
        "description": {
            "type": "string",
            "default": description,
            "doc": "the takeaway; alt text and table-view fallback (bin/chart check requires it)",
        },
    }


def readout(fields):
    """A one-line hover readout: the fields (FIELD, LABEL, SIGNED) of the datum."""
    children = []
    for i, (field, label, signed) in enumerate(fields):
        props = {"field": field, "label": label, "priority": 95 - 5 * i}
        children.append({"component": "signed-value" if signed else "quantity", "props": props})
    return {
        "max_lines": 1,
        "component": "row",
        "children": [
            {"component": "text", "props": {"text": {"expr": "at"}, "priority": 10, "style": {"dim": True}}},
            {"component": "when", "props": {"test": "isValid(datum.%s)" % fields[-1][0]},
             "children": [{"component": "row", "children": children}],
             "else": [{"component": "fields", "props": {"source": "auto"}}]},
        ],
    }


def spot_rule():
    return {
        "name": "spot",
        "data": {"values": [{"spot": slot("spot")}]},
        "mark": {"type": "rule", "color": "#d2ddeb", "strokeDash": [4, 2]},
        "encoding": {"x": {"field": "spot", "type": "quantitative", "scale": {"zero": False}},
                     "tooltip": [{"field": "spot", "type": "quantitative", "format": ".2f"}]},
    }


def max_pain_points():
    # Kept only when the max_pain slot is bound.
    return {"x-eas:when": "max_pain", "spec": {
        "name": "max-pain",
        "data": {"name": "max_pain"},
        "mark": {"type": "point", "shape": "triangle-up", "filled": True, "color": GOLD, "size": 80},
        "encoding": {"x": {"field": "max_pain", "type": "quantitative", "scale": {"zero": False}},
                     "y": {"datum": 0},
                     "tooltip": [{"field": "expiry", "type": "nominal"},
                                 {"field": "max_pain", "type": "quantitative", "title": "max pain"}]},
    }}


def strike_bars(call, put, title, fmt):
    return {
        "name": "strikes",
        "transform": [
            {"fold": [call, put], "as": ["side", "amount"]},
            {"calculate": "datum.side == '%s' ? 'puts' : 'calls'" % put, "as": "type"},
            {"calculate": "datum.side == '%s' ? -abs(datum.amount) : abs(datum.amount)" % put, "as": "signed"},
            {"aggregate": [{"op": "sum", "field": "signed", "as": "total"}], "groupby": ["strike", "type"]},
            {"calculate": "datum.strike - 1.8", "as": "left"},
            {"calculate": "datum.strike + 1.8", "as": "right"},
            {"calculate": "0", "as": "zero"},
        ],
        "mark": {"type": "rect"},
        "encoding": {
            "x": {"field": "left", "type": "quantitative", "scale": {"zero": False}, "title": "strike"},
            "x2": {"field": "right"},
            "y": {"field": "total", "type": "quantitative", "title": title},
            "y2": {"field": "zero"},
            "color": {"field": "type", "type": "nominal", "title": None,
                      "scale": {"domain": ["calls", "puts"], "range": [CALL, PUT]}},
            "tooltip": [{"field": "strike", "type": "quantitative"},
                        {"field": "type", "type": "nominal"},
                        {"field": "total", "type": "quantitative", "title": title, "format": fmt}],
        },
    }


MAX_PAIN_SLOT = {
    "shape": "plist",
    "default": None,
    "doc": "opt.max_pain rows {expiry, max_pain}; computed from the open interest when left out",
}
SPOT_SLOT = {"type": "number", "required": True, "doc": "the underlying's price, a plane through the strike axis"}

OI_DESC = ("Open interest by strike, calls up and puts down, summed over expiries, with spot ruled "
           "and each expiry's max-pain strike marked.")
GEX_DESC = ("Dealer gamma exposure by strike ($ per 1% move), call gamma up and put gamma down, "
            "summed over expiries, with spot ruled.")
SKEW_DESC = ("25-delta risk reversal and butterfly by tenor, one line per snapshot, the newest "
             "brightest: how put skew and wings move over time.")
VRP_DESC = ("Implied against realized volatility over time; the gap between them is the variance "
            "risk premium, green when implied is rich.")

CHARTS = {
    "options-oi": {
        "desc": OI_DESC,
        "slots": {
            "data": {"shape": "plist", "required": True,
                     "doc": "opt.chain_stats rows {expiry, strike, call_oi, put_oi}"},
            "spot": SPOT_SLOT,
            "max_pain": MAX_PAIN_SLOT,
        },
        "readout": [("strike", "strike", False), ("total", "OI", True)],
        "height": 240,
        "layer": [strike_bars("call_oi", "put_oi", "open interest (puts below)", ",.0f"),
                  spot_rule(), max_pain_points()],
        "example": "oi.data.json",
        "reading": "open interest as 3D bars on a strike by expiry floor, calls rising and puts "
                   "hanging below it, spot a plane across the strikes and each expiry's max-pain "
                   "strike a gold post",
        "graphics": {
            "mark": "fc-strikes3d",
            "encoding": {"x": {"field": "strike"}, "z": {"field": "expiry"},
                         "up": {"field": "call_oi"}, "down": {"field": "put_oi"}},
            "spot": slot("spot"),
            "max_pain": slot("max_pain"),
            "max_pain_auto": True,
            "camera": {"yaw": 12, "pitch": 24, "zoom": 1.3},
        },
    },
    "options-gex": {
        "desc": GEX_DESC,
        "slots": {
            "data": {"shape": "plist", "required": True,
                     "doc": "opt.gex rows {expiry, strike, call_gex, put_gex[, net_gex]}, $ per 1% move; "
                            "put gamma negative (dealers short puts)"},
            "spot": SPOT_SLOT,
            "max_pain": MAX_PAIN_SLOT,
        },
        "readout": [("strike", "strike", False), ("total", "GEX", True)],
        "height": 240,
        "layer": [strike_bars("call_gex", "put_gex", "gamma exposure ($M per 1%)", ",.1f"),
                  spot_rule()],
        "example": "gex.data.json",
        "reading": "dealer gamma as 3D bars on a strike by expiry floor, call gamma up and put gamma "
                   "down, spot a plane across the strikes and the gamma flip (where net gamma by "
                   "strike changes sign) a violet one",
        "graphics": {
            "mark": "fc-strikes3d",
            "encoding": {"x": {"field": "strike"}, "z": {"field": "expiry"},
                         "up": {"field": "call_gex"}, "down": {"field": "put_gex"}},
            "spot": slot("spot"),
            "flip": True,
            "camera": {"yaw": 12, "pitch": 24, "zoom": 1.3},
        },
    },
    "options-skew": {
        "desc": SKEW_DESC,
        "slots": {
            "data": {"shape": "plist", "required": True,
                     "doc": "opt.skew_rr25_bf25 rows {date, tenor, rr25, bf25}, vol points, tenor in days; "
                            "one snapshot per date"},
        },
        "readout": [("date", "", False), ("tenor", "days", False), ("value", "", True)],
        "height": 220,
        "example": "skew.data.json",
        "reading": "risk reversal (top) and butterfly (bottom) as ribbons over time, one per "
                   "tenor (shortest in front), the newest term structure traced bright across "
                   "them; replays snapshot by snapshot",
        "graphics": {
            "mark": "fc-skew3d",
            "encoding": {"x": {"field": "date"}, "z": {"field": "tenor"},
                         "time": replay("date", 0.5)},
            "panels": ["rr25", "bf25"],
            "timeline": TIMELINE,
            "camera": {"yaw": -20, "pitch": 28, "zoom": 1.3},
        },
    },
    "options-vrp": {
        "desc": VRP_DESC,
        "slots": {
            "data": {"shape": "plist", "required": True,
                     "doc": "rows {date, iv, rv[, vrp, iv_rank, calendar_ratio]}: opt.vrp joined with "
                            "stats.realized_vol, opt.iv_rank and opt.calendar_iv_ratio by date"},
        },
        "readout": [("date", "", False), ("iv", "IV", False), ("rv", "RV", False), ("vrp", "VRP", True)],
        "height": 220,
        "layer": "vrp",
        "example": "vrp.data.json",
        "reading": "implied and realized volatility as the two edges of one ribbon over time, filled "
                   "green where implied is rich and red where realized overtakes it, IV rank as "
                   "bars behind and the front/back IV ratio in front; replays from its first third day by day",
        "graphics": {
            "mark": "fc-vrp3d",
            "encoding": {"x": {"field": "date"}},
            "replay": "date",
            "timeline": {"loop": True, "autoplay": True, "duration": 6},
            "camera": {"yaw": -25, "pitch": 15, "zoom": 1.35},
        },
    },
}


def skew_body():
    return {
        "data": {"name": "data"},
        "transform": [{"fold": ["rr25", "bf25"], "as": ["measure", "value"]},
                      {"calculate": "datum.measure == 'rr25' ? '25d risk reversal' : '25d butterfly'",
                       "as": "panel"}],
        "facet": {"row": {"field": "panel", "type": "nominal", "title": None,
                          "sort": ["25d risk reversal", "25d butterfly"]}},
        "resolve": {"scale": {"y": "independent"}},
        "spec": {
            "height": 100,
            "mark": {"type": "line", "point": False},
            "encoding": {
                "x": {"field": "tenor", "type": "quantitative", "title": "tenor (days)"},
                "y": {"field": "value", "type": "quantitative", "title": "vol pts",
                      "scale": {"zero": False}},
                "color": {"field": "date", "type": "ordinal", "title": None, "legend": None,
                          "scale": {"scheme": "blues"}},
                "tooltip": [{"field": "date", "type": "nominal"},
                            {"field": "tenor", "type": "quantitative"},
                            {"field": "measure", "type": "nominal"},
                            {"field": "value", "type": "quantitative", "format": ".2f"}],
            },
        },
    }


def vrp_layers():
    x = {"field": "date", "type": "temporal", "title": None}
    return [
        {"name": "premium",
         "mark": {"type": "area", "opacity": 0.35},
         "encoding": {"x": x,
                      "y": {"field": "rv", "type": "quantitative", "title": "volatility (%)",
                            "scale": {"zero": False}},
                      "y2": {"field": "iv"},
                      "color": {"value": CALL}}},
        {"name": "iv",
         "mark": {"type": "line", "color": "#f58518"},
         "encoding": {"x": x, "y": {"field": "iv", "type": "quantitative"},
                      "tooltip": [{"field": "date", "type": "temporal"},
                                  {"field": "iv", "type": "quantitative", "title": "IV"},
                                  {"field": "rv", "type": "quantitative", "title": "RV"},
                                  {"field": "vrp", "type": "quantitative", "title": "VRP"},
                                  {"field": "iv_rank", "type": "quantitative", "title": "IV rank"}]}},
        {"name": "rv",
         "mark": {"type": "line", "color": "#4c78a8", "strokeDash": [4, 2]},
         "encoding": {"x": x, "y": {"field": "rv", "type": "quantitative"}}},
    ]


def template(name, cfg):
    meta = {
        "template": name,
        "version": "1.0.0",
        "readout": readout(cfg["readout"]),
        "doc": cfg["desc"],
        "slots": dict(cfg["slots"], **text_slots(cfg["desc"])),
        "example": "../" + FIXTURES + "/" + cfg["example"],
    }
    body = {"title": slot("title"), "description": slot("description"), "width": "container"}
    if name == "options-skew":
        body.update(skew_body())
    else:
        body["height"] = cfg["height"]
        body["data"] = {"name": "data"}
        body["layer"] = vrp_layers() if cfg["layer"] == "vrp" else cfg["layer"]
    out = {"$schema": "https://vega.github.io/schema/vega-lite/v6.json", "x-eas": meta}
    out.update(body)
    return out


def variant(name, cfg, base):
    meta = dict(base["x-eas"])
    meta["doc"] = (f"3D variant of financial-chart's {name} (graphics.el, mark "
                   f"{cfg['graphics']['mark']}): {cfg['reading']}.  2D: {base['x-eas']['doc']}")
    meta["example"] = FIXTURES + "/" + cfg["example"]
    meta["base"] = name
    meta["graphics"] = cfg["graphics"]
    out = {"$schema": base["$schema"], "x-eas": meta}
    out.update({k: v for k, v in base.items() if k not in ("$schema", "x-eas")})
    return out


def write(path, obj):
    with open(path, "w") as f:
        json.dump(obj, f, indent=2)
        f.write("\n")
    print(path)


def main():
    for name, cfg in CHARTS.items():
        base = template(name, cfg)
        write(os.path.join(ROOT, "templates", "financial", name + ".json"), base)
        write(os.path.join(ROOT, "templates3d", name + ".json"), variant(name, cfg, base))


if __name__ == "__main__":
    sys.exit(main())
