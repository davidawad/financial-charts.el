#!/usr/bin/env python3
"""Write the rates and credit templates: 2D and their 3D variants.

Each is a 2D template in templates/financial/ (a heatmap of the rows its
data shape makes from invest-metrics JSON) and its 3D variant in
templates3d/, written by scripts/templates3d.py's `variant' (body
unchanged, x-eas.graphics added).  src/financial-chart-3d-rates.el
registers the data shapes and the marks; docs/3d-rates.md has the data
contracts; the examples are test/fixtures/3d/rates/.

    python3 scripts/templates3d-rates.py
"""

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from templates3d import ROOT, replay_timeline, variant  # noqa: E402

FIXTURES = "test/fixtures/3d/rates"

REGIMES = {"normal": "#26a69a", "flat": "#eeca3b", "inverted": "#ef5350"}


def slots(shape, data_doc, doc, extra=None):
    out = {
        "data": {"shape": shape, "required": True, "doc": data_doc},
        "title": {"type": "string", "default": ""},
        "description": {
            "type": "string",
            "default": doc,
            "doc": "the takeaway; alt text and table-view fallback (bin/chart check requires it)",
        },
    }
    out.update(extra or {})
    return out


def heatmap(name, doc, data_doc, shape, x, y, color, tooltip, scheme, label,
            extra_slots=None, filter_=None, mid=None, height=None):
    scale = {"scheme": scheme}
    if mid is not None:
        scale["domainMid"] = mid
    transforms = [{"filter": filter_}] if filter_ else []
    return {
        "$schema": "https://vega.github.io/schema/vega-lite/v6.json",
        "x-eas": {
            "template": name,
            "version": "1.0.0",
            "doc": doc,
            "slots": slots(shape, data_doc, doc, extra_slots),
            "example": f"../{FIXTURES}/{name}.data.json",
        },
        "title": {"x-eas:slot": "title"},
        "description": {"x-eas:slot": "description"},
        "width": "container",
        "height": height or 220,
        "data": {"name": "data"},
        **({"transform": transforms} if transforms else {}),
        "encoding": {
            "x": {"field": x[0], "type": "nominal", "sort": None, "title": x[1]},
            "y": {"field": y[0], "type": "nominal", "sort": None, "title": y[1]},
        },
        "layer": [
            {
                "name": "cells",
                "mark": "rect",
                "encoding": {
                    "color": {"field": color[0], "type": "quantitative", "title": color[1],
                              "scale": scale},
                    "tooltip": tooltip,
                },
            },
            {
                "name": "values",
                "mark": {"type": "text", "fontSize": 9},
                "encoding": {"text": {"field": color[0], "type": "quantitative",
                                      "format": label}},
            },
        ],
    }


def q(field, fmt=None, title=None):
    t = {"field": field, "type": "quantitative"}
    if fmt:
        t["format"] = fmt
    if title:
        t["title"] = title
    return t


def n(field, title=None):
    t = {"field": field, "type": "nominal"}
    if title:
        t["title"] = title
    return t


TEMPLATES = {
    "yield-curve": {
        "template": heatmap(
            "yield-curve",
            "Fitted yield curves by date (fi.nss_fit per date, regime from fi.curve_regime): "
            "yield by tenor across and date down, so an inversion reads as a row whose short end "
            "is hotter than its long end.",
            "the curve history: entries {as_of, nss_fit, curve_regime?, forward_path?, "
            "rolldown_carry?}, oldest first, each metric's output as invest-metrics emits it "
            "(docs/3d-rates.md), or flat rows {date, tenor, yield, regime, kind}",
            "invest/yield-curve",
            ("tenor_label", "tenor"), ("date", "date"), ("yield", "yield %"),
            [n("date"), n("tenor_label", "tenor"), q("yield", ".2f", "yield %"), n("regime"),
             q("carry_bps", ".1f", "3m carry bp")],
            "viridis", ".1f", filter_="datum.kind == 'spot'"),
        "reading": "the fitted curves as a surface of lines over tenor and date, each "
                   "coloured by its regime (teal upward, amber flat, red inverted); replays "
                   "curve by curve from its first eight, the curves to come a dim ghost, the "
                   "forward curve beside the last",
        "graphics": {
            "mark": "fc-yield-curve3d",
            "encoding": {"x": {"field": "tenor"}, "z": {"field": "date"},
                         "y": {"field": "yield"}, "color": {"field": "regime"}},
            "replay": "date",
            "colors": REGIMES,
            "extent": [12, 14],
            "height": 8,
            "camera": {"yaw": 10, "pitch": 18, "zoom": 1.35},
            "timeline": replay_timeline(8),
        },
    },
    "credit-spreads": {
        "template": heatmap(
            "credit-spreads",
            "Credit spreads by rating and tenor (fi.spread_suite, fi.spread_pctile): spread in "
            "bps by tenor across and rating down, best rating at the top.",
            "spread outputs: fi.spread_suite and/or fi.spread_pctile outputs as invest-metrics "
            "emits them, inputs carrying rating and years (docs/3d-rates.md), or flat rows "
            "{rating, tenor, spread_bps, pctile}",
            "invest/credit-spreads",
            ("tenor_label", "tenor"), ("rating", "rating"), ("spread_bps", "spread bp"),
            [n("rating"), n("tenor_label", "tenor"), q("spread_bps", ".0f", "spread bp"),
             q("pctile", ".0f", "percentile")],
            "orangered", ".0f"),
        "reading": "spread surface over tenor and rating as a wire mesh, AAA in front, log "
                   "height, coloured by each spread's percentile in its own history (green "
                   "tight, red wide)",
        "graphics": {
            "mark": "fc-credit3d",
            "encoding": {"x": {"field": "tenor"}, "z": {"field": "rating"},
                         "y": {"field": "spread_bps"}, "color": {"field": "pctile"}},
            "scale": "log",
            "camera": {"yaw": 20, "pitch": 30, "zoom": 1.3},
        },
    },
    "cds-term-structure": {
        "template": heatmap(
            "cds-term-structure",
            "A name's CDS term structure by date (credit.cds_isda_implied_hazard_pd per date): "
            "par spread (or hazard rate, or default probability) by tenor across and date down.",
            "the CDS history: credit.cds_isda_implied_hazard_pd outputs, or {input, output} "
            "pairs so the par spreads come too, oldest first (docs/3d-rates.md), or flat rows "
            "{as_of, tenor, value}",
            "invest/cds-term",
            ("tenor_label", "tenor"), ("as_of", "date"), ({"x-eas:slot": "measure"}, "value"),
            [n("as_of", "date"), n("tenor_label", "tenor"), n("maturity"),
             q("spread_bp", ".0f", "par spread bp"), q("hazard_bps", ".0f", "hazard bp"),
             q("pd_pct", ".2f", "cum. PD %")],
            "orangered", ".0f",
            extra_slots={"measure": {
                "type": "string", "default": "value",
                "enum": ["value", "spread_bp", "hazard_bps", "pd_pct", "hazard_rate", "survival",
                         "cumulative_pd", "forward_hazard"],
                "doc": "the field drawn; auto resolves to one concrete field shared by the adapter and chart"}}),
        "reading": "the term structures as a surface of lines over tenor and date, coloured "
                   "by level; replays date by date from its first eight, the dates to come a "
                   "dim ghost, so a stress that flattens the front end stands out",
        "graphics": {
            "mark": "fc-cds-term3d",
            "encoding": {"x": {"field": "tenor"}, "z": {"field": "as_of"},
                         "y": {"field": {"x-eas:slot": "measure"}}},
            "replay": "as_of",
            "extent": [12, 12],
            "height": 8,
            "camera": {"yaw": 10, "pitch": 18, "zoom": 1.35},
            "timeline": replay_timeline(5),
        },
    },
    "key-rate-durations": {
        "template": heatmap(
            "key-rate-durations",
            "Key-rate durations across holdings (fi.krd, fi.dv01): duration at each key rate "
            "by tenor across and holding down, long exposure warm and short cool.",
            "the holdings: entries {holding, krd, dv01?, tenors?}, krd and dv01 the metrics' "
            "outputs as invest-metrics emits them (docs/3d-rates.md), or flat rows "
            "{holding, tenor, krd}",
            "invest/krd",
            ("tenor_label", "key rate"), ("holding", "holding"), ("krd", "KRD"),
            [n("holding"), n("tenor_label", "key rate"), q("krd", ".2f", "KRD"),
             q("duration", ".2f"), q("dv01", ".4f", "DV01 per 100")],
            "redyellowgreen", ".1f", mid=0),
        "reading": "a bar per holding and key rate, as tall as its duration (short exposure "
                   "below the floor), each holding its own colour",
        "graphics": {
            "mark": "fc-krd3d",
            "encoding": {"x": {"field": "tenor"}, "z": {"field": "holding"},
                         "y": {"field": "krd"}},
            "camera": {"yaw": 30, "pitch": 22, "zoom": 1.3},
        },
    },
}

TEMPLATES["cds-term-structure"]["template"]["title"] = {
    "text": {"x-eas:slot": "title"}, "subtitle": {"x-eas:slot": "description"}}


def cds_bond_ladder():
    """Write the CDS implied corporate bond ladder, 2D and 3D."""
    name = "cds-bond-ladder"
    tooltip = [n("name", "bond"), q("coupon", ".2%"), n("maturity"), q("years", ".2f"),
               q("principal", ".0f"), q("treasury_yield", ".3%"), n("treasury_extrapolated"),
               q("cds_spread_bp", ".1f"), n("spread_extrapolated"), q("model_yield", ".3%"),
               q("model_price", ".3f"), q("accrued_interest", ".3f"),
               q("modified_duration", ".3f"), q("dv01_per_100", ".4f"), q("dv01_usd", ".2f"),
               q("cumulative_pd", ".3%"), q("expected_loss_per_100", ".3f"), q("expected_loss_usd", ".2f")]
    base = {
        "$schema": "https://vega.github.io/schema/vega-lite/v6.json",
        "x-eas": {"template": name, "version": "1.0.0",
                   "doc": "CDS-implied corporate bond valuations; model, not traded.",
                   "slots": {
                       "data": {"shape": "invest/cds-bond-ladder", "required": True,
                                "doc": "bond_corp.cds_implied_ladder output"},
                       "title": {"type": "string", "default": "CDS implied bond ladder — model, not traded"},
                       "description": {"type": "string", "default": "CDS implied model, not traded"},
                       "measure": {"type": "string", "default": "model_yield",
                                   "enum": ["model_yield", "model_price", "cds_spread_bp",
                                            "expected_loss_per_100", "dv01_usd"]}},
                   "example": f"../{FIXTURES}/{name}.data.json"},
        "title": {"text": {"x-eas:slot": "title"}, "subtitle": {"x-eas:slot": "description"}},
        "width": "container", "height": 240, "data": {"name": "data"},
        "mark": {"type": "bar", "tooltip": True},
        "encoding": {
            "x": {"field": "name", "type": "nominal",
                  "sort": {"field": "years", "order": "ascending"}, "title": "bond"},
            "y": {"field": {"x-eas:slot": "measure"}, "type": "quantitative"},
            "color": {"field": "spread_extrapolated", "type": "nominal",
                      "scale": {"domain": ["null", "below", "above"],
                                "range": ["#4c9f70", "#e6ab02", "#d95f59"]}},
            "tooltip": tooltip},
    }
    graphics = {"mark": "fc-cds-bond-ladder3d",
                "encoding": {"x": {"field": "years"}, "y": {"field": {"x-eas:slot": "measure"}},
                             "z": {"field": "name"}, "color": {"field": "spread_extrapolated"}},
                "ribbon": "treasury_yield", "extent": [14, 10], "height": 8,
                "camera": {"yaw": 24, "pitch": 24, "zoom": 1.3}}
    dump(os.path.join(ROOT, "templates/financial/cds-bond-ladder.json"), base)
    dump(os.path.join(ROOT, "templates3d/cds-bond-ladder.json"),
         variant(name, {"source": "templates/financial/cds-bond-ladder.json",
                        "example": f"{FIXTURES}/{name}.data.json",
                        "reading": "CDS implied model yields as maturity bars with treasury yields as a ribbon, "
                                   "coloured by CDS spread extrapolation; model, not traded",
                        "graphics": graphics}))


def dump(path, obj):
    with open(path, "w") as f:
        json.dump(obj, f, indent=2)
        f.write("\n")
    print(path)


def main():
    for name, cfg in TEMPLATES.items():
        source = f"templates/financial/{name}.json"
        dump(os.path.join(ROOT, source), cfg["template"])
        dump(os.path.join(ROOT, "templates3d", name + ".json"),
             variant(name, {"source": source, "example": f"{FIXTURES}/{name}.data.json",
                            "reading": cfg["reading"], "graphics": cfg["graphics"]}))
    cds_bond_ladder()


if __name__ == "__main__":
    sys.exit(main())
