#!/usr/bin/env python3
"""Write the portfolio-risk templates: templates/financial/risk-*.json (2D)
and their 3D variants templates3d/risk-*.json, from one definition each.

The 2D template is the fallback eas draws without graphics.el; the 3D
variant is it with its body unchanged plus an x-eas.graphics naming a
mark src/financial-chart-3d-risk.el registers (as scripts/templates3d.py
does for the other templates).  Examples are test/fixtures/3d/risk/
(scripts/fixtures-3d-risk.py); data contracts are docs/3d-risk.md.

    python3 scripts/templates3d-risk.py
"""

import json
import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TIMELINE = {"loop": True, "autoplay": True}
DESCRIPTION_DOC = "the takeaway; alt text and table-view fallback (bin/chart check requires it)"


def time(field, step, mode="cumulative"):
    return {"field": field, "mode": mode, "step": step}


def tooltip(*fields):
    return [dict(f) for f in fields]


def slots(data_doc, description, extra=None):
    out = {
        "data": {"shape": "plist", "required": True, "doc": data_doc},
        "title": {"type": "string", "default": ""},
        "description": {"type": "string", "default": description, "doc": DESCRIPTION_DOC},
    }
    out.update(extra or {})
    return out


def body(height, spec):
    out = {
        "title": {"x-eas:slot": "title"},
        "description": {"x-eas:slot": "description"},
        "width": "container",
        "height": height,
        "data": {"name": "data"},
    }
    out.update(spec)
    return out


DIVERGING = {"scheme": "redyellowgreen", "domainMid": 0}

TEMPLATES = {
    "risk-correlation": {
        "doc": "Rolling correlation matrix: the latest window's pairwise correlations as "
               "coloured cells on a diverging scale, values printed.",
        "data_doc": "rows {window, row, column, value}: one full matrix per window (an "
                    "ISO date or index), windows oldest first; stats.correlation lowered by "
                    "financial-chart-3d-risk-matrix-rows (docs/3d-risk.md)",
        "body": body(200, {
            "transform": [
                {"window": [{"op": "dense_rank", "as": "rank"}], "sort": [{"field": "window", "order": "descending"}]},
                {"filter": "datum.rank == 1"},
            ],
            "encoding": {
                "x": {"field": "column", "type": "nominal", "sort": None, "title": None, "axis": {"orient": "top"}},
                "y": {"field": "row", "type": "nominal", "sort": None, "title": None},
            },
            "layer": [
                {"name": "cells", "mark": "rect",
                 "encoding": {"color": {"field": "value", "type": "quantitative", "title": None,
                                        "scale": {"scheme": "redyellowgreen", "domain": [-1, 1]}},
                              "tooltip": tooltip({"field": "window", "type": "nominal"},
                                                 {"field": "row", "type": "nominal"},
                                                 {"field": "column", "type": "nominal"},
                                                 {"field": "value", "type": "quantitative", "format": ".2f"})}},
                {"name": "values", "mark": {"type": "text", "fontSize": 10},
                 "encoding": {"text": {"field": "value", "type": "quantitative", "format": ".2f"}}},
            ],
        }),
        "reading": "the correlation matrix as signed 3D bars on an asset by asset floor "
                   "(up green for positive, down red for negative), animated window by "
                   "window so the bars rise and sink as correlations shift",
        "graphics": {
            "mark": "fc-risk-grid3d",
            "encoding": {"x": {"field": "column"}, "z": {"field": "row"}, "y": {"field": "value"},
                         "time": dict(time("window", 0.8, "frame"), easing="cubic-in-out")},
            "domain": [-1, 1],
            "diagonal": False,
            "camera": {"yaw": 35, "pitch": 35, "zoom": 1.2},
            "timeline": TIMELINE,
        },
    },
    "risk-contribution": {
        "doc": "Risk contribution over time: each asset's share of portfolio volatility, "
               "stacked to 100%.",
        "data_doc": "rows {date, asset, contribution}: percent of portfolio volatility per "
                    "asset and date (summing to 100 per date), dates oldest first; "
                    "stats.risk_contribution per rebalance date",
        "slots": {"x_type": {"enum": ["quantitative", "temporal"], "default": "temporal",
                             "doc": "temporal when date holds dates or epoch milliseconds"}},
        "body": body(200, {
            "mark": "area",
            "encoding": {
                "x": {"field": "date", "type": {"x-eas:slot": "x_type"}, "title": None},
                "y": {"field": "contribution", "type": "quantitative", "stack": "zero",
                      "title": "% of risk"},
                "color": {"field": "asset", "type": "nominal", "sort": None, "title": None},
                "tooltip": tooltip({"field": "date", "type": {"x-eas:slot": "x_type"}},
                                   {"field": "asset", "type": "nominal"},
                                   {"field": "contribution", "type": "quantitative", "format": ".1f"}),
            },
        }),
        "reading": "one ribbon per asset, its height that asset's share of portfolio "
                   "volatility; replays date by date",
        "graphics": {
            "mark": "fc-risk-ribbons3d",
            "encoding": {"x": {"field": "date"}, "z": {"field": "asset"}, "y": {"field": "contribution"},
                         "time": time("date", 0.25)},
            "camera": {"yaw": 40, "pitch": 30, "zoom": 1.2},
            "timeline": TIMELINE,
        },
    },
    "risk-factor-exposure": {
        "doc": "Factor exposures over time: each factor's rolling regression beta, with "
               "a zero line.",
        "data_doc": "rows {date, factor, beta}: rolling factor betas, dates oldest first; "
                    "stats.factor_regression per window (or port.beta_weighted_delta per "
                    "underlying, as factor)",
        "slots": {"x_type": {"enum": ["quantitative", "temporal"], "default": "temporal",
                             "doc": "temporal when date holds dates or epoch milliseconds"}},
        "body": body(200, {
            "layer": [
                {"name": "zero", "data": {"values": [{"zero": 0}]},
                 "mark": {"type": "rule", "color": "gray", "strokeDash": [2, 2]},
                 "encoding": {"y": {"field": "zero", "type": "quantitative"}}},
                {"name": "betas", "mark": "line",
                 "encoding": {
                     "x": {"field": "date", "type": {"x-eas:slot": "x_type"}, "title": None},
                     "y": {"field": "beta", "type": "quantitative", "title": "beta"},
                     "color": {"field": "factor", "type": "nominal", "sort": None, "title": None},
                     "tooltip": tooltip({"field": "date", "type": {"x-eas:slot": "x_type"}},
                                        {"field": "factor", "type": "nominal"},
                                        {"field": "beta", "type": "quantitative", "format": ".2f"}),
                 }},
            ],
        }),
        "reading": "one ribbon per factor around a zero plane, long exposures standing "
                   "above it and short ones hanging below; replays date by date",
        "graphics": {
            "mark": "fc-risk-ribbons3d",
            "encoding": {"x": {"field": "date"}, "z": {"field": "factor"}, "y": {"field": "beta"},
                         "time": time("date", 0.25)},
            "camera": {"yaw": 40, "pitch": 30, "zoom": 1.2},
            "timeline": TIMELINE,
        },
    },
    "risk-stress": {
        "doc": "Stress scenario grid: each position's P/L under each scenario as "
               "coloured cells on a diverging scale, values printed.",
        "data_doc": "rows {scenario, asset, pnl}: P/L (percent of NAV, or currency) of each "
                    "position under each scenario; port.stress",
        "body": body(200, {
            "encoding": {
                "x": {"field": "asset", "type": "nominal", "sort": None, "title": None, "axis": {"orient": "top"}},
                "y": {"field": "scenario", "type": "nominal", "sort": None, "title": None},
            },
            "layer": [
                {"name": "cells", "mark": "rect",
                 "encoding": {"color": {"field": "pnl", "type": "quantitative", "title": None,
                                        "scale": DIVERGING},
                              "tooltip": tooltip({"field": "scenario", "type": "nominal"},
                                                 {"field": "asset", "type": "nominal"},
                                                 {"field": "pnl", "type": "quantitative", "format": ".2f"})}},
                {"name": "values", "mark": {"type": "text", "fontSize": 10},
                 "encoding": {"text": {"field": "pnl", "type": "quantitative", "format": ".1f"}}},
            ],
        }),
        "reading": "the scenario by position grid as signed 3D bars, losses sinking red "
                   "through the floor and gains rising green, a net column per scenario",
        "graphics": {
            "mark": "fc-risk-grid3d",
            "encoding": {"x": {"field": "asset"}, "z": {"field": "scenario"}, "y": {"field": "pnl"}},
            "net": "net",
            "camera": {"yaw": 30, "pitch": -24, "zoom": 1.35},
        },
    },
    "risk-underwater": {
        "doc": "Underwater curves: each strategy's percent decline from its running high.",
        "data_doc": "rows {date, strategy, equity}: each strategy's equity (NAV, any scale), "
                    "dates oldest first; the drawdown is computed (stats.max_drawdown's "
                    "underwater series)",
        "slots": {"x_type": {"enum": ["quantitative", "temporal"], "default": "temporal",
                             "doc": "temporal when date holds dates or epoch milliseconds"}},
        "body": body(200, {
            "transform": [
                {"window": [{"op": "max", "field": "equity", "as": "peak"}],
                 "groupby": ["strategy"], "sort": [{"field": "date"}], "frame": [None, 0]},
                {"calculate": "datum.equity / datum.peak - 1", "as": "drawdown"},
            ],
            "mark": "line",
            "encoding": {
                "x": {"field": "date", "type": {"x-eas:slot": "x_type"}, "title": None},
                "y": {"field": "drawdown", "type": "quantitative", "title": "drawdown",
                      "axis": {"format": ".0%"}},
                "color": {"field": "strategy", "type": "nominal", "sort": None, "title": None},
                "tooltip": tooltip({"field": "date", "type": {"x-eas:slot": "x_type"}},
                                   {"field": "strategy", "type": "nominal"},
                                   {"field": "equity", "type": "quantitative"},
                                   {"field": "drawdown", "type": "quantitative", "format": ".1%"}),
            },
        }),
        "reading": "one slab per strategy hanging below its own high-water line, a "
                   "ridgeline as deep as each drawdown, the worst point of each marked; "
                   "replays week by week",
        "graphics": {
            "mark": "fc-risk-ribbons3d",
            "encoding": {"x": {"field": "date"}, "z": {"field": "strategy"}, "y": {"field": "equity"},
                         "time": time("date", 0.08)},
            "transform": "underwater",
            "stack": 0.9,
            "camera": {"yaw": 30, "pitch": 22, "zoom": 1.2},
            "timeline": TIMELINE,
        },
    },
    "risk-return-distribution": {
        "doc": "Return distributions across horizons: the share of returns in each bin, "
               "one curve per horizon.",
        "data_doc": "rows {horizon, return}: return samples (percent) per horizon label, "
                    "horizons shortest first; the samples stats.var / stats.var_cvar.* "
                    "read",
        "slots": {"bins": {"type": "integer", "default": 17, "doc": "return bins, shared by every horizon"}},
        "body": body(200, {
            "transform": [
                {"bin": {"maxbins": {"x-eas:slot": "bins"}}, "field": "return", "as": "bin"},
                {"aggregate": [{"op": "count", "as": "count"}], "groupby": ["horizon", "bin"]},
                {"joinaggregate": [{"op": "sum", "field": "count", "as": "total"}], "groupby": ["horizon"]},
                {"calculate": "datum.count / datum.total", "as": "share"},
            ],
            "mark": {"type": "line", "interpolate": "step"},
            "encoding": {
                "x": {"field": "bin", "type": "quantitative", "title": "return %"},
                "y": {"field": "share", "type": "quantitative", "title": "share",
                      "axis": {"format": ".0%"}},
                "color": {"field": "horizon", "type": "nominal", "sort": None, "title": None},
                "tooltip": tooltip({"field": "horizon", "type": "nominal"},
                                   {"field": "bin", "type": "quantitative", "title": "return %"},
                                   {"field": "share", "type": "quantitative", "format": ".1%"}),
            },
        }),
        "reading": "one histogram per horizon in lanes from front to back on shared "
                   "return bins, loss bins red and gain bins green, each lane's 95% VaR "
                   "ruled across it; builds horizon by horizon",
        "graphics": {
            "mark": "fc-risk-histogram3d",
            "bins": {"x-eas:slot": "bins"},
            "level": 0.95,
            "encoding": {"x": {"field": "return"}, "z": {"field": "horizon"},
                         "time": time("horizon", 0.7)},
            "spacing": 3,
            "height": 2.4,
            "camera": {"yaw": 8, "pitch": 45, "zoom": 1.3},
            "timeline": TIMELINE,
        },
    },
    "risk-frontier": {
        "doc": "Efficient frontiers: return against volatility, one curve per date.",
        "data_doc": "rows {date, vol, ret, sharpe}: points along each date's efficient "
                    "frontier (volatility and return in percent, Sharpe ratio), the same "
                    "number per date, dates oldest first; port.frontier (requested, "
                    "docs/3d-risk.md)",
        "body": body(200, {
            "mark": "line",
            "encoding": {
                "x": {"field": "vol", "type": "quantitative", "title": "volatility %",
                      "scale": {"zero": False}},
                "y": {"field": "ret", "type": "quantitative", "title": "return %",
                      "scale": {"zero": False}},
                "color": {"field": "date", "type": "nominal", "sort": None, "title": None},
                "order": {"field": "ret", "type": "quantitative"},
                "tooltip": tooltip({"field": "date", "type": "nominal"},
                                   {"field": "vol", "type": "quantitative", "format": ".2f"},
                                   {"field": "ret", "type": "quantitative", "format": ".2f"},
                                   {"field": "sharpe", "type": "quantitative", "format": ".2f"}),
            },
        }),
        "reading": "the frontiers as one surface of return over volatility (across) "
                   "and date (back), coloured by Sharpe, each date's maximum-Sharpe "
                   "portfolio marked; grows date by date",
        "graphics": {
            "mark": "fc-risk-frontier3d",
            "encoding": {"x": {"field": "vol"}, "y": {"field": "ret"}, "z": {"field": "date"},
                         "color": {"field": "sharpe"}, "time": time("date", 0.6)},
            "camera": {"yaw": -35, "pitch": 28, "zoom": 1.12},
            "timeline": TIMELINE,
        },
    },
}


def template(name, cfg, example):
    x_eas = {"template": name, "version": "1.0.0", "doc": cfg["doc"],
             "slots": slots(cfg["data_doc"], cfg["doc"], cfg.get("slots")),
             "example": example}
    out = {"$schema": "https://vega.github.io/schema/vega-lite/v6.json", "x-eas": x_eas}
    out.update(cfg["body"])
    return out


def variant(name, cfg):
    out = template(name, cfg, f"test/fixtures/3d/risk/{name}.json")
    x_eas = out["x-eas"]
    x_eas["doc"] = (f"3D variant of financial-chart's {name} (graphics.el, mark "
                    f"{cfg['graphics']['mark']}): {cfg['reading']}.  2D: {cfg['doc']}")
    x_eas["base"] = name
    x_eas["graphics"] = cfg["graphics"]
    return out


def dump(path, obj):
    with open(path, "w") as f:
        json.dump(obj, f, indent=2)
        f.write("\n")
    print(path)


def main():
    for name, cfg in TEMPLATES.items():
        dump(os.path.join(ROOT, "templates", "financial", name + ".json"),
             template(name, cfg, f"../test/fixtures/3d/risk/{name}.json"))
        dump(os.path.join(ROOT, "templates3d", name + ".json"), variant(name, cfg))


if __name__ == "__main__":
    sys.exit(main())
