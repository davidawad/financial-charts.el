#!/usr/bin/env python3
"""Write test/fixtures/invest-metrics/runs.json: real invest-metrics output.

The golden cases beside it (copied from the dotfiles repository) pin a few
JSON pointers of each output.  The 3D adapters read whole outputs, so this
runs the metrics themselves:

- cases: every golden case of a metric `financial-chart-3d-metric-templates'
  lists, run on its own input (test/financial-chart-3d-metrics-test.el
  checks each against the golden's pointers);
- series: hand-chosen inputs run as the invest lane's boards run them, one
  entry per date (or tenor, holding, scenario...), and the `--grid' runs of
  eq.dcf and stats.kelly.growth, so every template has a series to draw.

Usage, from a dotfiles checkout at the goldens' revision (README.md):

  cp scripts/invest-metrics-runall.rs DOTFILES/src/rust/crates/invest-metrics/examples/runall.rs
  (cd DOTFILES/src/rust && cargo build --example runall -p invest-metrics)
  RUNALL=DOTFILES/src/rust/target/debug/examples/runall scripts/invest-metrics-runs.py
"""

import glob
import json
import math
import os
import subprocess

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DIR = os.path.join(ROOT, "test", "fixtures", "invest-metrics")
RUNALL = os.environ.get("RUNALL", "runall")
REVISION = "ead6f883ea7"

# The metrics the adapters take: their golden cases are rerun whole.
METRICS = {
    "stats.rolling.correlation", "stats.rolling.factor_regression", "stats.risk_contribution",
    "port.stress", "port.beta_weighted_delta", "stats.var", "stats.var_cvar.historical",
    "stats.var_cvar.parametric", "stats.max_drawdown", "port.frontier",
    "opt.greeks_grid", "opt.payoff_grid", "opt.pop_grid",
    "opt.chain_stats", "opt.gex", "opt.max_pain", "opt.skew_rr25_bf25", "opt.vrp",
    "fi.nss_fit", "fi.curve_regime", "fi.spread_suite", "fi.spread_pctile", "fi.krd",
    "ta.rel_strength", "stats.kelly.growth", "fut.term_structure",
}


def lcg(seed):
    """A deterministic stream of uniforms in (0, 1)."""
    state = seed
    while True:
        state = (state * 6364136223846793005 + 1442695040888963407) % (1 << 64)
        yield ((state >> 11) + 0.5) / (1 << 53)


def normals(seed, n):
    u = lcg(seed)
    out = []
    while len(out) < n:
        a, b = next(u), next(u)
        out.append(math.sqrt(-2 * math.log(a)) * math.cos(2 * math.pi * b))
    return out


def dates(start_month, n, day=28, year=2026):
    return ["%d-%02d-%02d" % (year + (start_month - 1 + i) // 12, (start_month - 1 + i) % 12 + 1, day)
            for i in range(n)]


def days(n, start=1, month=9):
    """n weekdays from 2026-MONTH-START, ISO."""
    import datetime
    d = datetime.date(2026, month, start)
    out = []
    while len(out) < n:
        if d.weekday() < 5:
            out.append(d.isoformat())
        d += datetime.timedelta(days=1)
    return out


def fi_input(**over):
    """The fi golden's input with OVER replacing fields."""
    base = next(c["input"] for c in json.load(open(os.path.join(DIR, "fi.json")))["cases"]
                if c["metric"] == "fi.nss_fit")
    return dict(base, **over)


def series():
    """[(metric, name, args, [(stamp, input, grid)])]."""
    out = []
    # risk
    labels = ["SPY", "TLT", "GLD", "QQQ"]
    rets = {k: normals(11 + i, 18) for i, k in enumerate(labels)}
    mixed = {"SPY": [0.01 * a for a in rets["SPY"]],
             "QQQ": [0.012 * (0.8 * a + 0.6 * b) for a, b in zip(rets["SPY"], rets["QQQ"])],
             "TLT": [0.008 * (-0.3 * a + 0.95 * b) for a, b in zip(rets["SPY"], rets["TLT"])],
             "GLD": [0.009 * b for b in rets["GLD"]]}
    ds = days(18, 1, 6)
    out.append(("stats.rolling.correlation", "four assets, 8-day windows every 2 days", {},
                [({}, {"dates": ds, "series": [{"label": k, "values": mixed[k]} for k in labels],
                       "window": 8, "step": 2}, None)]))
    factors = ["MKT", "SMB", "HML"]
    f = {k: [0.01 * x for x in normals(31 + i, 18)] for i, k in enumerate(factors)}
    noise = normals(41, 18)
    port = [0.0002 + 1.1 * m + (0.4 - 0.05 * i) * s + (-0.3 + 0.06 * i) * h + 0.001 * e
            for i, (m, s, h, e) in enumerate(zip(f["MKT"], f["SMB"], f["HML"], noise))]
    out.append(("stats.rolling.factor_regression", "three factors, 8-day windows", {},
                [({}, {"dates": ds, "returns": port,
                       "factors": [{"label": k, "values": f[k]} for k in factors], "window": 8}, None)]))
    cov = [[0.04, 0.006, 0.002, 0.03], [0.006, 0.0144, 0.001, 0.004],
           [0.002, 0.001, 0.0225, 0.002], [0.03, 0.004, 0.002, 0.0484]]
    weights = [[0.4, 0.3, 0.1, 0.2], [0.35, 0.35, 0.1, 0.2], [0.3, 0.35, 0.15, 0.2],
               [0.3, 0.3, 0.15, 0.25], [0.25, 0.35, 0.2, 0.2]]
    out.append(("stats.risk_contribution", "four assets, quarterly weights", {"labels": labels},
                [({"as_of": d}, {"weights": w, "covariance": cov}, None)
                 for d, w in zip(dates(1, 5, 28, 2025), weights)]))
    exposures = [("equity", 0.55), ("bonds", 0.3), ("gold", 0.1), ("credit", 0.05)]
    scen = {"2008 GFC": [-0.45, 0.12, 0.05, -0.25], "2022 Rates": [-0.2, -0.17, -0.01, -0.12],
            "2020 Covid": [-0.34, 0.08, 0.03, -0.18], "Soft landing": [0.12, 0.04, 0.02, 0.05]}
    out.append(("port.stress", "four scenarios over four exposures", {},
                [({"scenario": s}, {"exposures": [{"asset": a, "weight": w, "scenario_return": r}
                                                  for (a, w), r in zip(exposures, rs)]}, None)
                 for s, rs in scen.items()]))
    out.append(("port.beta_weighted_delta", "three positions, monthly", {},
                [({"as_of": d}, {"positions": [
                    {"asset": "SPY calls", "quantity": 10 + 2 * i, "multiplier": 100, "delta": 0.5 + 0.05 * i,
                     "underlying_price": 580 + 5 * i, "beta": 1.0},
                    {"asset": "NVDA puts", "quantity": -4, "multiplier": 100, "delta": -0.35 - 0.03 * i,
                     "underlying_price": 140 - 2 * i, "beta": 1.8},
                    {"asset": "TLT stock", "quantity": 300 - 50 * i, "multiplier": 1, "delta": 1.0,
                     "underlying_price": 92, "beta": -0.3}]}, None)
                 for i, d in enumerate(dates(6, 5))]))
    z = normals(51, 120)
    horizons = [("1d", 1, 60), ("5d", 5, 60), ("21d", 21, 60)]
    out.append(("stats.var", "return samples over three horizons", {},
                [({"horizon": h}, {"returns": [round(0.0004 * n + 0.011 * math.sqrt(n) * x, 6)
                                               for x in z[k * 20:k * 20 + c]], "confidence": 0.95}, None)
                 for k, (h, n, c) in enumerate(horizons)]))
    for metric in ("stats.var_cvar.historical", "stats.var_cvar.parametric"):
        out.append((metric, "return samples over three horizons", {},
                    [({"horizon": h}, {"returns": [round(0.0004 * n + 0.011 * math.sqrt(n) * x, 6)
                                                   for x in z[k * 20:k * 20 + c]], "alpha": 0.95}, None)
                     for k, (h, n, c) in enumerate(horizons)]))
    strategies = {"Trend": (0.002, 0.012, 61), "Carry": (0.001, 0.006, 62), "Vol premium": (0.0015, 0.009, 63)}
    out.append(("stats.max_drawdown", "three strategies, 40 periods", {},
                [({"strategy": s}, {"returns": [round(m + v * x, 6) for x in normals(seed, 40)]}, None)
                 for s, (m, v, seed) in strategies.items()]))
    fz = {k: normals(71 + i, 16) for i, k in enumerate("ABC")}
    fr = {"A": [0.006 + 0.03 * x for x in fz["A"]], "B": [0.003 + 0.01 * x for x in fz["B"]],
          "C": [0.009 + 0.05 * (0.5 * a + 0.85 * x) for a, x in zip(fz["A"], fz["C"])]}
    out.append(("port.frontier", "as-of series: three assets, 10-period windows, 8 points", {},
                [({}, {"dates": days(16, 1, 3), "series": [{"label": k, "values": v} for k, v in fr.items()],
                       "window": 10, "annualization": 12, "points": 8, "long_only": True,
                       "output_scale": 100}, None)]))
    # positioning
    spot = 586.15
    strikes = [560, 570, 580, 590, 600, 610]
    expiries = ["2026-11-06", "2026-11-20", "2026-12-18"]
    oi = lambda e, k, side: int(800 + 4000 * math.exp(-((k - (595 if side == "call" else 575)) / (15 + 8 * e)) ** 2))
    chain = [({"expiry": x}, {"strikes": strikes, "call_open_interest": [oi(e, k, "call") for k in strikes],
                              "put_open_interest": [oi(e, k, "put") for k in strikes]}, None)
             for e, x in enumerate(expiries)]
    out.append(("opt.max_pain", "three expiries, six strikes", {"spot": spot}, chain))
    out.append(("opt.chain_stats", "three expiries, six strikes", {"spot": spot},
                [(s, dict(i, ivs=[round(0.19 + 0.0008 * (590 - k) + 0.01 * e, 4) for k in strikes]), g)
                 for e, (s, i, g) in enumerate(chain)]))
    gamma = lambda e, k: round(0.012 * math.exp(-((k - spot) / (12 + 6 * e)) ** 2) / (1 + e), 6)
    out.append(("opt.gex", "two expiries, calls and puts, dealer short", {},
                [({"expiry": x, "side": side},
                  {"underlying_price": spot, "strikes": strikes, "gamma": [gamma(e, k) for k in strikes],
                   "open_interest": [oi(e, k, side) for k in strikes],
                   "dealer_position": "long" if side == "call" else "short"}, None)
                 for e, x in enumerate(expiries[:2]) for side in ("call", "put")]))
    out.append(("opt.skew_rr25_bf25", "five weekly snapshots, four tenors", {},
                [({"date": d, "tenor": t},
                  {"atm_iv": round(0.16 + 0.01 * i + 0.0001 * t, 4),
                   "call_25d_iv": round(0.15 + 0.008 * i + 0.00012 * t, 4),
                   "put_25d_iv": round(0.19 + 0.015 * i + 0.00005 * t - 0.002 * (t > 60), 4)}, None)
                 for i, d in enumerate(dates(9, 5, 4)[:1] + days(4, 11, 9)) for t in (7, 30, 60, 90)]))
    vd = days(20, 1, 10)
    out.append(("opt.vrp", "twenty trading days", {},
                [({"date": d}, {"implied_vol": round(0.17 + 0.06 * math.exp(-((i - 6) / 3.0) ** 2), 4),
                                "realized_vol": round(0.15 + 0.07 * math.exp(-((i - 10) / 3.5) ** 2), 4)}, None)
                 for i, d in enumerate(vd)]))
    # rates
    years = [0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30]
    curves = {"2023-06-30": [0.0543, 0.0547, 0.054, 0.0487, 0.0452, 0.0413, 0.0393, 0.0381, 0.0406, 0.0385],
              "2024-06-28": [0.0548, 0.0533, 0.0509, 0.0471, 0.0455, 0.0433, 0.0433, 0.0436, 0.0461, 0.0451],
              "2025-06-30": [0.0435, 0.0427, 0.0409, 0.0372, 0.0368, 0.0379, 0.0398, 0.0424, 0.0478, 0.0478],
              "2026-06-30": [0.0372, 0.0368, 0.0361, 0.0352, 0.0355, 0.0369, 0.0386, 0.0409, 0.0461, 0.0465]}
    for metric in ("fi.nss_fit", "fi.curve_regime"):
        out.append((metric, "four quarter-end treasury curves", {},
                    [({"as_of": d}, fi_input(curve_years=years, curve_rates=r), None) for d, r in curves.items()]))
    hist = [60 + 40 * math.sin(i / 5.0) for i in range(40)]
    spreads = [(rating, y, round(base * (0.6 + 0.12 * y ** 0.6), 1))
               for rating, base in (("AAA", 35), ("A", 70), ("BBB", 120), ("BB", 260))
               for y in (2, 5, 10)]
    out.append(("fi.spread_suite", "four ratings by three tenors", {},
                [({}, fi_input(rating=r, years=float(y), spread_bps=s), None) for r, y, s in spreads]))
    out.append(("fi.spread_pctile", "four ratings by three tenors", {},
                [({}, fi_input(rating=r, years=float(y), spread_bps=s,
                               spread_history_bps=[round(h * s / 60, 1) for h in hist]), None)
                 for r, y, s in spreads]))
    keys = [0.5, 2, 5, 10, 20, 30]
    holdings = {"UST 2Y": [0.3, 1.6, 0.0, 0.0, 0.0, 0.0], "UST 10Y": [0.05, 0.2, 0.8, 7.1, 0.0, 0.0],
                "UST 30Y": [0.01, 0.05, 0.18, 0.7, 4.9, 10.6], "Payer swap 10Y": [0.0, -0.1, -0.6, -7.4, 0.0, 0.0]}
    out.append(("fi.krd", "four holdings at six key rates", {},
                [({"holding": h}, fi_input(curve_years=keys, curve_rates=[0.04] * 6, key_rate_shocks=s), None)
                 for h, s in holdings.items()]))
    cds = json.load(open(os.path.join(ROOT, "test", "fixtures", "3d", "rates", "cds-term-structure.data.json")))
    out.append(("credit.cds_isda_implied_hazard_pd", "the rates fixture's CDS curves", {},
                [({}, e["input"], None) for e in cds["data"]]))
    # market
    bench = [100 * math.exp(0.001 * i + 0.01 * x) for i, x in enumerate(normals(81, 30))]
    sym = {"XLK": (0.003, 0.0), "XLE": (-0.002, 1.0), "XLV": (0.0005, 2.0), "XLF": (0.001, 3.0)}
    out.append(("ta.rel_strength", "four sectors against the index, 30 days", {"dates": days(30, 1, 9)},
                [({"symbol": s}, {"closes": [round(b * math.exp(d * i + 0.03 * math.sin(i / 4.0 + p)), 4)
                                             for i, b in enumerate(bench)],
                                  "benchmark_closes": [round(b, 4) for b in bench]}, None)
                 for s, (d, p) in sym.items()]))
    bd = dates(5, 6)
    entries = []
    for s, (phase, seed) in {"Tech": (0.0, 91), "Energy": (2.0, 92), "Health": (4.0, 93),
                             "Financials": (1.0, 94)}.items():
        members = []
        for m in range(6):
            walk, logp = normals(seed * 10 + m, 260), 0.0
            closes = []
            for n, x in enumerate(walk):
                logp += 0.008 * x
                closes.append(100 * math.exp(logp + 0.12 * math.sin(n / 18.0 + phase + 0.4 * m)))
            members.append(closes)
        for j, d in enumerate(bd):
            entries.append(({"date": d, "sector": s},
                            {"constituent_closes": [[round(p, 3) for p in c[:206 + 10 * j]] for c in members]}, None))
    out.append(("market.breadth_pct_above_200d", "four sectors, six months", {"drop_input": True}, entries))
    # valuation
    dcf = {"historical_fcf": [5.0, 5.4], "projection_years": 5, "growth_rate": 0.08, "shares_outstanding": 1.0,
           "cash_and_equivalents": 0.0, "total_debt": 12.0, "discount_rate": 0.0, "terminal_growth_rate": 0.0}
    axes = ["wacc@discount_rate=0.07:0.11:0.005", "terminal_growth@terminal_growth_rate=0.01:0.04:0.005"]
    out.append(("eq.dcf", "two as-of runs of the WACC x terminal growth grid", {},
                [({"as_of": d, "price": p}, dict(dcf, historical_fcf=[5.0, fcf]),
                  {"axes": axes, "picks": ["value_per_share@dcf_intrinsic_value_per_share"],
                   "stamp": {"price": p, "as_of": d}})
                 for d, fcf, p in (("2026-06-30", 5.4, 118.6), ("2026-09-30", 5.9, 141.2))]))
    out.append(("stats.kelly.growth", "continuous, sigma 0.2", {},
                [({}, {"model": "continuous", "sigma": 0.2},
                  {"axes": ["edge=0.01:0.12:0.01", "fraction=0:3:0.25"], "picks": ["growth"], "stamp": {}})]))
    out.append(("stats.kelly.growth", "binary, even odds", {},
                [({}, {"model": "binary", "odds": 1.0},
                  {"axes": ["edge=0.01:0.12:0.01", "fraction=0:0.95:0.05"], "picks": ["growth"], "stamp": {}})]))
    # vol surface
    snaps = []
    for t, (time, s0) in enumerate((("2026-10-30T10:00:00", 581.4), ("2026-10-30T12:30:00", 578.9),
                                    ("2026-10-30T15:30:00", 586.15))):
        contracts = [{"days_to_expiry": d, "strike": float(k),
                      "implied_volatility": round(0.15 + 0.012 * t + 0.6 * (k / s0 - 1) ** 2
                                                  - 0.25 * (k / s0 - 1) + 0.02 * math.sqrt(30.0 / d), 4)}
                     for d in (9, 23, 51, 79) for k in range(550, 626, 5)]
        snaps.append(({"time": time}, {"underlying_price": s0, "contracts": contracts}, None))
    out.append(("opt.iv_surface", "three intraday snapshots", {"drop_input": True}, snaps))
    # Futures term structure: a synthetic 3-snapshot curve for the replay.
    futures = []
    for time, prices in (("2026-10-30T10:00:00", [101.0, 104.0]),
                         ("2026-10-30T12:00:00", [101.1, 104.2]),
                         ("2026-10-30T15:30:00", [100.8, 99.5])):
        inp = {"as_of": "2026-10-30", "spot": 100.0, "rate": 0.04,
               "contracts": [{"symbol": "F1", "expiry": "2026-11-29", "price": prices[0]},
                             {"symbol": "F2", "expiry": "2027-01-28", "price": prices[1]}]}
        futures.append(({"time": time}, inp, None))
    out.append(("fut.term_structure", "three futures curve snapshots", {"drop_input": True}, futures))
    return out


def main():
    cases = [c for f in sorted(glob.glob(os.path.join(DIR, "*.json"))) if not f.endswith("runs.json")
             for c in json.load(open(f))["cases"]
             if c["metric"] in METRICS and "expect_error" not in c and c.get("expect") is not None]
    groups = series()
    requests = [{"metric": c["metric"], "input": c["input"]} for c in cases]
    for metric, _, _, entries in groups:
        for _, inp, grid in entries:
            requests.append(dict({"metric": metric, "input": inp}, **({"grid": grid} if grid else {})))
    done = subprocess.run([RUNALL], input=json.dumps(requests), capture_output=True, text=True, check=True)
    results = iter(json.loads(done.stdout))

    def take(what):
        r = next(results)
        if "error" in r:
            raise SystemExit("%s: %s" % (what, r["error"]))
        return r["output"]

    doc = {"source": "invest-metrics at %s (scripts/invest-metrics-runs.py)" % REVISION,
           "cases": [{"metric": c["metric"], "name": c["name"], "input": c["input"], "output": take(c["name"])}
                     for c in cases],
           "series": []}
    for metric, name, args, entries in groups:
        drop = args.pop("drop_input", False)
        doc["series"].append({"metric": metric, "name": name, "args": args,
                              "entries": [dict(stamp, **({} if drop else {"input": inp}), output=take(name))
                                          for stamp, inp, _ in entries]})
    with open(os.path.join(DIR, "runs.json"), "w") as f:
        json.dump(doc, f, separators=(",", ":"))
        f.write("\n")


if __name__ == "__main__":
    main()
