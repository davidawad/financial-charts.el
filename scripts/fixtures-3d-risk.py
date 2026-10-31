#!/usr/bin/env python3
"""Write test/fixtures/3d/risk/: the examples of the portfolio-risk templates.

Hand-derived, deterministic data in the shapes docs/3d-risk.md specifies
(the rows the invest-metrics metrics lower to).  Correlations and risk
contributions come from one small factor model, so every correlation
matrix is positive semi-definite and every contribution row sums to 100;
the rest is closed-form or a seeded random walk.

    python3 scripts/fixtures-3d-risk.py

Rerun after changing it, then `make golden-3d'.
"""

import json
import math
import os
import random

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "test", "fixtures", "3d", "risk")


def month_ends(start_year, start_month, n):
    """N month-end dates (ISO strings) from START_YEAR-START_MONTH."""
    out = []
    y, m = start_year, start_month
    for _ in range(n):
        ny, nm = (y + 1, 1) if m == 12 else (y, m + 1)
        # Days in month y-m: the day before the 1st of the next one.
        days = [31, 29 if (y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)) else 28,
                31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1]
        out.append(f"{y:04d}-{m:02d}-{days:02d}")
        y, m = ny, nm
    return out


def write(name, title, key, rows, extra=None):
    os.makedirs(OUT, exist_ok=True)
    path = os.path.join(OUT, name + ".json")
    with open(path, "w") as f:
        f.write("{\n")
        f.write(f'  "title": {json.dumps(title)},\n')
        for k, v in (extra or {}).items():
            f.write(f'  {json.dumps(k)}: {json.dumps(v)},\n')
        f.write(f'  {json.dumps(key)}: [\n')
        f.write(",\n".join("    " + json.dumps(r, separators=(", ", ": ")) for r in rows))
        f.write("\n  ]\n}\n")
    print(path)


def r(v, d=4):
    return round(v + 0.0, d)


# -- the factor model ---------------------------------------------------
# Three factors (equity, rates, real assets), annualised vols; each
# asset's loadings and idiosyncratic vol, some drifting with time t in
# [0, 1]: the stock-bond correlation turns positive in an inflation
# scare mid-sample and back as it fades.

ASSETS = ["SPY", "QQQ", "TLT", "GLD", "HYG"]


def scare(t):
    """0..1..0: the inflation scare peaks a little after mid-sample."""
    return math.exp(-((t - 0.55) / 0.2) ** 2)


def loadings(t):
    s = scare(t)
    return {
        "SPY": ([1.00, 0.00, 0.05], 0.03),
        "QQQ": ([1.15, -0.10 - 0.25 * s, 0.00], 0.06),
        "TLT": ([-0.35 + 0.75 * s, 1.00, 0.10], 0.03),
        "GLD": ([0.08 + 0.10 * s, 0.25 - 0.20 * s, 1.00], 0.06),
        "HYG": ([0.42 + 0.10 * s, 0.30, 0.02], 0.025),
    }


def factor_vols(t):
    s = scare(t)
    return [0.15 + 0.08 * s, 0.09 + 0.05 * s, 0.14]


def covariance(t, assets):
    load = loadings(t)
    fv = factor_vols(t)
    cov = {}
    for a in assets:
        for b in assets:
            la, ea = load[a]
            lb, eb = load[b]
            c = sum(la[k] * lb[k] * fv[k] ** 2 for k in range(3))
            if a == b:
                c += ea ** 2
            cov[a, b] = c
    return cov


def correlation_fixture():
    windows = month_ends(2025, 11, 12)
    rows = []
    for i, w in enumerate(windows):
        t = i / (len(windows) - 1)
        cov = covariance(t, ASSETS)
        for a in ASSETS:
            for b in ASSETS:
                rows.append({"window": w, "row": a, "column": b,
                             "value": r(cov[a, b] / math.sqrt(cov[a, a] * cov[b, b]), 3)})
    write("risk-correlation", "Rolling 63-day correlation, month ends", "data", rows)


WEIGHTS = {"SPY": 0.35, "QQQ": 0.15, "TLT": 0.25, "GLD": 0.15, "HYG": 0.10}


def contribution_fixture():
    dates = month_ends(2024, 11, 24)
    rows = []
    for i, d in enumerate(dates):
        t = i / (len(dates) - 1)
        cov = covariance(t, ASSETS)
        sw = {a: sum(cov[a, b] * WEIGHTS[b] for b in ASSETS) for a in ASSETS}
        var = sum(WEIGHTS[a] * sw[a] for a in ASSETS)
        parts = [r(100 * WEIGHTS[a] * sw[a] / var, 2) for a in ASSETS]
        # Rounded to two places, they still sum to exactly 100.
        parts[0] = r(100 - sum(parts[1:]), 2)
        for a, p in zip(ASSETS, parts):
            rows.append({"date": d, "asset": a, "contribution": p})
    write("risk-contribution", "Contribution to portfolio volatility, %", "data", rows)


def exposure_fixture():
    dates = month_ends(2024, 11, 24)
    rng = random.Random(11)
    rows = []
    for i, d in enumerate(dates):
        t = i / (len(dates) - 1)
        s = scare(t)
        betas = {
            "Mkt-RF": 1.04 - 0.18 * s + 0.02 * math.sin(7 * t),
            "SMB": 0.22 - 0.38 * t,
            "HML": -0.28 + 0.45 * s,
            "UMD": 0.10 + 0.25 * math.sin(math.pi * t),
            "QMJ": 0.18 + 0.12 * s,
        }
        for f, b in betas.items():
            rows.append({"date": d, "factor": f, "beta": r(b + rng.gauss(0, 0.015), 3)})
    write("risk-factor-exposure", "Rolling 126-day factor betas (FF5 + momentum)", "data", rows)


SHOCKS = [
    # scenario, shock per asset (fractional return)
    ("2008 GFC", {"SPY": -0.51, "QQQ": -0.49, "TLT": 0.26, "GLD": 0.05, "HYG": -0.30}),
    ("2020 Covid", {"SPY": -0.34, "QQQ": -0.28, "TLT": 0.21, "GLD": -0.03, "HYG": -0.21}),
    ("2022 Rates", {"SPY": -0.25, "QQQ": -0.35, "TLT": -0.31, "GLD": -0.08, "HYG": -0.15}),
    ("Rates +100bp", {"SPY": -0.06, "QQQ": -0.09, "TLT": -0.17, "GLD": -0.04, "HYG": -0.05}),
    ("Equity -20%", {"SPY": -0.20, "QQQ": -0.24, "TLT": 0.06, "GLD": 0.03, "HYG": -0.09}),
    ("USD +10%", {"SPY": -0.04, "QQQ": -0.05, "TLT": 0.02, "GLD": -0.09, "HYG": -0.02}),
    ("Oil +50%", {"SPY": -0.07, "QQQ": -0.10, "TLT": -0.05, "GLD": 0.08, "HYG": -0.04}),
]


def stress_fixture():
    rows = []
    for scenario, shock in SHOCKS:
        for a in ASSETS:
            rows.append({"scenario": scenario, "asset": a, "pnl": r(100 * WEIGHTS[a] * shock[a], 2)})
    write("risk-stress", "Stress scenarios, P/L % of NAV by position", "data", rows)


def underwater_fixture():
    rng = random.Random(5)
    weeks = 104
    # date strings, weekly Fridays from 2024-11-01
    import datetime
    start = datetime.date(2024, 11, 1)
    dates = [(start + datetime.timedelta(weeks=i)).isoformat() for i in range(weeks)]
    strategies = {
        # drift, vol per week; shocks {week: return}
        "Trend": (0.0022, 0.022, {30: -0.035, 31: -0.04, 32: -0.03, 33: -0.02, 70: 0.03}),
        "Carry": (0.0018, 0.010, {58: -0.05, 59: -0.04, 60: -0.025}),
        "Vol premium": (0.0030, 0.007, {44: -0.11, 45: -0.06, 46: 0.02}),
        "60/40": (0.0012, 0.015, {50: -0.03, 51: -0.035, 52: -0.02, 53: -0.025, 54: -0.015}),
    }
    rows = []
    for name, (mu, sd, shocks) in strategies.items():
        nav = 100.0
        for i, d in enumerate(dates):
            if i > 0:
                nav *= 1 + mu + rng.gauss(0, sd) + shocks.get(i, 0.0)
            rows.append({"date": d, "strategy": name, "equity": r(nav, 2)})
    # rows by date, strategies within each: one replay keyframe per date
    rows.sort(key=lambda row: (row["date"], list(strategies).index(row["strategy"])))
    write("risk-underwater", "Underwater: drawdown from the running high, by strategy", "data", rows)


def distribution_fixture():
    rng = random.Random(3)
    # Two years of daily portfolio returns (percent): calm and stressed
    # regimes (a fat-tailed mixture) with a little positive drift.
    days = 520
    daily = []
    for i in range(days):
        stressed = 260 <= i < 300
        sd = 1.9 if stressed else 0.75
        shock = rng.gauss(0, sd) if rng.random() > 0.03 else rng.gauss(-1.5, 2.5)
        daily.append(0.035 + shock)
    rows = []
    for label, h in (("1d", 1), ("5d", 5), ("21d", 21), ("63d", 63)):
        # Overlapping h-day compounded returns, 200 evenly spaced ends.
        ends = [h + round(k * (days - h) / 199) for k in range(200)]
        for e in ends:
            g = 1.0
            for x in daily[e - h:e]:
                g *= 1 + x / 100
            rows.append({"horizon": label, "return": r(100 * (g - 1), 3)})
    write("risk-return-distribution", "Portfolio return distribution by horizon, %", "data", rows)


def frontier_fixture():
    quarters = ["2024-12-31", "2025-03-31", "2025-06-30", "2025-09-30",
                "2025-12-31", "2026-03-31", "2026-06-30", "2026-09-30"]
    rf = [4.3, 4.3, 4.2, 4.0, 3.7, 3.5, 3.4, 3.3]
    rows = []
    for q, (date, rate) in enumerate(zip(quarters, rf)):
        t = q / (len(quarters) - 1)
        s = scare(t)
        mu_min = 5.0 + 0.6 * t          # return of the minimum-variance portfolio
        vol_min = 6.0 + 2.5 * s         # its volatility
        k = 0.55 - 0.2 * s              # (return over mu_min)^2 per unit of excess variance
        for i in range(15):
            ret = mu_min + i * 0.5
            vol = math.sqrt(vol_min ** 2 + (ret - mu_min) ** 2 / k)
            rows.append({"date": date, "vol": r(vol, 3), "ret": r(ret, 3),
                         "sharpe": r((ret - rate) / vol, 3)})
    write("risk-frontier", "Efficient frontier by quarter, coloured by Sharpe", "data", rows)


def main():
    correlation_fixture()
    contribution_fixture()
    exposure_fixture()
    stress_fixture()
    underwater_fixture()
    distribution_fixture()
    frontier_fixture()


if __name__ == "__main__":
    main()
