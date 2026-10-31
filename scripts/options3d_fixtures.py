#!/usr/bin/env python3
"""Write the options surfaces' fixtures and examples.

test/fixtures/3d/options/NAME.data.json and examples/3d/NAME.data.json
(the template's example, which ships with the package; the same bytes).

Each fixture is the bindings its template takes, rows as the invest-metrics
grid metric docs/3d-options.md requests would emit them, derived here in
closed form (Black-Scholes-Merton, no dividends) so they are exact and
reproducible:

  greeks-surface.data.json        opt.greeks_grid: a long 100 call, 25% vol,
                                  4% rate, spot 70..130 by 7 dates to expiry
  position-pnl-surface.data.json  opt.payoff_grid: a long 90/100/110 call
                                  butterfly opened at 45 days, decaying to
                                  its expiry payoff
  pop-surface.data.json           opt.pop_grid: a short put's probability of
                                  profit and of touch by strike and expiry

    python3 scripts/options3d_fixtures.py

financial-chart-3d-options-test.el checks the rows against its own BSM.
"""

import json
import math
import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = [os.path.join(ROOT, "test", "fixtures", "3d", "options"),
       os.path.join(ROOT, "examples", "3d")]

VOL = 0.25
RATE = 0.04
SPOT = 100.0


def ncdf(x):
    return 0.5 * (1.0 + math.erf(x / math.sqrt(2.0)))


def npdf(x):
    return math.exp(-0.5 * x * x) / math.sqrt(2.0 * math.pi)


def d1d2(s, k, t, vol=VOL, r=RATE):
    sd = vol * math.sqrt(t)
    d1 = (math.log(s / k) + (r + 0.5 * vol * vol) * t) / sd
    return d1, d1 - sd


def call(s, k, t, vol=VOL, r=RATE):
    if t <= 0:
        return max(0.0, s - k)
    d1, d2 = d1d2(s, k, t, vol, r)
    return s * ncdf(d1) - k * math.exp(-r * t) * ncdf(d2)


def put(s, k, t, vol=VOL, r=RATE):
    if t <= 0:
        return max(0.0, k - s)
    return call(s, k, t, vol, r) - s + k * math.exp(-r * t)


def call_greeks(s, k, t, vol=VOL, r=RATE):
    """Delta, gamma, vega (per vol point), theta (per day), vanna (per vol
    point) and charm (delta change per day passing) of a long call."""
    d1, d2 = d1d2(s, k, t, vol, r)
    sq = math.sqrt(t)
    return {
        "delta": ncdf(d1),
        "gamma": npdf(d1) / (s * vol * sq),
        "vega": s * npdf(d1) * sq / 100.0,
        "theta": (-s * npdf(d1) * vol / (2 * sq) - r * k * math.exp(-r * t) * ncdf(d2)) / 365.0,
        "vanna": -npdf(d1) * d2 / vol / 100.0,
        "charm": -npdf(d1) * (2 * r * t - d2 * vol * sq) / (2 * t * vol * sq) / 365.0,
    }


def sig(v, digits=6):
    return float(f"{v:.{digits}g}") if v else 0.0


def rnd(v, places):
    return round(v + 0.0, places) + 0.0


def greeks_surface():
    days = [60, 45, 30, 21, 14, 7, 3]
    spots = [70 + 2.5 * i for i in range(25)]
    rows = []
    for dte in days:
        for s in spots:
            g = call_greeks(s, 100.0, dte / 365.0)
            row = {"spot": s, "dte": dte, "day": days[0] - dte}
            row.update({k: sig(v) for k, v in g.items()})
            rows.append(row)
    return {
        "title": "Long 100 call: gamma by spot and days to expiry",
        "greek": "gamma",
        "spot": SPOT,
        "data": rows,
    }


BUTTERFLY = [(1, 90.0), (-2, 100.0), (1, 110.0)]


def butterfly_value(s, t):
    return sum(q * call(s, k, t) for q, k in BUTTERFLY)


def pnl_surface():
    days = [45, 38, 31, 24, 17, 10, 5, 2, 0]
    spots = [80 + 2.0 * i for i in range(21)]
    cost = butterfly_value(SPOT, days[0] / 365.0)
    rows = []
    for dte in days:
        for s in spots:
            rows.append({"spot": s, "dte": dte, "day": days[0] - dte,
                         "pnl": rnd(100 * (butterfly_value(s, dte / 365.0) - cost), 2)})
    return {
        "title": "Long 90/100/110 call butterfly, P/L to expiry",
        "spot": SPOT,
        "data": rows,
    }


def touch(s, k, t, vol=VOL, r=RATE):
    """Probability that a lognormal price from S touches K < S within T."""
    if k >= s:
        return 1.0
    mu = r - 0.5 * vol * vol
    sd = vol * math.sqrt(t)
    b = math.log(k / s)
    return ncdf((b - mu * t) / sd) + (k / s) ** (2 * mu / (vol * vol)) * ncdf((b + mu * t) / sd)


def pop_surface():
    days = [90, 60, 45, 30, 21, 14, 7]
    strikes = [70 + 2.5 * i for i in range(17)]
    rows = []
    for dte in days:
        t = dte / 365.0
        for k in strikes:
            credit = put(SPOT, k, t)
            be = k - credit
            sd = VOL * math.sqrt(t)
            pop = ncdf((math.log(SPOT / be) + (RATE - 0.5 * VOL * VOL) * t) / sd)
            rows.append({"strike": k, "dte": dte, "credit": rnd(credit, 4),
                         "breakeven": rnd(be, 4), "pop": rnd(pop, 4),
                         "prob_touch": rnd(touch(SPOT, k, t), 4)})
    return {
        "title": "Short put: probability of profit by strike and expiry",
        "metric": "pop",
        "spot": SPOT,
        "data": rows,
    }


def write(name, bindings):
    for out in OUT:
        write_to(os.path.join(out, name + ".data.json"), bindings)


def write_to(path, bindings):
    with open(path, "w") as f:
        f.write("{\n")
        items = list(bindings.items())
        for i, (key, value) in enumerate(items):
            f.write(f"  {json.dumps(key)}: ")
            if isinstance(value, list):
                f.write("[\n" + ",\n".join("    " + json.dumps(r, separators=(",", ":")) for r in value) + "\n  ]")
            else:
                f.write(json.dumps(value))
            f.write(",\n" if i < len(items) - 1 else "\n")
        f.write("}\n")
    print(path)


def main():
    for out in OUT:
        os.makedirs(out, exist_ok=True)
    write("greeks-surface", greeks_surface())
    write("position-pnl-surface", pnl_surface())
    write("pop-surface", pop_surface())


if __name__ == "__main__":
    sys.exit(main())
