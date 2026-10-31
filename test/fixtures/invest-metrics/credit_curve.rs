//! `credit.cds_term_structure` and `bond_corp.cds_implied_ladder`: analytic
//! sanity checks (flat curve, inverted curve, skipped tenors, extrapolation).
//! Pinned numbers live in `fixtures/golden/credit_curve.json`.

use invest_metrics::run;
use serde_json::{json, Value};

fn quote(tenor: f64, bp: Option<f64>) -> Value {
    json!({"tenor_years": tenor, "par_spread_bp": bp, "stale": false, "trades_30d": 20})
}

fn curve(spreads: &[(f64, f64)]) -> Vec<Value> {
    spreads.iter().map(|(t, s)| quote(*t, Some(*s))).collect()
}

fn term(spreads: &[(f64, f64)]) -> Value {
    run(
        "credit.cds_term_structure",
        &json!({"as_of": "2026-10-30", "recovery": 0.4, "discount_rate": 0.04, "curve": curve(spreads)}),
    )
    .unwrap()
}

fn f(value: &Value, pointer: &str) -> f64 {
    value.pointer(pointer).and_then(Value::as_f64).unwrap()
}

const FLAT: [(f64, f64); 6] = [
    (1.0, 100.0),
    (2.0, 100.0),
    (3.0, 100.0),
    (5.0, 100.0),
    (7.0, 100.0),
    (10.0, 100.0),
];

#[test]
fn flat_curve_has_constant_hazard_near_credit_triangle() {
    let out = term(&FLAT);
    let triangle = 0.01 / 0.6;
    for bucket in out["buckets"].as_array().unwrap() {
        let h = bucket["forward_hazard"].as_f64().unwrap();
        assert!(
            (h / triangle - 1.0).abs() < 0.03,
            "hazard {h} vs {triangle}"
        );
    }
    assert_eq!(out["shape"], "flat");
    assert_eq!(out["buckets"].as_array().unwrap().len(), 6);
    assert_eq!(out["buckets"][3]["label"], "3-5y");
    assert!(f(&out, "/slopes/s2s5s_bp").abs() < 1e-9);
    assert!(f(&out, "/slopes/s5s10s_bp").abs() < 1e-9);
}

#[test]
fn conditional_pd_chains_to_cumulative_pd() {
    let out = term(&FLAT);
    let survival: f64 = out["buckets"]
        .as_array()
        .unwrap()
        .iter()
        .map(|b| 1.0 - b["conditional_pd"].as_f64().unwrap())
        .product();
    assert!((survival - f(&out, "/tenors/5/survival")).abs() < 1e-12);
}

#[test]
fn inverted_curve_has_declining_forward_hazard() {
    let out = term(&[
        (1.0, 300.0),
        (2.0, 250.0),
        (3.0, 220.0),
        (5.0, 180.0),
        (7.0, 160.0),
        (10.0, 150.0),
    ]);
    assert_eq!(out["shape"], "inverted");
    assert!(f(&out, "/slopes/s2s5s_bp") < 0.0);
    assert!(f(&out, "/slopes/s5s10s_bp") < 0.0);
    let first = f(&out, "/buckets/0/forward_hazard");
    let last = f(&out, "/buckets/5/forward_hazard");
    assert!(first > last, "{first} <= {last}");
}

#[test]
fn upward_curve_is_labelled_upward() {
    let out = term(&[
        (1.0, 30.0),
        (2.0, 40.0),
        (3.0, 50.0),
        (5.0, 70.0),
        (7.0, 85.0),
        (10.0, 100.0),
    ]);
    assert_eq!(out["shape"], "upward");
    assert!(f(&out, "/slopes/s2s5s_bp") > 0.0);
}

#[test]
fn missing_and_stale_tenors_are_skipped_not_filled() {
    let mut stale = quote(7.0, Some(138.0));
    stale["stale"] = json!(true);
    let out = run(
        "credit.cds_term_structure",
        &json!({"as_of": "2026-10-30", "recovery": 0.4, "curve": [
            quote(1.0, None), quote(2.0, Some(142.6)), quote(3.0, Some(114.7)),
            quote(5.0, Some(134.2)), stale, quote(10.0, Some(138.8))]}),
    )
    .unwrap();
    let skipped: Vec<(f64, &str)> = out["skipped"]
        .as_array()
        .unwrap()
        .iter()
        .map(|s| {
            (
                s["tenor_years"].as_f64().unwrap(),
                s["reason"].as_str().unwrap(),
            )
        })
        .collect();
    assert_eq!(skipped, vec![(1.0, "no_prints"), (7.0, "stale")]);
    assert_eq!(out["tenors"].as_array().unwrap().len(), 4);
    assert_eq!(out["buckets"][0]["label"], "0-2y");
    assert_eq!(out["buckets"][3]["label"], "5-10y");
    assert_eq!(out["discount_rate_assumed"], true);
}

#[test]
fn thin_tenor_is_flagged() {
    let mut thin = quote(7.0, Some(138.0));
    thin["trades_30d"] = json!(2);
    let out = run(
        "credit.cds_term_structure",
        &json!({"as_of": "2026-10-30", "recovery": 0.4, "discount_rate": 0.04,
                "curve": [quote(3.0, Some(100.0)), quote(5.0, Some(110.0)), thin]}),
    )
    .unwrap();
    assert_eq!(out["tenors"][2]["thin"], true);
    assert_eq!(out["tenors"][1]["thin"], false);
}

#[test]
fn all_tenors_unusable_is_insufficient_not_an_error() {
    let out = run(
        "credit.cds_term_structure",
        &json!({"as_of": "2026-10-30", "recovery": 0.4, "curve": [quote(1.0, None)]}),
    )
    .unwrap();
    assert_eq!(out["shape"], "insufficient");
    assert!(out["tenors"].as_array().unwrap().is_empty());
}

#[test]
fn invalid_recovery_is_rejected() {
    assert!(run(
        "credit.cds_term_structure",
        &json!({"as_of": "2026-10-30", "recovery": 1.0, "curve": curve(&FLAT)}),
    )
    .is_err());
}

fn ladder(notes: Value) -> Value {
    run(
        "bond_corp.cds_implied_ladder",
        &json!({
            "as_of": "2026-10-30",
            "recovery": 0.4,
            "notes": notes,
            "treasury": {"years": [1.0, 2.0, 5.0, 10.0, 30.0], "rates": [0.04, 0.04, 0.04, 0.04, 0.04]},
            "cds": curve(&FLAT),
        }),
    )
    .unwrap()
}

#[test]
fn par_note_at_flat_curve_prices_near_par_and_labels_basis() {
    // Coupon equal to treasury + spread (5.0%) on a coupon date: price ~ 100.
    let out = ladder(json!([
        {"name": "5Y par", "coupon": 0.05, "maturity": "2031-10-30", "principal": 1000000000.0}
    ]));
    assert_eq!(out["basis"], "cds_implied_model, not traded");
    assert_eq!(out["rows"][0]["basis"], "cds_implied_model, not traded");
    let row = &out["rows"][0];
    assert!((f(row, "/model_yield") - 0.05).abs() < 1e-12);
    assert!((f(row, "/model_price") - 100.0).abs() < 0.05, "{row}");
    assert!(f(row, "/modified_duration") > 4.0 && f(row, "/modified_duration") < 4.6);
    assert!(f(row, "/cumulative_pd") > 0.0);
    assert!(row["spread_extrapolated"].is_null());
}

#[test]
fn beyond_quoted_tenors_is_flagged_extrapolated() {
    let out = ladder(json!([
        {"name": "2060 note", "coupon": 0.037, "maturity": "2060-04-01"},
        {"name": "2027 note", "coupon": 0.0155, "maturity": "2027-06-15"},
        {"name": "matured", "coupon": 0.02, "maturity": "2026-01-01"}
    ]));
    assert_eq!(out["rows"][0]["name"], "2027 note");
    assert_eq!(out["rows"][1]["spread_extrapolated"], "above");
    assert_eq!(out["skipped"][0]["reason"], "matured");
    assert_eq!(out["summary"]["principal_complete"], false);
    assert_eq!(
        out["summary"]["extrapolated"],
        json!(["2027 note", "2060 note"])
    );
}

#[test]
fn lower_coupon_prices_below_par_and_longer_notes_have_more_duration() {
    let out = ladder(json!([
        {"name": "short", "coupon": 0.0125, "maturity": "2029-05-15"},
        {"name": "long", "coupon": 0.0125, "maturity": "2040-05-15"}
    ]));
    assert!(f(&out, "/rows/0/model_price") < 100.0);
    assert!(f(&out, "/rows/1/model_price") < f(&out, "/rows/0/model_price"));
    assert!(f(&out, "/rows/1/modified_duration") > f(&out, "/rows/0/modified_duration"));
}

#[test]
fn ladder_without_usable_cds_is_an_error() {
    assert!(run(
        "bond_corp.cds_implied_ladder",
        &json!({"as_of": "2026-10-30", "recovery": 0.4, "notes": [],
                "treasury": {"years": [5.0], "rates": [0.04]}, "cds": [quote(5.0, None)]}),
    )
    .is_err());
}

/// Prints the outputs the golden fixture pins; run with `--nocapture` and
/// `--ignored` when intentionally re-blessing.
#[test]
#[ignore = "bless helper: prints outputs for fixtures/golden/credit_curve.json"]
fn print_golden_outputs() {
    let nvda = json!({"as_of": "2026-10-30", "recovery": 0.4, "discount_rate": 0.0471,
        "curve": [quote(1.0, None), quote(2.0, Some(142.6226149354434)), quote(3.0, Some(114.74169006047353)),
                  quote(5.0, Some(134.21365116388264)), quote(7.0, Some(138.0)), quote(10.0, Some(138.77832029296403))]});
    let ladder = run(
        "bond_corp.cds_implied_ladder",
        &json!({
            "as_of": "2026-10-30",
            "recovery": 0.4,
            "notes": [
                {"name": "3.20% Notes Due 2026", "coupon": 0.032, "maturity": "2026-09-16", "principal": 1.0e9},
                {"name": "1.55% Notes Due 2028", "coupon": 0.0155, "maturity": "2028-06-15", "principal": 1.25e9},
                {"name": "2.85% Notes Due 2030", "coupon": 0.0285, "maturity": "2030-04-01", "principal": 1.5e9},
                {"name": "2.00% Notes Due 2031", "coupon": 0.02, "maturity": "2031-06-15", "principal": 1.25e9},
                {"name": "3.50% Notes Due 2040", "coupon": 0.035, "maturity": "2040-04-01", "principal": 1.0e9},
                {"name": "3.50% Notes Due 2050", "coupon": 0.035, "maturity": "2050-04-01", "principal": 2.0e9},
                {"name": "3.70% Notes Due 2060", "coupon": 0.037, "maturity": "2060-04-01", "principal": 0.5e9}
            ],
            "treasury": {
                "years": [0.083, 0.125, 0.167, 0.25, 0.333, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 10.0, 20.0, 30.0],
                "rates": [0.0406, 0.0409, 0.0416, 0.0416, 0.0418, 0.0425, 0.0436, 0.0456, 0.0465, 0.0471, 0.048, 0.0494, 0.0534, 0.0531]
            },
            "cds": [quote(1.0, None), quote(2.0, Some(142.6226149354434)), quote(3.0, Some(114.74169006047353)),
                    quote(5.0, Some(134.21365116388264)), quote(7.0, Some(138.0)), quote(10.0, Some(138.77832029296403))]
        }),
    )
    .unwrap();
    println!("LADDER {ladder}");
    println!("FLAT {}", term(&FLAT));
    println!("NVDA {}", run("credit.cds_term_structure", &nvda).unwrap());
    println!(
        "INV {}",
        term(&[
            (1.0, 300.0),
            (2.0, 250.0),
            (3.0, 220.0),
            (5.0, 180.0),
            (7.0, 160.0),
            (10.0, 150.0)
        ])
    );
}

/// The recorded NVDA CDS view (`invest cds show NVDA --json` minus history).
const NVDA_VIEW: &str = include_str!("fixtures/credit_nvda_view.json");

/// Prints the metric outputs for the Elisp board's recorded fixtures: the
/// real view curve, the discount rate the board passes (the 5y Treasury par
/// yield) and the notes as the board reshapes them from the 10-K facts.
#[test]
#[ignore = "bless helper: prints outputs for the Elisp board fixtures"]
fn print_board_fixture_outputs() {
    let view: Value = serde_json::from_str(NVDA_VIEW).unwrap();
    let curve = view["curve"].clone();
    let term = run(
        "credit.cds_term_structure",
        &json!({"as_of": view["as_of"], "recovery": view["implied"]["recovery"],
                "discount_rate": 0.0471, "curve": curve}),
    )
    .unwrap();
    println!("FIXTURE_TERM {term}");
    let notes: Vec<Value> = [
        (0.032, "2026-09-16", 1.0e9, "2026"),
        (0.0155, "2028-06-15", 1.25e9, "2028"),
        (0.0285, "2030-04-01", 1.5e9, "2030"),
        (0.02, "2031-06-15", 1.25e9, "2031"),
        (0.035, "2040-04-01", 1.0e9, "2040"),
        (0.035, "2050-04-01", 2.0e9, "2050"),
        (0.037, "2060-04-01", 0.5e9, "2060"),
    ]
    .iter()
    .map(|(c, m, p, y)| json!({"name": format!("{:.2}% Notes Due {y}", c * 100.0), "coupon": c, "maturity": m, "principal": p}))
    .collect();
    let ladder = run(
        "bond_corp.cds_implied_ladder",
        &json!({"as_of": view["as_of"], "recovery": view["implied"]["recovery"], "notes": notes,
                "treasury": {"years": [0.083, 0.125, 0.167, 0.25, 0.333, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 10.0, 20.0, 30.0],
                             "rates": [0.0406, 0.0409, 0.0416, 0.0416, 0.0418, 0.0425, 0.0436, 0.0456, 0.0465, 0.0471, 0.048, 0.0494, 0.0534, 0.0531]},
                "cds": curve}),
    )
    .unwrap();
    println!("FIXTURE_LADDER {ladder}");
}
