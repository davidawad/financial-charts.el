// Runs invest-metrics on a JSON array of requests read from stdin and
// prints one {output} or {error} per request, in order.  A request is
// {metric, input} (one run, as `invest metric run ID --data INPUT`) or
// {metric, input, grid: {axes, picks, stamp}} (as `--grid A ... --pick P
// ... --stamp K=V`).  scripts/invest-metrics-runs.py builds the requests
// and writes test/fixtures/invest-metrics/runs.json; copy this file to
// the dotfiles repository's src/rust/crates/invest-metrics/examples/.
use std::io::Read;

use invest_metrics::grid;
use serde_json::{json, Map, Value};

fn run(r: &Value) -> Result<Value, String> {
    let id = r["metric"].as_str().ok_or("request without metric")?;
    match r.get("grid") {
        None => invest_metrics::run(id, &r["input"]).map_err(|e| e.to_string()),
        Some(g) => {
            let strs = |k: &str| -> Vec<String> {
                g[k].as_array()
                    .map(|a| a.iter().filter_map(|s| s.as_str().map(String::from)).collect())
                    .unwrap_or_default()
            };
            let axes = strs("axes")
                .iter()
                .map(|a| grid::axis(a))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| e.to_string())?;
            let picks: Vec<_> = strs("picks").iter().map(|p| grid::pick(p)).collect();
            let stamp = g["stamp"].as_object().cloned().unwrap_or_else(Map::new);
            grid::grid(id, &r["input"], &axes, &picks, &stamp)
                .map(Value::Array)
                .map_err(|e| e.to_string())
        }
    }
}

fn main() {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).expect("stdin");
    let requests: Value = serde_json::from_str(&s).expect("a JSON array of requests");
    let out: Vec<Value> = requests
        .as_array()
        .expect("a JSON array of requests")
        .iter()
        .map(|r| match run(r) {
            Ok(v) => json!({"output": v}),
            Err(e) => json!({"error": e}),
        })
        .collect();
    println!("{}", serde_json::to_string(&out).expect("JSON"));
}
