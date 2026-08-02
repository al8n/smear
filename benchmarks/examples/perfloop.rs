//! A/B timing loop for one corpus entry: **min over blocks**, not mean.
//!
//! Criterion reports a mean with a bootstrap interval, which is the right statistic for an
//! idle machine and the wrong one for a shared one: a single interfering process inflates the
//! mean of a whole block and the interval with it. The minimum over independent blocks is the
//! order statistic that interference can only move in one direction, so on a loaded machine it
//! is the estimator that compares two builds rather than two schedulings.
//!
//! ```text
//! cargo run -q -p smear-apollo-bench --example perfloop --release -- alias
//! ```

use std::{hint::black_box, time::Instant};

use smear_apollo_bench::{CORPUS, apollo_parse, smear_parse};

fn main() {
  let args: Vec<String> = std::env::args().skip(1).collect();
  let want = args.first().map_or("alias", String::as_str);
  let entry = CORPUS
    .iter()
    .find(|e| e.name == want)
    .expect("no such corpus entry");
  let src = entry.source;

  // Block sizes chosen so one block is ~0.3 s on this entry: long enough to amortise the
  // clock read, short enough that a block is unlikely to span an interference event.
  let iters = (60_000_000 / src.len().max(1)).clamp(20, 20_000);

  for _ in 0..iters / 4 {
    black_box(smear_parse(black_box(src)));
  }

  let mut smear_best = f64::MAX;
  let mut apollo_best = f64::MAX;
  for _ in 0..9 {
    let t = Instant::now();
    for _ in 0..iters {
      black_box(smear_parse(black_box(src)));
    }
    let us = t.elapsed().as_secs_f64() * 1e6 / iters as f64;
    smear_best = smear_best.min(us);

    let t = Instant::now();
    for _ in 0..iters {
      black_box(apollo_parse(black_box(src)));
    }
    let us = t.elapsed().as_secs_f64() * 1e6 / iters as f64;
    apollo_best = apollo_best.min(us);
  }

  println!(
    "{} {} bytes  iters={iters}  smear {smear_best:.1} us  apollo {apollo_best:.1} us  gap {:.1} us",
    entry.name,
    src.len(),
    smear_best - apollo_best,
  );
}
