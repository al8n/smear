//! smear's GraphQL lossless CST parser vs `apollo-parser`, both sides in one criterion process.
//!
//! Three tiers, measured separately because `apollo-parser`'s own published `parse_query` bench
//! conflates parsing with a typed traversal and reports one number for both:
//!
//! * **tier0** — lex only, each lexer driven to exhaustion with every token black-boxed.
//! * **tier1** — parse to CST, no traversal. The headline number.
//! * **tier2** — parse plus `apollo-parser`'s own typed traversal, reproduced on both sides.
//!   `tier2 - tier1` isolates the traversal.
//!
//! The [correctness gate](smear_apollo_bench::run_gate) runs first, in this same process, and any
//! corpus entry that either side mishandles is excluded from timing and named on stdout. An
//! entry both sides handle is timed with ids paired as `tierN/<entry>/smear` and
//! `tierN/<entry>/apollo` so criterion reports them adjacently.
//!
//! Run with `cargo bench -p smear-apollo-bench --bench apollo_comparison`. A `harness = false`
//! bench invoked any other way runs its body once with no timing.

use core::hint::black_box;

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use smear_apollo_bench::{
  CORPUS, Entry, apollo_lex, apollo_parse, apollo_parse_and_traverse, print_gate, run_gate,
  smear_lex, smear_parse, smear_parse_and_traverse,
};

/// Everything that is timed, for one eligible corpus entry.
fn bench_entry(c: &mut Criterion, entry: &Entry) {
  let bytes = Throughput::Bytes(entry.len() as u64);
  let src = entry.source;
  let shape = entry.shape;

  // Tier 0 — lex only.
  {
    let mut group = c.benchmark_group(format!("tier0_lex/{}", entry.name));
    group.throughput(bytes.clone());
    group.bench_function("smear", |b| b.iter(|| black_box(smear_lex(black_box(src)))));
    group.bench_function("apollo", |b| {
      b.iter(|| black_box(apollo_lex(black_box(src))))
    });
    group.finish();
  }

  // Tier 1 — parse to CST.
  {
    let mut group = c.benchmark_group(format!("tier1_parse/{}", entry.name));
    group.throughput(bytes.clone());
    group.bench_function("smear", |b| {
      b.iter(|| black_box(smear_parse(black_box(src))))
    });
    group.bench_function("apollo", |b| {
      b.iter(|| black_box(apollo_parse(black_box(src))))
    });
    group.finish();
  }

  // Tier 2 — parse plus typed traversal.
  {
    let mut group = c.benchmark_group(format!("tier2_parse_traverse/{}", entry.name));
    group.throughput(bytes);
    group.bench_function("smear", |b| {
      b.iter(|| black_box(smear_parse_and_traverse(black_box(src), shape)))
    });
    group.bench_function("apollo", |b| {
      b.iter(|| black_box(apollo_parse_and_traverse(black_box(src), shape)))
    });
    group.finish();
  }
}

fn bench_all(c: &mut Criterion) {
  // The gate runs before a single timing sample is taken, and it runs here rather than in a
  // separate process so the tree that was checked is built by the same binary that is timed.
  let rows = run_gate();
  print_gate(&rows);

  let mut timed = 0usize;
  for entry in CORPUS {
    let row = rows
      .iter()
      .find(|r| r.name == entry.name)
      .expect("every corpus entry has a gate row");
    if !row.eligible() {
      // Named on stdout by `print_gate` already; skipping silently is the failure mode the
      // design calls out by name, so say it again at the point of the skip.
      eprintln!("skipping timing for excluded corpus entry: {}", entry.name);
      continue;
    }
    bench_entry(c, entry);
    timed += 1;
  }

  assert!(
    timed > 0,
    "the correctness gate excluded every corpus entry; there is nothing to compare"
  );
}

criterion_group!(benches, bench_all);
criterion_main!(benches);
