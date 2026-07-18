//! Baseline syntactic-parse throughput for the GraphQL document entry, over the
//! same fixture set `lex_baseline` (smear-lexer) uses, so parse cost reads
//! directly against lex cost per fixture.
//!
//! Each input is parsed to a full [`Document`] AST through the real entry
//! ([`ParseStr`]/[`ParseBytes`] → `run_parse_source` → `Fatal` context), in both
//! source representations:
//!   * `str` — the common path,
//!   * `[u8]` — the byte-slice path (same productions, different slice type).
//!
//! Throughput is reported in MB/s via `Throughput::Bytes(input.len())`. This is
//! the Phase-2S syntactic baseline: the lossless/CST waves must leave these
//! numbers unchanged (their no-op-emitter proof), and any dispatch upgrade
//! (e.g. `FusedDispatchOnKind`) must beat them sign-stably before adoption.

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use smear_parser_next::{
  entry::{ParseBytes, ParseStr},
  graphql::ast::Document,
};
use std::hint::black_box;

// The lex_baseline fixture set (workspace-relative include_str! — if the fixtures
// move, these fail at compile time, which is the right failure mode).

const Q_TINY: &str =
  include_str!("../../smear/tests/fixtures/executables/bench_01_tiny_simple.graphql");
const Q_SMALL: &str =
  include_str!("../../smear/tests/fixtures/executables/bench_03_small_variables.graphql");
const Q_MED_FRAG: &str =
  include_str!("../../smear/tests/fixtures/executables/bench_05_medium_fragments.graphql");
const Q_LARGE_COMPLEX: &str =
  include_str!("../../smear/tests/fixtures/executables/bench_06_large_complex.graphql");
const Q_HUGE: &str =
  include_str!("../../smear/tests/fixtures/executables/bench_10_huge_comprehensive.graphql");
const Q_KITCHEN_SINK: &str =
  include_str!("../../smear/tests/fixtures/executables/kitchen-sink_canonical.graphql");

const S_MINIMAL: &str = include_str!("../../smear/tests/fixtures/schemas/minimal.graphql");
const S_GMX: &str = include_str!("../../smear/tests/fixtures/schemas/gmx_schema.graphql");
const S_GITHUB: &str = include_str!("../../smear/tests/fixtures/schemas/github_schema.graphql");
const S_GITLAB: &str = include_str!("../../smear/tests/fixtures/schemas/gitlab_schema.graphql");

/// All inputs, paired with a short label for the bench id (the lex_baseline set).
const INPUTS: &[(&str, &str)] = &[
  ("query/tiny_8B", Q_TINY),
  ("query/small_76B", Q_SMALL),
  ("query/med_fragments_235B", Q_MED_FRAG),
  ("query/large_complex_1.3KB", Q_LARGE_COMPLEX),
  ("query/huge_4.2KB", Q_HUGE),
  ("query/kitchen_sink_2KB", Q_KITCHEN_SINK),
  ("schema/minimal_26B", S_MINIMAL),
  ("schema/gmx_69KB", S_GMX),
  ("schema/github_338KB", S_GITHUB),
  ("schema/gitlab_2.2MB", S_GITLAB),
];

fn parse_baseline(c: &mut Criterion) {
  let mut group = c.benchmark_group("parse_baseline");
  for (label, input) in INPUTS {
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_with_input(BenchmarkId::new("str", label), input, |b, input| {
      b.iter(|| Document::parse_str(black_box(input)).expect("fixture parses"))
    });
    group.bench_with_input(BenchmarkId::new("slice", label), input, |b, input| {
      b.iter(|| Document::parse_bytes(black_box(input.as_bytes())).expect("fixture parses"))
    });
  }
  group.finish();
}

criterion_group!(benches, parse_baseline);
criterion_main!(benches);
