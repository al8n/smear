//! Baseline lexer throughput benchmarks for `SyntacticLexer` (the
//! SIMD-accelerated lexer) and `LosslessLexer`.
//!
//! Goal: get a stable, comparable number for the *current* lexer across a
//! representative set of GraphQL inputs, so that any future SIMD layer can
//! be measured against the same fixtures.
//!
//! Each input is exercised in two ways:
//!   * `count` — drive `lex()` to completion and count tokens. Forces every
//!     byte to be classified; closest to "what does the lexer actually do".
//!   * `collect` — push every token into a `Vec`, simulating the typical
//!     downstream pattern where the parser pulls tokens one by one. Adds
//!     allocation + memcpy noise but is closer to real usage.
//!
//! Throughput is reported in MB/s via `Throughput::Bytes(input.len())`,
//! which makes it easy to compare across input sizes and across future
//! optimization passes.

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use smear_lexer::{
  graphql::{
    lossless::{LosslessLexer, LosslessLexerErrors, LosslessToken},
    simd::SimdSyntacticLexer,
    syntactic::{SyntacticLexerErrors, SyntacticToken},
  },
  tokit::lexer::{Lexer as _, LogosLexer},
};
use std::hint::black_box;

// Live Logos lexer for the baseline benches — named directly (no public
// alias). This is exactly what `SyntacticLexer<'a, &'a str>` used to resolve
// to.
type GraphqlLogos<'a> = LogosLexer<'a, SyntacticToken<&'a str>>;

// ---- inputs --------------------------------------------------------------
//
// A range of sizes spanning ~5 orders of magnitude (8 B → 2.3 MB) plus
// stylistic variety:
//   * tiny / small / medium / large / huge executable queries — exercise
//     short-input dispatch overhead and the per-call lexer cost
//   * canonical kitchen-sink — every token kind in one file
//   * minimal schema — degenerate small SDL
//   * gmx schema — a realistic medium SDL
//   * github schema — a large public SDL (~340 KB)
//   * gitlab schema — an extreme stress test (~2.3 MB)
//
// All paths are workspace-relative; if the fixtures move these `include_str!`
// calls will fail at compile time, which is the right failure mode.

const FIXTURES_EXEC: &str = "../smear/tests/fixtures/executables";
const FIXTURES_SCHEMA: &str = "../smear/tests/fixtures/schemas";

// Executable queries.
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

// Schemas — ascending in size.
const S_MINIMAL: &str = include_str!("../../smear/tests/fixtures/schemas/minimal.graphql");
const S_GMX: &str = include_str!("../../smear/tests/fixtures/schemas/gmx_schema.graphql");
const S_GITHUB: &str = include_str!("../../smear/tests/fixtures/schemas/github_schema.graphql");
const S_GITLAB: &str = include_str!("../../smear/tests/fixtures/schemas/gitlab_schema.graphql");

/// All inputs, paired with a short label for the bench id.
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

// ---- driving the lexers --------------------------------------------------

#[inline(always)]
fn syntactic_count(input: &str) -> usize {
  let mut lexer = GraphqlLogos::new(input);
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    // black_box on the result so the compiler can't elide the per-token
    // work (e.g., dropping `Identifier(slice)` without reading the slice).
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

#[inline(always)]
fn lossless_count(input: &str) -> usize {
  let mut lexer = LosslessLexer::<&str>::new(input);
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

/// Phase-1 SIMD-layered syntactic lexer: SIMD trivia skip + SIMD ident
/// + inline punct + Logos delegation for everything else.
#[inline(always)]
fn simd_count(input: &str) -> usize {
  let mut lexer = SimdSyntacticLexer::<[u8]>::new(input.as_bytes());
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

#[inline(always)]
fn simd_collect(input: &str) -> Vec<Result<SyntacticToken<&[u8]>, SyntacticLexerErrors<u8>>> {
  let mut lexer = SimdSyntacticLexer::<[u8]>::new(input.as_bytes());
  let mut tokens: Vec<Result<SyntacticToken<&[u8]>, SyntacticLexerErrors<u8>>> = Vec::new();
  while let Some(result) = lexer.lex() {
    tokens.push(result);
  }
  tokens
}

type SyntacticItem<'a> = Result<SyntacticToken<&'a str>, SyntacticLexerErrors>;
type LosslessItem<'a> = Result<LosslessToken<&'a str>, LosslessLexerErrors>;

#[inline(always)]
fn syntactic_collect(input: &str) -> Vec<SyntacticItem<'_>> {
  let mut lexer = GraphqlLogos::new(input);
  let mut tokens: Vec<SyntacticItem<'_>> = Vec::new();
  while let Some(result) = lexer.lex() {
    tokens.push(result);
  }
  tokens
}

#[inline(always)]
fn lossless_collect(input: &str) -> Vec<LosslessItem<'_>> {
  let mut lexer = LosslessLexer::<&str>::new(input);
  let mut tokens: Vec<LosslessItem<'_>> = Vec::new();
  while let Some(result) = lexer.lex() {
    tokens.push(result);
  }
  tokens
}

/// GraphQLx syntactic-mode token count, mirroring `syntactic_count` above but
/// against the GraphQLx grammar (generics, imports, type paths, etc.).
#[inline(always)]
fn gx_syntactic_count(input: &str) -> usize {
  use smear_lexer::graphqlx::syntactic::SyntacticToken as GxSyntacticToken;
  let mut lexer = LogosLexer::<GxSyntacticToken<&str>>::new(input);
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

/// GraphQLx lossless-mode token count, mirroring `lossless_count` above.
#[inline(always)]
fn gx_lossless_count(input: &str) -> usize {
  use smear_lexer::graphqlx::lossless::LosslessLexer;
  let mut lexer = LosslessLexer::<&str>::new(input);
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

// ---- groups --------------------------------------------------------------

/// Lexer-only throughput, syntactic mode (skips trivia).
fn bench_syntactic(c: &mut Criterion) {
  let mut group = c.benchmark_group("graphql/lex/syntactic");
  // Schemas in the MB range take a while at criterion's default sample
  // count; cap the time budget so the full suite finishes in minutes
  // rather than tens of minutes. Per-iteration measurement remains stable.
  group.sample_size(30);

  for (label, input) in INPUTS {
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_with_input(BenchmarkId::new("count", label), input, |b, input| {
      b.iter(|| black_box(syntactic_count(black_box(input))))
    });

    group.bench_with_input(BenchmarkId::new("collect", label), input, |b, input| {
      b.iter(|| black_box(syntactic_collect(black_box(input))))
    });
  }

  group.finish();
}

/// Lexer-only throughput, lossless mode (preserves trivia as tokens).
fn bench_lossless(c: &mut Criterion) {
  let mut group = c.benchmark_group("graphql/lex/lossless");
  group.sample_size(30);

  for (label, input) in INPUTS {
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_with_input(BenchmarkId::new("count", label), input, |b, input| {
      b.iter(|| black_box(lossless_count(black_box(input))))
    });

    group.bench_with_input(BenchmarkId::new("collect", label), input, |b, input| {
      b.iter(|| black_box(lossless_collect(black_box(input))))
    });
  }

  group.finish();
}

/// Phase-1 SIMD layer throughput, same fixtures as the syntactic baseline.
fn bench_simd(c: &mut Criterion) {
  let mut group = c.benchmark_group("graphql/lex/simd_phase1");
  group.sample_size(30);

  for (label, input) in INPUTS {
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_with_input(BenchmarkId::new("count", label), input, |b, input| {
      b.iter(|| black_box(simd_count(black_box(input))))
    });

    group.bench_with_input(BenchmarkId::new("collect", label), input, |b, input| {
      b.iter(|| black_box(simd_collect(black_box(input))))
    });
  }

  group.finish();
}

/// GraphQLx baseline throughput (Logos-driven), same fixtures as the GraphQL
/// baselines above. Gives every one of the four lexers (graphql/syntactic,
/// graphql/lossless, graphqlx/syntactic, graphqlx/lossless) a recorded
/// number to compare future SIMD work against.
fn bench_graphqlx(c: &mut Criterion) {
  let mut group = c.benchmark_group("graphqlx/lex/baseline");
  group.sample_size(30);
  for (label, input) in INPUTS {
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_with_input(
      BenchmarkId::new("syntactic_count", label),
      input,
      |b, input| b.iter(|| black_box(gx_syntactic_count(black_box(input)))),
    );
    group.bench_with_input(
      BenchmarkId::new("lossless_count", label),
      input,
      |b, input| b.iter(|| black_box(gx_lossless_count(black_box(input)))),
    );
  }
  group.finish();
}

// ---- registration --------------------------------------------------------

criterion_group!(
  benches,
  bench_syntactic,
  bench_lossless,
  bench_simd,
  bench_graphqlx
);
criterion_main!(benches);

// Compile-time touch on the fixture roots so reorganising those dirs gives
// a clear error pointing at this file instead of cryptic missing-file
// errors from `include_str!`.
#[allow(dead_code)]
const _FIXTURE_PATHS: (&str, &str) = (FIXTURES_EXEC, FIXTURES_SCHEMA);
