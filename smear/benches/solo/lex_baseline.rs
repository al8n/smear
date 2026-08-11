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
use smear::lexer::{
  graphql::{
    lossless::{LosslessLexer, LosslessLexerErrors, LosslessToken},
    syntactic::{SyntacticLexer, SyntacticLexerErrors, SyntacticToken},
  },
  tokora::lexer::Lexer as _,
};
use std::hint::black_box;

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
// Every path below is relative to THIS FILE, which is how `include_str!` resolves; the corpora
// are this package's own, two directories up in `smear/tests/fixtures/`. If a fixture moves,
// these calls fail at compile time, which is the right failure mode — but only for whoever
// compiles this target, and NOTHING in an ordinary run does: `cargo test`'s default selection is
// lib + bins + tests + examples, and a `[[bench]]` is in none of it. The gate is
// `.github/workflows/ci.yml`'s `build` job, which runs `cargo bench --no-run` for exactly that
// reason; read its Pass 3 comment before assuming a bench is covered by anything else.

const FIXTURES_EXEC: &str = "smear/tests/fixtures/executables";
const FIXTURES_SCHEMA: &str = "smear/tests/fixtures/schemas";

// Executable queries.
const Q_TINY: &str = include_str!("../../tests/fixtures/executables/bench_01_tiny_simple.graphql");
const Q_SMALL: &str =
  include_str!("../../tests/fixtures/executables/bench_03_small_variables.graphql");
const Q_MED_FRAG: &str =
  include_str!("../../tests/fixtures/executables/bench_05_medium_fragments.graphql");
const Q_LARGE_COMPLEX: &str =
  include_str!("../../tests/fixtures/executables/bench_06_large_complex.graphql");
const Q_HUGE: &str =
  include_str!("../../tests/fixtures/executables/bench_10_huge_comprehensive.graphql");
const Q_KITCHEN_SINK: &str =
  include_str!("../../tests/fixtures/executables/kitchen-sink_canonical.graphql");

// Schemas — ascending in size.
const S_MINIMAL: &str = include_str!("../../tests/fixtures/schemas/minimal.graphql");
const S_GMX: &str = include_str!("../../tests/fixtures/schemas/gmx_schema.graphql");
const S_GITHUB: &str = include_str!("../../tests/fixtures/schemas/github_schema.graphql");
const S_GITLAB: &str = include_str!("../../tests/fixtures/schemas/gitlab_schema.graphql");

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

/// SIMD-layered syntactic lexer: SIMD trivia skip + SIMD ident
/// + inline punct + Logos delegation for everything else.
#[inline(always)]
fn simd_count(input: &str) -> usize {
  let mut lexer = SyntacticLexer::<[u8]>::new(input.as_bytes());
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

#[inline(always)]
fn simd_collect(input: &str) -> Vec<Result<SyntacticToken<&[u8]>, SyntacticLexerErrors<u8>>> {
  let mut lexer = SyntacticLexer::<[u8]>::new(input.as_bytes());
  let mut tokens: Vec<Result<SyntacticToken<&[u8]>, SyntacticLexerErrors<u8>>> = Vec::new();
  while let Some(result) = lexer.lex() {
    tokens.push(result);
  }
  tokens
}

type LosslessItem<'a> = Result<LosslessToken<&'a str>, LosslessLexerErrors>;

#[inline(always)]
fn lossless_collect(input: &str) -> Vec<LosslessItem<'_>> {
  let mut lexer = LosslessLexer::<&str>::new(input);
  let mut tokens: Vec<LosslessItem<'_>> = Vec::new();
  while let Some(result) = lexer.lex() {
    tokens.push(result);
  }
  tokens
}

/// GraphQLx lossless-mode token count, mirroring `lossless_count` above.
#[inline(always)]
fn gx_lossless_count(input: &str) -> usize {
  use smear::lexer::graphqlx::lossless::LosslessLexer;
  let mut lexer = LosslessLexer::<&str>::new(input);
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

/// GraphQLx SIMD syntactic lexer token count, mirroring `simd_count`.
#[inline(always)]
fn gx_simd_count(input: &str) -> usize {
  use smear::lexer::graphqlx::syntactic::SyntacticLexer;
  let mut lexer = SyntacticLexer::<[u8]>::new(input.as_bytes());
  let mut count = 0usize;
  while let Some(result) = lexer.lex() {
    let _ = black_box(result);
    count = count.wrapping_add(1);
  }
  count
}

/// A decimal-number-dense GraphQLx input (~500 ints/floats in a list literal)
/// that stresses the decimal fast-path scanner. Built at runtime so the build
/// cost stays outside the measured `iter` closure.
fn number_heavy_input() -> String {
  use std::fmt::Write as _;
  let mut s = String::with_capacity(4096);
  s.push_str("query { metrics(samples: [");
  for i in 0..500u32 {
    if i > 0 {
      s.push(',');
    }
    match i % 3 {
      0 => {
        let _ = write!(s, "{i}");
      }
      1 => {
        let _ = write!(s, "{i}.{}", i % 100);
      }
      _ => {
        let _ = write!(s, "-{i}.{}e{}", i % 10, i % 5);
      }
    }
  }
  s.push_str("]) }");
  s
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

/// SIMD layer throughput, same fixtures as the lossless baseline.
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

/// GraphQLx lossless-mode baseline throughput, same fixtures as the GraphQL
/// baselines above.
fn bench_graphqlx(c: &mut Criterion) {
  let mut group = c.benchmark_group("graphqlx/lex/baseline");
  group.sample_size(30);
  for (label, input) in INPUTS {
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_with_input(
      BenchmarkId::new("lossless_count", label),
      input,
      |b, input| b.iter(|| black_box(gx_lossless_count(black_box(input)))),
    );
    group.bench_with_input(BenchmarkId::new("simd_count", label), input, |b, input| {
      b.iter(|| black_box(gx_simd_count(black_box(input))))
    });
  }

  // Decimal-dense synthetic input: exercises the decimal fast-path scanner.
  let num_heavy = number_heavy_input();
  group.throughput(Throughput::Bytes(num_heavy.len() as u64));
  group.bench_with_input(
    BenchmarkId::new("simd_count", "num_heavy"),
    num_heavy.as_str(),
    |b, input| b.iter(|| black_box(gx_simd_count(black_box(input)))),
  );

  group.finish();
}

criterion_group!(benches, bench_lossless, bench_simd, bench_graphqlx);
criterion_main!(benches);

// The two fixture roots every `include_str!` above reaches into, written once as
// WORKSPACE-relative paths so a reader can find them without composing `../../` by hand.
//
// This is documentation, not a check, and the comment that used to sit here called it a
// "compile-time touch on the fixture roots" — a `const &str` holding a path does not verify
// that the path exists, so it caught nothing and never could. Naming it accurately matters
// here specifically: a construct that reads as a gate and is not one is #73's whole subject.
// The real gate is the `include_str!` calls themselves, and CI reaching them is what
// `.github/workflows/ci.yml`'s `build` job now guarantees.
#[allow(dead_code)]
const _FIXTURE_ROOTS: (&str, &str) = (FIXTURES_EXEC, FIXTURES_SCHEMA);
