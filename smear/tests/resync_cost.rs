#![cfg(all(feature = "rowan", feature = "parser"))]

//! The cost curve of lossless recovery — smear issue #168.
//!
//! **This file is a measurement harness, not a gate.** Every test in it is `#[ignore]`d and
//! asserts nothing about time; what it asserts is the lossless guarantee, so a run that reports
//! numbers is at least reporting them for a parse that covered its input. Run it with
//!
//! ```text
//! cargo test --release -p smear --features rowan --test resync_cost -- --ignored --nocapture
//! ```
//!
//! It is committed because the numbers in #168 were taken from an uncommitted harness and the
//! round that followed could not tell a repair from a change of instrument. A figure quoted
//! against this file names a profile, a shape and a size, and can be reproduced.
//!
//! # What each shape isolates
//!
//! Both lossless recovery helpers scan through [`tokora::InputRef::sync_balanced`], whose
//! no-match exit **rewinds the whole scan**. A caller that then advances one token and asks
//! again pays for the tail once per token, which is the Θ(n²).
//!
//! - `! ` and `@ ( ) ` reach it through `recover::unexpected`, whose restart set is the wide
//!   `is_sync_point`. A tail of `! @ : = | &` and `(` contains no member of that set, so every
//!   scan runs to end of input and is thrown away.
//! - `[ type ] ` reaches it through `recover::resync_to`, whose restart set is the narrow
//!   `is_definition_start`. `[` is a sync point, so `unexpected` stays cheap through this shape
//!   and the whole cost is the resync's: every `type` sits at depth 1, where the scan never
//!   consults the predicate.
//! - `( type ) ` reaches it through **both**.
//! - `bypass` is #167's recovery-bypass document, kept because #168 quotes 407 s for it. It is
//!   no longer superlinear: since #169 a nesting refusal is terminal, so the document root
//!   returns instead of resynchronising and `Cst::finish_partial` tiles the tail.

use std::time::Instant;

/// `n` repetitions of `unit`.
fn rep(unit: &str, n: usize) -> String {
  unit.repeat(n)
}

/// #167's recovery-bypass document: `{` then `levels` copies of `" ) f {"`.
fn bypass(levels: usize) -> String {
  let mut src = String::with_capacity(levels * 6 + 1);
  src.push('{');
  for _ in 0..levels {
    src.push_str(" ) f {");
  }
  src
}

/// Parses `src` at `root`, asserts the lossless guarantee, and returns (seconds, diagnostics).
fn timed(root: fn(&str) -> (usize, usize), src: &str) -> (f64, usize) {
  let t = Instant::now();
  let (diags, covered) = root(src);
  let secs = t.elapsed().as_secs_f64();
  assert_eq!(
    covered,
    src.len(),
    "the parse did not cover its input, so its time is not a time for this document"
  );
  (secs, diags)
}

#[cfg(feature = "graphql")]
fn graphql(src: &str) -> (usize, usize) {
  let parse = smear::parser::graphql::lossless::parse_document(src);
  (
    parse.diagnostics().len(),
    parse.syntax().text().to_string().len(),
  )
}

#[cfg(feature = "graphqlx")]
fn graphqlx(src: &str) -> (usize, usize) {
  let parse = smear::parser::graphqlx::lossless::parse_document(src);
  (
    parse.diagnostics().len(),
    parse.syntax().text().to_string().len(),
  )
}

/// One doubling curve. A ratio near 2 is linear; near 4 is quadratic.
fn curve(label: &str, root: fn(&str) -> (usize, usize), unit: &str, sizes: &[usize]) {
  print!("{label:<10} {:<12}", format!("{unit:?}"));
  let mut prev: Option<f64> = None;
  for &n in sizes {
    let src = rep(unit, n);
    let (secs, diags) = timed(root, &src);
    let ratio = prev
      .map(|p| format!("x{:.2}", secs / p))
      .unwrap_or_else(|| "     ".into());
    print!(
      " |{:8}B {:9.1}ms {ratio:>6} d={diags}",
      src.len(),
      secs * 1e3
    );
    prev = Some(secs);
  }
  println!();
}

#[test]
#[ignore = "a measurement, not a gate: minutes of CPU by design"]
#[cfg(feature = "graphql")]
fn graphql_recovery_cost() {
  println!("\n== GraphQL, mixed document root ==");
  curve("gql", graphql, "! ", &[4_000, 8_000, 16_000]);
  curve("gql", graphql, "@ ( ) ", &[2_000, 4_000, 8_000]);
  curve("gql", graphql, "[ type ] ", &[2_000, 4_000, 8_000]);
  curve("gql", graphql, "( type ) ", &[2_000, 4_000, 8_000]);
}

#[test]
#[ignore = "a measurement, not a gate: minutes of CPU by design"]
#[cfg(feature = "graphqlx")]
fn graphqlx_recovery_cost() {
  println!("\n== GraphQLx, mixed document root — `<`/`>` is the fourth pair ==");
  curve("gqlx", graphqlx, "! ", &[4_000, 8_000, 16_000]);
  curve("gqlx", graphqlx, "< type > ", &[2_000, 4_000, 8_000]);
}

#[test]
#[ignore = "a measurement, not a gate"]
#[cfg(feature = "graphql")]
fn the_167_bypass_document_is_no_longer_superlinear() {
  println!("\n== #167's bypass document, which #168 quotes at 407 s ==");
  let mut prev: Option<f64> = None;
  for levels in [2_000usize, 16_000, 100_000] {
    let src = bypass(levels);
    let (secs, diags) = timed(graphql, &src);
    let ratio = prev
      .map(|p| format!("x{:.2}", secs / p))
      .unwrap_or_else(|| "     ".into());
    println!(
      "  levels={levels:7} {:8}B {:9.3}ms {ratio:>6} diagnostics={diags}",
      src.len(),
      secs * 1e3
    );
    prev = Some(secs);
  }
}
