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
//! again pays for the tail once per token, which was the Θ(n²).
//!
//! Since the scan allowance landed these curves read ×1.5 – ×2.2 instead of ×4.0, and the
//! diagnostic counts are unchanged from the ones taken before it. The shapes are kept because a
//! ratio is only evidence next to the ratio it replaced: 32 KB of `! ` went from 11 292 ms to
//! 24.4 ms and 72 KB of `( type ) ` from 19 431 ms to 28.9 ms. The *gate* on that property is
//! `tests/resync_allowance.rs`, which asserts on produce-events rather than on the clock; this
//! file stays a measurement.
//!
//! `padded` is the axis every other shape here is blind to. All of them are one-byte atoms, so
//! bytes and produce-events move together and a guard dividing one by the other looks sound over
//! the whole set. A comment runs to end of line, so one event can carry any number of bytes: the
//! allowance's first form refilled in bytes and this shape held it fully open — **64 MB in
//! 106 189 ms with zero refusals**, growing ×7.1 in time per ×4 in bytes. Metering events against
//! events puts the same document at **158.6 ms**.
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

/// A dialect's document root, labelled: what `long_trivia_tokens_do_not_buy_allowance` iterates.
type Root = (&'static str, fn(&str) -> (usize, usize));

/// `n` repetitions of `unit`.
fn rep(unit: &str, n: usize) -> String {
  unit.repeat(n)
}

/// #167's recovery-bypass document: `{` then `levels` copies of `" ) f {"`.
///
/// Gated with its only caller. This is a GraphQL document about a GraphQL finding — #167's bypass,
/// and the 407 s figure #168 quotes for it — and GraphQLx's own bypass shape lives in
/// `nesting_depth.rs` with its fourth closer. Without the gate this is dead on the GraphQLx-only
/// row that `ci.yml`'s `test` job builds, where `-Dwarnings` makes `dead_code` an error.
#[cfg(feature = "graphql")]
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

/// `k` copies of a one-byte junk atom, each followed by a `pad`-byte comment.
///
/// Bytes grow with `pad` while produce-events do not, which is the whole point.
fn padded(atom: &str, k: usize, pad: usize) -> String {
  let comment = format!("#{}\n", "x".repeat(pad));
  let mut src = String::with_capacity(k * (atom.len() + comment.len()));
  for _ in 0..k {
    src.push_str(atom);
    src.push_str(&comment);
  }
  src
}

#[test]
#[ignore = "a measurement, not a gate"]
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
#[allow(clippy::vec_init_then_push)]
fn long_trivia_tokens_do_not_buy_allowance() {
  // Both dialects, because the axis is: a comment runs to end of line in either of them, so one
  // produce-event carries as many bytes as the document likes. Running it on GraphQL alone was
  // also what left `padded` dead on the GraphQLx-only row.
  let mut roots: Vec<Root> = Vec::new();
  #[cfg(feature = "graphql")]
  roots.push(("gql", graphql));
  #[cfg(feature = "graphqlx")]
  roots.push(("gqlx", graphqlx));

  println!("\n== one-byte junk atoms alternating with comments of growing length ==");
  println!("  the byte-denominated allowance read `refusals = 0` from pad=1024 upward");
  for (dialect, root) in &roots {
    for pad in [0usize, 16, 256, 1_024, 4_096] {
      let src = padded("! ", 4_000, pad);
      let (secs, diags) = timed(*root, &src);
      println!(
        "  {dialect:<5} pad={pad:6} {:9}B {:9.1}ms diagnostics={diags}",
        src.len(),
        secs * 1e3
      );
    }
  }
  println!("  -- padding scaled with the document, the worst case for the byte denominator --");
  for (dialect, root) in &roots {
    for k in [1_000usize, 2_000, 4_000, 8_000] {
      let src = padded("! ", k, k);
      let (secs, diags) = timed(*root, &src);
      println!(
        "  {dialect:<5} k={k:6} pad={k:6} {:10}B {:9.1}ms diagnostics={diags}",
        src.len(),
        secs * 1e3
      );
    }
  }
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
