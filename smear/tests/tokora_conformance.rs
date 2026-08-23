//! Drives smear's lexers through tokora's lexer conformance kit.
//!
//! `tokora::conformance::Harness` replays a set of deterministic
//! save/peek/drain/restore schedules against a `Lexer` impl and asserts the
//! trait-level contract after every operation. Two distinct things get covered
//! here:
//!
//! * the `SyntacticLexer`s are smear's own hand-written scanners, so the kit
//!   checks that smear honours the contract tokora's input machinery relies on;
//! * the `LosslessLexer`s are smear newtypes that delegate every `Lexer` item
//!   to `tokora::lexer::LogosLexer`, so those runs exercise tokora's own Logos
//!   adapter *and* the delegation over it, including gap-free tiling via
//!   `.lossless()`. They were bare aliases for `LogosLexer` until the newtype
//!   took away the three accessors that handed out the raw `logos::Lexer`; the
//!   kit is what says the delegation still answers as the adapter does.

#![cfg(all(feature = "std", any(feature = "graphql", feature = "graphqlx")))]

use tokora::conformance::Harness;

/// Valid documents spanning query and schema shapes, plus small hand-written
/// edge cases (block strings, escapes, comments, commas, unicode).
const VALID: &[&str] = &[
  include_str!("fixtures/executables/bench_01_tiny_simple.graphql"),
  include_str!("fixtures/executables/bench_05_medium_fragments.graphql"),
  include_str!("fixtures/executables/kitchen-sink_canonical.graphql"),
  "",
  "   ",
  "\n\t\r\n",
  "# just a comment",
  "# comment with no trailing newline",
  "{a}",
  "{ a b c }",
  "query Q($v: Int = 3) @dir(a: 1.5e-3) { f(x: \"s\") { ...F } }",
  "\"\"\"block\nstring\"\"\"",
  "\"\"\"esc \\\"\"\" done\"\"\"",
  "\"inline \\u0041 \\n \\\\ \\\"\"",
  "\"\"\"\"\"\"",
  "1 -1 0 1.0 1e10 -1.5E-10",
  "a,b,,,c",
  "\u{feff}{ a }",
  "{ \u{00e9}\u{4e2d}\u{6587} }",
];

/// Malformed inputs: the contract must hold on the error paths too.
const INVALID: &[&str] = &[
  "\"unterminated",
  "\"\"\"unterminated block",
  "\"bad escape \\q\"",
  "\"\\u00\"",
  "1.2.3",
  "1e",
  "-",
  "..",
  "....",
  "$",
  "@",
  "!@#$%^&*",
  "\"\\u{110000}\"",
  "\u{0000}\u{0001}",
];

/// Sources whose *complete* parse is one long item but whose truncations are shorter accepted
/// ones — the only shape that can falsify a `Token::SCAN_LOOKAHEAD` / `Lexer::read_frontier`
/// claim, and the shape neither list above has.
///
/// `run_partial` audits a corpus, not a vocabulary: with `Unbounded` on every lexer here nothing
/// is committed while a stream is open, so these cells pass and are meant to. What they are for
/// is the *next* edit — narrowing either answer to `WithinSpan`/`SpanEnd` reds `-.5` at split
/// `k = 2` in all four lexers, because `-` is an accepting rule of its own, `-.` is accepted by
/// nothing, and `-.5` is one float, so the prefix commits an item at `0..1` the complete parse
/// does not have. Without these cells that narrowing is green: the 33 sources above contain no
/// gap between an accepting prefix and the longer rule that swallows it.
const FRONTIER: &[&str] = &[
  "-.5", "-.", "-.5e3", ".5", "0.", "-0.5", "1.0", "1e10", "1.5e-3", "0x1F", "0x1.8p3", "0b10",
];

fn corpus() -> Vec<&'static str> {
  VALID
    .iter()
    .chain(INVALID.iter())
    .chain(FRONTIER.iter())
    .copied()
    .collect()
}

#[cfg(feature = "graphql")]
mod graphql {
  use super::*;
  use smear::lexer::graphql::{lossless::LosslessLexer, syntactic::SyntacticLexer};

  #[test]
  fn syntactic_conformance() {
    Harness::<SyntacticLexer<'_, str>>::over(corpus()).run();
  }

  #[test]
  fn syntactic_conformance_partial() {
    Harness::<SyntacticLexer<'_, str>>::over(corpus()).run_partial();
  }

  /// The lossless stream tiles the source with no gaps, so the strict knob applies.
  #[test]
  fn lossless_conformance() {
    Harness::<LosslessLexer<'_>>::over(corpus())
      .lossless()
      .run();
  }

  #[test]
  fn lossless_conformance_partial() {
    Harness::<LosslessLexer<'_>>::over(corpus())
      .lossless()
      .run_partial();
  }
}

#[cfg(feature = "graphqlx")]
mod graphqlx {
  use super::*;
  use smear::lexer::graphqlx::{lossless::LosslessLexer, syntactic::SyntacticLexer};

  #[test]
  fn syntactic_conformance() {
    Harness::<SyntacticLexer<'_, str>>::over(corpus()).run();
  }

  #[test]
  fn syntactic_conformance_partial() {
    Harness::<SyntacticLexer<'_, str>>::over(corpus()).run_partial();
  }

  #[test]
  fn lossless_conformance() {
    Harness::<LosslessLexer<'_>>::over(corpus())
      .lossless()
      .run();
  }

  #[test]
  fn lossless_conformance_partial() {
    Harness::<LosslessLexer<'_>>::over(corpus())
      .lossless()
      .run_partial();
  }
}
