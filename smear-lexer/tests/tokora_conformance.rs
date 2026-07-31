//! Drives smear's lexers through tokora's lexer conformance kit.
//!
//! `tokora::conformance::Harness` replays a set of deterministic
//! save/peek/drain/restore schedules against a `Lexer` impl and asserts the
//! trait-level contract after every operation. Two distinct things get covered
//! here:
//!
//! * the `SyntacticLexer`s are smear's own hand-written scanners, so the kit
//!   checks that smear honours the contract tokora's input machinery relies on;
//! * the `LosslessLexer`s are aliases for `tokora::lexer::LogosLexer`, so those
//!   runs exercise tokora's own Logos adapter, including gap-free tiling via
//!   `.lossless()`.

#![cfg(all(feature = "std", any(feature = "graphql", feature = "graphqlx")))]

use tokora::conformance::Harness;

/// Valid documents spanning query and schema shapes, plus small hand-written
/// edge cases (block strings, escapes, comments, commas, unicode).
const VALID: &[&str] = &[
  include_str!("../../smear/tests/fixtures/executables/bench_01_tiny_simple.graphql"),
  include_str!("../../smear/tests/fixtures/executables/bench_05_medium_fragments.graphql"),
  include_str!("../../smear/tests/fixtures/executables/kitchen-sink_canonical.graphql"),
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

fn corpus() -> Vec<&'static str> {
  VALID.iter().chain(INVALID.iter()).copied().collect()
}

#[cfg(feature = "graphql")]
mod graphql {
  use super::*;
  use smear_lexer::graphql::{lossless::LosslessLexer, syntactic::SyntacticLexer};

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
  use smear_lexer::graphqlx::{lossless::LosslessLexer, syntactic::SyntacticLexer};

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
