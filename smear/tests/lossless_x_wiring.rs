#![cfg(all(feature = "rowan", feature = "graphqlx"))]

//! The GraphQLx lossless suite's wiring guards — the twin of `lossless_wiring.rs`.
//!
//! # Why this is a file and not four items in the GraphQL one
//!
//! It was four items in the GraphQL one, under `#![cfg(all(feature = "rowan", feature =
//! "graphql"))]` and an inner `#[cfg(feature = "graphqlx")]` each. That reads as "compiled when
//! both dialects are on", and it is — but the build these guards exist for is
//! `--features "rowan,graphqlx"`, where the *header* is false and every one of them is compiled
//! away. Nothing said so, because `smear`'s self dev-dependency was missing
//! `default-features = false`, so `graphql` was unconditionally on for every test target and the
//! narrow build did not exist to be wrong about. #136 made it exist; this file is what these four
//! guards need in order to survive it.
//!
//! The dialect-independent guard stays where it was. `lossless_wiring.rs`'s
//! `tokoras_cst_layer_is_in_scope` is about the `rowan` feature reaching tokora and names no
//! dialect, so a copy here would be a second statement of one fact rather than a second
//! measurement.

use tokora::Token;

#[path = "support/trivia_probe.rs"]
mod trivia_probe;

use trivia_probe::assert_trivia_survives_lexing;

/// The wiring guard, stated where it is actually decided.
///
/// Phase B found this missing: `smear/src/lexer/graphqlx/lossless/token.rs`'s two `tokora::Token`
/// impls define `Kind`, `Error`, `kind` and `is_trivia` and stop there, so the associated constant
/// took tokora's default of `false` and a GraphQLx `Sink` would have failed to build — as an
/// `E0080` from inside tokora, a long way from the declaration that caused it. This states the
/// requirement where a reader of the parser can find it.
///
/// `lossless_wiring.rs`'s GraphQL constant carries the argument for why this is a `const` item
/// rather than a runtime `assert!`, and for what it does and does not check.
const _: () = assert!(
  <smear::lexer::graphqlx::lossless::LosslessToken<&str> as Token<'_>>::SURFACES_TRIVIA,
  "the GraphQLx lossless token must declare SURFACES_TRIVIA = true"
);

/// The guard above, named so `cargo test` reports it.
#[test]
fn the_graphqlx_lossless_token_surfaces_trivia() {}

/// The behavioural companion to the declaration above, for GraphQLx.
///
/// The same eight forms, and the same reason: the GraphQLx lexer is a separate `Logos` derive over
/// a wider token set, so it can lose a form on its own — the GraphQL probes leave this test green,
/// which is the measurement that says the two are independent. `support/trivia_probe.rs` owns the
/// probe and the three assertions both dialects are held to; see
/// `lossless_wiring.rs`'s `the_lossless_lexer_surfaces_every_trivia_form` for what they add over
/// `lossless_x_trivia_atoms.rs`'s enumeration of the same eight.
#[test]
fn the_graphqlx_lossless_lexer_surfaces_every_trivia_form() {
  use smear::lexer::graphqlx::lossless::{LosslessLexer, LosslessTokenKind as LK};

  assert_trivia_survives_lexing::<LosslessLexer<'_, &str>>(
    "graphqlx",
    &[
      ("space", LK::Space),
      ("tab", LK::Tab),
      ("newline", LK::Newline),
      ("carriage return", LK::CarriageReturn),
      ("CRLF", LK::CarriageReturnAndNewline),
      ("comment", LK::Comment),
      ("comma", LK::Comma),
      ("BOM", LK::Bom),
    ],
  );
}

/// The GraphQLx twin of `lossless_wiring.rs`'s
/// `the_graphql_lossless_suite_exposes_its_three_roots`, which carries the argument for what the
/// `fn(&str) -> Parse` coercion pins that a call could not.
///
/// This one is the guard `lossless_x_roundtrip.rs`'s `ROOTS` table depends on: that table picks a
/// root at run time, which is only possible because all three have the same shape.
#[test]
fn the_graphqlx_lossless_suite_exposes_its_three_roots() {
  use smear::parser::graphqlx::lossless::{
    Parse, parse_document, parse_executable_document, parse_type_system_document,
  };

  let roots: [fn(&str) -> Parse; 3] = [
    parse_document,
    parse_type_system_document,
    parse_executable_document,
  ];
  assert_eq!(roots.len(), 3);
}
