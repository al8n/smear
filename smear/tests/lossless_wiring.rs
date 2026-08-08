#![cfg(all(feature = "rowan", feature = "graphql"))]

//! The GraphQL lossless suite's wiring guards, plus the one guard that is about the `rowan`
//! feature rather than about a dialect.
//!
//! `lossless_x_wiring.rs` is the GraphQLx twin and carries that dialect's half. The two were one
//! file until #136, and that only held while a `--features "rowan,graphqlx"` test build compiled
//! `graphql` anyway — which it did, because the self dev-dependency omitted
//! `default-features = false` and re-enabled both dialects for every test target. See that
//! manifest entry for the measurement.
//!
//! [`tokoras_cst_layer_is_in_scope`] is deliberately not duplicated into the twin. It is a claim
//! about the `rowan` feature reaching tokora, with no dialect in it, and a second copy under a
//! second header would be two statements of one fact rather than a second measurement.

use smear::lexer::graphql::lossless::LosslessToken;
use tokora::Token;

#[path = "support/trivia_probe.rs"]
mod trivia_probe;

use trivia_probe::assert_trivia_survives_lexing;

/// The wiring guard, stated where it is actually decided.
///
/// tokora's `cst::Sink` is compile-time restricted to trivia-surfacing lexers. This declaration
/// is what admits the lossless door; without it `cst::parse_lossless` will not compile.
///
/// **Why a `const` item and not a runtime `assert!`.** `SURFACES_TRIVIA` is an associated
/// *constant*, so a runtime assertion over it re-checks a question the compiler already settled
/// while building the binary that runs the check — which is what `clippy::assertions_on_constants`
/// says, and it is right. A `const` item is the same assertion moved to the moment the answer
/// exists: a false declaration is then a build failure with this message in it, in **every**
/// `--features rowan` build, rather than one red test in the one profile that runs the suite.
/// Nothing is weakened by the move; the check is strictly earlier and strictly wider.
///
/// **What it does not check is that the declaration is true.** It is a promise about behaviour
/// made as a constant, so a lexer that stops surfacing a form while still declaring `true`
/// satisfies it and satisfies tokora's compile-time totality check with it — measured against both
/// of smear#131's drop defects. [`the_lossless_lexer_surfaces_every_trivia_form`] is the
/// behavioural companion, and the two are a pair: this one is what a build cannot get past, that
/// one is what a build cannot lie to.
const _: () = assert!(
  <LosslessToken<&str> as Token<'_>>::SURFACES_TRIVIA,
  "the lossless token must declare SURFACES_TRIVIA = true"
);

/// The guard above, named so `cargo test` reports it.
///
/// The body is deliberately empty: reaching this function at all means the crate compiled, and
/// the crate compiling is the assertion. Keeping the test is not ceremony — it is what puts
/// `the_lossless_token_surfaces_trivia` in the suite's roster, so the property has a name a
/// reader can look for rather than living only in a comment.
#[test]
fn the_lossless_token_surfaces_trivia() {}

/// The behavioural companion to the declaration above, for GraphQL.
///
/// The `const _` two items up checks that the lexer *says* it surfaces trivia. This checks that it
/// does. `support/trivia_probe.rs` owns the probe and the three assertions; this test is the
/// GraphQL alphabet they are applied to.
///
/// # What it adds over what was already here, measured rather than assumed
///
/// **It is not the only witness, and the issue that asked for it expected it would be.** smear#131
/// read the `pql` result — where byte-exact round-trip and gap-free tiling both stay green when the
/// lexer stops surfacing trivia — and inferred the same hole here. It is not the same hole. Planting
/// the two drop defects against this workspace reds seventeen test binaries (whitespace skipped) and
/// eleven (comments skipped), the gap census
/// `lossless_roundtrip::every_byte_is_carried_by_a_token` among them both times, because smear's
/// sink tiles the skipped run as a `Gap` and that census forbids a gap in a cleanly accepted parse.
/// And `lossless_trivia_atoms.rs::is_trivia_covers_every_graphql_ignored_token_and_commits_it`
/// already enumerates these same eight forms and asserts each is committed byte for byte. What
/// stayed green through both defects was the *declaration*, which is the hole the issue named
/// correctly.
///
/// So three things are on offer here rather than one:
///
/// - it sits **beside the constant it is about**, so a reader of the declaration meets the
///   measurement instead of the promise;
/// - it reads the **lexer**, where the atom-level test drives a parser `InputRef` behind
///   `feature = "test-support"` — this holds for a consumer who never builds a tree;
/// - it asserts the tokens **tile the source**, which nothing else asserts at this level and which
///   is the only claim here that still answers for a form no list names.
#[test]
fn the_lossless_lexer_surfaces_every_trivia_form() {
  use smear::lexer::graphql::lossless::{LosslessLexer, LosslessTokenKind as LK};

  assert_trivia_survives_lexing::<LosslessLexer<'_, &str>>(
    "graphql",
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

#[test]
fn tokoras_cst_layer_is_in_scope() {
  // Proves the `rowan` feature actually reaches tokora, not just this crate.
  fn _assert_sink_type_exists<'inp, L: tokora::Lexer<'inp>, E>(
    _: Option<tokora::cst::Sink<'inp, L, E>>,
  ) {
  }
}

/// Each dialect exposes **three** document roots as public `fn(&str) -> Parse`, at the dialect's
/// `lossless` module and not inside `test_support`.
///
/// This is the guard smear issue #67 was filed for the absence of. Two of the three roots existed,
/// worked, and were documented as things "a schema-only consumer would call directly" while being
/// `pub(crate)` — reachable only from a driver behind `feature = "test-support"`, which is
/// compiled out of every shipped build.
///
/// # What the coercion pins that a call could not
///
/// Binding each entry as a `fn(&str) -> Parse` **pointer** fails to compile if the item is moved
/// back behind the feature gate, made `pub(crate)`, renamed, or given a different signature —
/// including the plausible regression of taking the source by something other than `&str`, which
/// a call site passing a `&'static str` would silently coerce into. The array then states the
/// other half: all three have the *same* shape, so a consumer can pick a root at run time, which
/// is exactly what `lossless_x_roundtrip.rs`'s `ROOTS` table does.
///
/// The behavioural half — that the three are three *different* parsers rather than one function
/// under three names — is `lossless_parity.rs`'s
/// `both_alternate_roots_agree_with_their_syntactic_counterparts`, which holds each against its
/// own counterpart in the syntactic suite and counts the corpus entries that separate them.
#[test]
fn the_graphql_lossless_suite_exposes_its_three_roots() {
  use smear::parser::graphql::lossless::{
    Parse, parse_document, parse_executable_document, parse_type_system_document,
  };

  let roots: [fn(&str) -> Parse; 3] = [
    parse_document,
    parse_type_system_document,
    parse_executable_document,
  ];
  assert_eq!(roots.len(), 3);
}
