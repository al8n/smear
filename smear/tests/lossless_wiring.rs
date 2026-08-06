#![cfg(feature = "rowan")]

use smear::lexer::graphql::lossless::LosslessToken;
use tokora::Token;

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

/// The GraphQLx twin of the guard above.
///
/// Phase B found this missing: `smear/src/lexer/graphqlx/lossless/token.rs`'s two `tokora::Token`
/// impls define `Kind`, `Error`, `kind` and `is_trivia` and stop there, so the associated constant
/// took tokora's default of `false` and a GraphQLx `Sink` would have failed to build — as an
/// `E0080` from inside tokora, a long way from the declaration that caused it. This states the
/// requirement where a reader of the parser can find it.
#[cfg(feature = "graphqlx")]
const _: () = assert!(
  <smear::lexer::graphqlx::lossless::LosslessToken<&str> as Token<'_>>::SURFACES_TRIVIA,
  "the GraphQLx lossless token must declare SURFACES_TRIVIA = true"
);

/// The guard above, named so `cargo test` reports it.
#[cfg(feature = "graphqlx")]
#[test]
fn the_graphqlx_lossless_token_surfaces_trivia() {}

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
#[cfg(feature = "graphql")]
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

/// The GraphQLx twin of the guard above.
#[cfg(feature = "graphqlx")]
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
