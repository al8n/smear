#![cfg(feature = "rowan")]

use smear_lexer::graphql::lossless::LosslessToken;
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

#[test]
fn tokoras_cst_layer_is_in_scope() {
  // Proves the `rowan` feature actually reaches tokora, not just this crate.
  fn _assert_sink_type_exists<'inp, L: tokora::Lexer<'inp>, E>(
    _: Option<tokora::cst::Sink<'inp, L, E>>,
  ) {
  }
}
