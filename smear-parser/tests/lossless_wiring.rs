#![cfg(feature = "rowan")]

use smear_lexer::graphql::lossless::LosslessToken;
use tokora::Token;

#[test]
fn the_lossless_token_surfaces_trivia() {
  // tokora's `cst::Sink` is compile-time restricted to trivia-surfacing lexers. This
  // declaration is what admits the lossless door; without it `Sink::new` will not compile.
  assert!(
    <LosslessToken<&str> as Token<'_>>::SURFACES_TRIVIA,
    "the lossless token must declare SURFACES_TRIVIA = true"
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
