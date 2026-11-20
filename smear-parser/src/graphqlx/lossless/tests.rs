//! The block's mode across a rewind and across a stop, read from the lexer's state.
//!
//! No tree shows either: a driver hands back what was built, and only a root written by hand holds
//! the input after the production returns. So each cell runs its own root through the door, as
//! the drivers do, and reads the state there.

use smear_lexer::limits::LosslessLimits;
use tokora::{InputRef, cache::DefaultCache, cst::Sink, error::MaybeTerminal};

use super::{
  Brand, Lexer, LexerState, TokenKind,
  runner::{LosslessEmitter, parse_lossless_document},
  value::{Constness, value},
};
use crate::lossless::depth::RootStop;

/// The lexer every cell pins, over `str`.
type Lx<'inp> = Lexer<'inp, str>;

/// The input a door's root receives: the lexer, the sink the door mints from the source, and the
/// cache.
type Inp<'inp, 'closure> = InputRef<
  'inp,
  'closure,
  Lx<'inp>,
  (
    Sink<'inp, Lx<'inp>, LosslessEmitter<'inp>>,
    DefaultCache<'inp, Lx<'inp>>,
  ),
  Brand,
>;

/// Runs `cell` as the one root of a door's parse of `src`, its lexer started in `state`, and hands
/// back what `cell` answered.
fn hold<'inp, O>(
  src: &'inp str,
  state: LexerState,
  cell: impl for<'closure> FnOnce(&mut Inp<'inp, 'closure>) -> O,
) -> O {
  let mut answer = None;
  let _ = parse_lossless_document(
    src,
    state,
    |inp: &mut Inp<'inp, '_>, _: &mut RootStop| {
      answer = Some(cell(inp));
      Ok(())
    },
    |_| {},
  );
  answer.expect("the door runs its root once")
}

/// A rewind across a block restores the lexer to its state before the block, so what is read after
/// the rewind is lexed outside the block's mode: the prefixed string the block held is a name and a
/// string again.
#[test]
fn a_rewind_across_a_block_restores_the_lexer_outside_the_block() {
  let (before, after, kinds) = hold("const { b\"x\" }", LexerState::default(), |inp| {
    let before = inp.state();
    let declined = inp.attempt(|inp| {
      let _ = value::<str, _>(inp, Constness::Schema);
      None::<()>
    });
    assert!(declined.is_none());
    let after = inp.state();
    let mut kinds: Vec<TokenKind> = Vec::new();
    while let Ok(Some(token)) = inp.next() {
      if !token.data().is_trivia() {
        kinds.push(token.data().kind());
      }
    }
    (before, after, kinds)
  });
  assert_eq!(after, before);
  assert!(!after.in_const_block());
  assert_eq!(
    kinds,
    [
      TokenKind::Identifier,
      TokenKind::LBrace,
      TokenKind::Identifier,
      TokenKind::InlineString,
      TokenKind::RBrace,
    ]
  );
}

/// A stop inside a block leaves the lexer in the block: the production writes no state over a
/// terminal error or a scanner's stop, so the stop stays one. Each of the parse's two ceilings is a
/// cell: the brackets a state may hold open, and the items the input's budget allows.
#[test]
fn a_stop_inside_a_block_leaves_the_lexer_in_the_block() {
  const SRC: &str = "const { ((((1)))) }";
  for (what, limits) in [
    // The block's `{` opens the first bracket and each `(` one more, so a ceiling of three refuses
    // a bracket inside the block, whether the lexer's count or the parse's descent meets it first.
    (
      "the nesting ceiling",
      LosslessLimits::with_max_nesting_depth(3),
    ),
    // `const`, a space and the `{` are the three items before the block, so a budget of six refuses
    // an item inside it.
    (
      "the token budget",
      LosslessLimits::new().with_max_produce_events(6),
    ),
  ] {
    let (stopped, in_block) = hold(SRC, LexerState::new(limits), |inp| {
      match value::<str, _>(inp, Constness::Schema) {
        Ok(()) => (false, inp.state().in_const_block()),
        Err(error) => (
          error.is_terminal() || inp.at_scanner_stop(),
          inp.state().in_const_block(),
        ),
      }
    });
    assert!(
      stopped,
      "{what}: the refusal inside the block ends the parse"
    );
    assert!(
      in_block,
      "{what}: the production wrote no state over the stop"
    );
  }
}
