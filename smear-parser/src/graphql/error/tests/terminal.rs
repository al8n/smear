//! The `MaybeTerminal` arm census: which errors end the parse.

use smear_lexer::{
  error::BadStateError,
  graphql::{error::LexerErrors, syntactic::SyntacticTokenKind},
};
use tokora::{SimpleSpan as Span, error::MaybeTerminal};

use crate::graphql::error::{
  ErrorData, Expectation, GraphqlError, ObjectFieldValueHint, UnexpectedEnd,
};

/// The error this census is written over, keyed the way the **lossless** door keys it: the state
/// error is a real budget rather than `()`, because that is what makes the `Lexer` arm's question
/// a real one.
type Data = ErrorData<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;

/// A stand-in for `smear_lexer::limits`'s `LimitExceeded`. The arm asks whether the *variant* is
/// `State`, not what the payload is, so any inhabited type exercises it — and using one of this
/// crate's own keeps the test out of the lexer's feature matrix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StateErr;

/// Every arm of [`ErrorData`]'s [`MaybeTerminal`] impl, asserted at the value rather than through
/// a parse.
///
/// # Why this is not an end-to-end cell
///
/// Two of the three `true` arms cannot be reached through a shipped door. A `Lexer` error only
/// becomes an `Err` on the parser's channel when the **emitter rejects** its diagnostic, and both
/// lossless doors pin `Verbose`, which never rejects; with an accepting emitter a lexer state trip
/// latches tokora's poison boundary instead, the root loop's peek answers `None`, and the loop
/// exits `Ok` with no error to classify at all. So the arm is live only for a consumer with a
/// fail-fast emitter over this dialect's own container — which is precisely the caller tokora's
/// rule tells to write it, and precisely the caller no in-tree parse is.
///
/// That was measured rather than assumed: flipping the `Lexer` arm to `false` left every cell in
/// `nesting_depth.rs` green, including the rejecting-emitter matrix, whose error type is its own
/// rather than this container. A predicate no test can redden is a predicate that will drift, so
/// the arm is pinned here, at the value.
#[test]
fn the_terminal_arms_answer_for_every_variant() {
  let span = Span::new(0, 1);

  // ── `true`: a limit the runtime refused ──
  assert!(
    Data::NestingLimitExceeded.is_terminal(),
    "a frame budget is never cleared by more input"
  );
  assert!(
    GraphqlError::<&str>::nesting_limit_exceeded(span)
      .into_data()
      .is_terminal(),
    "the constructor `descend` reaches the variant through must agree with the arm"
  );

  let tripped: LexerErrors<char, StateErr> = LexerErrors::bad_state(span, StateErr);
  assert!(
    Data::Lexer(tripped).is_terminal(),
    "a `State` refusal in the lexer arm is a spent budget: the arm tokora's rule names, and the \
     one a scanner trip lands on unmarked when the emitter rejects its diagnostic"
  );

  // ── `false`: the same arm holding malformed input rather than a refused limit ──
  let malformed: LexerErrors<char, StateErr> = LexerErrors::default();
  assert!(
    !Data::Lexer(malformed).is_terminal(),
    "an empty lexer-error container holds no refusal, so the arm must not answer on the variant \
     alone"
  );

  // ── delegated: the value decides, not the variant ──
  let hint = UnexpectedEnd::with_name(
    0usize,
    tokora::utils::CowStr::from_static("object field value"),
    ObjectFieldValueHint::Value,
  );
  assert!(
    !Data::UnexpectedEndOfObjectFieldValue(hint.clone()).is_terminal(),
    "a production that ran out of input is a grammar rejection"
  );
  assert!(
    Data::UnexpectedEndOfObjectFieldValue(hint.into_terminal()).is_terminal(),
    "the same variant is terminal when the scanner raised the flag on it"
  );

  // ── `false`, and affirmatively so: built from what the grammar rejected ──
  assert!(
    !Data::EndOfInput.is_terminal(),
    "see the impl's last section"
  );
  assert!(!Data::Other(std::borrow::Cow::Borrowed("x")).is_terminal());
}
