//! The `MaybeTerminal` arm census: which errors end the parse.

use smear_lexer::{
  error::BadStateError,
  graphql::{error::LexerErrors, syntactic::SyntacticTokenKind},
};
use tokora::{SimpleSpan as Span, error::MaybeTerminal};

use crate::graphql::error::{
  Error, ErrorData, Errors, Expectation, GraphqlError, ObjectFieldValueHint, UnexpectedEnd,
};

/// The error this census is written over, keyed the way the **lossless** door keys it: the state
/// error is a real budget rather than `()`, because that is what makes the `Lexer` arm's question
/// a real one.
type Data = ErrorData<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;
type Err = Error<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;
type Errs = Errors<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;

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

  assert!(
    Data::TokenBudgetExhausted.is_terminal(),
    "a durable token budget that has refused is never cleared by more input: the tally is \
     monotone, outside every rollback, and has no public mutator"
  );
  assert!(
    GraphqlError::<&str>::token_budget_exhausted(span)
      .into_data()
      .is_terminal(),
    "the constructor `drain_unless_stopped` reaches the variant through must agree with the arm — \
     and this arm carries more weight than the descent one, because tokora refuses the item \
     silently and this value is the only report a refused document has"
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

/// The container fold is `any`, so a stop recorded beside an ordinary diagnostic is still a stop.
///
/// # Why the census above cannot ask this, and why nothing that parses can either
///
/// [`ErrorData`]'s arms are what the census covers. Two more [`MaybeTerminal`] impls sit above
/// them — [`Error`]'s delegation and [`Errors`]'s fold — and each is hand-written per dialect,
/// with its own way of being wrong. The fold's is `all` where it says `any`, and **no end-to-end
/// cell can see that one.** Every conversion `lossless_error_impls!` generates ends in
/// `…(span).into()`, and `From<Error> for Errors` is `core::iter::once(error).collect()`, so
/// every container the lossless machinery puts on the parser's error channel holds exactly one
/// error — the one length at which `any` and `all` agree.
///
/// [`Error`]'s delegation is **not** in that position and gets no cell of its own: it is on the
/// path every document-root catch site takes, so `nesting_depth.rs` pins it. The two
/// `Error`-level assertions below are this fold's premise rather than a pin — a fold cannot be
/// read without knowing what its elements answer.
///
/// # This is GraphQLx's twin, and the pairing is the point
///
/// `graphqlx/error/tests/terminal.rs`'s cell of this name came first, and reporting it left the
/// GraphQL half knowingly open. That is the exact shape smear issue #169 is a repair for — a fix
/// that lands in one file and dies there — so leaving it open inside the branch that exists to
/// end that shape was not available. The two impls are verbatim twins over disjoint variant sets;
/// the plants below were run against **this** one, because a plant on the sibling proves the
/// sibling.
///
/// # The multi-element container is reachable, so the fold is a contract and not dead code
///
/// [`Errors`] is public, with [`Extend`], `DerefMut<Target = Vec<_>>` and `From<Vec<_>>`, and
/// tokora's `ParseContext` is caller-implemented. An accumulating context is exactly the consumer
/// the fold's own note describes: the one whose real stop `all` would spend the moment one
/// ordinary diagnostic was recorded beside it.
///
/// # Three cells, three plants, because `all` reddens only the first of them
///
/// `all` fails this test at `[ordinary, stop]` and libtest stops there, which says nothing about
/// the two cells after it — so each was planted on its own, with a fold that is `any` everywhere
/// else:
///
/// * `self.0.is_empty() || …any(…)` → *an empty container holds no stop*. This is the shape
///   `all`'s vacuous truth takes, and it is the cell most easily written and never exercised.
/// * `self.0.last().is_some_and(…)` → *and so is one recorded before it*. A fold that reads one
///   position rather than the set passes both `[ordinary, stop]` and the empty case.
#[test]
fn the_container_fold_keeps_a_stop_that_is_not_alone() {
  fn container(errors: impl IntoIterator<Item = Err>) -> Errs {
    let mut collected = Errs::default();
    collected.extend(errors);
    collected
  }

  let span = Span::new(0, 1);
  let stop = Err::nesting_limit_exceeded(span);
  let ordinary = Err::unclosed_list(span);

  // The premise: what each element answers alone, through `Error`'s delegation to its data.
  assert!(
    stop.is_terminal(),
    "the refusal is the stop this fold exists to keep"
  );
  assert!(
    !ordinary.is_terminal(),
    "an unclosed list is a grammar rejection, not a stop"
  );

  // `n = 1` — the only length any in-tree parse reaches, and where `any` and `all` agree. Listed
  // so that the cells below are visibly the ones doing the work.
  assert!(container([stop.clone()]).is_terminal());
  assert!(!container([ordinary.clone()]).is_terminal());

  // `n = 2`, both orders. `all` answers `false` for each of these, and position must not decide.
  assert!(
    container([ordinary.clone(), stop.clone()]).is_terminal(),
    "a stop recorded after an ordinary diagnostic is still a stop"
  );
  assert!(
    container([stop, ordinary.clone()]).is_terminal(),
    "and so is one recorded before it"
  );
  assert!(
    !container([ordinary.clone(), ordinary]).is_terminal(),
    "two grammar rejections do not add up to a stop"
  );

  // `n = 0`. `all` answers `true` here, which would end a parse that had reported nothing.
  assert!(
    !Errs::default().is_terminal(),
    "an empty container holds no stop"
  );
}
