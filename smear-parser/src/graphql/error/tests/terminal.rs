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

  // ── the two ends of input, and the pair is the whole of smear issue #177 ──
  //
  // One carrier with two readings, split into two variants because tokora builds both at the same
  // offset and only the variant can carry the difference this far. Asserted TOGETHER: the repair
  // the impl's own note refuses — making the terminal case answer `true` by making *every* end of
  // input answer `true` — passes the first of these two cells and fails the third.
  assert!(
    Data::TerminalEndOfInput.is_terminal(),
    "an end of input standing in for a scanner stop is a stop: no input clears a tripped limit"
  );
  assert!(
    GraphqlError::<&str>::terminal_end_of_input(span)
      .into_data()
      .is_terminal(),
    "the constructor both `From<UnexpectedEot>` impls route to on a marked value must agree with \
     the arm"
  );
  assert!(
    !Data::EndOfInput.is_terminal(),
    "a genuine end of input is a grammar rejection and stays recoverable"
  );
  assert!(
    !GraphqlError::<&str>::unexpected_end_of_input(span)
      .into_data()
      .is_terminal(),
    "and so must the constructor the unmarked arm of both conversions reaches"
  );

  // ── `false`, and affirmatively so: built from what the grammar rejected ──
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

/// A **real parse** reaches the marked end of input, and the split is what keeps the mark.
///
/// # Why the census above is not this claim
///
/// It asks what an arm answers for a value. This asks whether the value exists: `is_terminal()` is
/// tokora's flag, raised inside tokora, on a value tokora builds — so a conversion that reads only
/// `offset()` produces an enum that is *internally* consistent, passes every arm census, and is
/// wrong about every document. Nothing at the value level can tell.
///
/// So this drives a parse. A [`TokenBudget`](tokora::input::TokenBudget) refusal latches tokora's
/// poison boundary, the next `peek_kind` meets it, and the end-of-input error it raises is the
/// marked one — the [`into_terminal`](tokora::error::UnexpectedEnd::into_terminal) path tokora's
/// own docs place at "the attempt/decline leaves and the delimited close". `peek_kind` is not an
/// exotic entry point: it is what every one of this dialect's trivia atoms is built on, and its
/// `Error: From<UnexpectedEot<L::Offset, Lang>>` bound is the reason the conversion under test
/// exists at all.
///
/// # The emitter has to accept, and that is the whole reachability condition
///
/// [`Verbose`](tokora::emitter::Verbose) — what both lossless doors pin. tokora reaches
/// `into_terminal` only once the trip's own diagnostic has been **accepted**; a rejecting emitter
/// propagates its own value from the scan instead and builds no `UnexpectedEnd` for the mark to
/// ride on. That case is unrouteable by construction and is the residual the [`MaybeTerminal`]
/// impl's closing note now records against the [`Lexer`](ErrorData::Lexer) arm.
///
/// # Measured before it was pinned — smear issue #177
///
/// At the base commit every budget below produced [`ErrorData::EndOfInput`] with
/// `is_terminal()` answering `false`, on a value tokora had marked `true`. The same sweep run
/// against the lossless door — `parse_document_with_limits` under
/// `LosslessLimits::with_max_tokens(n)`, `n` in `1..40`, over eight documents — reached the
/// conversion with the mark set at **every** ceiling, twice per parse: once from the failing
/// entry and once from the resynchronisation the root then attempted.
#[test]
fn a_scanner_stop_reaches_this_dialect_marked_and_the_split_keeps_the_mark() {
  use tokora::{
    InputRef, ParserContext, cache::DefaultCache, emitter::Verbose, error::UnexpectedEot,
    input::TokenBudget,
  };

  use crate::graphql::{GraphQL, error::GraphqlErrors, syntactic::GraphqlLexer};

  type Lx<'inp> = GraphqlLexer<'inp, str>;
  type Em<'inp> = Verbose<GraphqlErrors<&'inp str>, Span, GraphQL>;
  type Cx<'inp> = ParserContext<'inp, Lx<'inp>, Em<'inp>, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  /// Walks the document a token at a time, peeking before each one — the shape every trivia atom
  /// in this dialect has, reduced to the two calls that matter.
  fn walk<'inp>(src: &'inp str, budget: usize) -> Result<(), GraphqlErrors<&'inp str>> {
    tokora::parse_with::<Lx<'inp>, str, _, (), Cx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Cx<'inp>, GraphQL>| {
        while inp.peek_kind()?.is_some() {
          inp.next()?;
        }
        Ok(())
      },
      src,
      ParserContext::of(Verbose::default()).with_token_budget(TokenBudget::with_limitation(budget)),
    )
  }

  let src = "query A { a { b { c } } } query B { d } query C { e }";

  // Every ceiling the document reaches, so the finding is not one arithmetic coincidence. 20 is
  // the last one under the document's own cost; the control below is the first one over it.
  let mut cells = 0usize;
  for budget in [1usize, 2, 3, 4, 8, 12, 20] {
    let errors = walk(src, budget).expect_err("the budget refuses an item well inside the source");
    let error = errors
      .iter()
      .next()
      .expect("the conversion emits exactly one error");
    assert!(
      matches!(error.data(), ErrorData::TerminalEndOfInput),
      "budget={budget}: a scanner stop reached the conversion and came back as {:?} — the mark was \
       discarded, which is the whole of smear issue #177",
      error.data(),
    );
    assert!(
      errors.is_terminal(),
      "budget={budget}: the value a document root reads to decide whether to resynchronise"
    );
    cells += 1;
  }
  assert_eq!(cells, 7, "the cell set collapsed");

  // THE CONTROL, and it is the half a blanket `true` would break: a budget nothing reaches leaves
  // the walk to run out of document, and running out of document is not a stop.
  assert!(
    walk(src, 10_000).is_ok(),
    "a budget no parse reaches must not end anything"
  );

  // AND THE UNMARKED VALUE THROUGH THE SAME CONVERSION. tokora hands the two ends of input to one
  // `From` impl at one offset; this is the arm the marked cells above must not have swallowed.
  let ordinary: GraphqlErrors<&str> = UnexpectedEot::<usize, GraphQL>::eot_of(11).into();
  let ordinary = ordinary
    .iter()
    .next()
    .expect("the conversion emits exactly one error");
  assert!(
    matches!(ordinary.data(), ErrorData::EndOfInput),
    "an unmarked end of input must still be the recoverable variant: {:?}",
    ordinary.data(),
  );
  assert!(
    !ordinary.is_terminal(),
    "and must still answer `false`, or every recovery in the crate ends at the first short read"
  );
}

/// The two places **smear** has to make the mark, rather than keep one tokora made — smear issue
/// #177, Codex round 1.
///
/// # Keeping a flag only works where a flag was set
///
/// [`a_scanner_stop_reaches_this_dialect_marked_and_the_split_keeps_the_mark`] pins the routing:
/// given a marked `UnexpectedEot`, the conversion keeps the mark. That says nothing about the
/// paths where **no** marked value is ever built, and there were two.
///
/// * **A committed read written as `next()` or a raw `peek`.** tokora's contract folds a resource
///   trip and a genuine end of input into one `Ok(None)`; a leaf that turns `None` into an
///   end-of-input error therefore synthesises the *plain* one and the routing above has nothing to
///   keep. `next_or_stop` and `peek_head_map` are the terminal-aware primitives that split the
///   fold, and `next_or_stop`'s own doc calls this "the false negative it exists for". The
///   syntactic trees now use them at every committed read: 33 `next` sites and 20 raw head peeks.
/// * **A lexer error a rejecting emitter hands back.** On that path tokora builds no
///   `UnexpectedEnd` at all — it converts the lexer batch through `From<LexerErrors>` — and that
///   conversion erased the batch to `Other`, which is unconditionally recoverable.
///
/// # Both are on shipped doors, which is why the cells go through them
///
/// The budget half runs `IntValue::graphql` and `Name::graphql`, the two public productions Codex
/// named, under a [`TokenBudget`](tokora::input::TokenBudget) ceiling. The lexer half runs under
/// `Fatal` — the context every syntactic production in this crate is tested and shipped under, and
/// a **rejecting** emitter by construction — with the lexer's own nesting ceiling at zero, so the
/// first bracket is a real `smear_lexer::limits` refusal rather than a manufactured one.
///
/// Measured before the repair, at `e5eb462`: every terminal assertion below answered `false`, the
/// budget cells returning `EndOfInput` and the lexer cells `Other("lexer error")` at span `0..0`.
#[test]
fn the_marks_smear_has_to_make_itself_reach_the_caller() {
  use smear_lexer::limits::SyntacticLimits;
  use tokora::{
    FatalContext, Parse as _, Parser, ParserContext, cache::DefaultCache, emitter::Verbose,
    input::TokenBudget,
  };

  use crate::graphql::{
    GraphQL,
    ast::{InputValue, InputValueDefinition, IntValue, Name as AstName},
    error::GraphqlErrors,
    syntactic::{GraphqlInput, GraphqlLexer},
  };

  type Lx<'inp> = GraphqlLexer<'inp, str>;
  /// The shipped syntactic context: `Fatal`, which rejects every emission.
  type Fx<'inp> = FatalContext<'inp, Lx<'inp>, GraphqlErrors<&'inp str>, GraphQL>;
  /// An accepting context, so the budget half is about the read rather than about the emitter.
  type Vx<'inp> = ParserContext<
    'inp,
    Lx<'inp>,
    Verbose<GraphqlErrors<&'inp str>, Span, GraphQL>,
    DefaultCache<'inp, Lx<'inp>>,
    GraphQL,
  >;

  fn fatal<'inp, O>(
    f: impl for<'c> FnMut(
      &mut GraphqlInput<'inp, 'c, str, Fx<'inp>>,
    ) -> Result<O, GraphqlErrors<&'inp str>>,
    src: &'inp str,
    state: SyntacticLimits,
  ) -> Result<O, GraphqlErrors<&'inp str>> {
    Parser::with_parser::<'inp, Lx<'inp>, O, GraphqlErrors<&'inp str>, _, GraphQL>(f)
      .parse_str_with_state(src, state)
  }

  fn budgeted<'inp, O>(
    f: impl for<'c> FnMut(
      &mut GraphqlInput<'inp, 'c, str, Vx<'inp>>,
    ) -> Result<O, GraphqlErrors<&'inp str>>,
    src: &'inp str,
    budget: usize,
  ) -> Result<O, GraphqlErrors<&'inp str>> {
    Parser::with_parser_and_context::<'inp, Lx<'inp>, O, Vx<'inp>, _, GraphQL>(
      f,
      ParserContext::of(Verbose::default()).with_token_budget(TokenBudget::with_limitation(budget)),
    )
    .parse_str(src)
  }

  // ── the committed read: `next_or_stop` at a value leaf, `peek_head_map` at a phase guard ──
  //
  // `IntValue` reaches the budget through `next()`'s replacement and `Name` through the definition
  // phase guard's, so one cell covers both classes the census splits.
  let mut cells = 0usize;
  for (what, errors) in [
    (
      "IntValue::graphql",
      budgeted(|inp| IntValue::<_>::graphql(inp).map(|_| ()), "123", 0)
        .expect_err("a budget of zero refuses the first item"),
    ),
    (
      "Name::graphql",
      budgeted(|inp| AstName::<_>::graphql(inp).map(|_| ()), "Foo", 0)
        .expect_err("a budget of zero refuses the first item"),
    ),
    // THE `(b)` CLASS ON ITS OWN, and it needs its own door. `Name` and `IntValue` both reach the
    // budget through a `next`, so reverting one raw peek left them green — measured, which is how
    // this row exists. `InputValueDefinition` opens on `guard_definition_phase`, whose head read is
    // the peek, and it is the shallowest public production that does.
    (
      "InputValueDefinition::graphql",
      budgeted(
        |inp| InputValueDefinition::<_>::graphql(inp).map(|_| ()),
        "a: Int",
        0,
      )
      .expect_err("a budget of zero refuses the first item"),
    ),
  ] {
    let data = errors
      .iter()
      .next()
      .expect("the conversion emits exactly one error")
      .data();
    assert!(
      matches!(data, ErrorData::TerminalEndOfInput),
      "{what}: a refused item reached a public production as {data:?} — the read folded the trip \
       into an absence before the end-of-input routing could see it"
    );
    assert!(errors.is_terminal(), "{what}");
    cells += 1;
  }

  // THE CONTROL. A budget the document does not reach must leave the production alone; a blanket
  // `Err` from the new primitives would pass every assertion above and fail here.
  assert!(
    budgeted(|inp| IntValue::<_>::graphql(inp).map(|_| ()), "123", 8).is_ok(),
    "a budget no parse reaches must not refuse anything"
  );

  // ── the rejected lexer error: `From<LexerErrors>` inspects the batch ──
  //
  // A nesting ceiling of zero, so the opening bracket is the lexer's own `State` refusal. Three
  // sources because the refusal is raised by the depth counter rather than by any one pair.
  for src in ["{ a: 1 }", "[1]", "(1)"] {
    let errors = fatal(
      |inp| InputValue::<_>::graphql(inp).map(|_| ()),
      src,
      SyntacticLimits::with_max_nesting_depth(0),
    )
    .expect_err("a nesting ceiling of zero refuses the first bracket");
    let error = errors
      .iter()
      .next()
      .expect("the conversion emits exactly one error");
    assert!(
      matches!(error.data(), ErrorData::TerminalEndOfInput),
      "{src:?}: a rejected `State` refusal reached the caller as {:?}",
      error.data()
    );
    assert!(errors.is_terminal(), "{src:?}");
    assert_eq!(
      (error.span().start(), error.span().end()),
      (0, 1),
      "{src:?}: the batch's first span, not `0..0` — the refusal is at the bracket the lexer \
       refused to open"
    );
    cells += 1;
  }

  // THE CONTROL, and it is the half that says this is an inspection rather than a rewrite: an
  // ordinary lexical error through the same rejecting emitter stays recoverable AND keeps its span.
  for (src, end) in [("\u{1}", 1usize), ("\"unterminated", 13)] {
    let errors = fatal(
      |inp| InputValue::<_>::graphql(inp).map(|_| ()),
      src,
      SyntacticLimits::default(),
    )
    .expect_err("neither source lexes");
    let error = errors
      .iter()
      .next()
      .expect("the conversion emits exactly one error");
    assert!(
      matches!(error.data(), ErrorData::Other(note) if note == "lexer error"),
      "{src:?}: a malformed lexeme is a grammar rejection, not a scanner stop: {:?}",
      error.data()
    );
    assert!(
      !errors.is_terminal(),
      "{src:?}: recovering past a malformed lexeme is the whole point of recovery"
    );
    assert_eq!(
      (error.span().start(), error.span().end()),
      (0, end),
      "{src:?}: the batch's first span. `0..0` pointed every lexer error at byte zero"
    );
    cells += 1;
  }

  assert_eq!(cells, 8, "the cell set collapsed");
}

/// Every attempt and optional probe in this dialect raises the stop instead of declining — smear
/// issue #177, Codex round 2.
///
/// # What round 2 missed
///
/// Its census was derived over the two primitive **names** round 1 had mentioned, `next` and a raw
/// `peek`. `try_expect` and `try_expect_map` fold a stop into the same `Ok(None)` those do, and
/// tokora says so in the primitives' own docs: *"`Ok(None)` also covers a terminal stop (limit trip
/// / latched poison boundary); when a decline commits the caller to a different parse, use
/// `try_expect_or_stop`"* (`tokora-0.10.0/src/input/input_ref/try_expect.rs:262`). Two consequences
/// reached public doors:
///
/// * the six `try_graphql` scalar attempts returned `Ok(Decline)` on a refusal, so a caller could
///   select a different alternative — or recover — after the scanner had stopped;
/// * `peeks_where`, whose body was an always-declining `try_expect`, answered `false`, and
///   `variables_definition` reads that as "no `(`" and returns a successful **empty**
///   `VariablesDefinition`. tokora's `head_satisfies` names that hack in its own doc and replaces
///   it.
///
/// # The controls are what make this a split rather than a blanket `Err`
///
/// A decline on an ordinary mismatch must still be a decline, and must still **consume nothing** —
/// the committed end is asserted, not the lexer's scan frontier, because a declining `try_expect`
/// leaves the token at the cache front with the scanner already past it.
#[test]
fn every_attempt_and_optional_read_raises_the_stop_instead_of_declining() {
  use tokora::{
    Parse as _, Parser, ParserContext, cache::DefaultCache, emitter::Verbose, input::TokenBudget,
    try_parse_input::ParseAttempt,
  };

  use crate::graphql::{
    GraphQL,
    ast::{
      BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue, VariablesDefinition,
    },
    error::GraphqlErrors,
    syntactic::{GraphqlInput, GraphqlLexer},
  };

  type Lx<'inp> = GraphqlLexer<'inp, str>;
  type Vx<'inp> = ParserContext<
    'inp,
    Lx<'inp>,
    Verbose<GraphqlErrors<&'inp str>, Span, GraphQL>,
    DefaultCache<'inp, Lx<'inp>>,
    GraphQL,
  >;

  fn budgeted<'inp, O>(
    f: impl for<'c> FnMut(
      &mut GraphqlInput<'inp, 'c, str, Vx<'inp>>,
    ) -> Result<O, GraphqlErrors<&'inp str>>,
    src: &'inp str,
    budget: usize,
  ) -> Result<O, GraphqlErrors<&'inp str>> {
    Parser::with_parser_and_context::<'inp, Lx<'inp>, O, Vx<'inp>, _, GraphQL>(
      f,
      ParserContext::of(Verbose::default()).with_token_budget(TokenBudget::with_limitation(budget)),
    )
    .parse_str(src)
  }

  /// Asserts one door is terminal at every ceiling below the first that lets it through, and
  /// returns that ceiling. The source is chosen so the production consumes all of it, so every
  /// ceiling below is a real refusal inside the parse.
  macro_rules! sweep {
    ($label:literal, $src:literal, $probe:literal, $call:expr) => {{
      let mut first_ok = None;
      for budget in 0..48usize {
        match budgeted($call, $src, budget) {
          Ok(_) => {
            first_ok = Some(budget);
            break;
          }
          Err(errors) => {
            let data = errors
              .iter()
              .next()
              .expect("the conversion emits exactly one error")
              .data();
            assert!(
              matches!(data, ErrorData::TerminalEndOfInput),
              "{} at ceiling {budget}: the refusal reached the caller as {data:?}. The probe this \
               row is about is `{}`, whose blind form folds the stop into `Ok(None)`",
              $label,
              $probe,
            );
            assert!(errors.is_terminal(), "{} at ceiling {budget}", $label);
          }
        }
      }
      first_ok.unwrap_or_else(|| panic!("{}: no ceiling under 48 let it through", $label))
    }};
  }

  // ── `peeks_where`, through the production Codex named ──
  //
  // The one row where the defect was a SUCCESS rather than a wrong error: before the repair this
  // came back `Ok` with an empty `VariablesDefinition` at every ceiling below four.
  let first_ok = sweep!(
    "VariablesDefinition::graphql",
    "($x: Int)",
    "peeks_where / always-declining try_expect",
    |inp| VariablesDefinition::<_>::graphql(inp).map(|_| ())
  );
  assert!(
    first_ok >= 2,
    "the variables-definition sweep covered only {first_ok} ceiling(s)"
  );

  // ── the six public `try_graphql` scalars ──
  //
  // Each on input it matches, so a `Decline` can only mean the read folded the stop. Ceiling zero
  // is the whole sweep here: one token is all any of them reads.
  let mut scalars = 0usize;
  macro_rules! scalar {
    ($label:literal, $src:literal, $call:expr) => {
      let errors = budgeted($call, $src, 0).err().unwrap_or_else(|| {
        panic!(
          "{}: a ceiling of zero must refuse the first item, and this \
                                   attempt returned `Ok` — a decline over a stopped scanner",
          $label
        )
      });
      let data = errors
        .iter()
        .next()
        .expect("the conversion emits exactly one error")
        .data();
      assert!(
        matches!(data, ErrorData::TerminalEndOfInput),
        "{}: {data:?}",
        $label
      );
      assert!(errors.is_terminal(), "{}", $label);
      scalars += 1;
    };
  }
  scalar!("IntValue::try_graphql", "123", |inp| {
    IntValue::<_>::try_graphql(inp).map(|_| ())
  });
  scalar!("FloatValue::try_graphql", "1.5", |inp| {
    FloatValue::<_>::try_graphql(inp).map(|_| ())
  });
  scalar!("StringValue::try_graphql", "\"s\"", |inp| {
    StringValue::<_>::try_graphql(inp).map(|_| ())
  });
  scalar!("BooleanValue::try_graphql", "true", |inp| {
    BooleanValue::<_>::try_graphql(inp).map(|_| ())
  });
  scalar!("NullValue::try_graphql", "null", |inp| {
    NullValue::<_>::try_graphql(inp).map(|_| ())
  });
  scalar!("EnumValue::try_graphql", "FOO", |inp| {
    EnumValue::<_>::try_graphql(inp).map(|_| ())
  });
  assert_eq!(scalars, 6, "the scalar set collapsed");

  // ── THE CONTROLS ──
  //
  // An ordinary mismatch still declines, and still commits nothing. `span().end()` and not
  // `offset()`: the scanner is past the token either way — measured at 4 on this input — and what
  // "did not consume" means is that nothing was committed.
  let (declined, committed) = budgeted(
    |inp| {
      let attempt = IntValue::<_>::try_graphql(inp)?;
      Ok((matches!(attempt, ParseAttempt::Decline), inp.span().end()))
    },
    "true",
    64,
  )
  .expect("an ordinary mismatch is not an error");
  assert!(
    declined,
    "`true` is not an int literal, so the attempt declines"
  );
  assert_eq!(
    committed, 0,
    "a decline must leave the token for the next alternative"
  );

  // A genuine end of input still means the optional production is absent, not stopped.
  assert!(
    budgeted(
      |inp| VariablesDefinition::<_>::graphql(inp).map(|_| ()),
      "",
      0
    )
    .is_ok(),
    "an empty document has no variables definition, and that is not a stop"
  );
  // And so does an ordinary head that is not `(`.
  assert!(
    budgeted(
      |inp| VariablesDefinition::<_>::graphql(inp).map(|_| ()),
      "x",
      64
    )
    .is_ok(),
    "a head that is not `(` means the variables definition is absent"
  );
}
