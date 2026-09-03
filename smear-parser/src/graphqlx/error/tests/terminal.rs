//! The `MaybeTerminal` arm census: which errors end the parse.
//!
//! GraphQL's sibling is `graphql/error/tests/terminal.rs`, and the two exist as a **pair on
//! purpose**. The `MaybeTerminal` impls are hand-written per dialect over disjoint variant sets,
//! so they are two implementations of one ruling rather than one implementation reached twice —
//! the shape where a repair lands in one file and dies there. This branch was caught by planting
//! three times on that axis; the third catch was this census existing only for GraphQL, with the
//! motivating plant (flip the `Lexer` arm to `false`) leaving every cell in the tree green over
//! here.
//!
//! **There are three impls per dialect, not one, and only two of them can be reached by a
//! parse.** [`ErrorData`]'s arms are the census below; [`Error`]'s delegation and [`Errors`]'s
//! fold sit above them. The delegation is on the path every document-root catch site takes, so
//! `nesting_depth.rs` pins it — planted `false`, and
//! `a_refusal_is_one_diagnostic_at_every_cycle` and `a_refusal_ends_every_document_root` both
//! went red at their GraphQLx cells. The fold cannot be reached that way at all, and
//! [`the_container_fold_keeps_a_stop_that_is_not_alone`] is why it has a cell here.

use core::marker::PhantomData;

use smear_lexer::{
  error::BadStateError,
  graphqlx::{error::LexerErrors, syntactic::SyntacticTokenKind},
};
use tokora::{SimpleSpan as Span, error::MaybeTerminal, utils::CowStr};

use crate::graphqlx::error::{Error, ErrorData, Errors, Expectation, UnexpectedEnd};

/// A stand-in for `smear_lexer::limits`'s `LimitExceeded`. The `Lexer` arm asks whether the
/// *variant* is `State`, not what the payload is, so any inhabited type exercises it — and using
/// one of this crate's own keeps the census out of the lexer's feature matrix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StateErr;

/// The error this census is written over, keyed the way the **lossless** door keys it: the state
/// error is a real budget rather than `()`, because that is what makes the `Lexer` arm's question
/// a real one.
type Data = ErrorData<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;
type Err = Error<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;
type Errs = Errors<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;

/// Every variant of [`ErrorData`] answers [`MaybeTerminal::is_terminal`], and answers what this
/// list says.
///
/// # Two properties, one list, and the second is why this is a census rather than a few asserts
///
/// * **Every variant is covered.** The generated `tag` is a wildcard-free `match`, so a variant
///   added without an entry is an `E0004` *here* as well as in the impl. The impl's own
///   exhaustiveness already forces an *arm*; this is what forces the arm to be a **decision** — a
///   new variant cannot be waved through with `=> false` and no test saying so.
/// * **Each entry carries its expected answer beside its sample**, so there is no state in which a
///   variant is listed and unchecked.
///
/// Some variants appear **twice**, and those are the point: an arm that delegates has to be shown
/// reading the value rather than the variant, which takes one sample on each side. `Lexer` and
/// `UnexpectedEnd` are the two.
///
/// # Naming a variant directly is allowed here, and is not allowed in the producibility census
///
/// `graphql/error/tests/census.rs` forbids `ErrorData::Variant(…)` samples because the question
/// there is *can this be produced through a public door*, which a direct construction begs. The
/// question here is *what does the arm answer for this value*, and a value is exactly what it
/// needs; where a constructor exists it is used anyway, because a constructor `descend` or a
/// conversion actually reaches is the more useful sample.
///
/// # Why this is not an end-to-end cell
///
/// Two of the `true` answers cannot be reached through a shipped door. A `Lexer` error only
/// becomes an `Err` on the parser's channel when the **emitter rejects** its diagnostic, and both
/// lossless doors pin `Verbose`, which never rejects; with an accepting emitter a lexer state trip
/// latches tokora's poison boundary instead, the root loop's peek answers `None`, and the loop
/// exits `Ok` with no error to classify at all. So the arm is live only for a consumer with a
/// fail-fast emitter over this dialect's own container — precisely the caller tokora's rule tells
/// to write it, and precisely the caller no in-tree parse is.
#[test]
fn the_terminal_arms_answer_for_every_variant() {
  macro_rules! census {
    ($($variant:ident => [$(($sample:expr, $expected:expr)),+ $(,)?]),+ $(,)?) => {
      // Wildcard-free and generated from the same list as the samples below.
      fn tag(data: &Data) -> &'static str {
        match data {
          $(ErrorData::$variant { .. } => stringify!($variant),)+
        }
      }

      let mut cells = 0usize;
      $($({
        let sample: Data = $sample;
        assert_eq!(
          tag(&sample),
          stringify!($variant),
          "the sample recorded for {} produced a different variant",
          stringify!($variant),
        );
        assert_eq!(
          sample.is_terminal(),
          $expected,
          "{} answered {} for `is_terminal`, expected {}",
          stringify!($variant),
          sample.is_terminal(),
          $expected,
        );
        cells += 1;
      })+)+
      // A census that selected nothing exits `ok`. 11 = 9 variants + the two second samples.
      assert_eq!(cells, 11, "the cell set collapsed");
    };
  }

  let span = Span::new(0, 1);
  let end = UnexpectedEnd::with_name(
    0usize,
    CowStr::from_static("GraphQLx production"),
    Expectation::Name,
  );

  census! {
    // Terminal only when it holds a `State` refusal — a spent `smear_lexer::limits` budget, and
    // the carrier a scanner trip lands on unmarked when the emitter rejects its diagnostic. This
    // is the arm tokora's rule singles out as the one that catches people.
    Lexer => [
      (ErrorData::Lexer(LexerErrors::bad_state(span, StateErr)), true),
      (ErrorData::Lexer(LexerErrors::default()), false),
    ],

    // Always: a frame budget is never cleared by more input. Through the constructor
    // `lossless::depth::descend` reaches the variant by, so the two cannot disagree.
    NestingLimitExceeded => [(Err::nesting_limit_exceeded(span).into_data(), true)],

    // Always, and one scale more firmly than the frame budget above: the tally is outside every
    // rollback, no public mutator lowers it, and it is the only thing a refused document reports.
    TokenBudgetExhausted => [(Err::token_budget_exhausted(span).into_data(), true)],

    // Always: an end of input that stands in for a scanner stop is a stop, and no input clears a
    // tripped limit — smear issue #177. Through the constructor both `From<UnexpectedEot>` impls
    // route to when tokora's mark is set, so the conversion and the arm cannot disagree.
    TerminalEndOfInput => [(Err::terminal_end_of_input(span).into_data(), true)],

    // Delegated: the value decides, not the variant. A production that ran out of input is a
    // grammar rejection; the same variant is terminal when the scanner raised the flag on it.
    UnexpectedEnd => [
      (ErrorData::UnexpectedEnd(end.clone()), false),
      (ErrorData::UnexpectedEnd(end.into_terminal()), true),
    ],

    // Affirmatively recoverable: built from a construct the grammar rejected, not from a limit the
    // runtime refused.
    Unclosed => [(Err::unclosed_list(span).into_data(), false)],
    UnexpectedToken => [(
      Err::unexpected_token(SyntacticTokenKind::Colon, Expectation::Name, span).into_data(),
      false
    )],
    Other => [(ErrorData::Other(std::borrow::Cow::Borrowed("x")), false)],
    // Carries no diagnostic at all — it exists to keep `S` used.
    Source => [(ErrorData::Source(PhantomData), false)],
  }
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
/// error — the one length at which `any` and `all` agree. Planted: `any` → `all` left both
/// terminal censuses and all thirteen of `nesting_depth.rs`'s tests green.
///
/// [`Error`]'s delegation is **not** in that position and gets no cell of its own — it is on the
/// path every catch site takes, so planting `false` on it reddened
/// `a_refusal_is_one_diagnostic_at_every_cycle` and `a_refusal_ends_every_document_root` at their
/// GraphQLx cells. The two `Error`-level assertions below are this fold's premise rather than a
/// pin: a fold cannot be read without knowing what its elements answer.
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
/// GraphQL's twin — `graphql/error/tests/terminal.rs`'s cell of this name — carries why a census
/// over values cannot make this claim and why the emitter has to accept. It is a **pair on
/// purpose** for the reason this file's header gives: the two conversions are two hand-written
/// impls of one ruling, and this dialect's routes to a different variant on the unmarked arm than
/// GraphQL's does, so a plant on the sibling proves the sibling.
///
/// The unmarked arm is where the dialects differ and it is asserted below: GraphQLx has no
/// `EndOfInput` variant, so a genuine end of input arrives as an
/// [`UnexpectedToken`](ErrorData::UnexpectedToken) with no found token. Both arms of one `From`
/// impl, one offset, opposite verdicts.
#[test]
fn a_scanner_stop_reaches_this_dialect_marked_and_the_split_keeps_the_mark() {
  use tokora::{
    InputRef, ParserContext, cache::DefaultCache, emitter::Verbose, error::UnexpectedEot,
    input::TokenBudget,
  };

  use crate::graphqlx::{GraphQLx, error::GraphqlxErrors, syntactic::GraphqlxLexer};

  type Lx<'inp> = GraphqlxLexer<'inp, str>;
  type Em<'inp> = Verbose<GraphqlxErrors<&'inp str>, Span, GraphQLx>;
  type Cx<'inp> = ParserContext<'inp, Lx<'inp>, Em<'inp>, DefaultCache<'inp, Lx<'inp>>, GraphQLx>;

  /// Walks the document a token at a time, peeking before each one — the shape every trivia atom
  /// in this dialect has, reduced to the two calls that matter.
  fn walk<'inp>(src: &'inp str, budget: usize) -> Result<(), GraphqlxErrors<&'inp str>> {
    tokora::parse_with::<Lx<'inp>, str, _, (), Cx<'inp>, GraphQLx>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Cx<'inp>, GraphQLx>| {
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

  assert!(
    walk(src, 10_000).is_ok(),
    "a budget no parse reaches must not end anything"
  );

  // THE UNMARKED ARM, and this dialect's own spelling of it: an `UnexpectedToken` with no found
  // token, which is what `Error::unexpected_end_of_input`'s note keeps as the single spelling of a
  // genuine end of input across both layers.
  let ordinary: GraphqlxErrors<&str> = UnexpectedEot::<usize, GraphQLx>::eot_of(11).into();
  let ordinary = ordinary
    .iter()
    .next()
    .expect("the conversion emits exactly one error");
  assert!(
    matches!(ordinary.data(), ErrorData::UnexpectedToken(t) if t.found().is_none()),
    "an unmarked end of input must still be the recoverable spelling: {:?}",
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
/// GraphQL's twin — `graphql/error/tests/terminal.rs`'s cell of this name — carries the reasoning:
/// a committed read written as `next()` or a raw `peek` folds a resource trip into the same
/// `Ok(None)` a genuine end of input produces, and a lexer error a rejecting emitter hands back
/// reaches the caller through `From<LexerErrors>` rather than through any `UnexpectedEnd`.
///
/// **It is a pair for the reason this file's header gives, and this dialect had one extra place to
/// repair.** GraphQLx declares its own `syntactic::peek_kind` free function, which *shadows*
/// tokora's terminal-aware method of that name and was written over a raw `peek`; five call sites
/// read a scanner stop through it as "no head here". GraphQL has no such wrapper and calls
/// tokora's directly, so a plant on the sibling would have proved nothing about this.
#[test]
fn the_marks_smear_has_to_make_itself_reach_the_caller() {
  use smear_lexer::limits::SyntacticLimits;
  use tokora::{
    FatalContext, Parse as _, Parser, ParserContext, cache::DefaultCache, emitter::Verbose,
    input::TokenBudget,
  };

  use crate::graphqlx::{
    GraphQLx,
    ast::{InputValue, IntValue, Name as AstName, Selection},
    error::GraphqlxErrors,
    syntactic::{GraphqlxInput, GraphqlxLexer},
  };

  type Lx<'inp> = GraphqlxLexer<'inp, str>;
  type Fx<'inp> = FatalContext<'inp, Lx<'inp>, GraphqlxErrors<&'inp str>, GraphQLx>;
  type Vx<'inp> = ParserContext<
    'inp,
    Lx<'inp>,
    Verbose<GraphqlxErrors<&'inp str>, Span, GraphQLx>,
    DefaultCache<'inp, Lx<'inp>>,
    GraphQLx,
  >;

  fn fatal<'inp, O>(
    f: impl for<'c> FnMut(
      &mut GraphqlxInput<'inp, 'c, str, Fx<'inp>>,
    ) -> Result<O, GraphqlxErrors<&'inp str>>,
    src: &'inp str,
    state: SyntacticLimits,
  ) -> Result<O, GraphqlxErrors<&'inp str>> {
    Parser::with_parser::<'inp, Lx<'inp>, O, GraphqlxErrors<&'inp str>, _, GraphQLx>(f)
      .parse_str_with_state(src, state)
  }

  fn budgeted<'inp, O>(
    f: impl for<'c> FnMut(
      &mut GraphqlxInput<'inp, 'c, str, Vx<'inp>>,
    ) -> Result<O, GraphqlxErrors<&'inp str>>,
    src: &'inp str,
    budget: usize,
  ) -> Result<O, GraphqlxErrors<&'inp str>> {
    Parser::with_parser_and_context::<'inp, Lx<'inp>, O, Vx<'inp>, _, GraphQLx>(
      f,
      ParserContext::of(Verbose::default()).with_token_budget(TokenBudget::with_limitation(budget)),
    )
    .parse_str(src)
  }

  let mut cells = 0usize;
  for (what, errors) in [
    (
      "IntValue::graphqlx",
      budgeted(|inp| IntValue::<_>::graphqlx(inp).map(|_| ()), "123", 0)
        .expect_err("a budget of zero refuses the first item"),
    ),
    (
      "Name::graphqlx",
      budgeted(|inp| AstName::<_>::graphqlx(inp).map(|_| ()), "Foo", 0)
        .expect_err("a budget of zero refuses the first item"),
    ),
    // THE HEAD-PEEK CLASS ON ITS OWN, and finding a door for it was a measurement. Every door
    // above reaches the budget through a `next`, and at a ceiling of zero the very first read
    // answers — so reverting either head peek left all of them green. This one refuses *after* the
    // spread has been consumed, which is where `syntactic::peek_kind` and `unexpected_here` are,
    // and it is the shallowest public production that gets there.
    (
      "Selection::graphqlx (after the spread)",
      budgeted(
        |inp| Selection::<_>::graphqlx(inp).map(|_| ()),
        "...a::b",
        1,
      )
      .expect_err("a ceiling of one item refuses the second"),
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

  assert!(
    budgeted(|inp| IntValue::<_>::graphqlx(inp).map(|_| ()), "123", 8).is_ok(),
    "a budget no parse reaches must not refuse anything"
  );

  for src in ["{ a: 1 }", "[1]", "(1)"] {
    let errors = fatal(
      |inp| InputValue::<_>::graphqlx(inp).map(|_| ()),
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
      "{src:?}: the batch's first span, not `0..0`"
    );
    cells += 1;
  }

  for (src, end) in [("\u{1}", 1usize), ("\"unterminated", 13)] {
    let errors = fatal(
      |inp| InputValue::<_>::graphqlx(inp).map(|_| ()),
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
    assert!(!errors.is_terminal(), "{src:?}");
    assert_eq!(
      (error.span().start(), error.span().end()),
      (0, end),
      "{src:?}: the batch's first span"
    );
    cells += 1;
  }

  assert_eq!(cells, 8, "the cell set collapsed");
}

/// Every optional probe in this dialect raises the stop instead of reporting the thing absent —
/// smear issue #177, Codex round 2.
///
/// # What round 2 missed, and the shape of the miss
///
/// Round 2's census was derived over the two primitive **names** round 1 had mentioned, `next` and
/// a raw `peek`. `try_expect_map` folds a stop into the same `Ok(None)` those do — tokora says so
/// in the primitive's own doc (`input_ref/try_expect.rs:262`) — and this dialect reaches it from
/// ten places, every one of them an optional or attempt probe. `NamedSpecifier::graphqlx` on
/// `Foo as Bar` therefore **succeeded, as a bare `Foo`**, when the budget landed on `as`: a
/// truncated AST with no error at all, which is worse than the wrong error.
///
/// # The sweep is the assertion, and it is derived rather than picked
///
/// A cell at one chosen ceiling proves one arithmetic coincidence. Each row below is driven at
/// **every** ceiling from zero up to the one at which the production first succeeds: the source is
/// chosen so the production consumes all of it, so every ceiling below that is a real refusal
/// somewhere inside the parse, and *every one of them* must come back terminal. The first
/// succeeding ceiling is discovered, not written down, so a row cannot go vacuous by the grammar
/// shifting under it — and it is asserted to be at least two, which is what makes the sweep
/// non-empty.
#[test]
fn every_optional_probe_raises_the_stop_instead_of_declining() {
  use tokora::{
    Parse as _, Parser, ParserContext, cache::DefaultCache, emitter::Verbose, input::TokenBudget,
  };

  use crate::graphqlx::{
    GraphQLx,
    ast::{
      DescribedExecutableDefinition, DirectiveDefinition, InterfaceTypeDefinition, NamedSpecifier,
      Selection, WildcardSpecifier,
    },
    error::GraphqlxErrors,
    syntactic::{GraphqlxInput, GraphqlxLexer},
  };

  type Lx<'inp> = GraphqlxLexer<'inp, str>;
  type Vx<'inp> = ParserContext<
    'inp,
    Lx<'inp>,
    Verbose<GraphqlxErrors<&'inp str>, Span, GraphQLx>,
    DefaultCache<'inp, Lx<'inp>>,
    GraphQLx,
  >;

  fn budgeted<'inp, O>(
    f: impl for<'c> FnMut(
      &mut GraphqlxInput<'inp, 'c, str, Vx<'inp>>,
    ) -> Result<O, GraphqlxErrors<&'inp str>>,
    src: &'inp str,
    budget: usize,
  ) -> Result<O, GraphqlxErrors<&'inp str>> {
    Parser::with_parser_and_context::<'inp, Lx<'inp>, O, Vx<'inp>, _, GraphQLx>(
      f,
      ParserContext::of(Verbose::default()).with_token_budget(TokenBudget::with_limitation(budget)),
    )
    .parse_str(src)
  }

  /// Drives one door over every ceiling up to the first that lets it through, and returns that
  /// ceiling. Panics naming the ceiling if any refusal below it came back non-terminal.
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
               row is about is `{}`, whose blind form folds the stop into `Ok(None)` and lets the \
               production report the thing absent",
              $label,
              $probe,
            );
            assert!(errors.is_terminal(), "{} at ceiling {budget}", $label);
          }
        }
      }
      let first_ok = first_ok
        .unwrap_or_else(|| panic!("{}: no ceiling under 48 let the production through", $label));
      assert!(
        first_ok >= 2,
        "{}: the sweep covered only {first_ok} ceiling(s) — the source no longer reaches the \
         probe, so this row asserts nothing",
        $label
      );
      first_ok
    }};
  }

  // `optional_alias`, the probe Codex named, through both import specifiers.
  sweep!(
    "NamedSpecifier::graphqlx",
    "Foo as Bar",
    "optional_alias / try_expect_map",
    |inp| NamedSpecifier::<_>::graphqlx(inp).map(|_| ())
  );
  sweep!(
    "WildcardSpecifier::graphqlx",
    "* as Bar",
    "optional_alias / try_expect_map",
    |inp| WildcardSpecifier::<_>::graphqlx(inp).map(|_| ())
  );
  // `try_on` — the inline-fragment type condition.
  sweep!(
    "Selection::graphqlx (inline fragment)",
    "... on T { a }",
    "try_on / try_expect_map",
    |inp| Selection::<_>::graphqlx(inp).map(|_| ())
  );
  // `try_where_clause`, and the `description` probe in front of it.
  sweep!(
    "InterfaceTypeDefinition::graphqlx",
    "interface I where T: U { a: Int }",
    "try_where_clause / try_expect_map",
    |inp| InterfaceTypeDefinition::<_>::graphqlx(inp).map(|_| ())
  );
  // `try_contextual_keyword`, through the one optional keyword that reaches it.
  sweep!(
    "DirectiveDefinition::graphqlx",
    "directive @d repeatable on FIELD",
    "try_contextual_keyword / try_expect_map",
    |inp| DirectiveDefinition::<_>::graphqlx(inp).map(|_| ())
  );
  // `try_description`, the executable-side optional description.
  sweep!(
    "DescribedExecutableDefinition::graphqlx",
    "\"d\" query Q { a }",
    "try_description / try_expect_map",
    |inp| DescribedExecutableDefinition::<_>::graphqlx(inp).map(|_| ())
  );

  // ── THE CONTROLS: every probe must still report a genuinely absent thing as absent ──
  let no_alias = budgeted(
    |inp| NamedSpecifier::<_>::graphqlx(inp).map(|s| s.alias().is_some()),
    "Foo",
    64,
  )
  .expect("a bare specifier parses");
  assert!(!no_alias, "a specifier with no `as` must have no alias");
  let with_alias = budgeted(
    |inp| NamedSpecifier::<_>::graphqlx(inp).map(|s| s.alias().is_some()),
    "Foo as Bar",
    64,
  )
  .expect("an aliased specifier parses");
  assert!(with_alias, "a specifier with an `as` must have its alias");
}
