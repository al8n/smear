//! The four error conversions `lossless_error_impls!` generates for GraphQLx.
//!
//! # Why these need a direct call rather than a parse
//!
//! `Parse` drops the typed payload at the materialization boundary — deliberately, so a `Parse`
//! can outlive the source it was built from — so every parse-level assertion about an unterminated
//! `<` can say no more than `has_errors()`. Phase B Task 6 measured exactly that on the GraphQL
//! side: swapping `"[]" => unclosed_list` for `"[]" => unclosed_object`, and replacing the
//! catch-all with `unreachable!()`, both left the whole suite green.
//!
//! # And why GraphQLx needs its own file rather than a row in GraphQL's
//!
//! The `unclosed` list is a **macro argument**, so each dialect states its own pairs. GraphQLx
//! states a fourth — `<>` — because its lexer depth-counts `<` and `>` alongside the other three
//! (`smear-lexer/src/graphqlx/syntactic/mod.rs:807-814`). That fourth entry is the single line in
//! this task that a reviewer cannot check by symmetry with GraphQL, and it is the line that
//! decides whether an unterminated generic list reports `Unclosed::Angle` or the catch-all's
//! `Other("unclosed delimiter")`.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear_parser::{
  graphqlx::{
    GraphQLx,
    error::{ErrorData, Expectation, GraphqlxErrors, Unclosed as DialectUnclosed},
    lossless::GraphqlxLosslessErrors,
  },
  lexer::graphqlx::lossless::LosslessLexer,
};
use tokora::{
  SimpleSpan,
  emitter::FromUnclosed,
  error::{UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen, UnexpectedEot},
};

type Errs = GraphqlxLosslessErrors<&'static str>;
type Lx<'a> = LosslessLexer<'a, &'a str>;

/// The container holds exactly one error; hand it back.
macro_rules! only_error {
  ($err:expr) => {{
    let errs: Errs = <Errs as FromUnclosed<'_, Lx<'_>, GraphQLx>>::from_unclosed($err);
    let mut it = errs.into_iter();
    let first = it.next().expect("the conversion produced no error at all");
    assert!(it.next().is_none(), "the conversion produced more than one");
    first
  }};
}

/// All **four** declared pairs reach their own report, and an undeclared one reaches the
/// catch-all.
///
/// The `<>` row is the one that is new in this dialect. Getting it wrong is silent: an
/// unterminated `type T<A` would still report, still be an error, and still say something — it
/// would just say `Other("unclosed delimiter")` where the dialect has a typed
/// [`Unclosed::Angle`](DialectUnclosed::Angle) sitting unused.
///
/// The catch-all still has to be reachable and still has to produce an *error* rather than
/// panicking. GraphQLx declares every pair tokora ships a marker for, so this test reaches the
/// catch-all through a pair name no marker produces — the arm is unreachable for this grammar, and
/// "unreachable" is a claim about the grammar rather than something a conversion may assume.
#[test]
fn each_declared_delimiter_pair_reaches_its_own_report() {
  let span = SimpleSpan::new(3, 4);

  let bracket = only_error!(UnclosedBracket::<SimpleSpan, GraphQLx>::bracket_of(span));
  assert!(
    matches!(bracket.data(), ErrorData::Unclosed(DialectUnclosed::List)),
    "`[]` did not reach unclosed_list: {:?}",
    bracket.data()
  );

  let brace = only_error!(UnclosedBrace::<SimpleSpan, GraphQLx>::brace_of(span));
  assert!(
    matches!(brace.data(), ErrorData::Unclosed(DialectUnclosed::Object)),
    "`{{}}` did not reach unclosed_object: {:?}",
    brace.data()
  );

  let paren = only_error!(UnclosedParen::<SimpleSpan, GraphQLx>::paren_of(span));
  assert!(
    matches!(
      paren.data(),
      ErrorData::Unclosed(DialectUnclosed::Parentheses)
    ),
    "`()` did not reach unclosed_parentheses: {:?}",
    paren.data()
  );

  // The fourth pair, and the reason this file exists.
  let angle = only_error!(UnclosedAngle::<SimpleSpan, GraphQLx>::angle_of(span));
  assert!(
    matches!(angle.data(), ErrorData::Unclosed(DialectUnclosed::Angle)),
    "`<>` did not reach unclosed_angle — an unterminated generic list would report the \
     catch-all's untyped note instead: {:?}",
    angle.data()
  );

  // The span survives every arm — the diagnostic points at the opener, not at the end of input.
  assert_eq!(angle.span().start(), 3);
}

/// A pair the dialect does not name reaches the catch-all, and the catch-all answers with an
/// error.
///
/// The positive control for the four assertions above: if every pair fell through to the
/// catch-all, three of those four would still be *wrong* in a way the fourth could not reveal.
/// This is the only shape that distinguishes "the list is complete" from "the list is ignored".
#[test]
fn an_undeclared_pair_reaches_the_catch_all_and_still_produces_an_error() {
  use tokora::{delimiter::DelimiterKind, error::Unclosed as TokoraUnclosed, utils::CowStr};

  /// A delimiter marker GraphQLx's `unclosed` list does not name.
  struct Guillemet;

  let err = TokoraUnclosed::<Guillemet, SimpleSpan, GraphQLx>::of(
    SimpleSpan::new(7, 8),
    DelimiterKind::Custom("«»"),
    CowStr::from_static("«»"),
  );
  let caught = only_error!(err);
  assert!(
    matches!(caught.data(), ErrorData::Other(note) if note == "unclosed delimiter"),
    "an undeclared pair did not reach the catch-all: {:?}",
    caught.data()
  );
  assert_eq!(caught.span().start(), 7);
}

/// The lossless and syntactic layers report end of input the same way.
///
/// **This is a claim about one dialect's two layers, and nothing else checks it.** The acceptance
/// parity gate compares *verdicts* (`has_errors`), so two layers could disagree about what an
/// end-of-input error says forever without any gate noticing.
///
/// It is also the reason `graphqlx::error::Error::unexpected_end_of_input` exists at all.
/// `lossless_error_impls!` calls that constructor by name and GraphQLx had none — GraphQL's error
/// family carries a dedicated expectation-free `ErrorData::EndOfInput` variant and GraphQLx's does
/// not. Rather than give GraphQLx a second spelling that only the lossless half would use, the new
/// constructor produces exactly what this dialect's syntactic `UnexpectedEot` conversion already
/// produced. This test is what pins the two together.
#[test]
fn the_two_layers_agree_on_what_end_of_input_reports() {
  let eot = UnexpectedEot::<usize, GraphQLx>::eot_of(11);

  let lossless: GraphqlxLosslessErrors<&str> = eot.clone().into();
  let syntactic: GraphqlxErrors<&str> = eot.into();

  let lossless = lossless
    .into_iter()
    .next()
    .expect("the lossless conversion produced no error");
  let syntactic = syntactic
    .into_iter()
    .next()
    .expect("the syntactic conversion produced no error");

  let ErrorData::UnexpectedToken(lossless_data) = lossless.data() else {
    panic!(
      "the lossless end-of-input error is not an UnexpectedToken: {:?}",
      lossless.data()
    );
  };
  let ErrorData::UnexpectedToken(syntactic_data) = syntactic.data() else {
    panic!(
      "the syntactic end-of-input error is not an UnexpectedToken: {:?}",
      syntactic.data()
    );
  };

  assert!(
    lossless_data.found().is_none(),
    "an end of input has no found token"
  );
  assert!(syntactic_data.found().is_none());
  assert_eq!(
    lossless_data.expected(),
    syntactic_data.expected(),
    "the two GraphQLx layers report a different expectation for the identical event"
  );

  // The offset becomes an empty span at the position, in both.
  assert_eq!(lossless.span().start(), 11);
  assert_eq!(lossless.span().end(), 11);
  assert_eq!(syntactic.span().start(), 11);
}

/// A declined `expect` reports the kind it found and the expectation the dialect spells for it.
///
/// The third of the four generated impls, and the one that routes through `expectation_of`. The
/// seven images GraphQL has no counterpart for are the rows a mapper adapted from GraphQL's table
/// would be missing, so those are the rows asserted here; a missing row falls back to
/// `Expectation::Name`, which is a plausible-looking wrong answer rather than a crash.
#[test]
fn a_declined_expect_reports_the_graphqlx_expectation_for_its_kind() {
  use smear_parser::lexer::graphqlx::lossless::{LosslessToken, LosslessTokenKind as LK};
  use tokora::error::token::UnexpectedToken as TokUnexpectedToken;

  /// The expectation the lossless conversion produces when `expected` was demanded and `found`
  /// turned up.
  fn expectation_for(expected: LK) -> Expectation {
    let span = SimpleSpan::new(0, 1);
    let err = TokUnexpectedToken::<LosslessToken<&str>, LK, SimpleSpan, GraphQLx>::expected_one(
      span, expected,
    )
    .with_found(LosslessToken::Comma);
    let errs: Errs = err.into();
    let first = errs.into_iter().next().expect("no error was produced");
    match first.into_data() {
      ErrorData::UnexpectedToken(data) => *data.expected(),
      other => panic!("a declined expect produced {other:?}"),
    }
  }

  // The seven images this dialect has and GraphQL does not.
  assert_eq!(expectation_for(LK::LAngle), Expectation::LAngle);
  assert_eq!(expectation_for(LK::RAngle), Expectation::RAngle);
  assert_eq!(expectation_for(LK::Asterisk), Expectation::Asterisk);
  assert_eq!(expectation_for(LK::Plus), Expectation::Plus);
  assert_eq!(expectation_for(LK::Minus), Expectation::Minus);
  assert_eq!(
    expectation_for(LK::PathSeparator),
    Expectation::PathSeparator
  );
  assert_eq!(expectation_for(LK::FatArrow), Expectation::FatArrow);

  // The two rows that deliberately differ from GraphQL's lossless table: this dialect's own
  // syntactic `expectation_from_token_kind` sends both numeric images to `InputValue`, and the two
  // GraphQLx layers agreeing matters more than the two dialects agreeing.
  assert_eq!(expectation_for(LK::Int), Expectation::InputValue);
  assert_eq!(expectation_for(LK::Float), Expectation::InputValue);

  // A trivia kind is never demanded by any production, so it falls back — which is the arm that
  // makes the fallback provably a fallback rather than the whole table.
  assert_eq!(expectation_for(LK::Comment), Expectation::Name);
}

/// The lexer's error channel lands in the container with its payload intact.
///
/// The fourth generated impl. Unlike the syntactic twin, which flattens a lexer error to a bare
/// `Other("lexer error")` note, the payload survives here: the container's `StateError` is the
/// lexer's own `LimitExceeded`, so `ErrorData::Lexer` accepts it unchanged. The span is the one
/// thing that cannot be recovered — the lexer error type is a *batch* and the container's error
/// carries a single span — so it is zeroed.
#[test]
fn a_lexer_error_lands_as_error_data_lexer_not_as_a_note() {
  use smear_parser::lexer::graphqlx::error::LexerErrors;
  use tokora::state::tracker::LimitExceeded;

  let errs: Errs = LexerErrors::<char, LimitExceeded>::default().into();
  let first = errs.into_iter().next().expect("no error was produced");
  assert_eq!(first.span().start(), 0);
  assert!(
    matches!(first.data(), ErrorData::Lexer(_)),
    "a lexer error was flattened instead of kept: {:?}",
    first.data()
  );
}
