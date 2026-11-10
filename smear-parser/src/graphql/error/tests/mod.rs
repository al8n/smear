use smear_lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken, SyntacticTokenKind};
use tokora::{FatalContext, Lexer, SimpleSpan};

use super::{ErrorData, Expectation, GraphqlErrors, SeparatedError, TokUnexpectedToken};
use crate::{combinator::ParseCtx, graphql::GraphQL};

fn assert_parse_ctx<'inp, L, Ctx>()
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, GraphQL>,
{
}

#[test]
fn graphql_error_is_parse_ctx_over_str_and_slice() {
  // `Fatal<GraphqlErrors<slice>>` is a complete `ParseCtx` over both source
  // representations — the whole point of the `From` glue above.
  assert_parse_ctx::<
    SyntacticLexer<'_, str>,
    FatalContext<'_, SyntacticLexer<'_, str>, GraphqlErrors<&str>, GraphQL>,
  >();
  assert_parse_ctx::<
    SyntacticLexer<'_, [u8]>,
    FatalContext<'_, SyntacticLexer<'_, [u8]>, GraphqlErrors<&[u8]>, GraphQL>,
  >();
}

type TokoraTokenError = TokUnexpectedToken<
  'static,
  SyntacticToken<&'static str>,
  SyntacticTokenKind,
  SimpleSpan,
  GraphQL,
>;

fn unexpected_with(expected: SyntacticTokenKind) -> TokoraTokenError {
  TokUnexpectedToken::expected_one_with_found(
    SimpleSpan::new(4, 9),
    SyntacticToken::Identifier("found"),
    expected,
  )
}

fn assert_expected(errors: GraphqlErrors<&'static str>, expected: Expectation) {
  let error = errors
    .into_iter()
    .next()
    .expect("Tokora conversion emits one error");
  assert_eq!(error.span(), SimpleSpan::new(4, 9));
  let ErrorData::UnexpectedToken(unexpected) = error.data() else {
    panic!(
      "expected unexpected-token diagnostic, got {:?}",
      error.data()
    );
  };
  assert_eq!(unexpected.found(), Some(&SyntacticTokenKind::Identifier));
  assert_eq!(unexpected.expected(), &expected);
}

#[test]
fn tokora_unexpected_token_maps_single_expected_kinds() {
  for (kind, expected) in [
    (SyntacticTokenKind::Identifier, Expectation::Name),
    (SyntacticTokenKind::Int, Expectation::IntValue),
    (SyntacticTokenKind::Float, Expectation::FloatValue),
    (SyntacticTokenKind::InlineString, Expectation::InlineString),
    (SyntacticTokenKind::BlockString, Expectation::BlockString),
    (SyntacticTokenKind::Dollar, Expectation::Dollar),
    (SyntacticTokenKind::LParen, Expectation::LParen),
    (SyntacticTokenKind::RParen, Expectation::RParen),
    (SyntacticTokenKind::Spread, Expectation::Spread),
    (SyntacticTokenKind::Colon, Expectation::Colon),
    (SyntacticTokenKind::Equal, Expectation::Equal),
    (SyntacticTokenKind::At, Expectation::At),
    (SyntacticTokenKind::LBracket, Expectation::LBracket),
    (SyntacticTokenKind::RBracket, Expectation::RBracket),
    (SyntacticTokenKind::LBrace, Expectation::LBrace),
    (SyntacticTokenKind::RBrace, Expectation::RBrace),
    (SyntacticTokenKind::Pipe, Expectation::Pipe),
    (SyntacticTokenKind::Bang, Expectation::Bang),
    (SyntacticTokenKind::Ampersand, Expectation::Ampersand),
  ] {
    assert_expected(unexpected_with(kind).into(), expected);
  }
}

/// An absent or multi-kind expected set names no expectation, rather than one member of the set.
///
/// It fell back to `Name` until the diagnostic contract rendered the expectation into the
/// message, where "expected a name" for a `{`-or-`[` set reads as a fact about the grammar.
#[test]
fn tokora_unrepresentable_expected_sets_name_no_expectation() {
  static ONE_OF: [SyntacticTokenKind; 2] =
    [SyntacticTokenKind::LBrace, SyntacticTokenKind::LBracket];

  let without_expected =
    TokoraTokenError::of(SimpleSpan::new(4, 9)).with_found(SyntacticToken::Identifier("found"));
  assert_expected(without_expected.into(), Expectation::Unspecified);

  let one_of = TokoraTokenError::expected_one_of_with_found(
    SimpleSpan::new(4, 9),
    SyntacticToken::Identifier("found"),
    &ONE_OF,
  );
  assert_expected(one_of.into(), Expectation::Unspecified);

  assert_expected(
    SeparatedError::leading(unexpected_with(SyntacticTokenKind::LBrace)).into(),
    Expectation::LBrace,
  );
}

mod census;
mod expectation;
#[cfg(feature = "materialized-numbers")]
mod int_overflow;
mod terminal;
