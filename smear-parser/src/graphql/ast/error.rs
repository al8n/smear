use crate::error::MissingDollarTokenError;
use logosky::{
  Token,
  utils::{IntoComponents, Span},
};
use crate::scaffold::error::{UnclosedBraceError, UnclosedBracketError};

use super::*;

impl<'a, S> MissingDollarTokenError<Name<S>> for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Name<S>, span: Span) -> Self {
    Self::unexpected_token(
      span,
      SyntacticToken::Identifier(name.into_components().1),
      SyntaxKind::Dollar,
    )
  }
}

impl<'a, S> MissingDollarTokenError<Name<S>> for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Name<S>, span: Span) -> Self {
    <SyntacticTokenError<'a, S> as MissingDollarTokenError<Name<S>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }
}

// ============================================================================
// From<IncompleteSyntax<*Syntax>> implementations
// ============================================================================
//
// Note: The Error enum derives From, which automatically generates From
// implementations for all IncompleteSyntax variants. No manual implementations needed!


impl<'a, S> UnclosedBraceError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_brace(span: Span) -> Self {
    Self::unclosed_brace(span)
  }
}

impl<'a, S> UnclosedBraceError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_brace(span: Span) -> Self {
    <SyntacticTokenError<'a, S> as UnclosedBraceError>::unclosed_brace(span).into()
  }
}

impl<'a, S> UnclosedBracketError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_bracket(span: Span) -> Self {
    Self::unclosed_bracket(span)
  }
}

impl<'a, S> UnclosedBracketError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_bracket(span: Span) -> Self {
    <SyntacticTokenError<'a, S> as UnclosedBracketError>::unclosed_bracket(span).into()
  }
}
