use crate::{error::ParseVariableValueError, hints::VariableValueHint};
use logosky::{
  Token,
  utils::{IntoComponents, Span},
};
use smear_scaffold::error::{InvalidFragmentTypePath, UnclosedBraceError, UnclosedBracketError};

use super::*;

impl<'a, S> ParseVariableValueError<Ident<S>> for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Ident<S>, span: Span) -> Self {
    Self::unexpected_token(
      SyntacticToken::Identifier(name.into_components().1),
      SyntaxKind::Dollar,
      span,
    )
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::unexpected_end_of_variable_value(hint, span)
  }
}

impl<'a, S> ParseVariableValueError<Ident<S>> for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Ident<S>, span: Span) -> Self {
    <SyntacticTokenError<'a, S> as ParseVariableValueError<Ident<S>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <SyntacticTokenError<'a, S> as ParseVariableValueError<Ident<S>>>::unexpected_end_of_variable_value(
      hint, span,
    )
    .into()
  }
}

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

impl<'a, S> InvalidFragmentTypePath for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  fn invalid_fragment_type_path(span: Span) -> Self {
    Self::invalid_fragment_type_path(span)
  }
}

impl<'a, S> InvalidFragmentTypePath for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  fn invalid_fragment_type_path(span: Span) -> Self {
    <SyntacticTokenError<'a, S> as InvalidFragmentTypePath>::invalid_fragment_type_path(span).into()
  }
}
