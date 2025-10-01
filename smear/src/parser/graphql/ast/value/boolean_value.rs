use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, cmp::Equivalent, sdl_display::DisplaySDL},
};

use crate::lexer::graphql::ast::{AstLexerErrors, AstTokenKind};

use super::super::*;

use core::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanValue {
  span: Span,
  value: bool,
}

impl Display for BooleanValue {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl AsSpan<Span> for BooleanValue {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for BooleanValue {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl IntoComponents for BooleanValue {
  type Components = (Span, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl AsRef<bool> for BooleanValue {
  #[inline]
  fn as_ref(&self) -> &bool {
    &self.value
  }
}

impl core::ops::Deref for BooleanValue {
  type Target = bool;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl BooleanValue {
  /// Creates a new boolean value.
  #[inline]
  pub(crate) const fn new(span: Span, value: bool) -> Self {
    Self { span, value }
  }

  /// Returns the span of the boolean value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the boolean value.
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }
}

impl DisplaySDL for BooleanValue {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for BooleanValue
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(ident) => Ok(match () {
            () if "true".equivalent(&ident) => BooleanValue::new(span, true),
            () if "false".equivalent(&ident) => BooleanValue::new(span, false),
            _ => return Err(Error::invalid_boolean_value(ident, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Boolean, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
