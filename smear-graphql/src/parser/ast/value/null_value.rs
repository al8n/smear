use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  },
};

use crate::{error::Error, lexer::ast::TokenKind};

use super::super::*;

use core::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NullValue<S> {
  source: S,
  span: Span,
}

impl<S> Display for NullValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.slice_ref(), f)
  }
}

impl<S> AsSpan<Span> for NullValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for NullValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for NullValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.source)
  }
}

impl<S> core::ops::Deref for NullValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.slice_ref()
  }
}

impl<S> NullValue<S> {
  /// Creates a new null value.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self {
      source: value,
      span,
    }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the slice of the null value.
  #[inline]
  pub const fn slice_ref(&self) -> &S {
    &self.source
  }

  /// Returns the slice of the null value.
  #[inline]
  pub const fn slice(&self) -> S
  where
    S: Copy,
  {
    self.source
  }
}

impl<S> DisplaySDL for NullValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.slice_ref().fmt(f)
  }
}

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for NullValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a>, Error = AstTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<'_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => match name {
            "null" => Ok(NullValue::new(span, name)),
            val => Err(Error::invalid_null_value(val, span).into()),
          },
          tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
