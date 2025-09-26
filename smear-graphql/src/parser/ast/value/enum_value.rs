use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  },
};

use core::fmt::Display;

use crate::{error::Error, lexer::ast::TokenKind};

use super::super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumValue<S>(Name<S>);

impl<S> Display for EnumValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S> AsSpan<Span> for EnumValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for EnumValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for EnumValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> core::ops::Deref for EnumValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S> EnumValue<S> {
  /// Creates a new name.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self(Name::new(span, value))
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the slice of the enum value.
  #[inline(always)]
  pub const fn slice(&self) -> S
  where
    S: Copy,
  {
    self.0.slice()
  }

  /// Returns a reference to the slice of the enum value.
  #[inline(always)]
  pub const fn source(&self) -> &S {
    self.0.slice_ref()
  }
}

impl<S> DisplaySDL for EnumValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source().fmt(f)
  }
}

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for EnumValue<&'a str>
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
            "true" | "false" | "null" => Err(Error::invalid_enum_value(name, span).into()),
            _ => Ok(EnumValue::new(span, name)),
          },
          tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
