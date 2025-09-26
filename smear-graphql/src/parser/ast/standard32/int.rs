use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{
    Span, IntoSpan, IntoComponents, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use core::fmt::Display;

use crate::{error::Error, lexer::ast::TokenKind};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntValue<S> {
  span: Span,
  value: S,
}

impl<S> Display for IntValue<S>
where
  S: Display,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value)
  }
}

impl<S> AsSpan<Span> for IntValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for IntValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for IntValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> core::ops::Deref for IntValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<S> IntValue<S> {
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the value of the int.
  #[inline]
  pub const fn value(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns the reference value of the int.
  #[inline]
  pub const fn value_ref(&self) -> &S {
    &self.value
  }
}

impl<S> DisplaySDL for IntValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for IntValue<&'a str>
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
          AstToken::Int(val) => Ok(Self::new(span, val)),
          tok => Err(Error::unexpected_token(tok, TokenKind::Int, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
