use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use core::fmt::Display;

use crate::{
  error::Error,
  lexer::ast::{AstLexerErrors, TokenKind},
};

use super::super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatValue<S> {
  span: Span,
  value: S,
}

impl<S> AsSpan<Span> for FloatValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for FloatValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for FloatValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> Display for FloatValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.slice_ref(), f)
  }
}

impl<S> AsRef<S> for FloatValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for FloatValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.slice_ref()
  }
}

impl<S> FloatValue<S> {
  /// Creates a new name.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the slice of the float.
  #[inline]
  pub const fn slice(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns the slice of the float.
  #[inline]
  pub const fn slice_ref(&self) -> &S {
    &self.value
  }
}

impl<S> DisplaySDL for FloatValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplaySyntaxTree for FloatValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- FLOAT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.slice_ref().display(),
    )
  }
}

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for FloatValue<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
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
          AstToken::Float(val) => Ok(Self::new(span, val)),
          tok => Err(Error::unexpected_token(tok, TokenKind::Float, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
