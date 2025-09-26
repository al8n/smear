use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use core::fmt::{self, Display};

use crate::{error::Error, lexer::ast::TokenKind};

use super::super::*;

pub use crate::lexer::{BlockString, InlineString};

/// A GraphQL string literal, either inline or block.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitStr<S> {
  /// An inline string literal.
  Inline(InlineString<S>),
  /// A block string literal.
  Block(BlockString<S>),
}

impl<S: DisplayHuman> Display for LitStr<S> {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Inline(s) => write!(f, "{s}"),
      Self::Block(s) => write!(f, "{s}"),
    }
  }
}

impl<S: DisplayHuman> DisplayHuman for LitStr<S> {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<S> LitStr<S> {
  /// Returns the underlying source
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Inline(s) => s.source(),
      Self::Block(s) => s.source(),
    }
  }

  /// Returns the reference to the underlying source
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Inline(s) => s.source_ref(),
      Self::Block(s) => s.source_ref(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<S> {
  span: Span,
  lit: LitStr<S>,
}

impl<S> AsSpan<Span> for StringValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for StringValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for StringValue<S> {
  type Components = (Span, LitStr<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.lit)
  }
}

impl<S> Display for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<S> StringValue<S> {
  #[inline(always)]
  pub(crate) const fn new(span: Span, lit: LitStr<S>) -> Self {
    Self { span, lit }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.lit.source()
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    self.lit.source_ref()
  }
}

impl<S> DisplaySDL for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> DisplaySyntaxTree for StringValue<S>
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
      "- STRING@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.source_ref().display(),
    )
  }
}

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for StringValue<&'a str>
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
        Ok(match tok {
          AstToken::InlineString(raw) => StringValue::new(span, raw.into()),
          AstToken::BlockString(raw) => StringValue::new(span, raw.into()),
          tok => return Err(Error::unexpected_token(tok, TokenKind::String, span).into()),
        })
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
