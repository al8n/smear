use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{
  Lexed, Parseable,
  utils::{
    Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use core::fmt::Display;

use crate::error::Error;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Kind {
  Inline,
  Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<S> {
  span: Span,
  raw: S,
  content: S,
  kind: Kind,
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
  pub(crate) const fn inline(span: Span, raw: S, content: S) -> Self {
    Self::new(span, raw, content, Kind::Inline)
  }

  #[inline(always)]
  pub(crate) const fn block(span: Span, raw: S, content: S) -> Self {
    Self::new(span, raw, content, Kind::Block)
  }

  #[inline(always)]
  const fn new(span: Span, raw: S, content: S, kind: Kind) -> Self {
    Self {
      span,
      raw,
      content,
      kind,
    }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the full raw string, including delimiters.
  #[inline]
  pub const fn raw(&self) -> &S {
    &self.raw
  }

  /// Returns the description content, without delimiters.
  #[inline]
  pub const fn content(&self) -> S
  where
    S: Copy,
  {
    self.content
  }

  /// Returns reference of the description content, without delimiters.
  #[inline]
  pub const fn content_ref(&self) -> &S {
    &self.content
  }
}

impl<S> DisplaySDL for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(&self.raw, f)
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
      self.raw.display(),
    )
  }
}

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for StringValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        Ok(match tok {
          FastToken::StringLiteral(raw) => {
            StringValue::new(span, raw, raw.trim_matches('"'), Kind::Inline)
          }
          FastToken::BlockStringLiteral(raw) => {
            StringValue::new(span, raw, raw.trim_matches('"'), Kind::Block)
          }
          tok => return Err(Error::unexpected_token(tok, TokenKind::String, span).into()),
        })
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
