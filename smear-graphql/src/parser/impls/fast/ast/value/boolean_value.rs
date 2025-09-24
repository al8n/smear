use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{
  Lexed, Parseable,
  utils::{
    Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use crate::error::Error;

use super::*;

use core::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanValue<S> {
  source: S,
  span: Span,
  value: bool,
}

impl<S> Display for BooleanValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.slice_ref(), f)
  }
}

impl<S> AsRef<S> for BooleanValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for BooleanValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.slice_ref()
  }
}

impl<S> BooleanValue<S> {
  /// Creates a new boolean value.
  #[inline]
  pub(crate) const fn new(span: Span, source: S, value: bool) -> Self {
    Self {
      source,
      span,
      value,
    }
  }

  /// Returns the span of the boolean value.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the source of the boolean value.
  #[inline]
  pub const fn slice(&self) -> S
  where
    S: Copy,
  {
    self.source
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn slice_ref(&self) -> &S {
    &self.source
  }

  /// Returns the boolean value.
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }
}

impl<S> DisplaySDL for BooleanValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.slice_ref().fmt(f)
  }
}

impl<S> DisplaySyntaxTree for BooleanValue<S>
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
    let mut padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- BOOLEAN_VALUE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    let kw = if self.value { "true_KW" } else { "false_KW" };
    write!(
      f,
      "- {}@{}..{} \"{}\"",
      kw,
      self.span().start(),
      self.span().end(),
      self.slice_ref().display(),
    )
  }
}

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for BooleanValue<&'a str>
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
        match tok {
          FastToken::Identifier(ident) => Ok(match ident {
            "true" => BooleanValue::new(span, ident, true),
            "false" => BooleanValue::new(span, ident, false),
            val => return Err(Error::invalid_boolean_value(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, TokenKind::Boolean, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
