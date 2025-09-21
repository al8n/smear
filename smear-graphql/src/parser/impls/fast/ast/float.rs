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
pub struct FloatValue<S> {
  span: Span,
  value: S,
}

impl<S> Display for FloatValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
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
    self.source()
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
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn source(&self) -> &S {
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
      self.source().display(),
    )
  }
}

impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>>
  for FloatValue<&'a str>
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
          Token::Float(val) => Ok(Self::new(span, val)),
          tok => Err(Error::unexpected_token(tok, TokenKind::Float, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::fast::FastParserExtra;

  use super::*;

  #[test]
  fn test_float_parser() {
    let parser = FloatValue::parser::<FastParserExtra<&str>>();
    let input = r#"1.3"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "1.3");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
