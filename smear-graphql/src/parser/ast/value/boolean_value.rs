use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{Span, sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree},
};

use crate::{error::Error, lexer::ast::TokenKind};

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
  pub const fn span(&self) -> Span {
    self.span
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

impl DisplaySyntaxTree for BooleanValue {
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
      self.value(),
    )
  }
}

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for BooleanValue
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
          AstToken::Identifier(ident) => Ok(match ident {
            "true" => BooleanValue::new(span, true),
            "false" => BooleanValue::new(span, false),
            val => return Err(Error::invalid_boolean_value(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, TokenKind::Boolean, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
