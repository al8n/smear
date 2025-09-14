use core::fmt::Display;

use chumsky::{Parser, extra::Full, prelude::any};
use logosky::{Lexed, TokenStream, Tokenizer, utils::{Span, sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree}};

use crate::lexer::token::fast;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Kind {
  Inline,
  Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<'a> {
  pub(crate) span: Span,
  pub(crate) raw: &'a str,
  pub(crate) content: &'a str,
  pub(crate) kind: Kind,
}

impl<'a> Display for StringValue<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<'a> AsRef<str> for StringValue<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.content
  }
}

impl<'a> core::ops::Deref for StringValue<'a> {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.content
  }
}

impl<'a> StringValue<'a> {
  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn as_str(&self) -> &'a str {
    self.content
  }

  pub fn parser() -> impl Parser<'a, TokenStream<'a, fast::Token<'a>>, Self, super::ParserExtra<'a, fast::Token<'a>>>
  {
    any().try_map(|res, span| {
      match res {
        Lexed::Token(tok) => {
          match tok {
            fast::Token::StringLiteral(data) => {
              todo!()
            },
            fast::Token::BlockStringLiteral(data) => {
              todo!()
            },
            _ => Err(todo!()),
          }
        },
        Lexed::Error(err) => {
          todo!()
        }
      }
    })
  }
}

impl<'a> DisplaySDL for StringValue<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.raw.fmt(f)
  }
}

impl<'a> DisplaySyntaxTree for StringValue<'a> {
  #[inline]
  fn fmt(&self, level: usize, indent: usize, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(f, "- STRING@{}..{} \"{}\"", self.span.start(), self.span.end(), self.raw)
  }
}
