use core::fmt::Display;

use chumsky::{Parser, extra::Full, prelude::any};
use logos::Logos;
use logosky::{
  Lexed, Token, TokenStream, Tokenizer, utils::{Span, recursion_tracker::RecursionLimitExceeded, sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree}
};

use crate::{error::{Err, Error, Errors}, lexer::token::fast};

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

  // pub fn parser<I, T>()
  // -> impl Parser<'a, I, Self, Err<T, T::Kind, T::Char, T::Extras>>
  // where
  //   T: Token<'a>,
  //   I: Tokenizer<'a, Token = Lexed<'a, T>>,
  // {
  //   any().try_map(|res, span| match res {
  //     Lexed::Token(tok) => match tok {
  //       fast::Token::StringLiteral(data) => {
  //         todo!()
  //       }
  //       fast::Token::BlockStringLiteral(data) => {
  //         todo!()
  //       }
  //       _ => Err(todo!()),
  //     },
  //     Lexed::Error(err) => {
  //       todo!()
  //     }
  //   })
  // }
}

impl<'a> DisplaySDL for StringValue<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.raw.fmt(f)
  }
}

impl<'a> DisplaySyntaxTree for StringValue<'a> {
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
      self.raw
    )
  }
}

pub trait Parseable<'a, I, E> {
  type Token: Token<'a>;

  fn parser() -> impl Parser<'a, I, Self, E>
  where
    Self: Sized,
    I: Tokenizer<'a, Token = Lexed<'a, Self::Token>, Span = Span>,
    E: chumsky::extra::ParserExtra<'a, I>;
}

impl<'a> Parseable<'a, TokenStream<'a, fast::Token<'a>>, Err<fast::Token<'a>, fast::TokenKind, char, RecursionLimitExceeded>> for StringValue<'a> {
  type Token = fast::Token<'a>;

  fn parser() -> impl Parser<'a, TokenStream<'a, fast::Token<'a>>, Self, Err<fast::Token<'a>, fast::TokenKind, char, RecursionLimitExceeded>>
  where
    Self: Sized,
    TokenStream<'a, fast::Token<'a>>: Tokenizer<'a, Token = Lexed<'a, Self::Token>>,
    Err<fast::Token<'a>, fast::TokenKind, char, RecursionLimitExceeded>: chumsky::extra::ParserExtra<'a, TokenStream<'a, fast::Token<'a>>>
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        fast::Token::StringLiteral(raw) => Ok(StringValue {
          span,
          raw,
          content: raw,
          kind: Kind::Inline,
        }),
        fast::Token::BlockStringLiteral(raw) => Ok(StringValue {
          span,
          raw,
          content: raw,
          kind: Kind::Block,
        }),
        tok => Err(Error::unexpected_token(tok, fast::TokenKind::String, span)),
      },
      Lexed::Error(err) => todo!(),
    })  
  }
}
