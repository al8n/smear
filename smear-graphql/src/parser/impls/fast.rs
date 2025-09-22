#![allow(clippy::type_complexity)]

use logosky::{Parseable, utils::recursion_tracker::RecursionLimitExceeded};

use chumsky::{ParseResult, Parser, extra::ParserExtra};

use crate::{
  error::{Error, Errors, Extra},
  lexer::token::fast::{Token, TokenKind},
};

pub use ast::*;

/// The token stream type used for the fast parser implementation.
pub type FastTokenStream<'a> = logosky::TokenStream<'a, Token<'a>>;
/// The parser extra type used for the fast parser implementation.
pub type FastParserExtra<'a, S> = Extra<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The error type used for the fast parser implementation.
pub type FastTokenError<'a, S> = Error<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The errors type used for the fast parser implementation.
pub type FastTokenErrors<'a, S> = Errors<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The token type used for the fast parser implementation.
pub type FastToken<'a> = Token<'a>;
/// The token kind type used for the fast parser implementation.
pub type FastTokenKind = TokenKind;

mod ast;

mod error;

pub trait ParseStr<'a> {
  fn parse_str<S>(input: &'a S) -> ParseResult<Self, FastTokenErrors<'a, &'a str>>
  where
    Self: Sized,
    S: ?Sized + AsRef<str>;
}

impl<'a, T> ParseStr<'a> for T
where
  T: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>,
{
  #[inline]
  fn parse_str<S>(input: &'a S) -> ParseResult<Self, FastTokenErrors<'a, &'a str>>
  where
    Self: Sized,
    S: ?Sized + AsRef<str>,
  {
    let s = input.as_ref();
    let parser =
      <T as Parseable<FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>>::parser::<
        FastParserExtra<&str>,
      >();
    let tokens = FastTokenStream::new(s);
    parser.parse(tokens)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test() {
    <TypeSystemDefinitionOrExtension<&str> as ParseStr<'_>>::parse_str(r#"{ field }"#).unwrap();
  }
}
