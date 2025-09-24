#![allow(clippy::type_complexity)]

use logosky::{Parseable, utils::recursion_tracker::RecursionLimitExceeded};

use chumsky::{ParseResult, Parser};

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

/// Parse a value of type `T` from a string slice using the fast token.
pub trait ParseStr<'a> {
  /// Parses a value of this type from the given string slice.
  fn parse_str<S>(input: &'a S) -> ParseResult<Self, FastTokenErrors<'a, &'a str>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<str>;
}

impl<'b, T> ParseStr<'b> for T
where
  T: Parseable<'b, FastTokenStream<'b>, FastToken<'b>, FastTokenErrors<'b, &'b str>>,
{
  #[inline]
  fn parse_str<S>(input: &'b S) -> ParseResult<Self, FastTokenErrors<'b, &'b str>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<str>,
  {
    let s = input.as_ref();
    let parser = <T as Parseable<
      'b,
      FastTokenStream<'b>,
      FastToken<'b>,
      FastTokenErrors<'b, &'b str>,
    >>::parser::<FastParserExtra<&str>>();
    let tokens = FastTokenStream::new(s);
    parser.parse(tokens)
  }
}
