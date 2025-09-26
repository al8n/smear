#![allow(clippy::type_complexity)]

use logosky::{
  Parseable,
  chumsky::{ParseResult, Parser},
  utils::{Span, recursion_tracker::RecursionLimitExceeded},
};

use crate::{
  error::{Error, Errors, Extra},
  lexer::ast::{AstToken, TokenKind},
};

pub use fragment::*;
pub use name::*;
pub use raw::*;
pub use value::*;

mod error;
mod field;
mod fragment;
mod keyword;
mod location;
mod name;
mod operation_type;
mod punctuator;
mod raw;
mod selection_set;
mod value;

/// The token stream type used for the AST parser implementation.
pub type AstTokenStream<'a> = logosky::TokenStream<'a, AstToken<'a>>;
/// The parser extra type used for the AST parser implementation.
pub type AstParserExtra<'a, S> = Extra<S, AstToken<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type AstTokenError<'a, S> = Error<S, AstToken<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type AstTokenErrors<'a, S> = Errors<S, AstToken<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The token kind type used for the AST parser implementation.
pub type AstTokenKind = TokenKind;

/// Parse a value of type `T` from a string slice using the AST token.
pub trait ParseStr<'a> {
  /// Parses a value of this type from the given string slice.
  fn parse_str<S>(input: &'a S) -> ParseResult<Self, AstTokenErrors<'a, &'a str>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<str>;
}

impl<'b, T> ParseStr<'b> for T
where
  T: Parseable<'b, AstTokenStream<'b>, AstToken<'b>, AstTokenErrors<'b, &'b str>>,
{
  #[inline]
  fn parse_str<S>(input: &'b S) -> ParseResult<Self, AstTokenErrors<'b, &'b str>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<str>,
  {
    <T as Parseable<'b, AstTokenStream<'b>, AstToken<'b>, AstTokenErrors<'b, &'b str>>>::parser::<
      AstParserExtra<&str>,
    >()
    .parse(AstTokenStream::new(input.as_ref()))
  }
}

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;
