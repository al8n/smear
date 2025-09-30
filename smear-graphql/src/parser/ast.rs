#![allow(clippy::type_complexity)]

use logosky::{
  Parseable,
  chumsky::{ParseResult, Parser},
  utils::{Span, recursion_tracker::RecursionLimitExceeded},
};

use crate::{
  error::{Error, Errors, Extra},
  lexer::ast::{StrAstToken, TokenKind},
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
pub type StrAstTokenStream<'a> = logosky::TokenStream<'a, StrAstToken<'a>>;
/// The parser extra type used for the AST parser implementation.
pub type StrAstParserExtra<'a, S> =
  Extra<S, StrAstToken<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type StrAstTokenError<'a, S> =
  Error<S, StrAstToken<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type StrAstTokenErrors<'a, S> =
  Errors<S, StrAstToken<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The token kind type used for the AST parser implementation.
pub type StrAstTokenKind = TokenKind;

/// Parse a value of type `T` from a string slice using the AST token.
pub trait ParseStr<'a> {
  /// Parses a value of this type from the given string slice.
  fn parse_str<S>(input: &'a S) -> ParseResult<Self, StrAstTokenErrors<'a, &'a str>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<str>;
}

impl<'b, T> ParseStr<'b> for T
where
  T: Parseable<'b, StrAstTokenStream<'b>, StrAstToken<'b>, StrAstTokenErrors<'b, &'b str>>,
{
  #[inline]
  fn parse_str<S>(input: &'b S) -> ParseResult<Self, StrAstTokenErrors<'b, &'b str>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<str>,
  {
    <T as Parseable<'b, StrAstTokenStream<'b>, StrAstToken<'b>, StrAstTokenErrors<'b, &'b str>>>::parser::<
      StrAstParserExtra<&str>,
    >()
    .parse(StrAstTokenStream::new(input.as_ref()))
  }
}

/// Parse a value of type `T` from a string slice using the AST token.
pub trait ParseSlice<'a> {
  /// Parses a value of this type from the given string slice.
  fn parse_slice<S>(input: &'a S) -> ParseResult<Self, StrAstTokenErrors<'a, &'a [u8]>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<[u8]>;
}

// impl<'b, T> ParseSlice<'b> for T
// where
//   T: Parseable<'b, StrAstTokenStream<'b>, StrAstToken<'b>, StrAstTokenErrors<'b, &'b [u8]>>,
// {
//   #[inline]
//   fn parse_slice<S>(input: &'b S) -> ParseResult<Self, StrAstTokenErrors<'b, &'b [u8]>>
//   where
//     Self: Sized + 'b,
//     S: ?Sized + AsRef<[u8]>,
//   {
//     <T as Parseable<'b, StrAstTokenStream<'b>, StrAstToken<'b>, StrAstTokenErrors<'b, &'b [u8]>>>::parser::<
//       StrAstParserExtra<&[u8]>,
//     >()
//     .parse(StrAstTokenStream::new(input.as_ref()))
//   }
// }

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;
