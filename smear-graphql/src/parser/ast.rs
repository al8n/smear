#![allow(clippy::type_complexity)]

use logosky::{
  Parseable,
  chumsky::{ParseResult, Parser},
  utils::{Span, recursion_tracker::RecursionLimitExceeded},
};

use crate::{
  error::{Error, Errors, Extra},
  lexer::ast::{AstToken, AstTokenChar, TokenKind},
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

pub type StrAstToken<'a> = AstToken<&'a str>;
/// The token stream type used for the AST parser implementation.
pub type StrAstTokenStream<'a> = logosky::TokenStream<'a, AstToken<&'a str>>;
/// The parser extra type used for the AST parser implementation.
pub type StrAstParserExtra<'a, S> =
  Extra<S, AstToken<&'a str>, TokenKind, char, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type StrAstTokenError<'a, S> =
  Error<S, AstToken<&'a str>, TokenKind, char, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type StrAstTokenErrors<'a, S> =
  Errors<S, AstToken<&'a str>, TokenKind, char, RecursionLimitExceeded>;
/// The token kind type used for the AST parser implementation.
pub type StrAstTokenKind = TokenKind;

/// The token stream type used for the AST parser implementation.
pub type AstTokenStream<'a, S> = logosky::TokenStream<'a, AstToken<S>>;
/// The parser extra type used for the AST parser implementation.
pub type AstParserExtra<'a, S> =
  Extra<S, AstToken<S>, TokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type AstTokenError<'a, S> =
  Error<S, AstToken<S>, TokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type AstTokenErrors<'a, S> =
  Errors<S, AstToken<S>, TokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The token kind type used for the AST parser implementation.
pub type AstTokenKind = TokenKind;

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
