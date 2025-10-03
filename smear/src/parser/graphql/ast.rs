#![allow(clippy::type_complexity)]

use logosky::{
  Parseable,
  chumsky::{ParseResult, Parser},
  utils::{Span, recursion_tracker::RecursionLimitExceeded},
};

use super::error::{Error, Errors, Extra};
use crate::lexer::graphql::ast::{AstToken, AstTokenChar, AstTokenKind};

pub use fragment::*;
pub use location::*;
pub use name::*;
pub use raw::*;
pub use ty::*;
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
mod ty;
mod value;

/// The token stream type used for the AST parser implementation.
pub type AstTokenStream<'a, S> = logosky::TokenStream<'a, AstToken<S>>;
/// The parser extra type used for the AST parser implementation.
pub type AstParserExtra<'a, S> =
  Extra<S, AstToken<S>, AstTokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type AstTokenError<'a, S> =
  Error<S, AstToken<S>, AstTokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type AstTokenErrors<'a, S> =
  Errors<S, AstToken<S>, AstTokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;

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
  T: Parseable<'b, AstTokenStream<'b, &'b str>, AstToken<&'b str>, AstTokenErrors<'b, &'b str>>,
{
  #[inline]
  fn parse_str<S>(input: &'b S) -> ParseResult<Self, AstTokenErrors<'b, &'b str>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<str>,
  {
    <T as Parseable<
      'b,
      AstTokenStream<'b, &'b str>,
      AstToken<&'b str>,
      AstTokenErrors<'b, &'b str>,
    >>::parser::<AstParserExtra<&str>>()
    .parse(AstTokenStream::new(input.as_ref()))
  }
}

/// Parse a value of type `T` from a bytes slice using the AST token.
pub trait ParseBytesSlice<'a> {
  /// Parses a value of this type from the given bytes slice.
  fn parse_bytes_slice<S>(input: &'a S) -> ParseResult<Self, AstTokenErrors<'a, &'a [u8]>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<[u8]>;
}

impl<'b, T> ParseBytesSlice<'b> for T
where
  T: Parseable<'b, AstTokenStream<'b, &'b [u8]>, AstToken<&'b [u8]>, AstTokenErrors<'b, &'b [u8]>>,
{
  #[inline]
  fn parse_bytes_slice<S>(input: &'b S) -> ParseResult<Self, AstTokenErrors<'b, &'b [u8]>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<[u8]>,
  {
    <T as Parseable<
      'b,
      AstTokenStream<'b, &'b [u8]>,
      AstToken<&'b [u8]>,
      AstTokenErrors<'b, &'b [u8]>,
    >>::parser::<AstParserExtra<&[u8]>>()
    .parse(AstTokenStream::new(input.as_ref()))
  }
}

/// Parse a value of type `T` from a bytes using the AST token.
#[cfg(feature = "bytes")]
#[cfg_attr(docsrs, doc(cfg(feature = "bytes")))]
pub trait ParseBytes<'a> {
  /// Parses a value of this type from the given bytes slice.
  fn parse_bytes(input: &'a bytes::Bytes) -> ParseResult<Self, AstTokenErrors<'a, bytes::Bytes>>
  where
    Self: Sized + 'a;
}

#[cfg(feature = "bytes")]
const _: () = {
  use bytes::Bytes;
  use logosky::source::CustomSource;

  impl<'a, T> ParseBytes<'a> for T
  where
    T: Parseable<'a, AstTokenStream<'a, Bytes>, AstToken<Bytes>, AstTokenErrors<'a, Bytes>>,
  {
    #[inline]
    fn parse_bytes(input: &'a Bytes) -> ParseResult<Self, AstTokenErrors<'a, Bytes>>
    where
      Self: Sized + 'a,
    {
      <T as Parseable<'a, AstTokenStream<'a, Bytes>, AstToken<Bytes>, AstTokenErrors<'a, Bytes>>>::parser::<
        AstParserExtra<Bytes>,
      >()
      .parse(AstTokenStream::new(CustomSource::from_ref(input)))
    }
  }
};
