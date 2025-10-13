use logosky::{
  Parseable,
  chumsky::{ParseResult, Parser},
  utils::recursion_tracker::RecursionLimitExceeded,
};

use super::error::{Error, Errors, Extra};
use crate::{
  lexer::graphqlx::ast::{AstToken, AstTokenChar, AstTokenKind},
  parser::{graphqlx::Expectation, ident::Ident},
};

pub use default::*;
pub use import::*;
pub use ty::*;
pub use value::*;

mod default;
mod error;
mod ident;
mod import;
mod keyword;
mod location;
mod operation_type;
mod punctuator;
mod ty;
mod value;

impl From<AstTokenKind> for Expectation {
  #[inline]
  fn from(kind: AstTokenKind) -> Self {
    match kind {
      AstTokenKind::Identifier => Self::Identifier,
      AstTokenKind::InlineString => Self::InlineString,
      AstTokenKind::BlockString => Self::BlockString,
      AstTokenKind::Dollar => Self::Dollar,
      AstTokenKind::At => Self::At,
      AstTokenKind::Ampersand => Self::Ampersand,
      AstTokenKind::Spread => Self::Spread,
      AstTokenKind::Pipe => Self::Pipe,
      AstTokenKind::Equal => Self::Equal,
      AstTokenKind::Colon => Self::Colon,
      AstTokenKind::Bang => Self::Bang,
      AstTokenKind::LBrace => Self::LBrace,
      AstTokenKind::RBrace => Self::RBrace,
      AstTokenKind::LBracket => Self::LBracket,
      AstTokenKind::RBracket => Self::RBracket,
      AstTokenKind::LParen => Self::LParen,
      AstTokenKind::RParen => Self::RParen,
      AstTokenKind::Int => Self::IntValue,
      AstTokenKind::Float => Self::FloatValue,
      AstTokenKind::Boolean => Self::BooleanValue,
      AstTokenKind::String => Self::StringValue,
      AstTokenKind::FatArrow => Self::FatArrow,
      AstTokenKind::LAngle => Self::LAngle,
      AstTokenKind::RAngle => Self::RAngle,
      AstTokenKind::Asterisk => Self::Asterisk,
      AstTokenKind::Plus => Self::Plus,
      AstTokenKind::Minus => Self::Minus,
      AstTokenKind::PathSeparator => Self::PathSeparator,
    }
  }
}

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;

/// The token stream type used for the AST parser implementation.
pub type AstTokenStream<'a, S> = logosky::TokenStream<'a, AstToken<S>>;
/// The parser extra type used for the AST parser implementation.
pub type AstParserExtra<'a, S> =
  Extra<S, AstToken<S>, AstTokenChar<'a, S>, Expectation, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type AstTokenError<'a, S> =
  Error<S, AstToken<S>, AstTokenChar<'a, S>, Expectation, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type AstTokenErrors<'a, S> =
  Errors<S, AstToken<S>, AstTokenChar<'a, S>, Expectation, RecursionLimitExceeded>;

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
