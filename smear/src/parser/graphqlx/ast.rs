use logosky::{
  Parseable,
  chumsky::{ParseResult, Parser},
  utils::recursion_tracker::RecursionLimitExceeded,
};

use super::error::{Error, Errors, Extra};
use crate::{
  lexer::graphqlx::ast::{SyntacticToken, SyntacticTokenChar, SyntacticTokenKind},
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

impl From<SyntacticTokenKind> for Expectation {
  #[inline]
  fn from(kind: SyntacticTokenKind) -> Self {
    match kind {
      SyntacticTokenKind::Identifier => Self::Identifier,
      SyntacticTokenKind::InlineString => Self::InlineString,
      SyntacticTokenKind::BlockString => Self::BlockString,
      SyntacticTokenKind::Dollar => Self::Dollar,
      SyntacticTokenKind::At => Self::At,
      SyntacticTokenKind::Ampersand => Self::Ampersand,
      SyntacticTokenKind::Spread => Self::Spread,
      SyntacticTokenKind::Pipe => Self::Pipe,
      SyntacticTokenKind::Equal => Self::Equal,
      SyntacticTokenKind::Colon => Self::Colon,
      SyntacticTokenKind::Bang => Self::Bang,
      SyntacticTokenKind::LBrace => Self::LBrace,
      SyntacticTokenKind::RBrace => Self::RBrace,
      SyntacticTokenKind::LBracket => Self::LBracket,
      SyntacticTokenKind::RBracket => Self::RBracket,
      SyntacticTokenKind::LParen => Self::LParen,
      SyntacticTokenKind::RParen => Self::RParen,
      SyntacticTokenKind::Int => Self::IntValue,
      SyntacticTokenKind::Float => Self::FloatValue,
      SyntacticTokenKind::FatArrow => Self::FatArrow,
      SyntacticTokenKind::LAngle => Self::LAngle,
      SyntacticTokenKind::RAngle => Self::RAngle,
      SyntacticTokenKind::Asterisk => Self::Asterisk,
      SyntacticTokenKind::Plus => Self::Plus,
      SyntacticTokenKind::Minus => Self::Minus,
      SyntacticTokenKind::PathSeparator => Self::PathSeparator,
    }
  }
}

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;

/// The token stream type used for the AST parser implementation.
pub type SyntacticTokenStream<'a, S> = logosky::TokenStream<'a, SyntacticToken<S>>;
/// The parser extra type used for the AST parser implementation.
pub type AstParserExtra<'a, S> =
  Extra<S, SyntacticToken<S>, SyntacticTokenChar<'a, S>, Expectation, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type SyntacticTokenError<'a, S> =
  Error<S, SyntacticToken<S>, SyntacticTokenChar<'a, S>, Expectation, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type SyntacticTokenErrors<'a, S> =
  Errors<S, SyntacticToken<S>, SyntacticTokenChar<'a, S>, Expectation, RecursionLimitExceeded>;

/// Parse a value of type `T` from a string slice using the AST token.
pub trait ParseStr<'a> {
  /// Parses a value of this type from the given string slice.
  fn parse_str<S>(input: &'a S) -> ParseResult<Self, SyntacticTokenErrors<'a, &'a str>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<str>;
}

impl<'b, T> ParseStr<'b> for T
where
  T: Parseable<
      'b,
      SyntacticTokenStream<'b, &'b str>,
      SyntacticToken<&'b str>,
      SyntacticTokenErrors<'b, &'b str>,
    >,
{
  #[inline]
  fn parse_str<S>(input: &'b S) -> ParseResult<Self, SyntacticTokenErrors<'b, &'b str>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<str>,
  {
    <T as Parseable<
      'b,
      SyntacticTokenStream<'b, &'b str>,
      SyntacticToken<&'b str>,
      SyntacticTokenErrors<'b, &'b str>,
    >>::parser::<AstParserExtra<&str>>()
    .parse(SyntacticTokenStream::new(input.as_ref()))
  }
}

/// Parse a value of type `T` from a bytes slice using the AST token.
pub trait ParseBytesSlice<'a> {
  /// Parses a value of this type from the given bytes slice.
  fn parse_bytes_slice<S>(input: &'a S) -> ParseResult<Self, SyntacticTokenErrors<'a, &'a [u8]>>
  where
    Self: Sized + 'a,
    S: ?Sized + AsRef<[u8]>;
}

impl<'b, T> ParseBytesSlice<'b> for T
where
  T: Parseable<
      'b,
      SyntacticTokenStream<'b, &'b [u8]>,
      SyntacticToken<&'b [u8]>,
      SyntacticTokenErrors<'b, &'b [u8]>,
    >,
{
  #[inline]
  fn parse_bytes_slice<S>(input: &'b S) -> ParseResult<Self, SyntacticTokenErrors<'b, &'b [u8]>>
  where
    Self: Sized + 'b,
    S: ?Sized + AsRef<[u8]>,
  {
    <T as Parseable<
      'b,
      SyntacticTokenStream<'b, &'b [u8]>,
      SyntacticToken<&'b [u8]>,
      SyntacticTokenErrors<'b, &'b [u8]>,
    >>::parser::<AstParserExtra<&[u8]>>()
    .parse(SyntacticTokenStream::new(input.as_ref()))
  }
}

/// Parse a value of type `T` from a bytes using the AST token.
#[cfg(feature = "bytes")]
#[cfg_attr(docsrs, doc(cfg(feature = "bytes")))]
pub trait ParseBytes<'a> {
  /// Parses a value of this type from the given bytes slice.
  fn parse_bytes(
    input: &'a bytes::Bytes,
  ) -> ParseResult<Self, SyntacticTokenErrors<'a, bytes::Bytes>>
  where
    Self: Sized + 'a;
}

#[cfg(feature = "bytes")]
const _: () = {
  use bytes::Bytes;
  use logosky::source::CustomSource;

  impl<'a, T> ParseBytes<'a> for T
  where
    T: Parseable<
        'a,
        SyntacticTokenStream<'a, Bytes>,
        SyntacticToken<Bytes>,
        SyntacticTokenErrors<'a, Bytes>,
      >,
  {
    #[inline]
    fn parse_bytes(input: &'a Bytes) -> ParseResult<Self, SyntacticTokenErrors<'a, Bytes>>
    where
      Self: Sized + 'a,
    {
      <T as Parseable<
        'a,
        SyntacticTokenStream<'a, Bytes>,
        SyntacticToken<Bytes>,
        SyntacticTokenErrors<'a, Bytes>,
      >>::parser::<AstParserExtra<Bytes>>()
      .parse(SyntacticTokenStream::new(CustomSource::from_ref(input)))
    }
  }
};
