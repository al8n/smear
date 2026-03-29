#![allow(clippy::type_complexity)]

use std::vec::Vec;

// PhantomData is unused currently but reserved for future use.
#[allow(unused_imports)]
use core::marker::PhantomData;

use smear_lexer::tokit::{
  Emitter, InputRef, Lexer, Parse, ParseContext, Parser, SimpleSpan as Span,
  error::token::UnexpectedTokenOf,
  lexer::FromLogos,
  span::Spanned,
  state::recursion_tracker::RecursionLimitExceeded,
};

use super::Expectation;
use super::error::{Error, Errors};
use crate::lexer::graphql::syntactic::{
  SyntacticLexer, SyntacticLexerErrors, SyntacticToken, SyntacticTokenKind,
};

pub use default::*;
pub use fragment::*;
pub use name::*;
pub use ty::*;
pub use type_system::*;
pub use value::*;

mod default;
mod error;
mod fragment;
mod keyword;
mod location;
mod name;
mod operation_type;
mod parse_str_impl;
mod punctuator;
mod ty;
mod type_system;
mod value;

impl From<SyntacticTokenKind> for Expectation {
  #[inline]
  fn from(kind: SyntacticTokenKind) -> Self {
    match kind {
      SyntacticTokenKind::Identifier => Self::Name,
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
      _ => unreachable!("unexpected token kind in parser: {:?}", kind),
    }
  }
}

/// The error type used for the AST parser implementation.
pub type SyntacticTokenError<S> =
  Error<S, SyntacticToken<S>, char, Expectation>;
/// The errors type used for the AST parser implementation.
pub type SyntacticTokenErrors<S> =
  Errors<S, SyntacticToken<S>, char, Expectation>;

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;

/// Helper to consume a token from an InputRef and produce a span.
///
/// Returns `Ok((span, token))` on success, or an appropriate error.
#[inline]
fn next_token<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Spanned<SyntacticToken<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  match input.next()? {
    Some(spanned) => Ok(spanned),
    None => {
      let span = input.span_since(&cursor);
      Err(SyntacticTokenError::unexpected_end_of_input(span).into())
    }
  }
}

/// Parse a value of type `T` from a string slice using the AST token.
///
/// This trait is implemented for types parameterized by `&'a str`, where the
/// lifetime connects the input string to the parsed output.
///
/// # Lifetime
///
/// The lifetime parameter `'a` connects the input string to the parsed output.
/// The parsed AST borrows from the input string.
pub trait ParseStr<'a>: Sized {
  /// Parses a value of this type from the given string slice.
  fn parse_str(input: &'a str) -> Result<Self, SyntacticTokenErrors<&'a str>>;
}

/// Parse a value of type `T` from a byte slice using the AST token.
pub trait ParseBytesSlice {
  /// Parses a value of this type from the given byte slice.
  fn parse_bytes_slice(input: &[u8]) -> Result<Self, SyntacticTokenErrors<&[u8]>>
  where
    Self: Sized;
}

/// Parse a value of type `T` from bytes using the AST token.
#[cfg(feature = "bytes")]
pub trait ParseBytes {
  /// Parses a value of this type from the given bytes.
  fn parse_bytes(input: &bytes::Bytes) -> Result<Self, SyntacticTokenErrors<bytes::Bytes>>
  where
    Self: Sized;
}

// Implement From<SyntacticLexerErrors> for SyntacticTokenErrors to satisfy FromEmitterError.
// This converts lexer errors (which include RecursionLimitExceeded) into parser errors.
impl<S> From<SyntacticLexerErrors> for SyntacticTokenErrors<S> {
  #[inline]
  fn from(err: SyntacticLexerErrors) -> Self {
    SyntacticTokenError::new(
      Span::new(0, 0),
      super::error::ErrorData::Other(std::borrow::Cow::Borrowed("lexer error")),
    ).into()
  }
}

// Implement From<UnexpectedTokenOf> for SyntacticTokenErrors to satisfy FromEmitterError.
impl<'inp, S: Clone> From<UnexpectedTokenOf<'inp, SyntacticLexer<'inp, S>>> for SyntacticTokenErrors<S>
where
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
{
  #[inline]
  fn from(err: UnexpectedTokenOf<'inp, SyntacticLexer<'inp, S>>) -> Self {
    let (span, found, _expected) = err.into_components();
    // Convert the expected token kinds to our Expectation type.
    // We take the first expected kind if available.
    let expectation = Expectation::Name; // default fallback
    match found {
      Some(token) => SyntacticTokenError::unexpected_token(token, expectation, span).into(),
      None => SyntacticTokenError::unexpected_end_of_input(span).into(),
    }
  }
}

/// Helper to run a parse function against a string input using tokit's Parser infrastructure.
pub fn run_parse_str<'inp, O>(
  f: impl for<'c> FnMut(&mut InputRef<'inp, 'c, SyntacticLexer<'inp, &'inp str>, smear_lexer::tokit::FatalContext<'inp, SyntacticLexer<'inp, &'inp str>, SyntacticTokenErrors<&'inp str>>>) -> Result<O, SyntacticTokenErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, SyntacticTokenErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}
