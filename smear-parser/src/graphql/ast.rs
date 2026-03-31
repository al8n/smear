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

pub use combinators::*;
pub use default::*;
pub use fragment::*;
pub use name::*;
pub use ty::*;
pub use type_system::*;
pub use value::*;

pub mod combinators;
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
/// Returns `Ok(spanned_token)` on success, or an appropriate error.
/// Avoids cursor clone on the happy path (token found).
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
  match input.next()? {
    Some(spanned) => Ok(spanned),
    None => {
      let span = input.span().clone();
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

// Implement From<FullContainer> for SyntacticTokenErrors to satisfy FullContainerEmitter.
// With Vec-based containers this error should never actually be triggered in practice.
impl<S, Lang: ?Sized> From<smear_lexer::tokit::error::syntax::FullContainer<Span, Lang>> for SyntacticTokenErrors<S> {
  #[inline]
  fn from(err: smear_lexer::tokit::error::syntax::FullContainer<Span, Lang>) -> Self {
    SyntacticTokenError::new(
      *err.span(),
      super::error::ErrorData::Other(std::borrow::Cow::Borrowed("container full")),
    ).into()
  }
}

// Implement FromSeparatedError for SyntacticTokenErrors to satisfy SeparatedEmitter via FatalContext.
// The `FromEmitterError` supertrait is satisfied by the blanket impl in tokit via the
// `From<SyntacticLexerErrors>` and `From<UnexpectedTokenOf>` impls above, but only when the
// compiler can resolve `<SyntacticToken<S> as Token>::Error`. We add an explicit
// `SyntacticTokenErrors<S>: From<<SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Error>`
// bound so the blanket can kick in.
impl<'inp, S: Clone> smear_lexer::tokit::emitter::FromSeparatedError<'inp, SyntacticLexer<'inp, S>> for SyntacticTokenErrors<S>
where
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span, Offset = usize>,
  SyntacticTokenErrors<S>: From<<SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Error>,
{
  #[inline]
  fn from_missing_separator(
    _name: smear_lexer::tokit::utils::CowStr,
    err: smear_lexer::tokit::error::token::MissingTokenOf<'inp, SyntacticLexer<'inp, S>>,
  ) -> Self {
    let off = err.offset();
    let span = Span::new(off, off);
    SyntacticTokenError::new(
      span,
      super::error::ErrorData::Other(std::borrow::Cow::Borrowed("missing separator")),
    ).into()
  }

  #[inline]
  fn from_missing_element(
    err: smear_lexer::tokit::error::syntax::MissingSyntaxOf<'inp, SyntacticLexer<'inp, S>>,
  ) -> Self {
    let off = err.offset();
    let span = Span::new(off, off);
    SyntacticTokenError::new(
      span,
      super::error::ErrorData::Other(std::borrow::Cow::Borrowed("missing element")),
    ).into()
  }
}

// Implement FromUnexpectedTrailingSeparatorError for SyntacticTokenErrors.
// Required by the `allow_leading` combinator variant.
impl<'inp, S: Clone> smear_lexer::tokit::emitter::FromUnexpectedTrailingSeparatorError<'inp, SyntacticLexer<'inp, S>> for SyntacticTokenErrors<S>
where
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span, Offset = usize>,
  SyntacticTokenErrors<S>: From<<SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Error>,
{
  #[inline]
  fn from_unexpected_trailing_separator(
    _name: smear_lexer::tokit::utils::CowStr,
    err: UnexpectedTokenOf<'inp, SyntacticLexer<'inp, S>>,
  ) -> Self {
    let (span, found, _expected) = err.into_components();
    match found {
      Some(token) => SyntacticTokenError::unexpected_token(token, Expectation::Name, span).into(),
      None => SyntacticTokenError::unexpected_end_of_input(span).into(),
    }
  }
}

// Implement FromUnexpectedLeadingSeparatorError for SyntacticTokenErrors.
// Required when the `UnexpectedLeadingSeparatorEmitter` bound is present on emitters.
impl<'inp, S: Clone> smear_lexer::tokit::emitter::FromUnexpectedLeadingSeparatorError<'inp, SyntacticLexer<'inp, S>> for SyntacticTokenErrors<S>
where
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span, Offset = usize>,
  SyntacticTokenErrors<S>: From<<SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Error>,
{
  #[inline]
  fn from_unexpected_leading_separator(
    _name: smear_lexer::tokit::utils::CowStr,
    err: UnexpectedTokenOf<'inp, SyntacticLexer<'inp, S>>,
  ) -> Self {
    let (span, found, _expected) = err.into_components();
    match found {
      Some(token) => SyntacticTokenError::unexpected_token(token, Expectation::Name, span).into(),
      None => SyntacticTokenError::unexpected_end_of_input(span).into(),
    }
  }
}

// Implement From<TooFew> for SyntacticTokenErrors to satisfy TooFewEmitter (at_least).
impl<S, Lang: ?Sized> From<smear_lexer::tokit::error::syntax::TooFew<Span, Lang>> for SyntacticTokenErrors<S> {
  #[inline]
  fn from(err: smear_lexer::tokit::error::syntax::TooFew<Span, Lang>) -> Self {
    SyntacticTokenError::new(
      err.span().clone(),
      super::error::ErrorData::Other(std::borrow::Cow::Borrowed("too few elements")),
    ).into()
  }
}

/// Helper to run a parse function against a string input using tokit's Parser infrastructure.
pub fn run_parse_str<'inp, O>(
  f: impl for<'c> FnMut(&mut InputRef<'inp, 'c, SyntacticLexer<'inp, &'inp str>, smear_lexer::tokit::FatalContext<'inp, SyntacticLexer<'inp, &'inp str>, SyntacticTokenErrors<&'inp str>>>) -> Result<O, SyntacticTokenErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, SyntacticTokenErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}
