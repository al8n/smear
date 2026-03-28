#![allow(clippy::type_complexity)]

use std::vec::Vec;

use smear_lexer::tokit::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::Spanned,
  lexer::FromLogos,
};

use super::Expectation;
use super::error::{Error, Errors};
use crate::lexer::graphqlx::syntactic::{
  SyntacticLexer, SyntacticToken, SyntacticTokenKind,
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
pub trait ParseStr {
  /// Parses a value of this type from the given string slice.
  fn parse_str(input: &str) -> Result<Self, SyntacticTokenErrors<&str>>
  where
    Self: Sized;
}
