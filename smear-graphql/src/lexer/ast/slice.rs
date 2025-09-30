use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexable,
  logos::Logos,
  utils::{
    IntoEquivalent,
    recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
  },
};

use super::{
  super::{
    handlers::{slice::*, unterminated_spread_operator},
    string_lexer::SealedWrapper,
  },
  AstToken, LitBlockStr, LitInlineStr, TokenKind, decrease_recursion_depth,
  increase_recursion_depth,
};

use crate::error::{self, *};
use sealed::Token;

pub type BytesSliceAstToken<'a> = AstToken<&'a [u8]>;

#[cfg(feature = "bytes")]
#[cfg_attr(docsrs, doc(cfg(feature = "bytes")))]
pub type BytesAstToken = AstToken<bytes::Bytes>;

/// The error type for lexing based on str AST [`Token`](super::Token).
type SliceLexerError = error::LexerError<u8, RecursionLimitExceeded>;
/// A collection of errors of str AST [`Token`](super::Token).
type SliceLexerErrors = error::LexerErrors<u8, RecursionLimitExceeded>;

impl<'a> logosky::Token<'a> for AstToken<&'a [u8]> {
  type Char = u8;
  type Kind = TokenKind;

  type Logos = Token<'a>;

  #[inline(always)]
  fn from_logos(value: Self::Logos) -> Self {
    value.into()
  }

  #[inline(always)]
  fn kind(&self) -> Self::Kind {
    self.kind()
  }
}

#[cfg(feature = "bytes")]
const _: () = {
  use bytes::Bytes;
  use logosky::source::CustomSource;

  impl<'a> logosky::Token<'a> for AstToken<Bytes> {
    type Kind = TokenKind;
    type Char = u8;
    type Logos = Token<'a>;

    #[inline]
    fn from_logos(value: Self::Logos) -> Self {
      value.into()
    }

    #[inline(always)]
    fn kind(&self) -> Self::Kind {
      self.kind()
    }
  }

  impl<'a> logosky::Token<'a> for AstToken<CustomSource<Bytes>> {
    type Kind = TokenKind;
    type Char = u8;
    type Logos = Token<'a>;

    #[inline]
    fn from_logos(value: Self::Logos) -> Self {
      value.into()
    }

    #[inline(always)]
    fn kind(&self) -> Self::Kind {
      self.kind()
    }
  }
};

#[cfg(feature = "hipstr")]
const _: () = {
  use hipstr::HipByt;

  impl<'a> logosky::Token<'a> for AstToken<HipByt<'a>> {
    type Kind = TokenKind;
    type Char = u8;
    type Logos = Token<'a>;

    #[inline(always)]
    fn from_logos(value: Self::Logos) -> Self {
      AstToken::<HipByt>::from(value)
    }

    #[inline(always)]
    fn kind(&self) -> Self::Kind {
      self.kind()
    }
  }
};

impl<'a, T> From<Token<'a>> for AstToken<T>
where
  &'a [u8]: IntoEquivalent<T>,
{
  #[inline(always)]
  fn from(value: Token<'a>) -> Self {
    match value {
      Token::Ampersand => Self::Ampersand,
      Token::At => Self::At,
      Token::RBrace => Self::RBrace,
      Token::RBracket => Self::RBracket,
      Token::RParen => Self::RParen,
      Token::Colon => Self::Colon,
      Token::Dollar => Self::Dollar,
      Token::Equal => Self::Equal,
      Token::Bang => Self::Bang,
      Token::LBrace => Self::LBrace,
      Token::LBracket => Self::LBracket,
      Token::LParen => Self::LParen,
      Token::Pipe => Self::Pipe,
      Token::Spread => Self::Spread,
      Token::Float(v) => Self::Float(v.into_equivalent()),
      Token::Identifier(v) => Self::Identifier(v.into_equivalent()),
      Token::Int(v) => Self::Int(v.into_equivalent()),
      Token::LitInlineStr(v) => Self::LitInlineStr(v.into_equivalent()),
      Token::LitBlockStr(v) => Self::LitBlockStr(v.into_equivalent()),
    }
  }
}

mod sealed {
  use super::*;

  /// Abstract syntax tree (AST) token lexer for the GraphQL specification: http://spec.graphql.org/.
  ///
  /// Used for the AST parser implementation.
  #[derive(
    Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
  )]
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  #[logos(
  crate = logosky::logos,
  source = [u8],
  extras = RecursionLimiter,
  skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*",
  error(SliceLexerErrors, |lexer| match lexer.slice().iter().next() {
    Some(&ch) => SliceLexerError::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => SliceLexerError::unexpected_eoi(lexer.span().into()),
  }.into())
)]
  pub enum Token<'a> {
    /// Ampersand `&` token
    #[token("&")]
    Ampersand,

    /// At `@` token
    #[token("@")]
    At,

    /// Right curly brace `}` token
    #[token("}", decrease_recursion_depth)]
    RBrace,

    /// Right square bracket `]` token
    #[token("]", decrease_recursion_depth)]
    RBracket,

    /// Right parenthesis `)` token
    #[token(")", decrease_recursion_depth)]
    RParen,

    /// Dot `.` token
    #[token(":")]
    Colon,

    /// Dollar `$` token
    #[token("$")]
    Dollar,

    /// Equal `=` token
    #[token("=")]
    Equal,

    /// Exclamation mark `!` token
    #[token("!")]
    Bang,

    /// Left curly brace `{` token
    #[token("{", increase_recursion_depth)]
    LBrace,

    /// Left square bracket `[` token
    #[token("[", increase_recursion_depth)]
    LBracket,

    /// Left parenthesis `(` token
    #[token("(", increase_recursion_depth)]
    LParen,

    /// Pipe `|` token
    #[token("|")]
    Pipe,

    /// Spread operator `...` token
    #[token("...")]
    #[token("..", unterminated_spread_operator)]
    #[token(".", unterminated_spread_operator)]
    Spread,

    /// Float literal token
    #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix))]
    #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handle_number_suffix(lexer, FloatError::UnexpectedSuffix))]
    #[regex(
      "-?\\.[0-9]+([eE][+-]?[0-9]+)?",
      handle_float_missing_integer_part_error_and_suffix
    )]
    #[regex("-?0[0-9]+\\.[0-9]+[eE][+-]?", handle_leading_zeros_and_exponent_error)]
    #[regex("-?(0|[1-9][0-9]*)\\.[0-9]+[eE][+-]?", handle_exponent_error)]
    #[regex("-?0[0-9]+\\.", handle_leading_zeros_and_fractional_error)]
    #[regex("-?(0|[1-9][0-9]*)\\.", handle_fractional_error)]
    #[regex("-?0[0-9]+[eE][+-]?", handle_leading_zeros_and_exponent_error)]
    #[regex("-?(0|[1-9][0-9]*)[eE][+-]?", handle_exponent_error)]
    Float(&'a [u8]),

    /// Identifier token
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'a [u8]),

    /// Integer literal token
    #[regex("-?(0|[1-9][0-9]*)", |lexer| handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
    #[regex("-?0[0-9]+", |lexer| handle_leading_zero_and_number_suffix_error(lexer, IntError::LeadingZeros, IntError::UnexpectedSuffix))]
    #[token("-", |lexer| Err(LexerError::unexpected_char(lexer.span().into(), b'-', lexer.span().start)))]
    #[token("+", |lexer| Err(LexerError::unexpected_char(lexer.span().into(), b'+', lexer.span().start)))]
    Int(&'a [u8]),
    #[token("\"", |lexer| {
    <LitInlineStr<&[u8]> as Lexable<_, SliceLexerError>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, Token<'_>>>::from_mut(lexer))
  })]
    LitInlineStr(LitInlineStr<&'a [u8]>),
    #[token("\"\"\"", |lexer| {
    <LitBlockStr<&[u8]> as Lexable<_, SliceLexerError>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, Token<'_>>>::from_mut(lexer))
  })]
    LitBlockStr(LitBlockStr<&'a [u8]>),
  }
}
