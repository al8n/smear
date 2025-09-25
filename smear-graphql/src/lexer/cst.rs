use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  logos::{Lexer, Logos},
  utils::tracker::{LimitExceeded, Tracker},
};

use super::{handlers::*, string_token::*};

use crate::error::{self, *};

#[cfg(test)]
mod tests;

/// The error data type for lexing based on lossless [`Token`].
pub type LexerErrorData = error::LexerErrorData<char, LimitExceeded>;
/// The error type for lexing based on lossless [`Token`].
pub type LexerError = error::LexerError<char, LimitExceeded>;
/// A collection of errors of lossless [`Token`].
pub type LexerErrors = error::LexerErrors<char, LimitExceeded>;

#[inline(always)]
pub(super) fn increase_recursion_depth_and_token<'a>(
  lexer: &mut Lexer<'a, CstToken<'a>>,
) -> Result<(), LexerError> {
  lexer.extras.increase_recursion();
  lexer.extras.increase_token();
  lexer
    .extras
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e)))
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a>(lexer: &mut Lexer<'a, CstToken<'a>>) {
  lexer.extras.decrease_recursion();
  // right punctuation also increases the token count
  lexer.extras.increase_token();
}

#[inline(always)]
pub(super) fn increase_token<'a>(lexer: &mut Lexer<'a, CstToken<'a>>) {
  lexer.extras.increase_token();
}

#[inline(always)]
fn tt_hook_and_then<'a, O>(
  lexer: &mut Lexer<'a, CstToken<'a>>,
  f: impl FnOnce(&mut Lexer<'a, CstToken<'a>>) -> Result<O, LexerError>,
) -> Result<O, LexerError> {
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e.into())))
    .and_then(|_| {
      f(lexer).inspect(|_| {
        increase_token(lexer);
      })
    })
}

#[allow(clippy::result_large_err)]
#[inline(always)]
fn tt_hook_and_then_into_errors<'a, O>(
  lexer: &mut Lexer<'a, CstToken<'a>>,
  f: impl FnOnce(&mut Lexer<'a, CstToken<'a>>) -> Result<O, LexerErrors>,
) -> Result<O, LexerErrors> {
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e.into())).into())
    .and_then(|_| {
      f(lexer).inspect(|_| {
        increase_token(lexer);
      })
    })
}

#[inline(always)]
fn tt_hook_map<'a, O>(
  lexer: &mut Lexer<'a, CstToken<'a>>,
  f: impl FnOnce(&mut Lexer<'a, CstToken<'a>>) -> O,
) -> Result<O, LexerError> {
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e.into())))
    .map(|_| {
      increase_token(lexer);
      f(lexer)
    })
}

#[inline(always)]
fn tt_hook<'a>(lexer: &mut Lexer<'a, CstToken<'a>>) -> Result<(), LexerError> {
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e.into())))
    .inspect(|_| {
      increase_token(lexer);
    })
}

/// The token kind for lossless lexing.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum TokenKind {
  Ampersand,
  At,
  RBrace,
  LBrace,
  RBracket,
  LBracket,
  RParen,
  LParen,
  Bang,
  Colon,
  Dollar,
  Equal,
  Float,
  Boolean,
  Identifier,
  Int,
  Whitespaces,
  Pipe,
  Spread,
  String,
  Comment,
}

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(
  Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[logos(
  crate = logosky::logos,
  extras = Tracker,
  error(LexerErrors, |lexer| match lexer.slice().chars().next() {
    Some(ch) => {
      lexer.extras.increase_token();
      LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start)
    },
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }.into())
)]
pub enum CstToken<'a> {
  /// Ampersand `&` token
  #[token("&", tt_hook)]
  Ampersand,

  /// At `@` token
  #[token("@", tt_hook)]
  At,

  /// Comma `,` token
  #[token("}", decrease_recursion_depth)]
  RBrace,

  /// Bracket `]` token
  #[token("]", decrease_recursion_depth)]
  RBracket,

  /// Parenthesis `)` token
  #[token(")", decrease_recursion_depth)]
  RParen,

  /// Dot `.` token
  #[token(":", tt_hook)]
  Colon,

  /// Dollar `$` token
  #[token("$", tt_hook)]
  Dollar,

  /// Equal `=` token
  #[token("=", tt_hook)]
  Equal,

  /// Exclamation mark `!` token
  #[token("!", tt_hook)]
  Bang,

  /// Left curly brace `{` token
  #[token("{", increase_recursion_depth_and_token)]
  LBrace,

  /// Left square bracket `[` token
  #[token("[", increase_recursion_depth_and_token)]
  LBracket,

  /// Left parenthesis `(` token
  #[token("(", increase_recursion_depth_and_token)]
  LParen,

  /// Pipe `|` token
  #[token("|", tt_hook)]
  Pipe,

  /// Spread operator `...` token
  #[token("...", tt_hook)]
  #[token("..", |lexer| tt_hook_and_then(lexer, unterminated_spread_operator))]
  #[token(".", |lexer| tt_hook_and_then(lexer, unterminated_spread_operator))]
  Spread,

  /// Comment token, including the leading `#`
  #[regex("#[^\n\r]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
  Comment(&'a str),

  /// Whitespace token
  #[regex("[ \t,\r\n\u{FEFF}]+")]
  Whitespaces(&'a str),

  /// Float literal token
  #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| tt_hook_and_then_into_errors(lexer, |lexer| handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix)))]
  #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| tt_hook_and_then(lexer, |lexer| handle_number_suffix(lexer, FloatError::UnexpectedSuffix)))]
  #[regex(
    "-?\\.[0-9]+([eE][+-]?[0-9]+)?",
    |lexer| tt_hook_and_then_into_errors(lexer, handle_float_missing_integer_part_error_and_suffix)
  )]
  #[regex("-?0[0-9]+\\.[0-9]+[eE][+-]?", |lexer| tt_hook_and_then_into_errors(lexer, handle_leading_zeros_and_exponent_error))]
  #[regex("-?(0|[1-9][0-9]*)\\.[0-9]+[eE][+-]?", |lexer| tt_hook_and_then(lexer, handle_exponent_error))]
  #[regex("-?0[0-9]+\\.", |lexer| tt_hook_and_then_into_errors(lexer, handle_leading_zeros_and_fractional_error))]
  #[regex("-?(0|[1-9][0-9]*)\\.", |lexer| tt_hook_and_then(lexer, handle_fractional_error))]
  #[regex("-?0[0-9]+[eE][+-]?", |lexer| tt_hook_and_then_into_errors(lexer, handle_leading_zeros_and_exponent_error))]
  #[regex("-?(0|[1-9][0-9]*)[eE][+-]?", |lexer| tt_hook_and_then(lexer, handle_exponent_error))]
  Float(&'a str),

  /// Identifier token
  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice())  })]
  Identifier(&'a str),

  /// Integer literal token
  #[regex("-?(0|[1-9][0-9]*)", |lexer| tt_hook_and_then(lexer, |lexer| handle_number_suffix(lexer, IntError::UnexpectedSuffix)))]
  #[regex("-?0[0-9]+", |lexer| {
    tt_hook_and_then_into_errors(lexer, |lexer| handle_leading_zero_and_number_suffix_error(lexer, IntError::LeadingZeros, IntError::UnexpectedSuffix))
  })]
  #[token("-", |lexer| {
    tt_hook_and_then(lexer, |lexer| Err(LexerError::unexpected_char(lexer.span().into(), '-', lexer.span().start)))
  })]
  #[token("+", |lexer| {
    tt_hook_and_then(lexer, |lexer| Err(LexerError::unexpected_char(lexer.span().into(), '+', lexer.span().start)))
  })]
  Int(&'a str),
  #[token("\"", |lexer| { tt_hook_and_then(lexer, lex_inline_string) })]
  StringLiteral(&'a str),
  #[token("\"\"\"", |lexer| { tt_hook_and_then(lexer, lex_block_string) })]
  BlockStringLiteral(&'a str),
}

impl<'a> logosky::Token<'a> for CstToken<'a> {
  type Kind = TokenKind;
  type Char = char;

  #[inline(always)]
  fn kind(&self) -> Self::Kind {
    match self {
      Self::Ampersand => TokenKind::Ampersand,
      Self::At => TokenKind::At,
      Self::RBrace => TokenKind::RBrace,
      Self::LBrace => TokenKind::LBrace,
      Self::RBracket => TokenKind::RBracket,
      Self::LBracket => TokenKind::LBracket,
      Self::RParen => TokenKind::RParen,
      Self::LParen => TokenKind::LParen,
      Self::Bang => TokenKind::Bang,
      Self::Colon => TokenKind::Colon,
      Self::Dollar => TokenKind::Dollar,
      Self::Equal => TokenKind::Equal,
      Self::Float(_) => TokenKind::Float,
      Self::Identifier(_) => TokenKind::Identifier,
      Self::Int(_) => TokenKind::Int,
      Self::Pipe => TokenKind::Pipe,
      Self::Spread => TokenKind::Spread,
      Self::StringLiteral(_) => TokenKind::String,
      Self::BlockStringLiteral(_) => TokenKind::String,
      Self::Comment(_) => TokenKind::Comment,
      Self::Whitespaces(_) => TokenKind::Whitespaces,
    }
  }
}

impl<'a> CstToken<'a> {
  /// Returns `true` if the token belongs to an ignored category.
  #[inline(always)]
  pub const fn is_ignored(&self) -> bool {
    self.is_whitespaces() || self.is_comment()
  }
}
