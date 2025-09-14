use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logos::{Lexer, Logos};
use logosky::utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter};

use super::{handlers::*, string_token::*};

use crate::error::{self, *};

#[cfg(test)]
mod tests;

/// The error data type for lexing based on fast [`Token`].
pub type LexerErrorData = error::LexerErrorData<char, RecursionLimitExceeded>;
/// The error type for lexing based on fast [`Token`].
pub type LexerError = error::LexerError<char, RecursionLimitExceeded>;
/// A collection of errors of fast [`Token`].
pub type LexerErrors = error::LexerErrors<char, RecursionLimitExceeded>;

#[inline(always)]
pub(super) fn increase_recursion_depth<'a>(
  lexer: &mut Lexer<'a, Token<'a>>,
) -> Result<(), LexerError> {
  lexer.extras.increase();

  lexer
    .extras
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e)))
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a>(lexer: &mut Lexer<'a, Token<'a>>) {
  lexer.extras.decrease();
}

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(
  Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[logos(
  extras = RecursionLimiter,
  skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*",
  error(LexerErrors, |lexer| match lexer.slice().chars().next() {
    Some(ch) => LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }.into())
)]
pub enum Token<'a> {
  /// Ampersand `&` token
  #[token("&")]
  Ampersand,

  /// At `@` token
  #[token("@")]
  At,

  /// Comma `,` token
  #[token("}", decrease_recursion_depth)]
  BraceClose,

  /// Bracket `]` token
  #[token("]", decrease_recursion_depth)]
  BracketClose,

  /// Parenthesis `)` token
  #[token(")", decrease_recursion_depth)]
  ParenClose,

  /// Dot `.` token
  #[token(":")]
  Colon,

  /// Dollar `$` token
  #[token("$")]
  Dollar,

  /// Equals `=` token
  #[token("=")]
  Equals,

  /// Exclamation mark `!` token
  #[token("!")]
  Bang,

  /// Left curly brace `{` token
  #[token("{", increase_recursion_depth)]
  BraceOpen,

  /// Left square bracket `[` token
  #[token("[", increase_recursion_depth)]
  BracketOpen,

  /// Left parenthesis `(` token
  #[token("(", increase_recursion_depth)]
  ParenOpen,

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
  Float(&'a str),

  /// Identifier token
  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  /// Integer literal token
  #[regex("-?(0|[1-9][0-9]*)", |lexer| handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
  #[regex("-?0[0-9]+", |lexer| handle_leading_zero_and_number_suffix_error(lexer, IntError::LeadingZeros, IntError::UnexpectedSuffix))]
  #[token("-", |lexer| Err(LexerError::unexpected_char(lexer.span().into(), '-', lexer.span().start)))]
  #[token("+", |lexer| Err(LexerError::unexpected_char(lexer.span().into(), '+', lexer.span().start)))]
  Int(&'a str),
  #[token("\"", lex_inline_string)]
  StringLiteral(&'a str),
  #[token("\"\"\"", lex_block_string)]
  BlockStringLiteral(&'a str),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum TokenKind {
  Identifier,
  Int,
  Float,
  String,
  BlockString,
  Dollar,
  ParenOpen,
  ParenClose,
  Spread,
  Colon,
  Equals,
  At,
  BracketOpen,
  BracketClose,
  BraceOpen,
  BraceClose,
  Pipe,
  Bang,
  Ampersand,
}

impl<'a> logosky::Token<'a> for Token<'a> {
  type Kind = TokenKind;
  type Char = char;

  #[inline(always)]
  fn kind(&self) -> Self::Kind {
    self.kind()
  }
}

impl<'a> Token<'a> {
  /// Returns the kind of the token.
  #[inline]
  pub const fn kind(&self) -> TokenKind {
    match self {
      Self::Identifier(_) => TokenKind::Identifier,
      Self::Int(_) => TokenKind::Int,
      Self::Float(_) => TokenKind::Float,
      Self::StringLiteral(_) => TokenKind::String,
      Self::BlockStringLiteral(_) => TokenKind::BlockString,
      Self::Dollar => TokenKind::Dollar,
      Self::ParenOpen => TokenKind::ParenOpen,
      Self::ParenClose => TokenKind::ParenClose,
      Self::Spread => TokenKind::Spread,
      Self::Colon => TokenKind::Colon,
      Self::Equals => TokenKind::Equals,
      Self::At => TokenKind::At,
      Self::BracketOpen => TokenKind::BracketOpen,
      Self::BracketClose => TokenKind::BracketClose,
      Self::BraceOpen => TokenKind::BraceOpen,
      Self::BraceClose => TokenKind::BraceClose,
      Self::Pipe => TokenKind::Pipe,
      Self::Bang => TokenKind::Bang,
      Self::Ampersand => TokenKind::Ampersand,
    }
  }
}

impl<'a> From<Token<'a>> for TokenKind {
  #[inline]
  fn from(token: Token<'a>) -> Self {
    TokenKind::from(&token)
  }
}

impl<'a> From<&Token<'a>> for TokenKind {
  #[inline]
  fn from(token: &Token<'a>) -> Self {
    token.kind()
  }
}
