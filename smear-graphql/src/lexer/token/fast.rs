use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logos::Logos;
use logosky::utils::{
  Lexeme,
  recursion_tracker::RecursionLimiter,
};

use super::super::error::{self, *};

use handlers::*;
use string_token::*;

mod string_token;
mod handlers;

#[cfg(test)]
mod tests;

/// The error type for the GraphQL lexer based on the fast token.
pub type Error = error::Error<char>;
/// The error data for the GraphQL lexer based on the fast token.
pub type ErrorData = error::ErrorData<char>;
/// A container for storing multiple lexer errors.
pub type Errors = error::Errors<char>;

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(
  Logos,
  Copy,
  Clone,
  Debug,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Hash,
  IsVariant,
  Unwrap,
  TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[logos(
  extras = RecursionLimiter,
  skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*",
  error(Errors, |lexer| match lexer.slice().chars().next() {
    Some(ch) => Error::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => Error::unexpected_eoi(lexer.span().into()),
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
  FloatLiteral(&'a str),

  /// Identifier token
  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  /// Integer literal token
  #[regex("-?(0|[1-9][0-9]*)", |lexer| handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
  #[regex("-?0[0-9]+", |lexer| handle_leading_zero_and_number_suffix_error(lexer, IntError::LeadingZeros, IntError::UnexpectedSuffix))]
  #[token("-", |lexer| Err(Error::unexpected_char(lexer.span().into(), '-', lexer.span().start)))]
  #[token("+", |lexer| Err(Error::unexpected_char(lexer.span().into(), '+', lexer.span().start)))]
  IntegerLiteral(&'a str),
  #[token("\"", lex_inline_string)]
  StringLiteral(&'a str),
  #[token("\"\"\"", lex_block_string)]
  BlockStringLiteral(&'a str),
}
