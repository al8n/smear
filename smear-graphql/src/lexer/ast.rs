use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexable,
  logos::{Lexer, Logos},
  utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use super::{LitBlockStr, LitInlineStr, handlers::*, string_lexer::SealedLexer};

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
  lexer: &mut Lexer<'a, AstToken<'a>>,
) -> Result<(), LexerError> {
  lexer.extras.increase();

  lexer
    .extras
    .check()
    .map_err(|e| LexerError::new(lexer.span(), LexerErrorData::State(e)))
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a>(lexer: &mut Lexer<'a, AstToken<'a>>) {
  lexer.extras.decrease();
}

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
  extras = RecursionLimiter,
  skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*",
  error(LexerErrors, |lexer| match lexer.slice().chars().next() {
    Some(ch) => LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }.into())
)]
pub enum AstToken<'a> {
  /// Ampersand `&` token
  #[token("&")]
  Ampersand,

  /// At `@` token
  #[token("@")]
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
  #[token("\"", |lexer| LitInlineStr::lex(SealedLexer::<'_, '_, AstToken<'_>>::from(lexer)))]
  LitInlineStr(LitInlineStr<&'a str>),
  #[token("\"\"\"", |lexer| {
    <LitBlockStr<&str> as Lexable<_, LexerError>>::lex(SealedLexer::<'_, '_, AstToken<'_>>::from(lexer))
  })]
  LitBlockStr(LitBlockStr<&'a str>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum TokenKind {
  Identifier,
  Int,
  Boolean,
  Float,
  String,
  LitBlockStr,
  Dollar,
  LParen,
  RParen,
  Spread,
  Colon,
  Equal,
  At,
  LBracket,
  RBracket,
  LBrace,
  RBrace,
  Pipe,
  Bang,
  Ampersand,
}

impl<'a> logosky::Token<'a> for AstToken<'a> {
  type Kind = TokenKind;
  type Char = char;

  #[inline(always)]
  fn kind(&self) -> Self::Kind {
    self.kind()
  }
}

impl<'a> AstToken<'a> {
  /// Returns the kind of the token.
  #[inline]
  pub const fn kind(&self) -> TokenKind {
    match self {
      Self::Identifier(_) => TokenKind::Identifier,
      Self::Int(_) => TokenKind::Int,
      Self::Float(_) => TokenKind::Float,
      Self::LitInlineStr(_) => TokenKind::String,
      Self::LitBlockStr(_) => TokenKind::LitBlockStr,
      Self::Dollar => TokenKind::Dollar,
      Self::LParen => TokenKind::LParen,
      Self::RParen => TokenKind::RParen,
      Self::Spread => TokenKind::Spread,
      Self::Colon => TokenKind::Colon,
      Self::Equal => TokenKind::Equal,
      Self::At => TokenKind::At,
      Self::LBracket => TokenKind::LBracket,
      Self::RBracket => TokenKind::RBracket,
      Self::LBrace => TokenKind::LBrace,
      Self::RBrace => TokenKind::RBrace,
      Self::Pipe => TokenKind::Pipe,
      Self::Bang => TokenKind::Bang,
      Self::Ampersand => TokenKind::Ampersand,
    }
  }
}

impl<'a> From<AstToken<'a>> for TokenKind {
  #[inline]
  fn from(token: AstToken<'a>) -> Self {
    TokenKind::from(&token)
  }
}

impl<'a> From<&AstToken<'a>> for TokenKind {
  #[inline]
  fn from(token: &AstToken<'a>) -> Self {
    token.kind()
  }
}
