use logosky::{
  Logos, Token,
  logos::Lexer,
  utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use super::{LitBlockStr, LitInlineStr};

use crate::error;

use token::token;

mod token;

#[cfg(test)]
mod tests;

mod slice;
mod str;

/// The char type used for the AST token.
pub type AstTokenChar<'a, S> = <AstToken<S> as Token<'a>>::Char;
/// The error data type for lexing based on AST [`Token`].
pub type AstLexerErrorData<'a, S> =
  error::LexerErrorData<<AstToken<S> as Token<'a>>::Char, RecursionLimitExceeded>;
/// The error type for lexing based on AST [`Token`].
pub type AstLexerError<'a, S> =
  error::LexerError<<AstToken<S> as Token<'a>>::Char, RecursionLimitExceeded>;
/// A collection of errors of AST [`Token`].
pub type AstLexerErrors<'a, S> =
  error::LexerErrors<<AstToken<S> as Token<'a>>::Char, RecursionLimitExceeded>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum AstToken<S> {
  /// Ampersand `&` token
  Ampersand,
  /// At `@` token
  At,
  /// Right curly brace `}` token
  RBrace,
  /// Right square bracket `]` token
  RBracket,
  /// Right parenthesis `)` token
  RParen,
  /// Dot `.` token
  Colon,
  /// Dollar `$` token
  Dollar,
  /// Equal `=` token
  Equal,
  /// Exclamation mark `!` token
  Bang,
  /// Left curly brace `{` token
  LBrace,
  /// Left square bracket `[` token
  LBracket,
  /// Left parenthesis `(` token
  LParen,
  /// Pipe `|` token
  Pipe,
  /// Spread operator `...` token
  Spread,
  /// Identifier token
  Identifier(S),
  Float(S),
  Int(S),
  LitInlineStr(LitInlineStr<S>),
  LitBlockStr(LitBlockStr<S>),
}

impl<S> AstToken<S> {
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

impl<S> From<AstToken<S>> for TokenKind {
  #[inline]
  fn from(token: AstToken<S>) -> Self {
    TokenKind::from(&token)
  }
}

impl<S> From<&AstToken<S>> for TokenKind {
  #[inline]
  fn from(token: &AstToken<S>) -> Self {
    token.kind()
  }
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
