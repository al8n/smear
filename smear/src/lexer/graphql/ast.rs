use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{Token, utils::recursion_tracker::RecursionLimitExceeded};

use super::{
  super::{LitBlockStr, LitInlineStr},
  error,
};

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

#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
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
  /// Float literal token
  LitFloat(S),
  /// Int literal token
  LitInt(S),
  /// Inline string token
  LitInlineStr(LitInlineStr<S>),
  /// Block string token
  LitBlockStr(LitBlockStr<S>),
}

impl<S> AstToken<S> {
  /// Returns the kind of the token.
  #[inline]
  pub const fn kind(&self) -> AstTokenKind {
    match self {
      Self::Identifier(_) => AstTokenKind::Identifier,
      Self::LitInt(_) => AstTokenKind::Int,
      Self::LitFloat(_) => AstTokenKind::Float,
      Self::LitInlineStr(_) => AstTokenKind::InlineString,
      Self::LitBlockStr(_) => AstTokenKind::BlockString,
      Self::Dollar => AstTokenKind::Dollar,
      Self::LParen => AstTokenKind::LParen,
      Self::RParen => AstTokenKind::RParen,
      Self::Spread => AstTokenKind::Spread,
      Self::Colon => AstTokenKind::Colon,
      Self::Equal => AstTokenKind::Equal,
      Self::At => AstTokenKind::At,
      Self::LBracket => AstTokenKind::LBracket,
      Self::RBracket => AstTokenKind::RBracket,
      Self::LBrace => AstTokenKind::LBrace,
      Self::RBrace => AstTokenKind::RBrace,
      Self::Pipe => AstTokenKind::Pipe,
      Self::Bang => AstTokenKind::Bang,
      Self::Ampersand => AstTokenKind::Ampersand,
    }
  }
}

impl<S> From<AstToken<S>> for AstTokenKind {
  #[inline]
  fn from(token: AstToken<S>) -> Self {
    AstTokenKind::from(&token)
  }
}

impl<S> From<&AstToken<S>> for AstTokenKind {
  #[inline]
  fn from(token: &AstToken<S>) -> Self {
    token.kind()
  }
}

/// The token kind for
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum AstTokenKind {
  Identifier,
  Int,
  Boolean,
  Float,
  InlineString,
  BlockString,
  String,
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
