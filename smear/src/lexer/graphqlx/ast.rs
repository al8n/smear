use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Token,
  utils::{human_display::DisplayHuman, recursion_tracker::RecursionLimitExceeded},
};

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
  /// Asterisk `*` token
  Asterisk,
  /// Ampersand `&` token
  Ampersand,
  /// At `@` token
  At,
  /// Right angle bracket `>` token
  RAngle,
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
  /// Left angle bracket `<` token
  LAngle,
  /// Left curly brace `{` token
  LBrace,
  /// Left square bracket `[` token
  LBracket,
  /// Left parenthesis `(` token
  LParen,
  /// Pipe `|` token
  Pipe,
  /// Fat arrow `=>` token
  FatArrow,
  /// Spread operator `...` token
  Spread,
  /// Plus `+` token
  Plus,
  /// Minus `-` token
  Minus,
  /// Path separator `::` token
  PathSeparator,
  /// Identifier token
  Identifier(S),
  /// Float literal token
  LitFloat(LitFloat<S>),
  /// Int literal token
  LitInt(LitInt<S>),
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
      Self::LitInlineStr(_) => AstTokenKind::String,
      Self::LitBlockStr(_) => AstTokenKind::LitBlockStr,
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
      Self::LAngle => AstTokenKind::LAngle,
      Self::RAngle => AstTokenKind::RAngle,
      Self::FatArrow => AstTokenKind::FatArrow,
      Self::Plus => AstTokenKind::Plus,
      Self::Minus => AstTokenKind::Minus,
      Self::PathSeparator => AstTokenKind::PathSeparator,
      Self::Asterisk => AstTokenKind::Asterisk,
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
  String,
  LitBlockStr,
  Dollar,
  FatArrow,
  LAngle,
  RAngle,
  LParen,
  RParen,
  Spread,
  Colon,
  Equal,
  Asterisk,
  At,
  LBracket,
  RBracket,
  LBrace,
  RBrace,
  Pipe,
  Bang,
  Ampersand,
  Plus,
  Minus,
  PathSeparator,
}

/// A GraphQLx integer literal, which can be in decimal, hexadecimal, binary, or octal format.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, TryUnwrap, Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitInt<S> {
  /// A decimal integer literal.
  Decimal(S),
  /// A hexadecimal integer literal.
  Hex(S),
  /// A binary integer literal.
  Binary(S),
  /// An octal integer literal.
  Octal(S),
}

impl<S> AsRef<S> for LitInt<S> {
  #[inline(always)]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitInt<&str> {
  #[inline(always)]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitInt<&[u8]> {
  #[inline(always)]
  fn as_ref(&self) -> &[u8] {
    self.source_ref()
  }
}

impl<S: core::fmt::Display> core::fmt::Display for LitInt<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S: DisplayHuman> DisplayHuman for LitInt<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> LitInt<S> {
  /// Returns the underlying source.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Decimal(s) => *s,
      Self::Hex(s) => *s,
      Self::Binary(s) => *s,
      Self::Octal(s) => *s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
      Self::Binary(s) => s,
      Self::Octal(s) => s,
    }
  }
}

/// A GraphQLx float literal, which can be in decimal or hexadecimal format.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, TryUnwrap, Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitFloat<S> {
  /// A decimal float literal.
  Decimal(S),
  /// A hexadecimal float literal.
  Hex(S),
}

impl<S> AsRef<S> for LitFloat<S> {
  #[inline(always)]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitFloat<&str> {
  #[inline(always)]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitFloat<&[u8]> {
  #[inline(always)]
  fn as_ref(&self) -> &[u8] {
    self.source_ref()
  }
}

impl<S: DisplayHuman> DisplayHuman for LitFloat<S> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S: core::fmt::Display> core::fmt::Display for LitFloat<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> LitFloat<S> {
  /// Returns the underlying source.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Decimal(s) => *s,
      Self::Hex(s) => *s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
    }
  }
}
