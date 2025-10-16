use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{Token, utils::tracker::LimitExceeded};

use super::{
  super::{LitBlockStr, LitInlineStr},
  error,
};

use token::token;

mod slice;
mod str;
mod token;

#[cfg(test)]
mod tests;

/// The char type used for the lossless token.
pub type LosslessTokenChar<'a, S> = <LosslessToken<S> as Token<'a>>::Char;
/// The error data type for lexing based on lossless [`Token`].
pub type LosslessLexerErrorData<'a, S> =
  error::LexerErrorData<<LosslessToken<S> as Token<'a>>::Char, LimitExceeded>;
/// The error type for lexing based on lossless [`Token`].
pub type LosslessLexerError<'a, S> =
  error::LexerError<<LosslessToken<S> as Token<'a>>::Char, LimitExceeded>;
/// A collection of errors of lossless [`Token`].
pub type LosslessLexerErrors<'a, S> =
  error::LexerErrors<<LosslessToken<S> as Token<'a>>::Char, LimitExceeded>;

/// A lossless token for GraphQL lexing, preserving all characters including ignored tokens.
#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LosslessToken<S> {
  /// Ampersand `&` token
  Ampersand,
  /// At `@` token
  At,
  /// BOM `\u{FEFF}` token
  Bom(S),
  /// Right curly brace `}` token
  RBrace,
  /// Right square bracket `]` token
  RBracket,
  /// Right parenthesis `)` token
  RParen,
  /// Space ` ` token
  Space,
  /// Tab `\t` token
  Tab,
  /// Carriage return `\r` token
  CarriageReturn,
  /// Newline `\n` token
  Newline,
  /// Carriage return + Newline `\r\n` token
  CarriageReturnAndNewline,
  /// Comment token, including the leading `#`
  Comment(S),
  /// Comma `,` token
  Comma,
  /// Colon `:` token
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

impl<S> LosslessToken<S> {
  /// Returns the kind of the token.
  #[inline]
  pub const fn kind(&self) -> LosslessTokenKind {
    match self {
      Self::Identifier(_) => LosslessTokenKind::Identifier,
      Self::LitInt(_) => LosslessTokenKind::Int,
      Self::LitFloat(_) => LosslessTokenKind::Float,
      Self::LitInlineStr(_) => LosslessTokenKind::InlineString,
      Self::LitBlockStr(_) => LosslessTokenKind::BlockString,
      Self::Dollar => LosslessTokenKind::Dollar,
      Self::LParen => LosslessTokenKind::LParen,
      Self::RParen => LosslessTokenKind::RParen,
      Self::Spread => LosslessTokenKind::Spread,
      Self::Colon => LosslessTokenKind::Colon,
      Self::Equal => LosslessTokenKind::Equal,
      Self::At => LosslessTokenKind::At,
      Self::LBracket => LosslessTokenKind::LBracket,
      Self::RBracket => LosslessTokenKind::RBracket,
      Self::LBrace => LosslessTokenKind::LBrace,
      Self::RBrace => LosslessTokenKind::RBrace,
      Self::Pipe => LosslessTokenKind::Pipe,
      Self::Bang => LosslessTokenKind::Bang,
      Self::Ampersand => LosslessTokenKind::Ampersand,
      Self::Comma => LosslessTokenKind::Comma,
      Self::Space => LosslessTokenKind::Space,
      Self::Tab => LosslessTokenKind::Tab,
      Self::CarriageReturn => LosslessTokenKind::CarriageReturn,
      Self::Newline => LosslessTokenKind::Newline,
      Self::CarriageReturnAndNewline => LosslessTokenKind::CarriageReturnAndNewline,
      Self::Bom(_) => LosslessTokenKind::Bom,
      Self::Comment(_) => LosslessTokenKind::Comment,
    }
  }
}

impl<S> From<LosslessToken<S>> for LosslessTokenKind {
  #[inline]
  fn from(token: LosslessToken<S>) -> Self {
    LosslessTokenKind::from(&token)
  }
}

impl<S> From<&LosslessToken<S>> for LosslessTokenKind {
  #[inline]
  fn from(token: &LosslessToken<S>) -> Self {
    token.kind()
  }
}

/// The token kind for lossless lexing.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum LosslessTokenKind {
  /// Ampersand `&` token
  Ampersand,
  /// At `@` token
  At,
  /// BOM `\u{FEFF}` token
  Bom,
  /// Right curly brace `}` token
  RBrace,
  /// Right square bracket `]` token
  LBrace,

  RBracket,
  /// Left square bracket `[` token
  LBracket,
  /// Right parenthesis `)` token
  RParen,
  /// Left parenthesis `(` token
  LParen,
  /// Bang `!` token
  Bang,
  /// Colon `:` token
  Colon,
  /// Dollar `$` token
  Dollar,
  /// Equal `=` token
  Equal,
  /// Space ` ` token
  Space,
  /// Tab `\t` token
  Tab,
  /// Carriage return token
  CarriageReturn,
  /// Newline token
  Newline,
  /// `\r\n` token
  CarriageReturnAndNewline,
  /// Comma `,` token
  Comma,
  /// Pipe `|` token
  Pipe,
  /// Spread operator `...` token
  Spread,

  /// Float literal token
  Float,
  /// Boolean literal token
  Boolean,
  /// Identifier token
  Identifier,
  /// Integer literal token
  Int,
  /// Inline string token
  InlineString,
  /// Block string token
  BlockString,
  /// Comment token
  Comment,
}
