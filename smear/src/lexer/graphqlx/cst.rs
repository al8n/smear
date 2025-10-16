use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{Token, utils::tracker::LimitExceeded};

use super::{
  super::{LitBlockStr, LitInlineStr},
  LitFloat, LitInt, error,
};

use token::token;

mod token;

#[cfg(test)]
mod tests;

mod slice;
mod str;

/// The char type used for the AST token.
pub type LosslessTokenChar<'a, S> = <LosslessToken<S> as Token<'a>>::Char;
/// The error data type for lexing based on AST [`Token`].
pub type CstLexerErrorData<'a, S> =
  error::LexerErrorData<<LosslessToken<S> as Token<'a>>::Char, LimitExceeded>;
/// The error type for lexing based on AST [`Token`].
pub type CstLexerError<'a, S> = error::LexerError<<LosslessToken<S> as Token<'a>>::Char, LimitExceeded>;
/// A collection of errors of AST [`Token`].
pub type CstLexerErrors<'a, S> =
  error::LexerErrors<<LosslessToken<S> as Token<'a>>::Char, LimitExceeded>;

#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LosslessToken<S> {
  /// Asterisk `*` token
  Asterisk,
  /// Ampersand `&` token
  Ampersand,
  /// At `@` token
  At,
  /// BOM `\u{FEFF}` token
  Bom(S),
  /// Right angle bracket `>` token
  RAngle,
  /// Right curly brace `}` token
  RBrace,
  /// Right square bracket `]` token
  RBracket,
  /// Right parenthesis `)` token
  RParen,
  /// Colon `:` token
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
  /// Space ` ` token
  Space,
  /// Tab `\t` token
  Tab,
  /// Newline `\n` token
  Newline,
  /// Carriage return `\r` token
  CarriageReturn,
  /// Carriage return + newline `\r\n` token
  CarriageReturnAndNewline,
  /// Comma `,` token
  Comma,
  /// Comment `# ...` token
  Comment(S),
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
      Self::LAngle => LosslessTokenKind::LAngle,
      Self::RAngle => LosslessTokenKind::RAngle,
      Self::FatArrow => LosslessTokenKind::FatArrow,
      Self::Plus => LosslessTokenKind::Plus,
      Self::Minus => LosslessTokenKind::Minus,
      Self::PathSeparator => LosslessTokenKind::PathSeparator,
      Self::Asterisk => LosslessTokenKind::Asterisk,
      Self::Space => LosslessTokenKind::Space,
      Self::Tab => LosslessTokenKind::Tab,
      Self::Newline => LosslessTokenKind::Newline,
      Self::CarriageReturn => LosslessTokenKind::CarriageReturn,
      Self::CarriageReturnAndNewline => LosslessTokenKind::CarriageReturnAndNewline,
      Self::Comma => LosslessTokenKind::Comma,
      Self::Comment(_) => LosslessTokenKind::Comment,
      Self::Bom(_) => LosslessTokenKind::Bom,
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

/// The token kind for
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum LosslessTokenKind {
  /// Asterisk `*` token
  Asterisk,
  /// Ampersand `&` token
  At,
  /// At `@` token
  Bom,
  /// BOM `\u{FEFF}` token
  Dollar,
  /// Fat arrow `=>` token
  FatArrow,
  /// Left angle bracket `<` token
  LAngle,
  /// Right angle bracket `>` token
  RAngle,
  /// Left parenthesis `(` token
  LParen,
  /// Right parenthesis `)` token
  RParen,
  /// Spread operator `...` token
  Spread,
  /// Colon `:` token
  Colon,
  /// Equal `=` token
  Equal,
  /// Left bracket `[` token
  LBracket,
  /// Right bracket `]` token
  RBracket,
  /// Left brace `{` token
  LBrace,
  /// Right brace `}` token
  RBrace,
  /// Pipe `|` token
  Pipe,
  /// Bang `!` token
  Bang,
  /// Ampersand `&` token
  Ampersand,
  /// Plus `+` token
  Plus,
  /// Minus `-` token
  Minus,
  /// Path separator `::` token
  PathSeparator,
  /// Comma `,` token
  Comma,
  /// Space ` ` token
  Space,
  /// Tab `\t` token
  Tab,
  /// Newline `\n` token
  Newline,
  /// Carriage return `\r` token
  CarriageReturn,
  /// Carriage return + newline `\r\n` token
  CarriageReturnAndNewline,
  /// Comment `# ...` token
  Comment,
  /// Identifier token
  Identifier,
  /// Float literal token
  Float,
  /// Int literal token
  Int,
  /// Inline string token
  InlineString,
  /// Block string token
  BlockString,
}
