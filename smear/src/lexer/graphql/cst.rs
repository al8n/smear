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

/// The char type used for the AST token.
pub type CstTokenChar<'a, S> = <CstToken<S> as Token<'a>>::Char;
/// The error data type for lexing based on AST [`Token`].
pub type CstLexerErrorData<'a, S> =
  error::LexerErrorData<<CstToken<S> as Token<'a>>::Char, LimitExceeded>;
/// The error type for lexing based on AST [`Token`].
pub type CstLexerError<'a, S> = error::LexerError<<CstToken<S> as Token<'a>>::Char, LimitExceeded>;
/// A collection of errors of AST [`Token`].
pub type CstLexerErrors<'a, S> =
  error::LexerErrors<<CstToken<S> as Token<'a>>::Char, LimitExceeded>;

#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum CstToken<S> {
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

impl<S> CstToken<S> {
  /// Returns the kind of the token.
  #[inline]
  pub const fn kind(&self) -> CstTokenKind {
    match self {
      Self::Identifier(_) => CstTokenKind::Identifier,
      Self::LitInt(_) => CstTokenKind::Int,
      Self::LitFloat(_) => CstTokenKind::Float,
      Self::LitInlineStr(_) => CstTokenKind::InlineString,
      Self::LitBlockStr(_) => CstTokenKind::BlockString,
      Self::Dollar => CstTokenKind::Dollar,
      Self::LParen => CstTokenKind::LParen,
      Self::RParen => CstTokenKind::RParen,
      Self::Spread => CstTokenKind::Spread,
      Self::Colon => CstTokenKind::Colon,
      Self::Equal => CstTokenKind::Equal,
      Self::At => CstTokenKind::At,
      Self::LBracket => CstTokenKind::LBracket,
      Self::RBracket => CstTokenKind::RBracket,
      Self::LBrace => CstTokenKind::LBrace,
      Self::RBrace => CstTokenKind::RBrace,
      Self::Pipe => CstTokenKind::Pipe,
      Self::Bang => CstTokenKind::Bang,
      Self::Ampersand => CstTokenKind::Ampersand,
      Self::Comma => CstTokenKind::Comma,
      Self::Space => CstTokenKind::Space,
      Self::Tab => CstTokenKind::Tab,
      Self::CarriageReturn => CstTokenKind::CarriageReturn,
      Self::Newline => CstTokenKind::Newline,
      Self::CarriageReturnAndNewline => CstTokenKind::CarriageReturnAndNewline,
      Self::Bom(_) => CstTokenKind::Bom,
      Self::Comment(_) => CstTokenKind::Comment,
    }
  }
}

impl<S> From<CstToken<S>> for CstTokenKind {
  #[inline]
  fn from(token: CstToken<S>) -> Self {
    CstTokenKind::from(&token)
  }
}

impl<S> From<&CstToken<S>> for CstTokenKind {
  #[inline]
  fn from(token: &CstToken<S>) -> Self {
    token.kind()
  }
}

/// The token kind for lossless lexing.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum CstTokenKind {
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
