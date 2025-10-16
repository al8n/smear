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
      Self::LAngle => CstTokenKind::LAngle,
      Self::RAngle => CstTokenKind::RAngle,
      Self::FatArrow => CstTokenKind::FatArrow,
      Self::Plus => CstTokenKind::Plus,
      Self::Minus => CstTokenKind::Minus,
      Self::PathSeparator => CstTokenKind::PathSeparator,
      Self::Asterisk => CstTokenKind::Asterisk,
      Self::Space => CstTokenKind::Space,
      Self::Tab => CstTokenKind::Tab,
      Self::Newline => CstTokenKind::Newline,
      Self::CarriageReturn => CstTokenKind::CarriageReturn,
      Self::CarriageReturnAndNewline => CstTokenKind::CarriageReturnAndNewline,
      Self::Comma => CstTokenKind::Comma,
      Self::Comment(_) => CstTokenKind::Comment,
      Self::Bom(_) => CstTokenKind::Bom,
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

/// The token kind for
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
pub enum CstTokenKind {
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
