use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokit::state::recursion_tracker::RecursionLimitExceeded;

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

// TODO: Lexer type alias needs migration - LogosLexer requires FromLogos trait
// pub type Lexer<'a, S = &'a str> = tokit::lexer::LogosLexer<'a, SyntacticToken<S>>;

/// The error data type for lexing based on syntactic token with `char` source.
pub type SyntacticLexerErrorData =
  error::LexerErrorData<char, RecursionLimitExceeded>;
/// The error type for lexing based on syntactic token with `char` source.
pub type SyntacticLexerError =
  error::LexerError<char, RecursionLimitExceeded>;
/// A collection of errors for syntactic token with `char` source.
pub type SyntacticLexerErrors =
  error::LexerErrors<char, RecursionLimitExceeded>;

/// A syntactic token for GraphQL lexing that only includes syntactically significant tokens.
///
/// This token type is optimized for high-performance parsing by **excluding trivia** (whitespace,
/// comments, and commas). It provides minimal memory footprint and fast lexing, making it ideal
/// for GraphQL servers, query execution, and other performance-critical applications.
///
/// # Ignored Tokens (Trivia)
///
/// The following tokens are automatically skipped during lexing and will NOT appear in the token stream:
/// - **Whitespace**: spaces, tabs, newlines, carriage returns
/// - **Comments**: `# ...` (from `#` to end of line)
/// - **Commas**: `,`
/// - **Byte Order Mark (BOM)**: `\u{FEFF}`
///
/// These trivia tokens are defined by the lexer's skip pattern and are discarded during tokenization.
///
/// # Use Cases
///
/// - **GraphQL servers**: Fast query parsing without formatting overhead
/// - **Query execution**: Minimal token stream for performance-critical paths
/// - **Schema compilation**: Efficient type system parsing
/// - **Production systems**: Where formatting preservation is not required
///
/// # Comparison with [`LosslessToken`](super::lossless::LosslessToken)
///
/// | Feature | `SyntacticToken` | [`LosslessToken`](super::lossless::LosslessToken) |
/// |---------|------------------|----------------------------------------------|
/// | Whitespace | ❌ Skipped | ✅ Preserved |
/// | Comments | ❌ Skipped | ✅ Preserved |
/// | Commas | ❌ Skipped | ✅ Preserved |
/// | Performance | ⚡ Fast | 🐢 Slower |
/// | Use case | Servers, execution | Formatters, linters, IDEs |
///
/// # Example
///
/// ```rust,ignore
/// use smear::lexer::graphql::syntactic::SyntacticToken;
/// use tokit::lexer::LogosLexer;
///
/// let source = "query { user { id } }";
/// let tokens = TokenStream::<SyntacticToken<&str>>::new(source);
///
/// // Only syntactically significant tokens appear in the stream:
/// // Identifier("query"), LBrace, Identifier("user"), LBrace, Identifier("id"), RBrace, RBrace
/// // (whitespace is automatically skipped)
/// ```
///
/// # Generic Over Source Type
///
/// `SyntacticToken<S>` is generic over the source type `S`, allowing zero-copy parsing:
/// - `SyntacticToken<&str>` - For borrowed string sources
/// - `SyntacticToken<&[u8]>` - For byte slice sources
/// - `SyntacticToken<bytes::Bytes>` - For shared ownership with cheap cloning
#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum SyntacticToken<S> {
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

impl<S> SyntacticToken<S> {
  /// Returns the kind of the token.
  #[inline]
  pub const fn kind(&self) -> SyntacticTokenKind {
    match self {
      Self::Identifier(_) => SyntacticTokenKind::Identifier,
      Self::LitInt(_) => SyntacticTokenKind::Int,
      Self::LitFloat(_) => SyntacticTokenKind::Float,
      Self::LitInlineStr(_) => SyntacticTokenKind::InlineString,
      Self::LitBlockStr(_) => SyntacticTokenKind::BlockString,
      Self::Dollar => SyntacticTokenKind::Dollar,
      Self::LParen => SyntacticTokenKind::LParen,
      Self::RParen => SyntacticTokenKind::RParen,
      Self::Spread => SyntacticTokenKind::Spread,
      Self::Colon => SyntacticTokenKind::Colon,
      Self::Equal => SyntacticTokenKind::Equal,
      Self::At => SyntacticTokenKind::At,
      Self::LBracket => SyntacticTokenKind::LBracket,
      Self::RBracket => SyntacticTokenKind::RBracket,
      Self::LBrace => SyntacticTokenKind::LBrace,
      Self::RBrace => SyntacticTokenKind::RBrace,
      Self::Pipe => SyntacticTokenKind::Pipe,
      Self::Bang => SyntacticTokenKind::Bang,
      Self::Ampersand => SyntacticTokenKind::Ampersand,
    }
  }
}

impl<S> From<SyntacticToken<S>> for SyntacticTokenKind {
  #[inline]
  fn from(token: SyntacticToken<S>) -> Self {
    SyntacticTokenKind::from(&token)
  }
}

impl<S> From<&SyntacticToken<S>> for SyntacticTokenKind {
  #[inline]
  fn from(token: &SyntacticToken<S>) -> Self {
    token.kind()
  }
}

/// The kind of a [`SyntacticToken`], without the associated source data.
///
/// This enum represents the type of a token without carrying the actual source slice,
/// making it useful for pattern matching and token classification without dealing with
/// the generic source type parameter.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
#[non_exhaustive]
pub enum SyntacticTokenKind {
  /// Identifier token
  Identifier,
  /// Int literal token
  Int,
  /// Float literal token
  Float,
  /// Inline string token
  InlineString,
  /// Block string token
  BlockString,
  /// Dollar `$` token
  Dollar,
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
  /// At `@` token
  At,
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
}

impl core::fmt::Display for SyntacticTokenKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Identifier => f.write_str("identifier"),
      Self::Int => f.write_str("int"),
      Self::Float => f.write_str("float"),
      Self::InlineString => f.write_str("string"),
      Self::BlockString => f.write_str("block string"),
      Self::Dollar => f.write_str("$"),
      Self::LParen => f.write_str("("),
      Self::RParen => f.write_str(")"),
      Self::Spread => f.write_str("..."),
      Self::Colon => f.write_str(":"),
      Self::Equal => f.write_str("="),
      Self::At => f.write_str("@"),
      Self::LBracket => f.write_str("["),
      Self::RBracket => f.write_str("]"),
      Self::LBrace => f.write_str("{"),
      Self::RBrace => f.write_str("}"),
      Self::Pipe => f.write_str("|"),
      Self::Bang => f.write_str("!"),
      Self::Ampersand => f.write_str("&"),
    }
  }
}
