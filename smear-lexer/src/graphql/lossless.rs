use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokit::state::tracker::LimitExceeded;

use super::{
  super::{LitBlockStr, LitInlineStr},
  error,
};

use token::{token, token_impl};

mod slice;
mod str;
mod token;

/// The lexer type for lossless GraphQL tokenization.
pub type LosslessLexer<'a, S = &'a str> = tokit::lexer::LogosLexer<'a, LosslessToken<S>>;

/// The error data type for lexing based on lossless token with `char` source.
pub type LosslessLexerErrorData = error::LexerErrorData<char, LimitExceeded>;
/// The error type for lexing based on lossless token with `char` source.
pub type LosslessLexerError = error::LexerError<char, LimitExceeded>;
/// A collection of errors for lossless token with `char` source.
pub type LosslessLexerErrors = error::LexerErrors<char, LimitExceeded>;

/// A lossless token for GraphQL lexing that preserves all source information including trivia.
///
/// This token type provides **complete fidelity** to the original source code by preserving
/// all characters including whitespace, comments, and formatting. This makes it essential for
/// developer tools that need to maintain or manipulate source code without losing information.
///
/// # Preserved Trivia Tokens
///
/// Unlike [`SyntacticToken`](super::syntactic::SyntacticToken), `LosslessToken` includes variants for all trivia:
/// - **Whitespace**: [`Space`](LosslessToken::Space), [`Tab`](LosslessToken::Tab), [`Newline`](LosslessToken::Newline), [`CarriageReturn`](LosslessToken::CarriageReturn), [`CarriageReturnAndNewline`](LosslessToken::CarriageReturnAndNewline)
/// - **Comments**: [`Comment`](LosslessToken::Comment) - preserves `# ...` comments with their content
/// - **Commas**: [`Comma`](LosslessToken::Comma)
/// - **Byte Order Mark**: [`Bom`](LosslessToken::Bom) - preserves `\u{FEFF}` characters
///
/// # Use Cases
///
/// - **Code formatters**: Preserve and reformat source code while maintaining structure
/// - **Linters**: Analyze code style including whitespace and comment conventions
/// - **IDEs**: Provide accurate "go to definition", refactoring, and code navigation
/// - **Documentation tools**: Extract and preserve comments for documentation generation
/// - **Source-to-source transformations**: Modify code while preserving unrelated formatting
/// - **Syntax highlighters**: Distinguish between code and comments for visual presentation
///
/// # Comparison with [`SyntacticToken`](super::syntactic::SyntacticToken)
///
/// | Feature | [`SyntacticToken`](super::syntactic::SyntacticToken) | `LosslessToken` |
/// |---------|------------------|-----------------|
/// | Whitespace | ❌ Skipped | ✅ Preserved |
/// | Comments | ❌ Skipped | ✅ Preserved |
/// | Commas | ❌ Skipped | ✅ Preserved |
/// | Performance | ⚡ Fast | 🐢 Slower |
/// | Use case | Servers, execution | Formatters, linters, IDEs |
///
/// # Example
///
/// ```rust,ignore
/// use smear::lexer::graphql::lossless::LosslessToken;
/// use tokit::lexer::LogosLexer;
///
/// let source = "query { # comment\n  user { id }\n}";
/// let tokens = TokenStream::<LosslessToken<&str>>::new(source);
///
/// // ALL tokens appear in the stream, including:
/// // Identifier("query"), Space, LBrace, Space, Comment("# comment"),
/// // Newline, Space, Space, Identifier("user"), Space, LBrace, ...
/// ```
///
/// # Generic Over Source Type
///
/// `LosslessToken<S>` is generic over the source type `S`, allowing zero-copy parsing:
/// - `LosslessToken<&str>` - For borrowed string sources
/// - `LosslessToken<&[u8]>` - For byte slice sources
/// - `LosslessToken<bytes::Bytes>` - For shared ownership with cheap cloning
///
/// # Building Concrete Syntax Trees (CST)
///
/// When you need to preserve all source information in your parse tree, use `LosslessToken`
/// as the token type for your parser. This enables building a **Concrete Syntax Tree** (CST)
/// that maintains complete fidelity to the original source.
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
  /// Returns `true` if this token is trivia (whitespace, comments, commas, BOM).
  #[inline]
  pub const fn is_trivia(&self) -> bool {
    matches!(
      self,
      Self::Space
        | Self::Tab
        | Self::Newline
        | Self::CarriageReturn
        | Self::CarriageReturnAndNewline
        | Self::Comment(_)
        | Self::Comma
        | Self::Bom(_)
    )
  }

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

/// The kind of a [`LosslessToken`], without the associated source data.
///
/// This enum represents the type of a token without carrying the actual source slice,
/// making it useful for pattern matching and token classification without dealing with
/// the generic source type parameter.
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
  /// Right square bracket `]` token
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

impl core::fmt::Display for LosslessTokenKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Ampersand => f.write_str("&"),
      Self::At => f.write_str("@"),
      Self::Bom => f.write_str("BOM"),
      Self::RBrace => f.write_str("}"),
      Self::LBrace => f.write_str("{"),
      Self::RBracket => f.write_str("]"),
      Self::LBracket => f.write_str("["),
      Self::RParen => f.write_str(")"),
      Self::LParen => f.write_str("("),
      Self::Bang => f.write_str("!"),
      Self::Colon => f.write_str(":"),
      Self::Dollar => f.write_str("$"),
      Self::Equal => f.write_str("="),
      Self::Space => f.write_str("space"),
      Self::Tab => f.write_str("tab"),
      Self::CarriageReturn => f.write_str("\\r"),
      Self::Newline => f.write_str("\\n"),
      Self::CarriageReturnAndNewline => f.write_str("\\r\\n"),
      Self::Comma => f.write_str(","),
      Self::Pipe => f.write_str("|"),
      Self::Spread => f.write_str("..."),
      Self::Float => f.write_str("float"),
      Self::Boolean => f.write_str("boolean"),
      Self::Identifier => f.write_str("identifier"),
      Self::Int => f.write_str("int"),
      Self::InlineString => f.write_str("string"),
      Self::BlockString => f.write_str("block string"),
      Self::Comment => f.write_str("comment"),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use tokit::token::IdentifierToken;

  #[test]
  fn identifier_capability_classifies_tokens() {
    // UFCS on `IdentifierToken`: the `IsVariant` derive also generates an inherent
    // `is_identifier`, so method-call syntax would resolve to that instead of the trait.
    assert!(IdentifierToken::is_identifier(
      &LosslessToken::<&str>::Identifier("x")
    ));
    assert!(!IdentifierToken::is_identifier(
      &LosslessToken::<&str>::LitInt("1")
    ));
  }
}
