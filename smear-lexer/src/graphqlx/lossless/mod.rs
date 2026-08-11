use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::state::tracker::LimitExceeded;

use super::{
  super::{LitBlockStr, LitInlineStr},
  LitFloat, LitInt, error,
};

use token::{token, token_impl};

mod token;

mod slice;
mod str;

/// The lexer type for lossless GraphQLx tokenization.
pub type LosslessLexer<'a, S = &'a str> = tokora::lexer::LogosLexer<'a, LosslessToken<S>>;

/// The error data type for lexing based on lossless token with `char` source.
pub type LosslessLexerErrorData = error::LexerErrorData<char, LimitExceeded>;
/// The error type for lexing based on lossless token with `char` source.
pub type LosslessLexerError = error::LexerError<char, LimitExceeded>;
/// A collection of errors for lossless token with `char` source.
pub type LosslessLexerErrors = error::LexerErrors<char, LimitExceeded>;

/// A lossless token for GraphQLx lexing that preserves all source information including trivia.
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
/// use smear::lexer::graphqlx::lossless::LosslessToken;
/// use tokora::lexer::LogosLexer;
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
#[non_exhaustive]
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

/// The kind of a [`LosslessToken`], without the associated source data.
///
/// This enum represents the type of a token without carrying the actual source slice,
/// making it useful for pattern matching and token classification without dealing with
/// the generic source type parameter.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u16)]
#[non_exhaustive]
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

impl core::fmt::Display for LosslessTokenKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(self, f)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::LitPlainStr;

  /// Census: every declared [`LosslessTokenKind`] variant must be producible by some
  /// [`LosslessToken`], i.e. by some arm of [`LosslessToken::kind`].
  ///
  /// See the graphql (non-x) twin of this test
  /// (`smear/src/lexer/graphql/lossless/mod.rs`) for why: that dialect shipped a
  /// `LosslessTokenKind::Boolean` with no `LosslessToken` able to produce it. GraphQLx's kind
  /// enum has no such gap today, but nothing previously stopped one from being added the same
  /// way, silently.
  ///
  /// `census!` builds an exhaustive `match` — no wildcard arm — mapping each
  /// `LosslessTokenKind` to a `LosslessToken` that produces it, from the same variant list it
  /// uses to drive the round-trip assertions below. There is one list, not two kept in sync by
  /// hand: add a `LosslessTokenKind` variant without adding a case here and the generated
  /// `match` stops being exhaustive, which is a compile error naming the missing variant
  /// rather than a runtime assertion that might simply never run.
  #[test]
  fn lossless_token_kind_census() {
    macro_rules! census {
      ($($variant:ident => $token:expr),+ $(,)?) => {
        fn sample(kind: LosslessTokenKind) -> LosslessToken<&'static str> {
          match kind {
            $(LosslessTokenKind::$variant => $token,)+
          }
        }

        $(
          assert_eq!(
            sample(LosslessTokenKind::$variant).kind(),
            LosslessTokenKind::$variant,
            "LosslessTokenKind::{} is declared but its sample token maps to a different kind",
            stringify!($variant),
          );
        )+
      };
    }

    census! {
      Asterisk => LosslessToken::Asterisk,
      At => LosslessToken::At,
      Bom => LosslessToken::Bom("\u{FEFF}"),
      Dollar => LosslessToken::Dollar,
      FatArrow => LosslessToken::FatArrow,
      LAngle => LosslessToken::LAngle,
      RAngle => LosslessToken::RAngle,
      LParen => LosslessToken::LParen,
      RParen => LosslessToken::RParen,
      Spread => LosslessToken::Spread,
      Colon => LosslessToken::Colon,
      Equal => LosslessToken::Equal,
      LBracket => LosslessToken::LBracket,
      RBracket => LosslessToken::RBracket,
      LBrace => LosslessToken::LBrace,
      RBrace => LosslessToken::RBrace,
      Pipe => LosslessToken::Pipe,
      Bang => LosslessToken::Bang,
      Ampersand => LosslessToken::Ampersand,
      Plus => LosslessToken::Plus,
      Minus => LosslessToken::Minus,
      PathSeparator => LosslessToken::PathSeparator,
      Comma => LosslessToken::Comma,
      Space => LosslessToken::Space,
      Tab => LosslessToken::Tab,
      Newline => LosslessToken::Newline,
      CarriageReturn => LosslessToken::CarriageReturn,
      CarriageReturnAndNewline => LosslessToken::CarriageReturnAndNewline,
      Comment => LosslessToken::Comment("# c"),
      Identifier => LosslessToken::Identifier("x"),
      Float => LosslessToken::LitFloat(LitFloat::Decimal("1.0")),
      Int => LosslessToken::LitInt(LitInt::Decimal("1")),
      InlineString => LosslessToken::LitInlineStr(LitInlineStr::Plain(LitPlainStr::new("\"s\""))),
      BlockString => LosslessToken::LitBlockStr(LitBlockStr::Plain(LitPlainStr::new("\"\"\"b\"\"\""))),
    }
  }
}
