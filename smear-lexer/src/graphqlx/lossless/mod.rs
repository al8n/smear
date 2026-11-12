use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::state::LimitExceeded;

use super::{
  super::{LitBlockStr, LitInlineStr},
  LitFloat, LitInt, error,
};

use token::{token, token_impl};

mod token;

mod slice;
mod str;

/// The lexer type for lossless GraphQLx tokenization.
///
/// [`crate::LosslessLexer`] at this dialect's vocabulary. It is a newtype over
/// [`tokora::lexer::LogosLexer`] rather than the alias for it this used to be, because that
/// alias also published `into_inner`, `inner` and `inner_mut` — and the raw `logos::Lexer`
/// they hand back runs neither the post-scan check that enforces
/// [`LosslessLimits::max_tokens`](crate::limits::LosslessLimits::max_tokens) nor the latch
/// that stops the scan after it trips. See [`crate::LosslessLexer`] for the measurement and
/// for why a latch inside the state cannot replace this.
pub type LosslessLexer<'a, S = &'a str> = crate::LosslessLexer<'a, LosslessToken<S>>;

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
  /// Tilde `~` token, the sigil of a path's package root `~::`
  Tilde,
  /// Dot-dot `..` token, the sigil of a path's parent-module root `..::`
  DotDot,
  /// Caret `^` token, the sigil of a path's dependency root `^::`
  Caret,
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
      Self::Tilde => LosslessTokenKind::Tilde,
      Self::DotDot => LosslessTokenKind::DotDot,
      Self::Caret => LosslessTokenKind::Caret,
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
  /// Tilde `~` token
  Tilde,
  /// Dot-dot `..` token
  DotDot,
  /// Caret `^` token
  Caret,
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

/// The kind as a diagnostic names what it found: punctuation in backticks, and every class —
/// trivia included — as a bare noun, because it follows "unexpected" in a message.
impl core::fmt::Display for LosslessTokenKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Asterisk => f.write_str("`*`"),
      Self::At => f.write_str("`@`"),
      Self::Bom => f.write_str("BOM"),
      Self::Dollar => f.write_str("`$`"),
      Self::FatArrow => f.write_str("`=>`"),
      Self::LAngle => f.write_str("`<`"),
      Self::RAngle => f.write_str("`>`"),
      Self::LParen => f.write_str("`(`"),
      Self::RParen => f.write_str("`)`"),
      Self::Spread => f.write_str("`...`"),
      Self::Colon => f.write_str("`:`"),
      Self::Equal => f.write_str("`=`"),
      Self::LBracket => f.write_str("`[`"),
      Self::RBracket => f.write_str("`]`"),
      Self::LBrace => f.write_str("`{`"),
      Self::RBrace => f.write_str("`}`"),
      Self::Pipe => f.write_str("`|`"),
      Self::Bang => f.write_str("`!`"),
      Self::Ampersand => f.write_str("`&`"),
      Self::Plus => f.write_str("`+`"),
      Self::Minus => f.write_str("`-`"),
      Self::PathSeparator => f.write_str("`::`"),
      Self::Tilde => f.write_str("`~`"),
      Self::DotDot => f.write_str("`..`"),
      Self::Caret => f.write_str("`^`"),
      Self::Comma => f.write_str("`,`"),
      Self::Space => f.write_str("space"),
      Self::Tab => f.write_str("tab"),
      Self::Newline => f.write_str("line feed"),
      Self::CarriageReturn => f.write_str("carriage return"),
      Self::CarriageReturnAndNewline => f.write_str("line break"),
      Self::Comment => f.write_str("comment"),
      Self::Identifier => f.write_str("name"),
      Self::Float => f.write_str("float"),
      Self::Int => f.write_str("integer"),
      Self::InlineString => f.write_str("string"),
      Self::BlockString => f.write_str("block string"),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{LitPlainBlockStr, LitPlainInlineStr, limits::LosslessLimits};
  use tokora::Lexer;

  /// Census: every declared [`LosslessTokenKind`] variant must be producible by some
  /// [`LosslessToken`], i.e. by some arm of [`LosslessToken::kind`].
  ///
  /// See the graphql (non-x) twin of this test
  /// (`smear-lexer/src/graphql/lossless/mod.rs`) for why: that dialect shipped a
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
      Tilde => LosslessToken::Tilde,
      DotDot => LosslessToken::DotDot,
      Caret => LosslessToken::Caret,
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
      InlineString => LosslessToken::LitInlineStr(LitInlineStr::Plain(LitPlainInlineStr::new("\"s\""))),
      BlockString => LosslessToken::LitBlockStr(LitBlockStr::Plain(LitPlainBlockStr::new("\"\"\"b\"\"\""))),
    }
  }

  /// Holds the brace count after each item `lexer` produces to `expected`, as it goes.
  fn assert_brace_counts<'a, L>(mut lexer: L, expected: &[usize], what: &str)
  where
    L: Lexer<'a, State = LosslessLimits>,
  {
    let mut items = 0usize;
    while lexer.lex().is_some() {
      assert_eq!(
        Some(lexer.state().brace_depth()),
        expected.get(items).copied(),
        "{what}: the count after item {items}"
      );
      items += 1;
    }
    assert_eq!(items, expected.len(), "{what}: the number of items");
  }

  /// [`assert_brace_counts`] over a `&str`, a `&[u8]` and, with its feature, an owned byte source.
  ///
  /// The token enum is written by two arms of one macro, one for borrowed slices and one for owned
  /// ones, and each arm spells the brace rules itself: a rule changed in one arm only fails here.
  fn assert_brace_counts_over_each_arm(text: &'static str, expected: &[usize]) {
    assert_brace_counts(LosslessLexer::<&str>::new(text), expected, "&str");
    assert_brace_counts(
      LosslessLexer::<&[u8]>::new(text.as_bytes()),
      expected,
      "&[u8]",
    );
    #[cfg(feature = "bytes_1")]
    assert_brace_counts(
      LosslessLexer::<bytes_1::Bytes>::new(text.as_bytes()),
      expected,
      "bytes::Bytes",
    );
  }

  /// The state `lexer` is left in after its last item.
  fn final_state<'a, L>(mut lexer: L) -> LosslessLimits
  where
    L: Lexer<'a, State = LosslessLimits>,
  {
    while lexer.lex().is_some() {}
    *lexer.state()
  }

  #[test]
  fn the_brace_count_follows_the_braces_of_a_balanced_text() {
    assert_brace_counts_over_each_arm(
      "{a{(b)[c]<d>}e}",
      &[1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0],
    );
  }

  #[test]
  fn only_braces_move_the_brace_count() {
    // Openers of the other three pairs, never closed: the bracket tally counts all six and no
    // brace is open.
    assert_brace_counts_over_each_arm("(([[<<", &[0; 6]);
    let opened = final_state(LosslessLexer::<&str>::new("(([[<<"));
    assert_eq!((opened.brace_depth(), opened.depth()), (0, 6));

    // Their closers with nothing open.
    assert_brace_counts_over_each_arm("))]]>>", &[0; 6]);

    // And a closer of another pair behind an open brace: the tally is pair-blind and comes back
    // to zero, while the brace is still open.
    let crossed = final_state(LosslessLexer::<&str>::new("{)"));
    assert_eq!((crossed.brace_depth(), crossed.depth()), (1, 0));
  }

  #[test]
  fn an_unclosed_brace_stays_counted_and_a_stray_one_stops_at_zero() {
    assert_brace_counts_over_each_arm("{{", &[1, 2]);
    assert_brace_counts_over_each_arm("}}{", &[0, 0, 1]);
  }

  #[test]
  fn a_brace_inside_a_string_or_a_comment_is_not_counted() {
    // One real `{`, then a `}` in a string, a `{` in a block string and both in a comment: seven
    // items, and the count is the one real brace's after every one of them.
    assert_brace_counts(
      LosslessLexer::<&str>::new("{ \"}\" \"\"\"{\"\"\" # } {"),
      &[1; 7],
      "&str",
    );
    assert_brace_counts(
      LosslessLexer::<&[u8]>::new(b"{ \"}\" \"\"\"{\"\"\" # } {"),
      &[1; 7],
      "&[u8]",
    );
  }

  #[test]
  fn a_refused_opener_is_counted_as_the_tally_counts_it() {
    let mut lexer =
      LosslessLexer::<&str>::with_state("{{", LosslessLimits::with_max_nesting_depth(1));
    assert!(matches!(lexer.lex(), Some(Ok(_))));
    assert_eq!((lexer.state().brace_depth(), lexer.state().depth()), (1, 1));

    // The second brace is past the ceiling. Both counts step before the check that refuses it.
    assert!(matches!(lexer.lex(), Some(Err(_))));
    assert_eq!((lexer.state().brace_depth(), lexer.state().depth()), (2, 2));
    assert!(lexer.lex().is_none(), "a refusal latches the lexer");
  }

  #[test]
  fn restoring_a_saved_state_restores_the_brace_count() {
    const TEXT: &str = "{a{b}c{{d}";

    let mut whole = LosslessLexer::<&str>::new(TEXT);
    for _ in 0..4 {
      assert!(matches!(whole.lex(), Some(Ok(_))));
    }
    let (saved, at) = (*whole.state(), whole.span().end());
    assert_eq!((saved.brace_depth(), at), (2, 4));
    let through = final_state(whole);

    // What a rewind does: a second lexer over the rest of the text, seeded with the saved state.
    let resumed = final_state(LosslessLexer::<&str>::with_state(&TEXT[at..], saved));
    assert_eq!(resumed.brace_depth(), through.brace_depth());
    assert_eq!(through.brace_depth(), 2, "four openers and two closers");
  }
}
