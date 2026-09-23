use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  state::recursion_tracker::RecursionLimitExceeded,
  utils::{Lexeme, human_display::DisplayHuman},
};

/// Errors for GraphQLx lexers
pub mod error;

/// GraphQLx contextual keywords recognized from identifier spellings.
pub mod keyword;

/// Syntactic tokens for GraphQLx - fast lexing that skips trivia.
///
/// This module provides [`SyntacticToken`](syntactic::SyntacticToken), which is optimized for
/// high-performance parsing by automatically filtering out whitespace, comments, and commas.
///
/// **Use this for**: GraphQLx servers, query execution, schema compilation, and any
/// performance-critical parsing where you don't need to preserve formatting.
///
/// **Key benefits**:
/// - Minimal memory footprint
/// - Maximum parsing speed
/// - Zero-copy token references
/// - Supports GraphQLx extensions (generics, imports, type paths, etc.)
///
/// See [`SyntacticToken`](syntactic::SyntacticToken) for detailed documentation.
pub mod syntactic;

/// Lossless tokens for GraphQLx - complete source preservation.
///
/// This module provides [`LosslessToken`](lossless::LosslessToken), which preserves all source
/// information including whitespace, comments, and formatting. Essential for developer tools
/// that need to maintain or manipulate source code without losing information.
///
/// **Use this for**: Code formatters, linters, IDEs, syntax highlighters, documentation tools,
/// and any application that needs perfect source reconstruction.
///
/// **Key benefits**:
/// - Complete source fidelity
/// - Access to all comments and formatting
/// - Build Concrete Syntax Trees (CST)
/// - Supports GraphQLx extensions (generics, imports, type paths, etc.)
///
/// See [`LosslessToken`](lossless::LosslessToken) for detailed documentation.
pub mod lossless;

pub use keyword::ContextualKeyword;

mod handlers;

/// The re-cooking doors' own cells — see [`scan_one_token`].
#[cfg(test)]
#[path = "number/tests.rs"]
mod number_tests;

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

/// One whole numeric literal, re-scanned by the lexer that produced it.
///
/// The private half of `LitInt`'s and `LitFloat`'s `TryFrom<&str>`, which carry the contract.
///
/// # Why this exists: a payload with no door is a payload with a second custodian
///
/// A GraphQLx string literal is re-cooked through [`LitStr`](crate::LitStr)'s `TryFrom<&str>`,
/// which **is** the string sub-lexer, so a consumer that has to rebuild one — the CST → AST
/// projection is the consumer — gets the lexer's own answer rather than a second implementation of
/// the escape rules. Numbers had no such door. The tree keeps one `Int` image over four radices and
/// one `Float` image over two, the `logos` number grammars are private, and the projection was left
/// classifying by prefix: total over what the lexer accepts, and silent about everything else.
/// `0b2`, `0x`, `007`, and a `Float`-labelled `0x1` all produced a literal.
///
/// So the door is the scanner. It is not a *second* grammar reached from here — it is
/// [`SyntacticLexer`](syntactic::SyntacticLexer), the shipped one, run over the slice, which is why
/// there is nothing left for the two layers to disagree about. al8n/smear#58.
///
/// # What "whole" means
///
/// The slice must scan to **exactly one** token, that token must be a numeric literal, and its span
/// must be the whole slice. Leading or trailing trivia fails it, a second token fails it, and a
/// token of any other kind fails it — a caller asking for a literal is asking about the bytes it
/// handed over, not about a prefix of them. The lexer's own error passes through unchanged; the
/// failures the lexer has no opinion about are one [`UnexpectedLexeme`](error::LexerErrorData) over
/// the slice.
///
/// # `&str` only
///
/// `LitStr` carries a `&[u8]` door beside its `&str` one because the string sub-lexer is
/// instantiated for both scan alphabets. This one is not, and the reason is the consumer: the
/// projection hands back an AST borrowing the caller's `&'src str`, so the slice it re-cooks is
/// always text. A byte door here would be surface with no caller.
fn scan_one_token<'de: 'a, 'a>(
  value: &'de str,
) -> Result<syntactic::SyntacticToken<&'a str>, error::LexerErrors<char, RecursionLimitExceeded>> {
  use tokora::Lexer as _;

  /// The slice is not one numeric literal, and the scanner had no complaint of its own to make.
  fn not_one_literal(len: usize) -> error::LexerErrors<char, RecursionLimitExceeded> {
    error::LexerError::const_new(
      SimpleSpan::new(0, len),
      error::LexerErrorData::UnexpectedLexeme(Lexeme::from_range_const(SimpleSpan::new(0, len))),
    )
    .into()
  }

  let mut lexer = syntactic::SyntacticLexer::<'de, str>::new(value);
  let Some(first) = lexer.lex() else {
    return Err(
      error::LexerError::const_new(
        SimpleSpan::new(0, value.len()),
        error::LexerErrorData::UnexpectedEndOfInput,
      )
      .into(),
    );
  };
  let token = first?;
  let span = lexer.span();
  if span.start() != 0 || span.end() != value.len() || lexer.lex().is_some() {
    return Err(not_one_literal(value.len()));
  }
  Ok(token)
}

/// The scanner's own reading of `value` as one whole identifier.
///
/// # Whole, and the scanner's
///
/// The slice must scan to **exactly one** token, that token must be an
/// [`Identifier`](syntactic::SyntacticToken::Identifier), and its span must be the whole slice:
/// leading or trailing trivia fails it, a second token fails it, and a token of any other kind
/// fails it. The scan is [`SyntacticLexer`](syntactic::SyntacticLexer) — the shipped one — so the
/// answer is the lexer's own rather than a second spelling of the name grammar.
///
/// It exists for the same consumer [`LitInt`]'s door does: the CST → AST projection reads a name
/// out of the source by the range of a token the *tree* labelled `Name`, and a tree it did not
/// build itself can label anything. Without this, `Name("1")` and `Name("a b")` were values the
/// projection would answer and the parser could never produce. al8n/smear#58.
///
/// A keyword passes: this dialect's keywords are contextual, so `query` and `on` are identifiers
/// to the scanner and it is [`ContextualKeyword`] that tells them apart afterwards.
///
/// The lexer's own error passes through unchanged; the failures the lexer has no opinion about are
/// one `UnexpectedLexeme` over the slice.
#[inline]
pub fn identifier(value: &str) -> Result<&str, error::LexerErrors<char, RecursionLimitExceeded>> {
  match scan_one_token(value)? {
    syntactic::SyntacticToken::Identifier(name) => Ok(name),
    _ => Err(
      error::LexerError::const_new(
        SimpleSpan::new(0, value.len()),
        error::LexerErrorData::UnexpectedLexeme(Lexeme::from_range_const(SimpleSpan::new(
          0,
          value.len(),
        ))),
      )
      .into(),
    ),
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitInt<&'a str> {
  type Error = error::LexerErrors<char, RecursionLimitExceeded>;

  /// The scanner's own reading of `value` as one whole integer literal.
  ///
  /// # Whole, and the scanner's
  ///
  /// The slice must scan to **exactly one** token, that token must be an integer literal, and its
  /// span must be the whole slice: leading or trailing trivia fails it, a second token fails it,
  /// and a token of any other kind fails it. A caller asking for a literal is asking about the
  /// bytes it handed over, not about a prefix of them.
  ///
  /// The scan is [`SyntacticLexer`](syntactic::SyntacticLexer) — the shipped one, not a second
  /// grammar — which is why there is nothing left for two layers to disagree about. It exists
  /// because a consumer that rebuilds a literal from a tree had nothing else to ask: the tree keeps
  /// one `Int` image over all four radices, the number grammars are private, and the CST → AST
  /// projection was classifying by prefix. `0b2`, `0x` and a `Float`-labelled `0x1` all produced a
  /// literal. al8n/smear#58.
  ///
  /// A slice the scanner reads as a **float** fails here rather than being coerced: the two
  /// carriers are different types because the grammar's two productions are different, and
  /// answering an `Int` for `0x1p3` would be this door inventing the classification it exists to
  /// stop inventing.
  ///
  /// The lexer's own error passes through unchanged; the failures the lexer has no opinion about
  /// are one `UnexpectedLexeme` over the slice.
  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    match scan_one_token(value)? {
      syntactic::SyntacticToken::LitInt(lit) => Ok(lit),
      _ => Err(
        error::LexerError::const_new(
          SimpleSpan::new(0, value.len()),
          error::LexerErrorData::UnexpectedLexeme(Lexeme::from_range_const(SimpleSpan::new(
            0,
            value.len(),
          ))),
        )
        .into(),
      ),
    }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitFloat<&'a str> {
  type Error = error::LexerErrors<char, RecursionLimitExceeded>;

  /// The scanner's own reading of `value` as one whole float literal.
  ///
  /// [`LitInt`]'s twin — see [`LitInt::try_from`] for what "whole" means, for why the door exists,
  /// and for why an integer slice fails here rather than being widened.
  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    match scan_one_token(value)? {
      syntactic::SyntacticToken::LitFloat(lit) => Ok(lit),
      _ => Err(
        error::LexerError::const_new(
          SimpleSpan::new(0, value.len()),
          error::LexerErrorData::UnexpectedLexeme(Lexeme::from_range_const(SimpleSpan::new(
            0,
            value.len(),
          ))),
        )
        .into(),
      ),
    }
  }
}
