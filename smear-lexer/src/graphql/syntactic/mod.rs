use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokit::{state::recursion_tracker::RecursionLimitExceeded, utils::cmp::Equivalent};

use super::{
  super::{LitBlockStr, LitInlineStr},
  error,
};

/// All GraphQL reserved keywords.
const GRAPHQL_KEYWORDS: &[&str] = &[
  "type",
  "interface",
  "union",
  "enum",
  "input",
  "scalar",
  "extend",
  "schema",
  "directive",
  "fragment",
  "query",
  "mutation",
  "subscription",
  "implements",
  "repeatable",
  "on",
  "true",
  "false",
  "null",
];

/// Check if a `SyntacticToken` is a GraphQL keyword, returning the keyword string if so.
#[inline]
pub fn graphql_keyword<S>(tok: &SyntacticToken<S>) -> Option<&'static str>
where
  str: tokit::utils::cmp::Equivalent<S>,
{
  match tok {
    SyntacticToken::Identifier(s) => GRAPHQL_KEYWORDS
      .iter()
      .copied()
      .find(|kw| (*kw).equivalent(s)),
    _ => None,
  }
}

#[cfg(test)]
mod tests;

/// The focused GraphQL number sub-lexer the SIMD lexer delegates malformed
/// numbers to; see [`number::NumberToken`].
pub(crate) mod number;

/// The syntactic GraphQL lexer — the SIMD-accelerated lexer. Generic over the
/// *source* type `S` (defaulting to `str`); Logos survives only as an internal
/// slow-path delegate of the SIMD lexer.
// ?Sized is required for the default `str` (and `[u8]`) source; the bound is
// enforced on SimdSyntacticLexer itself.
#[allow(type_alias_bounds)]
pub type SyntacticLexer<'a, S: ?Sized = str> = SimdSyntacticLexer<'a, S>;

/// The error data type for lexing based on syntactic token with `char` source.
pub type SyntacticLexerErrorData<Char = char> = error::LexerErrorData<Char, RecursionLimitExceeded>;
/// The error type for lexing based on syntactic token with `char` source.
pub type SyntacticLexerError<Char = char> = error::LexerError<Char, RecursionLimitExceeded>;
/// A collection of errors for syntactic token with `char` source.
pub type SyntacticLexerErrors<Char = char> = error::LexerErrors<Char, RecursionLimitExceeded>;

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
/// use smear::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};
/// use tokit::Lexer;
///
/// let source = "query { user { id } }";
/// let mut lexer = SyntacticLexer::new(source);
///
/// // Only syntactically significant tokens appear in the stream:
/// // Identifier("query"), LBrace, Identifier("user"), LBrace, Identifier("id"), RBrace, RBrace
/// // (whitespace is automatically skipped)
/// while let Some(token) = lexer.lex() {
///   // ...
/// }
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

// `SyntacticToken`'s public token/keyword/punctuator surface — the SIMD lexer's
// `Token` associated type and the parser's keyword/punctuator queries — NOT
// Logos-derive machinery. `S: Slice<'a>` supplies the `Char` for the frozen
// `Error` type, and `S::Char`/`Clone` reproduce each flavor's bounds exactly
// (str/HipStr -> char, &[u8]/Bytes/HipByt -> u8).

impl<'a, S> tokit::Token<'a> for SyntacticToken<S>
where
  S: tokit::Slice<'a> + Clone + 'a,
{
  type Kind = SyntacticTokenKind;
  type Error = error::LexerErrors<S::Char, RecursionLimitExceeded>;

  #[inline(always)]
  fn kind(&self) -> Self::Kind {
    self.kind()
  }

  #[inline(always)]
  fn is_trivia(&self) -> bool {
    false
  }
}

impl<'a, S> tokit::token::KeywordToken<'a> for SyntacticToken<S>
where
  S: tokit::Slice<'a> + Clone + 'a,
  str: Equivalent<S>,
{
  fn keyword(&self) -> Option<&'static str> {
    graphql_keyword(self)
  }
}

impl<'a, S> tokit::token::PunctuatorToken<'a> for SyntacticToken<S>
where
  S: tokit::Slice<'a> + Clone + 'a,
{
  fn pipe() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Pipe)
  }
  fn ampersand() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Ampersand)
  }
  fn at() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::At)
  }
  fn colon() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Colon)
  }
  fn open_paren() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::LParen)
  }
  fn close_paren() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::RParen)
  }
  fn open_brace() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::LBrace)
  }
  fn close_brace() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::RBrace)
  }
  fn open_bracket() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::LBracket)
  }
  fn close_bracket() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::RBracket)
  }
  fn equal() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Equal)
  }
  fn exclamation() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Bang)
  }
  fn dollar() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Dollar)
  }
}

// Required by the `Punctuator` trait impls on tokit's punctuator structs
// (e.g. `tokit::punct::Pipe`), which are used as the `Sep` type parameter
// in `SeparatedWhile`.

impl From<tokit::punct::Pipe<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::Pipe<(), (), ()>) -> Self {
    Self::Pipe
  }
}

impl From<tokit::punct::Ampersand<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::Ampersand<(), (), ()>) -> Self {
    Self::Ampersand
  }
}

impl From<tokit::punct::At<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::At<(), (), ()>) -> Self {
    Self::At
  }
}

impl From<tokit::punct::Colon<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::Colon<(), (), ()>) -> Self {
    Self::Colon
  }
}

impl From<tokit::punct::OpenParen<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::OpenParen<(), (), ()>) -> Self {
    Self::LParen
  }
}

impl From<tokit::punct::CloseParen<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::CloseParen<(), (), ()>) -> Self {
    Self::RParen
  }
}

impl From<tokit::punct::OpenBrace<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::OpenBrace<(), (), ()>) -> Self {
    Self::LBrace
  }
}

impl From<tokit::punct::CloseBrace<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::CloseBrace<(), (), ()>) -> Self {
    Self::RBrace
  }
}

impl From<tokit::punct::OpenBracket<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::OpenBracket<(), (), ()>) -> Self {
    Self::LBracket
  }
}

impl From<tokit::punct::CloseBracket<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::CloseBracket<(), (), ()>) -> Self {
    Self::RBracket
  }
}

impl From<tokit::punct::Equal<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::Equal<(), (), ()>) -> Self {
    Self::Equal
  }
}

impl From<tokit::punct::Exclamation<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::Exclamation<(), (), ()>) -> Self {
    Self::Bang
  }
}

impl From<tokit::punct::Dollar<(), (), ()>> for SyntacticTokenKind {
  #[inline]
  fn from(_: tokit::punct::Dollar<(), (), ()>) -> Self {
    Self::Dollar
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

use tokit::{
  Lexer, SimpleSpan, Slice, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::RecursionLimiter,
  utils::CharLen,
};

use crate::{
  LitComplexBlockStr, LitComplexInlineStr, LitPlainStr,
  error::{BadStateError, UnterminatedSpreadOperatorError},
  graphql::error::{LexerError, LexerErrorData, LexerErrors},
  skip_block_str_from_bytes, skip_inline_str_simd,
  string_lexer::DelegateStringError,
};

use self::number::NumberToken;

use crate::simd_common::{
  Delegated, NumberKind, memchr_newline, scan_identifier, scan_number, skip_ws_and_comma,
};

// Re-exported so the public `graphql::syntactic::{AsBytes, ScanSource}` paths
// and `DEFAULT_RECURSION_LIMIT` stay stable now that these dialect-agnostic
// items live in `crate::simd_common`.
pub use crate::simd_common::{AsBytes, DEFAULT_RECURSION_LIMIT, ScanSource};

/// SIMD layer over the GraphQL syntactic lexer. Streaming, single-pass, one
/// token per call.
///
/// Construct with [`SimdSyntacticLexer::new`] or
/// [`SimdSyntacticLexer::with_state`], then call [`lex`](Lexer::lex) in a
/// loop until it returns `None`.
///
/// Malformed numbers delegate to the focused `NumberToken` grammar (see
/// `delegate_number_to_logos`) and malformed strings to `string_lexer` (see
/// `delegate_string_error`), each by constructing a fresh sub-lexer over the
/// full source and bumping it to the current cursor rather than reusing one
/// across calls; the unknown-byte error is hand-rolled inline in [`lex`](Lexer::lex).
pub struct SimdSyntacticLexer<'inp, S: ?Sized = str> {
  src: &'inp S,
  /// Current scan position. Advanced by trivia-skip, comment-skip, and each
  /// token scan. Never exposed directly — callers see `last_span`.
  cursor: usize,
  /// Span of the most recently lexed token, valid or error. Updated on every
  /// return path so `span()`/`slice()` report the current token exactly like
  /// `LogosLexer`, including an error token.
  last_span: SimpleSpan,
  /// Span of the most recently returned error token, if any.
  last_error_span: Option<SimpleSpan>,
  state: RecursionLimiter,
}

impl<'inp, S> Lexer<'inp> for SimdSyntacticLexer<'inp, S>
where
  SyntacticToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  S: ScanSource + ?Sized,
  S::Slice<'inp>: AsBytes,
  // The string-error slow path (`delegate_string_error`) hands the malformed
  // literal straight to `string_lexer`'s `lex_*` over this source's scan
  // primitive; that primitive is always `str`/`[u8]`, both of which impl
  // `DelegateStringError`, and its `Char` is this lexer's `Slice::Char`.
  <S as ScanSource>::ScanPrimitive:
    DelegateStringError<Char = <S::Slice<'inp> as Slice<'inp>>::Char>,
  // The number slow path (`delegate_number_to_logos`) delegates malformed
  // numbers to the focused `NumberToken` grammar, driven over this source's
  // scan primitive; its `Error` is this lexer's error type, so the delegated
  // error needs no conversion, and its `Int`/`Float` map to `SyntacticToken`.
  NumberToken<S::Slice<'inp>>: FromLogos<'inp>,
  LogosLexer<'inp, NumberToken<S::Slice<'inp>>>: Lexer<
      'inp,
      State = RecursionLimiter,
      Token = NumberToken<S::Slice<'inp>>,
      Source = <S as ScanSource>::ScanPrimitive,
      Offset = usize,
    >,
  NumberToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  SyntacticToken<S::Slice<'inp>>: From<NumberToken<S::Slice<'inp>>>,
  // The hand-rolled unknown-byte `_` arm decodes the first char at the cursor
  // and needs its UTF-8 byte length to size the error span exactly like Logos.
  <S::Slice<'inp> as Slice<'inp>>::Char: CharLen,
{
  type State = RecursionLimiter;

  type Source = S;

  type Token = SyntacticToken<S::Slice<'inp>>;

  type Span = SimpleSpan;

  type Offset = usize;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn new(src: &'inp Self::Source) -> Self {
    Self::with_state(src, Self::State::default())
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn with_state(src: &'inp Self::Source, state: Self::State) -> Self {
    Self {
      src,
      cursor: 0,
      last_span: SimpleSpan::const_new(0, 0),
      last_error_span: None,
      state,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn check(&self) -> Result<(), <Self::Token as tokit::Token<'inp>>::Error> {
    self.state.check().map_err(Into::into)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn state(&self) -> &Self::State {
    &self.state
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn state_mut(&mut self) -> &mut Self::State {
    &mut self.state
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn into_state(self) -> Self::State {
    self.state
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn source(&self) -> &'inp Self::Source {
    self.src
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn span(&self) -> Self::Span {
    self.last_span
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn slice(&self) -> <Self::Source as Source<Self::Offset>>::Slice<'inp> {
    self
      .src
      .slice(self.last_span.start_ref()..self.last_span.end_ref())
      .unwrap()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn lex(&mut self) -> Option<Result<Self::Token, <Self::Token as tokit::Token<'inp>>::Error>> {
    // Bracket-close: decrement the depth, then re-check the limit *post-decrease*
    // (a decrease can bring an over-limit stream back under the limit). Because
    // it changes depth it cannot share the once-per-token entry-depth `recheck`
    // that the non-bracket emitters use below — it checks its own post-decrease
    // depth, mirroring `LogosLexer::lex`'s post-token `extras.check()`. The error
    // is `RecursionLimitExceeded.into()` (built out-of-line in
    // `over_recursion_limit`), matching Logos's `extras.check().map_err(Into::into)`.
    macro_rules! decrease_recursion {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 1;
        $this.last_span = SimpleSpan::new($token_start, $this.cursor);
        $this.state_mut().decrease();
        match $this.state.check() {
          Ok(()) => Ok($expr),
          Err(e) => Err($this.over_recursion_limit(e)),
        }
      }};
    }

    macro_rules! increase_recursion {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 1;
        let span = SimpleSpan::new($token_start, $this.cursor);
        $this.state_mut().increase();
        match $this.state.check() {
          Ok(()) => {
            $this.last_span = span;
            Ok($expr)
          }
          Err(e) => {
            $this.last_span = span;
            Err($this.bad_state_error(span, e))
          }
        }
      }};
    }

    loop {
      self.skip_ws_and_comma();

      let token_start = self.cursor;
      // Both `None` exits below are EOF. Mirror `LogosLexer`, whose span resets
      // to `cursor..cursor` (EOF..EOF) once `next()` returns `None` — including
      // after the trailing trivia/comments the loop just skipped — so that after
      // EOF `span()`/`slice()` report the empty EOF span and a post-EOF `bump`
      // grows from EOF, not the stale last token.
      let Some(src) = self.src.slice(&token_start..) else {
        self.last_span = SimpleSpan::new(self.cursor, self.cursor);
        return None;
      };
      if src.is_empty() {
        self.last_span = SimpleSpan::new(self.cursor, self.cursor);
        return None;
      }

      let bytes = src.as_bytes();
      let b0 = bytes[0];
      // Once-per-token entry-depth recursion check, shared by every non-bracket
      // emitter: the `match b0` below evaluates to the emitted `SyntacticToken`
      // (or diverges via `return`/`continue`), and a single tail check gates it,
      // off the emission critical path (one check rather than one per emitter).
      // Non-bracket tokens never change depth, so their post-token depth equals
      // this entry depth (Logos re-checks after every token; one shared check is
      // byte-for-byte equivalent for them). Bracket arms mutate depth and
      // re-check themselves in `increase_recursion!`/`decrease_recursion!`.
      let recheck = self.state.check();
      // Emit a single-byte punctuation token: advance past it and yield the token
      // as the `match` value; the shared tail check gates it.
      macro_rules! emit_punct {
        ($this:ident: $token_start:expr, $expr:expr) => {{
          $this.cursor += 1;
          $this.last_span = SimpleSpan::new($token_start, $this.cursor);
          $expr
        }};
      }
      let token = match b0 {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
          let len = scan_identifier(bytes);
          self.cursor += len;
          self.last_span = SimpleSpan::new(token_start, self.cursor);
          let ident = self.src.slice(&token_start..&(token_start + len))?;
          // Direct return (no shared-tail token move): identifiers are the most
          // common token and carry only a slice, so returning here — rather than
          // materializing the large `SyntacticToken` enum for the tail — avoids a
          // per-ident wide move. Still gated by the same shared `recheck`.
          return Some(match recheck {
            Ok(()) => Ok(SyntacticToken::Identifier(ident)),
            Err(e) => Err(self.over_recursion_limit(e)),
          });
        }
        // Valid-number fast path: `scan_number` only ever
        // answers "clean, valid literal of length N" or "anomaly" — it never
        // constructs an error. A clean literal is emitted directly (no
        // Logos involved); any anomaly (leading zeros, empty frac/exponent,
        // an illegal suffix, a lone `-`, ...) delegates the *whole* token to
        // the focused `NumberToken` grammar, which already knows how to build
        // the exact error. Because every error still originates from the same
        // reused handlers, oracle parity holds without re-deriving any error
        // logic here.
        //
        // A bare `+` is in this arm (not the `_` arm) purely so `scan_number`'s
        // `None` routes it to `NumberToken`'s `#[token("+")]` — an
        // `UnexpectedLexeme`, matching the pre-SIMD lexer — rather than the
        // hand-rolled `_` arm's `UnknownLexeme`. (`scan_number` still returns
        // `None` for a leading `+`: it is not a valid number start.)
        b'0'..=b'9' | b'-' | b'+' => match scan_number(bytes) {
          Some((NumberKind::Int, len)) => {
            self.cursor += len;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            let slice = self.src.slice(&token_start..&(token_start + len))?;
            SyntacticToken::LitInt(slice)
          }
          Some((NumberKind::Float, len)) => {
            self.cursor += len;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            let slice = self.src.slice(&token_start..&(token_start + len))?;
            SyntacticToken::LitFloat(slice)
          }
          None => return self.delegate_number_to_logos(token_start),
        },
        b'!' => emit_punct!(self: token_start, SyntacticToken::Bang),
        b'&' => emit_punct!(self: token_start, SyntacticToken::Ampersand),
        b'(' => return Some(increase_recursion!(self: token_start, SyntacticToken::LParen)),
        b')' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RParen)),
        b':' => emit_punct!(self: token_start, SyntacticToken::Colon),
        b'=' => emit_punct!(self: token_start, SyntacticToken::Equal),
        b'@' => emit_punct!(self: token_start, SyntacticToken::At),
        b'$' => emit_punct!(self: token_start, SyntacticToken::Dollar),
        b'[' => return Some(increase_recursion!(self: token_start, SyntacticToken::LBracket)),
        b']' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RBracket)),
        b'{' => return Some(increase_recursion!(self: token_start, SyntacticToken::LBrace)),
        b'}' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RBrace)),
        b'|' => emit_punct!(self: token_start, SyntacticToken::Pipe),
        // Spread / unterminated-spread: load b1/b2 lazily — only paid when b0 == b'.'.
        b'.' => {
          if bytes.starts_with(b"...") {
            self.cursor += 3;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            SyntacticToken::Spread
          } else if bytes.starts_with(b"..") {
            self.cursor += 2;
            let span = SimpleSpan::new(token_start, self.cursor);
            self.last_span = span;
            self.last_error_span = Some(span);
            let err = LexerErrors::<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>::unterminated_spread_operator(span);
            return Some(Err(err));
          } else if matches!(bytes.get(1), Some(b'0'..=b'9')) {
            // `.` immediately followed by a digit (e.g. `.5`) is a Float
            // literal missing its integer part, not a spread operator —
            // `NumberToken`'s Float regex (`-?(frac)(exp)?`) matches it and
            // reports the exact "missing integer part" error (plus any chained
            // suffix error), so hand the whole token to it rather than
            // mis-emitting an unterminated-spread-operator error.
            return self.delegate_number_to_logos(token_start);
          } else {
            self.cursor += 1;
            let span = SimpleSpan::new(token_start, self.cursor);
            self.last_span = span;
            self.last_error_span = Some(span);
            let err = LexerErrors::<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>::unterminated_spread_operator(span);
            return Some(Err(err));
          }
        }
        b'#' => {
          self.skip_comment();
          continue;
        }

        // Block strings (`"""…"""`): valid literals emit via the SIMD
        // scanner (structural twin of the inline-string arm below, with `3`
        // for the opening `"""` where inline uses `1`); an unterminated body
        // delegates the whole token to Logos so the error is built for
        // whatever `Char` this source uses.
        b'"' if bytes.starts_with(b"\"\"\"") => {
          match skip_block_str_from_bytes(&bytes[3..]) {
            Ok(lit) => {
              let consumed = *lit.source_ref(); // content + closing `"""`
              self.cursor += 3 + consumed; // 3 = opening `"""`
              self.last_span = SimpleSpan::new(token_start, self.cursor);
              let slice = self.src.slice(&token_start..&self.cursor).unwrap();
              let block = match lit {
                LitBlockStr::Plain(_) => LitBlockStr::Plain(LitPlainStr::new(slice)),
                LitBlockStr::Complex(c) => LitBlockStr::Complex(LitComplexBlockStr::new(
                  slice,
                  c.num_escaped_triple_quotes(),
                  c.has_cr_terminators(),
                  c.leading_blank_lines(),
                  c.trailing_blank_lines(),
                  c.common_indent(),
                  c.total_lines(),
                  c.required_capacity(),
                )),
              };
              SyntacticToken::LitBlockStr(block)
            }
            Err(_) => return self.delegate_string_error(token_start, true),
          }
        }
        b'"' if !bytes.starts_with(b"\"\"\"") => {
          match bytes.get(1).copied() {
            Some(b'"') => {
              // Empty inline string "".
              self.cursor += 2;
              self.last_span = SimpleSpan::new(token_start, self.cursor);
              let slice = self.src.slice(&token_start..&self.cursor).unwrap();
              SyntacticToken::LitInlineStr(LitInlineStr::Plain(LitPlainStr::new(slice)))
            }
            None => {
              // Lone `"` at end of input — an unterminated inline string.
              // Route it through the inline string sub-lexer (via the same
              // error-delegation path as any other inline-string error) so the
              // `unterminated_inline_string` error is built generically for
              // whatever `Char` this source uses, instead of hand-rolling it
              // here (hybrid: the fast path never constructs source-typed
              // errors itself).
              return self.delegate_string_error(token_start, false);
            }
            Some(_) => match skip_inline_str_simd(token_start + 1, &bytes[1..]) {
              Ok(lit) => {
                let consumed = *lit.source_ref();
                self.cursor += 1 + consumed;
                self.last_span = SimpleSpan::new(token_start, self.cursor);
                let slice = self.src.slice(&token_start..&self.cursor).unwrap();
                let inline = match lit {
                  LitInlineStr::Plain(_) => LitInlineStr::Plain(LitPlainStr::new(slice)),
                  LitInlineStr::Complex(c) => {
                    LitInlineStr::Complex(LitComplexInlineStr::new(slice, c.required_capacity()))
                  }
                };
                SyntacticToken::LitInlineStr(inline)
              }
              // `skip_inline_str_simd` only ever answers "clean, valid
              // literal" or "byte-indexed anomaly" — like the number fast
              // path, any anomaly delegates the *whole* token to the inline
              // string sub-lexer (`string_lexer::inline`) rather than
              // re-deriving the error here. This is also what drops the
              // `Char = u8` requirement.
              Err(_) => return self.delegate_string_error(token_start, false),
            },
          }
        }
        0xEF if bytes.starts_with(b"\xEF\xBB\xBF") => {
          self.cursor += 3;
          continue;
        }
        // Unknown byte: no fast path claims it, and no slow-path grammar
        // (numbers, strings) applies. Decode the first char at the cursor and
        // emit an `UnknownLexeme` error byte-identical to the full grammar's
        // Logos `default_error` (`graphql/handlers/{str,slice}.rs`): the char,
        // a `token_start..token_start + char_len` span, and `token_start` as
        // the position. The `unexpected_eoi` branch of `default_error` is
        // unreachable here — the loop's empty-source check above already
        // returned `None` for an empty slice, so `iter().next()` is always
        // `Some`.
        _ => {
          let ch = self.src.slice(&token_start..)?.iter().next()?;
          let len = ch.char_len();
          let span = SimpleSpan::new(token_start, token_start + len);
          self.cursor = token_start + len;
          self.last_span = span;
          self.last_error_span = Some(span);
          return Some(Err(LexerErrors::from(LexerError::unknown_char(
            span,
            ch,
            token_start,
          ))));
        }
      };
      // Single shared post-token recursion check for the just-produced
      // non-bracket token (see `recheck` above): equal to Logos's
      // post-every-token `extras.check()` for these depth-preserving tokens,
      // paid once here instead of at each emission site.
      return Some(match recheck {
        Ok(()) => Ok(token),
        Err(e) => Err(self.over_recursion_limit(e)),
      });
    }
  }

  /// Extend the current token's end by `n`, mirroring `logos::Lexer::bump`:
  /// the span start is kept while the end — and `cursor`, which always tracks
  /// the last token's end after a lex — grow by `n`, so `span()`/`slice()`
  /// include the bumped bytes. The new end is then validated as a source
  /// boundary.
  ///
  /// # Panics
  ///
  /// Panics with `"Invalid Lexer bump"` if the new end is not a boundary of
  /// the source — past its byte length, or (for `str` sources) in the middle
  /// of a UTF-8 code point — exactly as `logos::Lexer::bump` does.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn bump(&mut self, n: &Self::Offset) {
    let new_end = self.last_span.end() + *n;
    self.last_span = SimpleSpan::new(self.last_span.start(), new_end);
    self.cursor = new_end;
    assert!(self.src.is_boundary(new_end), "Invalid Lexer bump");
  }
}

// This block needs almost the trait impl's bound set — everything except
// `Span = SimpleSpan` and `S::Slice<'inp>: AsBytes`, neither of which
// `delegate_number_to_logos` touches (it builds `SimpleSpan` directly and reads
// the wrapped Logos lexer's own span, never `Self::Span` or `.as_bytes()`).
// Its return type names the concrete `SyntacticToken`/`Error` types, which
// requires the `Token<'inp, Error = ..>` + `From<NumberToken>` bounds. Every
// caller is inside `lex()` above, where the trait impl's full bound set already
// holds, so this is never more restrictive in practice than the trait impl.
//
// `NumberToken` is a crate-internal delegation carrier (`pub(crate)`); it
// appears here only as a bound on a private method, never as nameable API, so
// silence `private_bounds` rather than widen the type's visibility.
#[allow(private_bounds)]
impl<'inp, S> SimdSyntacticLexer<'inp, S>
where
  NumberToken<S::Slice<'inp>>: FromLogos<'inp>,
  LogosLexer<'inp, NumberToken<S::Slice<'inp>>>: Lexer<
      'inp,
      State = RecursionLimiter,
      Token = NumberToken<S::Slice<'inp>>,
      Source = <S as ScanSource>::ScanPrimitive,
      Offset = usize,
    >,
  NumberToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  SyntacticToken<S::Slice<'inp>>: From<NumberToken<S::Slice<'inp>>>,
  S: ScanSource + ?Sized,
{
  /// Cold, out-of-line constructor for the recursion-limit error a token
  /// emission yields when the depth is over the limit (the `finish!` gate's
  /// error arm). Marked `#[cold]` + `#[inline(never)]` so the never-taken
  /// (under-the-limit) error construction does not get inlined into the hot
  /// `lex()` at every one of its ~8 emission sites — the fast path is then just
  /// the inlined `state.check()` branch, restoring the pre-hardening throughput
  /// while keeping the post-token recursion parity `finish!` guarantees.
  #[cold]
  #[inline(never)]
  fn over_recursion_limit(
    &mut self,
    e: RecursionLimitExceeded,
  ) -> LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded> {
    self.last_error_span = Some(self.last_span);
    e.into()
  }

  /// Cold, out-of-line constructor for the over-limit error emitted by the
  /// *increase-bracket* path (`increase_recursion!`). Its error shape is
  /// `LexerError::bad_state(span, e)` — the bracket handler's own error, not the
  /// plain `RecursionLimitExceeded.into()` the post-token `finish!` gate uses.
  /// Kept `#[cold]` + `#[inline(never)]` for the same reason as
  /// [`Self::over_recursion_limit`]: the never-taken construction must not bloat
  /// the hot `lex()` at each of the three bracket-open sites.
  #[cold]
  #[inline(never)]
  fn bad_state_error(
    &mut self,
    span: SimpleSpan,
    e: RecursionLimitExceeded,
  ) -> LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded> {
    self.last_error_span = Some(span);
    LexerError::bad_state(span, e).into()
  }

  /// Delegate the malformed number opening at `token_start` (== `self.cursor`,
  /// prior to any mutation for this token) to the focused [`NumberToken`]
  /// grammar — the number-only slice of the full grammar's `#[regex]`/`#[token]`
  /// arms, calling the same frozen handlers — rather than rebuilding the whole
  /// `SyntacticToken` grammar.
  ///
  /// The number and `.`-led-float fast paths route here for any anomaly they
  /// detect (leading zeros, an empty fraction/exponent, an illegal suffix, a
  /// lone `-`/`+`, a `.`-led float missing its integer part, ...). Valid
  /// numbers are emitted directly by the SIMD fast path and never reach here,
  /// so the delegated grammar effectively only ever yields errors; the
  /// [`Delegated::Token`] arm — mapped through [`SyntacticToken::from`] — is
  /// kept for type-completeness. `NumberToken`'s `Error` is this lexer's error
  /// type, so the [`Delegated::Error`] arm needs no conversion.
  // The return type mirrors `Lexer::lex`'s own `Option<Result<Token, Error>>`
  // shape; clippy can't see through the associated-type projections, so silence
  // its type-complexity lint here.
  #[allow(clippy::type_complexity)]
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn delegate_number_to_logos(
    &mut self,
    token_start: usize,
  ) -> Option<
    Result<
      SyntacticToken<S::Slice<'inp>>,
      LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>,
    >,
  > {
    match crate::simd_common::delegate_to_logos::<NumberToken<S::Slice<'inp>>>(
      self.src.scan_primitive(),
      self.cursor,
      self.state,
    )? {
      Delegated::Token { token, end, state } => {
        self.cursor = end;
        self.last_span = SimpleSpan::new(token_start, end);
        self.state = state;
        Some(Ok(SyntacticToken::from(token)))
      }
      Delegated::Error { error, span } => {
        self.cursor = span.end();
        self.last_span = span;
        self.last_error_span = Some(span);
        Some(Err(error))
      }
    }
  }
}

// Bounds mirror the trait impl's, minus the Logos machinery this path never
// touches (it never builds a `LogosLexer`), plus the `DelegateStringError`
// predicate the string sub-lexer delegation needs — always satisfiable, since
// `ScanPrimitive` is `str`/`[u8]`. As with `delegate_number_to_logos`, every
// caller is inside `lex()`, where the trait impl's full bound set (which now
// includes this predicate) already holds.
//
// `DelegateStringError` is a crate-internal delegation detail (`pub(crate)`);
// it appears here only as a bound on a private method, never as nameable API,
// so silence `private_bounds` rather than widen the trait's visibility.
#[allow(private_bounds)]
impl<'inp, S> SimdSyntacticLexer<'inp, S>
where
  S: ScanSource + ?Sized,
  <S as ScanSource>::ScanPrimitive:
    DelegateStringError<Char = <S::Slice<'inp> as Slice<'inp>>::Char>,
{
  /// Delegate the malformed string literal opening at `token_start` directly to
  /// the `string_lexer` `lex_*` code — the same path the full grammar reaches
  /// through its `#[token("\"")]` / `#[token("\"\"\"")]` arms — instead of
  /// rebuilding the whole `SyntacticToken` grammar.
  ///
  /// `block` picks the `"""` (block) vs `"` (inline) carrier. Valid strings are
  /// emitted by the SIMD fast path and never reach here, so the delegated lexer
  /// always yields errors; each is wrapped in a `String` [`LexerErrorData`] with
  /// span `token_start..end` — byte-identical to the error token the full
  /// grammar produced — and `cursor`/`last_span`/`last_error_span` are folded
  /// exactly as [`delegate_number_to_logos`]'s [`Delegated::Error`] arm does.
  ///
  /// [`delegate_number_to_logos`]: Self::delegate_number_to_logos
  #[allow(clippy::type_complexity)]
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn delegate_string_error(
    &mut self,
    token_start: usize,
    block: bool,
  ) -> Option<
    Result<
      SyntacticToken<S::Slice<'inp>>,
      LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>,
    >,
  > {
    let (errors, end) = self
      .src
      .scan_primitive()
      .delegate_string_error(token_start, block);
    let span = SimpleSpan::new(token_start, end);
    self.cursor = end;
    self.last_span = span;
    self.last_error_span = Some(span);
    Some(Err(LexerErrors::from(LexerError::new(
      span,
      LexerErrorData::String(errors),
    ))))
  }
}

impl<S: ?Sized> SimdSyntacticLexer<'_, S> {
  /// Span of the most recently returned error token, or `None` if no error
  /// has been returned yet.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn error_span(&self) -> Option<SimpleSpan> {
    self.last_error_span
  }
}

impl<'inp, S: Source<usize> + ?Sized> SimdSyntacticLexer<'inp, S>
where
  S::Slice<'inp>: AsBytes,
{
  /// Skip the single-byte trivia class (space, tab, CR, LF, comma)
  /// at the cursor. Comment bodies and the UTF-8 BOM are handled in
  /// the dispatch loop so this stays a single SIMD scan with no per-call
  /// branchy interpretation overhead.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn skip_ws_and_comma(&mut self) {
    let Some(rest) = self.src.slice(&self.cursor..) else {
      return;
    };
    let n = skip_ws_and_comma(rest.as_bytes());
    self.cursor += n;
  }

  /// Skip a `#…\n|\r` comment body. Cursor is positioned at the `#`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn skip_comment(&mut self) {
    let Some(rest) = self.src.slice(&self.cursor..) else {
      return;
    };
    let len = match memchr_newline(rest.as_bytes()) {
      Some(n) => n,       // stop AT the newline; loop eats it next
      None => rest.len(), // unterminated -> EOF
    };
    self.cursor += len;
  }
}

#[cfg(test)]
mod str_arm_tests;

#[cfg(test)]
mod num_arm_tests;

#[cfg(test)]
mod generic_source_tests;

#[cfg(test)]
mod error_parity_tests;

#[cfg(test)]
mod bump_parity_tests;

#[cfg(test)]
mod trait_parity_tests;
