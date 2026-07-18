use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::{state::recursion_tracker::RecursionLimitExceeded, utils::cmp::Equivalent};

use super::{
  super::{LitBlockStr, LitInlineStr},
  LitFloat, LitInt, error,
};

/// All GraphQLx reserved keywords: the GraphQL set plus the GraphQLx-only
/// `import`/`from`/`as`/`where`/`set`/`map`. Every spelling is soft (contextual)
/// — the lexer still yields an [`Identifier`](SyntacticToken::Identifier) for
/// each — so this list only feeds the [`KeywordToken`](tokora::token::KeywordToken)
/// classification surface a production consults when it asks for a keyword.
const GRAPHQLX_KEYWORDS: &[&str] = &[
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
  "import",
  "from",
  "as",
  "where",
  "set",
  "map",
];

/// Check if a `SyntacticToken` is a GraphQLx keyword, returning the keyword string if so.
#[inline]
pub fn graphqlx_keyword<S>(tok: &SyntacticToken<S>) -> Option<&'static str>
where
  str: Equivalent<S>,
{
  match tok {
    SyntacticToken::Identifier(s) => GRAPHQLX_KEYWORDS
      .iter()
      .copied()
      .find(|kw| (*kw).equivalent(s)),
    _ => None,
  }
}

/// The focused GraphQLx number sub-lexer the SIMD lexer delegates malformed and
/// sign-ambiguous numbers to; see [`number::NumberToken`].
pub(crate) mod number;

/// The syntactic GraphQLx lexer — the SIMD-accelerated lexer. Generic over the
/// *source* type `S` (defaulting to `str`); Logos is used only as an internal
/// slow-path delegate of the SIMD lexer.
// ?Sized is required for the default `str` (and `[u8]`) source; the bound is
// enforced on SimdSyntacticLexer itself.
#[allow(type_alias_bounds)]
pub type SyntacticLexer<'a, S: ?Sized = str> = SimdSyntacticLexer<'a, S>;

/// The error data type for lexing based on syntactic token with `char` source.
pub type SyntacticLexerErrorData = error::LexerErrorData<char, RecursionLimitExceeded>;
/// The error type for lexing based on syntactic token with `char` source.
pub type SyntacticLexerError = error::LexerError<char, RecursionLimitExceeded>;
/// A collection of errors for syntactic token with `char` source.
pub type SyntacticLexerErrors = error::LexerErrors<char, RecursionLimitExceeded>;

/// A syntactic token for GraphQLx lexing that only includes syntactically significant tokens.
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
/// use smear::lexer::graphqlx::syntactic::{SyntacticLexer, SyntacticToken};
/// use tokora::Lexer;
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
  /// Asterisk `*` token
  Asterisk,
  /// Ampersand `&` token
  Ampersand,
  /// At `@` token
  At,
  /// Right angle bracket `>` token
  RAngle,
  /// Right curly brace `}` token
  RBrace,
  /// Right square bracket `]` token
  RBracket,
  /// Right parenthesis `)` token
  RParen,
  /// Dot `.` token
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
      Self::LAngle => SyntacticTokenKind::LAngle,
      Self::RAngle => SyntacticTokenKind::RAngle,
      Self::FatArrow => SyntacticTokenKind::FatArrow,
      Self::Plus => SyntacticTokenKind::Plus,
      Self::Minus => SyntacticTokenKind::Minus,
      Self::PathSeparator => SyntacticTokenKind::PathSeparator,
      Self::Asterisk => SyntacticTokenKind::Asterisk,
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

// `SyntacticToken`'s public `Token` surface — the SIMD lexer's `Token`
// associated type, one generic impl over every source flavor. `S: Slice<'a>`
// supplies the `Char` for the frozen `Error` type (str/HipStr -> char,
// &[u8]/Bytes/HipByt -> u8).

impl<'a, S> tokora::Token<'a> for SyntacticToken<S>
where
  S: tokora::Slice<'a> + Clone + 'a,
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

impl<'a, S> tokora::token::IdentifierToken<'a> for SyntacticToken<S>
where
  S: tokora::Slice<'a> + Clone + 'a,
{
  #[inline(always)]
  fn is_identifier(&self) -> bool {
    matches!(self, Self::Identifier(_))
  }
}

impl<'a, S> tokora::token::LitToken<'a> for SyntacticToken<S>
where
  S: tokora::Slice<'a> + Clone + 'a,
{
  // GraphQLx preserves the integer radix in the `LitInt` payload, so each radix
  // predicate inspects the payload variant rather than merely the token variant.
  // `is_integer_literal`/`is_numeric_literal` stay derived from these leaves.
  #[inline(always)]
  fn is_decimal_literal(&self) -> bool {
    matches!(self, Self::LitInt(LitInt::Decimal(_)))
  }

  #[inline(always)]
  fn is_hexadecimal_literal(&self) -> bool {
    matches!(self, Self::LitInt(LitInt::Hex(_)))
  }

  #[inline(always)]
  fn is_binary_literal(&self) -> bool {
    matches!(self, Self::LitInt(LitInt::Binary(_)))
  }

  #[inline(always)]
  fn is_octal_literal(&self) -> bool {
    matches!(self, Self::LitInt(LitInt::Octal(_)))
  }

  // GraphQLx has both decimal and hexadecimal floats; the latter reports through
  // `is_hex_float_literal`, which tokora keeps separate from `is_float_literal`.
  #[inline(always)]
  fn is_float_literal(&self) -> bool {
    matches!(self, Self::LitFloat(LitFloat::Decimal(_)))
  }

  #[inline(always)]
  fn is_hex_float_literal(&self) -> bool {
    matches!(self, Self::LitFloat(LitFloat::Hex(_)))
  }

  #[inline(always)]
  fn is_string_literal(&self) -> bool {
    matches!(self, Self::LitInlineStr(_) | Self::LitBlockStr(_))
  }

  #[inline(always)]
  fn is_inline_string_literal(&self) -> bool {
    matches!(self, Self::LitInlineStr(_))
  }

  #[inline(always)]
  fn is_multiline_string_literal(&self) -> bool {
    matches!(self, Self::LitBlockStr(_))
  }
}

impl<'a, S> tokora::token::KeywordToken<'a> for SyntacticToken<S>
where
  S: tokora::Slice<'a> + Clone + 'a,
  str: Equivalent<S>,
{
  fn keyword(&self) -> Option<&'static str> {
    graphqlx_keyword(self)
  }
}

impl<'a, S> tokora::token::PunctuatorToken<'a> for SyntacticToken<S>
where
  S: tokora::Slice<'a> + Clone + 'a,
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
  fn open_angle() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::LAngle)
  }
  fn close_angle() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::RAngle)
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
  fn spread() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Spread)
  }
  fn asterisk() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Asterisk)
  }
  fn plus() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Plus)
  }
  fn minus() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::Minus)
  }
  fn double_colon() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::PathSeparator)
  }
  fn fat_arrow() -> Option<Self::Kind> {
    Some(SyntacticTokenKind::FatArrow)
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
  /// Asterisk `*` token
  Asterisk,
  /// At `@` token
  At,
  /// Left square bracket `[` token
  LBracket,
  /// Right square bracket `]` token
  RBracket,
  /// Left curly brace `{` token
  LBrace,
  /// Right curly brace `}` token
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
}

impl core::fmt::Display for SyntacticTokenKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(self, f)
  }
}

// The GraphQLx syntactic lexer: a SIMD-accelerated, streaming, single-pass
// lexer (hybrid — SIMD fast paths + internal Logos slow-path delegation).
// This is the GraphQLx `SyntacticLexer`.
//
// This mirrors the GraphQL SIMD lexer (`crate::graphql::syntactic`) and shares its
// dialect-agnostic primitives through `crate::simd_common`: trivia, identifier,
// and valid-string scanning (inline and block, including block-string
// normalization) are SIMD-fast-pathed. The cold paths are handled without the
// full grammar: malformed / sign-ambiguous numbers delegate to the focused
// `NumberToken` grammar (see `delegate_number_to_logos`), malformed strings to
// `string_lexer` (see `delegate_string_error`), the unterminated-spread error
// is emitted inline, and the unknown-byte error is hand-rolled inline in
// [`lex`](tokora::Lexer::lex). Because the fast path never constructs a source-typed
// error itself, parity with the pre-SIMD lexer holds by construction.
//
// GraphQLx differs from GraphQL in the *dispatch*, not the architecture:
//
// - Angle brackets `<`/`>` are `LAngle`/`RAngle` and nest recursion (generics
//   like `Box<T>`), so they join `{}`/`[]`/`()` as recursion-affecting
//   punctuation.
// - `::` (`PathSeparator`) and `=>` (`FatArrow`) are two-byte tokens, peeled
//   apart from the single-byte `:` (`Colon`) and `=` (`Equal`) by peeking one
//   byte — the same longest match Logos performs.
// - `+` is always `Plus`, but `-` is a number *sign* whenever a digit or `.`
//   directly follows it (`-5` lexes as `Decimal("-5")`, `-.5` as a float), so
//   `-` is delegated to `NumberToken` alongside the digits rather than
//   fast-pathed as a bare `Minus` — `NumberToken`'s `#[token("-")] Minus` arm
//   resolves a `-` before any other byte back to the operator by longest match.
// - Numbers are radix-prefixed (decimal / hex / binary / octal, plus decimal
//   and hex floats) and always delegated to `NumberToken` — never hand-rolled.

use tokora::{
  Lexer, SimpleSpan, Slice, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::RecursionLimiter,
  utils::CharLen,
};

use crate::{
  LitComplexBlockStr, LitComplexInlineStr, LitPlainStr,
  error::{BadStateError, UnterminatedSpreadOperatorError},
  graphqlx::error::{LexerError, LexerErrorData, LexerErrors},
  skip_block_str_from_bytes, skip_inline_str_simd,
  string_lexer::DelegateStringError,
};

use self::number::NumberToken;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod error_parity_tests;

#[cfg(test)]
mod bump_parity_tests;

#[cfg(test)]
mod trait_parity_tests;

use crate::simd_common::{
  Delegated, NumberKind, memchr_newline, scan_identifier, scan_number, skip_ws_and_comma,
};

// Re-exported so the public `graphqlx::syntactic::{AsBytes, ScanSource}` paths
// and `DEFAULT_RECURSION_LIMIT` stay stable, matching `graphql::syntactic`, now
// that these dialect-agnostic items live in `crate::simd_common`.
pub use crate::simd_common::{AsBytes, DEFAULT_RECURSION_LIMIT, ScanSource};

/// SIMD layer over the GraphQLx syntactic lexer. Streaming, single-pass, one
/// token per call.
///
/// Construct with [`SimdSyntacticLexer::new`] or
/// [`SimdSyntacticLexer::with_state`], then call
/// [`lex`](Lexer::lex) in a loop until it returns `None`.
///
/// Malformed / sign-ambiguous numbers delegate to the focused `NumberToken`
/// grammar (see `delegate_number_to_logos`) and malformed strings to
/// `string_lexer` (see `delegate_string_error`), each by constructing a fresh
/// sub-lexer over the full source and bumping it to the current cursor rather
/// than reusing one across calls; the unterminated-spread and unknown-byte
/// errors are emitted inline in [`lex`](Lexer::lex).
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
  // The number slow path (`delegate_number_to_logos`) delegates malformed and
  // sign-ambiguous numbers to the focused `NumberToken` grammar, driven over
  // this source's scan primitive; its `Error` is this lexer's error type, so the
  // delegated error needs no conversion, and its radix/float/`Minus` variants
  // map to `SyntacticToken`.
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
  fn check(&self) -> Result<(), <Self::Token as tokora::Token<'inp>>::Error> {
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
  fn lex(&mut self) -> Option<Result<Self::Token, <Self::Token as tokora::Token<'inp>>::Error>> {
    // Post-token recursion gate, mirroring `LogosLexer::lex`: after a token is
    // scanned it re-checks the limiter and, while the depth is over the limit,
    // yields the recursion error in the token's place. The conversion is
    // `RecursionLimitExceeded.into()` (span `0..0`), matching Logos's
    // `extras.check().map_err(Into::into)` byte-for-byte — deliberately not
    // `bad_state(span, e)`, which is the *increase-bracket* handler's error
    // (and stays inside `increase_recursion!`). Below the default limit the
    // check always passes, so every existing stream is unchanged.
    macro_rules! finish {
      ($this:ident, $token:expr) => {
        match $this.state.check() {
          Ok(()) => Ok($token),
          Err(e) => Err($this.over_recursion_limit(e)),
        }
      };
    }

    // Emit a single-byte punctuation token. `token_start` is the loop-local
    // variable captured from the enclosing scope.
    macro_rules! emit_punct {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 1;
        $this.last_span = SimpleSpan::new($token_start, $this.cursor);
        finish!($this, $expr)
      }};
    }

    // Emit a two-byte punctuation token (`::`, `=>`).
    macro_rules! emit_punct2 {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 2;
        $this.last_span = SimpleSpan::new($token_start, $this.cursor);
        finish!($this, $expr)
      }};
    }

    macro_rules! decrease_recursion {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 1;
        $this.last_span = SimpleSpan::new($token_start, $this.cursor);
        $this.state_mut().decrease();
        finish!($this, $expr)
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
      match b0 {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
          let len = scan_identifier(bytes);
          self.cursor += len;
          self.last_span = SimpleSpan::new(token_start, self.cursor);
          let ident = self.src.slice(&token_start..&(token_start + len))?;
          return Some(finish!(self, SyntacticToken::Identifier(ident)));
        }
        // Radix numbers (decimal / hex / binary / octal, plus decimal and hex
        // floats) are delegated whole to the focused `NumberToken` grammar. `-`
        // joins them: a `-` directly followed by a digit or `.` is the sign of a
        // negative literal (`-5` -> `Decimal("-5")`, `-.5` -> a
        // missing-integer-part float), while a bare `-` is `Minus`. Logos
        // resolves that by longest match; the fast path cannot without
        // re-deriving the number grammar, so `-` always delegates and
        // `NumberToken` returns the correct single token either way (its
        // `#[token("-")] Minus` arm covers the bare operator).
        b'0'..=b'9' | b'-' => match scan_number(bytes) {
          // Valid *decimal* int/float: emit inline, byte-identical to what the
          // delegated grammar produces (`NumberToken::Decimal`/`Float` map to
          // `LitInt`/`LitFloat::Decimal`), skipping the Logos round-trip.
          // `finish!` applies the same per-token recursion check the ident fast
          // path uses, so deep-nesting behaviour matches. Everything the scanner
          // refuses — radix `0x`/`0b`/`0o`, hex floats, a bare `-` (`Minus`), a
          // `.`-led / empty-integer float, leading zeros, illegal suffixes —
          // returns `None` and delegates, preserving parity for those cases.
          Some((NumberKind::Int, len)) => {
            self.cursor += len;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            let slice = self.src.slice(&token_start..&(token_start + len))?;
            return Some(finish!(
              self,
              SyntacticToken::LitInt(LitInt::Decimal(slice))
            ));
          }
          Some((NumberKind::Float, len)) => {
            self.cursor += len;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            let slice = self.src.slice(&token_start..&(token_start + len))?;
            return Some(finish!(
              self,
              SyntacticToken::LitFloat(LitFloat::Decimal(slice))
            ));
          }
          None => return self.delegate_number_to_logos(token_start),
        },
        b'*' => return Some(emit_punct!(self: token_start, SyntacticToken::Asterisk)),
        b'&' => return Some(emit_punct!(self: token_start, SyntacticToken::Ampersand)),
        b'@' => return Some(emit_punct!(self: token_start, SyntacticToken::At)),
        b'$' => return Some(emit_punct!(self: token_start, SyntacticToken::Dollar)),
        b'!' => return Some(emit_punct!(self: token_start, SyntacticToken::Bang)),
        b'|' => return Some(emit_punct!(self: token_start, SyntacticToken::Pipe)),
        b'+' => return Some(emit_punct!(self: token_start, SyntacticToken::Plus)),
        b'(' => return Some(increase_recursion!(self: token_start, SyntacticToken::LParen)),
        b'[' => return Some(increase_recursion!(self: token_start, SyntacticToken::LBracket)),
        b'{' => return Some(increase_recursion!(self: token_start, SyntacticToken::LBrace)),
        b'<' => return Some(increase_recursion!(self: token_start, SyntacticToken::LAngle)),
        b')' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RParen)),
        b']' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RBracket)),
        b'}' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RBrace)),
        b'>' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RAngle)),
        // Two-byte punctuation: peek one byte so `::`/`=>` win over the
        // single-byte `:`/`=`, replicating Logos's longest match.
        b':' => {
          if bytes.get(1) == Some(&b':') {
            return Some(emit_punct2!(self: token_start, SyntacticToken::PathSeparator));
          }
          return Some(emit_punct!(self: token_start, SyntacticToken::Colon));
        }
        b'=' => {
          if bytes.get(1) == Some(&b'>') {
            return Some(emit_punct2!(self: token_start, SyntacticToken::FatArrow));
          }
          return Some(emit_punct!(self: token_start, SyntacticToken::Equal));
        }
        // Spread / unterminated-spread / `.`-led float: load b1 lazily — only
        // paid when b0 == b'.'.
        b'.' => {
          if bytes.starts_with(b"...") {
            self.cursor += 3;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            return Some(finish!(self, SyntacticToken::Spread));
          }
          if bytes.starts_with(b"..") {
            self.cursor += 2;
            let span = SimpleSpan::new(token_start, self.cursor);
            self.last_span = span;
            self.last_error_span = Some(span);
            let err = LexerErrors::<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>::unterminated_spread_operator(span);
            return Some(Err(err));
          }
          // `.` immediately followed by a digit (e.g. `.5`) is a decimal Float
          // literal missing its integer part, not a spread operator — the
          // focused `NumberToken`'s Float regex (`-?(frac)(exp)?`) matches it
          // and reports the exact "missing integer part" error (plus any chained
          // suffix error), so hand the whole token to it rather than
          // mis-emitting an unterminated-spread-operator error.
          if matches!(bytes.get(1), Some(b'0'..=b'9')) {
            return self.delegate_number_to_logos(token_start);
          }
          // A lone `.` (or `.` before any non-digit) is an unterminated spread
          // operator — the same error the full grammar's `#[token(".")]` arm
          // built, emitted inline here for whatever `Char` this source uses.
          self.cursor += 1;
          let span = SimpleSpan::new(token_start, self.cursor);
          self.last_span = span;
          self.last_error_span = Some(span);
          let err = LexerErrors::<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>::unterminated_spread_operator(span);
          return Some(Err(err));
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
              return Some(finish!(self, SyntacticToken::LitBlockStr(block)));
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
              return Some(finish!(
                self,
                SyntacticToken::LitInlineStr(LitInlineStr::Plain(LitPlainStr::new(slice)))
              ));
            }
            None => {
              // Lone `"` at end of input — an unterminated inline string.
              // Route it through the inline string sub-lexer so the
              // `unterminated_inline_string` error is built for whatever `Char`
              // this source uses.
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
                return Some(finish!(self, SyntacticToken::LitInlineStr(inline)));
              }
              // Any anomaly delegates the whole token to the inline string
              // sub-lexer (`string_lexer::inline`) rather than re-deriving the
              // error here.
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
        // Logos `default_error` (`graphqlx/handlers/{str,slice}.rs`): the char,
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
      }
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
  /// (under-the-limit) error construction is not inlined into the hot `lex()`
  /// at each of its emission sites — the fast path is then just the inlined
  /// `state.check()` branch. Mirrors the GraphQL lexer's helper of the same name.
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
  /// increase-bracket path (`increase_recursion!`): `LexerError::bad_state(span,
  /// e)`, the bracket handler's own error shape, rather than the plain
  /// `RecursionLimitExceeded.into()` the post-token `finish!` gate uses. Kept
  /// `#[cold]` + `#[inline(never)]` for the same reason as
  /// [`Self::over_recursion_limit`].
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

  /// Delegate the malformed / sign-ambiguous number opening at `token_start`
  /// (== `self.cursor`, prior to any mutation for this token) to the focused
  /// [`NumberToken`] grammar — the number-only slice of the full grammar's
  /// `#[regex]`/`#[token]` arms, calling the same frozen handlers — rather than
  /// rebuilding the whole `SyntacticToken` grammar.
  ///
  /// The number, `-`, and `.`-led-float arms route here. GraphQLx has no
  /// valid-number fast path, so a *valid* literal is produced by the delegated
  /// grammar too (mapped through [`SyntacticToken::from`]); a bare `-` before a
  /// non-digit returns `NumberToken::Minus` -> `SyntacticToken::Minus`; every
  /// anomaly (leading `0x`/`0o`/`0b` with no digits, an illegal suffix, an empty
  /// fraction/exponent, a `.`-led float missing its integer part, ...) returns
  /// the exact error. `NumberToken`'s `Error` is this lexer's error type, so the
  /// [`Delegated::Error`] arm needs no conversion.
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
  /// Skip the single-byte trivia class (space, tab, CR, LF, comma) at the
  /// cursor. Comment bodies and the UTF-8 BOM are handled in the dispatch loop
  /// so this stays a single SIMD scan.
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
