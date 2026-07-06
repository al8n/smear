//! The GraphQLx syntactic lexer: a SIMD-accelerated, streaming, single-pass
//! lexer (hybrid — SIMD fast paths + internal Logos slow-path delegation).
//! This is the GraphQLx `SyntacticLexer`.
//!
//! This mirrors the GraphQL SIMD lexer (`crate::graphql::simd`) and shares its
//! dialect-agnostic primitives through `crate::simd_common`: trivia, identifier,
//! and valid-string scanning (inline and block, including block-string
//! normalization) are SIMD-fast-pathed. The cold paths are handled without the
//! full grammar: malformed / sign-ambiguous numbers delegate to the focused
//! `NumberToken` grammar (see `delegate_number_to_logos`), malformed strings to
//! `string_lexer` (see `delegate_string_error`), the unterminated-spread error
//! is emitted inline, and the unknown-byte error is hand-rolled inline in
//! [`lex`](tokit::Lexer::lex). Because the fast path never constructs a source-typed
//! error itself, parity with the pre-SIMD lexer holds by construction.
//!
//! GraphQLx differs from GraphQL in the *dispatch*, not the architecture:
//!
//! - Angle brackets `<`/`>` are `LAngle`/`RAngle` and nest recursion (generics
//!   like `Box<T>`), so they join `{}`/`[]`/`()` as recursion-affecting
//!   punctuation.
//! - `::` (`PathSeparator`) and `=>` (`FatArrow`) are two-byte tokens, peeled
//!   apart from the single-byte `:` (`Colon`) and `=` (`Equal`) by peeking one
//!   byte — the same longest match Logos performs.
//! - `+` is always `Plus`, but `-` is a number *sign* whenever a digit or `.`
//!   directly follows it (`-5` lexes as `Decimal("-5")`, `-.5` as a float), so
//!   `-` is delegated to `NumberToken` alongside the digits rather than
//!   fast-pathed as a bare `Minus` — `NumberToken`'s `#[token("-")] Minus` arm
//!   resolves a `-` before any other byte back to the operator by longest match.
//! - Numbers are radix-prefixed (decimal / hex / binary / octal, plus decimal
//!   and hex floats) and always delegated to `NumberToken` — never hand-rolled.

use tokit::{
  Lexer, SimpleSpan, Slice, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
  utils::CharLen,
};

use crate::{
  LitBlockStr, LitComplexBlockStr, LitComplexInlineStr, LitInlineStr, LitPlainStr,
  error::{BadStateError, UnterminatedSpreadOperatorError},
  graphqlx::{
    ast::number::NumberToken,
    error::{LexerError, LexerErrorData, LexerErrors},
    syntactic::SyntacticToken,
  },
  skip_block_str_from_bytes, skip_inline_str_simd,
  string_lexer::DelegateStringError,
};

#[cfg(test)]
mod tests;

#[cfg(test)]
mod error_parity_tests;

#[cfg(test)]
mod bump_parity_tests;

#[cfg(test)]
mod trait_parity_tests;

use crate::simd_common::{Delegated, memchr_newline, scan_identifier, skip_ws_and_comma};

// Re-exported so the public `graphqlx::simd::{AsBytes, ScanSource}` paths and
// `DEFAULT_RECURSION_LIMIT` stay stable, matching `graphql::simd`, now that
// these dialect-agnostic items live in `crate::simd_common`.
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
  // Concrete associated types (formerly projected through
  // `LogosLexer<SyntacticToken>`, now deleted). These are exactly what those
  // projections resolved to under the old `LogosLexer<..>: Lexer<..>` bound.
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
        b'0'..=b'9' | b'-' => return self.delegate_number_to_logos(token_start),
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
  // shape; as with the former `delegate_to_logos`, clippy can't see through the
  // associated-type projections, so silence its type-complexity lint here.
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
