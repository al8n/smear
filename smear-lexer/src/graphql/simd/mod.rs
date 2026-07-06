//! The GraphQL syntactic lexer: a SIMD-accelerated, streaming, single-pass
//! lexer. It fast-paths the byte categories that dominate real input — trivia,
//! identifiers, valid numbers, and valid strings (inline AND block, including
//! block-string normalization) — and delegates only the cold error/anomaly
//! paths (malformed numbers, string-error diagnostics, unterminated literals)
//! to an internal Logos lexer (the hybrid design). This is the `SyntacticLexer`
//! the rest of the workspace uses.
//!
//! ```text
//! +-------------------------+
//! | SimdSyntacticLexer      |
//! |                         |
//! |  cursor, src, limiter   |
//! |                         |
//! |  lex() {                |
//! |    skip_trivia();       |  <-- memspan::skip_class!
//! |    match peek() {       |
//! |      ident-start  ===>  |  <-- memspan::skip::skip_ident
//! |      digit / '-'  ===>  |  <-- number::scan_number (valid => fast; else delegate)
//! |      '"' / '"""'  ===>  |  <-- SIMD string/block scan (valid => fast; else delegate)
//! |      single-byte punct  |  <-- inline match
//! |      anomaly/error ===> |  <-- delegate to Logos
//! |    }                    |
//! |  }                      |
//! +-------------------------+
//! ```
//!
//! The token mix profiler showed 85-93% of bytes in real **executable
//! queries** are trivia or identifier — that's the opportunity. Schemas
//! are dominated by block strings (~60% of bytes), which Logos still
//! handles, so this layer was expected to give only a small gain there.
//! Measured throughput says otherwise: this layer is ~1.9-2x faster than
//! Logos on both queries *and* schemas alike (`count`, `sample_size(30)`;
//! e.g. `query/huge_4.2KB` 855 MiB/s -> 1.69 GiB/s, `schema/gmx_69KB`
//! 1.40 GiB/s -> 2.73 GiB/s). See the `lex_baseline` bench output for full
//! before/after numbers.

use tokit::{
  Lexer, SimpleSpan, Slice, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use crate::{
  LitBlockStr, LitComplexBlockStr, LitComplexInlineStr, LitInlineStr, LitPlainStr,
  error::{BadStateError, UnterminatedSpreadOperatorError},
  graphql::{
    error::{LexerError, LexerErrors},
    syntactic::SyntacticToken,
  },
  skip_block_str_from_bytes, skip_inline_str_simd,
};

mod number;

use crate::simd_common::{Delegated, memchr_newline, scan_identifier, skip_ws_and_comma};

// Re-exported so the public `graphql::simd::{AsBytes, ScanSource}` paths and
// `DEFAULT_RECURSION_LIMIT` stay stable now that these dialect-agnostic items
// live in `crate::simd_common`.
pub use crate::simd_common::{AsBytes, DEFAULT_RECURSION_LIMIT, ScanSource};

/// SIMD layer over the GraphQL syntactic lexer. Streaming, single-pass, one
/// token per call.
///
/// Construct with [`SimdSyntacticLexer::new`] or
/// [`SimdSyntacticLexer::with_state`], then call [`lex`](Lexer::lex) in a
/// loop until it returns `None`.
///
/// Every slow-path token (numbers, strings, spread, errors) is handled by
/// constructing a fresh `LogosLexer` over the full source and bumping it to
/// the current cursor, rather than by reusing one lexer across calls — see
/// `delegate_to_logos`.
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
  SyntacticToken<S::Slice<'inp>>: FromLogos<'inp>,
  LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>>: Lexer<
      'inp,
      State = RecursionLimiter,
      Token = SyntacticToken<S::Slice<'inp>>,
      Source = <S as ScanSource>::ScanPrimitive,
      Span = SimpleSpan,
      Offset = usize,
    >,
  SyntacticToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  S: ScanSource + ?Sized,
  S::Slice<'inp>: AsBytes,
{
  type State = <LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>> as Lexer<'inp>>::State;

  type Source = S;

  type Token = <LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>> as Lexer<'inp>>::Token;

  type Span = <LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>> as Lexer<'inp>>::Span;

  type Offset = <LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>> as Lexer<'inp>>::Offset;

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
          Err(e) => {
            $this.last_error_span = Some($this.last_span);
            Err(e.into())
          }
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
            $this.last_error_span = Some(span);
            Err(LexerError::bad_state(span, e).into())
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
        // Valid-number fast path: `scan_number` only ever
        // answers "clean, valid literal of length N" or "anomaly" — it never
        // constructs an error. A clean literal is emitted directly (no
        // Logos involved); any anomaly (leading zeros, empty frac/exponent,
        // an illegal suffix, a lone `-`, ...) delegates the *whole* token to
        // Logos, which already knows how to build the exact error. Because
        // every error still originates from Logos, oracle parity holds
        // without re-deriving any error logic here.
        b'0'..=b'9' | b'-' => {
          return match number::scan_number(bytes) {
            Some((number::NumberKind::Int, len)) => {
              self.cursor += len;
              self.last_span = SimpleSpan::new(token_start, self.cursor);
              let slice = self.src.slice(&token_start..&(token_start + len))?;
              Some(finish!(self, SyntacticToken::LitInt(slice)))
            }
            Some((number::NumberKind::Float, len)) => {
              self.cursor += len;
              self.last_span = SimpleSpan::new(token_start, self.cursor);
              let slice = self.src.slice(&token_start..&(token_start + len))?;
              Some(finish!(self, SyntacticToken::LitFloat(slice)))
            }
            None => self.delegate_to_logos(token_start),
          };
        }
        b'!' => return Some(emit_punct!(self: token_start, SyntacticToken::Bang)),
        b'&' => return Some(emit_punct!(self: token_start, SyntacticToken::Ampersand)),
        b'(' => return Some(increase_recursion!(self: token_start, SyntacticToken::LParen)),
        b')' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RParen)),
        b':' => return Some(emit_punct!(self: token_start, SyntacticToken::Colon)),
        b'=' => return Some(emit_punct!(self: token_start, SyntacticToken::Equal)),
        b'@' => return Some(emit_punct!(self: token_start, SyntacticToken::At)),
        b'$' => return Some(emit_punct!(self: token_start, SyntacticToken::Dollar)),
        b'[' => return Some(increase_recursion!(self: token_start, SyntacticToken::LBracket)),
        b']' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RBracket)),
        b'{' => return Some(increase_recursion!(self: token_start, SyntacticToken::LBrace)),
        b'}' => return Some(decrease_recursion!(self: token_start, SyntacticToken::RBrace)),
        b'|' => return Some(emit_punct!(self: token_start, SyntacticToken::Pipe)),
        // Spread / unterminated-spread: load b1/b2 lazily — only paid when b0 == b'.'.
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
          // `.` immediately followed by a digit (e.g. `.5`) is a Float
          // literal missing its integer part, not a spread operator —
          // Logos's Float regex (`-?(frac)(exp)?`) wins the longest-match
          // race here and reports the exact "missing integer part" error
          // (plus any chained suffix error), so hand the whole token to it
          // rather than mis-emitting an unterminated-spread-operator error.
          if matches!(bytes.get(1), Some(b'0'..=b'9')) {
            return self.delegate_to_logos(token_start);
          }
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
            Err(_) => return self.delegate_to_logos(token_start),
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
              // Lone `"` at end of input — unterminated. Delegate to Logos
              // so the error is built generically for whatever `Char` this
              // source uses, instead of hand-rolling it here (hybrid: the
              // fast path never constructs source-typed errors itself).
              return self.delegate_to_logos(token_start);
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
              // `skip_inline_str_simd` only ever answers "clean, valid
              // literal" or "byte-indexed anomaly" — like the number fast
              // path, any anomaly delegates the *whole* token to
              // Logos rather than re-deriving the error here. This is also
              // what drops the `Char = u8` requirement: the only place that
              // ever built a `StringErrors<u8>` inline is gone.
              Err(_) => return self.delegate_to_logos(token_start),
            },
          }
        }
        0xEF if bytes.starts_with(b"\xEF\xBB\xBF") => {
          self.cursor += 3;
          continue;
        }
        _ => return self.delegate_to_logos(token_start),
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
// `delegate_to_logos` touches (it builds `SimpleSpan` directly and reads the
// wrapped Logos lexer's own span, never `Self::Span` or `.as_bytes()`) —
// rather than the far weaker one below: `delegate_to_logos`'s return type has
// to name the concrete `Token`/`Error` types, which requires the
// `Token<'inp, Error = ..>` bound. Every caller of this method is inside
// `lex()` above, where the trait impl's full bound set already holds, so
// this is never more restrictive in practice than the trait impl itself.
impl<'inp, S> SimdSyntacticLexer<'inp, S>
where
  SyntacticToken<S::Slice<'inp>>: FromLogos<'inp>,
  LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>>: Lexer<
      'inp,
      State = RecursionLimiter,
      Token = SyntacticToken<S::Slice<'inp>>,
      Source = <S as ScanSource>::ScanPrimitive,
      Offset = usize,
    >,
  SyntacticToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  S: ScanSource + ?Sized,
{
  /// Delegate the token starting at `token_start` (== `self.cursor`, prior
  /// to any mutation for this token) to the wrapped Logos lexer.
  ///
  /// This is the single, permanent slow-path fallback: the `_` arm in
  /// [`lex`](Lexer::lex) uses it for every byte none of the fast paths
  /// above claims, and the number/`.` fast paths use it for any anomaly
  /// they detect (leading zeros, an empty fraction/exponent, an illegal
  /// suffix, a lone `-`, a `.`-led float missing its integer part, ...).
  /// Logos re-derives the token — or the exact error — from scratch, so
  /// parity with the pre-SIMD lexer holds by construction: nothing here
  /// constructs an error itself.
  // The return type mirrors `Lexer::lex`'s own `Option<Result<Token,
  // Error>>` shape (see the trait impl above) — clippy can't see through
  // the associated-type projections to recognize the two are the same
  // complexity, so it flags this one as a bare function signature. A type
  // alias would need its own `?Sized` + `Source<usize>` bounds restating
  // the impl block's, which is more indirection than the signature it
  // replaces.
  #[allow(clippy::type_complexity)]
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn delegate_to_logos(
    &mut self,
    token_start: usize,
  ) -> Option<
    Result<
      SyntacticToken<S::Slice<'inp>>,
      LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>,
    >,
  > {
    match crate::simd_common::delegate_to_logos::<SyntacticToken<S::Slice<'inp>>>(
      self.src.scan_primitive(),
      self.cursor,
      self.state,
    )? {
      Delegated::Token { token, end, state } => {
        self.cursor = end;
        self.last_span = SimpleSpan::new(token_start, end);
        self.state = state;
        Some(Ok(token))
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
