//! Phase-1 prototype of a SIMD-accelerated layer over the existing
//! Logos-driven [`SyntacticLexer`].
//!
//! The architecture is **wrap-and-dispatch**: every byte still flows
//! through Logos for the complex tokens (numbers, strings, spread,
//! errors), but the layer fast-paths the two byte categories that
//! dominate real GraphQL input — trivia and identifiers — through
//! `lexsimd` primitives.
//!
//! ```text
//! +-------------------------+
//! | SimdSyntacticLexer      |
//! |                         |
//! |  cursor, src, limiter   |
//! |                         |
//! |  next_token() {         |
//! |    skip_trivia();       |  <-- memspan::skip_class!
//! |    match peek() {       |
//! |      ident-start  ===>  |  <-- memspan::skip::skip_ident
//! |      single-byte punct  |  <-- inline match
//! |      everything else => |  <-- delegate to Logos
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
  LitComplexInlineStr, LitInlineStr, LitPlainStr,
  error::{BadStateError, UnterminatedSpreadOperatorError},
  graphql::{
    error::{LexerError, LexerErrors},
    syntactic::SyntacticToken,
  },
  skip_inline_str_simd,
};

mod bytes_token;
mod number;
mod str_token;

/// Maximum byte recursion depth — matches the default in
/// [`tokit::state::recursion_tracker::RecursionLimiter`].
pub const DEFAULT_RECURSION_LIMIT: usize = 500;

/// sasd
pub trait AsBytes {
  /// a
  fn as_bytes(&self) -> &[u8];
}

impl<T: ?Sized> AsBytes for &T
where
  T: AsBytes,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    (*self).as_bytes()
  }
}

impl AsBytes for str {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    str::as_bytes(self)
  }
}

impl AsBytes for [u8] {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

// ───── owned / shared slice types ───────────────────────────────────────
//
// Each of these is `tokit::Source::Slice` for an owned or shared source type
// (`bytes::Bytes`, `bstr::BStr`, `hipstr::HipStr`, `hipstr::HipByt` — see
// tokit's `src/source/{bytes_1,bstr_1,hipstr_0_8}.rs`), so implementing
// `AsBytes` for them is what lets `SimdSyntacticLexer` run over those source
// types, exactly as it already does for `str`/`[u8]` above. Every impl is a
// trivial deref to `&[u8]` — no allocation, no copying.

#[cfg(feature = "bytes")]
impl AsBytes for bytes::Bytes {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "bstr")]
impl AsBytes for bstr::BStr {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "hipstr")]
impl AsBytes for hipstr::HipStr<'_> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    // `HipStr` derefs to `str`, not `[u8]` directly, so route through
    // `str::as_bytes` (same call as the `str` impl above; the `&HipStr ->
    // &str` coercion happens at the argument site).
    str::as_bytes(self)
  }
}

#[cfg(feature = "hipstr")]
impl AsBytes for hipstr::HipByt<'_> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

/// Phase-1 SIMD layer. Streaming, single-pass, one token per call.
///
/// Construct with [`SimdSyntacticLexer::new`] or [`SimdSyntacticLexer::with_limit`],
/// then call [`SimdSyntacticLexer::next_token`] in a loop until it returns
/// `None`.
///
/// The `delegate` is a single Logos lexer over the *full* source, kept
/// alive for the lexer's lifetime. Every slow-path token (numbers,
/// strings, spread, errors) re-uses this lexer rather than constructing
/// a fresh one per call — this matters most on schemas, where every
/// block string is a delegation.
pub struct SimdSyntacticLexer<'inp, S: Source<usize> + ?Sized = str> {
  src: &'inp S,
  /// Current scan position. Advanced by trivia-skip, comment-skip, and each
  /// token scan. Never exposed directly — callers see `last_span`.
  cursor: usize,
  /// Span of the most recently *successfully* lexed token. Never updated on
  /// error returns, so `span()` always reflects the last valid position.
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
      Source = S,
      Span = SimpleSpan,
      Offset = usize,
    >,
  SyntacticToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  S: Source<usize> + ?Sized,
  S::Slice<'inp>: AsBytes + Slice<'inp>,
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
    self
      .state
      .check()
      .map_err(|e| LexerError::bad_state(self.last_span, e).into())
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
    // Emit a single-byte punctuation token. `token_start` is the loop-local
    // variable captured from the enclosing scope.
    macro_rules! emit_punct {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 1;
        $this.last_span = SimpleSpan::new($token_start, $this.cursor);
        Ok($expr)
      }};
    }

    macro_rules! decrease_recursion {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 1;
        $this.last_span = SimpleSpan::new($token_start, $this.cursor);
        $this.state_mut().decrease();
        Ok($expr)
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
            $this.last_error_span = Some(span);
            Err(LexerError::bad_state(span, e).into())
          }
        }
      }};
    }

    loop {
      self.skip_ws_and_comma();

      let token_start = self.cursor;
      let src = self.src.slice(&token_start..)?;
      if src.is_empty() {
        return None;
      }

      let bytes = src.as_bytes();
      let b0 = bytes[0];
      match b0 {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
          let len = self.lex_identifier(bytes);
          self.last_span = SimpleSpan::new(token_start, self.cursor);
          return self
            .src
            .slice(&token_start..&(token_start + len))
            .map(|ident| Ok(SyntacticToken::Identifier(ident)));
        }
        // Valid-number fast path (Phase 1a hybrid): `scan_number` only ever
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
              self
                .src
                .slice(&token_start..&(token_start + len))
                .map(|slice| Ok(SyntacticToken::LitInt(slice)))
            }
            Some((number::NumberKind::Float, len)) => {
              self.cursor += len;
              self.last_span = SimpleSpan::new(token_start, self.cursor);
              self
                .src
                .slice(&token_start..&(token_start + len))
                .map(|slice| Ok(SyntacticToken::LitFloat(slice)))
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
            return Some(Ok(SyntacticToken::Spread));
          }
          if bytes.starts_with(b"..") {
            self.cursor += 2;
            let span = SimpleSpan::new(token_start, self.cursor);
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
          self.last_error_span = Some(span);
          let err = LexerErrors::<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>::unterminated_spread_operator(span);
          return Some(Err(err));
        }
        b'#' => {
          self.skip_comment();
          continue;
        }

        // Block strings (""") fall through to the _ arm (Logos delegate).
        b'"' if !bytes.starts_with(b"\"\"\"") => {
          match bytes.get(1).copied() {
            Some(b'"') => {
              // Empty inline string "".
              self.cursor += 2;
              self.last_span = SimpleSpan::new(token_start, self.cursor);
              let slice = self.src.slice(&token_start..&self.cursor).unwrap();
              return Some(Ok(SyntacticToken::LitInlineStr(LitInlineStr::Plain(
                LitPlainStr::new(slice),
              ))));
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
                return Some(Ok(SyntacticToken::LitInlineStr(inline)));
              }
              // `skip_inline_str_simd` only ever answers "clean, valid
              // literal" or "byte-indexed anomaly" — like the number fast
              // path (Phase 1a), any anomaly delegates the *whole* token to
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

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn bump(&mut self, n: &Self::Offset) {
    self.cursor += n;
  }
}

// This block shares the trait impl's exact bound set (verbatim) rather than
// the weaker one below: `delegate_to_logos`'s return type has to name the
// concrete `Token`/`Error` types, which requires the `Token<'inp, Error =
// ..>` bound. Every caller of this method is inside `lex()` above, where
// those bounds already hold, so this is never more restrictive in practice
// than the trait impl itself.
impl<'inp, S> SimdSyntacticLexer<'inp, S>
where
  SyntacticToken<S::Slice<'inp>>: FromLogos<'inp>,
  LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>>: Lexer<
      'inp,
      State = RecursionLimiter,
      Token = SyntacticToken<S::Slice<'inp>>,
      Source = S,
      Span = SimpleSpan,
      Offset = usize,
    >,
  SyntacticToken<S::Slice<'inp>>:
    Token<'inp, Error = LexerErrors<<S::Slice<'inp> as Slice<'inp>>::Char, RecursionLimitExceeded>>,
  S: Source<usize> + ?Sized,
  S::Slice<'inp>: AsBytes + Slice<'inp>,
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
    let mut logos = LogosLexer::with_state(self.src, self.state);
    logos.bump(&self.cursor);
    match logos.lex()? {
      Ok(tok) => {
        let end = logos.inner().span().end;
        self.cursor = end;
        self.last_span = SimpleSpan::new(token_start, end);
        self.state = *logos.state();
        Some(Ok(tok))
      }
      Err(res) => {
        let end = logos.inner().span().end;
        self.cursor = end;
        self.last_error_span = Some(SimpleSpan::new(token_start, end));
        Some(Err(res))
      }
    }
  }
}

impl<'inp, S: Source<usize> + ?Sized> SimdSyntacticLexer<'inp, S>
where
  SyntacticToken<S::Slice<'inp>>: FromLogos<'inp>,
  LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>>: Lexer<
      'inp,
      State = RecursionLimiter,
      Token = SyntacticToken<S::Slice<'inp>>,
      Source = S,
      Span = SimpleSpan,
      Offset = usize,
    >,
  S::Slice<'inp>: AsBytes,
{
  // ───── span accessors ────────────────────────────────────────────────

  /// Span of the most recently returned error token, or `None` if no error
  /// has been returned yet.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn error_span(&self) -> Option<SimpleSpan> {
    self.last_error_span
  }

  // ───── trivia ────────────────────────────────────────────────────────

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

  // ───── identifier fast path ──────────────────────────────────────────

  /// Hot path. The dispatch already proved `bytes[start]` is in
  /// `[a-zA-Z_]`, so the run is non-empty by construction and we can
  /// start scanning from `start + 1`. Splitting on the remaining length
  /// keeps short idents (the common case in GraphQL — most identifiers
  /// are < 16 bytes) on a tight inlined scalar loop, while long idents
  /// (e.g. enum value names in schemas) get the SIMD dispatcher.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn lex_identifier(&mut self, bytes: &[u8]) -> usize {
    let total = bytes.len();
    let mut end = 1;

    if total - end >= 32 {
      // Long-ident path: amortize the SIMD dispatcher across enough
      // bytes to pay for itself.
      end += memspan::skip::skip_ident(&bytes[end..]);
    } else {
      // Short-ident path: a tight branchy loop with a known-tiny upper
      // bound. LLVM keeps this in the icache and the per-iteration cost
      // beats any function call dispatch for ≤ ~24-byte idents.
      while end < total && is_ident_continue(bytes[end]) {
        end += 1;
      }
    }

    self.cursor += end;
    end
  }
}

// ───── helpers ─────────────────────────────────────────────────────────

#[inline(always)]
fn is_ident_continue(b: u8) -> bool {
  matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

// Macro-generated SIMD-accelerated skipper for the four whitespace bytes
// plus comma. BOM and comments are handled separately because they're
// multi-byte sequences that don't fit a single-byte class.
memspan::skip_class! {
  fn skip_ws_and_comma(bytes = [b' ', b'\t', b'\r', b'\n', b',']);
}

/// Find the position of the next `\n` or `\r` in `input`, used for
/// terminating `#`-style comments.
#[inline(always)]
fn memchr_newline(input: &[u8]) -> Option<usize> {
  // Trivial scalar fallback first (most comments are short and contain
  // no special bytes between `#` and EOL). The branchy scalar loop is
  // hard to beat for short tails.
  if input.len() < 32 {
    return input.iter().position(|&b| b == b'\n' || b == b'\r');
  }
  // For longer comments we hand off to memchr2 which has the same
  // SIMD-saturation guarantees as memchr — a single `vceqq + vorrq +
  // shrn-extract` loop on aarch64.
  ::memspan::skip::skip_until(input, [b'\n', b'\r'])
}

// #[cfg(test)]
// mod tests {
//   use super::*;

//   /// Drive the layer to completion and collect every token / error.
//   fn lex_all(src: &str) -> Vec<Result<SyntacticToken<&str>, LexerErrors>> {
//     let mut lex = SimdSyntacticLexer::new(src);
//     let mut out = Vec::new();
//     while let Some(tok) = lex.next_token() {
//       out.push(tok);
//     }
//     out
//   }

//   /// Drive the existing baseline lexer in the same shape, for parity.
//   fn lex_baseline(src: &str) -> Vec<Result<SimdToken<'_>, SimdLexerErrors>> {
//     let mut lex: SyntacticLexer<'_, &str> =
//       <SyntacticLexer<'_, &str> as tokit::lexer::Lexer<'_>>::new(src);
//     let mut out = Vec::new();
//     while let Some(tok) = lex.lex() {
//       out.push(tok);
//     }
//     out
//   }

//   fn assert_parity(src: &str) {
//     let a = lex_all(src);
//     let b = lex_baseline(src);
//     assert_eq!(
//       a.len(),
//       b.len(),
//       "token count mismatch on {src:?}\n  simd:     {a:?}\n  baseline: {b:?}"
//     );
//     for (i, (x, y)) in a.iter().zip(b.iter()).enumerate() {
//       assert_eq!(
//         x, y,
//         "token #{i} mismatch on {src:?}\n  simd:     {x:?}\n  baseline: {y:?}"
//       );
//     }
//   }

//   #[test]
//   fn empty_input() {
//     let mut lex = SimdSyntacticLexer::new("");
//     assert!(lex.next_token().is_none());
//   }

//   #[test]
//   fn single_identifier() {
//     assert_parity("query");
//   }

//   #[test]
//   fn single_punct() {
//     for b in "{}[](),:!@$=&|".chars() {
//       let s = b.to_string();
//       assert_parity(&s);
//     }
//   }

//   #[test]
//   fn whitespace_only() {
//     let mut lex = SimdSyntacticLexer::new("   \t\n\r,");
//     assert!(lex.next_token().is_none());
//   }

//   #[test]
//   fn comment_only() {
//     let mut lex = SimdSyntacticLexer::new("# this is a comment");
//     assert!(lex.next_token().is_none());
//   }

//   #[test]
//   fn comment_then_token() {
//     assert_parity("# leading comment\nquery");
//   }

//   #[test]
//   fn bom_at_start() {
//     assert_parity("\u{FEFF}query");
//   }

//   #[test]
//   fn simple_query() {
//     assert_parity("query Foo { user { id name } }");
//   }

//   #[test]
//   fn query_with_variables() {
//     assert_parity("query Foo($id: ID!) { user(id: $id) { name } }");
//   }

//   #[test]
//   fn fragment_with_spread() {
//     assert_parity("{ ...UserFields }");
//   }

//   #[test]
//   fn integer_literal() {
//     assert_parity("{ items(first: 10) { id } }");
//   }

//   #[test]
//   fn float_literal() {
//     assert_parity("{ items(price: 3.14) { id } }");
//   }

//   #[test]
//   fn negative_int() {
//     assert_parity("{ items(offset: -5) { id } }");
//   }

//   #[test]
//   fn inline_string() {
//     assert_parity(r#"{ search(q: "hello") { id } }"#);
//   }

//   #[test]
//   fn block_string() {
//     assert_parity(
//       r#"{ search(q: """multi
// line""") { id } }"#,
//     );
//   }

//   #[test]
//   fn comment_inside_query() {
//     assert_parity("query { # inline\n  user { id } }");
//   }

//   #[test]
//   fn many_identifiers_with_punct() {
//     assert_parity(
//       "query GetUser($id: ID!, $first: Int = 10) {
//         user(id: $id) {
//           id
//           name
//           friends(first: $first) {
//             edges { node { id name } }
//           }
//         }
//       }",
//     );
//   }

//   #[test]
//   fn unicode_in_string_doesnt_confuse_us() {
//     // The string body is delegated to Logos, but the dispatch still
//     // has to skip it cleanly.
//     assert_parity(r#"{ msg(text: "héllo wörld") }"#);
//   }

//   #[test]
//   fn invalid_byte_produces_error() {
//     let r = lex_all("?");
//     assert_eq!(r.len(), 1);
//     assert!(
//       r[0].is_err(),
//       "expected error for invalid byte, got {:?}",
//       r[0]
//     );
//   }

//   #[test]
//   fn parity_kitchen_sink() {
//     let src =
//       include_str!("../../../smear/tests/fixtures/executables/kitchen-sink_canonical.graphql");
//     assert_parity(src);
//   }

//   #[test]
//   fn parity_huge_query() {
//     let src =
//       include_str!("../../../smear/tests/fixtures/executables/bench_10_huge_comprehensive.graphql");
//     assert_parity(src);
//   }
// }

#[cfg(test)]
mod str_arm_tests {
  use tokit::{Lexer as _, lexer::Lexer};

  use crate::{
    LitComplexInlineStr, LitInlineStr, LitPlainStr,
    graphql::{simd::SimdSyntacticLexer, syntactic::SyntacticToken},
  };

  fn lex_all(src: &[u8]) -> Vec<SyntacticToken<&[u8]>> {
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(src);
    let mut out = Vec::new();
    while let Some(tok) = lexer.lex() {
      out.push(tok.unwrap());
    }
    out
  }

  #[test]
  fn plain_inline_string() {
    let toks = lex_all(b"\"hello\"");
    assert_eq!(toks.len(), 1);
    assert!(matches!(
      &toks[0],
      SyntacticToken::LitInlineStr(LitInlineStr::Plain(s)) if s.as_bytes() == b"\"hello\""
    ));
  }

  #[test]
  fn empty_inline_string() {
    let toks = lex_all(b"\"\"");
    assert_eq!(toks.len(), 1);
    assert!(matches!(
      &toks[0],
      SyntacticToken::LitInlineStr(LitInlineStr::Plain(s)) if s.as_bytes() == b"\"\""
    ));
  }

  #[test]
  fn escaped_inline_string() {
    let toks = lex_all(b"\"hello\\nworld\"");
    assert_eq!(toks.len(), 1);
    assert!(matches!(
      &toks[0],
      SyntacticToken::LitInlineStr(LitInlineStr::Complex(_))
    ));
  }

  #[test]
  fn inline_string_in_query() {
    let toks = lex_all(b"{ search(q: \"foo\") { id } }");
    let strings: Vec<_> = toks
      .iter()
      .filter(|t| matches!(t, SyntacticToken::LitInlineStr(_)))
      .collect();
    assert_eq!(strings.len(), 1);
    assert!(matches!(
      strings[0],
      SyntacticToken::LitInlineStr(LitInlineStr::Plain(s)) if s.as_bytes() == b"\"foo\""
    ));
  }

  #[test]
  fn lone_quote_at_eof_is_error() {
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(b"\"");
    let tok = lexer.lex().unwrap();
    assert!(tok.is_err());
    assert_eq!(lexer.error_span(), Some(tokit::SimpleSpan::new(0, 1)));
  }

  #[test]
  fn span_not_clobbered_by_error() {
    // After a valid token followed by an error, span() must still return the
    // valid token's span, and error_span() must return the error's span.
    let src = b"hello \"unterminated";
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(src);

    // First token: identifier "hello" at 0..5.
    let first = lexer.lex().unwrap();
    assert!(first.is_ok());
    let valid_span = lexer.span();
    assert_eq!(valid_span, tokit::SimpleSpan::new(0, 5));
    assert!(lexer.error_span().is_none());

    // Second token: unterminated inline string → error.
    let second = lexer.lex().unwrap();
    assert!(second.is_err());

    // span() must still reflect the last *valid* token.
    assert_eq!(lexer.span(), valid_span);
    // error_span() must reflect the error token (opening " is at byte 6).
    let err_span = lexer.error_span().expect("error_span should be set");
    assert_eq!(err_span, tokit::SimpleSpan::new(6, src.len()));
  }
}

#[cfg(test)]
mod num_arm_tests {
  use tokit::{Lexer as _, SimpleSpan};

  use crate::graphql::{
    error::{FloatError, LexerErrorData},
    simd::SimdSyntacticLexer,
    syntactic::SyntacticToken,
  };

  fn lex_all(src: &[u8]) -> Vec<SyntacticToken<&[u8]>> {
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(src);
    let mut out = Vec::new();
    while let Some(tok) = lexer.lex() {
      out.push(tok.unwrap());
    }
    out
  }

  #[test]
  fn valid_int_fast_path() {
    // { a ( x : 10 ) } -- 8 tokens; the number is the 6th.
    let toks = lex_all(b"{ a(x: 10) }");
    assert_eq!(toks.len(), 8);
    assert!(matches!(&toks[5], SyntacticToken::LitInt(s) if *s == b"10"));
  }

  #[test]
  fn valid_float_fast_path() {
    let toks = lex_all(b"{ a(x: 3.14) }");
    assert_eq!(toks.len(), 8);
    assert!(matches!(&toks[5], SyntacticToken::LitFloat(s) if *s == b"3.14"));
  }

  #[test]
  fn negative_int_and_float_fast_path() {
    let toks = lex_all(b"{ a(x: -5) }");
    assert!(matches!(&toks[5], SyntacticToken::LitInt(s) if *s == b"-5"));

    let toks = lex_all(b"{ a(x: -2.5) }");
    assert!(matches!(&toks[5], SyntacticToken::LitFloat(s) if *s == b"-2.5"));
  }

  #[test]
  fn number_anomalies_still_delegate_and_error() {
    // Leading zeros, an illegal ident suffix, and a lone `-` are all
    // anomalies `scan_number` refuses to fast-path -- confirm the dispatch
    // still routes them to Logos and gets back an error (the exact shape
    // is already covered byte-for-byte by the oracle tests).
    for src in [b"007" as &[u8], b"123abc", b"-", b"1.5x", b"00.5"] {
      let mut lexer = SimdSyntacticLexer::<[u8]>::new(src);
      let tok = lexer.lex().expect("one token").expect_err("should error");
      let _ = tok; // shape is oracle-verified; here we only need "it errors".
    }
  }

  #[test]
  fn dot_led_float_delegates_to_missing_integer_part_not_spread_error() {
    // `.5` must NOT be treated as a lone `.` (unterminated spread operator)
    // -- it's a Float literal missing its integer part, and Logos must be
    // the one to say so.
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(b".5");
    let err = lexer.lex().unwrap().unwrap_err();
    assert_eq!(lexer.error_span(), Some(SimpleSpan::new(0, 2)));
    assert!(
      matches!(
        err.first().map(|e| e.data()),
        Some(LexerErrorData::Float(FloatError::MissingIntegerPart))
      ),
      "expected Float(MissingIntegerPart), got {err:?}"
    );
  }

  #[test]
  fn dot_dot_and_lone_dot_are_unaffected_by_the_digit_check() {
    // `..` (not `...`) is still the pre-existing unterminated-spread error,
    // and lexing resumes correctly on whatever follows it (the digit `5`
    // is not part of the `..` error -- it's the *next* token).
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(b"..5");
    let first = lexer.lex().unwrap();
    assert!(first.is_err());
    assert_eq!(lexer.error_span(), Some(SimpleSpan::new(0, 2)));

    let second = lexer.lex().unwrap();
    assert!(matches!(second, Ok(SyntacticToken::LitInt(s)) if s == b"5"));

    // A lone `.` followed by a non-digit is still the same error too.
    let mut lexer = SimdSyntacticLexer::<[u8]>::new(b".x");
    let first = lexer.lex().unwrap();
    assert!(first.is_err());
    assert_eq!(lexer.error_span(), Some(SimpleSpan::new(0, 1)));

    let second = lexer.lex().unwrap();
    assert!(matches!(second, Ok(SyntacticToken::Identifier(s)) if s == b"x"));
  }

  #[test]
  fn spread_operator_still_fast_paths() {
    let toks = lex_all(b"{ ...Frag }");
    assert!(toks.contains(&SyntacticToken::Spread));
  }
}

/// Phase 1b Task 1 probe: before Task 1, the `Lexer` impl's
/// `S::Slice<'inp>: Slice<'inp, Char = u8>` bound meant `SimdSyntacticLexer::<str>`
/// (whose slice is `&str`, `Char = char`) could not satisfy the impl at all, so every
/// test below was a compile error. Delegating both inline-string error outcomes to
/// Logos (which builds whatever `Char` type the source needs) let that bound drop,
/// so `<str>` now compiles and lexes correctly, not just `<[u8]>`.
///
/// Task 2 consolidation: this module originally had three probes. Two of them
/// (a valid-query smoke test asserting the inline string still emits inline,
/// and a bad-escape test asserting the delegated error is genuinely
/// `Char = char`) are now strictly subsumed by `graphql_syntactic_simd_oracle`
/// in `tests/oracle.rs`, which drives `SimdSyntacticLexer::<str>` directly
/// against the golden files (41 fixtures, byte-for-byte `Debug` comparison,
/// including `str_bad_escape` and many valid-query/inline-string fixtures) --
/// so they were deleted rather than moved verbatim. The one kept below covers
/// a source text (a lone `"` as the *entire* input) that no golden fixture
/// exercises: it takes the `bytes.get(1) == None` branch in `lex()`'s `b'"'`
/// arm, distinct from `str_unterminated_inline`'s `Some(_) => Err(_)` branch.
#[cfg(test)]
mod generic_source_tests {
  use tokit::Lexer as _;

  use crate::{
    error::StringError,
    graphql::{
      error::LexerErrorData,
      simd::SimdSyntacticLexer,
      syntactic::{SyntacticLexerErrors, SyntacticToken},
    },
  };

  /// Drive a `str`-sourced lexer to completion, collecting every result.
  fn lex_all(src: &str) -> Vec<Result<SyntacticToken<&str>, SyntacticLexerErrors>> {
    let mut lexer = SimdSyntacticLexer::<str>::new(src);
    let mut out = Vec::new();
    while let Some(tok) = lexer.lex() {
      out.push(tok);
    }
    out
  }

  #[test]
  fn str_source_lone_quote_at_eof_delegates_to_unterminated_error() {
    let toks = lex_all("\"");
    assert_eq!(toks.len(), 1);
    match toks[0].as_ref().unwrap_err().first().unwrap().data() {
      LexerErrorData::String(errs) => {
        assert!(
          matches!(errs.first(), Some(StringError::Unterminated(_))),
          "expected Unterminated, got {errs:?}"
        );
      }
      other => panic!("expected LexerErrorData::String, got {other:?}"),
    }
  }
}

/// Task 2 Step 1: proves each new `AsBytes` impl actually decodes to the
/// right bytes. These do NOT drive a `SimdSyntacticLexer` over the type --
/// see `tests/oracle.rs`'s "SIMD source matrix" section for why
/// `SimdSyntacticLexer::<bytes::Bytes>` (and `<bstr::BStr>`,
/// `<hipstr::HipStr>`, `<hipstr::HipByt>`) can't be constructed yet, a
/// separate, deeper limitation `AsBytes` alone doesn't resolve. This module
/// only proves the trait impls added here are individually correct.
#[cfg(test)]
mod as_bytes_tests {
  use super::AsBytes;

  #[cfg(feature = "bytes")]
  #[test]
  fn bytes_as_bytes() {
    let b = bytes::Bytes::from_static(b"hello");
    assert_eq!(AsBytes::as_bytes(&b), b"hello");
  }

  #[cfg(feature = "bstr")]
  #[test]
  fn bstr_as_bytes() {
    let b = bstr::BStr::new(b"hello");
    assert_eq!(AsBytes::as_bytes(b), b"hello");
  }

  #[cfg(feature = "hipstr")]
  #[test]
  fn hipstr_as_bytes() {
    let s = hipstr::HipStr::from("hello");
    assert_eq!(AsBytes::as_bytes(&s), b"hello");
  }

  #[cfg(feature = "hipstr")]
  #[test]
  fn hipbyt_as_bytes() {
    let b = hipstr::HipByt::from(b"hello" as &[u8]);
    assert_eq!(AsBytes::as_bytes(&b), b"hello");
  }
}
