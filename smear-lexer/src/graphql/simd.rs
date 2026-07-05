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
//! are dominated by block strings (~60% of bytes) which Logos still
//! handles, so this layer is expected to give ~negligible gain on schemas.
//! See the `lex_baseline` bench output for before/after numbers.

use tokit::{
  Lexer, SimpleSpan, Slice, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use crate::{
  LitComplexInlineStr, LitInlineStr, LitPlainStr,
  error::{BadStateError, StringError, StringErrors, UnterminatedSpreadOperatorError},
  graphql::{
    error::{LexerError, LexerErrorData, LexerErrors},
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
  S::Slice<'inp>: AsBytes + Slice<'inp, Char = u8>,
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
              // Lone `"` at end of input — unterminated.
              self.cursor += 1;
              let span = SimpleSpan::new(token_start, self.cursor);
              self.last_error_span = Some(span);
              let mut errs = StringErrors::default();
              errs.push(StringError::unterminated_inline_string());
              return Some(Err(LexerErrors::from(LexerError::new(
                span,
                LexerErrorData::String(errs),
              ))));
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
              Err((consumed, string_errs)) => {
                self.cursor += 1 + consumed;
                let span = SimpleSpan::new(token_start, self.cursor);
                self.last_error_span = Some(span);
                let err = LexerError::new(span, LexerErrorData::String(string_errs));
                return Some(Err(LexerErrors::from(err)));
              }
            },
          }
        }
        0xEF if bytes.starts_with(b"\xEF\xBB\xBF") => {
          self.cursor += 3;
          continue;
        }
        _ => {
          let mut logos = LogosLexer::with_state(self.src, self.state);
          logos.bump(&self.cursor);
          match logos.lex()? {
            Ok(tok) => {
              let end = logos.inner().span().end;
              self.cursor = end;
              self.last_span = SimpleSpan::new(token_start, end);
              self.state = *logos.state();
              return Some(Ok(tok));
            }
            Err(res) => {
              let end = logos.inner().span().end;
              self.cursor = end;
              self.last_error_span = Some(SimpleSpan::new(token_start, end));
              return Some(Err(res));
            }
          }
        }
      }
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn bump(&mut self, n: &Self::Offset) {
    self.cursor += n;
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
