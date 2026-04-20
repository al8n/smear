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
//! |    skip_trivia();       |  <-- lexsimd::skip_class!
//! |    match peek() {       |
//! |      ident-start  ===>  |  <-- lexsimd::skip::skip_ident
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
  error::BadStateError,
  graphql::{
    error::{LexerError, LexerErrors},
    syntactic::SyntacticToken,
  },
};

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
pub struct SimdSyntacticLexer<'inp, S: Source<usize> + ?Sized = str>
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
{
  src: &'inp S,
  span: SimpleSpan,
  state: RecursionLimiter,
  /// Persistent Logos lexer. Reused across delegations so we pay
  /// only a `bump(delta)` fast-forward instead of a fresh-lexer
  /// construction + full-scan on every slow-path token.
  delegate: LogosLexer<'inp, SyntacticToken<S::Slice<'inp>>>,
}

impl<'inp, S: Source<usize> + ?Sized> Lexer<'inp> for SimdSyntacticLexer<'inp, S>
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
  S: Source<usize>,
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
      span: SimpleSpan::const_new(0, 0),
      state,
      delegate: LogosLexer::with_state(src, state),
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn check(&self) -> Result<(), <Self::Token as tokit::Token<'inp>>::Error> {
    self
      .state
      .check()
      .map_err(|e| LexerError::bad_state(self.span, e).into())
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
    self.span
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn slice(&self) -> <Self::Source as Source<Self::Offset>>::Slice<'inp> {
    self.delegate.slice()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn lex(&mut self) -> Option<Result<Self::Token, <Self::Token as tokit::Token<'inp>>::Error>> {
    macro_rules! decrease_recursion {
      ($this:ident: $expr:expr) => {{
        $this.span.bump(&1);
        $this.state_mut().decrease();
        Ok($expr)
      }};
    }

    macro_rules! increase_recursion {
      ($this:ident: $expr:expr) => {{
        $this.span.bump(&1);
        $this.state_mut().increase();
        $this.check().map(|_| $expr).map_err(Into::into)
      }};
    }

    macro_rules! emit_punct {
      ($this:ident: $expr:expr) => {{
        $this.span.bump(&1);
        Ok($expr)
      }};
    }

    loop {
      // Skip trivia BEFORE peeking, so the match below sees the first
      // meaningful byte and the ident / punct fast paths fire for every
      // token, not just ones with no leading whitespace.
      self.skip_ws_and_comma();

      let cursor = self.span.end();
      let src = self.src.slice(&cursor..)?;
      if src.is_empty() {
        return None;
      }

      let bytes = src.as_bytes();
      let byte = bytes[0];
      match byte {
        // Identifier — by far the most common GraphQL token. The arm
        // body (`lex_identifier`) already knows the first byte is in
        // [a-zA-Z_], so it skips re-checking it.
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
          return self
            .src
            .slice(&cursor..&(cursor + self.lex_identifier(bytes)))
            .map(|ident| Ok(SyntacticToken::Identifier(ident)));
        }
        // Single-byte structural punctuation, listed inline so the
        // compiler folds the dispatch into the same jump table as the
        // ident range. No `Option` unwrap on the hot path.
        b'!' => return Some(emit_punct!(self: SyntacticToken::Bang)),
        b'&' => return Some(emit_punct!(self: SyntacticToken::Ampersand)),
        b'(' => return Some(increase_recursion!(self: SyntacticToken::LParen)),
        b')' => return Some(decrease_recursion!(self: SyntacticToken::RParen)),
        b':' => return Some(emit_punct!(self: SyntacticToken::Colon)),
        b'=' => return Some(emit_punct!(self: SyntacticToken::Equal)),
        b'@' => return Some(emit_punct!(self: SyntacticToken::At)),
        b'$' => return Some(emit_punct!(self: SyntacticToken::Dollar)),
        b'[' => return Some(increase_recursion!(self: SyntacticToken::LBracket)),
        b']' => return Some(decrease_recursion!(self: SyntacticToken::RBracket)),
        b'{' => return Some(increase_recursion!(self: SyntacticToken::LBrace)),
        b'}' => return Some(decrease_recursion!(self: SyntacticToken::RBrace)),
        b'|' => return Some(emit_punct!(self: SyntacticToken::Pipe)),
        // Comment — eat the rest of the line and re-loop.
        b'#' => {
          self.skip_comment();
          continue;
        }
        // BOM — only a real BOM if the next two bytes match. Non-BOM
        // bytes that happen to start with 0xEF (UTF-8 high planes) fall
        // through to the delegate.
        0xEF if bytes.starts_with(b"\xEF\xBB\xBF") => {
          self.span.bump(&3);
          continue;
        }
        // Slow path: numbers, strings, spread `...`, errors.
        // Fast-forward the persistent delegate by the bytes the SIMD
        // layer consumed since the last delegation, then hand off.
        _ => {
          let logos_cursor = self.delegate.inner().span().end;
          let delta = self.span.end() - logos_cursor;
          if delta != 0 {
            self.delegate.inner_mut().bump(delta);
          }
          *self.delegate.state_mut() = self.state;
          match self.delegate.lex()? {
            Ok(tok) => {
              self.span = self.delegate.inner().span().into();
              self.state = *self.delegate.state();
              return Some(Ok(tok));
            }
            Err(res) => {
              self.span = self.delegate.inner().span().into();
              return Some(Err(res.into()));
            }
          }
        }
      }
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn bump(&mut self, n: &Self::Offset) {
    self.span.bump(n);
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
  // ───── trivia ────────────────────────────────────────────────────────

  /// Skip the single-byte trivia class (space, tab, CR, LF, comma)
  /// at the cursor. Comment bodies and the UTF-8 BOM are handled in
  /// the dispatch loop so this stays a single SIMD scan with no per-call
  /// branchy interpretation overhead.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn skip_ws_and_comma(&mut self) {
    let Some(rest) = self.src.slice(self.span.end_ref()..) else {
      return;
    };
    let n = skip_ws_and_comma(rest.as_bytes());
    self.span.bump(&n);
  }

  /// Skip a `#…\n|\r` comment body. Cursor is positioned at the `#`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn skip_comment(&mut self) {
    let Some(rest) = self.src.slice(self.span.end_ref()..) else {
      return;
    };
    let len = match memchr_newline(rest.as_bytes()) {
      Some(n) => n,       // stop AT the newline; loop eats it next
      None => rest.len(), // unterminated -> EOF
    };
    self.span.bump(&len);
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
      end += lexsimd::skip::skip_ident(&bytes[end..]);
    } else {
      // Short-ident path: a tight branchy loop with a known-tiny upper
      // bound. LLVM keeps this in the icache and the per-iteration cost
      // beats any function call dispatch for ≤ ~24-byte idents.
      while end < total && is_ident_continue(bytes[end]) {
        end += 1;
      }
    }

    self.span.bump(&end);
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
lexsimd::skip_class! {
  fn skip_ws_and_comma, bytes = [b' ', b'\t', b'\r', b'\n', b','];
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
  ::lexsimd::skip::skip_until(input, [b'\n', b'\r'])
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
