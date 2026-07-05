//! SIMD-accelerated layer over the Logos-driven GraphQLx [`SyntacticLexer`].
//!
//! This mirrors the GraphQL SIMD lexer (`crate::graphql::simd`) and shares its
//! dialect-agnostic primitives through [`crate::simd_common`]: trivia and
//! identifier scanning are SIMD-fast-pathed, while numbers, strings, the spread
//! operator, and every error are delegated one token at a time to a fresh Logos
//! lexer. Because the fast path never constructs a source-typed error itself,
//! parity with the pre-SIMD lexer holds by construction.
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
//!   `-` is delegated alongside the digits rather than fast-pathed as a bare
//!   `Minus`.
//! - Numbers are radix-prefixed (decimal / hex / binary / octal, plus decimal
//!   and hex floats) and always delegated — never hand-rolled here.

use tokit::{
  Lexer, SimpleSpan, Slice, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use crate::{
  LitComplexInlineStr, LitInlineStr, LitPlainStr,
  error::BadStateError,
  graphqlx::{
    error::{LexerError, LexerErrors},
    syntactic::SyntacticToken,
  },
  skip_inline_str_simd,
};

#[cfg(test)]
mod tests;

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
/// Every slow-path token (numbers, strings, the spread operator, and errors)
/// re-uses a fresh Logos lexer over the *full* source via
/// [`crate::simd_common::delegate_to_logos`] rather than constructing a bespoke
/// scanner per call.
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

    // Emit a two-byte punctuation token (`::`, `=>`).
    macro_rules! emit_punct2 {
      ($this:ident: $token_start:expr, $expr:expr) => {{
        $this.cursor += 2;
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
          let len = scan_identifier(bytes);
          self.cursor += len;
          self.last_span = SimpleSpan::new(token_start, self.cursor);
          return self
            .src
            .slice(&token_start..&(token_start + len))
            .map(|ident| Ok(SyntacticToken::Identifier(ident)));
        }
        // Radix numbers (decimal / hex / binary / octal, plus decimal and hex
        // floats) are delegated whole to Logos. `-` joins them: a `-` directly
        // followed by a digit or `.` is the sign of a negative literal (`-5` ->
        // `Decimal("-5")`, `-.5` -> a missing-integer-part float), while a bare
        // `-` is `Minus`. Logos resolves that by longest match; the fast path
        // cannot without re-deriving the number grammar, so `-` always
        // delegates and Logos returns the correct single token either way.
        b'0'..=b'9' | b'-' => return self.delegate_to_logos(token_start),
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
        // Only `...` is the spread operator. A shorter `..`/`.`, and a
        // `.`-led float such as `.5`, are all delegated so Logos builds the
        // exact error (unterminated spread / missing integer part) for
        // whatever `Char` this source uses.
        b'.' => {
          if bytes.starts_with(b"...") {
            self.cursor += 3;
            self.last_span = SimpleSpan::new(token_start, self.cursor);
            return Some(Ok(SyntacticToken::Spread));
          }
          return self.delegate_to_logos(token_start);
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
              // Lone `"` at end of input — unterminated. Delegate to Logos so
              // the error is built for whatever `Char` this source uses.
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
              // Any anomaly delegates the whole token to Logos rather than
              // re-deriving the error here.
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
// concrete `Token`/`Error` types, which requires the `Token<'inp, Error = ..>`
// bound. Every caller of this method is inside `lex()` above, where those
// bounds already hold, so this is never more restrictive in practice than the
// trait impl itself.
impl<'inp, S> SimdSyntacticLexer<'inp, S>
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
  /// Delegate the token starting at `token_start` (== `self.cursor`, prior to
  /// any mutation for this token) to the wrapped Logos lexer.
  ///
  /// This is the single, permanent slow-path fallback: the `_` arm in
  /// [`lex`](Lexer::lex) uses it for every byte none of the fast paths above
  /// claims, and the number / `-` / `.` / string fast paths use it for any
  /// anomaly they detect. Logos re-derives the token — or the exact error —
  /// from scratch, so parity with the pre-SIMD lexer holds by construction:
  /// nothing here constructs an error itself.
  // The return type mirrors `Lexer::lex`'s own `Option<Result<Token, Error>>`
  // shape; clippy can't see through the associated-type projections to
  // recognize the two are the same complexity, so it flags this one.
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
      Delegated::Error { error, end } => {
        self.cursor = end;
        self.last_error_span = Some(SimpleSpan::new(token_start, end));
        Some(Err(error))
      }
    }
  }
}

impl<'inp, S: Source<usize> + ?Sized> SimdSyntacticLexer<'inp, S>
where
  S::Slice<'inp>: AsBytes,
{
  /// Span of the most recently returned error token, or `None` if no error
  /// has been returned yet.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn error_span(&self) -> Option<SimpleSpan> {
    self.last_error_span
  }

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
