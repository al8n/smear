//! `Lexer::bump` parity between the SIMD lexer and `logos::Lexer::bump`.
//!
//! `bump` must mirror `logos::Lexer::bump`: it extends the current token's end
//! (so `span()`/`slice()` grow to include the bumped bytes) and validates the
//! new end as a source boundary, panicking (`"Invalid Lexer bump"`) when it
//! isn't — past the byte length, or mid-UTF-8 for a `str` source. These tests
//! drive the SIMD lexer over inputs + `bump` calls and assert every observable
//! against its frozen reference: `span()`, `slice()`, the next `lex()`, and
//! panic-vs-not.

use std::panic::{AssertUnwindSafe, catch_unwind};

use tokit::{Lexer, SimpleSpan};

use crate::graphql::syntactic::{SimdSyntacticLexer, SyntacticToken};

/// Run `f`, returning `true` if it panicked. The panic message is suppressed
/// for the duration so an *expected* panic doesn't clutter test output;
/// `catch_unwind` reports the outcome independently of the hook.
fn panics<F: FnOnce()>(f: F) -> bool {
  let prev = std::panic::take_hook();
  std::panic::set_hook(Box::new(|_| {}));
  let caught = catch_unwind(AssertUnwindSafe(f));
  std::panic::set_hook(prev);
  caught.is_err()
}

#[test]
fn valid_bump_grows_span_and_next_lex_matches_logos() {
  // `foo   bar`: lex `foo`, bump past two of the three trivia bytes, then let
  // the dispatch loop skip the third. Frozen reference: token 0 is
  // `Identifier("foo")` at 0..3; after `bump(2)` span/slice grow to 0..5 /
  // "foo  "; the next token is `Identifier("bar")` at 6..9; the stream ends
  // (`None`) immediately after.
  let src = "foo   bar";
  let mut simd = SimdSyntacticLexer::<str>::new(src);

  // Token 0.
  assert_eq!(
    simd.lex(),
    Some(Ok(SyntacticToken::Identifier("foo"))),
    "token 0"
  );
  assert_eq!(simd.span(), SimpleSpan::new(0, 3), "token 0 span");
  assert_eq!(simd.slice(), "foo", "token 0 slice");

  // Bump by 2 (into the run of spaces): span/slice must grow to match.
  let n = 2usize;
  simd.bump(&n);
  assert_eq!(simd.span(), SimpleSpan::new(0, 5), "span after bump");
  assert_eq!(simd.slice(), "foo  ", "slice after bump");

  // Next token — the loop skips remaining trivia.
  assert_eq!(
    simd.lex(),
    Some(Ok(SyntacticToken::Identifier("bar"))),
    "next lex after bump"
  );
  assert_eq!(simd.span(), SimpleSpan::new(6, 9), "next token span");
  assert_eq!(simd.slice(), "bar", "next token slice");

  // Drain — the stream ends right after `bar`.
  assert_eq!(simd.lex(), None, "tail lex");
}

#[test]
fn bump_after_error_token_matches_logos() {
  // `..x`: `..` is an unterminated-spread error token spanning `0..2`, and `x`
  // follows so a one-byte bump stays a valid boundary. Frozen reference: the
  // error token's `Debug` text is exactly the constant below; after `bump(1)`
  // the span grows to 0..3 and the slice becomes "..x".
  let src = "..x";
  let mut simd = SimdSyntacticLexer::<str>::new(src);

  let s0 = simd.lex();
  assert!(
    matches!(s0, Some(Err(_))),
    "expected an error token, got {s0:?}"
  );
  assert_eq!(
    format!("{s0:?}"),
    "Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 2 }, \
     data: UnterminatedSpreadOperator }])))",
    "error token"
  );
  assert_eq!(simd.span(), SimpleSpan::new(0, 2), "error token span");

  let n = 1usize;
  simd.bump(&n);
  assert_eq!(
    simd.span(),
    SimpleSpan::new(0, 3),
    "span after bump past error"
  );
  assert_eq!(simd.slice(), "..x", "slice after bump past error");
}

#[test]
fn bump_past_end_panics_like_logos() {
  // Whole source consumed, then a one-byte bump lands past the end. Frozen
  // reference: `logos::Lexer::bump` asserts `is_boundary(token_end)` (`index
  // <= len` for a `str`'s byte length), which fails unconditionally here, so
  // logos panics; the SIMD lexer must panic at the same point.
  let src = "ab";
  let simd_panicked = panics(|| {
    let mut simd = SimdSyntacticLexer::<str>::new(src);
    let _ = simd.lex(); // `ab`, span 0..2
    simd.bump(&1usize); // -> 3, past end
  });
  assert!(
    simd_panicked,
    "SIMD must panic bumping past end, like logos"
  );
}

#[test]
fn bump_into_multibyte_char_panics_like_logos() {
  // `aé`: `a` at byte 0, `é` at bytes 1..3 (0xC3 0xA9). After lexing `a` the
  // cursor sits at 1; a one-byte bump lands at byte 2 — the middle of `é`.
  // Frozen reference: for `str`, `logos::Lexer::bump` asserts
  // `is_char_boundary` and panics unconditionally here; the SIMD lexer must
  // too.
  let src = "aé";
  let simd_panicked = panics(|| {
    let mut simd = SimdSyntacticLexer::<str>::new(src);
    let _ = simd.lex();
    simd.bump(&1usize);
  });
  assert!(
    simd_panicked,
    "SIMD must panic bumping mid-UTF-8, like logos"
  );
}
