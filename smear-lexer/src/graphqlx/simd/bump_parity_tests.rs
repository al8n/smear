//! `Lexer::bump` parity between the SIMD lexer and the Logos `SyntacticLexer`
//! it drop-in replaces (the GraphQLx counterpart of the GraphQL bump tests).
//!
//! `bump` must mirror `logos::Lexer::bump`: it extends the current token's end
//! (so `span()`/`slice()` grow to include the bumped bytes) and validates the
//! new end as a source boundary, panicking (`"Invalid Lexer bump"`) when it
//! isn't — past the byte length, or mid-UTF-8 for a `str` source. These tests
//! drive both lexers over identical inputs and `bump` calls and assert every
//! observable agrees: `span()`, `slice()`, the next `lex()`, and panic-vs-not.

use std::panic::{AssertUnwindSafe, catch_unwind};

use tokit::Lexer;

use crate::graphqlx::{simd::SimdSyntacticLexer, syntactic::SyntacticLexer};

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
  // the dispatch loop skip the third. Both lexers must report the grown
  // span/slice and the same next token.
  let src = "foo   bar";
  let mut logos = SyntacticLexer::<&str>::new(src);
  let mut simd = SimdSyntacticLexer::<str>::new(src);

  // Token 0 — identical.
  assert_eq!(simd.lex(), logos.lex(), "token 0");
  assert_eq!(simd.span(), logos.span(), "token 0 span");
  assert_eq!(simd.slice(), logos.slice(), "token 0 slice");

  // Bump by 2 (into the run of spaces): span/slice must grow identically.
  let n = 2usize;
  logos.bump(&n);
  simd.bump(&n);
  assert_eq!(simd.span(), logos.span(), "span after bump");
  assert_eq!(simd.slice(), logos.slice(), "slice after bump");

  // Next token — the loop skips remaining trivia; both must agree, span/slice
  // included.
  assert_eq!(simd.lex(), logos.lex(), "next lex after bump");
  assert_eq!(simd.span(), logos.span(), "next token span");
  assert_eq!(simd.slice(), logos.slice(), "next token slice");

  // Drain — full-stream parity through EOF.
  loop {
    let (l, s) = (logos.lex(), simd.lex());
    assert_eq!(s, l, "tail lex");
    if l.is_none() {
      break;
    }
  }
}

#[test]
fn bump_after_error_token_matches_logos() {
  // `..x`: `..` is an unterminated-spread error token spanning `0..2`
  // (delegated to Logos in GraphQLx), and `x` follows so a one-byte bump stays
  // a valid boundary. Both lexers track the error token's span, so `bump` grows
  // both identically.
  let src = "..x";
  let mut logos = SyntacticLexer::<&str>::new(src);
  let mut simd = SimdSyntacticLexer::<str>::new(src);

  let l0 = logos.lex();
  let s0 = simd.lex();
  assert_eq!(s0, l0, "error token");
  assert!(
    matches!(l0, Some(Err(_))),
    "expected an error token, got {l0:?}"
  );
  assert_eq!(simd.span(), logos.span(), "error token span");

  let n = 1usize;
  logos.bump(&n);
  simd.bump(&n);
  assert_eq!(simd.span(), logos.span(), "span after bump past error");
  assert_eq!(simd.slice(), logos.slice(), "slice after bump past error");
}

#[test]
fn bump_past_end_panics_like_logos() {
  // Whole source consumed, then a one-byte bump lands past the end. logos'
  // `bump` asserts `is_boundary(token_end)` (`index <= len` for a `str`'s byte
  // length), so it panics; the SIMD lexer must panic at the same point.
  let src = "ab";
  let logos_panicked = panics(|| {
    let mut logos = SyntacticLexer::<&str>::new(src);
    let _ = logos.lex(); // `ab`, span 0..2
    logos.bump(&1usize); // -> 3, past end
  });
  let simd_panicked = panics(|| {
    let mut simd = SimdSyntacticLexer::<str>::new(src);
    let _ = simd.lex();
    simd.bump(&1usize);
  });
  assert!(logos_panicked, "logos should panic bumping past end");
  assert_eq!(
    simd_panicked, logos_panicked,
    "SIMD must panic exactly when logos does"
  );
}

#[test]
fn bump_into_multibyte_char_panics_like_logos() {
  // `aé`: `a` at byte 0, `é` at bytes 1..3 (0xC3 0xA9). After lexing `a` the
  // cursor sits at 1; a one-byte bump lands at byte 2 — the middle of `é`. For
  // `str`, logos asserts `is_char_boundary`, so it panics; the SIMD lexer must
  // too.
  let src = "aé";
  let logos_panicked = panics(|| {
    let mut logos = SyntacticLexer::<&str>::new(src);
    let _ = logos.lex(); // `a`, span 0..1
    logos.bump(&1usize); // -> 2, mid-`é`
  });
  let simd_panicked = panics(|| {
    let mut simd = SimdSyntacticLexer::<str>::new(src);
    let _ = simd.lex();
    simd.bump(&1usize);
  });
  assert!(logos_panicked, "logos should panic bumping mid-UTF-8");
  assert_eq!(
    simd_panicked, logos_panicked,
    "SIMD must panic exactly when logos does"
  );
}
