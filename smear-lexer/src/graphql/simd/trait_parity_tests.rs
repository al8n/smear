//! End-to-end `Lexer`-trait parity between the SIMD lexer and the Logos
//! `SyntacticLexer` it drop-in replaces, driven in lockstep over a diverse
//! input set.
//!
//! The narrower `error_parity_tests`/`bump_parity_tests` each pin one surface;
//! this harness asserts *every* observable agrees at *every* step of a full
//! drive: the `lex()` result, `span()`, and `slice()` at each token, then — the
//! step those tests skip — `span()`/`slice()` after both lexers reach EOF, and
//! whether a post-EOF `bump` panics. Logos resets its span to `cursor..cursor`
//! (EOF..EOF) once `next()` returns `None`, including after trailing
//! trivia/comments, so the SIMD layer must too — otherwise its stale span keeps
//! reporting the last token and a post-EOF `bump` grows from the wrong base.

use std::panic::{AssertUnwindSafe, catch_unwind};

use tokit::{Lexer, state::recursion_tracker::RecursionLimiter};

use crate::graphql::{simd::SimdSyntacticLexer, syntactic::SyntacticLexer};

/// Run `f`, returning `true` if it panicked, with the panic message suppressed
/// so an expected panic doesn't clutter test output.
fn panics<F: FnOnce()>(f: F) -> bool {
  let prev = std::panic::take_hook();
  std::panic::set_hook(Box::new(|_| {}));
  let caught = catch_unwind(AssertUnwindSafe(f));
  std::panic::set_hook(prev);
  caught.is_err()
}

/// Drive the Logos and SIMD lexers built by the two constructor expressions in
/// lockstep to EOF, asserting `lex()` (by `Debug`), `span()`, and `slice()`
/// agree at every step — including the terminal step where both return `None`,
/// which is where the EOF span/slice reset is observed — then assert a post-EOF
/// `bump(1)` panics on both or on neither.
///
/// The constructor expressions are re-evaluated for the bump phase, so they must
/// be pure (a `new`/`with_state` call), not stateful handles.
macro_rules! assert_full_parity {
  ($src:expr, $make_logos:expr, $make_simd:expr) => {{
    let src: &str = $src;

    {
      let mut logos = $make_logos;
      let mut simd = $make_simd;
      let mut idx = 0usize;
      loop {
        let l = logos.lex();
        let s = simd.lex();
        // Debug-equal covers the Ok token and every error variant without
        // requiring the error type be `PartialEq`.
        assert_eq!(
          format!("{s:?}"),
          format!("{l:?}"),
          "#{idx}: lex() result diverged for {src:?}"
        );
        // span()/slice() reflect the token just returned, or — once both return
        // None — the EOF reset. Both must agree at every step, EOF included.
        assert_eq!(
          simd.span(),
          logos.span(),
          "#{idx}: span() diverged for {src:?}"
        );
        assert_eq!(
          simd.slice(),
          logos.slice(),
          "#{idx}: slice() diverged for {src:?}"
        );
        if l.is_none() {
          break;
        }
        idx += 1;
      }
    }

    // A drained lexer sits at span EOF..EOF, so its end equals the source length
    // and `bump(1)` lands past the last byte: logos asserts the boundary and
    // panics, so the SIMD layer must panic at the same point. Before the EOF
    // span reset the SIMD span was stale (the last token, whose end can be below
    // the length after trailing trivia), so its `bump` stayed in bounds and
    // silently skipped the panic.
    let logos_bump_panics = panics(|| {
      let mut logos = $make_logos;
      while logos.lex().is_some() {}
      logos.bump(&1usize);
    });
    let simd_bump_panics = panics(|| {
      let mut simd = $make_simd;
      while simd.lex().is_some() {}
      simd.bump(&1usize);
    });
    assert_eq!(
      simd_bump_panics, logos_bump_panics,
      "post-EOF bump(1) panic parity diverged for {src:?}"
    );
  }};
}

/// Inputs spanning every dispatch path: fast-path identifiers and punctuation,
/// delegated numbers/strings/block strings, comments, a leading BOM, trailing
/// trivia, whitespace-only, empty, and each malformed shape — the cases whose
/// EOF or post-error span most easily diverges.
const INPUTS: &[&str] = &[
  "{ user(id: 4) { name, ...Frag @skip(if: true) } }",
  "\"\"\"desc\"\"\" type Query { id: ID! name: String }",
  "foo , \t\r\n  ",
  "foo # trailing comment",
  "\u{feff}foo",
  "  \t\n , ",
  "",
  "\"unterminated",
  "007",
  "1e",
  "foo ? bar",
];

#[test]
fn full_trait_parity_str() {
  for &src in INPUTS {
    assert_full_parity!(
      src,
      SyntacticLexer::<&str>::new(src),
      SimdSyntacticLexer::<str>::new(src)
    );
  }
}

#[test]
fn full_trait_parity_bytes() {
  // The same drive over `<[u8]>` (SIMD) vs `<&[u8]>` (Logos): they share the
  // `SyntacticToken<&[u8]>` token, so every observable is directly comparable.
  for &src in INPUTS {
    assert_full_parity!(
      src,
      SyntacticLexer::<&[u8]>::new(src.as_bytes()),
      SimdSyntacticLexer::<[u8]>::new(src.as_bytes())
    );
  }
}

#[test]
fn full_trait_parity_low_recursion_limit() {
  // Deep nesting past a low limit drives the over-limit region (recursion errors
  // in the token's place, plus the finish!/decrease paths) all the way to EOF.
  let depth = 8;
  let big = "(".repeat(depth) + "x" + &")".repeat(depth);
  let src: &str = &big;
  let limit = 3;
  assert_full_parity!(
    src,
    SyntacticLexer::<&str>::with_state(src, RecursionLimiter::with_limitation(limit)),
    SimdSyntacticLexer::<str>::with_state(src, RecursionLimiter::with_limitation(limit))
  );
}
