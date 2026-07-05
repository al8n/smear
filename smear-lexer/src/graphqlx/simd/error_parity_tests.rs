//! After-error `Lexer`-trait parity between the GraphQLx SIMD lexer and the
//! Logos `SyntacticLexer` it drop-in replaces (the GraphQLx counterpart of
//! `graphql/simd/error_parity_tests.rs`).
//!
//! The frozen oracle renders each error by its own `Debug` and only reads
//! `lexer.span()` on the *Ok* arm, so it never exercised the lexer-level
//! `span()`/`slice()`/`check()` on the error path. These tests drive both
//! lexers over the same error-producing inputs in lockstep and assert those
//! three methods agree token-for-token, including every error token.

use tokit::{Lexer, state::recursion_tracker::RecursionLimiter};

use crate::graphqlx::{simd::SimdSyntacticLexer, syntactic::SyntacticLexer};

/// Drive the Logos and SIMD lexers in lockstep over the same input, asserting
/// at every token that `span()` and `slice()` agree, and — for every error
/// token — that `check()` agrees too. Requires at least one error to appear.
macro_rules! assert_error_path_parity {
  ($src:expr, $logos:expr, $simd:expr) => {{
    let src: &str = $src;
    let mut logos = $logos;
    let mut simd = $simd;
    let mut saw_error = false;
    let mut idx = 0usize;
    loop {
      match (logos.lex(), simd.lex()) {
        (None, None) => break,
        (Some(l), Some(s)) => {
          assert_eq!(
            l.is_err(),
            s.is_err(),
            "#{idx}: Ok/Err shape diverged for {src:?} (logos_span={:?} simd_span={:?})",
            logos.span(),
            simd.span(),
          );
          // span()/slice() must match on every token — valid OR error.
          assert_eq!(
            simd.span(),
            logos.span(),
            "#{idx}: span() mismatch for {src:?}"
          );
          assert_eq!(
            simd.slice(),
            logos.slice(),
            "#{idx}: slice() mismatch for {src:?}"
          );
          if l.is_err() {
            saw_error = true;
            assert_eq!(
              format!("{:?}", simd.check()),
              format!("{:?}", logos.check()),
              "#{idx}: check() mismatch after error for {src:?}",
            );
          }
        }
        (l, s) => panic!(
          "#{idx}: stream length diverged for {src:?}: logos={} simd={}",
          l.is_some(),
          s.is_some(),
        ),
      }
      idx += 1;
    }
    assert!(saw_error, "expected at least one error for {src:?}");
  }};
}

#[test]
fn error_path_span_slice_check_match_logos() {
  // GraphQLx delegates numbers, strings, and `.` whole to Logos, so every
  // error below flows through `delegate_to_logos` except the recursion arm
  // (covered separately):
  //   - unterminated inline string
  //   - bad escape
  //   - unterminated block string
  //   - bad radix (hex) number
  //   - unknown byte
  //   - `..` (unterminated spread, delegated)
  for src in [
    r#""unterminated"#,
    r#""a\qb""#,
    r#""""oops"#,
    "0xZZ",
    "?",
    "..x",
  ] {
    assert_error_path_parity!(
      src,
      SyntacticLexer::<&str>::new(src),
      SimdSyntacticLexer::<str>::new(src)
    );
  }
}

#[test]
fn recursion_limit_region_matches_logos() {
  // At a low limit, every bracket past the limit yields the recursion error in
  // the token's place, and the region also drives the `finish!` (ident) and
  // decrease-bracket paths while over the limit. Both lexers must agree on
  // span()/slice()/check() across the entire depth-exceeded region.
  let depth = 10;
  let src = "(".repeat(depth) + "x" + &")".repeat(depth);
  let limit = 3;
  assert_error_path_parity!(
    src.as_str(),
    SyntacticLexer::<&str>::with_state(src.as_str(), RecursionLimiter::with_limitation(limit)),
    SimdSyntacticLexer::<str>::with_state(src.as_str(), RecursionLimiter::with_limitation(limit))
  );
}
