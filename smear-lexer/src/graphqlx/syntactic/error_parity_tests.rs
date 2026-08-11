//! After-error `Lexer`-trait parity for the GraphQLx SIMD lexer, mirroring
//! `graphql/syntactic/error_parity_tests.rs`.
//!
//! Each case freezes the exact `span()`/`slice()`/`check()` render a
//! `logos::Lexer` over the full `SyntacticToken` grammar yields for an
//! error-producing input, as a hardcoded per-input constant. These tests drive
//! the SIMD lexer over the same inputs and assert those three methods at every
//! token (error or not) against the frozen render, giving per-token, post-error
//! coverage.

use std::string::String;

use tokora::{Lexer, state::recursion_tracker::RecursionLimiter};

use crate::graphqlx::syntactic::SyntacticLexer;

/// Render every token of a drive: `is_err`, `span()`, `slice()` at each step,
/// plus `check()` whenever that step is an error. Panics if the drive never
/// produces an error.
fn render_error_path(mut lex: SyntacticLexer<'_, str>, src: &str) -> String {
  use std::fmt::Write as _;
  let mut out = String::new();
  let mut saw_error = false;
  let mut idx = 0usize;
  while let Some(item) = lex.lex() {
    let is_err = item.is_err();
    saw_error |= is_err;
    let _ = writeln!(
      out,
      "#{idx} is_err={is_err} span={:?} slice={:?}",
      lex.span(),
      lex.slice()
    );
    if is_err {
      let _ = writeln!(out, "#{idx} check={:?}", lex.check());
    }
    idx += 1;
  }
  assert!(saw_error, "expected at least one error for {src:?}");
  out
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
  //
  // Every one of these non-recursion cases' `check()` comes back `Ok(())`:
  // none of these errors perturb the recursion limiter — see
  // `recursion_limit_region_matches_logos` below for the one path that does.
  const CASES: &[(&str, &str)] = &[
    (
      r#""unterminated"#,
      "#0 is_err=true span=SimpleSpan { start: 0, end: 13 } slice=\"\\\"unterminated\"\n#0 check=Ok(())\n",
    ),
    (
      r#""a\qb""#,
      "#0 is_err=true span=SimpleSpan { start: 0, end: 6 } slice=\"\\\"a\\\\qb\\\"\"\n#0 check=Ok(())\n",
    ),
    (
      r#""""oops"#,
      "#0 is_err=true span=SimpleSpan { start: 0, end: 7 } slice=\"\\\"\\\"\\\"oops\"\n#0 check=Ok(())\n",
    ),
    (
      "0xZZ",
      "#0 is_err=true span=SimpleSpan { start: 0, end: 4 } slice=\"0xZZ\"\n#0 check=Ok(())\n",
    ),
    (
      "?",
      "#0 is_err=true span=SimpleSpan { start: 0, end: 1 } slice=\"?\"\n#0 check=Ok(())\n",
    ),
    (
      "..x",
      "#0 is_err=true span=SimpleSpan { start: 0, end: 2 } slice=\"..\"\n#0 check=Ok(())\n\
       #1 is_err=false span=SimpleSpan { start: 2, end: 3 } slice=\"x\"\n",
    ),
  ];

  for (src, expected) in CASES {
    let simd = SyntacticLexer::<str>::new(src);
    assert_eq!(
      &render_error_path(simd, src),
      expected,
      "mismatch for {src:?}"
    );
  }
}

#[test]
fn recursion_limit_region_matches_logos() {
  // At a low limit, every bracket past the limit yields the recursion error in
  // the token's place, and the region also drives the `finish!` (ident) and
  // decrease-bracket paths while over the limit. `check()` carries the
  // *current* depth at each step (it reads live `RecursionLimiter` state, not a
  // snapshot from when the error token was produced), so it changes
  // token-to-token as brackets close. This GraphQLx region is `(`/`)`-only
  // (like GraphQL's) and produces the same sequence — GraphQLx-specific
  // recursion coverage over `<`/`>` lives in `tests/oracle.rs`'s low-recursion
  // parity test.
  let depth = 10;
  let src = "(".repeat(depth) + "x" + &")".repeat(depth);
  let limit = 3;
  let expected = "\
#0 is_err=false span=SimpleSpan { start: 0, end: 1 } slice=\"(\"
#1 is_err=false span=SimpleSpan { start: 1, end: 2 } slice=\"(\"
#2 is_err=false span=SimpleSpan { start: 2, end: 3 } slice=\"(\"
#3 is_err=true span=SimpleSpan { start: 3, end: 4 } slice=\"(\"
#3 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 4 })) }]))
#4 is_err=true span=SimpleSpan { start: 4, end: 5 } slice=\"(\"
#4 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 5 })) }]))
#5 is_err=true span=SimpleSpan { start: 5, end: 6 } slice=\"(\"
#5 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 6 })) }]))
#6 is_err=true span=SimpleSpan { start: 6, end: 7 } slice=\"(\"
#6 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 7 })) }]))
#7 is_err=true span=SimpleSpan { start: 7, end: 8 } slice=\"(\"
#7 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 8 })) }]))
#8 is_err=true span=SimpleSpan { start: 8, end: 9 } slice=\"(\"
#8 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 9 })) }]))
#9 is_err=true span=SimpleSpan { start: 9, end: 10 } slice=\"(\"
#9 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 10 })) }]))
#10 is_err=true span=SimpleSpan { start: 10, end: 11 } slice=\"x\"
#10 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 10 })) }]))
#11 is_err=true span=SimpleSpan { start: 11, end: 12 } slice=\")\"
#11 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 9 })) }]))
#12 is_err=true span=SimpleSpan { start: 12, end: 13 } slice=\")\"
#12 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 8 })) }]))
#13 is_err=true span=SimpleSpan { start: 13, end: 14 } slice=\")\"
#13 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 7 })) }]))
#14 is_err=true span=SimpleSpan { start: 14, end: 15 } slice=\")\"
#14 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 6 })) }]))
#15 is_err=true span=SimpleSpan { start: 15, end: 16 } slice=\")\"
#15 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 5 })) }]))
#16 is_err=true span=SimpleSpan { start: 16, end: 17 } slice=\")\"
#16 check=Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 4 })) }]))
#17 is_err=false span=SimpleSpan { start: 17, end: 18 } slice=\")\"
#18 is_err=false span=SimpleSpan { start: 18, end: 19 } slice=\")\"
#19 is_err=false span=SimpleSpan { start: 19, end: 20 } slice=\")\"
#20 is_err=false span=SimpleSpan { start: 20, end: 21 } slice=\")\"
";
  let simd =
    SyntacticLexer::<str>::with_state(src.as_str(), RecursionLimiter::with_limitation(limit));
  assert_eq!(render_error_path(simd, &src), expected);
}
