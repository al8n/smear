//! Before the inline-string error path delegated to Logos, the `Lexer` impl's
//! `S::Slice<'inp>: Slice<'inp, Char = u8>` bound meant `SyntacticLexer::<str>`
//! (whose slice is `&str`, `Char = char`) could not satisfy the impl at all, so every
//! test below was a compile error. Delegating both inline-string error outcomes to
//! Logos (which builds whatever `Char` type the source needs) let that bound drop,
//! so `<str>` now compiles and lexes correctly, not just `<[u8]>`.
//!
//! This module originally had three tests. Two of them (a valid-query smoke
//! test asserting the inline string still emits inline, and a bad-escape test
//! asserting the delegated error is genuinely `Char = char`) are strictly
//! subsumed by `graphql_syntactic_simd_oracle` in `tests/oracle.rs`, which
//! drives `SyntacticLexer::<str>` directly against the golden files (41
//! fixtures, byte-for-byte `Debug` comparison, including `str_bad_escape` and
//! many valid-query/inline-string fixtures) -- so they were deleted rather
//! than kept. The one kept below covers a source text (a lone `"` as the
//! *entire* input) that no golden fixture exercises: it takes the
//! `bytes.get(1) == None` branch in `lex()`'s `b'"'` arm, distinct from
//! `str_unterminated_inline`'s `Some(_) => Err(_)` branch.

use std::vec::Vec;

use tokora::Lexer as _;

use crate::lexer::{
  error::StringError,
  graphql::{
    error::LexerErrorData,
    syntactic::{SyntacticLexer, SyntacticLexerErrors, SyntacticToken},
  },
};

/// Drive a `str`-sourced lexer to completion, collecting every result.
fn lex_all(src: &str) -> Vec<Result<SyntacticToken<&str>, SyntacticLexerErrors>> {
  let mut lexer = SyntacticLexer::<str>::new(src);
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
