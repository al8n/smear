use tokit::{Lexer, state::recursion_tracker::RecursionLimiter};

use super::*;

use crate::{
  LitInlineStr,
  graphql::{
    simd::SimdSyntacticLexer,
    tests::{self, TestToken},
  },
};

use std::string::ToString;

type StrSyntacticToken<'a> = SyntacticToken<&'a str>;

impl<'a> TestToken<'a> for SyntacticToken<&'a str> {
  #[inline]
  fn is_ignored(&self) -> bool {
    false
  }

  #[inline]
  fn inline_string_literal(&self) -> Option<&'a str> {
    match self {
      Self::LitInlineStr(s) => Some(s.as_str()),
      _ => None,
    }
  }

  #[inline]
  fn from_inline_string_literal(s: LitInlineStr<&'a str>) -> Self {
    Self::LitInlineStr(s)
  }

  #[inline]
  fn from_float_literal(s: &'a str) -> Self {
    Self::LitFloat(s)
  }

  #[inline]
  fn from_integer_literal(s: &'a str) -> Self {
    Self::LitInt(s)
  }

  #[inline]
  fn block_string_literal(&self) -> Option<&'a str> {
    match self {
      Self::LitBlockStr(s) => Some(s.as_str()),
      _ => None,
    }
  }
}

#[test]
fn test_unexpected_character() {
  tests::test_unexpected_character::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unknown_character() {
  tests::test_unknown_character::<StrSyntacticToken<'_>>();
}

#[test]
fn test_number_leading_zero() {
  tests::test_number_leading_zero::<StrSyntacticToken<'_>>();
}

#[test]
fn test_int_leading_zeros_then_check_suffix() {
  tests::test_int_leading_zeros_then_check_suffix::<StrSyntacticToken<'_>>();
}

#[test]
fn test_float_leading_zeros_and_other() {
  tests::test_float_leading_zeros_and_other::<StrSyntacticToken<'_>>();
}

#[test]
fn test_invalid_number_suffix() {
  tests::test_invalid_number_suffix::<StrSyntacticToken<'_>>();
}

#[test]
fn test_missing_integer_part() {
  tests::test_missing_integer_part::<StrSyntacticToken<'_>>();
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  tests::test_missing_integer_part_and_invalid_suffix::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unexpected_float_eof() {
  tests::test_unexpected_float_eof::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unexpected_number_lexme() {
  tests::test_unexpected_number_lexme::<StrSyntacticToken<'_>>();
}

#[test]
fn test_integer_ok() {
  tests::test_integer_ok::<StrSyntacticToken<'_>>();
}

#[test]
fn test_float_ok() {
  tests::test_float_ok::<StrSyntacticToken<'_>>();
}

#[test]
fn test_inline_string_ok() {
  tests::test_inline_string_ok::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unterminated_inline_string() {
  tests::test_unterminated_inline_string::<StrSyntacticToken<'_>>();
}

#[test]
fn test_incomplete_unicode_and_eof() {
  tests::test_incomplete_unicode_and_eof::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unexpected_line_terminator() {
  tests::test_unexpected_line_terminator::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unexpected_escaped() {
  tests::test_unexpected_escaped::<StrSyntacticToken<'_>>();
}

#[test]
fn test_surrogate_pair() {
  tests::test_surrogate_pair::<StrSyntacticToken<'_>>();
}

#[test]
fn test_invalid_surrogate_pair() {
  tests::test_invalid_surrogate_pair::<StrSyntacticToken<'_>>();
}

#[test]
fn test_unterminated_block_string() {
  tests::test_unterminated_block_string::<StrSyntacticToken<'_>>();
}

#[test]
fn test_block_string_literal() {
  tests::test_surrogate_pair_in_block_string::<StrSyntacticToken<'_>>();
}

#[test]
fn test_escape_triple_quote_block_string() {
  tests::test_escape_triple_quote_block_string::<StrSyntacticToken<'_>>();
}

#[test]
fn test_bom_lexing() {
  let input = "\u{feff}";

  // The SIMD lexer skips the BOM as trivia and reaches EOF immediately — the
  // full `SyntacticToken` Logos grammar (whose `skip` pattern once handled this)
  // is gone, so this drives the surviving `SimdSyntacticLexer` directly.
  let mut lexer = SimdSyntacticLexer::<str>::new(input);

  assert!(lexer.lex().is_none());
}

#[test]
fn test_recursion_limit() {
  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let mut lexer = SimdSyntacticLexer::<str>::with_state(
    query.as_str(),
    RecursionLimiter::with_limitation(depth - 1),
  );

  loop {
    match lexer.lex() {
      None => break,
      Some(Ok(_)) => {}
      Some(Err(mut errors)) => {
        let err = errors.pop().unwrap().into_data().unwrap_state();
        assert_eq!(err.depth(), depth);
        assert_eq!(err.limitation(), depth - 1);
        return;
      }
    }
  }

  panic!("expected recursion limit exceeded error");
}
