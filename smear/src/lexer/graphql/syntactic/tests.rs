use logosky::{Lexed, TokenStream, utils::recursion_tracker::RecursionLimiter};

use super::*;

use crate::lexer::{
  LitInlineStr,
  graphql::tests::{self, TestToken},
};

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
  tests::test_unexpected_character::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unknown_character() {
  tests::test_unknown_character::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_number_leading_zero() {
  tests::test_number_leading_zero::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_int_leading_zeros_then_check_suffix() {
  tests::test_int_leading_zeros_then_check_suffix::<StrSyntacticToken<'_>, RecursionLimitExceeded>(
  );
}

#[test]
fn test_float_leading_zeros_and_other() {
  tests::test_float_leading_zeros_and_other::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_invalid_number_suffix() {
  tests::test_invalid_number_suffix::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_missing_integer_part() {
  tests::test_missing_integer_part::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  tests::test_missing_integer_part_and_invalid_suffix::<
    StrSyntacticToken<'_>,
    RecursionLimitExceeded,
  >();
}

#[test]
fn test_unexpected_float_eof() {
  tests::test_unexpected_float_eof::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_number_lexme() {
  tests::test_unexpected_number_lexme::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_integer_ok() {
  tests::test_integer_ok::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_float_ok() {
  tests::test_float_ok::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_inline_string_ok() {
  tests::test_inline_string_ok::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unterminated_inline_string() {
  tests::test_unterminated_inline_string::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_incomplete_unicode_and_eof() {
  tests::test_incomplete_unicode_and_eof::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_line_terminator() {
  tests::test_unexpected_line_terminator::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_escaped() {
  tests::test_unexpected_escaped::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_surrogate_pair() {
  tests::test_surrogate_pair::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_invalid_surrogate_pair() {
  tests::test_invalid_surrogate_pair::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unterminated_block_string() {
  tests::test_unterminated_block_string::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_block_string_literal() {
  tests::test_surrogate_pair_in_block_string::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_escape_triple_quote_block_string() {
  tests::test_escape_triple_quote_block_string::<StrSyntacticToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_bom_lexing() {
  let input = "\u{feff}";

  let mut lexer = TokenStream::<StrSyntacticToken<'_>>::new(input).into_iter();

  assert_eq!(lexer.next(), None);
}

#[test]
fn test_recursion_limit() {
  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let lexer = TokenStream::<StrSyntacticToken<'_>>::with_state(
    query.as_str(),
    RecursionLimiter::with_limitation(depth - 1),
  );

  for result in lexer {
    match result {
      Lexed::Token(_) => {}
      Lexed::Error(mut errors) => {
        let err = errors.pop().unwrap().into_data().unwrap_state();
        assert_eq!(err.depth(), depth);
        assert_eq!(err.limitation(), depth - 1);
        return;
      }
    }
  }

  panic!("expected recursion limit exceeded error");
}
