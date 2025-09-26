use super::*;

use crate::lexer::tests::{self, TestToken};

impl<'a> TestToken<'a> for AstToken<'a> {
  #[inline]
  fn is_ignored(&self) -> bool {
    false
  }

  #[inline]
  fn inline_string_literal(&self) -> Option<&'a str> {
    match self {
      Self::StringLiteral(s) => Some(s.as_str()),
      _ => None,
    }
  }

  #[inline]
  fn from_inline_string_literal(s: &'a str) -> Self {
    Self::StringLiteral(InlineString::Clean(s))
  }

  #[inline]
  fn from_float_literal(s: &'a str) -> Self {
    Self::Float(s)
  }

  #[inline]
  fn from_integer_literal(s: &'a str) -> Self {
    Self::Int(s)
  }

  #[inline]
  fn block_string_literal(&self) -> Option<&'a str> {
    match self {
      Self::BlockStringLiteral(s) => Some(s.as_str()),
      _ => None,
    }
  }
}

#[test]
fn test_unexpected_character() {
  tests::test_unexpected_character::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unknown_character() {
  tests::test_unknown_character::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_number_leading_zero() {
  tests::test_number_leading_zero::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_int_leading_zeros_and_suffix() {
  tests::test_int_leading_zeros_and_suffix::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_float_leading_zeros_and_other() {
  tests::test_float_leading_zeros_and_other::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_invalid_number_suffix() {
  tests::test_invalid_number_suffix::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_missing_integer_part() {
  tests::test_missing_integer_part::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  tests::test_missing_integer_part_and_invalid_suffix::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_float_eof() {
  tests::test_unexpected_float_eof::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_number_lexme() {
  tests::test_unexpected_number_lexme::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_integer_ok() {
  tests::test_integer_ok::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_float_ok() {
  tests::test_float_ok::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_inline_string_ok() {
  tests::test_inline_string_ok::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unterminated_inline_string() {
  tests::test_unterminated_inline_string::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_incomplete_unicode_and_eof() {
  tests::test_incomplete_unicode_and_eof::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_line_terminator() {
  tests::test_unexpected_line_terminator::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_escaped() {
  tests::test_unexpected_escaped::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_surrogate_pair() {
  tests::test_surrogate_pair::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_invalid_surrogate_pair() {
  tests::test_invalid_surrogate_pair::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unterminated_block_string() {
  tests::test_unterminated_block_string::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_block_string_literal() {
  tests::test_surrogate_pair_in_block_string::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_escape_triple_quote_block_string() {
  tests::test_escape_triple_quote_block_string::<AstToken<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_bom_lexing() {
  let input = "\u{feff}";

  let mut lexer = AstToken::lexer(input);

  assert_eq!(lexer.next(), None);
}

#[test]
fn test_recursion_limit() {
  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let lexer =
    AstToken::lexer_with_extras(query.as_str(), RecursionLimiter::with_limitation(depth - 1));

  for result in lexer {
    match result {
      Ok(_) => {}
      Err(mut errors) => {
        let err = errors.pop().unwrap().into_data().unwrap_state();
        assert_eq!(err.depth(), depth);
        assert_eq!(err.limitation(), depth - 1);
        return;
      }
    }
  }

  panic!("expected recursion limit exceeded error");
}
