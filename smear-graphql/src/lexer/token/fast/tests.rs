use super::*;

use crate::lexer::token::tests::{self, TestToken};

impl<'a> TestToken<'a> for Token<'a> {
  #[inline]
  fn is_ignored(&self) -> bool {
    false
  }

  #[inline]
  fn inline_string_literal(&self) -> Option<&'a str> {
    match self {
      Token::StringLiteral(s) => Some(s),
      _ => None,
    }
  }

  #[inline]
  fn from_inline_string_literal(s: &'a str) -> Self {
    Token::StringLiteral(s)
  }

  #[inline]
  fn from_float_literal(s: &'a str) -> Self {
    Token::Float(s)
  }

  #[inline]
  fn from_integer_literal(s: &'a str) -> Self {
    Token::Int(s)
  }

  #[inline]
  fn block_string_literal(&self) -> Option<&'a str> {
    match self {
      Token::BlockStringLiteral(s) => Some(s),
      _ => None,
    }
  }
}

#[test]
fn test_unexpected_character() {
  tests::test_unexpected_character::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unknown_character() {
  tests::test_unknown_character::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_number_leading_zero() {
  tests::test_number_leading_zero::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_int_leading_zeros_and_suffix() {
  tests::test_int_leading_zeros_and_suffix::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_float_leading_zeros_and_other() {
  tests::test_float_leading_zeros_and_other::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_invalid_number_suffix() {
  tests::test_invalid_number_suffix::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_missing_integer_part() {
  tests::test_missing_integer_part::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  tests::test_missing_integer_part_and_invalid_suffix::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_float_eof() {
  tests::test_unexpected_float_eof::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_number_lexme() {
  tests::test_unexpected_number_lexme::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_integer_ok() {
  tests::test_integer_ok::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_float_ok() {
  tests::test_float_ok::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_inline_string_ok() {
  tests::test_inline_string_ok::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unterminated_inline_string() {
  tests::test_unterminated_inline_string::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_incomplete_unicode_and_eof() {
  tests::test_incomplete_unicode_and_eof::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_line_terminator() {
  tests::test_unexpected_line_terminator::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unexpected_escaped() {
  tests::test_unexpected_escaped::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_surrogate_pair() {
  tests::test_surrogate_pair::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_invalid_surrogate_pair() {
  tests::test_invalid_surrogate_pair::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_unterminated_block_string() {
  tests::test_unterminated_block_string::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_block_string_literal() {
  tests::test_surrogate_pair_in_block_string::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_escape_triple_quote_block_string() {
  tests::test_escape_triple_quote_block_string::<Token<'_>, RecursionLimitExceeded>();
}

#[test]
fn test_bom_lexing() {
  let input = "\u{feff}";

  let mut lexer = Token::lexer(input);

  assert_eq!(lexer.next(), None);
}

#[test]
fn test_recursion_limit() {
  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let lexer =
    Token::lexer_with_extras(query.as_str(), RecursionLimiter::with_limitation(depth - 1));

  for result in lexer {
    match result {
      Ok(_) => {}
      Err(mut errors) => {
        let err = errors
          .pop()
          .unwrap()
          .into_data()
          .unwrap_recursion_limit_exceeded();
        assert_eq!(err.depth(), depth);
        assert_eq!(err.limitation(), depth - 1);
        return;
      }
    }
  }

  panic!("expected recursion limit exceeded error");
}
