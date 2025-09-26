use logosky::utils::{recursion_tracker::RecursionLimiter, token_tracker::TokenLimiter};

use super::*;

use crate::lexer::tests::{self, TestToken};

impl<'a> TestToken<'a> for CstToken<'a> {
  #[inline]
  fn is_ignored(&self) -> bool {
    self.is_ignored()
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
  tests::test_unexpected_character::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unknown_character() {
  tests::test_unknown_character::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_number_leading_zero() {
  tests::test_number_leading_zero::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_int_leading_zeros_and_suffix() {
  tests::test_int_leading_zeros_and_suffix::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_float_leading_zeros_and_other() {
  tests::test_float_leading_zeros_and_other::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_invalid_number_suffix() {
  tests::test_invalid_number_suffix::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_missing_integer_part() {
  tests::test_missing_integer_part::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  tests::test_missing_integer_part_and_invalid_suffix::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_float_eof() {
  tests::test_unexpected_float_eof::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_number_lexme() {
  tests::test_unexpected_number_lexme::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_integer_ok() {
  tests::test_integer_ok::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_float_ok() {
  tests::test_float_ok::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_inline_string_ok() {
  tests::test_inline_string_ok::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unterminated_inline_string() {
  tests::test_unterminated_inline_string::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_incomplete_unicode_and_eof() {
  tests::test_incomplete_unicode_and_eof::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_line_terminator() {
  tests::test_unexpected_line_terminator::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_escaped() {
  tests::test_unexpected_escaped::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_surrogate_pair() {
  tests::test_surrogate_pair::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_invalid_surrogate_pair() {
  tests::test_invalid_surrogate_pair::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_unterminated_block_string() {
  tests::test_unterminated_block_string::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_block_string_literal() {
  tests::test_surrogate_pair_in_block_string::<CstToken<'_>, LimitExceeded>();
}

#[test]
fn test_recursion_limit() {
  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let lexer = CstToken::lexer_with_extras(
    query.as_str(),
    Tracker::with_recursion_tracker(RecursionLimiter::with_limitation(depth - 1)),
  );

  for result in lexer {
    match result {
      Ok(_) => {}
      Err(mut errors) => {
        let err = errors
          .pop()
          .unwrap()
          .into_data()
          .unwrap_state()
          .unwrap_recursion();
        assert_eq!(err.depth(), depth);
        assert_eq!(err.limitation(), depth - 1);
        return;
      }
    }
  }

  panic!("expected recursion limit exceeded error");
}

#[test]
fn test_token_limit() {
  let limit = 300;
  let source = "a ".repeat(limit);

  let lexer = CstToken::lexer_with_extras(
    source.as_str(),
    Tracker::with_token_tracker(TokenLimiter::with_limitation(limit - 1)),
  );

  for result in lexer {
    match result {
      Ok(_) => {}
      Err(mut errors) => {
        let err = errors.pop().unwrap().into_data().unwrap_state();
        assert_eq!(err.unwrap_token_ref().limitation(), limit - 1);
        return;
      }
    }
  }

  panic!("expected token limit exceeded error");
}
