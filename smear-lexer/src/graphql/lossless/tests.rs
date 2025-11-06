use logosky::Tokenizer;

use super::*;

use crate::{
  LitInlineStr,
  graphql::tests::{self, TestToken},
};

type StrLosslessToken<'a> = LosslessToken<&'a str>;

impl<'a> TestToken<'a> for LosslessToken<&'a str> {
  #[inline]
  fn is_ignored(&self) -> bool {
    matches!(
      self.kind(),
      LosslessTokenKind::Space
        | LosslessTokenKind::Bom
        | LosslessTokenKind::Tab
        | LosslessTokenKind::Comment
        | LosslessTokenKind::Newline
        | LosslessTokenKind::CarriageReturn
        | LosslessTokenKind::CarriageReturnAndNewline
    )
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
  tests::test_unexpected_character::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unknown_character() {
  tests::test_unknown_character::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_number_leading_zero() {
  tests::test_number_leading_zero::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_int_leading_zeros_then_check_suffix() {
  tests::test_int_leading_zeros_then_check_suffix::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_float_leading_zeros_and_other() {
  tests::test_float_leading_zeros_and_other::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_invalid_number_suffix() {
  tests::test_invalid_number_suffix::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_missing_integer_part() {
  tests::test_missing_integer_part::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  tests::test_missing_integer_part_and_invalid_suffix::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_float_eof() {
  tests::test_unexpected_float_eof::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_number_lexme() {
  tests::test_unexpected_number_lexme::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_integer_ok() {
  tests::test_integer_ok::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_float_ok() {
  tests::test_float_ok::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_inline_string_ok() {
  tests::test_inline_string_ok::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unterminated_inline_string() {
  tests::test_unterminated_inline_string::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_incomplete_unicode_and_eof() {
  tests::test_incomplete_unicode_and_eof::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_line_terminator() {
  tests::test_unexpected_line_terminator::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unexpected_escaped() {
  tests::test_unexpected_escaped::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_surrogate_pair() {
  tests::test_surrogate_pair::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_invalid_surrogate_pair() {
  tests::test_invalid_surrogate_pair::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_unterminated_block_string() {
  tests::test_unterminated_block_string::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_block_string_literal() {
  tests::test_surrogate_pair_in_block_string::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_escape_triple_quote_block_string() {
  tests::test_escape_triple_quote_block_string::<StrLosslessToken<'_>, LimitExceeded>();
}

#[test]
fn test_bom_lexing() {
  let input = "\u{feff}";

  let mut lexer = Tokenizer::<StrLosslessToken<'_>>::new(input).into_iter();

  assert_eq!(lexer.next(), None);
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[test]
fn test_recursion_limit() {
  use logosky::{
    Lexed, Tokenizer,
    utils::{recursion_tracker::RecursionLimiter, tracker::Limiter},
  };

  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let lexer = Tokenizer::<StrLosslessToken<'_>>::with_state(
    query.as_str(),
    Limiter::with_recursion_tracker(RecursionLimiter::with_limitation(depth - 1)),
  );

  for result in lexer {
    match result {
      Lexed::Token(_) => {}
      Lexed::Error(mut errors) => {
        let err = errors.pop().unwrap().unwrap_state();
        assert_eq!(err.unwrap_recursion_ref().depth(), depth);
        assert_eq!(err.unwrap_recursion_ref().limitation(), depth - 1);
        return;
      }
    }
  }

  panic!("expected recursion limit exceeded error");
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[test]
fn test_token_limit() {
  use logosky::{
    Lexed, Tokenizer,
    utils::{token_tracker::TokenLimiter, tracker::Limiter},
  };

  let limit = 300;
  let source = "a ".repeat(limit);

  let lexer = Tokenizer::<StrLosslessToken<'_>>::with_state(
    source.as_str(),
    Limiter::with_token_tracker(TokenLimiter::with_limitation(limit - 1)),
  );

  for result in lexer {
    match result {
      Lexed::Token(_) => {}
      Lexed::Error(mut errors) => {
        let err = errors.pop().unwrap().unwrap_state();
        assert_eq!(err.unwrap_token_ref().limitation(), limit - 1);
        return;
      }
    }
  }

  panic!("expected token limit exceeded error");
}
