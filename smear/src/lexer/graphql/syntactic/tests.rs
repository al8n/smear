use tokora::{Lexer, state::recursion_tracker::RecursionLimiter};

use super::*;

use crate::lexer::{
  LitInlineStr, LitPlainStr,
  graphql::{
    syntactic::SyntacticLexer,
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

  // The SIMD lexer skips the BOM as trivia and reaches EOF immediately.
  let mut lexer = SyntacticLexer::<str>::new(input);

  assert!(lexer.lex().is_none());
}

#[test]
fn test_recursion_limit() {
  let depth = 65;
  let field = "a {".repeat(depth) + &"}".repeat(depth);
  let query = field.replace("{}", "{b}").to_string();

  let mut lexer =
    SyntacticLexer::<str>::with_state(query.as_str(), RecursionLimiter::with_limitation(depth - 1));

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

#[test]
fn capability_traits_classify_tokens() {
  use tokora::token::{IdentifierToken, LitToken as _, PunctuatorToken};
  // UFCS on `IdentifierToken`: the `IsVariant` derive also generates an inherent
  // `is_identifier`, so method-call syntax would resolve to that instead of the trait.
  assert!(IdentifierToken::is_identifier(
    &SyntacticToken::<&str>::Identifier("x")
  ));
  assert!(!IdentifierToken::is_identifier(
    &SyntacticToken::<&str>::LitInt("1")
  ));
  assert!(SyntacticToken::<&str>::LitInt("1").is_integer_literal());
  assert!(SyntacticToken::<&str>::LitFloat("1.5").is_float_literal());

  let inline = SyntacticToken::LitInlineStr(LitPlainStr::new("\"hi\"").into());
  let block = SyntacticToken::LitBlockStr(LitPlainStr::new("\"\"\"b\"\"\"").into());
  assert!(inline.is_inline_string_literal());
  assert!(!inline.is_multiline_string_literal());
  assert!(block.is_multiline_string_literal());
  assert!(!block.is_inline_string_literal());
  assert!(inline.is_string_literal());
  assert!(block.is_string_literal());

  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::spread(),
    Some(SyntacticTokenKind::Spread)
  );
}

/// Census: every declared `SyntacticTokenKind` variant must be producible by some
/// `SyntacticToken`, i.e. by some arm of `SyntacticToken::kind`.
///
/// Mirrors `lossless_token_kind_census` (`graphql/lossless/mod.rs`), which exists because
/// `LosslessTokenKind::Boolean` was declared with no `LosslessToken` able to produce it.
/// `SyntacticTokenKind` has no such gap today — trivia kinds don't exist here at all, since
/// `SyntacticToken` skips trivia — so this is a regression guard, not a fix. See that test for
/// why the `census!` macro shape (one variant list driving both an exhaustive `match` and the
/// round-trip assertions) is the point: a variant added without a case here fails to compile.
#[test]
fn syntactic_token_kind_census() {
  macro_rules! census {
    ($($variant:ident => $token:expr),+ $(,)?) => {
      fn sample(kind: SyntacticTokenKind) -> SyntacticToken<&'static str> {
        match kind {
          $(SyntacticTokenKind::$variant => $token,)+
        }
      }

      $(
        assert_eq!(
          sample(SyntacticTokenKind::$variant).kind(),
          SyntacticTokenKind::$variant,
          "SyntacticTokenKind::{} is declared but its sample token maps to a different kind",
          stringify!($variant),
        );
      )+
    };
  }

  census! {
    Ampersand => SyntacticToken::Ampersand,
    At => SyntacticToken::At,
    RBrace => SyntacticToken::RBrace,
    RBracket => SyntacticToken::RBracket,
    RParen => SyntacticToken::RParen,
    Colon => SyntacticToken::Colon,
    Dollar => SyntacticToken::Dollar,
    Equal => SyntacticToken::Equal,
    Bang => SyntacticToken::Bang,
    LBrace => SyntacticToken::LBrace,
    LBracket => SyntacticToken::LBracket,
    LParen => SyntacticToken::LParen,
    Pipe => SyntacticToken::Pipe,
    Spread => SyntacticToken::Spread,
    Identifier => SyntacticToken::Identifier("x"),
    Float => SyntacticToken::LitFloat("1.0"),
    Int => SyntacticToken::LitInt("1"),
    InlineString => SyntacticToken::LitInlineStr(LitInlineStr::Plain(LitPlainStr::new("\"s\""))),
    BlockString => SyntacticToken::LitBlockStr(LitBlockStr::Plain(LitPlainStr::new("\"\"\"b\"\"\""))),
  }
}
