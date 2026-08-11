use std::{vec, vec::Vec};

use tokora::Lexer as _;

use crate::lexer::graphqlx::{
  LitFloat, LitInt,
  syntactic::{SyntacticLexer, SyntacticToken},
};

/// Drive a `<str>`-sourced lexer to completion, unwrapping every result.
fn lex_all(src: &str) -> Vec<SyntacticToken<&str>> {
  let mut lexer = SyntacticLexer::<str>::new(src);
  let mut out = Vec::new();
  while let Some(tok) = lexer.lex() {
    out.push(tok.unwrap());
  }
  out
}

#[test]
fn path_separator_vs_colon() {
  assert_eq!(
    lex_all("a::b"),
    vec![
      SyntacticToken::Identifier("a"),
      SyntacticToken::PathSeparator,
      SyntacticToken::Identifier("b"),
    ]
  );
  assert_eq!(
    lex_all("a: b"),
    vec![
      SyntacticToken::Identifier("a"),
      SyntacticToken::Colon,
      SyntacticToken::Identifier("b"),
    ]
  );
  // `:::` splits as `::` (PathSeparator) then a lone `:` (Colon).
  assert_eq!(
    lex_all(":::"),
    vec![SyntacticToken::PathSeparator, SyntacticToken::Colon]
  );
}

#[test]
fn fat_arrow_vs_equal() {
  assert_eq!(
    lex_all("<K => V>"),
    vec![
      SyntacticToken::LAngle,
      SyntacticToken::Identifier("K"),
      SyntacticToken::FatArrow,
      SyntacticToken::Identifier("V"),
      SyntacticToken::RAngle,
    ]
  );
  assert_eq!(lex_all("x = 1")[1], SyntacticToken::Equal);
  // `==` is two separate `Equal` tokens, never a fat arrow.
  assert_eq!(
    lex_all("=="),
    vec![SyntacticToken::Equal, SyntacticToken::Equal]
  );
}

#[test]
fn angle_brackets_carry_generics() {
  assert_eq!(
    lex_all("Box<T>"),
    vec![
      SyntacticToken::Identifier("Box"),
      SyntacticToken::LAngle,
      SyntacticToken::Identifier("T"),
      SyntacticToken::RAngle,
    ]
  );
}

#[test]
fn plus_is_always_an_operator() {
  // `+` is never a number sign: `+5` is Plus then Int(5).
  assert_eq!(
    lex_all("+5"),
    vec![
      SyntacticToken::Plus,
      SyntacticToken::LitInt(LitInt::Decimal("5")),
    ]
  );
}

#[test]
fn minus_before_digit_is_a_negative_literal() {
  assert_eq!(
    lex_all("-5"),
    vec![SyntacticToken::LitInt(LitInt::Decimal("-5"))]
  );
  assert_eq!(
    lex_all("-0xFF"),
    vec![SyntacticToken::LitInt(LitInt::Hex("-0xFF"))]
  );
  assert_eq!(
    lex_all("-2.5"),
    vec![SyntacticToken::LitFloat(LitFloat::Decimal("-2.5"))]
  );
}

#[test]
fn minus_before_non_digit_is_the_operator() {
  assert_eq!(
    lex_all("- 4"),
    vec![
      SyntacticToken::Minus,
      SyntacticToken::LitInt(LitInt::Decimal("4")),
    ]
  );
  assert_eq!(
    lex_all("-x"),
    vec![SyntacticToken::Minus, SyntacticToken::Identifier("x")]
  );
  assert_eq!(lex_all("-"), vec![SyntacticToken::Minus]);
}

#[test]
fn radix_numbers_delegate_to_logos() {
  assert_eq!(
    lex_all("0xFF"),
    vec![SyntacticToken::LitInt(LitInt::Hex("0xFF"))]
  );
  assert_eq!(
    lex_all("0b1010"),
    vec![SyntacticToken::LitInt(LitInt::Binary("0b1010"))]
  );
  assert_eq!(
    lex_all("0o755"),
    vec![SyntacticToken::LitInt(LitInt::Octal("0o755"))]
  );
  assert_eq!(
    lex_all("0x1.8p3"),
    vec![SyntacticToken::LitFloat(LitFloat::Hex("0x1.8p3"))]
  );
}

#[test]
fn asterisk_and_spread() {
  assert_eq!(lex_all("*"), vec![SyntacticToken::Asterisk]);
  assert_eq!(
    lex_all("...x"),
    vec![SyntacticToken::Spread, SyntacticToken::Identifier("x")]
  );
}

#[test]
fn dot_forms_delegate_for_the_exact_error() {
  // `..` and a lone `.` are unterminated-spread errors; `.5` is a
  // missing-integer-part float. All are delegated, so the fast path never
  // mis-reports one as another.
  let mut lexer = SyntacticLexer::<str>::new("..5");
  let first = lexer.lex().unwrap();
  assert!(first.is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(0, 2)));
  let second = lexer.lex().unwrap();
  assert_eq!(second, Ok(SyntacticToken::LitInt(LitInt::Decimal("5"))));

  let mut lexer = SyntacticLexer::<str>::new(".5");
  assert!(lexer.lex().unwrap().is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(0, 2)));
}

#[test]
fn inline_string_fast_path() {
  let toks = lex_all("\"hello\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(&toks[0], SyntacticToken::LitInlineStr(_)));

  // An escaped inline string still fast-paths (its normalization metadata is
  // derived by the shared `skip_inline_str_simd`).
  let toks = lex_all("\"a\\nb\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(&toks[0], SyntacticToken::LitInlineStr(_)));
}

#[test]
fn block_string_delegates() {
  let toks = lex_all("\"\"\"a block\"\"\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(&toks[0], SyntacticToken::LitBlockStr(_)));
}

#[test]
fn byte_source_matches_str_shapes() {
  // The `<[u8]>` source drives the same dispatch; spot-check a generic path
  // token and a path separator survive the byte flavor.
  let mut lexer = SyntacticLexer::<[u8]>::new(b"a::B<C>");
  let mut kinds = Vec::new();
  while let Some(tok) = lexer.lex() {
    kinds.push(tok.unwrap().kind());
  }
  use crate::lexer::graphqlx::syntactic::SyntacticTokenKind as K;
  assert_eq!(
    kinds,
    vec![
      K::Identifier,
      K::PathSeparator,
      K::Identifier,
      K::LAngle,
      K::Identifier,
      K::RAngle,
    ]
  );
}

#[test]
fn identifier_capability_classifies_tokens() {
  use tokora::token::IdentifierToken;
  // UFCS on `IdentifierToken`: the `IsVariant` derive also generates an inherent
  // `is_identifier`, so method-call syntax would resolve to that instead of the trait.
  assert!(IdentifierToken::is_identifier(
    &SyntacticToken::<&str>::Identifier("x")
  ));
  assert!(!IdentifierToken::is_identifier(
    &SyntacticToken::<&str>::LBrace
  ));
}

#[test]
fn keyword_literal_and_punctuator_capabilities_are_mapped() {
  use tokora::token::{KeywordToken, LitToken as _, PunctuatorToken};

  let true_token = SyntacticToken::<&str>::Identifier("true");
  assert_eq!(true_token.keyword(), Some("true"));
  assert!(true_token.is_true_literal());
  assert!(true_token.is_boolean_literal());
  assert!(SyntacticToken::<&str>::Identifier("false").is_false_literal());
  assert!(SyntacticToken::<&str>::Identifier("null").is_null_literal());

  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_angle(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::LAngle)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_angle(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::RAngle)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_brace(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::LBrace)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_brace(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::RBrace)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_paren(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::LParen)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_paren(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::RParen)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_bracket(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::LBracket)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_bracket(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::RBracket)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::at(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::At)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::fat_arrow(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::FatArrow)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::double_colon(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::PathSeparator)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::asterisk(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Asterisk)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::ampersand(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Ampersand)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::colon(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Colon)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::dollar(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Dollar)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::equal(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Equal)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::exclamation(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Bang)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::plus(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Plus)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::minus(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Minus)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::pipe(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Pipe)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::spread(),
    Some(crate::lexer::graphqlx::syntactic::SyntacticTokenKind::Spread)
  );
}

/// Census: every declared `SyntacticTokenKind` variant must be producible by some
/// `SyntacticToken`, i.e. by some arm of `SyntacticToken::kind`.
///
/// Mirrors `lossless_token_kind_census` (`graphqlx/lossless/mod.rs`), added after the graphql
/// (non-x) dialect shipped a `LosslessTokenKind::Boolean` with no `LosslessToken` able to
/// produce it. `SyntacticTokenKind` has no such gap today — trivia kinds don't exist here at
/// all, since `SyntacticToken` skips trivia — so this is a regression guard, not a fix. The
/// `census!` macro shape (one variant list driving both an exhaustive `match` and the
/// round-trip assertions) means a variant added without a case here fails to compile, naming
/// the variant, instead of silently staying untested.
#[test]
fn syntactic_token_kind_census() {
  use crate::lexer::{
    LitBlockStr, LitInlineStr, LitPlainStr, graphqlx::syntactic::SyntacticTokenKind,
  };

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
    Identifier => SyntacticToken::Identifier("x"),
    Int => SyntacticToken::LitInt(LitInt::Decimal("1")),
    Float => SyntacticToken::LitFloat(LitFloat::Decimal("1.0")),
    InlineString => SyntacticToken::LitInlineStr(LitInlineStr::Plain(LitPlainStr::new("\"s\""))),
    BlockString => SyntacticToken::LitBlockStr(LitBlockStr::Plain(LitPlainStr::new("\"\"\"b\"\"\""))),
    Dollar => SyntacticToken::Dollar,
    FatArrow => SyntacticToken::FatArrow,
    LAngle => SyntacticToken::LAngle,
    RAngle => SyntacticToken::RAngle,
    LParen => SyntacticToken::LParen,
    RParen => SyntacticToken::RParen,
    Spread => SyntacticToken::Spread,
    Colon => SyntacticToken::Colon,
    Equal => SyntacticToken::Equal,
    Asterisk => SyntacticToken::Asterisk,
    At => SyntacticToken::At,
    LBracket => SyntacticToken::LBracket,
    RBracket => SyntacticToken::RBracket,
    LBrace => SyntacticToken::LBrace,
    RBrace => SyntacticToken::RBrace,
    Pipe => SyntacticToken::Pipe,
    Bang => SyntacticToken::Bang,
    Ampersand => SyntacticToken::Ampersand,
    Plus => SyntacticToken::Plus,
    Minus => SyntacticToken::Minus,
    PathSeparator => SyntacticToken::PathSeparator,
  }
}
