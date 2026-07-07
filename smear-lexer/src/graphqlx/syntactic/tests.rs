use tokit::Lexer as _;

use crate::graphqlx::{
  LitFloat, LitInt,
  syntactic::{SimdSyntacticLexer, SyntacticToken},
};

/// Drive a `<str>`-sourced lexer to completion, unwrapping every result.
fn lex_all(src: &str) -> Vec<SyntacticToken<&str>> {
  let mut lexer = SimdSyntacticLexer::<str>::new(src);
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
  let mut lexer = SimdSyntacticLexer::<str>::new("..5");
  let first = lexer.lex().unwrap();
  assert!(first.is_err());
  assert_eq!(lexer.error_span(), Some(tokit::SimpleSpan::new(0, 2)));
  let second = lexer.lex().unwrap();
  assert_eq!(second, Ok(SyntacticToken::LitInt(LitInt::Decimal("5"))));

  let mut lexer = SimdSyntacticLexer::<str>::new(".5");
  assert!(lexer.lex().unwrap().is_err());
  assert_eq!(lexer.error_span(), Some(tokit::SimpleSpan::new(0, 2)));
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
  let mut lexer = SimdSyntacticLexer::<[u8]>::new(b"a::B<C>");
  let mut kinds = Vec::new();
  while let Some(tok) = lexer.lex() {
    kinds.push(tok.unwrap().kind());
  }
  use crate::graphqlx::syntactic::SyntacticTokenKind as K;
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
