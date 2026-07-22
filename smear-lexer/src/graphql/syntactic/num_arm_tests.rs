use tokora::{Lexer as _, SimpleSpan};

use crate::graphql::{
  error::{FloatError, LexerErrorData},
  syntactic::{SyntacticLexer, SyntacticToken},
};

fn lex_all(src: &[u8]) -> Vec<SyntacticToken<&[u8]>> {
  let mut lexer = SyntacticLexer::<[u8]>::new(src);
  let mut out = Vec::new();
  while let Some(tok) = lexer.lex() {
    out.push(tok.unwrap());
  }
  out
}

#[test]
fn valid_int_fast_path() {
  // { a ( x : 10 ) } -- 8 tokens; the number is the 6th.
  let toks = lex_all(b"{ a(x: 10) }");
  assert_eq!(toks.len(), 8);
  assert!(matches!(&toks[5], SyntacticToken::LitInt(s) if *s == b"10"));
}

#[test]
fn valid_float_fast_path() {
  let toks = lex_all(b"{ a(x: 3.14) }");
  assert_eq!(toks.len(), 8);
  assert!(matches!(&toks[5], SyntacticToken::LitFloat(s) if *s == b"3.14"));
}

#[test]
fn negative_int_and_float_fast_path() {
  let toks = lex_all(b"{ a(x: -5) }");
  assert!(matches!(&toks[5], SyntacticToken::LitInt(s) if *s == b"-5"));

  let toks = lex_all(b"{ a(x: -2.5) }");
  assert!(matches!(&toks[5], SyntacticToken::LitFloat(s) if *s == b"-2.5"));
}

#[test]
fn number_anomalies_still_delegate_and_error() {
  // Leading zeros, an illegal ident suffix, and a lone `-` are all
  // anomalies `scan_number` refuses to fast-path -- confirm the dispatch
  // still routes them to Logos and gets back an error (the exact shape
  // is already covered byte-for-byte by the oracle tests).
  for src in [b"007" as &[u8], b"123abc", b"-", b"1.5x", b"00.5"] {
    let mut lexer = SyntacticLexer::<[u8]>::new(src);
    let tok = lexer.lex().expect("one token").expect_err("should error");
    let _ = tok; // shape is oracle-verified; here we only need "it errors".
  }
}

#[test]
fn dot_led_float_delegates_to_missing_integer_part_not_spread_error() {
  // `.5` must NOT be treated as a lone `.` (unterminated spread operator)
  // -- it's a Float literal missing its integer part, and Logos must be
  // the one to say so.
  let mut lexer = SyntacticLexer::<[u8]>::new(b".5");
  let err = lexer.lex().unwrap().unwrap_err();
  assert_eq!(lexer.error_span(), Some(SimpleSpan::new(0, 2)));
  assert!(
    matches!(
      err.first().map(|e| e.data()),
      Some(LexerErrorData::Float(FloatError::MissingIntegerPart))
    ),
    "expected Float(MissingIntegerPart), got {err:?}"
  );
}

#[test]
fn dot_dot_and_lone_dot_are_unaffected_by_the_digit_check() {
  // `..` (not `...`) is still the pre-existing unterminated-spread error,
  // and lexing resumes correctly on whatever follows it (the digit `5`
  // is not part of the `..` error -- it's the *next* token).
  let mut lexer = SyntacticLexer::<[u8]>::new(b"..5");
  let first = lexer.lex().unwrap();
  assert!(first.is_err());
  assert_eq!(lexer.error_span(), Some(SimpleSpan::new(0, 2)));

  let second = lexer.lex().unwrap();
  assert!(matches!(second, Ok(SyntacticToken::LitInt(s)) if s == b"5"));

  // A lone `.` followed by a non-digit is still the same error too.
  let mut lexer = SyntacticLexer::<[u8]>::new(b".x");
  let first = lexer.lex().unwrap();
  assert!(first.is_err());
  assert_eq!(lexer.error_span(), Some(SimpleSpan::new(0, 1)));

  let second = lexer.lex().unwrap();
  assert!(matches!(second, Ok(SyntacticToken::Identifier(s)) if s == b"x"));
}

#[test]
fn spread_operator_still_fast_paths() {
  let toks = lex_all(b"{ ...Frag }");
  assert!(toks.contains(&SyntacticToken::Spread));
}
