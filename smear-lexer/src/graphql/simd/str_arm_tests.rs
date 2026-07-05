use tokit::lexer::Lexer;

use crate::{
  LitInlineStr,
  graphql::{simd::SimdSyntacticLexer, syntactic::SyntacticToken},
};

fn lex_all(src: &[u8]) -> Vec<SyntacticToken<&[u8]>> {
  let mut lexer = SimdSyntacticLexer::<[u8]>::new(src);
  let mut out = Vec::new();
  while let Some(tok) = lexer.lex() {
    out.push(tok.unwrap());
  }
  out
}

#[test]
fn plain_inline_string() {
  let toks = lex_all(b"\"hello\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(
    &toks[0],
    SyntacticToken::LitInlineStr(LitInlineStr::Plain(s)) if s.as_bytes() == b"\"hello\""
  ));
}

#[test]
fn empty_inline_string() {
  let toks = lex_all(b"\"\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(
    &toks[0],
    SyntacticToken::LitInlineStr(LitInlineStr::Plain(s)) if s.as_bytes() == b"\"\""
  ));
}

#[test]
fn escaped_inline_string() {
  let toks = lex_all(b"\"hello\\nworld\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(
    &toks[0],
    SyntacticToken::LitInlineStr(LitInlineStr::Complex(_))
  ));
}

#[test]
fn inline_string_in_query() {
  let toks = lex_all(b"{ search(q: \"foo\") { id } }");
  let strings: Vec<_> = toks
    .iter()
    .filter(|t| matches!(t, SyntacticToken::LitInlineStr(_)))
    .collect();
  assert_eq!(strings.len(), 1);
  assert!(matches!(
    strings[0],
    SyntacticToken::LitInlineStr(LitInlineStr::Plain(s)) if s.as_bytes() == b"\"foo\""
  ));
}

#[test]
fn lone_quote_at_eof_is_error() {
  let mut lexer = SimdSyntacticLexer::<[u8]>::new(b"\"");
  let tok = lexer.lex().unwrap();
  assert!(tok.is_err());
  assert_eq!(lexer.error_span(), Some(tokit::SimpleSpan::new(0, 1)));
}

#[test]
fn span_not_clobbered_by_error() {
  // After a valid token followed by an error, span() must still return the
  // valid token's span, and error_span() must return the error's span.
  let src = b"hello \"unterminated";
  let mut lexer = SimdSyntacticLexer::<[u8]>::new(src);

  // First token: identifier "hello" at 0..5.
  let first = lexer.lex().unwrap();
  assert!(first.is_ok());
  let valid_span = lexer.span();
  assert_eq!(valid_span, tokit::SimpleSpan::new(0, 5));
  assert!(lexer.error_span().is_none());

  // Second token: unterminated inline string → error.
  let second = lexer.lex().unwrap();
  assert!(second.is_err());

  // span() must still reflect the last *valid* token.
  assert_eq!(lexer.span(), valid_span);
  // error_span() must reflect the error token (opening " is at byte 6).
  let err_span = lexer.error_span().expect("error_span should be set");
  assert_eq!(err_span, tokit::SimpleSpan::new(6, src.len()));
}
