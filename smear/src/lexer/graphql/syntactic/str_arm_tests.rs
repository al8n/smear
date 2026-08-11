use std::vec::Vec;

use tokora::lexer::Lexer;

use crate::lexer::{
  LitInlineStr,
  graphql::syntactic::{SyntacticLexer, SyntacticToken},
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
  let mut lexer = SyntacticLexer::<[u8]>::new(b"\"");
  let tok = lexer.lex().unwrap();
  assert!(tok.is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(0, 1)));
}

#[test]
fn span_and_slice_track_error_token() {
  // After an error, span()/slice() report the CURRENT (error) token, exactly
  // like `LogosLexer` — not the previous valid token. error_span() reports the
  // same span too.
  let src = b"hello \"unterminated";
  let mut lexer = SyntacticLexer::<[u8]>::new(src);

  // First token: identifier "hello" at 0..5.
  let first = lexer.lex().unwrap();
  assert!(first.is_ok());
  assert_eq!(lexer.span(), tokora::SimpleSpan::new(0, 5));
  assert!(lexer.error_span().is_none());

  // Second token: unterminated inline string → error. The opening `"` is at
  // byte 6 and the error token runs to end of input.
  let second = lexer.lex().unwrap();
  assert!(second.is_err());

  let err_span = tokora::SimpleSpan::new(6, src.len());
  // span()/slice() now reflect the error token (Logos parity).
  assert_eq!(lexer.span(), err_span);
  assert_eq!(lexer.slice(), &src[6..]);
  // error_span() still returns the error span too.
  assert_eq!(lexer.error_span(), Some(err_span));
}
