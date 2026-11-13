//! How far a string literal reaches, through both of this dialect's lexers and both kinds of
//! source: an inline string ends at its first line terminator, before it, and the next line lexes
//! as it would anywhere else.

use std::{format, string::String, vec, vec::Vec};

use tokora::{Lexer as _, SimpleSpan};

use crate::{
  SyntaxErrorKind,
  graphqlx::{error::LexerErrorData, lossless::LosslessLexer, syntactic::SyntacticLexer},
};

/// One item a lexer answers: its span, and for a refused string literal the kinds of the errors
/// its record holds, in order.
type Item = (SimpleSpan, Option<Vec<SyntaxErrorKind>>);

/// Every item a lexer answers over its whole input.
macro_rules! items {
  ($lexer:expr) => {{
    let mut lexer = $lexer;
    let mut items: Vec<Item> = Vec::new();
    while let Some(item) = lexer.lex() {
      items.push(match item {
        Ok(_) => (lexer.span(), None),
        Err(errors) => {
          let [error] = errors.as_slice() else {
            panic!("one lexer error per refusal, got {errors:?}");
          };
          let LexerErrorData::String(errs) = error.data() else {
            panic!("a string literal's refusal, got {error:?}");
          };
          (error.span(), Some(errs.iter().map(|e| e.kind()).collect()))
        }
      });
    }
    items
  }};
}

/// What each of this dialect's four lexers answers for `src`, with the lexer's name: the syntactic
/// and the lossless one, each over the text and over its bytes.
fn every_lexer(src: &str) -> [(&'static str, Vec<Item>); 4] {
  [
    ("syntactic text", items!(SyntacticLexer::<str>::new(src))),
    (
      "syntactic bytes",
      items!(SyntacticLexer::<[u8]>::new(src.as_bytes())),
    ),
    ("lossless text", items!(LosslessLexer::<&str>::new(src))),
    (
      "lossless bytes",
      items!(LosslessLexer::<&[u8]>::new(src.as_bytes())),
    ),
  ]
}

/// The text of `src` an item spans.
fn text(src: &str, span: SimpleSpan) -> &str {
  &src[span.start()..span.end()]
}

/// An inline string ends at its first line terminator, before it, with the one error that it is
/// unterminated, and the next line lexes as it would anywhere: the syntactic lexers skip the
/// terminator, and the lossless lexers' tokens still tile the input.
#[test]
fn an_inline_string_ends_at_its_line() {
  for terminator in ["\n", "\r", "\r\n"] {
    let src = format!("\"abc{terminator}type Query {{ a: Int }}");
    for (lexer, items) in every_lexer(&src) {
      assert_eq!(
        items[0],
        (
          SimpleSpan::new(0, 4),
          Some(vec![SyntaxErrorKind::UnterminatedString])
        ),
        "{lexer}: {src:?}"
      );
      for (span, errors) in &items[1..] {
        assert_eq!(*errors, None, "{lexer}: {src:?} at {span:?}");
      }
      let words: Vec<&str> = items[1..]
        .iter()
        .map(|&(span, _)| text(&src, span))
        .filter(|word| !word.trim().is_empty())
        .collect();
      assert_eq!(
        words,
        ["type", "Query", "{", "a", ":", "Int", "}"],
        "{lexer}: {src:?}"
      );
      if lexer.starts_with("lossless") {
        let tiled: String = items.iter().map(|&(span, _)| text(&src, span)).collect();
        assert_eq!(tiled, src, "{lexer}: the tokens tile the input");
      }
    }
  }
}

/// How many line feeds follow the unclosed quote below: a hundred thousand, and fewer under Miri,
/// which interprets every byte of them.
const LINES: usize = if cfg!(miri) { 64 } else { 100_000 };

/// A `"` before [`LINES`] line feeds is one literal, the `"` alone, whose record holds one error,
/// however many lines follow it. Each line feed after it is the lossless lexers' token and nothing
/// to the syntactic ones.
#[test]
fn an_unclosed_quote_holds_one_error_however_many_lines_follow() {
  let src = format!("\"{}", "\n".repeat(LINES));
  for (lexer, items) in every_lexer(&src) {
    assert_eq!(
      items[0],
      (
        SimpleSpan::new(0, 1),
        Some(vec![SyntaxErrorKind::UnterminatedString])
      ),
      "{lexer}"
    );
    let lines = if lexer.starts_with("lossless") {
      LINES
    } else {
      0
    };
    assert_eq!(items.len(), 1 + lines, "{lexer}");
    for (span, errors) in &items[1..] {
      assert_eq!(*errors, None, "{lexer} at {span:?}");
      assert_eq!(text(&src, *span), "\n", "{lexer} at {span:?}");
    }
  }
}

/// What came before the line's end stays in the record, ahead of the unterminated error: a `\`
/// that its line ends escapes nothing, and a `\u` or a `\u{` takes nothing from the next line.
#[test]
fn what_a_line_held_stays_in_its_literals_record() {
  for (src, before) in [
    ("\"abc\\\nx", SyntaxErrorKind::IncompleteEscape),
    ("\"\\u1\nx", SyntaxErrorKind::InvalidUnicodeEscape),
    ("\"\\u{1\nx", SyntaxErrorKind::InvalidUnicodeEscape),
  ] {
    let end = src.find('\n').expect("a line feed");
    for (lexer, items) in every_lexer(src) {
      assert_eq!(
        items[0],
        (
          SimpleSpan::new(0, end),
          Some(vec![before, SyntaxErrorKind::UnterminatedString])
        ),
        "{lexer}: {src:?}"
      );
      let last = items.last().expect("the next line's name");
      assert_eq!(text(src, last.0), "x", "{lexer}: {src:?}");
    }
  }
}
