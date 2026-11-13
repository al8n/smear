//! How far a string literal reaches and how much its record holds, through both of this dialect's
//! lexers and both kinds of source: an inline string ends at its first line terminator, before it,
//! and the next line lexes as it would anywhere else; and a literal's record keeps its first
//! sixteen errors and one entry counting the rest.

use std::{format, string::String, vec, vec::Vec};

use tokora::{Lexer as _, SimpleSpan};

use crate::{
  SyntaxErrorKind,
  error::StringError,
  graphql::{error::LexerErrorData, lossless::LosslessLexer, syntactic::SyntacticLexer},
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

/// A refused literal's record: the kinds of the errors it holds, in order, and the count its last
/// entry holds when that entry is the one that counts the rest.
type Record = (Vec<SyntaxErrorKind>, Option<usize>);

/// The record of the string literal a lexer's input opens with, which it refuses.
macro_rules! first_record {
  ($lexer:expr) => {{
    let mut lexer = $lexer;
    let errors = match lexer.lex() {
      Some(Err(errors)) => errors,
      other => panic!("expected a refusal, got {other:?}"),
    };
    let [error] = errors.as_slice() else {
      panic!("one lexer error per refusal, got {errors:?}");
    };
    let LexerErrorData::String(errs) = error.data() else {
      panic!("a string literal's refusal, got {error:?}");
    };
    let omitted = match errs.last() {
      Some(StringError::Omitted(count)) => Some(*count),
      _ => None,
    };
    let record: Record = (errs.iter().map(|e| e.kind()).collect(), omitted);
    record
  }};
}

/// The record each of this dialect's four lexers refuses the literal `src` opens with, with the
/// lexer's name.
fn every_first_record(src: &str) -> [(&'static str, Record); 4] {
  [
    (
      "syntactic text",
      first_record!(SyntacticLexer::<str>::new(src)),
    ),
    (
      "syntactic bytes",
      first_record!(SyntacticLexer::<[u8]>::new(src.as_bytes())),
    ),
    (
      "lossless text",
      first_record!(LosslessLexer::<&str>::new(src)),
    ),
    (
      "lossless bytes",
      first_record!(LosslessLexer::<&[u8]>::new(src.as_bytes())),
    ),
  ]
}

/// A literal's record keeps its first sixteen errors and closes with one entry counting the rest,
/// exactly, through every lexer: a thousand unknown escapes are sixteen and 984 more, and one fault
/// more, an escape or the missing closing quote, makes it 985.
#[test]
fn a_literal_keeps_sixteen_errors_and_counts_the_rest() {
  let sixteen = vec![SyntaxErrorKind::UnknownEscape; 16];
  for (src, omitted) in [
    (format!("\"{}\"", "\\q".repeat(1000)), 984),
    (format!("\"{}\"", "\\q".repeat(1001)), 985),
    (format!("\"{}", "\\q".repeat(1000)), 985),
  ] {
    for (lexer, (kinds, count)) in every_first_record(&src) {
      assert_eq!(kinds.len(), 17, "{lexer}: {omitted}");
      assert_eq!(kinds[..16], sixteen[..], "{lexer}: {omitted}");
      assert_eq!(count, Some(omitted), "{lexer}");
    }
  }
}

/// A block literal in a byte source whose content is a thousand bytes that begin no UTF-8 sequence
/// keeps sixteen of them and counts the other 984, through both byte lexers.
#[test]
fn a_block_literal_keeps_sixteen_errors_and_counts_the_rest() {
  let mut src = b"\"\"\"".to_vec();
  src.resize(3 + 1000, 0xFF);
  src.extend_from_slice(b"\"\"\"");
  for (lexer, (kinds, count)) in [
    (
      "syntactic bytes",
      first_record!(SyntacticLexer::<[u8]>::new(src.as_slice())),
    ),
    (
      "lossless bytes",
      first_record!(LosslessLexer::<&[u8]>::new(src.as_slice())),
    ),
  ] {
    assert_eq!(kinds.len(), 17, "{lexer}");
    assert!(
      kinds[..16]
        .iter()
        .all(|kind| *kind == SyntaxErrorKind::InvalidUtf8),
      "{lexer}: {kinds:?}"
    );
    assert_eq!(count, Some(984), "{lexer}");
  }
}
