//! smear#246: a string literal in a byte source must be well-formed UTF-8, on both string paths.
//!
//! The SIMD fast path refuses a byte source's literal at its first ill-formed run and delegates the
//! whole literal to the Logos sub-lexer, which `expect_err`s it. So the two paths must refuse
//! exactly the same literals — a refusal only the fast path made is a panic — and both must refuse
//! exactly what `core::str::from_utf8` refuses. Every case in `utf8_cases::table` is held to both,
//! inline and block, here and again through the GraphQLx copy of the dispatch arms.

use core::ops::Range;

use std::{string::String, vec, vec::Vec};

use tokora::{Lexer as _, SimpleSpan, utils::Lexeme};

use crate::{
  LitBlockStr, LitInlineStr, LitStr,
  error::{StringError, StringErrors},
  graphql::{
    error::LexerErrorData,
    lossless::LosslessLexer,
    syntactic::{SyntacticLexer, SyntacticLexerErrors, SyntacticToken},
  },
  skip_block_str_from_bytes,
  string_lexer::utf8_cases::{self, Case},
};

/// Literals that are not UTF-8 in either kind, with where the one ill-formed byte is.
const ILL_FORMED: [(&[u8], Range<usize>); 2] =
  [(b"\"a\xffb\"", 2..3), (b"\"\"\"a\xffb\"\"\"", 4..5)];

/// The string errors of a delegated refusal, which come as one `String` lexer error.
fn string_errors(errors: &SyntacticLexerErrors<u8>) -> StringErrors<u8> {
  match errors.first().map(|error| error.data()) {
    Some(LexerErrorData::String(errs)) if errors.len() == 1 => errs.clone(),
    _ => panic!("expected one string error, got {errors:?}"),
  }
}

/// The fast path's answer for the inline literal `doc` opens with: the literal it emitted, or the
/// errors of the delegation that followed its refusal.
fn syntactic_inline(doc: &[u8]) -> Result<LitInlineStr<&[u8]>, StringErrors<u8>> {
  match SyntacticLexer::<[u8]>::new(doc).lex() {
    Some(Ok(SyntacticToken::LitInlineStr(lit))) => Ok(lit),
    Some(Err(errors)) => Err(string_errors(&errors)),
    other => panic!("expected an inline literal or its errors, got {other:?}"),
  }
}

/// [`syntactic_inline`] for a block literal.
fn syntactic_block(doc: &[u8]) -> Result<LitBlockStr<&[u8]>, StringErrors<u8>> {
  match SyntacticLexer::<[u8]>::new(doc).lex() {
    Some(Ok(SyntacticToken::LitBlockStr(lit))) => Ok(lit),
    Some(Err(errors)) => Err(string_errors(&errors)),
    other => panic!("expected a block literal or its errors, got {other:?}"),
  }
}

/// The Logos path's answer for the inline literal `doc` opens with, through `LitStr`'s byte door.
fn logos_inline(doc: &[u8]) -> Result<LitInlineStr<&[u8]>, StringErrors<u8>> {
  match LitStr::try_from(doc)? {
    LitStr::Inline(lit) => Ok(lit),
    LitStr::Block(lit) => panic!("expected an inline literal, got {lit:?}"),
  }
}

/// [`logos_inline`] for a block literal.
fn logos_block(doc: &[u8]) -> Result<LitBlockStr<&[u8]>, StringErrors<u8>> {
  match LitStr::try_from(doc)? {
    LitStr::Block(lit) => Ok(lit),
    LitStr::Inline(lit) => panic!("expected a block literal, got {lit:?}"),
  }
}

/// One `UnsupportedCharacter` per span, each moved to where the literal's content starts.
fn unsupported(spans: Vec<Range<usize>>, content_start: usize) -> Vec<StringError<u8>> {
  spans
    .into_iter()
    .map(|span| {
      StringError::UnsupportedCharacter(Lexeme::Range(SimpleSpan::new(
        content_start + span.start,
        content_start + span.end,
      )))
    })
    .collect()
}

/// The spans of the `UnsupportedCharacter` errors among `errs`, relative to the content start.
fn unsupported_spans(errs: &StringErrors<u8>, content_start: usize) -> Vec<Range<usize>> {
  errs
    .iter()
    .filter_map(|err| match err {
      StringError::UnsupportedCharacter(Lexeme::Range(span)) => {
        Some(span.start() - content_start..span.end() - content_start)
      }
      _ => None,
    })
    .collect()
}

/// The two inline paths against each other, and against `from_utf8` where nothing else can fail.
fn check_inline(case: &Case) {
  let doc = case.inline();
  let simd = syntactic_inline(&doc);
  let logos = logos_inline(&doc);
  assert_eq!(
    simd, logos,
    "{case}: the fast path and the Logos path disagree"
  );
  if case.is_plain_text() {
    let want = unsupported(utf8_cases::ill_formed(&case.body), 1);
    match logos {
      Ok(_) => assert!(want.is_empty(), "{case}: accepted, but not UTF-8"),
      Err(errs) => assert_eq!(errs.as_slice(), want.as_slice(), "{case}"),
    }
  }
}

/// The two block paths against each other and against `from_utf8`, which judges every block
/// literal in the table: block content has no escapes to fail.
fn check_block(case: &Case) {
  let doc = case.block();
  let scanned = skip_block_str_from_bytes::<true>(&doc[3..]);
  let simd = syntactic_block(&doc);
  let logos = logos_block(&doc);
  assert_eq!(
    scanned.is_ok(),
    simd.is_ok(),
    "{case}: the lexer and its scanner disagree"
  );
  assert_eq!(
    simd, logos,
    "{case}: the fast path and the Logos path disagree"
  );
  let want = unsupported(utf8_cases::ill_formed(&case.body), 3);
  match logos {
    Ok(_) => assert!(want.is_empty(), "{case}: accepted, but not UTF-8"),
    Err(errs) => assert_eq!(errs.as_slice(), want.as_slice(), "{case}"),
  }
}

#[test]
fn inline_literals_are_refused_alike_on_both_paths() {
  for case in utf8_cases::table() {
    check_inline(&case);
  }
}

#[test]
fn block_literals_are_refused_alike_on_both_paths() {
  for case in utf8_cases::table() {
    check_block(&case);
  }
}

/// The inline grammar's error tokens and the block walk measure the same sequence the same way.
#[test]
fn inline_and_block_literals_report_the_same_extents() {
  for case in utf8_cases::boundary_classes() {
    let inline = match syntactic_inline(&case.inline()) {
      Ok(_) => Vec::new(),
      Err(errs) => unsupported_spans(&errs, 1),
    };
    let block = match syntactic_block(&case.block()) {
      Ok(_) => Vec::new(),
      Err(errs) => unsupported_spans(&errs, 3),
    };
    assert_eq!(inline, block, "{case}");
  }

  for truncation in utf8_cases::TRUNCATIONS {
    let mut content = vec![b'a'];
    content.extend_from_slice(truncation);
    let mut block = b"\"\"\"".to_vec();
    block.extend_from_slice(&content);
    block.extend_from_slice(b"\"\"\"");
    let block = unsupported_spans(&syntactic_block(&block).expect_err("not UTF-8"), 3);

    // Before the closing quote, before an escape, and before the end of the input: the inline
    // grammar ends the error token at whichever comes first, the block walk at the run's end.
    for after in [&b"\""[..], b"\\n\"", b""] {
      let mut inline = b"\"".to_vec();
      inline.extend_from_slice(&content);
      inline.extend_from_slice(after);
      let inline = unsupported_spans(&syntactic_inline(&inline).expect_err("not UTF-8"), 1);
      assert_eq!(
        inline,
        block,
        "{} then {}",
        truncation.escape_ascii(),
        after.escape_ascii()
      );
    }
  }
}

/// A truncated sequence before an escape is one error, and the escape after it is still read.
#[test]
fn a_truncated_sequence_before_an_escape_is_one_error() {
  let doc = b"\"a\xe4\xb8\\nb\"";
  let want: [StringError<u8>; 1] = [StringError::UnsupportedCharacter(Lexeme::Range(
    SimpleSpan::new(2, 4),
  ))];
  assert_eq!(
    syntactic_inline(doc).expect_err("not UTF-8").as_slice(),
    &want
  );
  assert_eq!(logos_inline(doc).expect_err("not UTF-8").as_slice(), &want);
}

/// A backslash no escape rule takes is the same error from a text and a byte source, and the
/// ill-formed byte after it is an error of its own.
#[test]
fn a_dangling_backslash_reads_alike_through_delegation() {
  let text = match SyntacticLexer::<str>::new("\"abc\\").lex() {
    Some(Err(errors)) => match errors.first().map(|error| error.data()) {
      Some(LexerErrorData::String(errs)) => errs.clone(),
      other => panic!("expected a string error, got {other:?}"),
    },
    other => panic!("expected a refusal, got {other:?}"),
  };
  assert_eq!(
    text.as_slice(),
    &[
      StringError::<char>::default(),
      StringError::unterminated_inline_string()
    ]
  );
  assert_eq!(
    syntactic_inline(b"\"abc\\")
      .expect_err("unterminated")
      .as_slice(),
    &[
      StringError::<u8>::default(),
      StringError::unterminated_inline_string()
    ]
  );

  assert_eq!(
    syntactic_inline(b"\"\\\xff\"")
      .expect_err("not UTF-8")
      .as_slice(),
    &[
      StringError::default(),
      StringError::UnsupportedCharacter(Lexeme::Range(SimpleSpan::new(2, 3)))
    ]
  );
}

/// Ill-formed bytes in place in a block literal: on several lines, beside the `\"""` escape, and
/// right before the close.
#[test]
fn block_literals_report_each_sequence_in_place() {
  let cases: [(&[u8], &[Range<usize>]); 3] = [
    (
      b"\"\"\"a\xffb\n c\xfe\n\xc3\nd\"\"\"",
      &[4..5, 9..10, 11..12],
    ),
    (b"\"\"\"a\xff\\\"\"\"\xfeb\"\"\"", &[4..5, 9..10]),
    (b"\"\"\"a\xffb\xf0\x9f\x98\"\"\"", &[4..5, 6..9]),
  ];
  for (doc, spans) in cases {
    let want = unsupported(spans.to_vec(), 0);
    assert_eq!(
      syntactic_block(doc).expect_err("not UTF-8").as_slice(),
      want.as_slice(),
      "{}",
      doc.escape_ascii()
    );
    assert_eq!(
      logos_block(doc).expect_err("not UTF-8").as_slice(),
      want.as_slice(),
      "{}",
      doc.escape_ascii()
    );
  }
}

/// Only a terminated body is checked, so an unterminated one holding ill-formed bytes is refused as
/// unterminated and nothing else — by the scanner, and by the delegation after it, without a panic.
#[test]
fn an_unterminated_block_is_refused_as_unterminated_alone() {
  let doc = b"\"\"\"a\xffb\n\xfe";
  assert!(skip_block_str_from_bytes::<true>(&doc[3..]).is_err());
  assert_eq!(
    syntactic_block(doc).expect_err("unterminated").as_slice(),
    &[StringError::unterminated_block_string()]
  );
  assert_eq!(
    logos_block(doc).expect_err("unterminated").as_slice(),
    &[StringError::unterminated_block_string()]
  );
}

/// The refusal one byte source's lexer must give, as the one error it carries.
fn assert_refused<T: core::fmt::Debug>(
  item: Option<Result<T, SyntacticLexerErrors<u8>>>,
  at: &Range<usize>,
  source: &str,
) {
  match item {
    Some(Err(errors)) => assert_eq!(
      string_errors(&errors).as_slice(),
      &[StringError::UnsupportedCharacter(Lexeme::Range(
        SimpleSpan::new(at.start, at.end)
      ))],
      "{source}"
    ),
    other => panic!("{source}: expected a refusal, got {other:?}"),
  }
}

/// Every byte source refuses, as an error rather than a panic. A source whose `UTF8` were wrongly
/// `true` would accept these unchecked.
#[test]
fn a_byte_slice_source_refuses_ill_formed_text() {
  for (doc, at) in ILL_FORMED {
    assert_refused(SyntacticLexer::<[u8]>::new(doc).lex(), &at, "[u8]");
  }
}

#[cfg(feature = "bytes")]
#[test]
fn a_bytes_source_refuses_ill_formed_text() {
  for (doc, at) in ILL_FORMED {
    let owned = bytes::Bytes::copy_from_slice(doc);
    assert_refused(
      SyntacticLexer::<bytes::Bytes>::new(&owned).lex(),
      &at,
      "Bytes",
    );
  }
}

#[cfg(feature = "bstr")]
#[test]
fn a_bstr_source_refuses_ill_formed_text() {
  for (doc, at) in ILL_FORMED {
    assert_refused(
      SyntacticLexer::<bstr::BStr>::new(bstr::BStr::new(doc)).lex(),
      &at,
      "BStr",
    );
  }
}

#[cfg(feature = "hipstr")]
#[test]
fn a_hipbyt_source_refuses_ill_formed_text() {
  for (doc, at) in ILL_FORMED {
    let owned = hipstr::HipByt::from(doc);
    assert_refused(
      SyntacticLexer::<hipstr::HipByt<'_>>::new(&owned).lex(),
      &at,
      "HipByt",
    );
  }
}

/// The valid non-ASCII literals in the table, as text, both kinds.
fn valid_text() -> Vec<String> {
  let mut docs = Vec::new();
  for case in utf8_cases::table() {
    if case.is_plain_text() && !case.body.is_ascii() && core::str::from_utf8(&case.body).is_ok() {
      for doc in [case.inline(), case.block()] {
        docs.push(String::from_utf8(doc).expect("valid UTF-8"));
      }
    }
  }
  docs
}

/// A text source still emits every valid non-ASCII literal the table holds, unchecked.
#[test]
fn a_str_source_accepts_valid_text() {
  for doc in valid_text() {
    let mut lexer = SyntacticLexer::<str>::new(&doc);
    match lexer.lex() {
      Some(Ok(SyntacticToken::LitInlineStr(_) | SyntacticToken::LitBlockStr(_))) => {}
      other => panic!("{}: expected a literal, got {other:?}", doc.escape_debug()),
    }
    assert_eq!(
      lexer.span().end(),
      doc.trim_end().len(),
      "{}",
      doc.escape_debug()
    );
  }
}

#[cfg(feature = "hipstr")]
#[test]
fn a_hipstr_source_accepts_valid_text() {
  for doc in valid_text() {
    let owned = hipstr::HipStr::from(doc.as_str());
    let mut lexer = SyntacticLexer::<hipstr::HipStr<'_>>::new(&owned);
    match lexer.lex() {
      Some(Ok(SyntacticToken::LitInlineStr(_) | SyntacticToken::LitBlockStr(_))) => {}
      other => panic!("{}: expected a literal, got {other:?}", doc.escape_debug()),
    }
    assert_eq!(
      lexer.span().end(),
      doc.trim_end().len(),
      "{}",
      doc.escape_debug()
    );
  }
}

/// The lossless byte lexer reaches the same Logos path through `Lexable`, and refuses alike.
#[test]
fn the_lossless_byte_lexer_refuses_ill_formed_text() {
  for (doc, at) in ILL_FORMED {
    match LosslessLexer::<&[u8]>::new(doc).lex() {
      Some(Err(errors)) => match errors.first().map(|error| error.data()) {
        Some(LexerErrorData::String(errs)) => assert_eq!(
          errs.as_slice(),
          &[StringError::UnsupportedCharacter(Lexeme::Range(
            SimpleSpan::new(at.start, at.end)
          ))],
          "{}",
          doc.escape_ascii()
        ),
        other => panic!("expected a string error, got {other:?}"),
      },
      other => panic!("expected a refusal, got {other:?}"),
    }
  }
}
