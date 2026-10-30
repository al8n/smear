//! smear#246 again, through GraphQLx's own copy of the string dispatch arms.
//!
//! The scanners and the Logos sub-lexers are shared with GraphQL, but the arms that pick a scanner,
//! read its answer and delegate a refusal are this dialect's, so the whole table runs here too: the
//! fast path and the Logos path refuse the same literals, and refuse what `core::str::from_utf8`
//! refuses. `graphql/syntactic/utf8_tests.rs` holds the rest of the matrix.

use core::ops::Range;

use std::vec::Vec;

use tokora::{Lexer as _, SimpleSpan, state::RecursionLimitExceeded, utils::Lexeme};

use crate::{
  LitBlockStr, LitInlineStr, LitStr,
  error::{StringError, StringErrors},
  graphqlx::{
    error::{LexerErrorData, LexerErrors},
    lossless::LosslessLexer,
    syntactic::{SyntacticLexer, SyntacticToken},
  },
  string_lexer::utf8_cases,
};

/// The string errors of a delegated refusal, which come as one `String` lexer error.
fn string_errors(errors: &LexerErrors<u8, RecursionLimitExceeded>) -> StringErrors<u8> {
  match errors.first().map(|error| error.data()) {
    Some(LexerErrorData::String(errs)) if errors.len() == 1 => errs.clone(),
    _ => panic!("expected one string error, got {errors:?}"),
  }
}

/// The fast path's answer for the inline literal `doc` opens with.
fn syntactic_inline(doc: &[u8]) -> Result<LitInlineStr<&[u8]>, StringErrors<u8>> {
  match SyntacticLexer::<[u8]>::new(doc).lex() {
    Some(Ok(SyntacticToken::LitInlineStr(lit))) => Ok(lit),
    Some(Err(errors)) => Err(string_errors(&errors)),
    other => panic!("expected an inline literal or its errors, got {other:?}"),
  }
}

/// The fast path's answer for the block literal `doc` opens with.
fn syntactic_block(doc: &[u8]) -> Result<LitBlockStr<&[u8]>, StringErrors<u8>> {
  match SyntacticLexer::<[u8]>::new(doc).lex() {
    Some(Ok(SyntacticToken::LitBlockStr(lit))) => Ok(lit),
    Some(Err(errors)) => Err(string_errors(&errors)),
    other => panic!("expected a block literal or its errors, got {other:?}"),
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

#[test]
fn inline_literals_are_refused_alike_on_both_paths() {
  for case in utf8_cases::table() {
    let doc = case.inline();
    let simd = syntactic_inline(&doc);
    let logos = match LitStr::try_from(doc.as_slice()) {
      Ok(LitStr::Inline(lit)) => Ok(lit),
      Ok(LitStr::Block(lit)) => panic!("{case}: expected an inline literal, got {lit:?}"),
      Err(errs) => Err(errs),
    };
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
}

#[test]
fn block_literals_are_refused_alike_on_both_paths() {
  for case in utf8_cases::table() {
    let doc = case.block();
    let simd = syntactic_block(&doc);
    let logos = match LitStr::try_from(doc.as_slice()) {
      Ok(LitStr::Block(lit)) => Ok(lit),
      Ok(LitStr::Inline(lit)) => panic!("{case}: expected a block literal, got {lit:?}"),
      Err(errs) => Err(errs),
    };
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
}

/// This dialect's lossless byte lexer reaches the shared Logos path through `Lexable`, and refuses
/// alike.
#[test]
fn the_lossless_byte_lexer_refuses_ill_formed_text() {
  let cases: [(&[u8], Range<usize>); 2] = [(b"\"a\xffb\"", 2..3), (b"\"\"\"a\xffb\"\"\"", 4..5)];
  for (doc, at) in cases {
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
