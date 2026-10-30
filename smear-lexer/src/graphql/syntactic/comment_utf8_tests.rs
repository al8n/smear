//! smear#251: a comment in a byte source must be well-formed UTF-8, on both lexers.
//!
//! Draft §2.1.3 spells a comment's body as `SourceCharacter`s up to a line terminator. Over a byte
//! source both lexers walk the body with the same `IllFormedUtf8`: the syntactic lexer as it skips
//! the comment, the lossless lexer as it keeps it. An ill-formed body is refused with one
//! `UnknownLexeme` per sequence `core::str::from_utf8` finds, at that sequence's span, by one error
//! token that covers the whole comment, and the next line lexes as usual. Every case in
//! `utf8_cases::table` is held to that here, and again through the GraphQLx copy of the arm and
//! the rule.

use core::ops::Range;

use std::{vec, vec::Vec};

use tokora::{Lexer as _, SimpleSpan, utils::Lexeme};

use crate::{
  graphql::{
    error::{LexerErrorData, LexerErrors},
    lossless::{LosslessLexer, LosslessToken},
    syntactic::{SyntacticLexer, SyntacticToken},
  },
  string_lexer::utf8_cases::{self, Case, Unknown},
};

/// A comment every byte source must refuse, then the line after it.
const ILL_FORMED: &[u8] = b"#a\xffb\nx";

/// What [`ILL_FORMED`] is refused with: its one ill-formed byte.
const REFUSAL: [Unknown; 1] = [(2..3, 0xFF, 2)];

/// Well-formed comments, ASCII and not, each ended by a line terminator or the end of input: the
/// document, where its comment ends, and where the `x` on the next line starts, if there is one.
const VALID: [(&str, usize, Option<usize>); 7] = [
  ("# plain ascii\nx", 13, Some(14)),
  ("#\u{e9}\u{4e2d}\u{1f600}\r\nx", 10, Some(12)),
  ("#caf\u{e9} \u{feff}tail\rx", 14, Some(15)),
  ("# \"quoted\" \\ and # again\nx", 24, Some(25)),
  ("#\u{10ffff}", 5, None),
  ("#\n", 1, None),
  ("#", 1, None),
];

/// One item of the lossless byte lexer, projected: a token, or the errors of an error item; and
/// the item's span.
type Item<'a> = (Result<LosslessToken<&'a [u8]>, Vec<Unknown>>, Range<usize>);

/// A line terminator, and the lossless token it lexes as; none for the end of input.
type Terminator = (&'static [u8], Option<LosslessToken<&'static [u8]>>);

/// The `UnknownLexeme` errors an error item carries, projected. The two lexers' containers differ
/// in their state error, so what is compared across them is the projection.
fn unknowns<State: core::fmt::Debug>(errors: &LexerErrors<u8, State>) -> Vec<Unknown> {
  errors
    .iter()
    .map(|error| match error.data() {
      LexerErrorData::UnknownLexeme(Lexeme::Char(ch)) => (
        error.span().start()..error.span().end(),
        ch.char(),
        ch.position(),
      ),
      other => panic!("expected an unknown lexeme, got {other:?}"),
    })
    .collect()
}

/// What the syntactic lexer makes of a document that opens with a comment and holds nothing after
/// it but trivia: the refusal and the span of its error token, or `None` when the comment was
/// skipped.
fn syntactic(doc: &[u8]) -> Option<(Vec<Unknown>, Range<usize>)> {
  let mut lexer = SyntacticLexer::<[u8]>::new(doc);
  let refused = match lexer.lex() {
    None => None,
    Some(Err(errors)) => {
      let span = lexer.span();
      assert_eq!(lexer.error_span(), Some(span), "{}", doc.escape_ascii());
      Some((unknowns(&errors), span.start()..span.end()))
    }
    Some(Ok(token)) => panic!("{}: expected no token, got {token:?}", doc.escape_ascii()),
  };
  assert!(
    lexer.lex().is_none(),
    "{}: more than trivia after the comment",
    doc.escape_ascii()
  );
  refused
}

/// Every item the lossless byte lexer yields for `doc`.
fn lossless(doc: &[u8]) -> Vec<Item<'_>> {
  let mut lexer = LosslessLexer::<&[u8]>::new(doc);
  let mut items = Vec::new();
  while let Some(item) = lexer.lex() {
    let span = lexer.span();
    items.push((
      item.map_err(|errors| unknowns(&errors)),
      span.start()..span.end(),
    ));
  }
  items
}

/// The first item the lossless byte lexer yields for `doc`, which opens with a comment.
fn lossless_first(doc: &[u8]) -> Item<'_> {
  let mut lexer = LosslessLexer::<&[u8]>::new(doc);
  let item = lexer
    .lex()
    .expect("a document that opens with a comment has an item");
  let span = lexer.span();
  (
    item.map_err(|errors| unknowns(&errors)),
    span.start()..span.end(),
  )
}

/// The slices of every item the lossless byte lexer yields for `doc`, error items' included,
/// concatenated.
fn lossless_text(doc: &[u8]) -> Vec<u8> {
  let mut lexer = LosslessLexer::<&[u8]>::new(doc);
  let mut text = Vec::new();
  while lexer.lex().is_some() {
    text.extend_from_slice(lexer.slice());
  }
  text
}

/// The two spellings of each case: a comment a line feed ends, with the case's tail after it, and
/// a comment the end of input ends.
fn spellings(case: &Case) -> [Vec<u8>; 2] {
  [case.comment(), case.comment_at_eof()]
}

/// Both lexers against `from_utf8` for the comment that opens `doc`, whose body is `body`: each
/// refuses exactly a body `from_utf8` refuses, with one error per sequence, and its token is the
/// whole comment either way.
fn check(label: &dyn core::fmt::Display, body: &[u8], doc: &[u8]) {
  let want = utf8_cases::comment_refusals(body);
  let end = 1 + body.len();

  match syntactic(doc) {
    None => assert!(want.is_empty(), "{label}: skipped, but not UTF-8"),
    Some((errors, span)) => {
      assert_eq!(errors, want, "{label}");
      assert_eq!(
        span,
        0..end,
        "{label}: the error token is not the whole comment"
      );
    }
  }

  let (first, span) = lossless_first(doc);
  assert_eq!(span, 0..end, "{label}: the token is not the whole comment");
  match first {
    Ok(LosslessToken::Comment(text)) => {
      assert!(want.is_empty(), "{label}: kept, but not UTF-8");
      assert_eq!(text, &doc[..end], "{label}");
    }
    Err(errors) => assert_eq!(errors, want, "{label}"),
    Ok(other) => panic!("{label}: expected the comment, got {other:?}"),
  }
}

/// Every comment the table spells, through both lexers, against `from_utf8`.
#[test]
fn comments_are_refused_as_from_utf8_refuses_them() {
  for case in utf8_cases::table() {
    if case.is_comment_text() {
      for doc in spellings(&case) {
        check(&case, &case.body, &doc);
      }
    }
  }
}

/// The two lexers refuse a comment with the same errors, compared by projection, and with the same
/// error token, over every comment the table spells.
#[test]
fn both_lexers_refuse_a_comment_with_the_same_errors() {
  for case in utf8_cases::table() {
    if case.is_comment_text() {
      for doc in spellings(&case) {
        let by_syntactic = syntactic(&doc);
        let by_lossless = match lossless_first(&doc) {
          (Ok(_), _) => None,
          (Err(errors), span) => Some((errors, span)),
        };
        assert_eq!(by_syntactic, by_lossless, "{case}: the lexers disagree");
      }
    }
  }
}

/// The lossless lexer gives back every byte of every comment the table spells, the error tokens'
/// bytes included.
#[test]
fn the_lossless_lexer_round_trips_every_comment() {
  for case in utf8_cases::table() {
    if case.is_comment_text() {
      for doc in spellings(&case) {
        assert_eq!(lossless_text(&doc), doc, "{case}");
      }
    }
  }
}

/// A non-ASCII run longer than a vector inside a comment, with a long tail after it: once
/// well-formed, and once with one byte broken at each run offset around the 32- and 64-byte seams.
#[test]
fn a_long_non_ascii_run_in_a_comment_is_checked_across_the_seams() {
  // 66 bytes: 22 three-byte sequences.
  let run = "\u{4e2d}".repeat(22).into_bytes();
  let mut runs = vec![run.clone()];
  for broken in [0usize, 31, 32, 63, 64] {
    let mut run = run.clone();
    run[broken] = 0xFF;
    runs.push(run);
  }
  for run in runs {
    let mut body = b"ab".to_vec();
    body.extend_from_slice(&run);
    body.extend_from_slice(b"cd");
    let mut doc = vec![b'#'];
    doc.extend_from_slice(&body);
    doc.push(b'\n');
    doc.resize(doc.len() + 80, b' ');
    check(&body.escape_ascii(), &body, &doc);
  }
}

/// All-ASCII comments around each vector width, each ended by a line feed, with and without a long
/// tail after it: both lexers end the comment at the line feed and lex what follows.
#[test]
fn ascii_comments_end_at_the_line_feed_around_each_vector_width() {
  for len in [15usize, 16, 17, 31, 32, 33, 63, 64, 65] {
    // The comment is `len` or `len + 1` bytes with its `#`. The body runs through printable ASCII
    // from `!`, so it holds `"` and `#`, which end nothing in a comment.
    for body_len in [len - 1, len] {
      let body: Vec<u8> = (b'!'..=b'~').cycle().take(body_len).collect();
      for tail in [false, true] {
        let mut doc = vec![b'#'];
        doc.extend_from_slice(&body);
        doc.push(b'\n');
        if tail {
          doc.push(b'x');
          doc.resize(doc.len() + 79, b' ');
        }
        let end = 1 + body_len;
        let label = doc.escape_ascii();

        let mut lexer = SyntacticLexer::<[u8]>::new(&doc);
        match (tail, lexer.lex()) {
          (false, None) => {}
          (true, Some(Ok(SyntacticToken::Identifier(name)))) => {
            assert_eq!(name, &b"x"[..], "{label}");
            assert_eq!(lexer.span(), SimpleSpan::new(end + 1, end + 2), "{label}");
          }
          (_, other) => panic!("{label}: expected the line after the comment, got {other:?}"),
        }

        let items = lossless(&doc);
        assert_eq!(
          items[0],
          (Ok(LosslessToken::Comment(&doc[..end])), 0..end),
          "{label}"
        );
        assert_eq!(
          items[1],
          (Ok(LosslessToken::Newline), end..end + 1),
          "{label}"
        );
        if tail {
          assert_eq!(
            items[2],
            (Ok(LosslessToken::Identifier(&b"x"[..])), end + 1..end + 2),
            "{label}"
          );
        }
      }
    }
  }
}

/// An ill-formed comment ended by each line terminator, and by the end of input.
/// Both lexers refuse it whole, then lex the terminator and the next line as usual; at the end of
/// input there is nothing after it.
#[test]
fn an_ill_formed_comment_is_refused_whole_and_the_next_line_lexes() {
  let terminators: [Terminator; 4] = [
    (b"\n", Some(LosslessToken::Newline)),
    (b"\r", Some(LosslessToken::CarriageReturn)),
    (b"\r\n", Some(LosslessToken::CarriageReturnAndNewline)),
    (b"", None),
  ];
  for (terminator, token) in terminators {
    let mut doc = b"#a\xffb".to_vec();
    doc.extend_from_slice(terminator);
    if token.is_some() {
      doc.extend_from_slice(b"next");
    }
    let next = 4 + terminator.len();
    let label = doc.escape_ascii();

    let mut lexer = SyntacticLexer::<[u8]>::new(&doc);
    match lexer.lex() {
      Some(Err(errors)) => assert_eq!(unknowns(&errors), REFUSAL, "{label}"),
      other => panic!("{label}: expected a refusal, got {other:?}"),
    }
    assert_eq!(lexer.span(), SimpleSpan::new(0, 4), "{label}");
    if token.is_some() {
      match lexer.lex() {
        Some(Ok(SyntacticToken::Identifier(name))) => assert_eq!(name, &b"next"[..], "{label}"),
        other => panic!("{label}: expected the next line, got {other:?}"),
      }
      assert_eq!(lexer.span(), SimpleSpan::new(next, next + 4), "{label}");
    }
    assert!(lexer.lex().is_none(), "{label}");

    let mut want: Vec<Item<'_>> = vec![(Err(REFUSAL.to_vec()), 0..4)];
    if let Some(token) = token {
      want.push((Ok(token), 4..next));
      want.push((Ok(LosslessToken::Identifier(&b"next"[..])), next..next + 4));
    }
    assert_eq!(lossless(&doc), want, "{label}");
  }
}

/// An ill-formed comment the end of input ends, after a token and alone. The error token reaches
/// the end of the document, and both lexers then stay at its end.
#[test]
fn an_ill_formed_comment_at_the_end_of_input_ends_the_stream() {
  // Each document, and where its comment starts.
  let docs: [(&[u8], usize); 3] = [(b"#\xff", 0), (b"#a\xe4\xb8", 0), (b"x #\xfe\xfe", 2)];
  for (doc, start) in docs {
    let label = doc.escape_ascii();

    let mut lexer = SyntacticLexer::<[u8]>::new(doc);
    if start > 0 {
      assert!(
        matches!(lexer.lex(), Some(Ok(SyntacticToken::Identifier(_)))),
        "{label}"
      );
    }
    assert!(matches!(lexer.lex(), Some(Err(_))), "{label}");
    assert_eq!(lexer.span(), SimpleSpan::new(start, doc.len()), "{label}");
    assert!(lexer.lex().is_none(), "{label}");
    assert!(lexer.lex().is_none(), "{label}");

    let mut lexer = LosslessLexer::<&[u8]>::new(doc);
    if start > 0 {
      assert!(
        matches!(lexer.lex(), Some(Ok(LosslessToken::Identifier(_)))),
        "{label}"
      );
      assert!(
        matches!(lexer.lex(), Some(Ok(LosslessToken::Space))),
        "{label}"
      );
    }
    assert!(matches!(lexer.lex(), Some(Err(_))), "{label}");
    assert_eq!(lexer.span(), SimpleSpan::new(start, doc.len()), "{label}");
    assert!(lexer.lex().is_none(), "{label}");
    assert!(lexer.lex().is_none(), "{label}");
  }
}

/// The byte lexers keep every well-formed comment as before: the lossless one as a `Comment` token
/// that ends before the line terminator, and the syntactic one by skipping it to the next line.
#[test]
fn a_byte_source_keeps_well_formed_comments() {
  for (doc, end, next) in VALID {
    let label = doc.escape_debug();
    let bytes = doc.as_bytes();

    let mut lexer = LosslessLexer::<&[u8]>::new(bytes);
    match lexer.lex() {
      Some(Ok(LosslessToken::Comment(text))) => assert_eq!(text, &bytes[..end], "{label}"),
      other => panic!("{label}: expected a comment, got {other:?}"),
    }
    assert_eq!(lexer.span(), SimpleSpan::new(0, end), "{label}");

    let mut lexer = SyntacticLexer::<[u8]>::new(bytes);
    match (next, lexer.lex()) {
      (None, None) => {}
      (Some(start), Some(Ok(SyntacticToken::Identifier(_)))) => {
        assert_eq!(lexer.span(), SimpleSpan::new(start, start + 1), "{label}");
      }
      (_, other) => panic!("{label}: expected the line after the comment, got {other:?}"),
    }
  }
}

/// The `Bytes` instantiation of the lossless lexer keeps every well-formed comment as before.
#[cfg(feature = "bytes")]
#[test]
fn a_lossless_bytes_lexer_keeps_well_formed_comments() {
  for (doc, end, _) in VALID {
    let mut lexer = LosslessLexer::<bytes::Bytes>::new(doc.as_bytes());
    match lexer.lex() {
      Some(Ok(LosslessToken::Comment(text))) => {
        assert_eq!(&text[..], &doc.as_bytes()[..end], "{}", doc.escape_debug());
      }
      other => panic!("{}: expected a comment, got {other:?}", doc.escape_debug()),
    }
    assert_eq!(
      lexer.span(),
      SimpleSpan::new(0, end),
      "{}",
      doc.escape_debug()
    );
  }
}

/// The `HipByt` instantiation of the lossless lexer keeps every well-formed comment as before.
#[cfg(feature = "hipstr")]
#[test]
fn a_lossless_hipbyt_lexer_keeps_well_formed_comments() {
  for (doc, end, _) in VALID {
    let mut lexer = LosslessLexer::<hipstr::HipByt<'_>>::new(doc.as_bytes());
    match lexer.lex() {
      Some(Ok(LosslessToken::Comment(text))) => {
        assert_eq!(&text[..], &doc.as_bytes()[..end], "{}", doc.escape_debug());
      }
      other => panic!("{}: expected a comment, got {other:?}", doc.escape_debug()),
    }
    assert_eq!(
      lexer.span(),
      SimpleSpan::new(0, end),
      "{}",
      doc.escape_debug()
    );
  }
}

/// A text source's comments are unchanged, through the lossless `&str` instantiation and the
/// syntactic `str` lexer.
#[test]
fn a_str_source_keeps_its_comments() {
  for (doc, end, next) in VALID {
    let label = doc.escape_debug();

    let mut lexer = LosslessLexer::<&str>::new(doc);
    match lexer.lex() {
      Some(Ok(LosslessToken::Comment(text))) => assert_eq!(text, &doc[..end], "{label}"),
      other => panic!("{label}: expected a comment, got {other:?}"),
    }
    assert_eq!(lexer.span(), SimpleSpan::new(0, end), "{label}");

    let mut lexer = SyntacticLexer::<str>::new(doc);
    match (next, lexer.lex()) {
      (None, None) => {}
      (Some(start), Some(Ok(SyntacticToken::Identifier(_)))) => {
        assert_eq!(lexer.span(), SimpleSpan::new(start, start + 1), "{label}");
      }
      (_, other) => panic!("{label}: expected the line after the comment, got {other:?}"),
    }
  }
}

/// A `HipStr` source's comments are unchanged, through the lossless instantiation and the syntactic
/// lexer.
#[cfg(feature = "hipstr")]
#[test]
fn a_hipstr_source_keeps_its_comments() {
  for (doc, end, next) in VALID {
    let label = doc.escape_debug();

    let mut lexer = LosslessLexer::<hipstr::HipStr<'_>>::new(doc);
    match lexer.lex() {
      Some(Ok(LosslessToken::Comment(text))) => assert_eq!(&text[..], &doc[..end], "{label}"),
      other => panic!("{label}: expected a comment, got {other:?}"),
    }
    assert_eq!(lexer.span(), SimpleSpan::new(0, end), "{label}");

    let owned = hipstr::HipStr::from(doc);
    let mut lexer = SyntacticLexer::<hipstr::HipStr<'_>>::new(&owned);
    match (next, lexer.lex()) {
      (None, None) => {}
      (Some(start), Some(Ok(SyntacticToken::Identifier(_)))) => {
        assert_eq!(lexer.span(), SimpleSpan::new(start, start + 1), "{label}");
      }
      (_, other) => panic!("{label}: expected the line after the comment, got {other:?}"),
    }
  }
}

/// The refusal a byte source's lexer must give for [`ILL_FORMED`], as its first item.
fn assert_refused<T: core::fmt::Debug, State: core::fmt::Debug>(
  item: Option<Result<T, LexerErrors<u8, State>>>,
  source: &str,
) {
  match item {
    Some(Err(errors)) => assert_eq!(unknowns(&errors), REFUSAL, "{source}"),
    other => panic!("{source}: expected a refusal, got {other:?}"),
  }
}

/// A `[u8]` source refuses an ill-formed comment through both lexers, as an error rather than a
/// panic.
#[test]
fn a_byte_slice_source_refuses_an_ill_formed_comment() {
  assert_refused(SyntacticLexer::<[u8]>::new(ILL_FORMED).lex(), "[u8]");
  assert_refused(
    LosslessLexer::<&[u8]>::new(ILL_FORMED).lex(),
    "lossless &[u8]",
  );
}

/// `Bytes` refuses through both lexers. A source whose `UTF8` were wrongly `true` would skip the
/// comment unchecked.
#[cfg(feature = "bytes")]
#[test]
fn a_bytes_source_refuses_an_ill_formed_comment() {
  let owned = bytes::Bytes::from_static(ILL_FORMED);
  assert_refused(SyntacticLexer::<bytes::Bytes>::new(&owned).lex(), "Bytes");
  assert_refused(
    LosslessLexer::<bytes::Bytes>::new(ILL_FORMED).lex(),
    "lossless Bytes",
  );
}

/// `BStr` refuses through the syntactic lexer; there is no `BStr` lossless instantiation.
#[cfg(feature = "bstr")]
#[test]
fn a_bstr_source_refuses_an_ill_formed_comment() {
  assert_refused(
    SyntacticLexer::<bstr::BStr>::new(bstr::BStr::new(ILL_FORMED)).lex(),
    "BStr",
  );
}

/// `HipByt` refuses through both lexers.
#[cfg(feature = "hipstr")]
#[test]
fn a_hipbyt_source_refuses_an_ill_formed_comment() {
  let owned = hipstr::HipByt::from(ILL_FORMED);
  assert_refused(
    SyntacticLexer::<hipstr::HipByt<'_>>::new(&owned).lex(),
    "HipByt",
  );
  assert_refused(
    LosslessLexer::<hipstr::HipByt<'_>>::new(ILL_FORMED).lex(),
    "lossless HipByt",
  );
}
