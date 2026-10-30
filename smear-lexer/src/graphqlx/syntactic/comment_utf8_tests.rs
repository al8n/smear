//! smear#251 again, through GraphQLx's own `#` arm, comment rule and comment callbacks.
//!
//! The walk is shared with GraphQL, but the arm that picks the scanner and builds the refusal, the
//! lossless rule, and its two callbacks are this dialect's, so the table runs here too: both lexers
//! refuse what `core::str::from_utf8` refuses, with the same errors, and the lossless one keeps every
//! byte. `graphql/syntactic/comment_utf8_tests.rs` holds the rest of the matrix: the long non-ASCII
//! run and the all-ASCII comments around each vector width, which test the shared walk.

use core::ops::Range;

use std::{vec, vec::Vec};

use tokora::{Lexer as _, SimpleSpan, state::LimitExceeded, utils::Lexeme};

use crate::{
  graphqlx::{
    error::{LexerErrorData, LexerErrors},
    lossless::{LosslessLexer, LosslessToken},
    syntactic::{SyntacticLexer, SyntacticToken},
  },
  limits::LosslessLimits,
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

/// A comment of more than 40 bytes, all ASCII, then the next line.
const LONG_COMMENT: &[u8] = b"# a comment body of well over forty bytes, all of it ASCII\nnext";

/// [`LONG_COMMENT`]'s shape, with a byte in the body that is not UTF-8.
const LONG_ILL_FORMED_COMMENT: &[u8] =
  b"# a comment body of well over forty bytes, then \xff in it\nnext";

/// Where the comment that opens `doc` ends: at its line feed.
fn line_end(doc: &[u8]) -> usize {
  doc
    .iter()
    .position(|&byte| byte == b'\n')
    .expect("the comment ends at a line feed")
}

/// Whether an error item is the token ceiling's refusal and nothing else, compared by variant: a
/// tripped state outranks any lexer error it arrives with.
fn is_token_refusal<Char>(errors: &LexerErrors<Char, LimitExceeded>) -> bool {
  errors.len() == 1
    && errors
      .iter()
      .all(|error| matches!(error.data(), LexerErrorData::State(LimitExceeded::Token(_))))
}

/// Drives a lossless lexer whose token ceiling is spent when it reaches a comment: `before` tokens,
/// then the ceiling's refusal, whose token must be all of `comment`, then nothing, twice.
///
/// The span is read from the lexer rather than from the error, whose own span is `0..0`.
fn assert_ceiling_refusal<'a, L, T, Char>(
  mut lexer: L,
  before: usize,
  comment: Range<usize>,
  label: &str,
) where
  L: tokora::Lexer<'a, Token = T, Span = SimpleSpan>,
  T: tokora::Token<'a, Error = LexerErrors<Char, LimitExceeded>>,
  Char: core::fmt::Debug,
{
  for item in 0..before {
    assert!(
      matches!(lexer.lex(), Some(Ok(_))),
      "{label}: item {item} is not a token"
    );
  }
  match lexer.lex() {
    Some(Err(errors)) => assert!(is_token_refusal(&errors), "{label}: {errors:?}"),
    other => panic!("{label}: expected the ceiling's refusal, got {other:?}"),
  }
  assert_eq!(
    lexer.span(),
    SimpleSpan::new(comment.start, comment.end),
    "{label}: the refusal is not the whole comment"
  );
  assert!(lexer.lex().is_none(), "{label}: the lexer did not latch");
  assert!(lexer.lex().is_none(), "{label}: the lexer did not latch");
}

/// A spent token ceiling refuses a whole comment as one item, then latches, so nothing in the
/// comment's body is ever lexed as source.
///
/// Spent by the third item, after two the ceiling allowed, the comment passes its rule's hook and
/// the logos adapter's post-scan check refuses it. Spent before the first item, the rule's own hook
/// refuses it before the callback runs. The token is the whole comment either way, because the
/// rule's pattern matches it before the hook, and the refusal outranks an ill-formed body's UTF-8
/// errors.
#[test]
fn a_spent_token_ceiling_refuses_a_whole_comment() {
  // `x` and a space are two items, so a ceiling of two refuses the comment after them.
  let after_two = LosslessLimits::default().with_max_tokens(2);
  let mut spent = LosslessLimits::default().with_max_tokens(0);
  spent.increase_token();

  let end = line_end(LONG_COMMENT);
  let text = core::str::from_utf8(LONG_COMMENT).expect("ASCII");
  let prefixed = [&b"x "[..], LONG_COMMENT].concat();
  let prefixed_text = core::str::from_utf8(&prefixed).expect("ASCII");
  assert_ceiling_refusal(
    LosslessLexer::<&str>::with_state(prefixed_text, after_two),
    2,
    2..2 + end,
    "&str after two items",
  );
  assert_ceiling_refusal(
    LosslessLexer::<&str>::with_state(text, spent),
    0,
    0..end,
    "&str spent",
  );

  let byte_cases: [(&[u8], &str, &str); 2] = [
    (LONG_COMMENT, "&[u8] after two items", "&[u8] spent"),
    (
      LONG_ILL_FORMED_COMMENT,
      "&[u8] ill-formed after two items",
      "&[u8] ill-formed spent",
    ),
  ];
  for (comment, after_label, spent_label) in byte_cases {
    let end = line_end(comment);
    let prefixed = [&b"x "[..], comment].concat();
    assert_ceiling_refusal(
      LosslessLexer::<&[u8]>::with_state(prefixed.as_slice(), after_two),
      2,
      2..2 + end,
      after_label,
    );
    assert_ceiling_refusal(
      LosslessLexer::<&[u8]>::with_state(comment, spent),
      0,
      0..end,
      spent_label,
    );
  }

  #[cfg(feature = "bytes")]
  {
    assert_ceiling_refusal(
      LosslessLexer::<bytes::Bytes>::with_state(LONG_ILL_FORMED_COMMENT, spent),
      0,
      0..line_end(LONG_ILL_FORMED_COMMENT),
      "Bytes ill-formed spent",
    );
  }

  #[cfg(feature = "hipstr")]
  {
    assert_ceiling_refusal(
      LosslessLexer::<hipstr::HipByt<'_>>::with_state(LONG_ILL_FORMED_COMMENT, spent),
      0,
      0..line_end(LONG_ILL_FORMED_COMMENT),
      "HipByt ill-formed spent",
    );
    assert_ceiling_refusal(
      LosslessLexer::<hipstr::HipStr<'_>>::with_state(text, spent),
      0,
      0..end,
      "HipStr spent",
    );
  }
}
