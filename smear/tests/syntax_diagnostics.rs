#![cfg(all(
  feature = "alloc",
  feature = "parser",
  any(feature = "graphql", feature = "graphqlx")
))]
#![allow(missing_docs)]

//! Draft §2 syntax errors answer the diagnostic contract — the close of al8n/smear#126's phase E.
//!
//! Every refusal a parse of a document can end on, lexical or grammatical and in either dialect,
//! reports one `smear::syntax` code through one fieldless `SyntaxErrorKind`, renders a sentence
//! that names no position, and points at the text it refused. This file drives each kind from a
//! real input through the public syntactic doors — `TokenBudget` and `NestingLimit`, which only
//! the lossless door's own machinery raises, are driven in `smear-parser`'s lossless tests,
//! because a `Parse` drops the payload that would carry them out here — and `InvalidSyntax`, the
//! one kind no input reaches, by construction.
//!
//! The message predicate is the one al8n/smear#126's gate wrote down so that "no position" is a
//! check rather than a sentiment: no `[0-9]+..[0-9]+` range, and none of "byte", "offset",
//! "position" or " at ". A hint that counts digits, such as "more than 6 hex digits", passes.

use smear::{
  diagnostic::{Diagnose, Severity},
  lexer::SyntaxErrorKind,
};

/// Whether `text` names a source position.
fn names_a_position(text: &str) -> bool {
  let bytes = text.as_bytes();
  let range = bytes.windows(2).enumerate().any(|(i, pair)| {
    pair == b".."
      && i > 0
      && bytes[i - 1].is_ascii_digit()
      && bytes.get(i + 2).is_some_and(u8::is_ascii_digit)
  });
  range
    || ["byte", "offset", "position", " at "]
      .iter()
      .any(|word| text.contains(word))
}

/// How a row pins the sentence a refusal renders.
#[derive(Clone, Copy, Debug)]
enum Message {
  /// The whole sentence.
  Exact(&'static str),
  /// Its opening, where the expectation after it is the grammar's to name.
  Starts(&'static str),
}

/// Asserts everything the contract reads off one syntax error.
fn check(error: &dyn Diagnose, what: &str, kind: SyntaxErrorKind, message: Message) {
  let rendered = error.to_string();
  assert_eq!(
    error.code(),
    kind.code(),
    "{what} was refused as {rendered:?}"
  );
  match message {
    Message::Exact(text) => assert_eq!(rendered, text, "{what}"),
    Message::Starts(text) => assert!(rendered.starts_with(text), "{what}: {rendered:?}"),
  }
  assert!(!names_a_position(&rendered), "{what}: {rendered:?}");
  assert_eq!(error.severity(), Severity::Error, "{what}");
  assert!(!error.primary().is_entire(), "{what}");
  assert_eq!(error.primary().source(), 0, "{what}");
  assert_eq!(error.primary_label(), Some(kind.message()), "{what}");
  assert_eq!(error.help(), kind.help(), "{what}");
  assert_eq!(error.labels(), 0, "{what}");
  assert!(error.label(0).is_none(), "{what}");
  assert_eq!(error.path_segments(), 0, "{what}");
  assert!(error.path_segment(0).is_none(), "{what}");
}

#[test]
fn the_predicate_is_a_check() {
  assert!(names_a_position("unexpected `}` at 3..5"));
  assert!(names_a_position("refused 12..40"));
  assert!(names_a_position("the byte after it"));
  assert!(!names_a_position(
    "invalid Unicode escape, more than 6 hex digits"
  ));
  assert!(!names_a_position(
    "unterminated spread operator, expected `...`"
  ));
}

/// The one kind only a caller's spelling reaches: the lexer never starts a string anywhere but at
/// a quote, so `LitStr::try_from` is the door that answers it.
#[test]
fn a_spelling_with_no_opening_quote_is_an_unopened_string() {
  let errors = smear::lexer::LitStr::<&str>::try_from("value").expect_err("no opening quote");
  let error = errors.first().expect("a refused spelling reports an error");
  assert_eq!(error.kind(), SyntaxErrorKind::UnopenedString);
  assert_eq!(
    error.kind().code().as_str(),
    "smear::syntax::unopened-string"
  );
  assert_eq!(
    error.kind().message(),
    "a string value must open with `\"` or `\"\"\"`"
  );
  assert!(!names_a_position(error.kind().message()));
}

/// Every kind's sentence, rendered: each holds the predicate, and the kinds no row above renders —
/// the lossless door's two budgets, read here at the value — read as their kind's message.
#[test]
fn every_kind_renders_a_positionless_sentence() {
  for kind in SyntaxErrorKind::ALL {
    assert!(
      !names_a_position(kind.message()),
      "{kind:?}: {:?}",
      kind.message()
    );
  }
}

#[cfg(feature = "graphql")]
mod graphql {
  use smear::{
    diagnostic::Diagnose,
    lexer::{
      SyntaxErrorKind,
      graphql::{
        error::LexerErrors,
        syntactic::{SyntacticToken, SyntacticTokenKind},
      },
      limits::SyntacticLimits,
      tokora::{Parse as _, Parser, SimpleSpan},
    },
    parser::graphql::{
      GraphQL,
      ast::Document,
      error::{ErrorData, Expectation, GraphqlError, GraphqlErrors},
      syntactic::{GraphqlLexer, document},
    },
  };
  use tokora::error::{MaybeTerminal, UnexpectedToken as TokUnexpectedToken};

  use super::{Message, check, names_a_position};

  /// Every refusal the syntactic document door reports for `src`.
  fn refusals(src: &str, limits: SyntacticLimits) -> GraphqlErrors<&str> {
    Parser::with_parser::<GraphqlLexer<'_, str>, Document<&str>, GraphqlErrors<&str>, _, GraphQL>(
      document,
    )
    .parse_str_with_state(src, limits)
    .expect_err("the fixture does not parse")
  }

  /// The first refusal, at the default ceilings.
  fn refuse(src: &str) -> GraphqlError<&str> {
    refusals(src, SyntacticLimits::default())
      .into_iter()
      .next()
      .expect("a refused parse reports an error")
  }

  /// The first refusal over a byte source.
  fn refuse_bytes(src: &[u8]) -> GraphqlError<&[u8]> {
    Parser::with_parser::<
      GraphqlLexer<'_, [u8]>,
      Document<&[u8]>,
      GraphqlErrors<&[u8]>,
      _,
      GraphQL,
    >(document)
    .parse(src)
    .expect_err("the fixture does not parse")
    .into_iter()
    .next()
    .expect("a refused parse reports an error")
  }

  /// Every kind a GraphQL document can be refused with, from a real input through the syntactic
  /// door, with the sentence it renders.
  #[test]
  fn every_kind_is_reached_from_a_real_document() {
    use Message::{Exact, Starts};

    let rows: &[(&str, SyntaxErrorKind, Message)] = &[
      (
        "{ a(x: +) }",
        SyntaxErrorKind::UnexpectedCharacter,
        Exact("unexpected character"),
      ),
      (
        "{ a ? b }",
        SyntaxErrorKind::UnknownCharacter,
        Exact("unknown character"),
      ),
      (
        "{ ..x }",
        SyntaxErrorKind::UnterminatedSpread,
        Exact("unterminated spread operator"),
      ),
      (
        "{ a(x: 007) }",
        SyntaxErrorKind::LeadingZero,
        Exact("leading zero in a number"),
      ),
      (
        "{ a(x: 1e",
        SyntaxErrorKind::IncompleteNumber,
        Exact("incomplete number"),
      ),
      (
        "{ a(x: 123abc) }",
        SyntaxErrorKind::InvalidNumber,
        Exact("invalid number"),
      ),
      (
        "{ a(x: \"oops) }",
        SyntaxErrorKind::UnterminatedString,
        Exact("unterminated string"),
      ),
      (
        "{ a(x: \"\nhello\n\") }",
        SyntaxErrorKind::LineTerminatorInString,
        Exact("line terminator in a string"),
      ),
      (
        r#"{ a(x: "a\qb") }"#,
        SyntaxErrorKind::UnknownEscape,
        Exact("unknown escape sequence"),
      ),
      (
        r#"{ a(x: "abc\"#,
        SyntaxErrorKind::IncompleteEscape,
        Exact("incomplete escape sequence"),
      ),
      (
        r#"{ a(x: "\u12zz") }"#,
        SyntaxErrorKind::InvalidUnicodeEscape,
        Exact("invalid Unicode escape"),
      ),
      (
        r#"{ a(x: "\uD83D") }"#,
        SyntaxErrorKind::InvalidUnicodeScalar,
        Exact("escape is not a Unicode scalar value"),
      ),
      (
        "{ user: }",
        SyntaxErrorKind::UnexpectedToken,
        Exact("unexpected `}`, expected a name"),
      ),
      (
        "query Q",
        SyntaxErrorKind::UnexpectedEndOfInput,
        Starts("unexpected end of input"),
      ),
      (
        "{ id",
        SyntaxErrorKind::UnclosedDelimiter,
        Exact("unclosed `{`"),
      ),
    ];
    for (src, kind, message) in rows {
      check(&refuse(src), &format!("{src:?}"), *kind, *message);
    }

    check(
      &refuse_bytes(b"{ a(x: \"a\xffb\") }"),
      "a byte source's ill-formed string",
      SyntaxErrorKind::InvalidUtf8,
      Exact("invalid UTF-8"),
    );

    let stopped = refusals("{ a }", SyntacticLimits::with_max_nesting_depth(0));
    check(
      &stopped[0],
      "a nesting ceiling of zero",
      SyntaxErrorKind::ResourceLimit,
      Exact("a resource limit stopped the parse"),
    );
  }

  /// The syntactic path keeps a lexical refusal's own kind — one `Other("lexer error")` used to
  /// stand for all of them — at the literal's own span, and still recovers past it.
  #[test]
  fn the_syntactic_path_keeps_the_lexical_kind() {
    for (src, kind) in [
      ("{ a(x: \"oops) }", SyntaxErrorKind::UnterminatedString),
      (r#"{ a(x: "\q") }"#, SyntaxErrorKind::UnknownEscape),
      ("{ a(x: 00) }", SyntaxErrorKind::LeadingZero),
      (
        r#"{ a(x: "\u{D800}") }"#,
        SyntaxErrorKind::InvalidUnicodeScalar,
      ),
    ] {
      let errors = refusals(src, SyntacticLimits::default());
      let error = &errors[0];
      assert!(
        matches!(error.data(), ErrorData::Lexical(found) if *found == kind),
        "{src:?}: {:?}",
        error.data()
      );
      assert_ne!(error.kind(), SyntaxErrorKind::InvalidSyntax, "{src:?}");
      assert_eq!(error.span().start(), 7, "{src:?}: the literal's own span");
      assert!(
        !errors.is_terminal(),
        "{src:?}: a malformed literal is recoverable"
      );
    }

    let error = refuse_bytes(b"{ a(x: \"\xff\") }");
    assert!(
      matches!(
        error.data(),
        ErrorData::Lexical(SyntaxErrorKind::InvalidUtf8)
      ),
      "{:?}",
      error.data()
    );
    assert_eq!(error.span().start(), 7);
  }

  /// A nesting ceiling the syntactic door trips is a resource limit, and it ends the parse.
  #[test]
  fn a_tripped_nesting_ceiling_is_a_terminal_resource_limit() {
    let errors = refusals("{ a { b } }", SyntacticLimits::with_max_nesting_depth(0));
    assert!(errors.is_terminal(), "a tripped ceiling ends the parse");
    assert_eq!(errors[0].kind(), SyntaxErrorKind::ResourceLimit);
    assert_eq!(errors[0].code().as_str(), "smear::syntax::resource-limit");
  }

  /// An expected set tokora could not narrow to one kind names no expectation, so the sentence
  /// has no "expected …" clause; a real keyword is backticked and a production is not.
  #[test]
  fn an_unknown_expectation_drops_its_clause_and_a_keyword_is_not_a_production() {
    type TokoraTokenError = TokUnexpectedToken<
      'static,
      SyntacticToken<&'static str>,
      SyntacticTokenKind,
      SimpleSpan,
      GraphQL,
    >;
    static ONE_OF: [SyntacticTokenKind; 2] =
      [SyntacticTokenKind::LBrace, SyntacticTokenKind::LBracket];

    let span = SimpleSpan::new(4, 9);
    let absent: GraphqlErrors<&str> = TokoraTokenError::of(span)
      .with_found(SyntacticToken::Identifier("found"))
      .into();
    let several: GraphqlErrors<&str> = TokoraTokenError::expected_one_of_with_found(
      span,
      SyntacticToken::Identifier("found"),
      &ONE_OF,
    )
    .into();
    for errors in [absent, several] {
      let error = &errors[0];
      assert!(
        matches!(error.data(), ErrorData::UnexpectedToken(token) if *token.expected() == Expectation::Unspecified),
        "{:?}",
        error.data()
      );
      let rendered = error.to_string();
      assert_eq!(rendered, "unexpected name");
      assert!(!rendered.contains(", expected"), "{rendered:?}");
      assert_eq!(error.kind(), SyntaxErrorKind::UnexpectedToken);
    }
    assert_eq!(Expectation::Unspecified.to_string(), "");

    let keyword = GraphqlError::<&str>::unexpected_token(
      SyntacticTokenKind::Identifier,
      Expectation::Keyword("on"),
      span,
    );
    assert_eq!(keyword.to_string(), "unexpected name, expected `on`");
    let production = GraphqlError::<&str>::unexpected_token(
      SyntacticTokenKind::Identifier,
      Expectation::Production("a type definition"),
      span,
    );
    assert_eq!(
      production.to_string(),
      "unexpected name, expected a type definition"
    );
    let end = GraphqlError::<&str>::maybe_unexpected_token(None, Expectation::Name, span);
    assert_eq!(end.to_string(), "unexpected end of input, expected a name");
    assert_eq!(end.kind(), SyntaxErrorKind::UnexpectedEndOfInput);
  }

  /// The kinds no document reaches through this door, read at the value: the lossless door's two
  /// budgets, which `smear-parser`'s lossless tests drive from a real input, and `InvalidSyntax`,
  /// which only a free-form message is — here, the empty lexer batch's note.
  #[test]
  fn the_kinds_this_door_does_not_reach_render_their_sentences() {
    let span = SimpleSpan::new(3, 3);
    check(
      &GraphqlError::<&str>::nesting_limit_exceeded(span),
      "a parser-frame refusal",
      SyntaxErrorKind::NestingLimit,
      Message::Exact("nesting limit exceeded"),
    );
    check(
      &GraphqlError::<&str>::token_budget_exhausted(span),
      "a durable token-budget refusal",
      SyntaxErrorKind::TokenBudget,
      Message::Exact("token budget exhausted"),
    );

    let empty = GraphqlErrors::<&str>::from(LexerErrors::<char, ()>::default());
    check(
      &empty[0],
      "an empty lexer batch",
      SyntaxErrorKind::InvalidSyntax,
      Message::Exact("lexer error"),
    );
    assert!(!names_a_position(&empty[0].to_string()));
  }

  /// The variants the rows above leave out, read at the value through the producers a parse
  /// reaches them by: the two delimiters besides `{`, both halves of an object field's end of
  /// input, the genuine end of input tokora hands the conversion, and a kept lexer batch with no
  /// error in it, which has no kind, sentence or position of its own to lend.
  #[test]
  fn the_remaining_variants_render_their_sentences() {
    use smear::{diagnostic::Location, parser::graphql::error::ObjectFieldValueHint};
    use tokora::error::{UnexpectedEnd, UnexpectedEot};

    let span = SimpleSpan::new(3, 4);
    check(
      &GraphqlError::<&str>::unclosed_parentheses(span),
      "an unclosed parenthesis",
      SyntaxErrorKind::UnclosedDelimiter,
      Message::Exact("unclosed `(`"),
    );
    check(
      &GraphqlError::<&str>::unclosed_list(span),
      "an unclosed list",
      SyntaxErrorKind::UnclosedDelimiter,
      Message::Exact("unclosed `[`"),
    );
    check(
      &GraphqlError::<&str>::unexpected_end_of_object_field_value(
        ObjectFieldValueHint::Colon,
        span,
      ),
      "an object field the document ends inside",
      SyntaxErrorKind::UnexpectedEndOfInput,
      Message::Exact("unexpected end of input, expected a colon"),
    );

    // The same carrier marked as a scanner stop: the document did not end, a limit did.
    let stopped = GraphqlError::<&str>::new(
      span,
      ErrorData::UnexpectedEndOfObjectFieldValue(
        UnexpectedEnd::new(0, ObjectFieldValueHint::Value).into_terminal(),
      ),
    );
    check(
      &stopped,
      "an object field a scanner stop ends",
      SyntaxErrorKind::ResourceLimit,
      Message::Exact("a resource limit stopped the parse"),
    );

    let ended: GraphqlErrors<&str> = UnexpectedEot::<usize, GraphQL>::eot_of(7).into();
    assert!(
      matches!(ended[0].data(), ErrorData::EndOfInput),
      "{:?}",
      ended[0].data()
    );
    check(
      &ended[0],
      "a genuine end of input",
      SyntaxErrorKind::UnexpectedEndOfInput,
      Message::Exact("unexpected end of input"),
    );

    let unfilled = GraphqlError::<&str>::from_lexer_errors(LexerErrors::default(), span);
    assert_eq!(unfilled.primary(), Location::new(0, span));
    check(
      &unfilled,
      "an empty kept batch",
      SyntaxErrorKind::InvalidSyntax,
      Message::Exact("invalid syntax"),
    );
  }

  /// The lossless door keeps a lexical refusal's whole batch where the syntactic door keeps only
  /// its kind. The error itself carries the zeroed span, and reads its kind, its sentence and its
  /// position off the batch's first error.
  #[cfg(feature = "rowan_0_17")]
  #[test]
  fn a_kept_lexer_batch_answers_through_its_first_error() {
    use smear::{
      diagnostic::Location, lexer::graphql::lossless::LosslessLexer,
      parser::graphql::lossless::GraphqlLosslessErrors,
    };
    use tokora::Lexer as _;

    let src = "{ a ? b }";
    let mut lexer = LosslessLexer::<&str>::new(src);
    let batch = loop {
      match lexer.lex() {
        Some(Err(errors)) => break errors,
        Some(Ok(_)) => {}
        None => panic!("{src:?} lexed without an error"),
      }
    };

    let kept: GraphqlLosslessErrors<&str> = batch.into();
    assert!(
      matches!(kept[0].data(), ErrorData::Lexer(_)),
      "{:?}",
      kept[0].data()
    );
    assert_eq!(kept[0].span(), SimpleSpan::new(0, 0));
    assert_eq!(kept[0].primary(), Location::new(0, SimpleSpan::new(4, 5)));
    check(
      &kept[0],
      "a kept lexer batch",
      SyntaxErrorKind::UnknownCharacter,
      Message::Exact("unknown character"),
    );
  }

  /// No production demands a trivia kind, so the lossless door's expectation table sends one to
  /// `Unspecified`, and the sentence drops its "expected" clause rather than naming it.
  #[cfg(feature = "rowan_0_17")]
  #[test]
  fn a_trivia_expectation_names_none() {
    use smear::{
      lexer::graphql::lossless::{LosslessToken, LosslessTokenKind},
      parser::graphql::lossless::GraphqlLosslessErrors,
    };

    type Declined = TokUnexpectedToken<
      'static,
      LosslessToken<&'static str>,
      LosslessTokenKind,
      SimpleSpan,
      GraphQL,
    >;

    let declined = Declined::expected_one(SimpleSpan::new(4, 5), LosslessTokenKind::Comma)
      .with_found(LosslessToken::Comma);
    let errors: GraphqlLosslessErrors<&str> = declined.into();
    assert!(
      matches!(
        errors[0].data(),
        ErrorData::UnexpectedToken(token) if *token.expected() == Expectation::Unspecified
      ),
      "{:?}",
      errors[0].data()
    );
    check(
      &errors[0],
      "a declined trivia expectation",
      SyntaxErrorKind::UnexpectedToken,
      Message::Exact("unexpected `,`"),
    );
  }

  #[cfg(feature = "materialized-numbers")]
  #[test]
  fn an_out_of_range_literal_names_its_width_not_its_spelling() {
    use smear::parser::graphql::{
      ast::materialized::ConstInputValue, syntactic::value::materialized::const_value,
    };

    let refuse_value = |src: &'static str| {
      Parser::with_parser::<
        GraphqlLexer<'_, str>,
        ConstInputValue<&'static str, i32>,
        GraphqlErrors<&'static str>,
        _,
        GraphQL,
      >(const_value::<_, _, i32>)
      .parse_str(src)
      .expect_err("the literal does not convert")
      .into_iter()
      .next()
      .expect("a refused conversion reports an error")
    };

    check(
      &refuse_value("2147483648"),
      "an `Int` one past `i32::MAX`",
      SyntaxErrorKind::IntOverflow,
      Message::Exact("integer out of range for a 32-bit integer"),
    );
    check(
      &refuse_value("1e400"),
      "a `Float` past the largest double",
      SyntaxErrorKind::FloatOverflow,
      Message::Exact("float out of range for a 64-bit float"),
    );
  }
}

#[cfg(feature = "graphqlx")]
mod graphqlx {
  use smear::{
    lexer::{
      SyntaxErrorKind,
      limits::SyntacticLimits,
      tokora::{Parse as _, Parser},
    },
    parser::graphqlx::{
      GraphQLx,
      ast::Document,
      error::{ErrorData, GraphqlxError, GraphqlxErrors},
      syntactic::{GraphqlxLexer, document},
    },
  };
  use tokora::error::MaybeTerminal;

  use super::{Message, check};

  /// Every refusal the syntactic document door reports for `src`.
  fn refusals(src: &str, limits: SyntacticLimits) -> GraphqlxErrors<&str> {
    Parser::with_parser::<
      GraphqlxLexer<'_, str>,
      Document<&str>,
      GraphqlxErrors<&str>,
      _,
      GraphQLx,
    >(document)
    .parse_str_with_state(src, limits)
    .expect_err("the fixture does not parse")
  }

  /// The first refusal, at the default ceilings.
  fn refuse(src: &str) -> GraphqlxError<&str> {
    refusals(src, SyntacticLimits::default())
      .into_iter()
      .next()
      .expect("a refused parse reports an error")
  }

  /// The first refusal over a byte source.
  fn refuse_bytes(src: &[u8]) -> GraphqlxError<&[u8]> {
    Parser::with_parser::<
      GraphqlxLexer<'_, [u8]>,
      Document<&[u8]>,
      GraphqlxErrors<&[u8]>,
      _,
      GraphQLx,
    >(document)
    .parse(src)
    .expect_err("the fixture does not parse")
    .into_iter()
    .next()
    .expect("a refused parse reports an error")
  }

  /// Every kind a GraphQLx document can be refused with, from a real input through the syntactic
  /// door. `UnexpectedCharacter` and `LeadingZero` are GraphQL's alone: GraphQLx lexes `+` and
  /// `-` as operators and admits a leading zero.
  #[test]
  fn every_kind_is_reached_from_a_real_document() {
    use Message::{Exact, Starts};

    let rows: &[(&str, SyntaxErrorKind, Message)] = &[
      (
        "{ a ? b }",
        SyntaxErrorKind::UnknownCharacter,
        Exact("unknown character"),
      ),
      (
        "{ ..x }",
        SyntaxErrorKind::UnterminatedSpread,
        Exact("unterminated spread operator"),
      ),
      (
        "{ a(x: 1e",
        SyntaxErrorKind::IncompleteNumber,
        Exact("incomplete number"),
      ),
      (
        "{ a(x: 0xZZ) }",
        SyntaxErrorKind::InvalidNumber,
        Exact("invalid number"),
      ),
      (
        "{ a(x: \"oops) }",
        SyntaxErrorKind::UnterminatedString,
        Exact("unterminated string"),
      ),
      (
        "{ a(x: \"\nhello\n\") }",
        SyntaxErrorKind::LineTerminatorInString,
        Exact("line terminator in a string"),
      ),
      (
        r#"{ a(x: "a\qb") }"#,
        SyntaxErrorKind::UnknownEscape,
        Exact("unknown escape sequence"),
      ),
      (
        r#"{ a(x: "abc\"#,
        SyntaxErrorKind::IncompleteEscape,
        Exact("incomplete escape sequence"),
      ),
      (
        r#"{ a(x: "\u12zz") }"#,
        SyntaxErrorKind::InvalidUnicodeEscape,
        Exact("invalid Unicode escape"),
      ),
      (
        r#"{ a(x: "\u{D800}") }"#,
        SyntaxErrorKind::InvalidUnicodeScalar,
        Exact("escape is not a Unicode scalar value"),
      ),
      (
        "{ user: }",
        SyntaxErrorKind::UnexpectedToken,
        Exact("unexpected `}`, expected a name"),
      ),
      (
        "query Q",
        SyntaxErrorKind::UnexpectedEndOfInput,
        Starts("unexpected end of input"),
      ),
      (
        "{ id",
        SyntaxErrorKind::UnclosedDelimiter,
        Exact("unclosed `{`"),
      ),
    ];
    for (src, kind, message) in rows {
      check(&refuse(src), &format!("{src:?}"), *kind, *message);
    }

    check(
      &refuse_bytes(b"{ a(x: \"a\xffb\") }"),
      "a byte source's ill-formed string",
      SyntaxErrorKind::InvalidUtf8,
      Exact("invalid UTF-8"),
    );

    let stopped = refusals("{ a }", SyntacticLimits::with_max_nesting_depth(0));
    assert!(stopped.is_terminal(), "a tripped ceiling ends the parse");
    check(
      &stopped[0],
      "a nesting ceiling of zero",
      SyntaxErrorKind::ResourceLimit,
      Exact("a resource limit stopped the parse"),
    );
  }

  /// The syntactic path keeps a lexical refusal's own kind, as GraphQL's does.
  #[test]
  fn the_syntactic_path_keeps_the_lexical_kind() {
    for (src, kind) in [
      ("{ a(x: \"oops) }", SyntaxErrorKind::UnterminatedString),
      (r#"{ a(x: "\q") }"#, SyntaxErrorKind::UnknownEscape),
      (
        r#"{ a(x: "\u{D800}") }"#,
        SyntaxErrorKind::InvalidUnicodeScalar,
      ),
    ] {
      let errors = refusals(src, SyntacticLimits::default());
      assert!(
        matches!(errors[0].data(), ErrorData::Lexical(found) if *found == kind),
        "{src:?}: {:?}",
        errors[0].data()
      );
      assert_eq!(errors[0].span().start(), 7, "{src:?}");
      assert!(!errors.is_terminal(), "{src:?}");
    }
  }

  /// The variants no document here reaches, read at the value as GraphQL's twin reads them: the
  /// lossless door's two budgets, the empty lexer batch's note, the three delimiters besides `{`,
  /// an empty kept batch, and the variant that exists only to keep `S`.
  #[test]
  fn the_kinds_this_door_does_not_reach_render_their_sentences() {
    use smear::{
      diagnostic::{Diagnose, Location},
      lexer::{graphqlx::error::LexerErrors, tokora::SimpleSpan},
    };

    let span = SimpleSpan::new(3, 3);
    check(
      &GraphqlxError::<&str>::nesting_limit_exceeded(span),
      "a parser-frame refusal",
      SyntaxErrorKind::NestingLimit,
      Message::Exact("nesting limit exceeded"),
    );
    check(
      &GraphqlxError::<&str>::token_budget_exhausted(span),
      "a durable token-budget refusal",
      SyntaxErrorKind::TokenBudget,
      Message::Exact("token budget exhausted"),
    );

    let empty = GraphqlxErrors::<&str>::from(LexerErrors::<char, ()>::default());
    assert!(
      matches!(empty[0].data(), ErrorData::Other(_)),
      "{:?}",
      empty[0].data()
    );
    check(
      &empty[0],
      "an empty lexer batch",
      SyntaxErrorKind::InvalidSyntax,
      Message::Exact("lexer error"),
    );

    for (error, what, message) in [
      (
        GraphqlxError::<&str>::unclosed_parentheses(span),
        "an unclosed parenthesis",
        "unclosed `(`",
      ),
      (
        GraphqlxError::<&str>::unclosed_list(span),
        "an unclosed list",
        "unclosed `[`",
      ),
      (
        GraphqlxError::<&str>::unclosed_angle(span),
        "an unclosed angle bracket",
        "unclosed `<`",
      ),
    ] {
      check(
        &error,
        what,
        SyntaxErrorKind::UnclosedDelimiter,
        Message::Exact(message),
      );
    }

    let unfilled = GraphqlxError::<&str>::new(span, ErrorData::Lexer(LexerErrors::default()));
    assert_eq!(unfilled.primary(), Location::new(0, span));
    check(
      &unfilled,
      "an empty kept batch",
      SyntaxErrorKind::InvalidSyntax,
      Message::Exact("invalid syntax"),
    );
    check(
      &GraphqlxError::<&str>::new(span, ErrorData::Source(core::marker::PhantomData)),
      "the variant that keeps the source type",
      SyntaxErrorKind::InvalidSyntax,
      Message::Exact("invalid syntax"),
    );
  }

  /// GraphQLx's lossless door keeps a lexical refusal's whole batch as GraphQL's does, and the
  /// error reads its kind, its sentence and its position off the batch's first error rather than
  /// off the zeroed span it carries itself.
  #[cfg(feature = "rowan_0_17")]
  #[test]
  fn a_kept_lexer_batch_answers_through_its_first_error() {
    use smear::{
      diagnostic::{Diagnose, Location},
      lexer::{graphqlx::lossless::LosslessLexer, tokora::SimpleSpan},
      parser::graphqlx::lossless::GraphqlxLosslessErrors,
    };
    use tokora::Lexer as _;

    let src = "type T { f(x: Int = 0xZZ): Int }";
    let mut lexer = LosslessLexer::<&str>::new(src);
    let batch = loop {
      match lexer.lex() {
        Some(Err(errors)) => break errors,
        Some(Ok(_)) => {}
        None => panic!("{src:?} lexed without an error"),
      }
    };

    let kept: GraphqlxLosslessErrors<&str> = batch.into();
    assert!(
      matches!(kept[0].data(), ErrorData::Lexer(_)),
      "{:?}",
      kept[0].data()
    );
    assert_eq!(kept[0].span(), SimpleSpan::new(0, 0));
    assert_eq!(kept[0].primary(), Location::new(0, SimpleSpan::new(20, 24)));
    check(
      &kept[0],
      "a kept lexer batch",
      SyntaxErrorKind::InvalidNumber,
      Message::Exact("invalid number, unexpected character after a hexadecimal integer"),
    );
  }
}
