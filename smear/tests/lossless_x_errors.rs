//! The four error conversions `lossless_error_impls!` generates for GraphQLx.
//!
//! # Why these need a direct call rather than a parse
//!
//! `Parse` drops the typed payload at the materialization boundary — deliberately, so a `Parse`
//! can outlive the source it was built from — so every parse-level assertion about an unterminated
//! `<` can say no more than `has_errors()`. Phase B Task 6 measured exactly that on the GraphQL
//! side: swapping `"[]" => unclosed_list` for `"[]" => unclosed_object`, and replacing the
//! catch-all with `unreachable!()`, both left the whole suite green.
//!
//! # And why GraphQLx needs its own file rather than a row in GraphQL's
//!
//! The `unclosed` list is a **macro argument**, so each dialect states its own pairs. GraphQLx
//! states a fourth — `<>` — because its lexer depth-counts `<` and `>` alongside the other three
//! (`smear-lexer/src/graphqlx/syntactic/mod.rs:807-814`). That fourth entry is the single line in
//! this task that a reviewer cannot check by symmetry with GraphQL, and it is the line that
//! decides whether an unterminated generic list reports `Unclosed::Angle` or the catch-all's
//! `Other("unclosed delimiter")`.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear::parser::{
  graphqlx::{
    GraphQLx,
    error::{ErrorData, Expectation, GraphqlxErrors, Unclosed as DialectUnclosed},
    lossless::GraphqlxLosslessErrors,
  },
  lexer::graphqlx::lossless::LosslessLexer,
};
use tokora::{
  SimpleSpan,
  emitter::FromUnclosed,
  error::{
    MaybeTerminal, UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen, UnexpectedEot,
  },
};

type Errs = GraphqlxLosslessErrors<&'static str>;
type Lx<'a> = LosslessLexer<'a, &'a str>;

/// The container holds exactly one error; hand it back.
macro_rules! only_error {
  ($err:expr) => {{
    let errs: Errs = <Errs as FromUnclosed<'_, Lx<'_>, GraphQLx>>::from_unclosed($err);
    let mut it = errs.into_iter();
    let first = it.next().expect("the conversion produced no error at all");
    assert!(it.next().is_none(), "the conversion produced more than one");
    first
  }};
}

/// All **four** declared pairs reach their own report, and an undeclared one reaches the
/// catch-all.
///
/// The `<>` row is the one that is new in this dialect. Getting it wrong is silent: an
/// unterminated `type T<A` would still report, still be an error, and still say something — it
/// would just say `Other("unclosed delimiter")` where the dialect has a typed
/// [`Unclosed::Angle`](DialectUnclosed::Angle) sitting unused.
///
/// The catch-all still has to be reachable and still has to produce an *error* rather than
/// panicking. GraphQLx declares every pair tokora ships a marker for, so this test reaches the
/// catch-all through a pair name no marker produces — the arm is unreachable for this grammar, and
/// "unreachable" is a claim about the grammar rather than something a conversion may assume.
#[test]
fn each_declared_delimiter_pair_reaches_its_own_report() {
  let span = SimpleSpan::new(3, 4);

  let bracket = only_error!(UnclosedBracket::<SimpleSpan, GraphQLx>::bracket_of(span));
  assert!(
    matches!(bracket.data(), ErrorData::Unclosed(DialectUnclosed::List)),
    "`[]` did not reach unclosed_list: {:?}",
    bracket.data()
  );

  let brace = only_error!(UnclosedBrace::<SimpleSpan, GraphQLx>::brace_of(span));
  assert!(
    matches!(brace.data(), ErrorData::Unclosed(DialectUnclosed::Object)),
    "`{{}}` did not reach unclosed_object: {:?}",
    brace.data()
  );

  let paren = only_error!(UnclosedParen::<SimpleSpan, GraphQLx>::paren_of(span));
  assert!(
    matches!(
      paren.data(),
      ErrorData::Unclosed(DialectUnclosed::Parentheses)
    ),
    "`()` did not reach unclosed_parentheses: {:?}",
    paren.data()
  );

  // The fourth pair, and the reason this file exists.
  let angle = only_error!(UnclosedAngle::<SimpleSpan, GraphQLx>::angle_of(span));
  assert!(
    matches!(angle.data(), ErrorData::Unclosed(DialectUnclosed::Angle)),
    "`<>` did not reach unclosed_angle — an unterminated generic list would report the \
     catch-all's untyped note instead: {:?}",
    angle.data()
  );

  // The span survives every arm — the diagnostic points at the opener, not at the end of input.
  assert_eq!(angle.span().start(), 3);
}

/// A pair the dialect does not name reaches the catch-all, and the catch-all answers with an
/// error.
///
/// The positive control for the four assertions above: if every pair fell through to the
/// catch-all, three of those four would still be *wrong* in a way the fourth could not reveal.
/// This is the only shape that distinguishes "the list is complete" from "the list is ignored".
#[test]
fn an_undeclared_pair_reaches_the_catch_all_and_still_produces_an_error() {
  use tokora::{delimiter::DelimiterKind, error::Unclosed as TokoraUnclosed, utils::CowStr};

  /// A delimiter marker GraphQLx's `unclosed` list does not name.
  struct Guillemet;

  let err = TokoraUnclosed::<Guillemet, SimpleSpan, GraphQLx>::of(
    SimpleSpan::new(7, 8),
    DelimiterKind::Custom("«»"),
    CowStr::from_static("«»"),
  );
  let caught = only_error!(err);
  assert!(
    matches!(caught.data(), ErrorData::Other(note) if note == "unclosed delimiter"),
    "an undeclared pair did not reach the catch-all: {:?}",
    caught.data()
  );
  assert_eq!(caught.span().start(), 7);
}

/// The lossless and syntactic layers report end of input the same way.
///
/// **This is a claim about one dialect's two layers, and nothing else checks it.** The acceptance
/// parity gate compares *verdicts* (`has_errors`), so two layers could disagree about what an
/// end-of-input error says forever without any gate noticing.
///
/// It is also the reason `graphqlx::error::Error::unexpected_end_of_input` exists at all.
/// `lossless_error_impls!` calls that constructor by name and GraphQLx had none — GraphQL's error
/// family carries a dedicated expectation-free `ErrorData::EndOfInput` variant and GraphQLx's does
/// not. Rather than give GraphQLx a second spelling that only the lossless half would use, the new
/// constructor produces exactly what this dialect's syntactic `UnexpectedEot` conversion already
/// produced. This test is what pins the two together.
///
/// # Both events, since smear issue #177
///
/// `UnexpectedEot` carries two: a genuine end of input, and one standing in for a **terminal
/// scanner stop**, told apart by [`UnexpectedEnd::is_terminal`](tokora::error::UnexpectedEnd) and
/// by nothing else — the offset is the same on both. Each layer routes on the flag in its own
/// hand-written impl, so "the two layers agree" is now two claims and the second one is the one a
/// dialect can lose silently: a `MaybeTerminal` answer that differs between layers is a document
/// root that resynchronises in one of them and stops in the other, on the same input.
#[test]
fn the_two_layers_agree_on_what_end_of_input_reports() {
  // ── the marked event, first, because it is the one the flag decides ──
  //
  // A macro rather than a loop: the two layers are two *keyings* of one error family — different
  // token kind, different `StateError` — so no array can hold both.
  macro_rules! is_the_stop {
    ($layer:literal, $errors:expr) => {{
      let error = $errors
        .iter()
        .next()
        .unwrap_or_else(|| panic!("the {} conversion produced no error", $layer));
      assert!(
        matches!(error.data(), ErrorData::TerminalEndOfInput),
        "the {} layer discarded the terminal mark: {:?}",
        $layer,
        error.data()
      );
      assert!(
        MaybeTerminal::is_terminal(error),
        "the {} layer built the variant and then answered `false` for it",
        $layer
      );
      assert_eq!(error.span().start(), 11, "{}", $layer);
      assert_eq!(error.span().end(), 11, "{}", $layer);
    }};
  }

  let stop = UnexpectedEot::<usize, GraphQLx>::eot_of(11).into_terminal();
  let lossless_stop: GraphqlxLosslessErrors<&str> = stop.clone().into();
  let syntactic_stop: GraphqlxErrors<&str> = stop.into();
  is_the_stop!("lossless", lossless_stop);
  is_the_stop!("syntactic", syntactic_stop);

  // ── and the genuine one, whose spelling this dialect deliberately shares with a declined
  // `expect`: an `UnexpectedToken` with no found token ──
  let eot = UnexpectedEot::<usize, GraphQLx>::eot_of(11);

  let lossless: GraphqlxLosslessErrors<&str> = eot.clone().into();
  let syntactic: GraphqlxErrors<&str> = eot.into();

  let lossless = lossless
    .into_iter()
    .next()
    .expect("the lossless conversion produced no error");
  let syntactic = syntactic
    .into_iter()
    .next()
    .expect("the syntactic conversion produced no error");

  let ErrorData::UnexpectedToken(lossless_data) = lossless.data() else {
    panic!(
      "the lossless end-of-input error is not an UnexpectedToken: {:?}",
      lossless.data()
    );
  };
  let ErrorData::UnexpectedToken(syntactic_data) = syntactic.data() else {
    panic!(
      "the syntactic end-of-input error is not an UnexpectedToken: {:?}",
      syntactic.data()
    );
  };

  assert!(
    lossless_data.found().is_none(),
    "an end of input has no found token"
  );
  assert!(syntactic_data.found().is_none());
  assert_eq!(
    lossless_data.expected(),
    syntactic_data.expected(),
    "the two GraphQLx layers report a different expectation for the identical event"
  );

  // The offset becomes an empty span at the position, in both.
  assert_eq!(lossless.span().start(), 11);
  assert_eq!(lossless.span().end(), 11);
  assert_eq!(syntactic.span().start(), 11);

  // The control the marked half needs: an unmarked value must stay recoverable in BOTH layers, or
  // the routing above was a blanket rewrite rather than a split.
  assert!(!MaybeTerminal::is_terminal(&lossless));
  assert!(!MaybeTerminal::is_terminal(&syntactic));
}

/// A declined `expect` reports the kind it found and the expectation the dialect spells for it.
///
/// The third of the four generated impls, and the one that routes through `expectation_of`. The
/// seven images GraphQL has no counterpart for are the rows a mapper adapted from GraphQL's table
/// would be missing, so those are the rows asserted here; a missing row falls back to
/// `Expectation::Name`, which is a plausible-looking wrong answer rather than a crash.
#[test]
fn a_declined_expect_reports_the_graphqlx_expectation_for_its_kind() {
  use smear::parser::lexer::graphqlx::lossless::{LosslessToken, LosslessTokenKind as LK};
  use tokora::error::token::UnexpectedToken as TokUnexpectedToken;

  /// The expectation the lossless conversion produces when `expected` was demanded and `found`
  /// turned up.
  fn expectation_for(expected: LK) -> Expectation {
    let span = SimpleSpan::new(0, 1);
    let err = TokUnexpectedToken::<LosslessToken<&str>, LK, SimpleSpan, GraphQLx>::expected_one(
      span, expected,
    )
    .with_found(LosslessToken::Comma);
    let errs: Errs = err.into();
    let first = errs.into_iter().next().expect("no error was produced");
    match first.into_data() {
      ErrorData::UnexpectedToken(data) => *data.expected(),
      other => panic!("a declined expect produced {other:?}"),
    }
  }

  // The seven images this dialect has and GraphQL does not.
  assert_eq!(expectation_for(LK::LAngle), Expectation::LAngle);
  assert_eq!(expectation_for(LK::RAngle), Expectation::RAngle);
  assert_eq!(expectation_for(LK::Asterisk), Expectation::Asterisk);
  assert_eq!(expectation_for(LK::Plus), Expectation::Plus);
  assert_eq!(expectation_for(LK::Minus), Expectation::Minus);
  assert_eq!(
    expectation_for(LK::PathSeparator),
    Expectation::PathSeparator
  );
  assert_eq!(expectation_for(LK::FatArrow), Expectation::FatArrow);

  // The two rows that deliberately differ from GraphQL's lossless table: this dialect's own
  // syntactic `expectation_from_token_kind` sends both numeric images to `InputValue`, and the two
  // GraphQLx layers agreeing matters more than the two dialects agreeing.
  assert_eq!(expectation_for(LK::Int), Expectation::InputValue);
  assert_eq!(expectation_for(LK::Float), Expectation::InputValue);

  // A trivia kind is never demanded by any production, so it falls back — which is the arm that
  // makes the fallback provably a fallback rather than the whole table.
  assert_eq!(expectation_for(LK::Comment), Expectation::Name);
}

/// The lexer's error channel lands in the container with its payload intact.
///
/// The fourth generated impl. Unlike the syntactic twin, which flattens a lexer error to a bare
/// `Other("lexer error")` note, the payload survives here: the container's `StateError` is the
/// lexer's own `LimitExceeded`, so `ErrorData::Lexer` accepts it unchanged. The span is the one
/// thing that cannot be recovered — the lexer error type is a *batch* and the container's error
/// carries a single span — so it is zeroed.
#[test]
fn a_lexer_error_lands_as_error_data_lexer_not_as_a_note() {
  use smear::parser::lexer::graphqlx::error::LexerErrors;
  use tokora::state::tracker::LimitExceeded;

  let errs: Errs = LexerErrors::<char, LimitExceeded>::default().into();
  let first = errs.into_iter().next().expect("no error was produced");
  assert_eq!(first.span().start(), 0);
  assert!(
    matches!(first.data(), ErrorData::Lexer(_)),
    "a lexer error was flattened instead of kept: {:?}",
    first.data()
  );
}

/// A path the scanner stopped inside is not committed as a complete `Path` node.
///
/// # This is the one shipped-tree change smear issue #177 makes, and it is here so it is a
/// decision rather than an accident
///
/// Round 1 measured the lossless `Parse` byte-identical before and after the terminal split, over
/// 2 239 door/dialect/document/ceiling configurations. Round 4 moved the substrate's own blind
/// reads onto their terminal-aware twins and re-ran that diff: **twelve of the 2 240 rows moved,
/// all of them this one shape**, and reverting a single site — `lossless::trivia::eat_if` — makes
/// the output byte-identical again, which is how the cause was attributed rather than read off the
/// code.
///
/// `ty.rs`'s `path_tail` is `while eat_if(PathSeparator) { expect(Identifier) }`. `eat_if`'s
/// `false` used to mean "no `::` follows" for a genuine end of input **and** for a scanner stop, so
/// a type position truncated mid-path closed its `Path` node and the tree said the path was
/// complete. `A` where the source is `A::B` and the scanner never reached `::` is exactly the
/// structural form of the AST defect Codex found in `NamedSpecifier` — `Foo as Bar` succeeding as a
/// bare `Foo`.
///
/// **The node did not merely wrap one name — it swallowed the tail.** Measured at ceiling 10 with
/// the site planted back: `Path@12..61`, holding `Name@12..15 "Int"` and then `Gap@15..61`, the
/// whole 46 refused bytes of the other two definitions. A path node over a gap is a claim about
/// input nothing read.
///
/// What did **not** move, on every one of the twelve rows: the diagnostic count, its span, its
/// severity, `has_errors`, and the tree's extent. The committed tokens are the same tokens, and the
/// `Gap` covers the same bytes; it is no longer inside a node that says they are part of a path.
#[test]
fn a_path_the_scanner_stopped_inside_is_not_committed_as_a_whole_path() {
  use smear::{
    lexer::limits::LosslessLimits, parser::graphqlx::lossless::parse_document_with_limits,
  };

  // Three type positions, so the ceiling can land inside one of them.
  const SRC: &str = "type T { a: Int }\ntype U { b: [Int!]! }\ntype V { c: String }\n";

  // The six ceilings the sweep found, and the two roots that reach them.
  let mut checked = 0usize;
  for ceiling in [10usize, 11, 24, 26, 40, 41] {
    let parse = parse_document_with_limits(SRC, LosslessLimits::default().with_max_tokens(ceiling));

    // The property is positional, not a count: a document refused at ceiling 24 has already
    // committed a whole first definition whose `Int` legitimately IS a `Path`. What must not exist
    // is a `Path` that runs right up to where the scanner stopped — a path closed at the gap is one
    // the parse never saw the end of.
    let gap_start = parse
      .syntax()
      .descendants_with_tokens()
      .find(|c| format!("{:?}", c.kind()) == "Gap")
      .map(|c| c.text_range().start())
      .unwrap_or_else(|| panic!("ceiling {ceiling}: a refused parse tiles its tail with a gap"));
    let truncated: Vec<String> = parse
      .syntax()
      .descendants()
      .filter(|n| format!("{:?}", n.kind()) == "Path" && n.text_range().end() > gap_start)
      .map(|n| format!("{:?}@{:?}", n.kind(), n.text_range()))
      .collect();
    assert!(
      truncated.is_empty(),
      "ceiling {ceiling}: a `Path` node reaches past the point the scanner stopped at \
       ({gap_start:?}) — it is a node claiming a path over bytes nothing read: {truncated:?}"
    );

    // The half that must NOT have moved. A repair that ended the parse earlier, or reported twice,
    // would satisfy the assertion above and fail here.
    assert_eq!(
      parse.diagnostics().len(),
      1,
      "ceiling {ceiling}: the refusal is one diagnostic, as it was before the split"
    );
    assert!(parse.has_errors(), "ceiling {ceiling}");
    assert_eq!(
      parse.syntax().text_range().end(),
      u32::try_from(SRC.len())
        .expect("the fixture is small")
        .into(),
      "ceiling {ceiling}: a lossless tree covers every byte, refused or not"
    );
    checked += 1;
  }
  assert_eq!(checked, 6, "the ceiling set collapsed");

  // THE CONTROL: unrefused, the same document still builds its `Path` nodes.
  let whole = parse_document_with_limits(SRC, LosslessLimits::default());
  assert!(!whole.has_errors(), "the fixture parses");
  assert_eq!(
    whole
      .syntax()
      .descendants()
      .filter(|n| format!("{:?}", n.kind()) == "Path")
      .count(),
    3,
    "an unrefused document must still commit one `Path` per type position — otherwise the \
     assertions above are about a node this dialect stopped building at all"
  );
}
