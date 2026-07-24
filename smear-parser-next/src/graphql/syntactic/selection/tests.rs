//! Selection production tests — `field`, `selection`, `selection_set`, and the
//! `...` fragment fork (`FragmentSpread` / `InlineFragment`).
//!
//! Mirrors the `argument`/`value` harness: every case drives the real GraphQL
//! syntactic lexer under a `Fatal<GraphqlErrors>` context, accept cases run the full
//! source matrix, reject cases assert the error family, and a table-driven oracle
//! pins the frozen `smear-parser` `parse_field`/`parse_selection`/
//! `parse_selection_set` verdicts — excluding the empty-`{}` row, which parser-next
//! rejects per the spec-cardinality rule (plan Amendment 2).

use tokora::{
  FatalContext, Parse, Parser, SimpleSpan, try_parse_input::ParseAttempt, utils::cmp::Equivalent,
};

use super::{
  field, fragment_spread, inline_fragment, selection, selection_set, try_type_condition,
  type_condition,
};
use crate::graphql::{
  GraphQL,
  ast::{Field, FragmentSpread, InlineFragment, Selection, SelectionSet, TypeCondition},
  error::{ErrorData, Expectation, GraphqlErrors, Unclosed},
  syntactic::{GraphqlInput, GraphqlLexer, GraphqlToken},
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, GraphqlLexer<'inp, str>, GraphqlErrors<&'inp str>, GraphQL>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>, GraphQL>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser_of::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(
    f,
  )
  .parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(f)
  .parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(f)
  .parse_bytes(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`,
/// applying the generic `check` fn to each accepted AST — the source matrix.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

/// Asserts `parser` rejects `src` over both `str` and `[u8]`.
macro_rules! reject_all {
  ($parser:expr, $src:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $src).is_err(),
      "str should reject: {:?}",
      $src
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $src.as_bytes()).is_err(),
      "slice should reject: {:?}",
      $src
    );
  }};
}

fn assert_str_expectation(
  result: Result<(), GraphqlErrors<&str>>,
  expected: Expectation,
  span: SimpleSpan,
) {
  let error = result
    .expect_err("fixture should fail")
    .into_iter()
    .next()
    .expect("fatal context emits one error");
  assert_eq!(error.span(), span);
  match error.data() {
    ErrorData::UnexpectedToken(unexpected) => assert_eq!(unexpected.expected(), &expected),
    other => panic!("expected unexpected-token diagnostic, got {other:?}"),
  }
}

fn assert_str_end_of_input(result: Result<(), GraphqlErrors<&str>>, span: SimpleSpan) {
  let error = result
    .expect_err("fixture should fail")
    .into_iter()
    .next()
    .expect("fatal context emits one error");
  assert_eq!(error.span(), span);
  assert!(matches!(error.data(), ErrorData::EndOfInput));
}

fn assert_str_unclosed_object(result: Result<(), GraphqlErrors<&str>>, span: SimpleSpan) {
  let error = result
    .expect_err("fixture should fail")
    .into_iter()
    .next()
    .expect("fatal context emits one error");
  assert_eq!(error.span(), span);
  assert!(matches!(
    error.data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));
}

// ─── field ─────────────────────────────────────────────────────────────────────

#[test]
fn field_accepts_bare_name() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    assert!(f.alias().is_none());
    assert!("name".equivalent(f.name().source()));
    assert!(f.arguments().is_none());
    assert!(f.directives().is_none());
    assert!(f.selection_set().is_none());
    assert_eq!(f.span().start(), 0);
  }
  accept_all!(field, "name", check);
}

#[test]
fn field_accepts_alias() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    let alias = f.alias().expect("alias present");
    assert!("user".equivalent(alias.name().source()));
    assert!("profile".equivalent(f.name().source()));
  }
  accept_all!(field, "user: profile", check);
}

#[test]
fn field_accepts_arguments_and_directives() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    assert!("user".equivalent(f.name().source()));
    assert_eq!(f.arguments().expect("args").arguments().len(), 1);
    assert_eq!(f.directives().expect("dirs").directives().len(), 1);
  }
  accept_all!(field, "user(id: 1) @include(if: true)", check);
}

#[test]
fn field_accepts_nested_selection_set() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    let ss = f.selection_set().expect("selection set");
    assert_eq!(ss.selections().len(), 2);
  }
  accept_all!(field, "user { id name }", check);
}

#[test]
fn field_accepts_alias_with_everything() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    assert!("u".equivalent(f.alias().expect("alias").name().source()));
    assert!("user".equivalent(f.name().source()));
    assert!(f.arguments().is_some());
    assert!(f.directives().is_some());
    assert_eq!(f.selection_set().expect("ss").selections().len(), 1);
  }
  accept_all!(field, "u: user(id: 1) @skip(if: false) { id }", check);
}

#[test]
fn field_rejects_missing_name() {
  reject_all!(field, "");
  reject_all!(field, "{ id }");
}

#[test]
fn field_rejects_alias_without_second_name() {
  reject_all!(field, "user:");
  reject_all!(field, "user: { id }");
}

#[test]
fn field_reports_local_required_name_phases_without_consuming_them() {
  assert_str_expectation(
    drive_str(|inp| field(inp).map(|_| ()), "{ id }"),
    Expectation::Name,
    SimpleSpan::new(0, 1),
  );
  assert_str_expectation(
    drive_str(|inp| field(inp).map(|_| ()), "user: }"),
    Expectation::Name,
    SimpleSpan::new(6, 7),
  );
}

// ─── fragment productions ───────────────────────────────────────────────────

#[test]
fn type_condition_accepts_keyword_and_name() {
  fn user<S: AsRef<[u8]>>(condition: TypeCondition<S>) {
    assert!("User".equivalent(condition.name().source()));
    assert_eq!(condition.span(), &SimpleSpan::new(0, 7));
  }
  fn on_name<S: AsRef<[u8]>>(condition: TypeCondition<S>) {
    assert!("on".equivalent(condition.name().source()));
  }
  accept_all!(type_condition, "on User", user);
  accept_all!(type_condition, "on on", on_name);
}

#[test]
fn type_condition_reports_committed_keyword_and_name_phases() {
  reject_all!(type_condition, "");
  reject_all!(type_condition, "User");
  reject_all!(type_condition, "on");
  reject_all!(type_condition, "on @dir");

  assert_str_expectation(
    drive_str(|inp| type_condition(inp).map(|_| ()), "User"),
    Expectation::Keyword("on"),
    SimpleSpan::new(0, 4),
  );
  assert_str_expectation(
    drive_str(|inp| type_condition(inp).map(|_| ()), ""),
    Expectation::Keyword("on"),
    SimpleSpan::new(0, 0),
  );
  assert_str_expectation(
    drive_str(|inp| type_condition(inp).map(|_| ()), "on @dir"),
    Expectation::Name,
    SimpleSpan::new(3, 4),
  );
  assert_str_expectation(
    drive_str(|inp| type_condition(inp).map(|_| ()), "on"),
    Expectation::Name,
    SimpleSpan::new(2, 2),
  );
}

#[test]
fn try_type_condition_declines_without_consuming_and_commits_on_keyword() {
  let accepted = drive_str(try_type_condition, "on User").expect("`on User` should accept");
  let ParseAttempt::Accept(condition) = accepted else {
    panic!("`on User` should not decline");
  };
  assert!("User".equivalent(condition.name().source()));

  let empty = drive_str(try_type_condition, "").expect("empty input should decline");
  assert!(empty.is_decline());

  let (declined, recovered) = drive_str(
    |inp| {
      let attempt = try_type_condition(inp)?;
      let recovered = field(inp)?;
      Ok::<_, GraphqlErrors<&str>>((
        attempt.is_decline(),
        "User".equivalent(recovered.name().source()),
      ))
    },
    "User",
  )
  .expect("non-`on` identifier should remain available");
  assert_eq!((declined, recovered), (true, true));

  assert_str_expectation(
    drive_str(|inp| try_type_condition(inp).map(|_| ()), "on"),
    Expectation::Name,
    SimpleSpan::new(2, 2),
  );

  let result = drive_str(
    |inp| {
      let result = try_type_condition(inp).map(|_| ());
      let tail = inp
        .next()?
        .expect("wrong type-name token remains available");
      assert_eq!(tail.span, SimpleSpan::new(3, 4));
      assert!(matches!(tail.data, GraphqlToken::<'_, str>::At));
      Ok(result)
    },
    "on @dir",
  )
  .expect("inspection parser should consume the retained type-name token");
  assert_str_expectation(result, Expectation::Name, SimpleSpan::new(3, 4));
}

#[test]
fn fragment_spread_accepts_plain_and_directive_forms() {
  fn plain<S: AsRef<[u8]>>(spread: FragmentSpread<S>) {
    assert!("UserFields".equivalent(spread.name().source()));
    assert!(spread.directives().is_none());
  }
  fn with_directive<S: AsRef<[u8]>>(spread: FragmentSpread<S>) {
    assert!("UserFields".equivalent(spread.name().source()));
    assert_eq!(
      spread.directives().expect("directives").directives().len(),
      1
    );
  }
  accept_all!(fragment_spread, "...UserFields", plain);
  accept_all!(
    fragment_spread,
    "...UserFields @include(if: true)",
    with_directive
  );
}

#[test]
fn fragment_spread_reports_committed_fragment_name_phase() {
  reject_all!(fragment_spread, "...");
  reject_all!(fragment_spread, "... 123");

  assert_str_expectation(
    drive_str(|inp| fragment_spread(inp).map(|_| ()), "..."),
    Expectation::FragmentName,
    SimpleSpan::new(3, 3),
  );
  assert_str_expectation(
    drive_str(|inp| fragment_spread(inp).map(|_| ()), "... 123"),
    Expectation::FragmentName,
    SimpleSpan::new(4, 7),
  );
}

#[test]
fn inline_fragment_accepts_typed_untyped_and_directive_forms() {
  fn typed<S: AsRef<[u8]>>(fragment: InlineFragment<S>) {
    assert!(
      "User".equivalent(
        fragment
          .type_condition()
          .expect("type condition")
          .name()
          .source()
      )
    );
    assert!(fragment.directives().is_none());
  }
  fn untyped<S: AsRef<[u8]>>(fragment: InlineFragment<S>) {
    assert!(fragment.type_condition().is_none());
    assert!(fragment.directives().is_none());
  }
  fn with_directive<S: AsRef<[u8]>>(fragment: InlineFragment<S>) {
    assert!(fragment.type_condition().is_none());
    assert_eq!(
      fragment
        .directives()
        .expect("directives")
        .directives()
        .len(),
      1
    );
  }
  accept_all!(inline_fragment, "... on User { id }", typed);
  accept_all!(inline_fragment, "... { id }", untyped);
  accept_all!(
    inline_fragment,
    "... @skip(if: false) { id }",
    with_directive
  );
}

#[test]
fn inline_fragment_propagates_native_selection_set_tail_errors() {
  reject_all!(inline_fragment, "... on User");
  reject_all!(inline_fragment, "... UserFields");

  assert_str_end_of_input(
    drive_str(|inp| inline_fragment(inp).map(|_| ()), "... on User"),
    SimpleSpan::new(11, 11),
  );
  assert_str_expectation(
    drive_str(|inp| inline_fragment(inp).map(|_| ()), "... UserFields"),
    Expectation::LBrace,
    SimpleSpan::new(4, 14),
  );
}

// ─── selection ───────────────────────────────────────────────────────────────

#[test]
fn selection_dispatches_field() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    assert!(s.is_field());
  }
  accept_all!(selection, "name", check);
  accept_all!(selection, "alias: name(x: 1)", check);
}

#[test]
fn selection_dispatches_fragment_spread() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let fs = s.unwrap_fragment_spread_ref();
    assert!("UserFields".equivalent(fs.name().source()));
  }
  accept_all!(selection, "...UserFields", check);
}

#[test]
fn selection_fragment_spread_with_directives() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let fs = s.unwrap_fragment_spread_ref();
    assert!(fs.directives().is_some());
  }
  accept_all!(selection, "...UserFields @include(if: true)", check);
}

#[test]
fn selection_dispatches_inline_fragment_with_type_condition() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    let tc = ifr.type_condition().expect("type condition");
    assert!("User".equivalent(tc.name().source()));
    assert_eq!(ifr.selection_set().selections().len(), 1);
  }
  accept_all!(selection, "... on User { id }", check);
}

#[test]
fn selection_dispatches_inline_fragment_without_type_condition() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    assert!(ifr.type_condition().is_none());
    assert_eq!(ifr.selection_set().selections().len(), 1);
  }
  accept_all!(selection, "... { id }", check);
}

#[test]
fn selection_inline_fragment_with_directives_no_type_condition() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    assert!(ifr.type_condition().is_none());
    assert!(ifr.directives().is_some());
  }
  accept_all!(selection, "... @skip(if: false) { id }", check);
}

#[test]
fn selection_inline_fragment_with_type_condition_and_directives() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    assert!(ifr.type_condition().is_some());
    assert!(ifr.directives().is_some());
  }
  accept_all!(selection, "... on User @skip(if: false) { id }", check);
}

#[test]
fn selection_rejects_bare_spread() {
  // `...` with no fragment name, type condition, `{`, or `@`.
  reject_all!(selection, "...");
  // Inline fragment with a type condition requires the type name and a selection set.
  reject_all!(selection, "... on");
  reject_all!(selection, "... on User");
  // A spread followed by a non-name (an int) is neither a fragment spread nor an
  // inline fragment.
  reject_all!(selection, "... 123");
}

#[test]
fn selection_reports_local_dispatch_and_native_inline_fragment_tail_errors() {
  assert_str_expectation(
    drive_str(|inp| selection(inp).map(|_| ()), "123"),
    Expectation::Selection,
    SimpleSpan::new(0, 3),
  );
  assert_str_expectation(
    drive_str(|inp| selection(inp).map(|_| ()), "... on"),
    Expectation::Name,
    SimpleSpan::new(6, 6),
  );
  assert_str_end_of_input(
    drive_str(|inp| selection(inp).map(|_| ()), "... @d"),
    SimpleSpan::new(6, 6),
  );
}

#[test]
fn selection_dispatch_does_not_consume_an_invalid_head() {
  let result = drive_str(
    |inp| {
      let result = selection(inp).map(|_| ());
      let tail = inp
        .next()?
        .expect("invalid selection head remains available");
      assert_eq!(tail.span, SimpleSpan::new(0, 3));
      assert!(matches!(tail.data, GraphqlToken::<'_, str>::LitInt(_)));
      Ok(result)
    },
    "123",
  )
  .expect("inspection parser should consume the retained head");

  assert_str_expectation(result, Expectation::Selection, SimpleSpan::new(0, 3));
}

#[test]
fn selection_dispatch_commits_a_spread_before_its_fragment_name_error() {
  assert_str_expectation(
    drive_str(|inp| selection(inp).map(|_| ()), "..."),
    Expectation::FragmentName,
    SimpleSpan::new(3, 3),
  );

  let result = drive_str(
    |inp| {
      let result = selection(inp).map(|_| ());
      let tail = inp.next()?.expect("fragment-name tail remains available");
      assert_eq!(tail.span, SimpleSpan::new(4, 7));
      assert!(matches!(tail.data, GraphqlToken::<'_, str>::LitInt(_)));
      Ok(result)
    },
    "... 123",
  )
  .expect("inspection parser should consume the retained fragment-name tail");

  assert_str_expectation(result, Expectation::FragmentName, SimpleSpan::new(4, 7));
}

#[test]
fn fragment_spread_named_on_is_unrepresentable() {
  // `FragmentName : Name but not on`, spread side. The fixed two-token `...`
  // dispatch rules `on` out before the fragment-spread branch, so a fragment spread named
  // `on` is structurally unrepresentable; the concrete GraphQL `FragmentName`
  // production remains defense in depth. Behavior per input shape, pinned:
  //
  //   `... on X { f }`   -> inline fragment, type condition `X`
  //   `... on on { f }`  -> inline fragment, type condition `on` (the FIRST `on` is
  //                         always the keyword; `NamedType` has no exclusion)
  //   `... on { f }`     -> ERROR (inline arm committed; `{` is no type name) —
  //                         never a fragment spread named `on`
  //   `... on @d { f }`  -> ERROR (inline arm committed; `@` is no type name)
  //   `... on`           -> ERROR (end of input at the type name)
  fn tc_x<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    let tc = ifr.type_condition().expect("type condition");
    assert!("X".equivalent(tc.name().source()));
  }
  fn tc_on<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    let tc = ifr.type_condition().expect("type condition");
    assert!("on".equivalent(tc.name().source()));
  }
  accept_all!(selection, "... on X { f }", tc_x);
  accept_all!(selection, "... on on { f }", tc_on);
  reject_all!(selection, "... on { f }");
  reject_all!(selection, "... on @d { f }");
  reject_all!(selection, "... on");
}

// ─── selection_set ─────────────────────────────────────────────────────────────

#[test]
fn selection_set_accepts_single() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    assert_eq!(ss.selections().len(), 1);
    assert!(ss.selections()[0].is_field());
  }
  accept_all!(selection_set, "{ id }", check);
}

#[test]
fn selection_set_accepts_multiple_mixed() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    assert_eq!(ss.selections().len(), 3);
    assert!(ss.selections()[0].is_field());
    assert!(ss.selections()[1].is_fragment_spread());
    assert!(ss.selections()[2].is_inline_fragment());
  }
  accept_all!(selection_set, "{ id ...Frag ... on T { x } }", check);
}

#[test]
fn selection_set_accepts_commas_as_trivia() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    assert_eq!(ss.selections().len(), 2);
  }
  accept_all!(selection_set, "{ id, name }", check);
}

#[test]
fn selection_set_accepts_nested() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    let outer = &ss.selections()[0];
    let f = outer.unwrap_field_ref();
    assert_eq!(f.selection_set().expect("nested").selections().len(), 2);
  }
  accept_all!(selection_set, "{ user { id name } }", check);
}

#[test]
fn selection_set_uses_native_missing_opener_diagnostics() {
  reject_all!(selection_set, "");
  reject_all!(selection_set, "id");
  reject_all!(selection_set, "id }");

  assert_str_end_of_input(
    drive_str(|inp| selection_set(inp).map(|_| ()), ""),
    SimpleSpan::new(0, 0),
  );
  assert_str_expectation(
    drive_str(|inp| selection_set(inp).map(|_| ()), "id"),
    Expectation::LBrace,
    SimpleSpan::new(0, 2),
  );
  assert_str_expectation(
    drive_str(|inp| selection_set(inp).map(|_| ()), "id }"),
    Expectation::LBrace,
    SimpleSpan::new(0, 2),
  );
}

#[test]
fn selection_set_empty_braces_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `SelectionSet : { Selection+ }` demands
  // one-or-more, so an empty `{}` errors — a documented deviation from the frozen
  // parser, whose `while` loop accepted it.
  reject_all!(selection_set, "{}");
  reject_all!(selection_set, "{ }");
  // The native `at_least(1)` delimiter pipeline maps Tokora's `TooFew` to the
  // dialect's generic repetition diagnostic.
  let error = drive_str(|inp| selection_set(inp).map(|_| ()), "{}")
    .expect_err("empty selection set should fail")
    .into_iter()
    .next()
    .expect("fatal context emits one error");
  assert_eq!(error.span(), SimpleSpan::new(0, 2));
  assert!(matches!(
    error.data(),
    ErrorData::Other(message) if message == "too few elements"
  ));
}

#[test]
fn selection_set_reports_native_cardinality_and_item_diagnostics() {
  assert_str_unclosed_object(
    drive_str(|inp| selection_set(inp).map(|_| ()), "{"),
    SimpleSpan::new(0, 1),
  );
  assert_str_expectation(
    drive_str(|inp| selection_set(inp).map(|_| ()), "{ 123 }"),
    Expectation::Selection,
    SimpleSpan::new(2, 5),
  );
  assert_str_expectation(
    drive_str(|inp| selection_set(inp).map(|_| ()), "{ id 123 }"),
    Expectation::Selection,
    SimpleSpan::new(5, 8),
  );
}

#[test]
fn selection_set_unterminated_is_unclosed_object() {
  reject_all!(selection_set, "{ id");

  assert_str_unclosed_object(
    drive_str(|inp| selection_set(inp).map(|_| ()), "{ id"),
    SimpleSpan::new(0, 1),
  );
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_field` produces.
const FIELD_ORACLE: &[(&str, bool)] = &[
  ("name", true),
  ("user: profile", true),
  ("user(id: 1)", true),
  ("name @deprecated", true),
  ("user { id }", true),
  ("primaryUser: user(id: 1) @include(if: true) { id }", true),
  ("", false),
  ("user:", false),
  ("{ id }", false),
];

#[test]
fn field_matches_frozen_verdicts() {
  for (src, accept) in FIELD_ORACLE {
    assert_eq!(
      drive_str(|inp| field(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str field({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| field(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice field({src:?})"
    );
  }
}

/// Accept/reject verdicts for `selection` (field / spread / inline dispatch).
const SELECTION_ORACLE: &[(&str, bool)] = &[
  ("id", true),
  ("...Frag", true),
  ("...Frag @dir", true),
  ("... on User { id }", true),
  ("... { id }", true),
  ("... @skip(if: true) { id }", true),
  ("...", false),
  ("... on User", false),
];

#[test]
fn selection_matches_frozen_verdicts() {
  for (src, accept) in SELECTION_ORACLE {
    assert_eq!(
      drive_str(|inp| selection(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str selection({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| selection(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice selection({src:?})"
    );
  }
}

/// Accept/reject verdicts for `selection_set`. The oracle EXCLUDES the empty-`{}`
/// row: frozen accepts it, parser-next rejects it per the spec-cardinality rule
/// (plan Amendment 2) — the deviation is pinned by
/// `selection_set_empty_braces_error_per_spec`, not re-blessed here.
const SELECTION_SET_ORACLE: &[(&str, bool)] = &[
  ("{ id }", true),
  ("{ id name }", true),
  ("{ id, name }", true),
  ("{ user { id } }", true),
  ("{ ...Frag }", true),
  ("{ ... on T { x } }", true),
  ("{ id", false),
  ("id }", false),
];

#[test]
fn selection_set_matches_frozen_verdicts() {
  for (src, accept) in SELECTION_SET_ORACLE {
    assert_eq!(
      drive_str(|inp| selection_set(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str selection_set({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| selection_set(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice selection_set({src:?})"
    );
  }
}

#[test]
fn associated_selection_apis_infer_str_and_byte_slice_sources() {
  let _: Field<&str> = drive_str(Field::<&str>::graphql, "id").expect("str field");
  let _: Field<&[u8]> = drive_slice(Field::<&[u8]>::graphql, b"id").expect("slice field");

  let _: TypeCondition<&str> =
    drive_str(TypeCondition::<&str>::graphql, "on User").expect("str type condition");
  let _: TypeCondition<&[u8]> =
    drive_slice(TypeCondition::<&[u8]>::graphql, b"on User").expect("slice type condition");
  let _: ParseAttempt<TypeCondition<&str>> =
    drive_str(TypeCondition::<&str>::try_graphql, "on User").expect("str try type condition");
  let _: ParseAttempt<TypeCondition<&[u8]>> =
    drive_slice(TypeCondition::<&[u8]>::try_graphql, b"on User").expect("slice try type condition");

  let _: FragmentSpread<&str> =
    drive_str(FragmentSpread::<&str>::graphql, "...Part").expect("str fragment spread");
  let _: FragmentSpread<&[u8]> =
    drive_slice(FragmentSpread::<&[u8]>::graphql, b"...Part").expect("slice fragment spread");

  let _: InlineFragment<&str> =
    drive_str(InlineFragment::<&str>::graphql, "... { id }").expect("str inline fragment");
  let _: InlineFragment<&[u8]> =
    drive_slice(InlineFragment::<&[u8]>::graphql, b"... { id }").expect("slice inline fragment");

  let _: Selection<&str> = drive_str(Selection::<&str>::graphql, "...Part").expect("str selection");
  let _: Selection<&[u8]> =
    drive_slice(Selection::<&[u8]>::graphql, b"...Part").expect("slice selection");

  let _: SelectionSet<&str> =
    drive_str(SelectionSet::<&str>::graphql, "{ id }").expect("str selection set");
  let _: SelectionSet<&[u8]> =
    drive_slice(SelectionSet::<&[u8]>::graphql, b"{ id }").expect("slice selection set");
}
