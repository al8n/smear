//! Executable-definition production tests — variable definitions, operations,
//! fragments, executable definitions, and the executable document.
//!
//! Mirrors the `argument`/`selection` harness. The frozen-parity oracles pin the
//! frozen `smear-parser` verdicts, EXCLUDING the empty-`()` and empty-document rows,
//! which parser-next rejects per the spec-cardinality rule (plan Amendment 2), and
//! noting the `operation`-name `on` row, where parser-next is spec-correct and frozen
//! is not (see the module deviation note).

use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use super::{
  executable_definition, executable_document, fragment_definition, operation_definition,
  variable_definition, variables_definition,
};
use crate::graphql::{
  GraphQL,
  ast::{
    DescribedVariableDefinition, ExecutableDefinition, ExecutableDocument, FragmentDefinition,
    OperationDefinition, OperationType, VariableDefinition, VariablesDefinition,
  },
  error::{ErrorData, Expectation, GraphqlErrors},
  syntactic::{GraphqlInput, GraphqlLexer},
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

// ─── variable_definition ─────────────────────────────────────────────────────

#[test]
fn variable_definition_accepts_minimal() {
  fn check<S: AsRef<[u8]>>(v: DescribedVariableDefinition<S>) {
    assert!(v.description().is_none());
    assert!("x".equivalent(v.variable().name().source()));
    assert!(v.default_value().is_none());
    assert!(v.directives().is_none());
  }
  accept_all!(variable_definition, "$x: Int", check);
}

#[test]
fn variable_definition_accepts_list_type() {
  fn check<S: AsRef<[u8]>>(v: DescribedVariableDefinition<S>) {
    assert!(v.ty().is_list());
    assert!(v.ty().required());
  }
  accept_all!(variable_definition, "$x: [Int!]!", check);
}

#[test]
fn variable_definition_accepts_default_and_directives() {
  fn check<S: AsRef<[u8]>>(v: DescribedVariableDefinition<S>) {
    assert!(v.default_value().is_some());
    assert!(v.directives().is_some());
  }
  // Source order per spec: DefaultValue before Directives.
  accept_all!(variable_definition, "$x: Int = 5 @deprecated", check);
}

#[test]
fn variable_definition_accepts_inline_description() {
  fn check<S: AsRef<[u8]>>(v: DescribedVariableDefinition<S>) {
    assert!(v.description().is_some());
    assert!("id".equivalent(v.variable().name().source()));
  }
  accept_all!(variable_definition, "\"the id\" $id: ID!", check);
}

#[test]
fn variable_definition_accepts_block_description() {
  fn check<S: AsRef<[u8]>>(v: DescribedVariableDefinition<S>) {
    assert!(v.description().is_some());
  }
  accept_all!(variable_definition, "\"\"\"desc\"\"\" $x: Int", check);
}

#[test]
fn variable_definition_rejects_missing_dollar_or_type() {
  reject_all!(variable_definition, "x: Int");
  reject_all!(variable_definition, "$x:");
  reject_all!(variable_definition, "$x");
}

#[test]
fn variable_definition_reports_local_tail_phases_without_consuming_them() {
  assert_str_expectation(
    drive_str(|inp| variable_definition(inp).map(|_| ()), "x: Int"),
    Expectation::Dollar,
    SimpleSpan::new(0, 1),
  );
  assert_str_expectation(
    drive_str(|inp| variable_definition(inp).map(|_| ()), "$x }"),
    Expectation::Colon,
    SimpleSpan::new(3, 4),
  );
  assert_str_expectation(
    drive_str(|inp| variable_definition(inp).map(|_| ()), "$x: }"),
    Expectation::Type,
    SimpleSpan::new(4, 5),
  );
  assert_str_expectation(
    drive_str(|inp| variable_definition(inp).map(|_| ()), "$x: Int = }"),
    Expectation::ConstInputValue,
    SimpleSpan::new(10, 11),
  );
  assert_str_expectation(
    drive_str(|inp| variable_definition(inp).map(|_| ()), "$x: Int @"),
    Expectation::Name,
    SimpleSpan::new(9, 9),
  );
}

#[test]
fn variable_definition_directives_are_constant() {
  reject_all!(variable_definition, "$x: Int @skip(if: $condition)");
  assert!(
    drive_str(variable_definition, "$x: Int @skip(if: false)").is_ok(),
    "constant directive arguments should parse"
  );
}

// ─── variables_definition ────────────────────────────────────────────────────

#[test]
fn variables_definition_accepts_single_and_multiple() {
  fn one<S: AsRef<[u8]>>(v: VariablesDefinition<S>) {
    assert_eq!(v.variable_definitions().len(), 1);
  }
  fn two<S: AsRef<[u8]>>(v: VariablesDefinition<S>) {
    assert_eq!(v.variable_definitions().len(), 2);
  }
  accept_all!(variables_definition, "($x: Int)", one);
  accept_all!(variables_definition, "($x: Int, $y: String!)", two);
}

#[test]
fn variables_definition_is_empty_and_zero_width_without_leading_paren() {
  let empty = drive_str(variables_definition, "").unwrap();
  assert!(empty.variable_definitions().is_empty());
  assert_eq!(empty.span(), &SimpleSpan::new(0, 0));

  let absent = drive_str(variables_definition, "{ x }").unwrap();
  assert!(absent.variable_definitions().is_empty());
  assert_eq!(absent.span(), &SimpleSpan::new(0, 0));
}

#[test]
fn variables_definition_empty_parens_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `VariableDefinition+` demands
  // one-or-more, so an empty `()` errors — a documented deviation from the frozen
  // parser, whose unenforced `+` accepted it.
  reject_all!(variables_definition, "()");
  let family = match drive_str(|inp| variables_definition(inp).map(|_| ()), "()") {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  };
  assert!(family);
}

#[test]
fn variables_definition_empty_parens_reports_the_collection_phase() {
  assert_str_expectation(
    drive_str(|inp| variables_definition(inp).map(|_| ()), "()"),
    Expectation::VariableDefinition,
    SimpleSpan::new(1, 2),
  );
}

#[test]
fn variables_definition_rejects_unterminated() {
  reject_all!(variables_definition, "($x: Int");
}

// ─── operation_definition ────────────────────────────────────────────────────

#[test]
fn operation_definition_accepts_shorthand() {
  fn check<S: AsRef<[u8]>>(op: OperationDefinition<S>) {
    assert!(op.is_shorthand());
  }
  accept_all!(operation_definition, "{ id name }", check);
}

#[test]
fn operation_definition_accepts_named_query() {
  fn check<S: AsRef<[u8]>>(op: OperationDefinition<S>) {
    let named = op.unwrap_named_ref();
    assert!(named.operation_type().is_query());
    assert!("GetUser".equivalent(named.name().expect("name").source()));
    assert!(named.variable_definitions().is_some());
    assert!(named.directives().is_some());
    assert_eq!(named.selection_set().selections().len(), 1);
  }
  accept_all!(
    operation_definition,
    "query GetUser($id: ID!) @cached { user }",
    check
  );
}

#[test]
fn operation_definition_accepts_mutation_and_subscription() {
  fn is_mut<S: AsRef<[u8]>>(op: OperationDefinition<S>) {
    assert!(op.unwrap_named_ref().operation_type().is_mutation());
  }
  fn is_sub<S: AsRef<[u8]>>(op: OperationDefinition<S>) {
    assert!(op.unwrap_named_ref().operation_type().is_subscription());
  }
  accept_all!(operation_definition, "mutation { setThing }", is_mut);
  accept_all!(operation_definition, "subscription { onThing }", is_sub);
}

#[test]
fn operation_definition_accepts_anonymous_named_form() {
  fn check<S: AsRef<[u8]>>(op: OperationDefinition<S>) {
    let named = op.unwrap_named_ref();
    assert!(named.name().is_none());
  }
  accept_all!(operation_definition, "query { id }", check);
}

#[test]
fn operation_definition_allows_on_as_name_deviation() {
  // Deviation from frozen (module note): the operation name is any `Name`; `on` is
  // accepted (spec-correct). Frozen excludes it via `!peek_keyword("on")`.
  fn check<S: AsRef<[u8]>>(op: OperationDefinition<S>) {
    let named = op.unwrap_named_ref();
    assert!("on".equivalent(named.name().expect("name").source()));
  }
  accept_all!(operation_definition, "query on { id }", check);
}

#[test]
fn operation_definition_rejects_bad_type_or_missing_set() {
  reject_all!(operation_definition, "queryy { id }");
  reject_all!(operation_definition, "query GetUser");
  reject_all!(operation_definition, "");
}

#[test]
fn operation_definition_reports_operation_type_and_selection_set_phases() {
  assert_str_expectation(
    drive_str(|inp| operation_definition(inp).map(|_| ()), "queryy { id }"),
    Expectation::OperationType,
    SimpleSpan::new(0, 6),
  );
  assert_str_expectation(
    drive_str(|inp| operation_definition(inp).map(|_| ()), "query Q"),
    Expectation::LBrace,
    SimpleSpan::new(7, 7),
  );
}

// ─── fragment_definition ─────────────────────────────────────────────────────

#[test]
fn fragment_definition_accepts() {
  fn check<S: AsRef<[u8]>>(f: FragmentDefinition<S>) {
    assert!("UserFields".equivalent(f.name().source()));
    assert!("User".equivalent(f.type_condition().name().source()));
    assert!(f.directives().is_none());
    assert_eq!(f.selection_set().selections().len(), 2);
  }
  accept_all!(
    fragment_definition,
    "fragment UserFields on User { id name }",
    check
  );
}

#[test]
fn fragment_definition_accepts_directives() {
  fn check<S: AsRef<[u8]>>(f: FragmentDefinition<S>) {
    assert!(f.directives().is_some());
  }
  accept_all!(fragment_definition, "fragment F on T @dir { x }", check);
}

#[test]
fn fragment_definition_rejects_missing_parts() {
  reject_all!(fragment_definition, "fragment on User { id }");
  reject_all!(fragment_definition, "fragment F User { id }");
  reject_all!(fragment_definition, "fragment F on User");
}

#[test]
fn fragment_definition_reports_local_name_and_directive_tails() {
  assert_str_expectation(
    drive_str(|inp| fragment_definition(inp).map(|_| ()), "fragment F on"),
    Expectation::Name,
    SimpleSpan::new(13, 13),
  );
  assert_str_expectation(
    drive_str(
      |inp| fragment_definition(inp).map(|_| ()),
      "fragment F on T @",
    ),
    Expectation::Name,
    SimpleSpan::new(17, 17),
  );
}

#[test]
fn fragment_named_on_error_per_spec() {
  // `FragmentName : Name but not on` — the spec's second named exclusion, enforced
  // through the concrete GraphQL `FragmentName` production. A documented deviation from the frozen parser,
  // which parsed `fragment on on X { … }`: its enforcing `parse_fragment_name`
  // helper had no callers — `parse_fragment_definition` used plain `parse_name`.
  reject_all!(fragment_definition, "fragment on on User { id }");
  // Through the executable-definition dispatch too (the `fragment` keyword commits).
  reject_all!(executable_definition, "fragment on on User { id }");
  // The rejection is the concrete production's unexpected-token at the `on` name.
  let family = match drive_str(
    |inp| fragment_definition(inp).map(|_| ()),
    "fragment on on User { id }",
  ) {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  };
  assert!(family);
  // The exclusion is exactly `on`: `true`/`false`/`null` stay legal fragment names
  // (the `enum_value` exclusions do not cross over), and a type NAMED `on` is legal
  // (`NamedType` carries no exclusion).
  fn named_true<S: AsRef<[u8]>>(f: FragmentDefinition<S>) {
    assert!("true".equivalent(f.name().source()));
  }
  fn on_typed<S: AsRef<[u8]>>(f: FragmentDefinition<S>) {
    assert!("on".equivalent(f.type_condition().name().source()));
  }
  accept_all!(
    fragment_definition,
    "fragment true on User { id }",
    named_true
  );
  accept_all!(fragment_definition, "fragment F on on { id }", on_typed);
}

// ─── executable_definition ───────────────────────────────────────────────────

#[test]
fn executable_definition_dispatches() {
  fn is_op<S: AsRef<[u8]>>(d: ExecutableDefinition<S>) {
    assert!(d.is_operation());
  }
  fn is_frag<S: AsRef<[u8]>>(d: ExecutableDefinition<S>) {
    assert!(d.is_fragment());
  }
  accept_all!(executable_definition, "{ id }", is_op);
  accept_all!(executable_definition, "query Q { id }", is_op);
  accept_all!(executable_definition, "fragment F on T { id }", is_frag);
}

#[test]
fn executable_definition_rejects_an_invalid_head_locally() {
  assert_str_expectation(
    drive_str(|inp| executable_definition(inp).map(|_| ()), "nope"),
    Expectation::ExecutableDefinition,
    SimpleSpan::new(0, 4),
  );
}

// ─── executable_document ─────────────────────────────────────────────────────

#[test]
fn executable_document_accepts_single_and_multiple() {
  fn one<S: AsRef<[u8]>>(d: ExecutableDocument<S>) {
    assert_eq!(d.definitions().len(), 1);
  }
  fn many<S: AsRef<[u8]>>(d: ExecutableDocument<S>) {
    assert_eq!(d.definitions().len(), 3);
  }
  accept_all!(executable_document, "{ id }", one);
  accept_all!(
    executable_document,
    "query A { a } mutation B { b } fragment F on T { c }",
    many
  );
}

#[test]
fn executable_document_empty_input_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `ExecutableDefinition+` demands
  // one-or-more, so an empty input errors — a documented deviation from the frozen
  // parser, whose unenforced `+` accepted it.
  reject_all!(executable_document, "");
  reject_all!(executable_document, "   ");
}

#[test]
fn executable_document_rejects_trailing_garbage() {
  reject_all!(executable_document, "{ id } )");
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts for `operation_definition`. The `query on { id }` row is
/// parser-next's spec-correct verdict (frozen rejects it — an unspec'd `on`
/// exclusion); it is pinned by `operation_definition_allows_on_as_name_deviation`.
const OPERATION_ORACLE: &[(&str, bool)] = &[
  ("{ id }", true),
  ("query { id }", true),
  ("query Q { id }", true),
  ("query Q($x: Int) @dir { id }", true),
  ("mutation { m }", true),
  ("subscription { s }", true),
  ("query Q", false),
  ("nope { id }", false),
  ("", false),
];

#[test]
fn operation_definition_matches_frozen_verdicts() {
  for (src, accept) in OPERATION_ORACLE {
    assert_eq!(
      drive_str(|inp| operation_definition(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str operation_definition({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| operation_definition(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice operation_definition({src:?})"
    );
  }
}

/// Accept/reject verdicts for `executable_document`. The oracle EXCLUDES the
/// empty-input row (frozen accepts, parser-next rejects per Amendment 2 — pinned by
/// `executable_document_empty_input_error_per_spec`).
const DOCUMENT_ORACLE: &[(&str, bool)] = &[
  ("{ id }", true),
  ("query Q { id }", true),
  ("fragment F on T { id }", true),
  ("query A { a } query B { b }", true),
  ("query Q {", false),
  ("fragment F on T", false),
];

#[test]
fn executable_document_matches_frozen_verdicts() {
  for (src, accept) in DOCUMENT_ORACLE {
    assert_eq!(
      drive_str(|inp| executable_document(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str executable_document({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| executable_document(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice executable_document({src:?})"
    );
  }
}

#[test]
fn associated_executable_apis_infer_str_and_byte_slice_sources() {
  let _: VariableDefinition<&str> =
    drive_str(VariableDefinition::<&str>::graphql, "$x: Int").expect("str variable");
  let _: VariableDefinition<&[u8]> =
    drive_slice(VariableDefinition::<&[u8]>::graphql, b"$x: Int").expect("slice variable");

  let _: DescribedVariableDefinition<&str> = drive_str(
    DescribedVariableDefinition::<&str>::graphql,
    "\"desc\" $x: Int",
  )
  .expect("str described variable");
  let _: DescribedVariableDefinition<&[u8]> = drive_slice(
    DescribedVariableDefinition::<&[u8]>::graphql,
    b"\"desc\" $x: Int",
  )
  .expect("slice described variable");

  let _: VariablesDefinition<&str> =
    drive_str(VariablesDefinition::<&str>::graphql, "($x: Int)").expect("str variables");
  let _: VariablesDefinition<&[u8]> =
    drive_slice(VariablesDefinition::<&[u8]>::graphql, b"($x: Int)").expect("slice variables");

  let _: OperationType = drive_str(OperationType::graphql, "query").expect("str operation type");
  let _: OperationType =
    drive_slice(OperationType::graphql, b"mutation").expect("slice operation type");

  let _: OperationDefinition<&str> =
    drive_str(OperationDefinition::<&str>::graphql, "query { id }").expect("str operation");
  let _: OperationDefinition<&[u8]> =
    drive_slice(OperationDefinition::<&[u8]>::graphql, b"query { id }").expect("slice operation");

  let _: FragmentDefinition<&str> = drive_str(
    FragmentDefinition::<&str>::graphql,
    "fragment F on T { id }",
  )
  .expect("str fragment");
  let _: FragmentDefinition<&[u8]> = drive_slice(
    FragmentDefinition::<&[u8]>::graphql,
    b"fragment F on T { id }",
  )
  .expect("slice fragment");

  let _: ExecutableDefinition<&str> =
    drive_str(ExecutableDefinition::<&str>::graphql, "{ id }").expect("str definition");
  let _: ExecutableDefinition<&[u8]> =
    drive_slice(ExecutableDefinition::<&[u8]>::graphql, b"{ id }").expect("slice definition");

  let _: ExecutableDocument<&str> =
    drive_str(ExecutableDocument::<&str>::graphql, "{ id }").expect("str document");
  let _: ExecutableDocument<&[u8]> =
    drive_slice(ExecutableDocument::<&[u8]>::graphql, b"{ id }").expect("slice document");
}
