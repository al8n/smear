//! SDL type-system definition production tests — descriptions, input-value/field
//! definitions, the `implements`/union-member/directive-location clauses, enum value
//! definitions, root operation types, the eight type-system definitions, the
//! `type_definition` dispatch, and the described dispatch.
//!
//! Mirrors the `executable` harness. The frozen-parity oracles pin the frozen
//! `smear-parser` verdicts, EXCLUDING the empty-`()`/`{}` rows (parser-next rejects
//! them per the spec-cardinality rule, plan Amendment 2) and the `enum { true }` /
//! `{ false }` / `{ null }` rows (parser-next rejects them per Deviations Register
//! entry 1 — the `enum_value` exclusion — where frozen accepted them).

use smear_lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, utils::cmp::Equivalent};

use super::{
  arguments_definition, described_type_definition, directive_definition, directive_locations,
  enum_type_definition, enum_value_definition, enum_values_definition, field_definition,
  fields_definition, implements, input_fields_definition, input_object_type_definition,
  input_value_definition, interface_type_definition, object_type_definition,
  opt_arguments_definition, root_operation_type_definition, root_operation_types_definition,
  scalar_type_definition, schema_definition, type_definition, union_members, union_type_definition,
};
use crate::graphql::{
  ast::{
    ArgumentsDefinition, Described, DirectiveDefinition, DirectiveLocations, EnumTypeDefinition,
    EnumValueDefinition, EnumValuesDefinition, FieldDefinition, FieldsDefinition,
    ImplementInterfaces, InputFieldsDefinition, InputObjectTypeDefinition, InputValueDefinition,
    InterfaceTypeDefinition, Location, ObjectTypeDefinition, RootOperationTypeDefinition,
    RootOperationTypesDefinition, ScalarTypeDefinition, SchemaDefinition, TypeDefinition,
    UnionMemberTypes, UnionTypeDefinition,
  },
  error::GraphqlErrors,
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_bytes(input)
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

/// Asserts the first error's data is an unexpected-token (the house rejection family
/// the cardinality and exclusion deviations report).
fn first_is_unexpected_token<'inp>(
  parser: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<(), GraphqlErrors<&'inp str>>,
  src: &'inp str,
) -> bool {
  match drive_str(parser, src) {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  }
}

// ─── description ─────────────────────────────────────────────────────────────

#[test]
fn description_accepts_inline_and_block() {
  fn some<S: AsRef<[u8]>>(d: Option<crate::graphql::ast::StringValue<S>>) {
    assert!(d.is_some());
  }
  accept_all!(super::description, "\"a docstring\"", some);
  accept_all!(super::description, "\"\"\"block\"\"\"", some);
}

#[test]
fn description_declines_on_non_string() {
  assert!(drive_str(super::description, "Name").unwrap().is_none());
  assert!(drive_str(super::description, "").unwrap().is_none());
}

// ─── input_value_definition ──────────────────────────────────────────────────

#[test]
fn input_value_definition_accepts_minimal() {
  fn check<S: AsRef<[u8]>>(v: InputValueDefinition<S>) {
    assert!(v.description().is_none());
    assert!("x".equivalent(v.name().source_ref()));
    assert!(v.default_value().is_none());
    assert!(v.directives().is_none());
  }
  accept_all!(input_value_definition, "x: Int", check);
}

#[test]
fn input_value_definition_accepts_description_default_directives() {
  fn check<S: AsRef<[u8]>>(v: InputValueDefinition<S>) {
    assert!(v.description().is_some());
    assert!(v.default_value().is_some());
    assert!(v.directives().is_some());
  }
  accept_all!(
    input_value_definition,
    "\"doc\" x: Int = 5 @deprecated",
    check
  );
}

#[test]
fn input_value_definition_rejects_missing_parts() {
  reject_all!(input_value_definition, "x");
  reject_all!(input_value_definition, "x:");
  reject_all!(input_value_definition, ": Int");
}

// ─── arguments_definition / opt_arguments_definition ─────────────────────────

#[test]
fn arguments_definition_accepts_single_and_multiple() {
  fn one<S: AsRef<[u8]>>(a: ArgumentsDefinition<S>) {
    assert_eq!(a.input_value_definitions().len(), 1);
  }
  fn two<S: AsRef<[u8]>>(a: ArgumentsDefinition<S>) {
    assert_eq!(a.input_value_definitions().len(), 2);
  }
  accept_all!(arguments_definition, "(x: Int)", one);
  accept_all!(arguments_definition, "(x: Int, y: String!)", two);
}

#[test]
fn arguments_definition_empty_parens_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `InputValueDefinition+` demands
  // one-or-more, so an empty `()` errors — a documented deviation from the frozen
  // parser, whose unenforced `+` accepted it.
  reject_all!(arguments_definition, "()");
  assert!(first_is_unexpected_token(
    |inp| arguments_definition(inp).map(|_| ()),
    "()"
  ));
}

#[test]
fn arguments_definition_rejects_unterminated() {
  reject_all!(arguments_definition, "(x: Int");
}

#[test]
fn opt_arguments_definition_declines_without_paren() {
  assert!(drive_str(opt_arguments_definition, "").unwrap().is_none());
  assert!(
    drive_str(opt_arguments_definition, ": Int")
      .unwrap()
      .is_none()
  );
  assert!(
    drive_str(opt_arguments_definition, "(x: Int)")
      .unwrap()
      .is_some()
  );
}

// ─── field_definition / fields_definition ────────────────────────────────────

#[test]
fn field_definition_accepts_minimal() {
  fn check<S: AsRef<[u8]>>(f: FieldDefinition<S>) {
    assert!(f.description().is_none());
    assert!("name".equivalent(f.name().source_ref()));
    assert!(f.arguments_definition().is_none());
    assert!(f.directives().is_none());
  }
  accept_all!(field_definition, "name: String", check);
}

#[test]
fn field_definition_accepts_args_directives_description() {
  fn check<S: AsRef<[u8]>>(f: FieldDefinition<S>) {
    assert!(f.description().is_some());
    assert!(f.arguments_definition().is_some());
    assert!(f.directives().is_some());
  }
  accept_all!(
    field_definition,
    "\"doc\" field(x: Int): String @dir",
    check
  );
}

#[test]
fn field_definition_rejects_missing_type() {
  reject_all!(field_definition, "name");
  reject_all!(field_definition, "name:");
}

#[test]
fn fields_definition_accepts_single_and_multiple() {
  fn one<S: AsRef<[u8]>>(f: Option<FieldsDefinition<S>>) {
    assert_eq!(f.expect("present").field_definitions().len(), 1);
  }
  fn two<S: AsRef<[u8]>>(f: Option<FieldsDefinition<S>>) {
    assert_eq!(f.expect("present").field_definitions().len(), 2);
  }
  accept_all!(fields_definition, "{ a: Int }", one);
  accept_all!(fields_definition, "{ a: Int b: String }", two);
}

#[test]
fn fields_definition_declines_without_brace() {
  assert!(drive_str(fields_definition, "").unwrap().is_none());
  assert!(drive_str(fields_definition, "foo").unwrap().is_none());
}

#[test]
fn fields_definition_empty_braces_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `FieldDefinition+` demands one-or-more,
  // so an empty `{}` errors — a documented deviation from the frozen parser.
  reject_all!(fields_definition, "{}");
  assert!(first_is_unexpected_token(
    |inp| fields_definition(inp).map(|_| ()),
    "{}"
  ));
}

// ─── input_fields_definition ─────────────────────────────────────────────────

#[test]
fn input_fields_definition_accepts_and_declines() {
  fn two<S: AsRef<[u8]>>(f: Option<InputFieldsDefinition<S>>) {
    assert_eq!(f.expect("present").input_value_definitions().len(), 2);
  }
  accept_all!(input_fields_definition, "{ x: Int y: String }", two);
  assert!(drive_str(input_fields_definition, "").unwrap().is_none());
}

#[test]
fn input_fields_definition_empty_braces_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `InputValueDefinition+` demands
  // one-or-more, so an empty `{}` errors — a documented deviation from the frozen parser.
  reject_all!(input_fields_definition, "{}");
}

// ─── implements ──────────────────────────────────────────────────────────────

#[test]
fn implements_accepts_single_multiple_and_leading_amp() {
  fn one<S: AsRef<[u8]>>(i: Option<ImplementInterfaces<crate::graphql::ast::Name<S>>>) {
    assert_eq!(i.expect("present").interfaces().len(), 1);
  }
  fn two<S: AsRef<[u8]>>(i: Option<ImplementInterfaces<crate::graphql::ast::Name<S>>>) {
    assert_eq!(i.expect("present").interfaces().len(), 2);
  }
  accept_all!(implements, "implements Node", one);
  accept_all!(implements, "implements Node & Timestamped", two);
  // Optional leading `&` per spec, consumed and not counted.
  accept_all!(implements, "implements & Node & Other", two);
}

#[test]
fn implements_declines_without_keyword() {
  assert!(drive_str(implements, "").unwrap().is_none());
  assert!(drive_str(implements, "@dir").unwrap().is_none());
}

#[test]
fn implements_rejects_trailing_amp() {
  reject_all!(implements, "implements Node &");
}

// ─── union_members ───────────────────────────────────────────────────────────

#[test]
fn union_members_accepts_single_multiple_and_leading_pipe() {
  fn one<S: AsRef<[u8]>>(m: Option<UnionMemberTypes<crate::graphql::ast::Name<S>>>) {
    assert_eq!(m.expect("present").members().len(), 1);
  }
  fn two<S: AsRef<[u8]>>(m: Option<UnionMemberTypes<crate::graphql::ast::Name<S>>>) {
    assert_eq!(m.expect("present").members().len(), 2);
  }
  accept_all!(union_members, "= A", one);
  accept_all!(union_members, "= A | B", two);
  accept_all!(union_members, "= | A | B", two);
}

#[test]
fn union_members_declines_without_equal() {
  assert!(drive_str(union_members, "").unwrap().is_none());
  assert!(drive_str(union_members, "@dir").unwrap().is_none());
}

#[test]
fn union_members_rejects_trailing_pipe() {
  reject_all!(union_members, "= A |");
}

// ─── directive_locations ─────────────────────────────────────────────────────

#[test]
fn directive_locations_accepts_all_nineteen_and_leading_pipe() {
  fn nineteen(l: DirectiveLocations<Location>) {
    assert_eq!(l.locations().len(), 19);
  }
  fn one(l: DirectiveLocations<Location>) {
    assert_eq!(l.locations().len(), 1);
  }
  // Every spec location, executable + type-system; FIELD and FIELD_DEFINITION,
  // ENUM and ENUM_VALUE are distinct exact matches (not prefixes).
  accept_all!(
    directive_locations,
    "QUERY | MUTATION | SUBSCRIPTION | FIELD | FRAGMENT_DEFINITION | FRAGMENT_SPREAD | \
     INLINE_FRAGMENT | VARIABLE_DEFINITION | SCHEMA | SCALAR | OBJECT | FIELD_DEFINITION | \
     ARGUMENT_DEFINITION | INTERFACE | UNION | ENUM | ENUM_VALUE | INPUT_OBJECT | \
     INPUT_FIELD_DEFINITION",
    nineteen
  );
  accept_all!(directive_locations, "| QUERY", one);
}

#[test]
fn directive_locations_rejects_unknown_and_empty() {
  reject_all!(directive_locations, "BOGUS");
  reject_all!(directive_locations, "");
  reject_all!(directive_locations, "FIELD |");
}

// ─── enum_value_definition (headline deviation) ──────────────────────────────

#[test]
fn enum_value_definition_accepts() {
  fn check<S: AsRef<[u8]>>(v: EnumValueDefinition<S>) {
    assert!(v.description().is_none());
    assert!("ACTIVE".equivalent(v.value().source_ref()));
    assert!(v.directives().is_none());
  }
  fn described<S: AsRef<[u8]>>(v: EnumValueDefinition<S>) {
    assert!(v.description().is_some());
    assert!(v.directives().is_some());
  }
  accept_all!(enum_value_definition, "ACTIVE", check);
  accept_all!(
    enum_value_definition,
    "\"doc\" ACTIVE @deprecated",
    described
  );
}

#[test]
fn enum_value_definition_rejects_true_false_null_per_spec() {
  // Deviations Register entry 1 (plan Ruling 3): the `enum_value` atom excludes
  // `true`/`false`/`null`, so each REJECTS as an enum value definition — where the
  // frozen parser accepted them (its `parse_enum_value_definition` used plain
  // `parse_name`). All three words, both source flavors, direct and through the enum.
  for word in ["true", "false", "null"] {
    assert!(
      drive_str(|inp| enum_value_definition(inp).map(|_| ()), word).is_err(),
      "str enum_value_definition should reject {word:?}"
    );
    assert!(
      drive_slice(
        |inp| enum_value_definition(inp).map(|_| ()),
        word.as_bytes()
      )
      .is_err(),
      "slice enum_value_definition should reject {word:?}"
    );
    let via_enum = format!("enum X {{ {word} }}");
    assert!(
      drive_str(|inp| enum_type_definition(inp).map(|_| ()), &via_enum).is_err(),
      "str enum_type_definition should reject {via_enum:?}"
    );
    assert!(
      drive_slice(
        |inp| enum_type_definition(inp).map(|_| ()),
        via_enum.as_bytes()
      )
      .is_err(),
      "slice enum_type_definition should reject {via_enum:?}"
    );
  }
  // The rejection is the exclusion atom's unexpected-token at the excluded word.
  assert!(first_is_unexpected_token(
    |inp| enum_value_definition(inp).map(|_| ()),
    "true"
  ));

  // Positive: every OTHER soft keyword stays a legal enum value (the `enum_value`
  // exclusion is exactly the three literals — soft keywords are not reserved).
  fn three<S: AsRef<[u8]>>(e: EnumTypeDefinition<S>) {
    assert_eq!(
      e.enum_values_definition()
        .expect("values")
        .enum_value_definitions()
        .len(),
      3
    );
  }
  accept_all!(enum_type_definition, "enum X { on type query }", three);
}

// ─── enum_values_definition ──────────────────────────────────────────────────

#[test]
fn enum_values_definition_accepts_and_declines() {
  fn three<S: AsRef<[u8]>>(v: Option<EnumValuesDefinition<S>>) {
    assert_eq!(v.expect("present").enum_value_definitions().len(), 3);
  }
  accept_all!(enum_values_definition, "{ A B C }", three);
  assert!(drive_str(enum_values_definition, "").unwrap().is_none());
}

#[test]
fn enum_values_definition_empty_braces_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `EnumValueDefinition+` demands
  // one-or-more, so an empty `{}` errors — a documented deviation from the frozen parser.
  reject_all!(enum_values_definition, "{}");
}

// ─── root operation types ────────────────────────────────────────────────────

#[test]
fn root_operation_type_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: RootOperationTypeDefinition<S>) {
    assert!(d.operation_type().is_query());
    assert!("Query".equivalent(d.name().source_ref()));
  }
  accept_all!(root_operation_type_definition, "query: Query", check);
}

#[test]
fn root_operation_type_definition_rejects_missing_parts() {
  reject_all!(root_operation_type_definition, "query Query");
  reject_all!(root_operation_type_definition, "nope: Query");
}

#[test]
fn root_operation_types_definition_accepts() {
  fn two<S: AsRef<[u8]>>(d: RootOperationTypesDefinition<S>) {
    assert_eq!(d.root_operation_type_definitions().len(), 2);
  }
  accept_all!(
    root_operation_types_definition,
    "{ query: Q mutation: M }",
    two
  );
}

#[test]
fn root_operation_types_definition_empty_braces_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `RootOperationTypeDefinition+` demands
  // one-or-more, so an empty `{}` errors — a documented deviation from the frozen parser.
  reject_all!(root_operation_types_definition, "{}");
}

// ─── scalar / object / interface / union / enum / input object ───────────────

#[test]
fn scalar_type_definition_accepts_and_rejects() {
  fn check<S: AsRef<[u8]>>(d: ScalarTypeDefinition<S>) {
    assert!("DateTime".equivalent(d.name().source_ref()));
  }
  fn with_dir<S: AsRef<[u8]>>(d: ScalarTypeDefinition<S>) {
    assert!(d.directives().is_some());
  }
  accept_all!(scalar_type_definition, "scalar DateTime", check);
  accept_all!(
    scalar_type_definition,
    "scalar DateTime @specifiedBy",
    with_dir
  );
  reject_all!(scalar_type_definition, "scalar");
  reject_all!(scalar_type_definition, "DateTime");
}

#[test]
fn object_type_definition_accepts_full() {
  fn check<S: AsRef<[u8]>>(d: ObjectTypeDefinition<S>) {
    assert!("User".equivalent(d.name().source_ref()));
    assert_eq!(d.implements().expect("impls").interfaces().len(), 2);
    assert!(d.directives().is_some());
    assert_eq!(
      d.fields_definition()
        .expect("fields")
        .field_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    object_type_definition,
    "type User implements Node & Timestamped @dir { id: ID name: String }",
    check
  );
}

#[test]
fn object_type_definition_accepts_bare_and_rejects_empty_fields() {
  fn bare<S: AsRef<[u8]>>(d: ObjectTypeDefinition<S>) {
    assert!(d.implements().is_none());
    assert!(d.fields_definition().is_none());
  }
  accept_all!(object_type_definition, "type Empty", bare);
  // Empty `{}` is the FieldsDefinition cardinality deviation.
  reject_all!(object_type_definition, "type Bad {}");
  reject_all!(object_type_definition, "type");
}

#[test]
fn interface_type_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: InterfaceTypeDefinition<S>) {
    assert!("Node".equivalent(d.name().source_ref()));
    assert_eq!(
      d.fields_definition()
        .expect("fields")
        .field_definitions()
        .len(),
      1
    );
  }
  accept_all!(
    interface_type_definition,
    "interface Node { id: ID }",
    check
  );
  fn with_impls<S: AsRef<[u8]>>(d: InterfaceTypeDefinition<S>) {
    assert!(d.implements().is_some());
  }
  accept_all!(
    interface_type_definition,
    "interface Named implements Node { name: String }",
    with_impls
  );
}

#[test]
fn union_type_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: UnionTypeDefinition<S>) {
    assert!("Pet".equivalent(d.name().source_ref()));
    assert_eq!(d.member_types().expect("members").members().len(), 2);
  }
  accept_all!(union_type_definition, "union Pet = Dog | Cat", check);
  fn bare<S: AsRef<[u8]>>(d: UnionTypeDefinition<S>) {
    assert!(d.member_types().is_none());
    assert!(d.directives().is_some());
  }
  accept_all!(union_type_definition, "union Empty @dir", bare);
}

#[test]
fn enum_type_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: EnumTypeDefinition<S>) {
    assert!("Direction".equivalent(d.name().source_ref()));
    assert_eq!(
      d.enum_values_definition()
        .expect("values")
        .enum_value_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    enum_type_definition,
    "enum Direction { NORTH SOUTH }",
    check
  );
  fn bare<S: AsRef<[u8]>>(d: EnumTypeDefinition<S>) {
    assert!(d.enum_values_definition().is_none());
  }
  accept_all!(enum_type_definition, "enum Empty", bare);
}

#[test]
fn input_object_type_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: InputObjectTypeDefinition<S>) {
    assert!("Point".equivalent(d.name().source_ref()));
    assert_eq!(
      d.fields_definition()
        .expect("fields")
        .input_value_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    input_object_type_definition,
    "input Point { x: Int y: Int }",
    check
  );
}

// ─── directive_definition ────────────────────────────────────────────────────

#[test]
fn directive_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: DirectiveDefinition<S>) {
    assert!("deprecated".equivalent(d.name().source_ref()));
    assert!(!d.repeatable());
    assert!(d.arguments_definition().is_none());
    assert_eq!(d.locations().locations().len(), 1);
  }
  accept_all!(
    directive_definition,
    "directive @deprecated on FIELD",
    check
  );
}

#[test]
fn directive_definition_accepts_repeatable_and_args() {
  fn check<S: AsRef<[u8]>>(d: DirectiveDefinition<S>) {
    assert!(d.repeatable());
    assert!(d.arguments_definition().is_some());
    assert_eq!(d.locations().locations().len(), 2);
  }
  accept_all!(
    directive_definition,
    "directive @foo(reason: String) repeatable on FIELD | OBJECT",
    check
  );
}

#[test]
fn directive_definition_rejects_missing_parts() {
  reject_all!(directive_definition, "directive deprecated on FIELD");
  reject_all!(directive_definition, "directive @deprecated FIELD");
  reject_all!(directive_definition, "directive @deprecated on");
}

// ─── schema_definition ───────────────────────────────────────────────────────

#[test]
fn schema_definition_accepts() {
  fn check<S: AsRef<[u8]>>(d: SchemaDefinition<S>) {
    assert!(d.directives().is_none());
    assert_eq!(
      d.root_operation_types_definition()
        .root_operation_type_definitions()
        .len(),
      1
    );
  }
  accept_all!(schema_definition, "schema { query: Query }", check);
  fn with_dir<S: AsRef<[u8]>>(d: SchemaDefinition<S>) {
    assert!(d.directives().is_some());
    assert_eq!(
      d.root_operation_types_definition()
        .root_operation_type_definitions()
        .len(),
      2
    );
  }
  accept_all!(
    schema_definition,
    "schema @dir { query: Q mutation: M }",
    with_dir
  );
}

#[test]
fn schema_definition_empty_root_ops_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `RootOperationTypeDefinition+` demands
  // one-or-more, so `schema {}` errors — a documented deviation from the frozen parser.
  reject_all!(schema_definition, "schema {}");
  reject_all!(schema_definition, "schema");
}

// ─── type_definition dispatch ────────────────────────────────────────────────

#[test]
fn type_definition_dispatches_each_arm() {
  fn is_scalar<S: AsRef<[u8]>>(d: TypeDefinition<S>) {
    assert!(d.is_scalar());
  }
  fn is_object<S: AsRef<[u8]>>(d: TypeDefinition<S>) {
    assert!(d.is_object());
  }
  fn is_interface<S: AsRef<[u8]>>(d: TypeDefinition<S>) {
    assert!(d.is_interface());
  }
  fn is_union<S: AsRef<[u8]>>(d: TypeDefinition<S>) {
    assert!(d.is_union());
  }
  fn is_enum<S: AsRef<[u8]>>(d: TypeDefinition<S>) {
    assert!(d.is_enum());
  }
  fn is_input<S: AsRef<[u8]>>(d: TypeDefinition<S>) {
    assert!(d.is_input_object());
  }
  accept_all!(type_definition, "scalar DateTime", is_scalar);
  accept_all!(type_definition, "type Foo { a: Int }", is_object);
  accept_all!(type_definition, "interface Node { id: ID }", is_interface);
  accept_all!(type_definition, "union U = A | B", is_union);
  accept_all!(type_definition, "enum E { A B }", is_enum);
  accept_all!(type_definition, "input I { x: Int }", is_input);
}

#[test]
fn type_definition_rejects_non_type_keywords() {
  // `schema` and `directive` are type-SYSTEM definitions, not type definitions.
  reject_all!(type_definition, "schema { query: Q }");
  reject_all!(type_definition, "directive @x on FIELD");
  reject_all!(type_definition, "bogus Foo");
  reject_all!(type_definition, "");
}

// ─── described_type_definition ───────────────────────────────────────────────

#[test]
fn described_type_definition_carries_description() {
  fn with_desc<S: AsRef<[u8]>>(d: Described<TypeDefinition<S>, S>) {
    assert!(d.description().is_some());
    assert!(d.node().is_object());
  }
  fn without_desc<S: AsRef<[u8]>>(d: Described<TypeDefinition<S>, S>) {
    assert!(d.description().is_none());
    assert!(d.node().is_scalar());
  }
  accept_all!(
    described_type_definition,
    "\"a user\" type User { id: ID }",
    with_desc
  );
  accept_all!(described_type_definition, "scalar DateTime", without_desc);
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts for `type_definition`. Empty `{}` rows are EXCLUDED — those
/// are the spec-cardinality deviations (Amendment 2), pinned by the per-clause
/// `*_error_per_spec` tests.
const TYPE_DEFINITION_ORACLE: &[(&str, bool)] = &[
  ("scalar DateTime", true),
  ("scalar DateTime @dir", true),
  ("type Foo { a: Int }", true),
  ("type Foo", true),
  ("type Foo implements Bar { a: Int }", true),
  ("interface I { a: Int }", true),
  ("union U = A | B", true),
  ("union U", true),
  ("enum E { A B }", true),
  ("enum E", true),
  ("input In { a: Int }", true),
  ("scalar", false),
  ("type", false),
  ("schema { query: Q }", false),
  ("directive @x on FIELD", false),
  ("", false),
];

#[test]
fn type_definition_matches_frozen_verdicts() {
  for (src, accept) in TYPE_DEFINITION_ORACLE {
    assert_eq!(
      drive_str(|inp| type_definition(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str type_definition({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| type_definition(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice type_definition({src:?})"
    );
  }
}

/// Accept/reject verdicts for `directive_definition` (frozen parity — no deviations on
/// this production).
const DIRECTIVE_DEFINITION_ORACLE: &[(&str, bool)] = &[
  ("directive @a on FIELD", true),
  ("directive @a on FIELD | OBJECT", true),
  ("directive @a repeatable on FIELD", true),
  ("directive @a(x: Int) on FIELD", true),
  ("directive @a on | FIELD", true),
  ("directive a on FIELD", false),
  ("directive @a FIELD", false),
  ("directive @a on", false),
  ("directive @a on BOGUS", false),
];

#[test]
fn directive_definition_matches_frozen_verdicts() {
  for (src, accept) in DIRECTIVE_DEFINITION_ORACLE {
    assert_eq!(
      drive_str(|inp| directive_definition(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str directive_definition({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| directive_definition(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice directive_definition({src:?})"
    );
  }
}
