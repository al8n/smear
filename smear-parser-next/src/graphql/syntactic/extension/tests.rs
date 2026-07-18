//! SDL type-system extension production tests — the seven standalone `extend`
//! shapes and the `type_extension` / `type_system_extension` dispatches.
//!
//! Mirrors the `definition` harness. The frozen-parity oracle pins the frozen
//! `smear-parser` verdicts, EXCLUDING the extension-audit deviation rows: the bare
//! `extend scalar Name` (parser-next rejects it — the spec REQUIRES the directives
//! where frozen fabricated an empty list) and the implements-only object/interface
//! forms (parser-next accepts them — spec-legal, frozen rejected them).

use smear_lexer::graphql::syntactic::SyntacticLexer;
use smear_scaffold::ast as scaffold;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::{
  enum_type_extension, input_object_type_extension, interface_type_extension,
  object_type_extension, scalar_type_extension, schema_extension, type_extension,
  type_system_extension, union_type_extension,
};
use crate::graphql::{
  ast::{
    EnumTypeExtension, InputObjectTypeExtension, InterfaceTypeExtension, ObjectTypeExtension,
    ScalarTypeExtension, SchemaExtension, TypeExtension, TypeSystemExtension, UnionTypeExtension,
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

/// Views a slice (`&str` or `&[u8]`) as bytes, so one assertion body reads across
/// every source representation.
fn bytes<S: AsRef<[u8]>>(slice: &S) -> &[u8] {
  slice.as_ref()
}

/// Asserts the first error's data is an unexpected-token (the house rejection family
/// the cardinality and audit deviations report).
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

/// Asserts the first error's data is end-of-input (the house rejection family a
/// bare extension truncated at EOF reports).
fn first_is_end_of_input<'inp>(
  parser: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<(), GraphqlErrors<&'inp str>>,
  src: &'inp str,
) -> bool {
  match drive_str(parser, src) {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_end_of_input()),
    Ok(()) => false,
  }
}

// ─── scalar_type_extension ───────────────────────────────────────────────────

#[test]
fn scalar_type_extension_accepts_directives() {
  fn check<S: AsRef<[u8]>>(e: ScalarTypeExtension<S>) {
    assert_eq!(bytes(e.name().source_ref()), b"DateTime");
    assert_eq!(e.directives().directives().len(), 2);
    // Span note ruling: the extension span starts at `extend` (the scaffold
    // contract), not at the shape keyword as frozen anchored it.
    assert_eq!(e.span().start(), 0);
  }
  accept_all!(
    scalar_type_extension,
    "extend scalar DateTime @a @b(x: 1)",
    check
  );
}

#[test]
fn scalar_type_extension_requires_directives_per_spec() {
  // Extension audit deviation: `extend scalar Name Directives[Const]` — the spec
  // form has no `?`, so a bare `extend scalar Name` errors where frozen accepted it
  // (fabricating an empty directives list).
  reject_all!(scalar_type_extension, "extend scalar DateTime");
  // Truncated at EOF the family is end-of-input; with a following token it is the
  // unexpected-token the missing-directives position reports.
  assert!(first_is_end_of_input(
    |inp| scalar_type_extension(inp).map(|_| ()),
    "extend scalar DateTime"
  ));
  assert!(first_is_unexpected_token(
    |inp| scalar_type_extension(inp).map(|_| ()),
    "extend scalar DateTime type T { a: Int }"
  ));
  // The same rejection holds through the dispatch.
  reject_all!(type_extension, "extend scalar DateTime");
}

#[test]
fn scalar_type_extension_rejects_missing_parts() {
  reject_all!(scalar_type_extension, "extend scalar");
  reject_all!(scalar_type_extension, "scalar DateTime @a");
  reject_all!(scalar_type_extension, "extend type T @a");
}

// ─── object_type_extension ───────────────────────────────────────────────────

#[test]
fn object_type_extension_accepts_fields_form() {
  fn check<S: AsRef<[u8]>>(e: ObjectTypeExtension<S>) {
    assert_eq!(bytes(e.name().source_ref()), b"User");
    assert!(matches!(
      e.data(),
      scaffold::ObjectTypeExtensionData::Fields { .. }
    ));
  }
  accept_all!(
    object_type_extension,
    "extend type User { age: Int }",
    check
  );
  accept_all!(
    object_type_extension,
    "extend type User implements Node @dir { age: Int }",
    check
  );
}

#[test]
fn object_type_extension_accepts_directives_form() {
  fn check<S: AsRef<[u8]>>(e: ObjectTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::ObjectTypeExtensionData::Directives { .. }
    ));
  }
  accept_all!(object_type_extension, "extend type User @deprecated", check);
  accept_all!(
    object_type_extension,
    "extend type User implements Node @deprecated",
    check
  );
}

#[test]
fn object_type_extension_accepts_implements_only_per_spec() {
  // Extension audit deviation (spec-correct relaxation): the spec's third form
  // `extend type Name ImplementsInterfaces` is legal; frozen rejected it even
  // though the scaffold AST models it (`ObjectTypeExtensionData::Implements`).
  fn check<S: AsRef<[u8]>>(e: ObjectTypeExtension<S>) {
    match e.data() {
      scaffold::ObjectTypeExtensionData::Implements(i) => {
        assert_eq!(i.interfaces().len(), 2);
      }
      _ => panic!("expected implements-only data"),
    }
  }
  accept_all!(
    object_type_extension,
    "extend type User implements Node & Timestamped",
    check
  );
}

#[test]
fn object_type_extension_bare_error_per_spec() {
  // At least one of implements/directives/fields must be present (spec + frozen
  // agree a bare extension is malformed).
  reject_all!(object_type_extension, "extend type User");
  assert!(first_is_end_of_input(
    |inp| object_type_extension(inp).map(|_| ()),
    "extend type User"
  ));
  assert!(first_is_unexpected_token(
    |inp| object_type_extension(inp).map(|_| ()),
    "extend type User scalar S"
  ));
}

// ─── interface_type_extension ────────────────────────────────────────────────

#[test]
fn interface_type_extension_arms_and_bare_error() {
  fn fields<S: AsRef<[u8]>>(e: InterfaceTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::InterfaceTypeExtensionData::Fields { .. }
    ));
  }
  fn dirs<S: AsRef<[u8]>>(e: InterfaceTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::InterfaceTypeExtensionData::Directives { .. }
    ));
  }
  fn impls<S: AsRef<[u8]>>(e: InterfaceTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::InterfaceTypeExtensionData::Implements(_)
    ));
  }
  accept_all!(
    interface_type_extension,
    "extend interface Node { id: ID }",
    fields
  );
  accept_all!(interface_type_extension, "extend interface Node @dir", dirs);
  // Spec-correct relaxation (see the object twin).
  accept_all!(
    interface_type_extension,
    "extend interface Node implements Base",
    impls
  );
  reject_all!(interface_type_extension, "extend interface Node");
}

// ─── union_type_extension ────────────────────────────────────────────────────

#[test]
fn union_type_extension_arms_and_bare_error() {
  fn members<S: AsRef<[u8]>>(e: UnionTypeExtension<S>) {
    match e.data() {
      scaffold::UnionTypeExtensionData::Members { members, .. } => {
        assert_eq!(members.members().len(), 2);
      }
      _ => panic!("expected members data"),
    }
  }
  fn dirs<S: AsRef<[u8]>>(e: UnionTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::UnionTypeExtensionData::Directives(_)
    ));
  }
  accept_all!(union_type_extension, "extend union U = A | B", members);
  // Optional leading `|` per spec, consumed and not counted.
  accept_all!(union_type_extension, "extend union U = | A | B", members);
  accept_all!(union_type_extension, "extend union U @dir = A | B", members);
  accept_all!(union_type_extension, "extend union U @dir", dirs);
  reject_all!(union_type_extension, "extend union U");
  reject_all!(union_type_extension, "extend union U =");
}

// ─── enum_type_extension ─────────────────────────────────────────────────────

#[test]
fn enum_type_extension_arms_and_bare_error() {
  fn values<S: AsRef<[u8]>>(e: EnumTypeExtension<S>) {
    match e.data() {
      scaffold::EnumTypeExtensionData::Values { values, .. } => {
        assert_eq!(values.enum_value_definitions().len(), 2);
      }
      _ => panic!("expected values data"),
    }
  }
  fn dirs<S: AsRef<[u8]>>(e: EnumTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::EnumTypeExtensionData::Directives(_)
    ));
  }
  accept_all!(enum_type_extension, "extend enum E { A B }", values);
  accept_all!(enum_type_extension, "extend enum E @dir { A B }", values);
  accept_all!(enum_type_extension, "extend enum E @dir", dirs);
  reject_all!(enum_type_extension, "extend enum E");
}

#[test]
fn enum_type_extension_inherits_enum_value_exclusion() {
  // Deviations Register entry 1 rides into the extension position: the values
  // block goes through the same `enum_value` atom, so `true`/`false`/`null`
  // reject here exactly as they do in `enum_type_definition`.
  reject_all!(enum_type_extension, "extend enum E { true }");
  reject_all!(enum_type_extension, "extend enum E { false }");
  reject_all!(enum_type_extension, "extend enum E { null }");
  // Every other soft keyword stays legal as an enum value.
  fn ok<S: AsRef<[u8]>>(_e: EnumTypeExtension<S>) {}
  accept_all!(enum_type_extension, "extend enum E { on type query }", ok);
}

#[test]
fn enum_type_extension_empty_braces_error_per_spec() {
  // The Wave-4 `EnumValueDefinition+` non-emptiness deviation rides into the
  // extension position through `enum_values_definition`.
  reject_all!(enum_type_extension, "extend enum E {}");
}

// ─── input_object_type_extension ─────────────────────────────────────────────

#[test]
fn input_object_type_extension_arms_and_bare_error() {
  fn fields<S: AsRef<[u8]>>(e: InputObjectTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::InputObjectTypeExtensionData::Fields { .. }
    ));
  }
  fn dirs<S: AsRef<[u8]>>(e: InputObjectTypeExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::InputObjectTypeExtensionData::Directives(_)
    ));
  }
  accept_all!(
    input_object_type_extension,
    "extend input In { x: Int = 3 }",
    fields
  );
  accept_all!(input_object_type_extension, "extend input In @dir", dirs);
  reject_all!(input_object_type_extension, "extend input In");
  reject_all!(input_object_type_extension, "extend input In {}");
}

// ─── schema_extension ────────────────────────────────────────────────────────

#[test]
fn schema_extension_arms_and_bare_error() {
  fn ops<S: AsRef<[u8]>>(e: SchemaExtension<S>) {
    match e.data() {
      scaffold::SchemaExtensionData::Operations { definitions, .. } => {
        assert_eq!(definitions.root_operation_type_definitions().len(), 1);
      }
      _ => panic!("expected operations data"),
    }
  }
  fn dirs<S: AsRef<[u8]>>(e: SchemaExtension<S>) {
    assert!(matches!(
      e.data(),
      scaffold::SchemaExtensionData::Directives(_)
    ));
  }
  accept_all!(schema_extension, "extend schema { query: Q }", ops);
  accept_all!(schema_extension, "extend schema @dir { query: Q }", ops);
  accept_all!(schema_extension, "extend schema @dir", dirs);
  reject_all!(schema_extension, "extend schema");
  // The Wave-4 `RootOperationTypeDefinition+` non-emptiness deviation rides in.
  reject_all!(schema_extension, "extend schema {}");
}

// ─── type_extension dispatch ─────────────────────────────────────────────────

#[test]
fn type_extension_dispatches_each_arm() {
  fn is_scalar<S: AsRef<[u8]>>(e: TypeExtension<S>) {
    assert!(e.is_scalar());
  }
  fn is_object<S: AsRef<[u8]>>(e: TypeExtension<S>) {
    assert!(e.is_object());
  }
  fn is_interface<S: AsRef<[u8]>>(e: TypeExtension<S>) {
    assert!(e.is_interface());
  }
  fn is_union<S: AsRef<[u8]>>(e: TypeExtension<S>) {
    assert!(e.is_union());
  }
  fn is_enum<S: AsRef<[u8]>>(e: TypeExtension<S>) {
    assert!(e.is_enum());
  }
  fn is_input<S: AsRef<[u8]>>(e: TypeExtension<S>) {
    assert!(e.is_input_object());
  }
  accept_all!(type_extension, "extend scalar S @d", is_scalar);
  accept_all!(type_extension, "extend type T { a: Int }", is_object);
  accept_all!(type_extension, "extend interface I @d", is_interface);
  accept_all!(type_extension, "extend union U = A", is_union);
  accept_all!(type_extension, "extend enum E { A }", is_enum);
  accept_all!(type_extension, "extend input In @d", is_input);
}

#[test]
fn type_extension_rejects_non_type_keywords() {
  // `schema` is a type-SYSTEM extension, not a type extension.
  reject_all!(type_extension, "extend schema @d");
  reject_all!(type_extension, "extend bogus B @d");
  reject_all!(type_extension, "extend");
  reject_all!(type_extension, "");
}

// ─── type_system_extension dispatch ──────────────────────────────────────────

#[test]
fn type_system_extension_dispatches_schema_and_type_arms() {
  fn is_schema<S: AsRef<[u8]>>(e: TypeSystemExtension<S>) {
    assert!(e.is_schema());
  }
  fn is_type<S: AsRef<[u8]>>(e: TypeSystemExtension<S>) {
    assert!(e.is_type());
  }
  accept_all!(type_system_extension, "extend schema @d", is_schema);
  accept_all!(
    type_system_extension,
    "extend schema { mutation: M }",
    is_schema
  );
  accept_all!(type_system_extension, "extend scalar S @d", is_type);
  accept_all!(type_system_extension, "extend type T @d", is_type);
  accept_all!(type_system_extension, "extend union U = A", is_type);
}

#[test]
fn type_system_extension_rejects_malformed() {
  reject_all!(type_system_extension, "extend");
  reject_all!(type_system_extension, "extend bogus B");
  reject_all!(type_system_extension, "schema @d");
  reject_all!(type_system_extension, "");
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts for `type_system_extension`. The extension-audit deviation
/// rows are EXCLUDED: bare `extend scalar Name` (frozen accepted; parser-next rejects
/// per the spec's required directives) and the implements-only object/interface forms
/// (frozen rejected; parser-next accepts per the spec's third form) — both pinned by
/// their dedicated `*_per_spec` tests above.
const TYPE_SYSTEM_EXTENSION_ORACLE: &[(&str, bool)] = &[
  ("extend scalar S @d", true),
  ("extend type T { a: Int }", true),
  ("extend type T @d", true),
  ("extend type T implements I { a: Int }", true),
  ("extend type T implements I @d", true),
  ("extend interface I { a: Int }", true),
  ("extend interface I @d", true),
  ("extend union U = A", true),
  ("extend union U = A | B", true),
  ("extend union U @d", true),
  ("extend enum E { A }", true),
  ("extend enum E @d", true),
  ("extend input In { x: Int }", true),
  ("extend input In @d", true),
  ("extend schema @d", true),
  ("extend schema { query: Q }", true),
  ("extend schema @d { query: Q }", true),
  ("extend type T", false),
  ("extend union U", false),
  ("extend enum E", false),
  ("extend input In", false),
  ("extend schema", false),
  ("extend", false),
  ("extend bogus B", false),
  ("scalar S @d", false),
  ("", false),
];

#[test]
fn type_system_extension_matches_frozen_verdicts() {
  for (src, accept) in TYPE_SYSTEM_EXTENSION_ORACLE {
    assert_eq!(
      drive_str(|inp| type_system_extension(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str type_system_extension({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| type_system_extension(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice type_system_extension({src:?})"
    );
  }
}
