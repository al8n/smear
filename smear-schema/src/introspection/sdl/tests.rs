//! The renderer's comparison data, held to what the build injects.

use std::{format, string::String, vec::Vec};

use super::{super::parse, META_TYPES, SPECIFIED_DIRECTIVES, canonical_default};
use crate::{PackedType, Schema, builtin};

/// The SDL spelling of a packed type reference.
fn spell(schema: &Schema, ty: PackedType) -> String {
  if ty.is_non_null() {
    let inner = ty.strip_outer().expect("a non-null has an inner type");
    return format!("{}!", spell(schema, inner));
  }
  if ty.is_list() {
    let inner = ty.strip_outer().expect("a list has an item type");
    return format!("[{}]", spell(schema, inner));
  }
  String::from(schema.name(ty.base()))
}

/// The five specified directives the door compares a response's with are the five every build
/// injects, to the last argument's description and default.
#[test]
fn the_specified_directives_are_the_injected_ones() {
  let document = parse("type Query { ok: Int }").expect("the SDL parses");
  let schema = Schema::build(&document).expect("a schema");

  let names: Vec<&str> = SPECIFIED_DIRECTIVES
    .iter()
    .map(|specified| specified.name)
    .collect();
  assert_eq!(names, builtin::BUILT_IN_DIRECTIVES);

  for specified in &SPECIFIED_DIRECTIVES {
    let name = specified.name;
    let (def, meta) = schema
      .directives_in_order()
      .find(|(def, _)| schema.name(def.name()) == name)
      .unwrap_or_else(|| panic!("@{name} is injected"));
    assert!(def.is_built_in(), "@{name}");
    assert_eq!(
      schema.text(meta.description()),
      Some(specified.description),
      "@{name}"
    );
    assert_eq!(def.locations(), specified.locations, "@{name}");
    assert_eq!(def.is_repeatable(), specified.repeatable, "@{name}");

    let arguments: Vec<_> = schema
      .inputs_in_order(def.args())
      .map(|(argument, meta)| {
        assert!(!meta.deprecation().is_deprecated(), "@{name}");
        (
          schema.name(argument.name()),
          schema.text(meta.description()),
          spell(&schema, argument.ty()),
          schema.text(meta.default_value()),
        )
      })
      .collect();
    let expected: Vec<_> = specified
      .argument
      .iter()
      .map(|argument| {
        (
          argument.name,
          Some(argument.description),
          String::from(argument.ty),
          argument.default,
        )
      })
      .collect();
    assert_eq!(arguments, expected, "@{name}");
  }
}

/// A server's printing of a default compares as the build would keep it, and text that is not one
/// const value compares as nothing.
#[test]
fn a_default_compares_by_its_canonical_text() {
  assert_eq!(
    canonical_default("\"No longer supported\"").as_deref(),
    Some("\"No longer supported\"")
  );
  assert_eq!(
    canonical_default("\"\"\"No longer supported\"\"\"").as_deref(),
    Some("\"No longer supported\"")
  );
  assert_eq!(
    canonical_default("{a:1,b:[2]}").as_deref(),
    Some("{a: 1, b: [2]}")
  );
  assert_eq!(
    canonical_default("1} type Evil { x: Int } input Z { q: Int "),
    None
  );
}

/// The meta-schema the door holds a response's meta types to is the one every build injects: kind,
/// fields in smear's order with their types and arguments, and enum values.
#[test]
fn the_meta_types_are_the_injected_ones() {
  let document = parse("type Query { ok: Int }").expect("the SDL parses");
  let schema = Schema::build(&document).expect("a schema");

  let names: Vec<&str> = META_TYPES.iter().map(|meta| meta.name).collect();
  assert_eq!(names, builtin::INTROSPECTION_TYPES);

  for meta in &META_TYPES {
    let name = meta.name;
    let (id, def) = schema
      .type_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("{name} is injected"));
    assert_eq!(def.kind(), meta.kind, "{name}");

    let fields: Vec<_> = schema
      .fields_in_order(id)
      .filter(|(field, _)| !schema.name(field.name()).starts_with("__"))
      .map(|(field, _)| {
        let arguments: Vec<_> = schema
          .inputs_in_order(field.args())
          .map(|(argument, _)| (schema.name(argument.name()), spell(&schema, argument.ty())))
          .collect();
        (
          schema.name(field.name()),
          spell(&schema, field.ty()),
          arguments,
        )
      })
      .collect();
    let expected: Vec<_> = meta
      .fields
      .iter()
      .map(|field| {
        let arguments: Vec<_> = field
          .args
          .iter()
          .map(|argument| (argument.name, String::from(argument.ty)))
          .collect();
        (field.name, String::from(field.ty), arguments)
      })
      .collect();
    assert_eq!(fields, expected, "{name}");

    let values: Vec<&str> = schema
      .enum_values_in_order(id)
      .map(|(value, _)| schema.name(value))
      .collect();
    assert_eq!(values, meta.values, "{name}");
  }
}
