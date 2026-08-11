//! The draft §4 introspection shape, as plain records.
//!
//! One `struct` per meta-schema type, named for it, carrying exactly the fields the door reads.
//! Unknown fields are ignored — deliberately, and it is the difference between a door that works
//! against real servers and one that does not: an introspection response carries whatever the
//! *query* asked for, servers add fields ahead of the specification, and federation gateways add
//! their own.
//!
//! # Required means non-null in the meta-schema
//!
//! A field without `#[serde(default)]` here is one draft §4 declares non-null, so its absence is a
//! malformed response and `serde` says so before the door runs at all. The three exceptions each
//! name a server that omits the field:
//!
//! - `__Field.args` and `__Directive.args` — non-null in the meta-schema, universally emitted, and
//!   defaulted anyway so a hand-written fixture can leave out the empty list.
//! - `__Directive.isRepeatable` — added to the specification in 2020; a server older than that
//!   emits no such field and no such directive.
//! - `__Type.isOneOf` — nullable in the meta-schema and absent from every server predating
//!   OneOf Input Objects.
//!
//! # What is deliberately not here
//!
//! `description`, `isDeprecated`, `deprecationReason` and `specifiedByURL` are read by nothing,
//! because [`Schema`](crate::Schema) retains none of them — it is the substrate
//! validation rules stand on, and no draft §5 rule asks whether a field is deprecated. Declaring
//! them would be declaring that the door drops them, which is
//! [the module header's](super) job, not the model's.

use std::{boxed::Box, string::String, vec::Vec};

use serde::Deserialize;

/// `__Schema` — the root of an introspection result.
#[derive(Debug, Deserialize)]
pub(super) struct IntrospectedSchema {
  /// Every type the schema defines, the meta-schema and the built-in scalars included.
  pub(super) types: Vec<IntrospectedType>,
  /// The query root. Non-null in the meta-schema, and draft §3.3 requires one regardless.
  #[serde(rename = "queryType")]
  pub(super) query_type: NamedTypeRef,
  #[serde(rename = "mutationType")]
  pub(super) mutation_type: Option<NamedTypeRef>,
  #[serde(rename = "subscriptionType")]
  pub(super) subscription_type: Option<NamedTypeRef>,
  pub(super) directives: Vec<IntrospectedDirective>,
}

/// A `__Type` reduced to its name, as `__Schema`'s three root slots return it.
#[derive(Debug, Deserialize)]
pub(super) struct NamedTypeRef {
  pub(super) name: Option<String>,
}

/// `__Type`, as a member of `__Schema.types`.
///
/// The five child lists are `Option` because the meta-schema makes them nullable and returns null
/// for the kinds they do not apply to: `fields` is null on a scalar, `possibleTypes` on an object,
/// and so on. Null and empty are not the same thing, and the difference is a refusal — draft §3
/// rejects an object with no fields, and the door must be able to hand that case to it rather than
/// silently rendering a type with no field block.
#[derive(Debug, Deserialize)]
pub(super) struct IntrospectedType {
  pub(super) kind: String,
  pub(super) name: Option<String>,
  pub(super) fields: Option<Vec<IntrospectedField>>,
  #[serde(rename = "inputFields")]
  pub(super) input_fields: Option<Vec<IntrospectedInputValue>>,
  pub(super) interfaces: Option<Vec<TypeRef>>,
  #[serde(rename = "enumValues")]
  pub(super) enum_values: Option<Vec<IntrospectedEnumValue>>,
  /// Read for a union and **ignored for an interface**, deliberately.
  ///
  /// A union's members are only in `possibleTypes`, so there is nowhere else to read them. An
  /// interface's implementors are derivable from the objects' own `interfaces`, which is the same
  /// path the SDL door takes and the one the build computes a transitive closure from — reading
  /// `possibleTypes` instead would make the implementor relation a second source of truth that a
  /// response could contradict.
  #[serde(rename = "possibleTypes")]
  pub(super) possible_types: Option<Vec<TypeRef>>,
  /// `@oneOf`, the one applied directive introspection reports that
  /// [`Schema`](crate::Schema) retains.
  #[serde(rename = "isOneOf", default)]
  pub(super) is_one_of: Option<bool>,
}

/// `__Field`.
#[derive(Debug, Deserialize)]
pub(super) struct IntrospectedField {
  pub(super) name: String,
  #[serde(default)]
  pub(super) args: Vec<IntrospectedInputValue>,
  #[serde(rename = "type")]
  pub(super) ty: TypeRef,
}

/// `__InputValue` — a field argument, a directive argument, or an input-object field.
#[derive(Debug, Deserialize)]
pub(super) struct IntrospectedInputValue {
  pub(super) name: String,
  #[serde(rename = "type")]
  pub(super) ty: TypeRef,
  /// The server's printed spelling of the default, or null for no default.
  ///
  /// `Schema` keeps only [`DefaultKind`](crate::DefaultKind) — presence and
  /// null-ness — so the spelling matters exactly twice: it must parse, and it must be
  /// distinguishable from `null`. Both fall out of handing it back to the const-value parser.
  #[serde(rename = "defaultValue")]
  pub(super) default_value: Option<String>,
}

/// `__EnumValue`.
#[derive(Debug, Deserialize)]
pub(super) struct IntrospectedEnumValue {
  pub(super) name: String,
}

/// `__Directive`.
#[derive(Debug, Deserialize)]
pub(super) struct IntrospectedDirective {
  pub(super) name: String,
  pub(super) locations: Vec<String>,
  #[serde(default)]
  pub(super) args: Vec<IntrospectedInputValue>,
  #[serde(rename = "isRepeatable", default)]
  pub(super) is_repeatable: bool,
}

/// `__Type` in its reference position: a wrapper chain bottoming out at a named type.
///
/// `Box` on `of_type` is what makes the recursion representable. Its depth is bounded by
/// `serde_json`'s own 128-deep nesting limit, which the reader enforces before this type is ever
/// constructed, so neither the deserializer nor the renderer that walks it can be driven off the
/// stack by a hostile response.
#[derive(Debug, Deserialize)]
pub(super) struct TypeRef {
  pub(super) kind: String,
  pub(super) name: Option<String>,
  #[serde(rename = "ofType", default)]
  pub(super) of_type: Option<Box<TypeRef>>,
}
