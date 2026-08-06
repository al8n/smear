//! The specification-provided part of every schema, as SDL.
//!
//! # Why the meta-schema is unconditional
//!
//! An introspection *query* is an executable document like any other, and draft 5.3.1 asks
//! whether each of its fields exists on the type it is selected from. `{ __schema { types { name
//! } } }` therefore only validates against a schema that knows `__Schema` and `__Type` — so the
//! meta-schema is injected into every schema this crate builds, not behind a feature. The
//! `introspection` cargo feature is a *construction door* (building a schema out of an
//! introspection response), which is a different thing and does not gate this.
//!
//! # Why the built-in scalars and directives are conditional
//!
//! `Int`, `Float`, `String`, `Boolean`, `ID` and the five specified directives are injected only
//! when the document does not define them. A document that spells one out replaces it, which is
//! what the ecosystem does and what a schema printed by one server and re-read by another
//! requires — a printed schema repeats the built-ins verbatim, and rejecting that would make
//! round-tripping impossible. The introspection types are not replaceable the same way: they are
//! `__`-prefixed, so a definition of one is a reserved-name violation, reported as
//! [`RedefinedBuiltInType`](super::SchemaErrorKind::RedefinedBuiltInType).

/// The five built-in scalars, injected when the document does not define them.
pub const BUILT_IN_SCALARS_SDL: &str = "\
scalar Int
scalar Float
scalar String
scalar Boolean
scalar ID
";

/// The names of the five built-in scalars, in the order [`BUILT_IN_SCALARS_SDL`] declares them.
pub const BUILT_IN_SCALARS: [&str; 5] = ["Int", "Float", "String", "Boolean", "ID"];

/// The five specified directives, injected when the document does not define them.
pub const BUILT_IN_DIRECTIVES_SDL: &str = "\
directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
directive @include(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
directive @deprecated(reason: String = \"No longer supported\") on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE
directive @specifiedBy(url: String!) on SCALAR
directive @oneOf on INPUT_OBJECT
";

/// The names of the five specified directives, without the `@`.
pub const BUILT_IN_DIRECTIVES: [&str; 5] =
  ["skip", "include", "deprecated", "specifiedBy", "oneOf"];

/// The draft §4 introspection type system, injected into every schema.
pub const INTROSPECTION_SDL: &str = "\
type __Schema {
  description: String
  types: [__Type!]!
  queryType: __Type!
  mutationType: __Type
  subscriptionType: __Type
  directives: [__Directive!]!
}

type __Type {
  kind: __TypeKind!
  name: String
  description: String
  specifiedByURL: String
  fields(includeDeprecated: Boolean = false): [__Field!]
  interfaces: [__Type!]
  possibleTypes: [__Type!]
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]
  inputFields(includeDeprecated: Boolean = false): [__InputValue!]
  ofType: __Type
  isOneOf: Boolean
}

type __Field {
  name: String!
  description: String
  args(includeDeprecated: Boolean = false): [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}

type __InputValue {
  name: String!
  description: String
  type: __Type!
  defaultValue: String
  isDeprecated: Boolean!
  deprecationReason: String
}

type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}

enum __TypeKind {
  SCALAR
  OBJECT
  INTERFACE
  UNION
  ENUM
  INPUT_OBJECT
  LIST
  NON_NULL
}

type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args(includeDeprecated: Boolean = false): [__InputValue!]!
  isRepeatable: Boolean!
}

enum __DirectiveLocation {
  QUERY
  MUTATION
  SUBSCRIPTION
  FIELD
  FRAGMENT_DEFINITION
  FRAGMENT_SPREAD
  INLINE_FRAGMENT
  VARIABLE_DEFINITION
  SCHEMA
  SCALAR
  OBJECT
  FIELD_DEFINITION
  ARGUMENT_DEFINITION
  INTERFACE
  UNION
  ENUM
  ENUM_VALUE
  INPUT_OBJECT
  INPUT_FIELD_DEFINITION
}
";

/// The names of the introspection types, in the order [`INTROSPECTION_SDL`] declares them.
pub const INTROSPECTION_TYPES: [&str; 8] = [
  "__Schema",
  "__Type",
  "__Field",
  "__InputValue",
  "__EnumValue",
  "__TypeKind",
  "__Directive",
  "__DirectiveLocation",
];

/// `__typename`, the meta-field every object, interface and union carries (draft §4.4).
///
/// The three meta-fields are assembled directly into the flattened field table rather than
/// declared as SDL: they belong to types the document defines, not to a type of their own, and
/// giving them a synthetic carrier would leave a phantom entry in the type table forever.
pub const TYPENAME_FIELD: &str = "__typename";

/// `__schema`, the query root's meta-field returning the whole schema (draft §4.4).
pub const SCHEMA_FIELD: &str = "__schema";

/// `__type`, the query root's meta-field returning one named type (draft §4.4).
pub const TYPE_FIELD: &str = "__type";

/// The name of `__type`'s single argument.
pub const TYPE_FIELD_ARGUMENT: &str = "name";
