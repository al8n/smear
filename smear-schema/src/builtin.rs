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
//!
//! # The descriptions are graphql-js's, not the specification's
//!
//! Every built-in definition carries the description graphql-js gives it, word for word. The
//! specification's prose about these types is written for a reader of the specification and is
//! not what any introspection client has ever been shown; draft §4 returns a built-in's description
//! like any other, so introspecting the built-ins answers what the reference implementation
//! answers. Validation reads none of it, and a description interns no name.
//!
//! What still differs from graphql-js's introspection of the same schema, recorded for the parity
//! work that compares the two: `__Directive` declares `isRepeatable` last where graphql-js declares
//! it third; `__Schema.types` lists the document's types in document order, then these, where
//! graphql-js walks the types reachable from the roots; and a built-in scalar nothing references is
//! still injected, where graphql-js leaves it out of the type map.

/// The five built-in scalars, injected when the document does not define them.
pub const BUILT_IN_SCALARS_SDL: &str = "\
\"\"\"
The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1.
\"\"\"
scalar Int
\"\"\"
The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point).
\"\"\"
scalar Float
\"\"\"
The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.
\"\"\"
scalar String
\"The `Boolean` scalar type represents `true` or `false`.\"
scalar Boolean
\"\"\"
The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID.
\"\"\"
scalar ID
";

/// The names of the five built-in scalars, in the order [`BUILT_IN_SCALARS_SDL`] declares them.
pub const BUILT_IN_SCALARS: [&str; 5] = ["Int", "Float", "String", "Boolean", "ID"];

/// The five specified directives, injected when the document does not define them.
pub const BUILT_IN_DIRECTIVES_SDL: &str = "\
\"Directs the executor to skip this field or fragment when the `if` argument is true.\"
directive @skip(\"Skipped when true.\" if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
\"Directs the executor to include this field or fragment only when the `if` argument is true.\"
directive @include(\"Included when true.\" if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
\"Marks an element of a GraphQL schema as no longer supported.\"
directive @deprecated(
  \"\"\"
  Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).
  \"\"\"
  reason: String = \"No longer supported\"
) on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE
\"Exposes a URL that specifies the behavior of this scalar.\"
directive @specifiedBy(\"The URL that specifies the behavior of this scalar.\" url: String!) on SCALAR
\"Indicates exactly one field must be supplied and this field must not be `null`.\"
directive @oneOf on INPUT_OBJECT
";

/// The names of the five specified directives, without the `@`.
pub const BUILT_IN_DIRECTIVES: [&str; 5] =
  ["skip", "include", "deprecated", "specifiedBy", "oneOf"];

/// The draft §4 introspection type system, injected into every schema.
pub const INTROSPECTION_SDL: &str = "\
\"A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.\"
type __Schema {
  description: String
  \"A list of all types supported by this server.\"
  types: [__Type!]!
  \"The type that query operations will be rooted at.\"
  queryType: __Type!
  \"If this server supports mutation, the type that mutation operations will be rooted at.\"
  mutationType: __Type
  \"If this server support subscription, the type that subscription operations will be rooted at.\"
  subscriptionType: __Type
  \"A list of all directives supported by this server.\"
  directives: [__Directive!]!
}

\"\"\"
The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.

Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name, description and optional `specifiedByURL`, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.
\"\"\"
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

\"Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.\"
type __Field {
  name: String!
  description: String
  args(includeDeprecated: Boolean = false): [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}

\"Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.\"
type __InputValue {
  name: String!
  description: String
  type: __Type!
  \"A GraphQL-formatted string representing the default value for this input value.\"
  defaultValue: String
  isDeprecated: Boolean!
  deprecationReason: String
}

\"One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.\"
type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}

\"An enum describing what kind of type a given `__Type` is.\"
enum __TypeKind {
  \"Indicates this type is a scalar.\"
  SCALAR
  \"Indicates this type is an object. `fields` and `interfaces` are valid fields.\"
  OBJECT
  \"Indicates this type is an interface. `fields`, `interfaces`, and `possibleTypes` are valid fields.\"
  INTERFACE
  \"Indicates this type is a union. `possibleTypes` is a valid field.\"
  UNION
  \"Indicates this type is an enum. `enumValues` is a valid field.\"
  ENUM
  \"Indicates this type is an input object. `inputFields` is a valid field.\"
  INPUT_OBJECT
  \"Indicates this type is a list. `ofType` is a valid field.\"
  LIST
  \"Indicates this type is a non-null. `ofType` is a valid field.\"
  NON_NULL
}

\"\"\"
A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.

In some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.
\"\"\"
type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args(includeDeprecated: Boolean = false): [__InputValue!]!
  isRepeatable: Boolean!
}

\"A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.\"
enum __DirectiveLocation {
  \"Location adjacent to a query operation.\"
  QUERY
  \"Location adjacent to a mutation operation.\"
  MUTATION
  \"Location adjacent to a subscription operation.\"
  SUBSCRIPTION
  \"Location adjacent to a field.\"
  FIELD
  \"Location adjacent to a fragment definition.\"
  FRAGMENT_DEFINITION
  \"Location adjacent to a fragment spread.\"
  FRAGMENT_SPREAD
  \"Location adjacent to an inline fragment.\"
  INLINE_FRAGMENT
  \"Location adjacent to a variable definition.\"
  VARIABLE_DEFINITION
  \"Location adjacent to a schema definition.\"
  SCHEMA
  \"Location adjacent to a scalar definition.\"
  SCALAR
  \"Location adjacent to an object type definition.\"
  OBJECT
  \"Location adjacent to a field definition.\"
  FIELD_DEFINITION
  \"Location adjacent to an argument definition.\"
  ARGUMENT_DEFINITION
  \"Location adjacent to an interface definition.\"
  INTERFACE
  \"Location adjacent to a union definition.\"
  UNION
  \"Location adjacent to an enum definition.\"
  ENUM
  \"Location adjacent to an enum value definition.\"
  ENUM_VALUE
  \"Location adjacent to an input object type definition.\"
  INPUT_OBJECT
  \"Location adjacent to an input object field definition.\"
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
