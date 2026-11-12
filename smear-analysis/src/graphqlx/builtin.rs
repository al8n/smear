//! The built-in packages, `core`, `alloc` and `std`: smear's own, numbered before every supplied
//! package, and never supplied, shadowed or replaced by a caller.
//!
//! Each is one embedded GraphQLx file, `lib.graphqlx`, read exactly as a supplied package's files
//! are: numbered after its package's package source, parsed with the same parser on every
//! analysis, and answered by `SourceMap::bind`. The three are one library, so a definition of one
//! may name another's: `core`'s directives take `string!` arguments although `string` is
//! `alloc`'s.
//!
//! - `core` needs no allocator. It holds `NonZero`, `TypeRef`, the marker `MapKey`, `enum Tier`
//!   and the directives `@marker`, `@set`, `@map`, `@extern`, `@repr` and `@heap`; and GraphQL's
//!   own: the specified directives `@deprecated`, `@specifiedBy`, `@oneOf`, `@skip` and `@include`,
//!   with the signatures, locations and descriptions `smear-schema` gives them, and the
//!   introspection meta-types, `__Schema` to `__DirectiveLocation`.
//! - `alloc` needs an allocator. It holds `string`, `bytes`, `id`, `BTreeMap` and `BTreeSet`.
//! - `std` needs the standard library. It holds `HashMap` and `HashSet`.
//!
//! What source cannot say is a table here, keyed by what the model answers:
//!
//! - the intrinsic families, `bool`, `f32`, `f64`, `iN` and `uN`, recognised by pattern, of which
//!   the analysis materialises in `core` each one a program writes and may have (`intrinsic`,
//!   `written`, `available`), with `NonZeroIN` and `NonZeroUN` as names of `NonZero<iN>` and
//!   `NonZero<uN>`;
//! - GraphQL's five scalar names, each a second name of one definition (`ALIASES`);
//! - GraphQL's five scalars and five specified directives, which a root package's own definition
//!   replaces (`ALIASES` and `SPECIFIED_DIRECTIVES`);
//! - GraphQL's introspection meta-types, whose origin is `Compat` (`META_TYPES`).
//!
//! Each built-in definition requires its package's tier: the tier is a tag of the definition,
//! whatever its arguments name, and the library is analysed as one, at tier `Std`.

use super::{
  model::Primitive,
  package::{IntegerProvider, Tier},
};

/// One built-in package.
pub(crate) struct BuiltIn {
  /// The package's name, which no supplied package may take.
  pub(crate) name: &'static str,
  /// The tier the package's own definitions require.
  pub(crate) tier: Tier,
  /// The package's embedded source files, as `(path, text)` pairs, in any order: the analysis
  /// numbers them by path, as it numbers a supplied package's files.
  pub(crate) files: &'static [(&'static str, &'static str)],
}

/// The built-in packages, in [`PackageId`](super::PackageId) order.
pub(crate) const BUILT_INS: [BuiltIn; 3] = [
  BuiltIn {
    name: "core",
    tier: Tier::Core,
    files: CORE_FILES,
  },
  BuiltIn {
    name: "alloc",
    tier: Tier::Alloc,
    files: ALLOC_FILES,
  },
  BuiltIn {
    name: "std",
    tier: Tier::Std,
    files: STD_FILES,
  },
];

/// The index of `core` among the packages.
pub(crate) const CORE: usize = 0;

/// The index of `alloc` among the packages.
pub(crate) const ALLOC: usize = 1;

/// `core`'s embedded files.
const CORE_FILES: &[(&str, &str)] = &[("lib.graphqlx", CORE_LIB)];

/// `alloc`'s embedded files.
const ALLOC_FILES: &[(&str, &str)] = &[("lib.graphqlx", ALLOC_LIB)];

/// `std`'s embedded files.
const STD_FILES: &[(&str, &str)] = &[("lib.graphqlx", STD_LIB)];

/// `core`'s `lib.graphqlx`.
const CORE_LIB: &str = r#"# GraphQLx's own definitions that need no allocator.

"""
An integer that is never zero. `T` is an integer primitive, `iN` or `uN`, at least one bit wide, and `NonZeroU32`, `NonZeroI64` and their kin are second names of `NonZero<u32>`, `NonZero<i64>` and the like.
"""
scalar NonZero<T>

"The type of a directive argument whose value names a definition."
scalar TypeRef

"""
The marker a map's key type and a set's element type satisfy. Every integer primitive, `bool`, `string`, `id` and every enum satisfy it without saying so.
"""
interface MapKey @marker

"The tiers of the built-in library, each needing what the one before it needs, and more."
enum Tier {
  "Needs no allocator."
  CORE
  "Needs an allocator."
  ALLOC
  "Needs the standard library."
  STD
}

"Makes an interface a marker: it has no field, and only `implements` and `where` name it."
directive @marker on INTERFACE

"Makes a generic scalar of one type parameter a set of that type."
directive @set on SCALAR

"Makes a generic scalar of two type parameters a map from the first to the second."
directive @map on SCALAR

"""
Binds a scalar to a host type of one language: `type`, spelled as `lang` spells types, which smear never reads. `tier` is the tier the host type needs, or the declaring package's when it is not written.
"""
directive @extern(lang: string!, type: string!, tier: Tier) repeatable on SCALAR

"""
Chooses the host type of one language for a position, or, on the schema, for every position of the type `for` names. `for` is written on the schema and nowhere else.
"""
directive @repr(lang: string!, type: string!, for: TypeRef) repeatable on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | SCHEMA

"Keeps a position's value on the heap, which makes the position need an allocator."
directive @heap on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION

# GraphQL's specified directives, as smear-schema defines them.

"Directs the executor to skip this field or fragment when the `if` argument is true."
directive @skip("Skipped when true." if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
"Directs the executor to include this field or fragment only when the `if` argument is true."
directive @include("Included when true." if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
"Marks an element of a GraphQL schema as no longer supported."
directive @deprecated(
  """
  Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).
  """
  reason: String! = "No longer supported"
) on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE | DIRECTIVE_DEFINITION
"Exposes a URL that specifies the behavior of this scalar."
directive @specifiedBy("The URL that specifies the behavior of this scalar." url: String!) on SCALAR
"Indicates exactly one field must be supplied and this field must not be `null`."
directive @oneOf on INPUT_OBJECT

# GraphQL's introspection meta-types, kept for compatibility alone.

"A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations."
type __Schema {
  description: String
  "A list of all types supported by this server."
  types: [__Type!]!
  "The type that query operations will be rooted at."
  queryType: __Type!
  "If this server supports mutation, the type that mutation operations will be rooted at."
  mutationType: __Type
  "If this server support subscription, the type that subscription operations will be rooted at."
  subscriptionType: __Type
  "A list of all directives supported by this server."
  directives(includeDeprecated: Boolean! = false): [__Directive!]!
}

"""
The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.

Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name, description and optional `specifiedByURL`, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.
"""
type __Type {
  kind: __TypeKind!
  name: String
  description: String
  specifiedByURL: String
  fields(includeDeprecated: Boolean! = false): [__Field!]
  interfaces: [__Type!]
  possibleTypes: [__Type!]
  enumValues(includeDeprecated: Boolean! = false): [__EnumValue!]
  inputFields(includeDeprecated: Boolean! = false): [__InputValue!]
  ofType: __Type
  isOneOf: Boolean
}

"Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
type __Field {
  name: String!
  description: String
  args(includeDeprecated: Boolean! = false): [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}

"Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value."
type __InputValue {
  name: String!
  description: String
  type: __Type!
  "A GraphQL-formatted string representing the default value for this input value."
  defaultValue: String
  isDeprecated: Boolean!
  deprecationReason: String
}

"One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string."
type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}

"An enum describing what kind of type a given `__Type` is."
enum __TypeKind {
  "Indicates this type is a scalar."
  SCALAR
  "Indicates this type is an object. `fields` and `interfaces` are valid fields."
  OBJECT
  "Indicates this type is an interface. `fields`, `interfaces`, and `possibleTypes` are valid fields."
  INTERFACE
  "Indicates this type is a union. `possibleTypes` is a valid field."
  UNION
  "Indicates this type is an enum. `enumValues` is a valid field."
  ENUM
  "Indicates this type is an input object. `inputFields` is a valid field."
  INPUT_OBJECT
  "Indicates this type is a list. `ofType` is a valid field."
  LIST
  "Indicates this type is a non-null. `ofType` is a valid field."
  NON_NULL
}

"""
A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.

In some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.
"""
type __Directive {
  name: String!
  description: String
  isRepeatable: Boolean!
  locations: [__DirectiveLocation!]!
  args(includeDeprecated: Boolean! = false): [__InputValue!]!
  isDeprecated: Boolean!
  deprecationReason: String
}

"A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies."
enum __DirectiveLocation {
  "Location adjacent to a query operation."
  QUERY
  "Location adjacent to a mutation operation."
  MUTATION
  "Location adjacent to a subscription operation."
  SUBSCRIPTION
  "Location adjacent to a field."
  FIELD
  "Location adjacent to a fragment definition."
  FRAGMENT_DEFINITION
  "Location adjacent to a fragment spread."
  FRAGMENT_SPREAD
  "Location adjacent to an inline fragment."
  INLINE_FRAGMENT
  "Location adjacent to a variable definition."
  VARIABLE_DEFINITION
  "Location adjacent to a schema definition."
  SCHEMA
  "Location adjacent to a scalar definition."
  SCALAR
  "Location adjacent to an object type definition."
  OBJECT
  "Location adjacent to a field definition."
  FIELD_DEFINITION
  "Location adjacent to an argument definition."
  ARGUMENT_DEFINITION
  "Location adjacent to an interface definition."
  INTERFACE
  "Location adjacent to a union definition."
  UNION
  "Location adjacent to an enum definition."
  ENUM
  "Location adjacent to an enum value definition."
  ENUM_VALUE
  "Location adjacent to an input object type definition."
  INPUT_OBJECT
  "Location adjacent to an input object field definition."
  INPUT_FIELD_DEFINITION
  "Location adjacent to a directive definition."
  DIRECTIVE_DEFINITION
}
"#;

/// `alloc`'s `lib.graphqlx`.
const ALLOC_LIB: &str = r#"# The definitions that need an allocator.

"""
The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.
"""
scalar string

"A sequence of bytes, which need not be text."
scalar bytes

"""
The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `"4"`) or integer (such as `4`) input value will be accepted as an ID.
"""
scalar id

"A map from keys of type `K` to values of type `V`, ordered by key."
scalar BTreeMap<K, V> @map where K: MapKey

"A set of values of type `T`, ordered by value."
scalar BTreeSet<T> @set where T: MapKey
"#;

/// `std`'s `lib.graphqlx`.
const STD_LIB: &str = r#"# The definitions that need the standard library.

"A map from keys of type `K` to values of type `V`, in no particular order."
scalar HashMap<K, V> @map where K: MapKey

"A set of values of type `T`, in no particular order."
scalar HashSet<T> @set where T: MapKey
"#;

/// Whether `name` is a built-in package's name, which no supplied package may take.
pub(crate) fn is_built_in(name: &[u8]) -> bool {
  BUILT_INS
    .iter()
    .any(|package: &BuiltIn| package.name.as_bytes() == name)
}

/// The integer widths `core` provides itself, signed and unsigned. Every other width needs the
/// writing package's integer provider.
pub(crate) const CORE_WIDTHS: [u32; 5] = [8, 16, 32, 64, 128];

/// What a compatibility alias is a second name of.
#[derive(Clone, Copy)]
pub(crate) enum Target {
  /// An intrinsic primitive, which `core` materialises.
  Primitive(Primitive),
  /// The definition of this name in the root module of the built-in package at this index.
  Named(usize, &'static str),
}

/// GraphQL's built-in scalar names, each with the built-in package whose root module gives it as
/// a second name, and the one definition it names. A root package's own scalar of one of these
/// names, in its root module, replaces that definition.
pub(crate) const ALIASES: [(usize, &str, Target); 5] = [
  (CORE, "Int", Target::Primitive(Primitive::signed(32))),
  (CORE, "Float", Target::Primitive(Primitive::F64)),
  (CORE, "Boolean", Target::Primitive(Primitive::Bool)),
  (ALLOC, "String", Target::Named(ALLOC, "string")),
  (ALLOC, "ID", Target::Named(ALLOC, "id")),
];

/// GraphQL's specified directives, which `core` defines. A root package's own directive of one of
/// these names, in its root module, replaces `core`'s.
pub(crate) const SPECIFIED_DIRECTIVES: [&str; 5] =
  ["deprecated", "specifiedBy", "oneOf", "skip", "include"];

/// GraphQL's introspection meta-types, which `core` defines for GraphQL's sake alone: their origin
/// is `Compat`, and a consumer may skip them.
pub(crate) const META_TYPES: [&str; 8] = [
  "__Schema",
  "__Type",
  "__Field",
  "__InputValue",
  "__EnumValue",
  "__TypeKind",
  "__Directive",
  "__DirectiveLocation",
];

/// The intrinsic primitive `spelling` names, recognised by pattern: `bool`, `f32`, `f64`, and
/// `iN` or `uN` whose width `N` is written in decimal with no leading zero, `0` itself included,
/// and fits a `u32`. Whether a package may have the width is [`available`]'s question.
pub(crate) fn intrinsic(spelling: &[u8]) -> Option<Primitive> {
  match spelling {
    b"bool" => Some(Primitive::Bool),
    b"f32" => Some(Primitive::F32),
    b"f64" => Some(Primitive::F64),
    [b'i', digits @ ..] => width(digits).map(Primitive::signed),
    [b'u', digits @ ..] => width(digits).map(Primitive::unsigned),
    _ => None,
  }
}

/// The primitive whose definition a spelling in a type position, or in a `use`, needs: an
/// intrinsic primitive's own name; `Int`, `Float` or `Boolean`, which are second names of `i32`,
/// `f64` and `bool`; or `NonZeroIN` or `NonZeroUN`, `N` at least 1, which name `NonZero<iN>` and
/// `NonZero<uN>` and so write the width `N`.
pub(crate) fn written(spelling: &[u8]) -> Option<Primitive> {
  if let Some(primitive) = intrinsic(spelling) {
    return Some(primitive);
  }
  for (_, alias, target) in ALIASES {
    if let Target::Primitive(primitive) = target
      && alias.as_bytes() == spelling
    {
      return Some(primitive);
    }
  }
  let (signed, digits) = match spelling.strip_prefix(b"NonZero".as_slice())? {
    [b'I', digits @ ..] => (true, digits),
    [b'U', digits @ ..] => (false, digits),
    _ => return None,
  };
  match width(digits)? {
    0 => None,
    width if signed => Some(Primitive::signed(width)),
    width => Some(Primitive::unsigned(width)),
  }
}

/// Whether a package whose integer provider is `integers` may have `primitive`: `bool`, `f32`,
/// `f64` and `core`'s own widths always, and any other width only inside the range the provider
/// gives that signedness.
pub(crate) fn available(primitive: Primitive, integers: Option<&IntegerProvider>) -> bool {
  match primitive {
    Primitive::Int { signed, width } => {
      CORE_WIDTHS.contains(&width)
        || integers.is_some_and(|provider: &IntegerProvider| {
          let range = if signed {
            provider.signed()
          } else {
            provider.unsigned()
          };
          range.contains(&width)
        })
    }
    Primitive::Bool | Primitive::F32 | Primitive::F64 => true,
  }
}

/// The width `digits` write: decimal digits with no leading zero, or `0` alone, within a `u32`.
fn width(digits: &[u8]) -> Option<u32> {
  match digits {
    [] | [b'0', _, ..] => None,
    _ => {
      let mut width: u32 = 0;
      for &digit in digits {
        if !digit.is_ascii_digit() {
          return None;
        }
        width = width
          .checked_mul(10)?
          .checked_add(u32::from(digit - b'0'))?;
      }
      Some(width)
    }
  }
}
