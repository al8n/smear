//! The introspection construction door, and the property that makes it a door rather than a
//! second implementation.
//!
//! # "Parse some JSON, get a `Schema`" proves nothing
//!
//! It proves the door returns `Ok`. It says nothing about whether the `Schema` it returned is the
//! one the response described, and a door that is 90% right returns `Ok` too. The property worth
//! pinning is **agreement between the two doors**: the same schema, built from SDL and built from
//! an introspection result describing it, must be indistinguishable to every rule that will ever
//! read it.
//!
//! [`two_doors_agree`] is that test. It takes [`FIXTURE`], builds it with
//! [`Schema::build`], turns the resulting schema back into the introspection response a
//! conforming server would return for it, feeds that to
//! [`Schema::from_introspection`], and compares the two schemas on every observation a rule can
//! make: type lookup, kind, flags, field lookup, argument lists, packed type spellings,
//! [`DefaultKind`], enum values, interface closures, union members, possible-object bitsets,
//! directive locations and repeatability, and the three root operations. Any divergence is a bug
//! in one door or the other, and **no self-consistent test on either side would find it** — the
//! SDL door has no reason to disagree with itself and neither has the introspection door.
//!
//! # Why the response is generated rather than written by hand
//!
//! A hand-written response would test that one transcription of one schema was transcribed
//! correctly. [`server`] generates the response from the built schema instead, so every construct
//! the fixture contains is covered without anyone remembering to cover it, and adding a construct
//! to [`FIXTURE`] extends the gate for free. The two directions are structurally unalike — one
//! reads `Schema`'s tables and writes JSON, the other reads JSON and writes SDL — so a pair of
//! cancelling bugs is not a realistic failure mode, and [`the_door_reads_a_real_response`] pins
//! the door against a response nobody generated as well.
//!
//! # What the comparison deliberately does not cover, and why
//!
//! Stated here rather than silently omitted, because a comparison that skips a field is a
//! comparison that cannot fail on it:
//!
//! - **Symbol values, type ids and object ordinals.** They are indices into tables built in
//!   document order, and the two doors read definitions in different orders. Every fact below is
//!   therefore keyed by *name*: [`Facts`] stores `BTreeMap`s and sorted `Vec`s, never a raw
//!   [`Sym`](smear::validator::schema::Sym) or [`TypeId`](smear::validator::schema::TypeId).
//! - **Descriptions, deprecation and `specifiedByURL`.** A `Schema` retains none of them, from
//!   either door. There is nothing to compare.
//! - **Applied directives other than `@oneOf`.** Introspection has no field for them, so the
//!   response cannot carry them; `@oneOf` is the only one a `Schema` observes, as
//!   `TypeFlags::ONE_OF`, and `isOneOf` carries it. [`one_of_survives_the_round_trip`] pins that
//!   one rather than leaving it to the aggregate.
//! - **A default value's content.** `Schema` keeps [`DefaultKind`] and nothing else, so the
//!   generated response cannot restate the original spelling — it synthesises a literal of the
//!   right shape. Presence and null-ness are what survive and what the comparison covers.
//! - **A redefined built-in scalar or specified directive.** `scalar String` in a document makes
//!   `TypeDef::is_built_in` false; an introspection result reports `String` identically either
//!   way, so the flag is unrecoverable. The fixture does not redefine one, and
//!   [`redefining_a_built_in_is_the_one_flag_introspection_loses`] pins the loss instead of hiding
//!   it.

#![allow(missing_docs)]

use std::collections::BTreeMap;

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::TypeSystemDocument,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  validator::schema::{
    DefaultKind, DirectiveDef, FieldDef, InputValueDef, IntrospectionError, PackedType,
    RootOperation, Schema, SchemaErrorKind, TypeDef, TypeId, TypeKind,
    introspection::{self, ResponseErrorKind},
  },
};

// ---------------------------------------------------------------------------------------------
// harness
// ---------------------------------------------------------------------------------------------

fn parse(sdl: &str) -> TypeSystemDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("fixture SDL does not parse: {errors:?}\n---\n{sdl}"))
}

fn built(sdl: &str) -> Schema {
  Schema::build(&parse(sdl))
    .unwrap_or_else(|errors| panic!("expected a schema, got:\n{errors}\n---\n{sdl}"))
}

fn from_response(json: &str) -> Schema {
  Schema::from_introspection(json)
    .unwrap_or_else(|error| panic!("expected a schema, got:\n{error}\n---\n{json}"))
}

fn refused(json: &str) -> IntrospectionError {
  match Schema::from_introspection(json) {
    Err(error) => error,
    Ok(_) => panic!("expected a refusal, got a schema\n---\n{json}"),
  }
}

// ---------------------------------------------------------------------------------------------
// the fixture
// ---------------------------------------------------------------------------------------------

/// One schema exercising every construct a `Schema` can hold.
///
/// Deliberately dense: the agreement test's reach is exactly this document's, so anything not
/// spelled here is a shape the two doors have never been compared on. Present on purpose —
/// non-conventional root names (so the `schema` block has to round-trip rather than the
/// `Query`/`Mutation` convention covering for it), an interface implementing an interface, a
/// five-wrapper type reference, all three [`DefaultKind`]s, a `@oneOf` input object, a repeatable
/// directive, applied directives that introspection cannot carry, and a custom scalar.
const FIXTURE: &str = r#"
schema {
  query: RootQuery
  mutation: RootMutation
  subscription: RootSubscription
}

"An instant on a timeline."
scalar DateTime @specifiedBy(url: "https://example.com/spec/datetime")

directive @auth(
  role: Role! = ADMIN
  scopes: [String!] = null
  note: String
) repeatable on FIELD_DEFINITION | OBJECT | QUERY

directive @tag(name: String!) on FIELD_DEFINITION | ENUM_VALUE

interface Node {
  id: ID!
}

interface Timestamped implements Node {
  id: ID!
  createdAt: DateTime
}

type User implements Node & Timestamped @auth(role: MEMBER) {
  id: ID!
  createdAt: DateTime
  name: String!
  nicknames: [[String!]!]!
  role: Role! @deprecated(reason: "use roles")
  posts(first: Int = 10, after: String, filter: PostFilter, tags: [String!] = null): [Post!]!
}

type Post implements Node {
  id: ID!
  title: String
  author: User!
}

union SearchResult = User | Post

enum Role {
  ADMIN
  MEMBER
  GUEST @tag(name: "legacy")
}

input PostFilter {
  titleContains: String
  published: Boolean = true
  authored: AuthorFilter
  ranked: [[Int]!]
}

input AuthorFilter @oneOf {
  byId: ID
  byName: String
}

type RootQuery {
  node(id: ID!): Node
  search(term: String!, limit: Int = 25): [SearchResult!]!
  me: User @auth(role: ADMIN)
  when: DateTime
}

type RootMutation {
  createPost(filter: PostFilter!, draft: Boolean = null): Post
}

type RootSubscription {
  posted: Post!
}
"#;

// ---------------------------------------------------------------------------------------------
// the server side: a `Schema` rendered as the introspection response one would return for it
// ---------------------------------------------------------------------------------------------

/// Generates the draft §4 response a conforming server returns for a given [`Schema`].
///
/// It emits what a real server emits, meta-schema and built-ins included, because that is the
/// input the door has to survive: `__Schema` and friends are in every real response and the door
/// has to drop them, `Int` and `@skip` are in every real response and the door has to drop those
/// too. Emitting a tidied response would test a door nobody has.
///
/// What it omits is what a real server omits: the three meta-*fields*. `__typename`, `__schema`
/// and `__type` are in `Schema`'s field tables — every build injects them — and in no server's
/// `__Type.fields`, because draft §4.4 defines them outside the type system.
///
/// The JSON is assembled with `format!` rather than a serializer on purpose. The door is the code
/// under test and it uses `serde_json`; a test that shared that library would be unable to
/// distinguish "the door reads JSON correctly" from "both sides agree on a mistake". Everything
/// interpolated is either a GraphQL name, a fixed literal, or passed through [`json_string`].
mod server {
  use super::*;

  pub fn introspect(schema: &Schema) -> String {
    let types: Vec<String> = schema
      .types()
      .iter()
      .enumerate()
      .map(|(index, def)| ty(schema, TypeId::new(index as u32), def))
      .collect();
    let directives: Vec<String> = schema
      .directives()
      .iter()
      .map(|d| directive(schema, d))
      .collect();

    std::format!(
      r#"{{"data":{{"__schema":{{"queryType":{},"mutationType":{},"subscriptionType":{},"types":[{}],"directives":[{}]}}}}}}"#,
      root(schema, RootOperation::Query),
      root(schema, RootOperation::Mutation),
      root(schema, RootOperation::Subscription),
      types.join(","),
      directives.join(","),
    )
  }

  fn root(schema: &Schema, operation: RootOperation) -> String {
    match schema.root(operation) {
      Some(id) => std::format!(r#"{{"name":{}}}"#, json_string(type_name(schema, id))),
      None => "null".into(),
    }
  }

  fn ty(schema: &Schema, id: TypeId, def: &TypeDef) -> String {
    let kind = def.kind();

    let fields = match kind {
      TypeKind::Object | TypeKind::Interface => {
        let rows: Vec<String> = schema
          .fields_of(id)
          .iter()
          // The meta-fields. `Schema` holds them, no server reports them, and the build puts them
          // back — so omitting them here is what makes the round-trip realistic AND correct.
          .filter(|field| !schema.name(field.name()).starts_with("__"))
          .map(|field| self::field(schema, field))
          .collect();
        std::format!("[{}]", rows.join(","))
      }
      _ => "null".into(),
    };

    let input_fields = match kind {
      TypeKind::InputObject => {
        let rows: Vec<String> = schema
          .input_fields_of(id)
          .iter()
          .map(|value| input_value(schema, value))
          .collect();
        std::format!("[{}]", rows.join(","))
      }
      _ => "null".into(),
    };

    let interfaces = match kind {
      TypeKind::Object | TypeKind::Interface => named_list(schema, schema.interfaces_of(id)),
      _ => "null".into(),
    };

    let enum_values = match kind {
      TypeKind::Enum => {
        let rows: Vec<String> = schema
          .enum_values_of(id)
          .iter()
          .map(|value| std::format!(r#"{{"name":{}}}"#, json_string(schema.name(*value))))
          .collect();
        std::format!("[{}]", rows.join(","))
      }
      _ => "null".into(),
    };

    // A union reports its members; an interface reports its implementors, which the door
    // deliberately ignores in favour of deriving them from `interfaces` — emitting it is how that
    // choice gets exercised rather than assumed.
    let possible_types = match kind {
      TypeKind::Union => named_list(schema, schema.members_of(id)),
      TypeKind::Interface => {
        let ids: Vec<TypeId> = schema.possible_object_ids(id).collect();
        named_list(schema, &ids)
      }
      _ => "null".into(),
    };

    let is_one_of = match kind {
      TypeKind::InputObject => {
        if def.is_one_of() {
          "true"
        } else {
          "false"
        }
      }
      _ => "null",
    };

    std::format!(
      r#"{{"kind":"{}","name":{},"description":null,"specifiedByURL":null,"fields":{},"inputFields":{},"interfaces":{},"enumValues":{},"possibleTypes":{},"isOneOf":{}}}"#,
      kind.as_str(),
      json_string(schema.name(def.name())),
      fields,
      input_fields,
      interfaces,
      enum_values,
      possible_types,
      is_one_of,
    )
  }

  fn field(schema: &Schema, field: &FieldDef) -> String {
    let args: Vec<String> = schema
      .inputs(field.args())
      .iter()
      .map(|arg| input_value(schema, arg))
      .collect();
    std::format!(
      r#"{{"name":{},"description":null,"args":[{}],"type":{},"isDeprecated":false,"deprecationReason":null}}"#,
      json_string(schema.name(field.name())),
      args.join(","),
      type_ref(schema, field.ty()),
    )
  }

  fn input_value(schema: &Schema, value: &InputValueDef) -> String {
    std::format!(
      r#"{{"name":{},"description":null,"type":{},"defaultValue":{}}}"#,
      json_string(schema.name(value.name())),
      type_ref(schema, value.ty()),
      default_value(schema, value),
    )
  }

  /// A default value of the right *kind*, which is all a `Schema` retains.
  ///
  /// The original spelling is gone — the build reduced it to [`DefaultKind`] — so the response
  /// carries a literal that the base type would plausibly accept rather than the one the document
  /// wrote. Presence and null-ness are what round-trip, and what the comparison checks.
  fn default_value(schema: &Schema, value: &InputValueDef) -> String {
    match value.default_kind() {
      DefaultKind::Absent => "null".into(),
      DefaultKind::Null => json_string("null"),
      DefaultKind::NonNull => {
        let base = schema.type_def(value.ty().base_id());
        let literal: String = match base.kind() {
          TypeKind::Enum => schema
            .enum_values_of(value.ty().base_id())
            .first()
            .map(|first| schema.name(*first).into())
            .unwrap_or_else(|| "null".into()),
          TypeKind::InputObject => "{}".into(),
          TypeKind::Scalar => match schema.name(base.name()) {
            "String" | "ID" => "\"x\"".into(),
            "Boolean" => "true".into(),
            _ => "42".into(),
          },
          _ => "42".into(),
        };
        json_string(&literal)
      }
    }
  }

  fn directive(schema: &Schema, directive: &DirectiveDef) -> String {
    let locations: Vec<String> = directive
      .locations()
      .iter()
      .map(|location| json_string(location.as_str()))
      .collect();
    let args: Vec<String> = schema
      .inputs(directive.args())
      .iter()
      .map(|arg| input_value(schema, arg))
      .collect();
    std::format!(
      r#"{{"name":{},"description":null,"locations":[{}],"args":[{}],"isRepeatable":{}}}"#,
      json_string(schema.name(directive.name())),
      locations.join(","),
      args.join(","),
      directive.is_repeatable(),
    )
  }

  fn type_ref(schema: &Schema, ty: PackedType) -> String {
    if ty.is_non_null() {
      let inner = ty.strip_outer().expect("a non-null has an inner type");
      return std::format!(
        r#"{{"kind":"NON_NULL","name":null,"ofType":{}}}"#,
        type_ref(schema, inner)
      );
    }
    if ty.is_list() {
      let inner = ty.strip_outer().expect("a list has an item type");
      return std::format!(
        r#"{{"kind":"LIST","name":null,"ofType":{}}}"#,
        type_ref(schema, inner)
      );
    }
    std::format!(
      r#"{{"kind":"{}","name":{},"ofType":null}}"#,
      schema.type_def(ty.base_id()).kind().as_str(),
      json_string(schema.name(ty.base())),
    )
  }

  fn named_list(schema: &Schema, ids: &[TypeId]) -> String {
    let rows: Vec<String> = ids
      .iter()
      .map(|id| {
        std::format!(
          r#"{{"kind":"{}","name":{},"ofType":null}}"#,
          schema.type_def(*id).kind().as_str(),
          json_string(type_name(schema, *id)),
        )
      })
      .collect();
    std::format!("[{}]", rows.join(","))
  }

  fn type_name(schema: &Schema, id: TypeId) -> &str {
    schema.name(schema.type_def(id).name())
  }

  /// A JSON string literal. Only `"` and `\` can occur in anything this test emits — GraphQL names
  /// are ASCII identifiers and the synthesised default literals are ASCII — and both are escaped.
  fn json_string(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
      match ch {
        '"' => out.push_str("\\\""),
        '\\' => out.push_str("\\\\"),
        other => out.push(other),
      }
    }
    out.push('"');
    out
  }
}

// ---------------------------------------------------------------------------------------------
// facts: everything a rule can observe, keyed by name
// ---------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq)]
struct Facts {
  /// The width of one possible-object bitset. Different object counts would change every
  /// bitset's meaning, so it is compared before the bitsets are.
  possible_words: u32,
  roots: [Option<String>; 3],
  types: BTreeMap<String, TypeFacts>,
  directives: BTreeMap<String, DirectiveFacts>,
}

#[derive(Debug, PartialEq, Eq)]
struct TypeFacts {
  kind: &'static str,
  one_of: bool,
  built_in: bool,
  /// Whether the type occupies a bit in the possible-object bitsets.
  has_object_ordinal: bool,
  fields: BTreeMap<String, FieldFacts>,
  input_fields: BTreeMap<String, InputFacts>,
  enum_values: Vec<String>,
  interfaces: Vec<String>,
  members: Vec<String>,
  /// The possible-object bitset, read back as the set of object type *names* it selects.
  possible: Option<Vec<String>>,
}

#[derive(Debug, PartialEq, Eq)]
struct FieldFacts {
  ty: String,
  args: BTreeMap<String, InputFacts>,
}

#[derive(Debug, PartialEq, Eq)]
struct InputFacts {
  ty: String,
  default: &'static str,
  required: bool,
}

#[derive(Debug, PartialEq, Eq)]
struct DirectiveFacts {
  locations: u32,
  repeatable: bool,
  built_in: bool,
  args: BTreeMap<String, InputFacts>,
}

/// Reads every observation a validation rule can make out of a schema, keyed by name.
///
/// Two things happen here beyond collecting. Every field, input field, argument and directive is
/// *looked up* as well as iterated, so the sym-sorted binary-search invariant is exercised on both
/// schemas rather than assumed; and every type reference is spelled out through
/// [`PackedType`]'s wrappers, so a mis-packed reference shows up as a different string rather than
/// as a different integer nobody compares.
fn facts(schema: &Schema) -> Facts {
  let mut types = BTreeMap::new();
  for (index, def) in schema.types().iter().enumerate() {
    let id = TypeId::new(index as u32);
    let name = schema.name(def.name()).to_owned();

    let (found_id, found_def) = schema
      .type_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("`{name}` is in the table but `type_by_name` misses it"));
    assert_eq!(found_id, id, "`{name}` resolves to a different row");
    assert_eq!(found_def.kind(), def.kind());

    let mut fields = BTreeMap::new();
    for field in schema.fields_of(id) {
      let field_name = schema.name(field.name()).to_owned();
      let looked_up = schema
        .field(id, field.name())
        .unwrap_or_else(|| panic!("`{name}.{field_name}` is in the group but lookup misses it"));
      assert_eq!(looked_up.ty(), field.ty());
      fields.insert(
        field_name,
        FieldFacts {
          ty: spell(schema, field.ty()),
          args: inputs(schema, field.args()),
        },
      );
    }

    let mut input_fields = BTreeMap::new();
    for value in schema.input_fields_of(id) {
      let value_name = schema.name(value.name()).to_owned();
      let looked_up = schema
        .input_field(id, value.name())
        .unwrap_or_else(|| panic!("`{name}.{value_name}` is in the group but lookup misses it"));
      assert_eq!(looked_up.ty(), value.ty());
      input_fields.insert(value_name, input_facts(schema, value));
    }

    let mut enum_values: Vec<String> = schema
      .enum_values_of(id)
      .iter()
      .map(|value| {
        assert!(
          schema.has_enum_value(id, *value),
          "`{name}` lists a value `has_enum_value` cannot find"
        );
        schema.name(*value).to_owned()
      })
      .collect();
    enum_values.sort();

    let mut interfaces = sorted_names(schema, schema.interfaces_of(id));
    interfaces.dedup();
    let members = sorted_names(schema, schema.members_of(id));

    let possible = schema.possible_objects(id).map(|_| {
      let mut names: Vec<String> = schema
        .possible_object_ids(id)
        .map(|object| {
          assert!(
            schema.is_possible_object(id, object),
            "`{name}`'s bitset selects an object `is_possible_object` denies"
          );
          schema.name(schema.type_def(object).name()).to_owned()
        })
        .collect();
      names.sort();
      names
    });

    types.insert(
      name,
      TypeFacts {
        kind: def.kind().as_str(),
        one_of: def.is_one_of(),
        built_in: def.is_built_in(),
        has_object_ordinal: def.object_ordinal() != TypeDef::NONE,
        fields,
        input_fields,
        enum_values,
        interfaces,
        members,
        possible,
      },
    );
  }

  let mut directives = BTreeMap::new();
  for def in schema.directives() {
    let name = schema.name(def.name()).to_owned();
    let looked_up = schema
      .directive_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("`@{name}` is in the table but lookup misses it"));
    assert_eq!(looked_up.locations().bits(), def.locations().bits());
    directives.insert(
      name,
      DirectiveFacts {
        locations: def.locations().bits(),
        repeatable: def.is_repeatable(),
        built_in: def.is_built_in(),
        args: inputs(schema, def.args()),
      },
    );
  }

  let roots = [
    RootOperation::Query,
    RootOperation::Mutation,
    RootOperation::Subscription,
  ]
  .map(|operation| {
    schema
      .root(operation)
      .map(|id| schema.name(schema.type_def(id).name()).to_owned())
  });

  Facts {
    possible_words: schema.possible_words(),
    roots,
    types,
    directives,
  }
}

fn inputs(
  schema: &Schema,
  args: smear::validator::schema::Range32,
) -> BTreeMap<String, InputFacts> {
  let mut out = BTreeMap::new();
  for value in schema.inputs(args) {
    let name = schema.name(value.name()).to_owned();
    let looked_up = schema
      .input(args, value.name())
      .unwrap_or_else(|| panic!("`{name}` is in the group but lookup misses it"));
    assert_eq!(looked_up.ty(), value.ty());
    out.insert(name, input_facts(schema, value));
  }
  out
}

fn input_facts(schema: &Schema, value: &InputValueDef) -> InputFacts {
  InputFacts {
    ty: spell(schema, value.ty()),
    default: value.default_kind().as_str(),
    required: value.is_required(),
  }
}

fn sorted_names(schema: &Schema, ids: &[TypeId]) -> Vec<String> {
  let mut names: Vec<String> = ids
    .iter()
    .map(|id| schema.name(schema.type_def(*id).name()).to_owned())
    .collect();
  names.sort();
  names
}

/// The GraphQL spelling of a packed type reference.
fn spell(schema: &Schema, ty: PackedType) -> String {
  if ty.is_non_null() {
    let inner = ty.strip_outer().expect("a non-null has an inner type");
    return std::format!("{}!", spell(schema, inner));
  }
  if ty.is_list() {
    let inner = ty.strip_outer().expect("a list has an item type");
    return std::format!("[{}]", spell(schema, inner));
  }
  schema.name(ty.base()).to_owned()
}

// ---------------------------------------------------------------------------------------------
// the gate
// ---------------------------------------------------------------------------------------------

#[test]
fn two_doors_agree() {
  let from_sdl = built(FIXTURE);
  let response = server::introspect(&from_sdl);
  let from_json = from_response(&response);

  assert_eq!(
    facts(&from_sdl),
    facts(&from_json),
    "the SDL door and the introspection door built different schemas for the same schema"
  );
}

/// The agreement above must not pass by comparing two nearly-empty schemas.
///
/// A census over a thin fixture is the failure mode this file exists to avoid, so the fixture's
/// reach is asserted rather than reviewed: every type kind present, both abstract kinds carrying
/// possible-object sets, all three [`DefaultKind`]s, a wrapper chain deeper than two, and all
/// three roots.
#[test]
fn the_agreement_is_not_vacuous() {
  let schema = built(FIXTURE);
  let observed = facts(&schema);

  for name in [
    "RootQuery",
    "RootMutation",
    "RootSubscription",
    "User",
    "Post",
    "Node",
    "Timestamped",
    "SearchResult",
    "Role",
    "PostFilter",
    "AuthorFilter",
    "DateTime",
  ] {
    assert!(
      observed.types.contains_key(name),
      "the fixture no longer defines `{name}`; the gate's reach shrank"
    );
  }
  for name in ["auth", "tag", "skip", "include", "deprecated"] {
    assert!(
      observed.directives.contains_key(name),
      "the fixture no longer reaches `@{name}`"
    );
  }
  assert_eq!(
    observed.roots,
    [
      Some("RootQuery".to_owned()),
      Some("RootMutation".to_owned()),
      Some("RootSubscription".to_owned()),
    ]
  );

  let kinds: std::collections::BTreeSet<&str> = observed.types.values().map(|ty| ty.kind).collect();
  assert_eq!(
    kinds,
    [
      "ENUM",
      "INPUT_OBJECT",
      "INTERFACE",
      "OBJECT",
      "SCALAR",
      "UNION"
    ]
    .into_iter()
    .collect::<std::collections::BTreeSet<_>>(),
    "the fixture no longer reaches every type kind"
  );

  assert!(
    observed.types["AuthorFilter"].one_of,
    "@oneOf is not reached"
  );
  assert!(
    observed.directives["auth"].repeatable,
    "repeatable is not reached"
  );
  assert_eq!(
    observed.types["User"].fields["nicknames"].ty, "[[String!]!]!",
    "the deep wrapper chain is not reached"
  );
  assert_eq!(
    observed.types["SearchResult"].possible,
    Some(vec!["Post".to_owned(), "User".to_owned()]),
    "the union's possible-object set is not reached"
  );
  assert_eq!(
    observed.types["Node"].possible,
    Some(vec!["Post".to_owned(), "User".to_owned()]),
    "the interface's possible-object set is not reached"
  );
  assert_eq!(
    observed.types["Timestamped"].interfaces,
    vec!["Node".to_owned()]
  );

  let defaults: std::collections::BTreeSet<&str> = observed.directives["auth"]
    .args
    .values()
    .map(|arg| arg.default)
    .collect();
  assert_eq!(
    defaults,
    ["absent", "non-null", "null"]
      .into_iter()
      .collect::<std::collections::BTreeSet<_>>(),
    "the fixture no longer reaches all three DefaultKinds"
  );
}

/// `@oneOf` is the only applied directive a `Schema` retains, so it is pinned on its own rather
/// than trusted to the aggregate.
#[test]
fn one_of_survives_the_round_trip() {
  let from_sdl = built(FIXTURE);
  let from_json = from_response(&server::introspect(&from_sdl));

  for schema in [&from_sdl, &from_json] {
    let (author, def) = schema.type_by_name(b"AuthorFilter").expect("AuthorFilter");
    assert!(def.is_one_of(), "`@oneOf` was lost");
    let (filter, def) = schema.type_by_name(b"PostFilter").expect("PostFilter");
    assert!(!def.is_one_of(), "`@oneOf` was invented");
    assert_ne!(author, filter);
  }
}

/// The one flag introspection cannot carry, pinned as a loss rather than left to be discovered.
///
/// A document that spells out `scalar String` gets a `String` whose `is_built_in` is false. The
/// introspection result for that schema is byte-identical to the result for one that did not, so
/// the door cannot reproduce the flag and the round-trip diverges on it. This test is the record:
/// if a future change makes the flag recoverable, it fails and the module header's table is wrong.
#[test]
fn redefining_a_built_in_is_the_one_flag_introspection_loses() {
  let sdl = "type Query { ok: String } scalar String";
  let from_sdl = built(sdl);
  let from_json = from_response(&server::introspect(&from_sdl));

  let (_, sdl_string) = from_sdl.type_by_name(b"String").expect("String");
  let (_, json_string) = from_json.type_by_name(b"String").expect("String");
  assert!(
    !sdl_string.is_built_in(),
    "a spelled-out scalar is not built-in"
  );
  assert!(
    json_string.is_built_in(),
    "the door recovered the flag; the loss recorded in the module header is stale"
  );

  // And it is the *only* divergence: everything else about the two schemas still agrees.
  let mut sdl_facts = facts(&from_sdl);
  let mut json_facts = facts(&from_json);
  sdl_facts.types.get_mut("String").expect("String").built_in = true;
  json_facts.types.get_mut("String").expect("String").built_in = true;
  assert_eq!(sdl_facts, json_facts);
}

// ---------------------------------------------------------------------------------------------
// the door against a response nobody generated
// ---------------------------------------------------------------------------------------------

/// A response written by hand, in the shape a server returns.
///
/// The generated response above is exhaustive but shares an author with the door. This one shares
/// nothing: it carries real default-value spellings, a `data` envelope, deprecation fields the
/// door ignores, and `isRepeatable` where the door reads it.
const HAND_WRITTEN: &str = r#"
{"data":{"__schema":{
  "queryType": {"name": "Query"},
  "mutationType": null,
  "subscriptionType": null,
  "directives": [
    {"name":"skip","locations":["FIELD"],"args":[
      {"name":"if","type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}},"defaultValue":null}
    ],"isRepeatable":false},
    {"name":"cacheControl","description":"Cache hints.","locations":["FIELD_DEFINITION","OBJECT"],"args":[
      {"name":"maxAge","type":{"kind":"SCALAR","name":"Int"},"defaultValue":"0"},
      {"name":"scope","type":{"kind":"ENUM","name":"CacheScope"},"defaultValue":"PUBLIC"}
    ],"isRepeatable":true}
  ],
  "types": [
    {"kind":"OBJECT","name":"Query","description":"The root.","interfaces":[],"fields":[
      {"name":"greeting","description":"Says hello.","args":[
        {"name":"to","type":{"kind":"SCALAR","name":"String"},"defaultValue":"\"world\""},
        {"name":"loudly","type":{"kind":"SCALAR","name":"Boolean"},"defaultValue":"null"},
        {"name":"repeats","type":{"kind":"SCALAR","name":"Int"},"defaultValue":null}
      ],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}},
      "isDeprecated":false,"deprecationReason":null},
      {"name":"scope","args":[],"type":{"kind":"ENUM","name":"CacheScope"},
      "isDeprecated":true,"deprecationReason":"unused"}
    ]},
    {"kind":"ENUM","name":"CacheScope","enumValues":[
      {"name":"PUBLIC","isDeprecated":false},{"name":"PRIVATE","isDeprecated":false}
    ]},
    {"kind":"SCALAR","name":"String"},
    {"kind":"SCALAR","name":"Boolean"},
    {"kind":"SCALAR","name":"Int"},
    {"kind":"OBJECT","name":"__Schema","fields":[
      {"name":"types","args":[],"type":{"kind":"SCALAR","name":"String"}}
    ]}
  ]
}}}
"#;

#[test]
fn the_door_reads_a_real_response() {
  let schema = from_response(HAND_WRITTEN);

  let (query, _) = schema.type_by_name(b"Query").expect("Query is defined");
  assert_eq!(schema.root(RootOperation::Query), Some(query));
  assert_eq!(schema.root(RootOperation::Mutation), None);

  let greeting = schema.sym(b"greeting").expect("`greeting` is interned");
  let field = schema.field(query, greeting).expect("Query.greeting");
  assert!(field.ty().is_non_null());

  // The three default kinds, read out of three real spellings.
  let args = field.args();
  let kind = |name: &[u8]| {
    schema
      .input(args, schema.sym(name).expect("interned"))
      .expect("an argument")
      .default_kind()
  };
  assert_eq!(kind(b"to"), DefaultKind::NonNull);
  assert_eq!(kind(b"loudly"), DefaultKind::Null);
  assert_eq!(kind(b"repeats"), DefaultKind::Absent);

  // The deprecated enum value and field are present: introspection reported them, so they are in
  // the schema. What the door cannot know is whether the *query* asked for them.
  let scope = schema.sym(b"scope").expect("`scope` is interned");
  assert!(schema.field(query, scope).is_some());
  let (cache_scope, _) = schema.type_by_name(b"CacheScope").expect("CacheScope");
  assert_eq!(schema.enum_values_of(cache_scope).len(), 2);

  // `@skip` came back injected, not as the response declared it; `@cacheControl` came from the
  // response and is repeatable.
  let skip = schema.directive_by_name(b"skip").expect("@skip");
  assert!(skip.is_built_in());
  let cache = schema
    .directive_by_name(b"cacheControl")
    .expect("@cacheControl");
  assert!(!cache.is_built_in());
  assert!(cache.is_repeatable());

  // `__Schema` came back injected too — with its real six fields, not the response's one.
  let (meta, def) = schema.type_by_name(b"__Schema").expect("__Schema");
  assert!(def.is_built_in());
  assert_eq!(
    schema.fields_of(meta).len(),
    7,
    "six fields plus `__typename`"
  );
}

/// All three envelopes name the same schema.
#[test]
fn every_envelope_is_read() {
  const BARE: &str = r#"{"queryType":{"name":"Query"},"directives":[],"types":[
    {"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
    ]}
  ]}"#;
  let unwrapped = std::format!(r#"{{"__schema":{BARE}}}"#);
  let full = std::format!(r#"{{"data":{{"__schema":{BARE}}}}}"#);

  let facts_of = |json: &str| facts(&from_response(json));
  assert_eq!(facts_of(BARE), facts_of(&unwrapped));
  assert_eq!(facts_of(BARE), facts_of(&full));
}

// ---------------------------------------------------------------------------------------------
// what `to_sdl` emits, and what it leaves to the build
// ---------------------------------------------------------------------------------------------

#[test]
fn the_sdl_omits_what_every_build_injects() {
  let from_sdl = built(FIXTURE);
  let sdl = introspection::to_sdl(&server::introspect(&from_sdl)).expect("SDL");

  for injected in ["__Schema", "__Type", "__TypeKind", "__DirectiveLocation"] {
    assert!(
      !sdl.contains(injected),
      "the door re-declared `{injected}`, which every build injects"
    );
  }
  for scalar in [
    "scalar Int",
    "scalar String",
    "scalar Boolean",
    "scalar ID",
    "scalar Float",
  ] {
    assert!(!sdl.contains(scalar), "the door re-declared `{scalar}`");
  }
  for directive in [
    "@skip",
    "@include",
    "@deprecated",
    "@specifiedBy",
    "@oneOf on",
  ] {
    assert!(
      !sdl.contains(&std::format!("directive {directive}")),
      "the door re-declared `directive {directive}`"
    );
  }

  // And it does emit what the response actually adds.
  assert!(sdl.contains("scalar DateTime"));
  assert!(sdl.contains("union SearchResult = User | Post"));
  assert!(sdl.contains("input AuthorFilter @oneOf"));
  assert!(sdl.contains("directive @auth("));
  // Bitmask order, not the document's. A response reports `locations` in whatever order the
  // server holds them and `Schema` holds a `u32` mask, so the spelling is not preserved and
  // nothing observable depends on it — `two_doors_agree` compares the mask.
  assert!(sdl.contains(" repeatable on QUERY | OBJECT | FIELD_DEFINITION"));
  assert!(sdl.contains("interface Timestamped implements Node"));
  assert!(sdl.contains("type User implements Node & Timestamped"));
  assert!(sdl.contains("nicknames: [[String!]!]!"));
  assert!(sdl.contains(
    "schema {\n  query: RootQuery\n  mutation: RootMutation\n  subscription: RootSubscription\n}"
  ));

  // The SDL it emits is the SDL it builds from, so it builds.
  assert_eq!(
    facts(&built(&sdl)),
    facts(&from_response(&server::introspect(&from_sdl)))
  );
}

// ---------------------------------------------------------------------------------------------
// the refusal floor
// ---------------------------------------------------------------------------------------------

fn response(types: &str) -> String {
  std::format!(
    r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[],"types":[{types}]}}}}"#
  )
}

fn with_directives(directives: &str) -> String {
  std::format!(
    r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[{directives}],"types":[{}]}}}}"#,
    OK_QUERY
  )
}

/// A minimal query root, so a fixture's defect is the only thing wrong with it.
const OK_QUERY: &str = r#"{"kind":"OBJECT","name":"Query","fields":[
  {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
]}"#;

/// Kinds with no fixture, each excused in writing.
const UNFIREABLE: &[(ResponseErrorKind, &str)] = &[(
  ResponseErrorKind::UnparsableSdl,
  "every byte the renderer emits is either its own punctuation or a fragment it checked first — \
   names against the grammar, kinds and locations against closed sets, default values against the \
   parser itself — so no response can reach the SDL parser with anything unexamined in it. The \
   variant exists because \"unreachable\" is a claim, and a claim that costs a `Result` arm is \
   cheaper than one that costs a panic in a server.",
)];

/// Every response-shape kind, with a response that makes it fire and the owner it names.
///
/// Reading a response is fail-fast — one bug in a machine-generated blob has one cause, and
/// listing its twenty consequences helps nobody — so what a fixture pins is the *single* refusal
/// it produces, kind and owner both. Pinning the owner is what keeps a fixture from quietly
/// starting to fail somewhere else for some other reason.
const FIXTURES: &[(ResponseErrorKind, &str, Option<&str>)] = &[
  (ResponseErrorKind::MalformedJson, "{\"__schema\": ", None),
  (
    ResponseErrorKind::MalformedResponse,
    r#"{"__schema":{"queryType":{"name":"Query"},"directives":[]}}"#,
    None,
  ),
  (
    ResponseErrorKind::MissingSchema,
    r#"{"data":{"nope":1}}"#,
    None,
  ),
  (ResponseErrorKind::UnnamedType, r#"__unnamed__"#, None),
  (ResponseErrorKind::InvalidName, r#"__invalid_name__"#, None),
  (
    ResponseErrorKind::UnknownTypeKind,
    r#"__unknown_kind__"#,
    None,
  ),
  (
    ResponseErrorKind::NotANamedType,
    r#"__wrapper_in_types__"#,
    None,
  ),
  (
    ResponseErrorKind::MissingItemType,
    r#"__missing_of_type__"#,
    Some("Query"),
  ),
  (
    ResponseErrorKind::DoubleNonNull,
    r#"__double_non_null__"#,
    Some("Query"),
  ),
  (
    ResponseErrorKind::InvalidEnumValue,
    r#"__bad_enum_value__"#,
    Some("Verdict"),
  ),
  (
    ResponseErrorKind::UnknownDirectiveLocation,
    r#"__bad_location__"#,
    Some("weird"),
  ),
  (
    ResponseErrorKind::MalformedDefaultValue,
    r#"__default_breakout__"#,
    Some("Query.ok.arg"),
  ),
];

/// Expands a fixture's placeholder into the response it stands for.
///
/// The indirection keeps [`FIXTURES`] readable: a JSON blob per row would bury the one thing each
/// row is about.
fn fixture_response(marker: &str) -> String {
  match marker {
    "__unnamed__" => response(r#"{"kind":"OBJECT","name":null,"fields":[]}"#),
    "__invalid_name__" => response(r#"{"kind":"OBJECT","name":"Not A Name","fields":[]}"#),
    "__unknown_kind__" => response(r#"{"kind":"WIDGET","name":"Widget"}"#),
    "__wrapper_in_types__" => response(r#"{"kind":"LIST","name":"Listy"}"#),
    "__missing_of_type__" => response(
      r#"{"kind":"OBJECT","name":"Query","fields":[
        {"name":"ok","args":[],"type":{"kind":"NON_NULL","name":null,"ofType":null}}
      ]}"#,
    ),
    "__double_non_null__" => response(
      r#"{"kind":"OBJECT","name":"Query","fields":[
        {"name":"ok","args":[],"type":{"kind":"NON_NULL","ofType":
          {"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Int"}}}}
      ]}"#,
    ),
    "__bad_enum_value__" => response(&std::format!(
      r#"{OK_QUERY},{{"kind":"ENUM","name":"Verdict","enumValues":[{{"name":"true"}}]}}"#
    )),
    "__bad_location__" => with_directives(r#"{"name":"weird","locations":["NOWHERE"],"args":[]}"#),
    "__default_breakout__" => response(
      r#"{"kind":"OBJECT","name":"Query","fields":[
        {"name":"ok","args":[
          {"name":"arg","type":{"kind":"SCALAR","name":"Int"},
           "defaultValue":"1} type Evil { x: Int } input Z { q: Int "}
        ],"type":{"kind":"SCALAR","name":"Int"}}
      ]}"#,
    ),
    literal => literal.to_owned(),
  }
}

#[test]
fn refusal_floor() {
  let mut missing = Vec::new();
  for kind in ResponseErrorKind::ALL {
    let excused = UNFIREABLE.iter().any(|(excused, _)| excused == kind);
    let has_fixture = FIXTURES.iter().any(|(fixture, ..)| fixture == kind);
    if !has_fixture && !excused {
      missing.push(kind);
    }
    assert!(
      !(has_fixture && excused),
      "{kind:?} is both excused and fixtured; delete the excuse"
    );
  }
  assert!(
    missing.is_empty(),
    "these refusal kinds have no response that makes them fire: {missing:?} — add one to \
     FIXTURES, or record a written reason in UNFIREABLE"
  );

  // The census must not pass vacuously.
  assert!(
    ResponseErrorKind::ALL.len() >= 12,
    "read only {} kinds; the enumeration is wrong, not the fixtures",
    ResponseErrorKind::ALL.len()
  );

  for (kind, marker, owner) in FIXTURES {
    let json = fixture_response(marker);
    let error = refused(&json);
    let IntrospectionError::Response(response) = &error else {
      panic!(
        "fixture for {kind:?} was refused by draft §3, not by the shape: {error}\n---\n{json}"
      );
    };
    assert_eq!(
      response.kind(),
      *kind,
      "fixture for {kind:?} produced {:?}\n---\n{json}",
      response.kind()
    );
    assert_eq!(
      response.owner(),
      *owner,
      "fixture for {kind:?} names owner {:?}\n---\n{json}",
      response.owner()
    );
    assert!(
      !response.subject().is_empty(),
      "fixture for {kind:?} refused without naming what it refused"
    );
  }
}

/// A default value cannot close the definition it sits in and open another.
///
/// The fixture above pins the refusal. This pins the consequence: without the check, the response
/// would have added a type named `Evil` to a schema nobody described. It also pins the other
/// direction — a brace inside a string is a brace inside a string, not a breakout.
#[test]
fn a_default_value_cannot_rewrite_the_schema() {
  let hostile = fixture_response("__default_breakout__");
  match Schema::from_introspection(&hostile) {
    Err(IntrospectionError::Response(error)) => {
      assert_eq!(error.kind(), ResponseErrorKind::MalformedDefaultValue);
    }
    Err(other) => panic!("refused for the wrong reason: {other}"),
    Ok(schema) => panic!(
      "the response injected a definition: `Evil` is {:?}",
      schema.type_by_name(b"Evil").map(|(_, def)| def.kind())
    ),
  }

  let benign = response(
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[
        {"name":"arg","type":{"kind":"SCALAR","name":"String"},"defaultValue":"\"} not a breakout {\""}
      ],"type":{"kind":"SCALAR","name":"Int"}}
    ]}"#,
  );
  let schema = from_response(&benign);
  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  let ok = schema
    .field(query, schema.sym(b"ok").expect("interned"))
    .expect("Query.ok");
  let arg = schema
    .input(ok.args(), schema.sym(b"arg").expect("interned"))
    .expect("Query.ok.arg");
  assert_eq!(arg.default_kind(), DefaultKind::NonNull);
  assert!(schema.type_by_name(b"not").is_none());
}

// ---------------------------------------------------------------------------------------------
// draft §3, through the introspection door
// ---------------------------------------------------------------------------------------------

/// Responses whose *shape* is fine and whose *schema* is not, with the exact set of draft §3 kinds
/// the build reports.
///
/// This is the test the §3 decision rests on. Each of these would be `Ok` from a door that trusted
/// the server, and each produces the same `SchemaErrorKind` the equivalent SDL produces from
/// `Schema::build` — because it *is* `Schema::build` that produced it.
#[allow(clippy::type_complexity)]
const SCHEMA_REFUSALS: &[(&str, &str, &[SchemaErrorKind])] = &[
  (
    "a field naming a type the response does not define",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[],"type":{"kind":"OBJECT","name":"Missing"}}
    ]}"#,
    &[SchemaErrorKind::UndefinedType],
  ),
  (
    "an object with no fields",
    r#"{"kind":"OBJECT","name":"Query","fields":[]}"#,
    &[SchemaErrorKind::EmptyFieldsDefinition],
  ),
  (
    "a union with no members",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[],"type":{"kind":"UNION","name":"Empty"}}
    ]},
    {"kind":"UNION","name":"Empty","possibleTypes":[]}"#,
    &[SchemaErrorKind::EmptyUnionMembers],
  ),
  (
    "a union whose member is an interface",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[],"type":{"kind":"UNION","name":"U"}}
    ]},
    {"kind":"UNION","name":"U","possibleTypes":[{"kind":"INTERFACE","name":"I"}]},
    {"kind":"INTERFACE","name":"I","interfaces":[],"fields":[
      {"name":"id","args":[],"type":{"kind":"SCALAR","name":"ID"}}
    ]}"#,
    &[SchemaErrorKind::UnionMemberNotObject],
  ),
  (
    "an implementor missing one of its interface's fields",
    r#"{"kind":"OBJECT","name":"Query","interfaces":[{"kind":"INTERFACE","name":"Node"}],"fields":[
      {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
    ]},
    {"kind":"INTERFACE","name":"Node","interfaces":[],"fields":[
      {"name":"id","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"ID"}}}
    ]}"#,
    &[SchemaErrorKind::MissingInterfaceField],
  ),
  (
    "a @oneOf input object with a non-null field",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[
        {"name":"by","type":{"kind":"INPUT_OBJECT","name":"By"},"defaultValue":null}
      ],"type":{"kind":"SCALAR","name":"Int"}}
    ]},
    {"kind":"INPUT_OBJECT","name":"By","isOneOf":true,"inputFields":[
      {"name":"id","type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"ID"}},"defaultValue":null}
    ]}"#,
    &[SchemaErrorKind::OneOfFieldNotNullable],
  ),
  (
    "a field name reserved for introspection",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"__secret","args":[],"type":{"kind":"SCALAR","name":"Int"}}
    ]}"#,
    &[SchemaErrorKind::ReservedFieldName],
  ),
  (
    "a type name reserved for introspection",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[],"type":{"kind":"OBJECT","name":"__Secret"}}
    ]},
    {"kind":"OBJECT","name":"__Secret","fields":[
      {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
    ]}"#,
    &[SchemaErrorKind::ReservedTypeName],
  ),
];

#[test]
fn draft_3_runs_in_the_introspection_door_too() {
  for (what, types, expected) in SCHEMA_REFUSALS {
    let json = response(types);
    let error = refused(&json);
    let Some(errors) = error.schema_errors() else {
      panic!("{what}: refused by the shape, not by draft §3: {error}\n---\n{json}");
    };
    let mut want = expected.to_vec();
    want.sort_unstable();
    want.dedup();
    assert_eq!(
      errors.kinds(),
      want,
      "{what} produced {:?}\n---\n{json}",
      errors.kinds()
    );
  }
}

/// A root operation naming a type the response does not define.
///
/// Separate from the table above because the defect is in the `__Schema` header rather than in
/// `types`, so it needs a response the helper cannot build.
#[test]
fn an_undefined_root_is_refused_by_draft_3() {
  let json = std::format!(
    r#"{{"__schema":{{"queryType":{{"name":"Nope"}},"directives":[],"types":[{OK_QUERY}]}}}}"#
  );
  let errors = refused(&json)
    .schema_errors()
    .expect("draft §3 refused it")
    .kinds();
  assert_eq!(errors, vec![SchemaErrorKind::UndefinedRootOperationType]);
}

/// The refusals are values that own everything they say.
///
/// `SchemaErrors` already promises this for the SDL door; the response arm has to promise it too,
/// or an error that crossed a thread would be an error that borrowed a request buffer.
#[test]
fn every_refusal_is_owned_and_renders() {
  fn owned<T: Send + Sync + Clone + std::fmt::Display + std::error::Error + 'static>(
    value: T,
  ) -> T {
    value
  }

  for (kind, marker, _) in FIXTURES {
    let error = owned(refused(&fixture_response(marker)));
    let rendered = std::string::ToString::to_string(&error);
    assert!(
      !rendered.is_empty(),
      "the refusal for {kind:?} renders as nothing"
    );
    assert_eq!(error, error.clone());
    assert_eq!(error.response_kind(), Some(*kind));
    assert!(error.schema_errors().is_none());
  }

  let schema_error = owned(refused(&response(SCHEMA_REFUSALS[0].1)));
  assert!(schema_error.response_kind().is_none());
  assert!(schema_error.schema_errors().is_some());
  assert!(!std::string::ToString::to_string(&schema_error).is_empty());
}
