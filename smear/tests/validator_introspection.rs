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
//! directive locations and repeatability, and the three root operations — and on everything else
//! draft §4 returns: descriptions, deprecation, `specifiedByURL`, each default's canonical text,
//! and the order every list was declared in. Any divergence is a bug in one door or the other, and
//! **no self-consistent test on either side would find it** — the SDL door has no reason to
//! disagree with itself and neither has the introspection door.
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
//!   therefore keyed by *name*: [`Facts`] stores `BTreeMap`s and `Vec`s of names — sorted, or in
//!   declaration order where the order is the fact — never a raw
//!   [`Sym`](smear::validator::schema::Sym) or [`TypeId`](smear::validator::schema::TypeId).
//! - **Applied directives other than `@oneOf`, `@deprecated` and `@specifiedBy`.** Introspection
//!   has no field for them, so the response cannot carry them. Those three are the ones a `Schema`
//!   observes — `TypeFlags::ONE_OF`, a [`Deprecation`], a scalar's `specifiedByURL` — and
//!   `isOneOf`, `isDeprecated` / `deprecationReason` and `specifiedByURL` carry them.
//!   [`one_of_survives_the_round_trip`] pins the first rather than leaving it to the aggregate.
//! - **A default value as a real server reports it.** The generated response reports the
//!   schema's canonical text for each default, which is the literal the document wrote, printed;
//!   graphql-js reports its printing of the *coerced* value, which differs for a list default
//!   written as one item and for a float written `1.0`. Printing is a fixed point, so the round
//!   trip compares the text exactly, and [`the_door_reads_a_real_response`] reads a server's own
//!   spellings.
//! - **A spelled-out built-in the response cannot tell from the build's own.** A built-in scalar
//!   or a specified directive whose response entry says nothing the injected one would not — for a
//!   scalar, no `specifiedByURL` and the built-in's own description or none asked for; for a
//!   directive, its description (or none asked for), locations, argument and repeatability exactly
//!   as injected — is left to the build. A document that spelled it out then gets `is_built_in`
//!   false from the SDL door and true from this one, and the definition sits where the build
//!   injects it — a scalar after the document's types and the meta-schema, a directive after the
//!   document's own directives, both in the built-in SDL's order — rather than where the document
//!   wrote it. [`Facts`] compares the directive order, so that place shows; it keys types by name,
//!   so a scalar's does not. One the response reports otherwise is written out and agrees on
//!   everything, place included ([`a_built_in_scalar_the_response_describes_is_written_out`],
//!   [`a_specified_directive_the_response_reports_otherwise_is_written_out`]). The fixture
//!   redefines none; [`redefining_a_built_in_is_the_one_flag_introspection_loses`] and
//!   [`a_specified_directive_spelled_as_the_build_injects_it_loses_its_flag_and_its_place`] pin the
//!   losses instead of hiding them.
//! - **The meta types' descriptions, the order of their members, and members the specification's
//!   meta-schema lacks.** A document cannot define a `__` type, so both doors carry smear's
//!   meta-schema, and what a response says about the specification's own types beyond their shape
//!   — graphql-js 15's `__Type.specifiedByUrl`, the legacy `__Directive.onField`, a
//!   `__DirectiveLocation` value from a later specification — is that server's rendering, not the
//!   schema's content: it is ignored, and so are an introspection extension's own `__` types along
//!   with its fields. The generated response is smear's own, so the comparison cannot see any of
//!   it either way. A contradiction of the shape, or anything listed twice, is refused;
//!   [`a_meta_type_is_held_to_its_shape_and_not_to_its_rendering`] and
//!   [`a_meta_type_that_contradicts_its_shape_is_refused`] pin the two halves.

// `Schema::from_introspection`, `IntrospectionError` and `ResponseErrorKind` below all live behind
// `#[cfg(feature = "introspection")]` in `smear-schema/src/introspection/`, not merely
// behind `validator` — the module itself is separately gated. This file carried neither gate, so
// it was a hard `E0432`/`E0433` under any feature selection that excludes `introspection`,
// including plain `--features validator`. `cargo test -p smear --no-run` in CI now compiles the
// default-feature selection that always excludes it.
#![cfg(feature = "introspection")]
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
    DefaultKind, Deprecation, DirectiveDef, DirectiveMeta, FieldDef, FieldMeta, InputValueDef,
    InputValueMeta, IntrospectionError, PackedType, Range32, RootOperation, Schema,
    SchemaErrorKind, Sym, Text, TypeDef, TypeId, TypeKind, builtin,
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
/// directive, applied directives that introspection cannot carry, and a custom scalar. And what
/// draft §4 returns beyond the shape: a description everywhere one may stand — block strings,
/// escapes, an empty one — each of the three deprecations, a `@specifiedBy`, defaults whose
/// canonical text is not their spelling, and lists declared out of the order they are stored in.
const FIXTURE: &str = r#"
"""
Every construct a schema can hold, described, so the agreement covers what draft §4 returns
beyond the shape as well as the shape.
"""
schema {
  query: RootQuery
  mutation: RootMutation
  subscription: RootSubscription
}

"An instant on a timeline."
scalar DateTime @specifiedBy(url: "https://example.com/spec/datetime")

"Who may see a field, an object or an operation."
directive @auth(
  "The least role that may see it."
  role: Role! = ADMIN
  scopes: [String!] = null
  """
  A note for whoever reads the schema.
  """
  note: String @deprecated(reason: "say it in the description")
) repeatable on FIELD_DEFINITION | OBJECT | QUERY

directive @tag(name: String!) on FIELD_DEFINITION | ENUM_VALUE

"Anything with an id."
interface Node {
  "Unique across every type that implements `Node`."
  id: ID!
}

interface Timestamped implements Node {
  id: ID!
  createdAt: DateTime
}

"""
A person with an account.

  Indented past the common indentation, which a block string keeps.
"""
type User implements Timestamped & Node @auth(role: MEMBER) {
  id: ID!
  createdAt: DateTime
  "What they are called — in any script."
  name: String!
  nicknames: [[String!]!]!
  role: Role! @deprecated(reason: "use roles")
  """
  Their posts, newest first.
  A \""" inside a block string, and a "quote".
  """
  posts(
    first: Int = 10
    "Where the previous page ended."
    after: String @deprecated(reason: "use first")
    filter: PostFilter = {published:false,ranked:[[1,null],[]]}
    tags: [String!] = null
  ): [Post!]!
}

""
type Post implements Node {
  id: ID!
  title: String
  author: User!
}

"Whatever a search can find."
union SearchResult = User | Post

"The roles a user can hold."
enum Role {
  "May do anything."
  ADMIN
  MEMBER
  GUEST @tag(name: "legacy") @deprecated
  OWNER @deprecated(reason: null)
}

"How to narrow a list of posts."
input PostFilter {
  "Matched as a substring.\tA tab, a \"quote\" and a \\ are escapes in the source."
  titleContains: String @deprecated(reason: "use search")
  published: Boolean = true
  authored: AuthorFilter
  ranked: [[Int]!]
}

input AuthorFilter @oneOf {
  byId: ID
  byName: String
}

type RootQuery {
  node("The id to look up." id: ID!): Node
  search(term: String!, limit: Int = 25, order: String = """the "newest" first"""): [SearchResult!]!
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
/// Every list is in the order the document declared it, which is the order a server reports, and
/// every description, deprecation, `specifiedByURL` and default is the schema's own text for it.
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
      .directives_in_order()
      .map(|(def, meta)| directive(schema, def, meta))
      .collect();

    std::format!(
      r#"{{"data":{{"__schema":{{"description":{},"queryType":{},"mutationType":{},"subscriptionType":{},"types":[{}],"directives":[{}]}}}}}}"#,
      json_text(schema.description()),
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
          .fields_in_order(id)
          // The meta-fields. `Schema` holds them, no server reports them, and the build puts them
          // back — so omitting them here is what makes the round-trip realistic AND correct.
          .filter(|(field, _)| !schema.name(field.name()).starts_with("__"))
          .map(|(field, meta)| self::field(schema, field, meta))
          .collect();
        std::format!("[{}]", rows.join(","))
      }
      _ => "null".into(),
    };

    let input_fields = match kind {
      TypeKind::InputObject => {
        let rows: Vec<String> = schema
          .input_fields_in_order(id)
          .map(|(value, meta)| input_value(schema, value, meta))
          .collect();
        std::format!("[{}]", rows.join(","))
      }
      _ => "null".into(),
    };

    let interfaces = match kind {
      TypeKind::Object | TypeKind::Interface => {
        let ids: Vec<TypeId> = schema.interfaces_in_order(id).collect();
        named_list(schema, &ids)
      }
      _ => "null".into(),
    };

    let enum_values = match kind {
      TypeKind::Enum => {
        let rows: Vec<String> = schema
          .enum_values_in_order(id)
          .map(|(value, meta)| {
            std::format!(
              r#"{{"name":{},"description":{},{}}}"#,
              json_string(schema.name(value)),
              json_text(schema.text(meta.description())),
              deprecation(schema, meta.deprecation()),
            )
          })
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
      r#"{{"kind":"{}","name":{},"description":{},"specifiedByURL":{},"fields":{},"inputFields":{},"interfaces":{},"enumValues":{},"possibleTypes":{},"isOneOf":{}}}"#,
      kind.as_str(),
      json_string(schema.name(def.name())),
      json_text(schema.text(schema.type_meta(id).description())),
      json_text(schema.specified_by_url(id)),
      fields,
      input_fields,
      interfaces,
      enum_values,
      possible_types,
      is_one_of,
    )
  }

  fn field(schema: &Schema, field: &FieldDef, meta: &FieldMeta) -> String {
    let args: Vec<String> = schema
      .inputs_in_order(field.args())
      .map(|(arg, meta)| input_value(schema, arg, meta))
      .collect();
    std::format!(
      r#"{{"name":{},"description":{},"args":[{}],"type":{},{}}}"#,
      json_string(schema.name(field.name())),
      json_text(schema.text(meta.description())),
      args.join(","),
      type_ref(schema, field.ty()),
      deprecation(schema, meta.deprecation()),
    )
  }

  fn input_value(schema: &Schema, value: &InputValueDef, meta: &InputValueMeta) -> String {
    std::format!(
      r#"{{"name":{},"description":{},"type":{},"defaultValue":{},{}}}"#,
      json_string(schema.name(value.name())),
      json_text(schema.text(meta.description())),
      type_ref(schema, value.ty()),
      json_text(schema.text(meta.default_value())),
      deprecation(schema, meta.deprecation()),
    )
  }

  /// `isDeprecated` and `deprecationReason`, the two members a server writes for one
  /// [`Deprecation`].
  fn deprecation(schema: &Schema, deprecation: Deprecation) -> String {
    std::format!(
      r#""isDeprecated":{},"deprecationReason":{}"#,
      deprecation.is_deprecated(),
      json_text(schema.text(deprecation.reason())),
    )
  }

  fn directive(schema: &Schema, directive: &DirectiveDef, meta: &DirectiveMeta) -> String {
    let locations: Vec<String> = directive
      .locations()
      .iter()
      .map(|location| json_string(location.as_str()))
      .collect();
    let args: Vec<String> = schema
      .inputs_in_order(directive.args())
      .map(|(arg, meta)| input_value(schema, arg, meta))
      .collect();
    std::format!(
      r#"{{"name":{},"description":{},"locations":[{}],"args":[{}],"isRepeatable":{}}}"#,
      json_string(schema.name(directive.name())),
      json_text(schema.text(meta.description())),
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

  /// A nullable JSON string: `null` for no text, and the literal otherwise.
  fn json_text(text: Option<&str>) -> String {
    text.map_or_else(|| "null".into(), json_string)
  }

  /// A JSON string literal, escaped as a serializer escapes one: `"` and `\`, the short forms for
  /// a line feed, a carriage return and a tab, and a `\u` escape for every other C0 control.
  ///
  /// Names and the fixed literals are ASCII with neither a quote nor a backslash in them;
  /// descriptions and defaults are text, and carry all of these.
  fn json_string(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
      match ch {
        '"' => out.push_str("\\\""),
        '\\' => out.push_str("\\\\"),
        '\n' => out.push_str("\\n"),
        '\r' => out.push_str("\\r"),
        '\t' => out.push_str("\\t"),
        // The backslash is pushed on its own so the `u` and its digits are assembled, never
        // written as one escape.
        control if u32::from(control) < 0x20 => {
          out.push('\\');
          out.push_str(&std::format!("u{:04X}", u32::from(control)));
        }
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
  description: Option<String>,
  types: BTreeMap<String, TypeFacts>,
  directives: BTreeMap<String, DirectiveFacts>,
  /// The directive definitions in the order they were defined.
  directive_order: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct TypeFacts {
  kind: &'static str,
  one_of: bool,
  built_in: bool,
  /// Whether the type occupies a bit in the possible-object bitsets.
  has_object_ordinal: bool,
  description: Option<String>,
  specified_by: Option<String>,
  fields: BTreeMap<String, FieldFacts>,
  input_fields: BTreeMap<String, InputFacts>,
  enum_values: BTreeMap<String, MemberFacts>,
  interfaces: Vec<String>,
  members: Vec<String>,
  /// The possible-object bitset, read back as the set of object type *names* it selects.
  possible: Option<Vec<String>>,
  /// The four lists draft §4 returns in the order the document declared them, as that order.
  field_order: Vec<String>,
  input_field_order: Vec<String>,
  enum_value_order: Vec<String>,
  interface_order: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct FieldFacts {
  ty: String,
  args: BTreeMap<String, InputFacts>,
  arg_order: Vec<String>,
  description: Option<String>,
  deprecated: Option<Option<String>>,
}

#[derive(Debug, PartialEq, Eq)]
struct InputFacts {
  ty: String,
  default: &'static str,
  required: bool,
  /// The default's canonical text: draft §4's `defaultValue`.
  default_text: Option<String>,
  description: Option<String>,
  deprecated: Option<Option<String>>,
}

/// What draft §4 returns about an enum value beyond its name.
#[derive(Debug, PartialEq, Eq)]
struct MemberFacts {
  description: Option<String>,
  deprecated: Option<Option<String>>,
}

#[derive(Debug, PartialEq, Eq)]
struct DirectiveFacts {
  locations: u32,
  repeatable: bool,
  built_in: bool,
  description: Option<String>,
  args: BTreeMap<String, InputFacts>,
  arg_order: Vec<String>,
}

/// Reads every observation a validation rule can make out of a schema, and everything else draft
/// §4 returns, keyed by name.
///
/// Three things happen here beyond collecting. Every field, input field, argument and directive is
/// *looked up* as well as iterated, so the sym-sorted binary-search invariant is exercised on both
/// schemas rather than assumed; every type reference is spelled out through [`PackedType`]'s
/// wrappers, so a mis-packed reference shows up as a different string rather than as a different
/// integer nobody compares; and every declaration order is checked to be a permutation of the group
/// it orders, so an order table that dropped a member cannot pass by dropping it from both schemas'
/// facts alike.
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
    assert_eq!(
      schema.field_meta_of(id).len(),
      schema.fields_of(id).len(),
      "`{name}`'s field metadata is not row for row with its fields"
    );
    for (field, meta) in schema.fields_of(id).iter().zip(schema.field_meta_of(id)) {
      let field_name = schema.name(field.name()).to_owned();
      let looked_up = schema
        .field(id, field.name())
        .unwrap_or_else(|| panic!("`{name}.{field_name}` is in the group but lookup misses it"));
      assert_eq!(looked_up.ty(), field.ty());
      let args = inputs(schema, field.args());
      let arg_order = declared_inputs(schema, field.args());
      permutation(
        &arg_order,
        args.keys(),
        &std::format!("{name}.{field_name}"),
      );
      fields.insert(
        field_name,
        FieldFacts {
          ty: spell(schema, field.ty()),
          args,
          arg_order,
          description: text(schema, meta.description()),
          deprecated: deprecated(schema, meta.deprecation()),
        },
      );
    }
    let field_order = names(
      schema,
      schema.fields_in_order(id).map(|(field, _)| field.name()),
    );
    permutation(&field_order, fields.keys(), &name);

    let mut input_fields = BTreeMap::new();
    assert_eq!(
      schema.input_field_meta_of(id).len(),
      schema.input_fields_of(id).len(),
      "`{name}`'s input field metadata is not row for row with its input fields"
    );
    for (value, meta) in schema
      .input_fields_of(id)
      .iter()
      .zip(schema.input_field_meta_of(id))
    {
      let value_name = schema.name(value.name()).to_owned();
      let looked_up = schema
        .input_field(id, value.name())
        .unwrap_or_else(|| panic!("`{name}.{value_name}` is in the group but lookup misses it"));
      assert_eq!(looked_up.ty(), value.ty());
      input_fields.insert(value_name, input_facts(schema, value, meta));
    }
    let input_field_order = names(
      schema,
      schema
        .input_fields_in_order(id)
        .map(|(value, _)| value.name()),
    );
    permutation(&input_field_order, input_fields.keys(), &name);

    let mut enum_values = BTreeMap::new();
    assert_eq!(
      schema.enum_value_meta_of(id).len(),
      schema.enum_values_of(id).len(),
      "`{name}`'s enum value metadata is not row for row with its values"
    );
    for (value, meta) in schema
      .enum_values_of(id)
      .iter()
      .zip(schema.enum_value_meta_of(id))
    {
      assert!(
        schema.has_enum_value(id, *value),
        "`{name}` lists a value `has_enum_value` cannot find"
      );
      enum_values.insert(
        schema.name(*value).to_owned(),
        MemberFacts {
          description: text(schema, meta.description()),
          deprecated: deprecated(schema, meta.deprecation()),
        },
      );
    }
    let enum_value_order = names(
      schema,
      schema.enum_values_in_order(id).map(|(value, _)| value),
    );
    permutation(&enum_value_order, enum_values.keys(), &name);

    let mut interfaces = sorted_names(schema, schema.interfaces_of(id));
    let interface_order: Vec<String> = schema
      .interfaces_in_order(id)
      .map(|interface| schema.name(schema.type_def(interface).name()).to_owned())
      .collect();
    permutation(&interface_order, &interfaces, &name);
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
        description: text(schema, schema.type_meta(id).description()),
        specified_by: schema.specified_by_url(id).map(str::to_owned),
        fields,
        input_fields,
        enum_values,
        interfaces,
        members,
        possible,
        field_order,
        input_field_order,
        enum_value_order,
        interface_order,
      },
    );
  }

  let mut directives = BTreeMap::new();
  assert_eq!(
    schema.directive_meta().len(),
    schema.directives().len(),
    "the directive metadata is not row for row with the directives"
  );
  for (def, meta) in schema.directives().iter().zip(schema.directive_meta()) {
    let name = schema.name(def.name()).to_owned();
    let looked_up = schema
      .directive_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("`@{name}` is in the table but lookup misses it"));
    assert_eq!(looked_up.locations().bits(), def.locations().bits());
    let args = inputs(schema, def.args());
    let arg_order = declared_inputs(schema, def.args());
    permutation(&arg_order, args.keys(), &std::format!("@{name}"));
    directives.insert(
      name,
      DirectiveFacts {
        locations: def.locations().bits(),
        repeatable: def.is_repeatable(),
        built_in: def.is_built_in(),
        description: text(schema, meta.description()),
        args,
        arg_order,
      },
    );
  }
  let directive_order = names(
    schema,
    schema.directives_in_order().map(|(def, _)| def.name()),
  );
  permutation(&directive_order, directives.keys(), "the directive table");

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
    description: schema.description().map(str::to_owned),
    types,
    directives,
    directive_order,
  }
}

fn inputs(schema: &Schema, args: Range32) -> BTreeMap<String, InputFacts> {
  assert_eq!(
    schema.input_meta(args).len(),
    schema.inputs(args).len(),
    "an argument group's metadata is not row for row with the group"
  );
  let mut out = BTreeMap::new();
  for (value, meta) in schema.inputs(args).iter().zip(schema.input_meta(args)) {
    let name = schema.name(value.name()).to_owned();
    let looked_up = schema
      .input(args, value.name())
      .unwrap_or_else(|| panic!("`{name}` is in the group but lookup misses it"));
    assert_eq!(looked_up.ty(), value.ty());
    out.insert(name, input_facts(schema, value, meta));
  }
  out
}

fn input_facts(schema: &Schema, value: &InputValueDef, meta: &InputValueMeta) -> InputFacts {
  InputFacts {
    ty: spell(schema, value.ty()),
    default: value.default_kind().as_str(),
    required: value.is_required(),
    default_text: text(schema, meta.default_value()),
    description: text(schema, meta.description()),
    deprecated: deprecated(schema, meta.deprecation()),
  }
}

/// An argument group's names, in the order the document declared them.
fn declared_inputs(schema: &Schema, args: Range32) -> Vec<String> {
  names(
    schema,
    schema.inputs_in_order(args).map(|(value, _)| value.name()),
  )
}

fn names(schema: &Schema, syms: impl Iterator<Item = Sym>) -> Vec<String> {
  syms.map(|sym| schema.name(sym).to_owned()).collect()
}

/// Checks that a declaration order lists every member of the group it orders, once each.
fn permutation<'a>(order: &[String], members: impl IntoIterator<Item = &'a String>, owner: &str) {
  let mut sorted: Vec<&String> = order.iter().collect();
  sorted.sort();
  let members: Vec<&String> = members.into_iter().collect();
  assert_eq!(
    sorted, members,
    "`{owner}`'s declaration order is not a permutation of its members"
  );
}

/// A string out of the schema's text arena, owned so two schemas' facts compare.
fn text(schema: &Schema, text: Text) -> Option<String> {
  schema.text(text).map(str::to_owned)
}

/// `isDeprecated` and `deprecationReason` as one value: `None` when the member is not deprecated,
/// and its reason — itself `None` for a `null` one — when it is.
fn deprecated(schema: &Schema, deprecation: Deprecation) -> Option<Option<String>> {
  deprecation
    .is_deprecated()
    .then(|| text(schema, deprecation.reason()))
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
/// possible-object sets, all three [`DefaultKind`]s, a wrapper chain deeper than two, all three
/// roots, and each kind of thing draft §4 returns beyond the shape.
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

  // What draft §4 returns beyond the shape.
  assert!(
    observed.description.is_some(),
    "the schema description is not reached"
  );
  assert_eq!(
    observed.types["DateTime"].specified_by.as_deref(),
    Some("https://example.com/spec/datetime"),
    "`@specifiedBy` is not reached"
  );
  assert_eq!(
    observed.types["Post"].description.as_deref(),
    Some(""),
    "an empty description is not reached, so absent and empty are not told apart"
  );
  assert_eq!(
    observed.types["User"].fields["posts"]
      .description
      .as_deref(),
    Some("Their posts, newest first.\nA \"\"\" inside a block string, and a \"quote\"."),
    "a block string with an escaped triple quote is not reached"
  );
  assert_eq!(
    observed.types["PostFilter"].input_fields["titleContains"]
      .description
      .as_deref(),
    Some("Matched as a substring.\tA tab, a \"quote\" and a \\ are escapes in the source."),
    "a description with escapes is not reached"
  );
  assert_eq!(
    observed.types["User"].fields["role"].deprecated,
    Some(Some("use roles".to_owned())),
    "a deprecation with a reason is not reached"
  );
  assert_eq!(
    observed.types["Role"].enum_values["GUEST"].deprecated,
    Some(Some("No longer supported".to_owned())),
    "a bare `@deprecated` is not reached"
  );
  assert_eq!(
    observed.types["Role"].enum_values["OWNER"].deprecated,
    Some(None),
    "`@deprecated(reason: null)` is not reached"
  );
  assert!(
    observed.types["User"].fields["posts"].args["after"]
      .deprecated
      .is_some(),
    "a deprecated argument is not reached"
  );
  assert!(
    observed.directives["auth"].args["note"]
      .deprecated
      .is_some(),
    "a deprecated directive argument is not reached"
  );
  assert_eq!(
    observed.types["User"].fields["posts"].args["filter"]
      .default_text
      .as_deref(),
    Some("{published: false, ranked: [[1, null], []]}"),
    "a default whose canonical text is not its spelling is not reached"
  );
  assert_eq!(
    observed.types["RootQuery"].fields["search"].args["order"]
      .default_text
      .as_deref(),
    Some("\"the \\\"newest\\\" first\""),
    "a block-string default is not reached"
  );

  // And the declaration orders compare something: in each of these the document's order is not
  // the order the group is stored in, so two schemas that kept only the stored order would differ.
  assert_eq!(
    observed.types["User"].interface_order,
    ["Timestamped", "Node"],
    "`User` no longer declares its interfaces out of type-id order"
  );
  let (user, _) = schema.type_by_name(b"User").expect("User");
  assert!(
    schema
      .field_order_of(user)
      .iter()
      .enumerate()
      .any(|(at, &position)| at as u32 != position),
    "`User` declares its fields in the order they are stored, so the field order compares nothing"
  );
}

/// `@oneOf` is the one applied directive a `Schema` keeps as a flag validation reads, so it is
/// pinned on its own rather than trusted to the aggregate.
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

/// A built-in scalar the response describes as the build's own would not is written out as a
/// definition of its own, so the two doors agree on all of it, the user-spelled flag included.
///
/// Both ways a response can say so: a description and a `specifiedByURL` of the document's own, and
/// a `null` description — a document that spelled `String` out and gave it none.
#[test]
fn a_built_in_scalar_the_response_describes_is_written_out() {
  for sdl in [
    "type Query { ok: String }\n\"Text, the way we use it.\"\nscalar String @specifiedBy(url: \"https://example.com/string\")",
    "type Query { ok: String }\nscalar String",
  ] {
    let from_sdl = built(sdl);
    let from_json = from_response(&server::introspect(&from_sdl));
    let (_, string) = from_json.type_by_name(b"String").expect("String");
    assert!(
      !string.is_built_in(),
      "the door left the response's `String` to the build\n---\n{sdl}"
    );
    assert_eq!(facts(&from_sdl), facts(&from_json), "{sdl}");
  }
}

/// The one flag introspection cannot carry, pinned as a loss rather than left to be discovered.
///
/// A document that spells out `scalar String` gets a `String` whose `is_built_in` is false. When
/// the response describes that `String` as the build's own — the built-in's description and no
/// `specifiedByURL`, or no description at all because the query did not ask — the door cannot tell
/// it from the built-in, leaves it to the build, and the round-trip diverges on the flag. Where the
/// description was not asked for it diverges on that too: the re-injected `String` describes
/// itself. This test is the record: if a future change makes the flag recoverable here, it fails
/// and the module header's table is wrong.
#[test]
fn redefining_a_built_in_is_the_one_flag_introspection_loses() {
  let at = builtin::BUILT_IN_SCALARS
    .iter()
    .position(|name| *name == "String")
    .expect("String is built in");
  let injected = builtin::BUILT_IN_SCALAR_DESCRIPTIONS[at];

  // The built-in's own text, spelled out by the document.
  let spelled = built(&std::format!(
    "type Query {{ ok: String }}\n\"\"\"{injected}\"\"\"\nscalar String"
  ));
  // No text at all, answered by a query that did not ask for descriptions.
  let unasked = built("type Query { ok: String }\nscalar String");
  let asked = server::introspect(&unasked);
  let not_asked = asked.replace(
    r#""name":"String","description":null,"#,
    r#""name":"String","#,
  );
  assert_ne!(not_asked, asked, "the fixture lost its edit");

  for (from_sdl, response) in [
    (&spelled, server::introspect(&spelled)),
    (&unasked, not_asked),
  ] {
    let from_json = from_response(&response);
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

    // The flag is the divergence, and the description the build injects where none was asked
    // for. Everything else about the two schemas still agrees.
    let mut sdl_facts = facts(from_sdl);
    let mut json_facts = facts(&from_json);
    assert_eq!(
      json_facts.types["String"].description.as_deref(),
      Some(injected)
    );
    for facts in [&mut sdl_facts, &mut json_facts] {
      let string = facts.types.get_mut("String").expect("String");
      string.built_in = true;
      string.description = Some(injected.to_owned());
    }
    assert_eq!(sdl_facts, json_facts);
  }
}

/// A specified directive the response reports other than as the build injects it is written out
/// as a definition of its own, so the two doors agree on all of it: the user-spelled flag, the
/// texts, and its place among the document's directives.
///
/// One spelling for each way an entry can differ: its description, a `null` one, its locations,
/// its argument's type, its argument's default, and its repeatability.
#[test]
fn a_specified_directive_the_response_reports_otherwise_is_written_out() {
  const REASON: &str = "\"Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).\"";
  const DEPRECATED: &str = "\"Marks an element of a GraphQL schema as no longer supported.\"";
  let spellings = [
    (
      "skip",
      String::from(
        "\"Skips, our way.\" directive @skip(\"Skipped when true.\" if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT",
      ),
    ),
    ("oneOf", String::from("directive @oneOf on INPUT_OBJECT")),
    (
      "deprecated",
      std::format!(
        "{DEPRECATED} directive @deprecated({REASON} reason: String = \"No longer supported\") on FIELD_DEFINITION | ENUM_VALUE"
      ),
    ),
    (
      "include",
      String::from(
        "\"Directs the executor to include this field or fragment only when the `if` argument is true.\" directive @include(\"Included when true.\" if: Boolean) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT",
      ),
    ),
    (
      "deprecated",
      std::format!(
        "{DEPRECATED} directive @deprecated({REASON} reason: String = \"Gone.\") on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE"
      ),
    ),
    (
      "specifiedBy",
      String::from(
        "\"Exposes a URL that specifies the behavior of this scalar.\" directive @specifiedBy(\"The URL that specifies the behavior of this scalar.\" url: String!) repeatable on SCALAR",
      ),
    ),
  ];

  for (name, spelled) in spellings {
    let sdl = std::format!(
      "type Query {{ ok: Int }}\ndirective @before on FIELD\n{spelled}\ndirective @after on FIELD"
    );
    let from_sdl = built(&sdl);
    let from_json = from_response(&server::introspect(&from_sdl));
    let directive = from_json
      .directive_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("@{name}"));
    assert!(
      !directive.is_built_in(),
      "the door left the response's `@{name}` to the build\n---\n{sdl}"
    );
    assert_eq!(facts(&from_sdl), facts(&from_json), "{sdl}");
  }
}

/// The specified-directive twin of the loss above.
///
/// A document that spells out a specified directive exactly as the build injects it gets a
/// definition of its own from the SDL door, and the door, which cannot tell it from the built-in,
/// leaves it to the build. The flag diverges, and so does its place: the build appends the
/// specified directives it injects after the document's own, where the document wrote this one
/// among them. GitLab's `@specifiedBy` is this case. Nothing else diverges.
#[test]
fn a_specified_directive_spelled_as_the_build_injects_it_loses_its_flag_and_its_place() {
  let sdl = "type Query { ok: Int }\n\
    \"Exposes a URL that specifies the behavior of this scalar.\"\n\
    directive @specifiedBy(\"The URL that specifies the behavior of this scalar.\" url: String!) on SCALAR\n\
    directive @after on FIELD";
  let from_sdl = built(sdl);
  let from_json = from_response(&server::introspect(&from_sdl));

  let sdl_facts = facts(&from_sdl);
  let mut json_facts = facts(&from_json);
  assert!(!sdl_facts.directives["specifiedBy"].built_in);
  assert!(
    json_facts.directives["specifiedBy"].built_in,
    "the door recovered the flag; the loss recorded in the module header is stale"
  );
  assert_eq!(
    sdl_facts.directive_order,
    [
      "specifiedBy",
      "after",
      "skip",
      "include",
      "deprecated",
      "oneOf"
    ]
  );
  assert_eq!(
    json_facts.directive_order,
    [
      "after",
      "skip",
      "include",
      "deprecated",
      "specifiedBy",
      "oneOf"
    ]
  );

  json_facts
    .directives
    .get_mut("specifiedBy")
    .expect("@specifiedBy")
    .built_in = false;
  json_facts.directive_order = sdl_facts.directive_order.clone();
  assert_eq!(sdl_facts, json_facts);
}

// ---------------------------------------------------------------------------------------------
// the door against a response nobody generated
// ---------------------------------------------------------------------------------------------

/// A response written by hand, in the shape a server returns.
///
/// The generated response above is exhaustive but shares an author with the door. This one shares
/// nothing: it carries real default-value spellings, a `data` envelope, descriptions and
/// deprecation where servers write them, and `isRepeatable` where the door reads it.
const HAND_WRITTEN: &str = r#"
{"data":{"__schema":{
  "queryType": {"name": "Query"},
  "mutationType": null,
  "subscriptionType": null,
  "directives": [
    {"name":"skip","locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[
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
      {"name":"types","args":[],"type":{"kind":"NON_NULL","ofType":
        {"kind":"LIST","ofType":{"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__Type"}}}}}
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

  // `@skip`, reported as the build injects it — no description asked for, the specified locations
  // and argument — came back injected; `@cacheControl` came from the response and is repeatable.
  let skip = schema.directive_by_name(b"skip").expect("@skip");
  assert!(skip.is_built_in());
  let cache = schema
    .directive_by_name(b"cacheControl")
    .expect("@cacheControl");
  assert!(!cache.is_built_in());
  assert!(cache.is_repeatable());

  // `__Schema` came back injected too — with its six fields, not the one the response listed.
  let (meta, def) = schema.type_by_name(b"__Schema").expect("__Schema");
  assert!(def.is_built_in());
  assert_eq!(
    schema.fields_of(meta).len(),
    7,
    "six fields plus `__typename`"
  );

  // What a server writes beyond the shape is kept: descriptions, deprecation, and each default
  // as the build printed what the server printed.
  assert_eq!(
    schema.text(schema.type_meta(query).description()),
    Some("The root.")
  );
  let field_meta = |name: &str| {
    schema
      .fields_in_order(query)
      .find(|(field, _)| schema.name(field.name()) == name)
      .map(|(_, meta)| *meta)
      .expect("a field of `Query`")
  };
  assert_eq!(
    schema.text(field_meta("greeting").description()),
    Some("Says hello.")
  );
  assert!(!field_meta("greeting").deprecation().is_deprecated());
  assert!(field_meta("scope").deprecation().is_deprecated());
  assert_eq!(
    schema.text(field_meta("scope").deprecation().reason()),
    Some("unused")
  );

  let declared: Vec<(&str, Option<&str>)> = schema
    .inputs_in_order(args)
    .map(|(value, meta)| (schema.name(value.name()), schema.text(meta.default_value())))
    .collect();
  assert_eq!(
    declared,
    [
      ("to", Some("\"world\"")),
      ("loudly", Some("null")),
      ("repeats", None)
    ],
    "the arguments in the order the server listed them, each with its default's text"
  );

  let (_, cache_meta) = schema
    .directives_in_order()
    .find(|(directive, _)| schema.name(directive.name()) == "cacheControl")
    .expect("@cacheControl");
  assert_eq!(schema.text(cache_meta.description()), Some("Cache hints."));
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
// the meta-schema: held to its shape, not to a server's rendering of it
// ---------------------------------------------------------------------------------------------

/// A response whose types are the query root and the meta types given.
fn with_meta_types(meta_types: &str) -> String {
  response(&std::format!("{OK_QUERY},{meta_types}"))
}

/// graphql-js's `__Directive`, in graphql-js's order: `isRepeatable` third, where smear's is last.
const GRAPHQL_JS_DIRECTIVE: &str = r#"{"kind":"OBJECT","name":"__Directive","interfaces":[],"fields":[
  {"name":"name","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}}},
  {"name":"description","args":[],"type":{"kind":"SCALAR","name":"String"}},
  {"name":"isRepeatable","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}}},
  {"name":"locations","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
    {"kind":"NON_NULL","ofType":{"kind":"ENUM","name":"__DirectiveLocation"}}}}},
  {"name":"args","args":[
    {"name":"includeDeprecated","type":{"kind":"SCALAR","name":"Boolean"},"defaultValue":"false"}
  ],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
    {"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__InputValue"}}}}}
]}"#;

/// The meta types are held to draft §4's shape and nothing more: what a server says about them
/// beyond it — the order graphql-js lists `__Directive`'s fields in, a description of its own or a
/// `null` one, members the specification's meta-schema lacks — is not kept, and what an older
/// server leaves out is not missed.
#[test]
fn a_meta_type_is_held_to_its_shape_and_not_to_its_rendering() {
  let own = built("type Query { ok: Int }");
  let field_names = |schema: &Schema, name: &[u8]| -> Vec<String> {
    let (id, _) = schema.type_by_name(name).expect("a meta type");
    schema
      .fields_in_order(id)
      .map(|(field, _)| schema.name(field.name()).to_owned())
      .collect()
  };
  let description = |schema: &Schema, name: &[u8]| -> Option<String> {
    let (id, _) = schema.type_by_name(name).expect("a meta type");
    schema
      .text(schema.type_meta(id).description())
      .map(str::to_owned)
  };

  // graphql-js lists `isRepeatable` third, and the schema keeps smear's order, `isRepeatable` last.
  let graphql_js = from_response(&with_meta_types(GRAPHQL_JS_DIRECTIVE));
  assert_eq!(
    field_names(&graphql_js, b"__Directive"),
    [
      "name",
      "description",
      "locations",
      "args",
      "isRepeatable",
      "__typename"
    ],
    "the order a server lists a meta type's fields in is its rendering, not the schema's"
  );

  // A `null` description and one of the server's own: both accepted, and neither kept.
  for described in [
    r#""description":null"#,
    r#""description":"The server's own words for __Type.""#,
  ] {
    let json = with_meta_types(&std::format!(
      r#"{{"kind":"OBJECT","name":"__Type",{described},"interfaces":[],"fields":[
        {{"name":"kind","args":[],"type":{{"kind":"NON_NULL","ofType":{{"kind":"ENUM","name":"__TypeKind"}}}}}}
      ]}}"#
    ));
    let schema = from_response(&json);
    assert_eq!(
      description(&schema, b"__Type"),
      description(&own, b"__Type"),
      "{described}"
    );
  }

  // An older server: no `specifiedByURL` or `isOneOf` on `__Type`, no `isRepeatable` on
  // `__Directive`, and `__Directive.args` taking no `includeDeprecated`.
  let older = from_response(&with_meta_types(
    r#"{"kind":"OBJECT","name":"__Type","fields":[
      {"name":"kind","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"ENUM","name":"__TypeKind"}}},
      {"name":"name","args":[],"type":{"kind":"SCALAR","name":"String"}},
      {"name":"fields","args":[
        {"name":"includeDeprecated","type":{"kind":"SCALAR","name":"Boolean"},"defaultValue":"false"}
      ],"type":{"kind":"LIST","ofType":{"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__Field"}}}}
    ]},
    {"kind":"OBJECT","name":"__Directive","fields":[
      {"name":"name","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}}},
      {"name":"locations","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
        {"kind":"NON_NULL","ofType":{"kind":"ENUM","name":"__DirectiveLocation"}}}}},
      {"name":"args","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
        {"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__InputValue"}}}}}
    ]}"#,
  ));
  assert_eq!(
    field_names(&older, b"__Directive"),
    field_names(&own, b"__Directive")
  );
  assert_eq!(field_names(&older, b"__Type"), field_names(&own, b"__Type"));

  // What a server adds ahead of the specification, or keeps from behind it, is ignored: graphql-js
  // 15's `specifiedByUrl`, an applied-directives extension's field and its two `__` types, the
  // legacy `onOperation`, `onFragment` and `onField`, and a kind and a location from a later
  // specification in the enums' listings.
  let additions = from_response(&with_meta_types(
    r#"{"kind":"OBJECT","name":"__Type","fields":[
      {"name":"kind","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"ENUM","name":"__TypeKind"}}},
      {"name":"specifiedByUrl","args":[],"type":{"kind":"SCALAR","name":"String"}},
      {"name":"appliedDirectives","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
        {"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__AppliedDirective"}}}}}
    ]},
    {"kind":"OBJECT","name":"__Directive","fields":[
      {"name":"name","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}}},
      {"name":"onOperation","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}}},
      {"name":"onFragment","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}}},
      {"name":"onField","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}}}
    ]},
    {"kind":"ENUM","name":"__TypeKind","enumValues":[{"name":"SCALAR"},{"name":"A_LATER_KIND"}]},
    {"kind":"ENUM","name":"__DirectiveLocation","enumValues":[
      {"name":"QUERY"},{"name":"FIELD"},{"name":"A_LATER_LOCATION"}
    ]},
    {"kind":"OBJECT","name":"__AppliedDirective","fields":[
      {"name":"name","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}}},
      {"name":"args","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
        {"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__DirectiveArgument"}}}}}
    ]},
    {"kind":"OBJECT","name":"__DirectiveArgument","fields":[
      {"name":"name","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}}},
      {"name":"value","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"String"}}}
    ]}"#,
  ));
  for extension in [&b"__AppliedDirective"[..], b"__DirectiveArgument"] {
    assert!(
      additions.type_by_name(extension).is_none(),
      "an introspection extension's own type was kept"
    );
  }
  assert_eq!(
    field_names(&additions, b"__Type"),
    field_names(&own, b"__Type")
  );
  assert_eq!(
    field_names(&additions, b"__Directive"),
    field_names(&own, b"__Directive")
  );
  for name in [&b"__TypeKind"[..], b"__DirectiveLocation"] {
    let (kept, _) = additions.type_by_name(name).expect("a meta enum");
    let (injected, _) = own.type_by_name(name).expect("a meta enum");
    assert_eq!(
      additions.enum_values_of(kept).len(),
      own.enum_values_of(injected).len(),
      "a value from a later specification became one of the schema's"
    );
  }
}

/// A meta type that contradicts draft §4's shape, or lists anything twice, is refused, never
/// quietly given smear's — each way, with the owner and the subject that say where and what.
#[test]
fn a_meta_type_that_contradicts_its_shape_is_refused() {
  let cases = [
    // Another kind.
    (
      r#"{"kind":"SCALAR","name":"__Type"}"#,
      None,
      "__Type: SCALAR rather than OBJECT",
    ),
    // A field of another type: `__Field.type` is `__Type!`.
    (
      r#"{"kind":"OBJECT","name":"__Field","fields":[
        {"name":"type","args":[],"type":{"kind":"OBJECT","name":"__Type"}}
      ]}"#,
      Some("__Field"),
      "type: __Type rather than __Type!",
    ),
    // An argument of another type.
    (
      r#"{"kind":"OBJECT","name":"__Type","fields":[
        {"name":"fields","args":[
          {"name":"includeDeprecated","type":{"kind":"SCALAR","name":"String"},"defaultValue":null}
        ],"type":{"kind":"LIST","ofType":{"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__Field"}}}}
      ]}"#,
      Some("__Type.fields"),
      "includeDeprecated: String rather than Boolean",
    ),
    // An interface, where the meta types implement nothing.
    (
      r#"{"kind":"OBJECT","name":"__Schema","interfaces":[{"kind":"INTERFACE","name":"Node"}],"fields":[]}"#,
      Some("__Schema"),
      "implements Node",
    ),
    // The meta type listed twice.
    (
      r#"{"kind":"OBJECT","name":"__Schema","fields":[]},{"kind":"OBJECT","name":"__Schema","fields":[]}"#,
      None,
      "__Schema listed twice",
    ),
    // One of the eight listed again, as an introspection extension's own `__` type would be: the
    // name is still one of the eight's, so it is refused as listed twice rather than dropped.
    (
      r#"{"kind":"OBJECT","name":"__Directive","fields":[]},
      {"kind":"OBJECT","name":"__Directive","fields":[
        {"name":"appliedDirectives","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"LIST","ofType":
          {"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__AppliedDirective"}}}}}
      ]}"#,
      None,
      "__Directive listed twice",
    ),
    // A field listed twice — one smear's meta-schema has, and one it would otherwise ignore.
    (
      r#"{"kind":"OBJECT","name":"__Type","fields":[
        {"name":"name","args":[],"type":{"kind":"SCALAR","name":"String"}},
        {"name":"name","args":[],"type":{"kind":"SCALAR","name":"String"}}
      ]}"#,
      Some("__Type"),
      "name listed twice",
    ),
    (
      r#"{"kind":"OBJECT","name":"__Directive","fields":[
        {"name":"onField","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}}},
        {"name":"onField","args":[],"type":{"kind":"NON_NULL","ofType":{"kind":"SCALAR","name":"Boolean"}}}
      ]}"#,
      Some("__Directive"),
      "onField listed twice",
    ),
    // An argument listed twice.
    (
      r#"{"kind":"OBJECT","name":"__Type","fields":[
        {"name":"fields","args":[
          {"name":"includeDeprecated","type":{"kind":"SCALAR","name":"Boolean"},"defaultValue":null},
          {"name":"includeDeprecated","type":{"kind":"SCALAR","name":"Boolean"},"defaultValue":null}
        ],"type":{"kind":"LIST","ofType":{"kind":"NON_NULL","ofType":{"kind":"OBJECT","name":"__Field"}}}}
      ]}"#,
      Some("__Type.fields"),
      "includeDeprecated listed twice",
    ),
    // An enum value listed twice.
    (
      r#"{"kind":"ENUM","name":"__TypeKind","enumValues":[{"name":"SCALAR"},{"name":"SCALAR"}]}"#,
      Some("__TypeKind"),
      "SCALAR listed twice",
    ),
  ];
  for (meta_type, owner, subject) in cases {
    let json = with_meta_types(meta_type);
    let IntrospectionError::Response(error) = refused(&json) else {
      panic!("refused by draft §3, not by the shape\n---\n{json}");
    };
    assert_eq!(error.kind(), ResponseErrorKind::InvalidMetaType, "{json}");
    assert_eq!(error.owner(), owner, "{json}");
    assert_eq!(error.subject(), subject, "{json}");
  }
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
  // Declaration order, not the closure's type-id order.
  assert!(sdl.contains("type User implements Timestamped & Node"));
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
   parser itself — or prose it wrote as a quoted string with every quote, backslash and control \
   escaped, so no response can reach the SDL parser with anything unexamined in it. The variant \
   exists because \"unreachable\" is a claim, and a claim that costs a `Result` arm is cheaper \
   than one that costs a panic in a server.",
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
  (ResponseErrorKind::InvalidMetaType, r#"__meta_kind__"#, None),
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
    "__meta_kind__" => response(&std::format!(
      r#"{OK_QUERY},{{"kind":"SCALAR","name":"__Type"}}"#
    )),
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

/// A default value cannot carry a directive into the definition it sits in.
///
/// The grammar admits const directives after a default, so `1 @deprecated(reason: "x")` is one
/// input value with a default, and copied through it would deprecate an input value the response
/// reports as not deprecated. It is refused wherever a default is read: on a field's argument, on
/// an input-object field, and on a specified directive's argument — whose default is compared with
/// the injected one first, and does not pass for it, so the directive is written out and refused
/// rather than left to the build.
#[test]
fn a_default_value_cannot_carry_a_directive() {
  let cases = [
    (
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[
            {"name":"arg","type":{"kind":"SCALAR","name":"Int"},
             "defaultValue":"1 @deprecated(reason: \"x\")","isDeprecated":false}
          ],"type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
      "Query.ok.arg",
    ),
    (
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[
            {"name":"by","type":{"kind":"INPUT_OBJECT","name":"By"},"defaultValue":null}
          ],"type":{"kind":"SCALAR","name":"Int"}}
        ]},
        {"kind":"INPUT_OBJECT","name":"By","inputFields":[
          {"name":"id","type":{"kind":"SCALAR","name":"Int"},
           "defaultValue":"1 @deprecated(reason: \"x\")","isDeprecated":false}
        ]}"#,
      ),
      "By.id",
    ),
    // `@deprecated` as the build injects it but for the suffix on its default.
    (
      with_directives(
        r#"{"name":"deprecated","locations":[
          "FIELD_DEFINITION","ARGUMENT_DEFINITION","INPUT_FIELD_DEFINITION","ENUM_VALUE"
        ],"args":[
          {"name":"reason","type":{"kind":"SCALAR","name":"String"},
           "defaultValue":"\"No longer supported\" @deprecated"}
        ]}"#,
      ),
      "deprecated.reason",
    ),
  ];
  for (json, owner) in cases {
    match Schema::from_introspection(&json) {
      Err(IntrospectionError::Response(error)) => {
        assert_eq!(
          error.kind(),
          ResponseErrorKind::MalformedDefaultValue,
          "{json}"
        );
        assert_eq!(error.owner(), Some(owner), "{json}");
      }
      Err(other) => panic!("refused for the wrong reason: {other}\n---\n{json}"),
      Ok(_) => panic!("a default with a directive after it was accepted\n---\n{json}"),
    }
  }

  // Whitespace after the value is still just the value, on an argument and in the comparison.
  let plain = response(
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[
        {"name":"a","type":{"kind":"SCALAR","name":"Int"},"defaultValue":"1"},
        {"name":"b","type":{"kind":"SCALAR","name":"Int"},"defaultValue":"1 \n"}
      ],"type":{"kind":"SCALAR","name":"Int"}}
    ]}"#,
  );
  let schema = from_response(&plain);
  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  let ok = schema
    .field(query, schema.sym(b"ok").expect("interned"))
    .expect("Query.ok");
  for name in [&b"a"[..], b"b"] {
    let arg = schema
      .input(ok.args(), schema.sym(name).expect("interned"))
      .expect("an argument of Query.ok");
    assert_eq!(arg.default_kind(), DefaultKind::NonNull);
  }

  let injected = from_response(&with_directives(
    r#"{"name":"deprecated","locations":[
      "FIELD_DEFINITION","ARGUMENT_DEFINITION","INPUT_FIELD_DEFINITION","ENUM_VALUE"
    ],"args":[
      {"name":"reason","type":{"kind":"SCALAR","name":"String"},
       "defaultValue":"\"No longer supported\" "}
    ]}"#,
  ));
  assert!(
    injected
      .directive_by_name(b"deprecated")
      .expect("@deprecated")
      .is_built_in(),
    "the injected default followed by whitespace was not taken for the injected one"
  );
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
    "a field typed by a `__` type, which the door drops as an introspection extension's own",
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[],"type":{"kind":"OBJECT","name":"__Secret"}}
    ]},
    {"kind":"OBJECT","name":"__Secret","fields":[
      {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
    ]}"#,
    &[SchemaErrorKind::UndefinedType],
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
