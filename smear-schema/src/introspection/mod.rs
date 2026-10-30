//! The second construction door: a [`Schema`] built from a draft §4 introspection response.
//!
//! ```
//! use smear_schema::Schema;
//!
//! let response = r#"{"data":{"__schema":{
//!   "queryType": {"name": "Query"},
//!   "mutationType": null,
//!   "subscriptionType": null,
//!   "directives": [],
//!   "types": [
//!     {"kind":"OBJECT","name":"Query","interfaces":[],"fields":[
//!       {"name":"hero","args":[],"type":{"kind":"INTERFACE","name":"Character","ofType":null}}
//!     ]},
//!     {"kind":"INTERFACE","name":"Character","interfaces":[],"fields":[
//!       {"name":"name","args":[],"type":
//!         {"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}
//!     ]}
//!   ]
//! }}}"#;
//!
//! let schema = Schema::from_introspection(response).expect("a schema");
//! let (character, _) = schema.type_by_name(b"Character").expect("Character is defined");
//! let name = schema.sym(b"name").expect("`name` is interned");
//! assert!(schema.field(character, name).expect("Character.name").ty().is_non_null());
//! ```
//!
//! # This is not what puts introspection in a schema
//!
//! The `__`-prefixed meta-schema is injected by **every** build, feature or no feature, because an
//! introspection *query* is an executable document and draft 5.3.1 asks whether each of its fields
//! exists like it does for any other. [`builtin`](super::builtin) is where that happens and why.
//! This module is about where a schema comes *from*, which is a different question, and it is
//! behind a cargo feature because it is the one part of the validator that costs a dependency.
//!
//! # One builder, two front doors
//!
//! [`Schema::from_introspection`] renders the response as SDL and hands it to
//! [`Schema::build`]. There is no second builder. Everything a [`Schema`] promises — interned
//! symbols, sym-sorted field groups, packed type references, possible-object bitsets, the 2-bit
//! [`DefaultKind`](super::DefaultKind), the directive-location bitmask — is produced by the code
//! that produces it for SDL, because it *is* that code.
//!
//! **Draft §3 "Type Validation" therefore runs, and that is the point rather than an accident.**
//! The argument for skipping it is real: a response comes from a server that has presumably
//! already validated its own schema, so re-checking it is work nobody asked for. It is refused
//! anyway, for a reason that outweighs the work:
//!
//! - **A `Schema` that exists is a `Schema` you can trust.** Every rule in `smear-compiler`'s
//!   `validate_executable` — named rather than linked, because it is in a crate above this one —
//!   reads the tables assuming draft
//!   §3 holds — that a union's members are objects, that an implementor has its interface's fields,
//!   that no input-object cycle is unsatisfiable. A door that skipped the check would produce a
//!   value indistinguishable from a checked one and violate an invariant the rules do not re-test.
//! - **"Presumably already validated" is the premise, not the fact.** The response arrived over a
//!   network, from a server of unknown version, possibly through a gateway that composed it. The
//!   composition step is precisely where a schema stops being valid.
//! - **The cost is a startup cost, once.** The whole representation exists to move work off the
//!   request path; this is work that is already off it.
//!
//! So the two doors have one guarantee between them, and a response describing a schema with an
//! undefined type comes back as
//! [`SchemaErrorKind::UndefinedType`](super::SchemaErrorKind::UndefinedType) — the same kind, from
//! the same rule, as the SDL saying the same thing.
//!
//! # What introspection cannot express
//!
//! An introspection result is not a lossless encoding of a schema, and the door does not pretend
//! otherwise. What is lost, and whether a [`Schema`] can tell:
//!
//! | Lost | Visible in a `Schema`? |
//! |---|---|
//! | **Applied directives**, other than `@oneOf`, `@deprecated` and `@specifiedBy` — introspection has no field for them, so `type User @key(fields: "id")` simply is not in the response | Not today: the applied directives `Schema` retains are those three — `@oneOf` as [`TypeFlags::ONE_OF`](super::TypeFlags::ONE_OF), carried by `isOneOf`; `@deprecated` as a [`Deprecation`](super::Deprecation), carried by `isDeprecated` and `deprecationReason`; `@specifiedBy` as a scalar's `specifiedByURL` — and the door renders each back as the directive. It becomes visible the moment a rule reads directive *usages* |
//! | **Deprecated fields, arguments and enum values**, when the introspection query omitted `includeDeprecated: true` | **Yes, and silently.** They are simply absent from the response, and nothing in it says so. This is the one loss the door cannot detect, and the reason an introspection query feeding this door must ask for them |
//! | **`extend` structure** — a response reports the merged schema | No: a `Schema` holds the merge, so there is nothing to lose |
//! | **A default value's exact spelling** — the server prints its own coerced form | **Yes**, in [`InputValueMeta::default_value`](super::InputValueMeta::default_value): the build keeps the canonical text of the literal it read, which from this door is the server's printing of its *coerced* value — `[1]` for an SDL that wrote `= 1` on a list, `1` for a float written `1.0`. Presence and null-ness, which [`DefaultKind`](super::DefaultKind) keeps, survive either way |
//! | **Interfaces implemented by interfaces**, from a server predating the 2021 specification | Under-constrains rather than misreports: nothing is claimed that is not in the response |
//! | **That a document spelled out a built-in** — `scalar String` and `directive @skip(…)` are reported by every server whether the document wrote them or not | **Yes, when the response cannot tell.** The door leaves a built-in scalar or a specified directive to the build only when the response says nothing the injected one would not: for a scalar, no `specifiedByURL` and a `description` that is the built-in's own text or absent because the query did not ask; for a directive, the built-in's description or none asked for, and its locations, its argument — name, description, type and default — and its repeatability exactly. Such a definition is then the build's in two ways a document that spelled it out is not: [`TypeDef::is_built_in`](super::TypeDef::is_built_in) or [`DirectiveDef::is_built_in`](super::DirectiveDef::is_built_in) is true where the SDL door says false, and it sits where the build injects it — a scalar after the document's types and the meta-schema in the type table, a directive after the document's own directives in definition order, both in the built-in SDL's order — rather than where the document wrote it. Where descriptions were not asked for, the injected texts stand in for the document's too. Anything the response reports otherwise, `null` descriptions included, is written out as a definition of its own and agrees with the SDL door on flag, text and place. No draft §5 rule reads any of it |
//! | **The meta types' descriptions and the order of their members** — what a response's `__Schema.types` says about `__Schema`, `__Type`, `__Field` and the other five beyond their shape | **No, by design.** No document can define a `__`-prefixed type ([`RedefinedBuiltInType`](super::SchemaErrorKind::RedefinedBuiltInType)), so the SDL door always carries smear's meta-schema and this door carries the same one: what a server says about the specification's own types beyond their shape is its rendering of the specification, not content of the schema it describes — graphql-js lists `__Directive.isRepeatable` third, where smear lists it last. The shape is held, though: a meta type of another kind, implementing anything, or reporting a field, an argument or an enum value smear's meta-schema does not have, or has with another type, is refused as [`InvalidMetaType`](ResponseErrorKind::InvalidMetaType). One that leaves members out, as a server older than `isRepeatable` or `specifiedByURL` does, is not |
//!
//! Descriptions, `@deprecated` and `@specifiedBy` were rows here and are not any more: the door
//! reads `description`, `isDeprecated` / `deprecationReason` and `specifiedByURL` wherever draft §4
//! has them, renders them back into the SDL, and the build keeps what it keeps from the SDL door.
//!
//! Everything else round-trips, and [the two-door agreement test] is what says so rather than this
//! table: it builds the same schema through both doors and compares every observation a rule can
//! make. Each row above but the last is a row *because* it would otherwise make that test fail;
//! the last records what both doors carry instead of the response's, which a server other than the
//! test's own renders differently. The built-in flag and the meta types each have a test of their
//! own, so those losses are pinned rather than described. The `includeDeprecated` row cannot have
//! one, and that is the point of it: a response with the
//! deprecated members omitted is indistinguishable from a response for a schema that never had
//! them, so no test on this side of the wire can tell — only the query that produced the response
//! can.
//!
//! [the two-door agreement test]: https://github.com/al8n/smear/blob/main/smear/tests/validator_introspection.rs
//! [`Schema::build`]: super::Schema::build

mod decode;
mod error;
mod json;
mod model;
mod sdl;

pub use error::{IntrospectionError, ResponseError, ResponseErrorKind};

use std::string::String;

use tokora::{Parse as _, Parser};

use smear_parser::graphql::{
  GraphQL, ast::TypeSystemDocument, error::GraphqlErrors, syntactic::GraphqlLexer,
};

use super::Schema;

impl Schema {
  /// Builds a schema from a draft §4 introspection response.
  ///
  /// The response may be the whole GraphQL reply (`{"data": {"__schema": …}}`), the same with the
  /// transport peeled off (`{"__schema": …}`), or the `__Schema` object on its own.
  ///
  /// Draft §3 "Type Validation" runs, exactly as it does in [`Schema::build`]: a response that
  /// describes something that is not a schema comes back as
  /// [`IntrospectionError::Schema`] carrying the same
  /// [`SchemaErrors`](super::SchemaErrors) the equivalent SDL would produce. See [the module
  /// header](self) for why, and for what an introspection result cannot say.
  pub fn from_introspection(response: &str) -> Result<Self, IntrospectionError> {
    let sdl = to_sdl(response)?;
    let document = parse(&sdl)?;
    Schema::build(&document).map_err(IntrospectionError::Schema)
  }
}

/// Renders a draft §4 introspection response as SDL.
///
/// This is the text [`Schema::from_introspection`] builds from, and it is public for two reasons:
/// converting a server's introspection result into a schema document is a job people have
/// independently of validating anything, and a door whose intermediate form can be printed is one
/// whose behaviour can be argued about.
///
/// The SDL describes the same schema, not the same document. Descriptions are written back, and so
/// are the three applied directives introspection carries: `@oneOf`, `@deprecated` and
/// `@specifiedBy`. Every other applied directive is absent — [the module header](self) says what a
/// [`Schema`] could have observed of one — and so are the definitions every build injects for
/// itself: the introspection meta-schema, the five built-in scalars and the five specified
/// directives. A built-in scalar or a specified directive the response reports other than as the
/// build injects it is the exception, and is written out. The meta-schema is held to draft §4's
/// shape before it is dropped — a meta type in another shape is refused as
/// [`InvalidMetaType`](ResponseErrorKind::InvalidMetaType) — while its descriptions and the
/// order of its members, which no document can define either, are smear's rather than the
/// response's.
///
/// ```
/// use smear_schema::introspection;
///
/// let response = r#"{"__schema":{
///   "queryType": {"name": "Query"},
///   "directives": [],
///   "types": [
///     {"kind":"OBJECT","name":"Query","fields":[
///       {"name":"ok","type":{"kind":"SCALAR","name":"Int","ofType":null}}
///     ]},
///     {"kind":"SCALAR","name":"Int"}
///   ]
/// }}"#;
///
/// let sdl = introspection::to_sdl(response).expect("a schema document");
/// assert!(sdl.contains("type Query {\n  ok: Int\n}"));
/// // `Int` is injected by every build, so the door does not re-declare it.
/// assert!(!sdl.contains("scalar Int"));
/// ```
pub fn to_sdl(response: &str) -> Result<String, IntrospectionError> {
  sdl::render(response).map_err(IntrospectionError::Response)
}

/// Parses the renderer's own output.
///
/// The failure arm is a backstop for a bug in this crate, not for anything a response can do:
/// every byte the renderer emits is its own punctuation, a fragment it checked first, or prose it
/// wrote as a quoted string nothing in it can close.
/// It is an error rather than an `unwrap` because "unreachable" is a claim, and a claim that costs
/// a `Result` arm is cheaper than a claim that costs a panic in a server.
fn parse(sdl: &str) -> Result<TypeSystemDocument<&'_ str>, IntrospectionError> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(smear_parser::graphql::syntactic::type_system_document)
  .parse_str(sdl)
  .map_err(|errors| {
    IntrospectionError::Response(ResponseError::new(
      ResponseErrorKind::UnparsableSdl,
      std::format!("{errors:?}"),
    ))
  })
}
