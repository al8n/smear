//! The built-once schema: representation, construction, and draft §3 refusal.
//!
//! [`Schema`] is the substrate every validation rule sits on. It is built once from a
//! `TypeSystemDocument`, owns everything it needs, and is then read — never written — by an
//! unbounded number of concurrent validations holding `&Schema`.
//!
//! # The `graphql` feature, and what the default tier is for
//!
//! This module is the crate's runtime layer, and every build compiles it. Three consumers read it,
//! and they do not want the same thing. The validator, the crate's `graphql` module, works on a
//! schema the builder BUILDS, so it needs the parser underneath it; `graphql-proto` READS one at
//! execution time and `smear-derive` will generate from one, and neither has any use for a front
//! end. So the builder and the introspection door sit behind the crate's `graphql` feature, which is
//! what pulls `smear-parser` — and a consumer who only reads a schema depends on `smear-compiler`
//! with `default-features = false` and a tier, and gets `core` + `alloc` with `tokora` as its
//! dependency, and smol-bytes beside it on a target with pointer-width atomics, for the owned text
//! this module shares with `graphql-proto`.
//!
//! That is a FEATURE boundary and not a crate boundary, and the distinction is the whole reason the
//! builder is a feature of this crate and not a crate of its own: `Schema::build` and
//! `Schema::from_introspection` are **inherent** `impl Schema` blocks, and an inherent impl cannot
//! cross a crate boundary. Splitting them out would have cost an extension trait per body and a
//! `use` line at every call site, for a tier the feature buys outright.
//!
#![cfg_attr(
  all(any(feature = "std", feature = "alloc"), feature = "graphql"),
  doc = "```
use smear_parser::{
  graphql::{
    GraphQL, ast::TypeSystemDocument, error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_compiler::schema::{RootOperation, Schema};

let sdl = \"type Query { hero: Character } interface Character { name: String! }\";
let document = Parser::with_parser::<
  GraphqlLexer<'_, str>,
  TypeSystemDocument<&str>,
  GraphqlErrors<&str>,
  _,
  GraphQL,
>(type_system_document)
.parse_str(sdl)
.expect(\"the SDL parses\");

let schema = Schema::build(&document).expect(\"the SDL is a schema\");
let (query, _) = schema.type_by_name(b\"Query\").expect(\"Query is defined\");
assert_eq!(schema.root(RootOperation::Query), Some(query));

// Introspection is part of every schema, so an introspection query has something to
// validate against.
assert!(schema.type_by_name(b\"__Schema\").is_some());
```"
)]

// The diagnostic vocabulary: tokora's `diagnostic` module, re-exported item by item under the
// path smear has always published it at.
//
// It sits in this module, the layer every build compiles, and it is ungated because
// `smear::diagnostic` is ungated and moving it behind a feature would be an API change. That is
// also why `smear` depends on `smear-compiler` unconditionally rather than behind `validator`.
//
// The contract used to be declared here, written to be lifted into `tokora` unchanged; it has
// been, and this module now carries only the re-export. The declaration still carries no outer doc
// comment: rustdoc resolves the MERGED fragments of a module's documentation in the scope of
// whichever attribute came from outside, so an outer comment here would reinterpret every link in
// the module's own header as one rooted in this module, where the re-exported names are not.
pub mod diagnostic;

pub mod builtin;

// Deliberately no outer doc comment, for the reason recorded on `diagnostic` above: an outer
// comment here would silently reinterpret every `super::` link in `introspection/mod.rs` as one
// rooted in this crate's root, and they would all stop resolving under `RUSTDOCFLAGS="-D warnings"`.
#[cfg(all(any(feature = "std", feature = "alloc"), feature = "introspection"))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(any(feature = "std", feature = "alloc"), feature = "introspection")))
)]
pub mod introspection;

#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql"))]
mod builder;
#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql"))]
mod error;
#[cfg(any(feature = "std", feature = "alloc"))]
mod repr;
#[cfg(any(feature = "std", feature = "alloc"))]
mod text;

/// The literal-shape coercion table, which the builder alone reads.
///
/// Crate-private on purpose: it is a seam inside the builder, not a promise to anyone outside the
/// crate, and `repr` — the part of this module that is published widest — stays free of it.
#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql"))]
pub(crate) mod literal;

#[cfg(all(any(feature = "std", feature = "alloc"), feature = "introspection"))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(any(feature = "std", feature = "alloc"), feature = "introspection")))
)]
pub use introspection::IntrospectionError;

/// Draft §6.3's collection indexes the *document's* names the way [`NameIndex`] indexes the
/// schema's, and one hash for both is one hash to reason about. See [`NameIndex`] for why the two
/// callers owe different arguments about it being unkeyed.
///
/// `#[doc(hidden)]`: a seam between this crate and `graphql-proto`, not a promise to anyone outside
/// the workspace. It was `pub(crate)` behind `#[cfg(feature = "proto")]` while both lived in one
/// crate; a crate boundary has no narrower spelling to offer, and two functions is the whole of the
/// widening.
#[cfg(any(feature = "std", feature = "alloc"))]
#[doc(hidden)]
pub use repr::name::{bucket, hash_bytes};

/// Owned text, cheap to clone where the target has pointer-width atomics and a plain box where it
/// has not: `text.rs`'s header has the two arms and why the choice is made once, here.
///
/// `#[doc(hidden)]`, and so are the two types' inherent methods: a seam between this crate and
/// `graphql-proto`, like the hash above, and not a promise to anyone outside the workspace. The
/// methods carry the attribute themselves because the source census reads a `pub use` whatever
/// it says, and skips an `impl` block that is hidden.
#[cfg(any(feature = "std", feature = "alloc"))]
#[doc(hidden)]
pub use text::{FrozenStr, StrBuf};

#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql"))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql")))
)]
pub use builder::{
  MAX_CONST_VALUE_DEPTH, MAX_INTERFACE_IMPLEMENTATION_MISMATCHES,
  MAX_MISSING_TRANSITIVE_INTERFACES, SchemaBuilder,
};
#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql"))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql")))
)]
pub use error::{SchemaError, SchemaErrorKind, SchemaErrors};
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use repr::{
  DefaultKind, Deprecation, DirectiveDef, DirectiveLocation, DirectiveLocations, DirectiveMeta,
  EnumValueMeta, FieldDef, FieldMeta, InputValueDef, InputValueMeta, MAX_DIRECTIVE_ARGUMENTS,
  MAX_FIELD_ARGUMENTS, MAX_SYMBOLS, MAX_WRAPPERS, NameIndex, PackedType, Range32, RootOperation,
  Schema, Sym, Text, TypeDef, TypeFlags, TypeId, TypeKind, TypeMeta, TypeRef, is_name,
  is_name_tail, is_reserved,
};
