//! The built-once schema: representation, construction, and draft §3 refusal.
//!
//! [`Schema`] is the substrate every validation rule sits on. It is built once from a
//! `TypeSystemDocument`, owns everything it needs, and is then read — never written — by an
//! unbounded number of concurrent validations holding `&Schema`.
//!
//! ```
//! use smear::{
//!   parser::graphql::{
//!     GraphQL, ast::TypeSystemDocument, error::GraphqlErrors,
//!     syntactic::{GraphqlLexer, type_system_document},
//!   },
//!   validator::schema::{RootOperation, Schema},
//! };
//! use smear::lexer::tokora::{Parse as _, Parser};
//!
//! let sdl = "type Query { hero: Character } interface Character { name: String! }";
//! let document = Parser::with_parser::<
//!   GraphqlLexer<'_, str>,
//!   TypeSystemDocument<&str>,
//!   GraphqlErrors<&str>,
//!   _,
//!   GraphQL,
//! >(type_system_document)
//! .parse_str(sdl)
//! .expect("the SDL parses");
//!
//! let schema = Schema::build(&document).expect("the SDL is a schema");
//! let (query, _) = schema.type_by_name(b"Query").expect("Query is defined");
//! assert_eq!(schema.root(RootOperation::Query), Some(query));
//!
//! // Introspection is part of every schema, so an introspection query has something to
//! // validate against.
//! assert!(schema.type_by_name(b"__Schema").is_some());
//! ```

pub mod builtin;

// Deliberately no outer doc comment. The module carries its own `//!` header, and rustdoc
// resolves the *merged* fragments of a module's documentation in the scope of whichever attribute
// came from outside — so an outer comment here would silently reinterpret every `super::` link in
// `introspection/mod.rs` as one rooted in `validator`, and they would all stop resolving under
// `RUSTDOCFLAGS="-D warnings"`. `validator/mod.rs`'s header records the same trap one level up.
#[cfg(feature = "introspection")]
#[cfg_attr(docsrs, doc(cfg(feature = "introspection")))]
pub mod introspection;

mod builder;
mod error;
mod repr;

/// The literal-shape coercion table, shared with the executable rules.
///
/// Crate-private on purpose: it is a seam between two callers inside this crate, not a promise to
/// anyone outside it, and `repr` — the part of this module that *is* published — stays free of it.
pub(crate) mod literal;

#[cfg(feature = "introspection")]
#[cfg_attr(docsrs, doc(cfg(feature = "introspection")))]
pub use introspection::IntrospectionError;

pub use builder::SchemaBuilder;
pub use error::{SchemaError, SchemaErrorKind, SchemaErrors};
pub use repr::{
  DefaultKind, DirectiveDef, DirectiveLocation, DirectiveLocations, FieldDef, InputValueDef,
  MAX_SYMBOLS, MAX_WRAPPERS, NameIndex, PackedType, Range32, RootOperation, Schema, Sym, TypeDef,
  TypeFlags, TypeId, TypeKind, is_name, is_reserved,
};
