//! GraphQL document validation.
//!
//! Standard GraphQL only. The `graphqlx` dialect's generics, `where` constraints, map and set
//! types and namespaced paths have no specification semantics to validate against, so nothing in
//! this module knows about them.
//!
//! # What is here now
//!
//! [`schema`](crate::validator::schema) — the built-once
//! [`Schema`](crate::validator::schema::Schema) every rule reads, and the draft §3 "Type
//! Validation" pass that runs while building it. A server rejects a malformed SDL exactly once,
//! at startup, and never rediscovers it per request.
//!
//! Every link in this module's docs is crate-absolute on purpose. `pub mod validator;` in
//! `lib.rs` carries an outer doc comment as well, and rustdoc resolves the merged fragments in
//! the **parent** module's scope — so a `schema`-relative link here reports "no item named
//! `schema` in module `smear`" under `RUSTDOCFLAGS="-D warnings"`.
//!
//! Executable-document validation (draft §5) is built on this substrate and lands separately.
//!
//! # The shape, and why
//!
//! Three properties of [`Schema`](crate::validator::schema::Schema) decide everything
//! downstream:
//!
//! - **It is not generic over the document's source type.** The builder consumes a
//!   `TypeSystemDocument<S>` for any `S: AsRef<[u8]>` — the bound `tokora`'s entire source lattice
//!   satisfies — and produces one owned, source-independent value. So the SDL can be dropped the
//!   moment the schema exists, and the schema can be `'static`, sent across threads, or handed to
//!   a foreign caller.
//! - **It holds no `Arc`, no atomics and no interior mutability.** Validation takes `&Schema`, so
//!   there is no shared-ownership primitive to choose and no target tier to gate: the caller wraps
//!   it in whatever pointer their platform has. That also forbids lazy interning and per-query
//!   memoisation inside it, which is a constraint, not an oversight — it is what keeps a rule a
//!   pure function of `(schema, document)`.
//! - **Every cost a rule would otherwise pay per query is paid once, here.** Names are `u32`
//!   symbols, field lookup is a binary search over a sym-sorted group, directive locations are a
//!   `u32` mask, and an interface's implementors are a bitset — so draft 5.5.2.3, all four of its
//!   subsections, is one word-`AND`.

pub mod schema;

pub use schema::{Schema, SchemaBuilder, SchemaError, SchemaErrorKind, SchemaErrors};
