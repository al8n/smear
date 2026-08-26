#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]
// Was `#[allow(clippy::type_complexity)]` on `pub mod parser;` in `smear`'s crate root, which is
// where it landed when #84 merged the two crates. Splitting them again puts it back on the crate
// root it started on, and it covers the parser alone once more rather than reaching the lexer.
#![allow(clippy::type_complexity)]

// The alloc-as-std alias: the crate's files spell their allocations `std::boxed::Box`, which
// resolves to `alloc` in a `no_std` build and to the real `std` otherwise.
//
// `#[allow(unused_extern_crates)]`, ON EVERY MEMBER AND NOT ONLY THE ONE THAT TRIPS TODAY. The
// alias is a crate-wide compatibility shim whose USERS are feature-gated, and
// `unused_extern_crates` is a lint about the item: it fires in any configuration that happens to
// compile none of them. On this tree that is `smear-parser --no-default-features`, where every
// `std::` use is inside a dialect or `rowan` module — but the construct is identical in all five
// members and which one trips is an accident of where the uses sit. Measured: `smear-lexer` 41,
// `smear-parser` 70, `smear-schema` 15, `smear-compiler` 8, `graphql-proto` 82.
//
// Not a narrower `#[cfg]`: the honest predicate would be "any feature whose module names `std::`",
// which is a restatement of the module list and drifts the first time one is added. And not
// deletion, because the gated modules need it.
//
// `unused_extern_crates` is DENIED in `[workspace.lints.rust]` so that this allow is the recorded
// exception rather than the lint being off — a stray `extern crate` anywhere else is a hard error
// locally, with no `RUSTFLAGS` needed. That deny is the repair for the gate, not for the code:
// this construct reddened CI twice, and both times the local run that cleared it was narrower
// than CI's.
#[cfg(not(feature = "std"))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(feature = "std")]
#[allow(unused_extern_crates)]
extern crate std;

/// The lexer layer these parsers are built on.
///
/// A re-export of the `smear-lexer` crate, so `smear_parser::lexer::X` — and
/// `smear::parser::lexer::X` through the umbrella — keeps resolving to the same items as
/// `smear_lexer::X`.
pub use smear_lexer as lexer;

pub mod combinator;

/// The dialect-generic lossless substrate: the kind-space contract, the trivia atoms, the
/// `Parse` surface, the coverage shims and the typed-wrapper macro's helpers.
#[cfg(feature = "rowan")]
pub mod lossless;

// -- THE LEXER'S CEILING AGAINST THE SUBSTRATE'S, AND WHY THE COMPARISON IS WRITTEN HERE -------
//
// `smear_lexer::limits::HARD_MAX` is what every lossless door clamps a parse's recursion budget
// to, and `lossless::project::MAX_GREEN_DEPTH` is how deep the projection's walks will descend.
// They live in different crates and had a relationship nothing enforced: a margin derived over 24
// open brackets went on being stated over 256 of them, and at the top of that range a document
// this crate's own parser accepted with no diagnostic projected as `TooDeep`. al8n/smear#198.
//
// The comparison cannot be written in the substrate. `src/lossless/` is parameterised over
// `L: Lexer` and is forbidden from naming a concrete lexer crate — `SUBSTRATE_FORBIDDEN` in
// `smear/tests/lossless_isolation.rs` refuses the spelling outright, and the same file's
// `ALLOWED_CRATE_ROOTS` grants that root to the two dialect trees and to neither the substrate nor
// anything else. It cannot be written in the lexer either, which does not depend on this crate.
//
// So it is written where all three are legitimately in scope: the root of the crate that assembles
// the lexer, the substrate and the dialects. That keeps it ONE site rather than one per dialect —
// a per-dialect assertion is a thing a third dialect can be added without — and it compiles in
// exactly the configurations the substrate itself does, since `mod lossless` carries the same cfg.
//
// The predicate is the substrate's own `MAX_DOOR_BRACKETS`, which is
// `MAX_GREEN_DEPTH / GREEN_LEVELS_PER_BRACKET`; over integers that is the same set of accepted
// values as `HARD_MAX * GREEN_LEVELS_PER_BRACKET <= MAX_GREEN_DEPTH`, so moving the assertion did
// not widen it. The plant that proves it is live is a `HARD_MAX` of 342: that value passes
// `HARD_MAX`'s OWN 1.9x margin assertion, so every gate that predates this invariant admits it,
// and it is exactly the edit that reopens the projection window.
#[cfg(feature = "rowan")]
const _: () = assert!(
  smear_lexer::limits::HARD_MAX <= lossless::project::MAX_DOOR_BRACKETS,
  "the lossless doors can accept a document whose green tree is deeper than the projection's walks \
   will descend, so a projection would refuse a parse this crate just produced. Raising HARD_MAX \
   means re-running WORST_DOOR_GREEN_TREE's shape table, not relaxing this."
);

/// Name-node carrier shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod name;

/// Namespaced-path carrier used by the GraphQLx dialect AST; vanilla GraphQL
/// has no namespaced paths.
#[cfg(feature = "graphqlx")]
mod path;

/// Generic-definition carriers shared by extended GraphQL-family dialect ASTs.
#[cfg(feature = "graphqlx")]
mod generic;

/// Selection-node carriers shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod selection;

/// Executable-document carriers shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod executable;

/// Type-reference carriers shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod ty;

/// Argument-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod argument;

/// Directive-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod directive;

/// Type-system AST carriers shared by the GraphQL-family dialects.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod type_system;

/// Value-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod value;

/// The GraphQL dialect: productions, syntax kinds, keyword atoms, AST node types,
/// and the dialect error, all keyed to the [`GraphQL`](graphql::GraphQL) marker.
#[cfg(feature = "graphql")]
pub mod graphql;

/// The GraphQLx dialect: namespaced paths, collection values, generic type
/// references, and imports over the concrete GraphQLx lexer.
#[cfg_attr(
  all(feature = "graphql", feature = "rowan"),
  doc = r#"
# A GraphQL node may not be cast through a GraphQLx wrapper

True by construction — the two dialects derive their kind spaces independently, so
`rowan::SyntaxNode<GraphQLLang>` and `rowan::SyntaxNode<GraphQLxLang>` are different types. But
"true by construction" is what a refactor breaks silently, so it is asserted:

```compile_fail,E0308
# use smear_parser::{graphql, graphqlx};
# use smear_parser::lossless::ast::CastNode;
let parse = graphql::lossless::parse_document("type T { f: Int }");
let node = parse.syntax();
// error[E0308]: expected `SyntaxNode<GraphQLxLang>`, found `SyntaxNode<GraphQLLang>`
let _ = graphqlx::lossless::ast::ObjectTypeDefinition::cast_node(node);
```

and this one must compile, over the same paths:

```
# use smear_parser::{graphql, graphqlx};
# use smear_parser::lossless::ast::CastNode;
let parse = graphql::lossless::parse_document("type T { f: Int }");
let node = parse.syntax();
// The same call with the dialects matching. `cast_node` is a kind check, so the document root
// answers `None` — the point is that it resolves.
assert!(graphql::lossless::ast::ObjectTypeDefinition::cast_node(node).is_none());
let _ = graphqlx::lossless::parse_document("type T { f: Int }");
```

Three things about how this is written.

It is **coded**, and the control beneath it is why the code can be trusted. A bare `compile_fail`
is satisfied by *any* failure, so an unresolved import, a renamed `cast_node` or a moved
`parse_document` would keep it green while never reaching the mismatch it claims to pin — the
failure mode the paragraph below names for the module gate, reached through the snippet instead.
`E0308` makes rustdoc report `Some expected error codes were not found` for those, but a rustdoc
checks the code only on nightly, and this repository has exactly one nightly `cargo test --doc`
(the `coverage` job). The block above carries the same names in a snippet that must **compile**, so
a rename is caught on every toolchain that runs doctests, whether or not the code is being read.

It is **gated on `graphql`**. Without the gate the snippet would fail on `graphql` being an
unresolved module rather than on the cast — `E0433`, not `E0308`, which the fence now rejects on
nightly and the positive control rejects everywhere.

It is **at the crate root and not in `graphqlx::lossless::ast`**, where the plan put it. A doctest
that names both dialects has to live in a module allowed to name both, and
`tests/lossless_isolation.rs` forbids exactly that of either dialect's tree. This module
declaration is where the two dialects are introduced, so it is the one place the assertion is not
itself a boundary crossing.
"#
)]
#[cfg(feature = "graphqlx")]
pub mod graphqlx;

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` re-exports this crate whole, so every `#[cfg(feature = …)]` inside
/// it is gated by THIS crate's features — and cargo unifies a package's features across the entire
/// graph, so a second dependency naming `smear-parser` directly could switch a capability on behind a
/// `smear` consumer who never asked for it. Observed, not argued: with
/// `smear = { default-features = false, features = ["std"] }` plus a direct `smear-parser` dependency,
/// a path the consumer had not enabled resolved.
///
/// `smear` reads these constants and refuses to compile when one disagrees with its own matching
/// feature, which is what makes "the umbrella's feature is the gate" true rather than advertised.
/// The alternative — a facade module per gated path — cannot reach trait impls, which are not
/// namespaced, and would have to mirror every nested `#[cfg]` besides.
///
/// `ci/feature_reachability.py` derives this list from `cargo metadata` and fails when a feature
/// this crate declares has no constant here or no assertion in `smear`.
#[doc(hidden)]
pub mod __features {
  /// `bstr`, as this crate resolved it.
  pub const BSTR: bool = cfg!(feature = "bstr");
  /// `bytes`, as this crate resolved it.
  pub const BYTES: bool = cfg!(feature = "bytes");
  /// `graphql`, as this crate resolved it.
  pub const GRAPHQL: bool = cfg!(feature = "graphql");
  /// `graphqlx`, as this crate resolved it.
  pub const GRAPHQLX: bool = cfg!(feature = "graphqlx");
  /// `hipstr`, as this crate resolved it.
  pub const HIPSTR: bool = cfg!(feature = "hipstr");
  /// `lossless-coverage`, as this crate resolved it.
  pub const LOSSLESS_COVERAGE: bool = cfg!(feature = "lossless-coverage");
  /// `materialized-numbers`, as this crate resolved it.
  pub const MATERIALIZED_NUMBERS: bool = cfg!(feature = "materialized-numbers");
  /// `rowan`, as this crate resolved it.
  pub const ROWAN: bool = cfg!(feature = "rowan");
  /// `smallvec`, as this crate resolved it.
  pub const SMALLVEC: bool = cfg!(feature = "smallvec");
  /// `smol-bytes`, as this crate resolved it.
  pub const SMOL_BYTES: bool = cfg!(feature = "smol-bytes");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
  /// `test-support`, as this crate resolved it.
  pub const TEST_SUPPORT: bool = cfg!(feature = "test-support");
}
