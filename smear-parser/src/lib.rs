#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]
// Was `#[allow(clippy::type_complexity)]` on `pub mod parser;` in `smear`'s crate root, which is
// where it landed when #84 merged the two crates. Splitting them again puts it back on the crate
// root it started on, and it covers the parser alone once more rather than reaching the lexer.
#![allow(clippy::type_complexity)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
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

```compile_fail
# use smear_parser::{graphql, graphqlx};
# use smear_parser::lossless::ast::CastNode;
let parse = graphql::lossless::parse_document("type T { f: Int }");
let node = parse.syntax();
// error[E0308]: expected `SyntaxNode<GraphQLxLang>`, found `SyntaxNode<GraphQLLang>`
let _ = graphqlx::lossless::ast::ObjectTypeDefinition::cast_node(node);
```

Two things about where this sits.

It is **gated on `graphql`**. Without the gate the snippet would still fail to compile — on
`graphql` being an unresolved module — and a `compile_fail` doctest that fails for the wrong reason
is green in exactly the same way as one that fails for the right one. The failure was checked by
running the block un-gated and reading the error: `E0308`, naming both languages.

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
