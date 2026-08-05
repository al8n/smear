#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![allow(clippy::type_complexity)]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Re-exported lexer crate.
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
let parse = graphql::lossless::parse_str("type T { f: Int }");
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
