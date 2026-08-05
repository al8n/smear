//! The GraphQLx dialect assembly layer.
//!
//! GraphQLx extends GraphQL's lexical name and value grammar with `::` paths,
//! `set`/`map` collection constructors, generic type arguments, and imports.
//! Its parsers are specialized to the concrete GraphQLx syntactic lexer while
//! retaining source-generic, zero-copy AST slices.

/// GraphQLx AST aliases and dialect-specific import nodes.
pub mod ast;
/// GraphQLx parser errors and Tokora conversion glue.
pub mod error;
// No `///` here, and that is deliberate. `kinds.rs` carries its own `//!` header, and when a
// module has doc fragments from both places rustdoc resolves the intra-doc links in *all* of them
// against the scope of the *first* — this file's — where `SyntaxKind` is not in scope. Fifteen
// links in that header silently broke that way; `crate::lossless`'s own header records the same
// finding. One fragment, one scope, links that resolve where they are written.
//
// Not gated on `rowan`: the space describes GraphQLx's grammar, which exists whether or not a tree
// is ever built from it. Its `KindSpace` impl and its `rowan::Language` are the gated parts,
// because those are the parts that need the substrate.
pub mod kinds;
/// GraphQLx syntactic productions over the concrete lexer.
pub mod syntactic;

/// The GraphQLx dialect marker.
///
/// This is a type-level tag used to brand AST carriers and parser contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQLx(());

#[cfg(test)]
mod tests;
