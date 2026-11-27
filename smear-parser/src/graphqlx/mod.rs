//! The GraphQLx dialect assembly layer.
//!
//! GraphQLx extends GraphQL's lexical name and value grammar with `::` paths
//! under an optional `~::`, `^::` or `..::` root, `set`/`map` collection
//! constructors, generic type arguments, `use` items and inline `mod`s.
//! Its parsers are specialized to the concrete GraphQLx syntactic lexer while
//! retaining source-generic, zero-copy AST slices.
//!
//! # What no valid GraphQL document contains
//!
//! The following occur in no valid GraphQL document:
//!
//! - `~`, `..` and `^`;
//! - a `use` or `mod` entry;
//! - `set!` and `map!`;
//! - `where` or `implements` after a scalar, enum, union or input header.
//!
//! So every valid GraphQL document lexes and parses as GraphQL parses it, through all three
//! document roots and every production entry point.

/// GraphQLx AST aliases and dialect-specific value, `use` and `mod` nodes.
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
// No `///` here either, and for the same reason `kinds` has none: `lossless/mod.rs` carries its
// own `//!` header, and a module with doc fragments from both places has the intra-doc links in
// *all* of them resolved against the scope of the first — this file's.
//
// Gated on `rowan` because `crate::lossless`, the substrate every item under it is written over,
// is. `kinds` above is *not* gated, which is the asymmetry that keeps the `graphqlx`-without-
// `rowan` build green: the kind space describes GraphQLx's grammar, while a tree over it needs
// rowan.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
pub mod lossless;
/// GraphQLx syntactic productions over the concrete lexer.
pub mod syntactic;

mod layer;

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
pub use layer::Lossless;
pub use layer::{Layer, Syntactic};

pub use crate::layer::{Parsed, ParsedComponents, Recovery, Rejected, Report};

/// The GraphQLx dialect marker.
///
/// This is a type-level tag used to brand AST carriers and parser contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQLx(());

/// Whether a number's token, `source`, begins with `-`, read from its first character.
///
/// After a value where no second value may follow, a number written with a `-` goes on with an
/// expression: `HOUR-1` is `HOUR` and the literal `-1`. Both layers ask it there, and only of a
/// token whose kind is a number's.
#[inline]
pub(crate) fn begins_with_minus<'source, S>(source: &S) -> bool
where
  S: tokora::Slice<'source>,
  S::Char: tokora::utils::IsAsciiChar,
{
  source
    .iter()
    .next()
    .is_some_and(|first| tokora::utils::IsAsciiChar::is_ascii_char(&first, ascii::AsciiChar::Minus))
}

#[cfg(test)]
mod tests;
