//! GraphQLx AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate (`graphqlx/ast/`): the
//! node types the productions return, aliased onto the
//! [`smear_scaffold`](crate::scaffold) node vocabulary and keyed by the source
//! slice `S`, with spans pinned to [`SimpleSpan`](tokora::SimpleSpan). The parser
//! fns that lived beside them in the frozen crate are *not* copied — the
//! productions are rebuilt on the atom layer, wave by wave.
//!
//! The substrate wave landed the value layer (`value`), the type-reference layer
//! (`ty`), the generic / where family (`generic`), and the import types
//! (`import`); the executable wave added the argument / directive / selection /
//! executable-definition aliases (`argument`, `directive`, `field`, `executable`).
//! The SDL definition and document aliases land with their production waves.

use std::vec::Vec;

pub use argument::*;
pub use directive::*;
pub use executable::*;
pub use field::*;
pub use generic::*;
pub use import::*;
pub use name::*;
pub use ty::*;
pub use value::*;

mod argument;
mod directive;
mod executable;
mod field;
mod generic;
mod import;
mod name;
mod ty;
mod value;

/// The default container type used for AST collections (lists, sets, maps,
/// objects).
pub type DefaultVec<T> = Vec<T>;
