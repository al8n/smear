//! GraphQLx AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate (`graphqlx/ast/`): the
//! node types the productions return, aliased onto the
//! [`smear_scaffold`](crate::scaffold) node vocabulary and keyed by the source
//! slice `S`, with spans pinned to [`SimpleSpan`](tokora::SimpleSpan). The parser
//! fns that lived beside them in the frozen crate are *not* copied — the
//! productions are rebuilt on the atom layer, wave by wave.
//!
//! This substrate wave lands the value layer (`value`), the type-reference layer
//! (`ty`), the generic / where family (`generic`), and the import types
//! (`import`); the SDL definition and document aliases land with their production
//! waves.

use std::vec::Vec;

pub use generic::*;
pub use import::*;
pub use name::*;
pub use ty::*;
pub use value::*;

mod generic;
mod import;
mod name;
mod ty;
mod value;

/// The default container type used for AST collections (lists, sets, maps,
/// objects).
pub type DefaultVec<T> = Vec<T>;
