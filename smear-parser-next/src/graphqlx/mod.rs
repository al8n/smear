//! The GraphQLx dialect assembly layer.
//!
//! GraphQLx is GraphQL extended with generics (`<…>`), `::`-paths, `set`/`map`
//! composites, and `import` definitions. Every GraphQLx production is a free fn
//! generic over `L: Lexer<'inp>` — bounded only by tokora capability traits and
//! the [`ParseCtx`](crate::combinator::ParseCtx) bundle — with `Lang` left generic
//! and `Span = SimpleSpan`, exactly as the GraphQL dialect. Keeping the productions
//! `Lang`-generic is what lets the same assemblies serve the syntactic
//! (trivia-skipped) and lossless (trivia-preserving) suites: swap the lexer and the
//! emitter, nothing else.
//!
//! This module carries the dialect substrate the productions build on:
//!
//! - `GraphQLx` — the dialect marker,
//! - `kinds` — the unified `#[repr(u16)]` `SyntaxKind` space (token kinds, node
//!   kinds, and the `Root`/`Error`/`Gap` bookkeeping kinds), a superset of the
//!   GraphQL space that adds the generic / path / set / map / import kinds,
//! - `keyword` — the typed keyword node atoms (the GraphQL set plus
//!   `import`/`from`/`as`/`where`/`set`/`map`),
//! - `ast` — the AST node types the productions return, aliased onto the
//!   [`smear_scaffold`](crate::scaffold) node vocabulary,
//! - `error` — the dialect error and the `From` glue that makes it a
//!   [`ParseCtx`](crate::combinator::ParseCtx) error over every lexer and source,
//! - `prelude` — the generic atom vocabulary, re-exported for productions.

pub mod ast;
pub mod error;
pub mod keyword;
pub mod kinds;

/// The GraphQLx dialect marker.
///
/// Pins the `Lang` type parameter of every GraphQLx production, atom
/// instantiation, and error type, so one set of free-fn assemblies specializes to
/// GraphQLx over any lexer and source. It is a pure type-level tag — never
/// constructed, only named in type position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQLx;

/// The generic atom vocabulary, re-exported for GraphQLx productions.
///
/// A production writes `use crate::graphqlx::prelude::*;` to pull in the combinator
/// atoms it assembles — the [Lego bricks](crate::combinator) that name only
/// capability traits, the [`ParseCtx`](crate::combinator::ParseCtx) bundle, and the
/// slice/error projections, never a concrete lexer. The dialect-specific pieces
/// (`GraphQLx`, `kinds`, `ast`, `error`) are named through `super::` at each
/// production, keeping the marker and the kind space explicit at the use site.
pub mod prelude {
  pub use crate::combinator::*;
}
