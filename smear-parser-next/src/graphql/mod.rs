//! The GraphQL dialect assembly layer.
//!
//! Every GraphQL production is a free fn generic over `L: Lexer<'inp>` — bounded
//! only by tokora capability traits and the [`ParseCtx`](crate::combinator::ParseCtx)
//! bundle — with the `Lang` type parameter pinned to the [`GraphQL`] marker and
//! `Span = SimpleSpan`. Pinning the marker is what lets the same assemblies serve
//! the syntactic (trivia-skipped) and lossless (trivia-preserving) suites: swap the
//! lexer and the emitter, nothing else.
//!
//! This module carries the dialect substrate the productions build on:
//!
//! - [`GraphQL`] — the dialect marker,
//! - [`kinds`] — the unified `#[repr(u16)]` [`SyntaxKind`](kinds::SyntaxKind) space
//!   (token kinds, node kinds, and the `Root`/`Error`/`Gap` bookkeeping kinds),
//! - [`keyword`] — the typed keyword node atoms,
//! - [`ast`] — the AST node types the productions return,
//! - [`error`] — the dialect error and the `From` glue that makes it a
//!   [`ParseCtx`](crate::combinator::ParseCtx) error over every lexer and source,
//! - [`prelude`] — the generic atom vocabulary, re-exported for productions.

pub mod ast;
pub mod error;
pub mod keyword;
pub mod kinds;

/// The GraphQL dialect marker.
///
/// Pins the `Lang` type parameter of every GraphQL production, atom
/// instantiation, and error type, so one set of free-fn assemblies specializes to
/// GraphQL over any lexer and source. It is a pure type-level tag — never
/// constructed, only named in type position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQL;

/// The generic atom vocabulary, re-exported for GraphQL productions.
///
/// A production writes `use crate::graphql::prelude::*;` to pull in the combinator
/// atoms it assembles — the [Lego bricks](crate::combinator) that name only
/// capability traits, the [`ParseCtx`](crate::combinator::ParseCtx) bundle, and the
/// slice/error projections, never a concrete lexer. The dialect-specific pieces
/// ([`GraphQL`], [`kinds`], [`ast`], [`error`]) are named through `super::` at each
/// production, keeping the marker and the kind space explicit at the use site.
pub mod prelude {
  pub use crate::combinator::*;
}

#[cfg(test)]
mod tests;
