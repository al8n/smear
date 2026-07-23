//! The GraphQL dialect assembly layer.
//!
//! GraphQL syntactic productions are specialized to the concrete
//! [`crate::graphql::syntactic::GraphqlLexer`] and the
//! [`crate::graphql::GraphQL`] marker. GraphQL AST types expose `graphql` /
//! `try_graphql` methods where available; for example, calling
//! `Directive::<&str>::graphql` on [`crate::graphql::ast::Directive`] parses a
//! directive whose source slices are `&str`. The concrete lexer source is
//! inferred from the parser input. Free production functions remain available
//! for composition.
//!
//! This module carries the dialect substrate the productions build on:
//!
//! - `GraphQL` — the dialect marker,
//! - `kinds` — the unified `#[repr(u16)]` `SyntaxKind` space (token kinds, node
//!   kinds, and the `Root`/`Error`/`Gap` bookkeeping kinds),
//! - `keyword` — the typed keyword node atoms,
//! - `ast` — the AST node types the productions return,
//! - `error` — the dialect error and the `From` glue that makes it a
//!   [`ParseCtx`](crate::combinator::ParseCtx) error over every lexer and source,
//! - `prelude` — the generic atom vocabulary, re-exported for productions.

pub mod ast;
pub mod error;
pub mod keyword;
pub mod kinds;
pub mod syntactic;

/// The GraphQL dialect marker.
///
/// Marks GraphQL parser inputs, contexts, and errors.
///
/// It is a pure type-level tag — never constructed, only named in type position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQL(());

#[cfg(test)]
mod tests;
