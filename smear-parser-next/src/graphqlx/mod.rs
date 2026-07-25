//! The GraphQLX dialect assembly layer.
//!
//! GraphQLX extends GraphQL's lexical name and value grammar with `::` paths,
//! `set`/`map` collection constructors, generic type arguments, and imports.
//! Its parsers are specialized to the concrete GraphQLX syntactic lexer while
//! retaining source-generic, zero-copy AST slices.

/// GraphQLX AST aliases and dialect-specific import nodes.
pub mod ast;
/// GraphQLX parser errors and Tokora conversion glue.
pub mod error;
/// GraphQLX syntactic productions over the concrete lexer.
pub mod syntactic;

/// The GraphQLX dialect marker.
///
/// This is a type-level tag used to brand AST carriers and parser contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQLX(());

#[cfg(test)]
mod tests;
