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
/// GraphQLx syntactic productions over the concrete lexer.
pub mod syntactic;

/// The GraphQLx dialect marker.
///
/// This is a type-level tag used to brand AST carriers and parser contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphQLx(());

#[cfg(test)]
mod tests;
