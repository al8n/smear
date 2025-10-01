/// Errors for standard GraphQL lexers
pub mod error;

/// Abstract syntax tree (AST) lexers for GraphQL
pub mod ast;

mod handlers;

#[cfg(test)]
mod tests;
