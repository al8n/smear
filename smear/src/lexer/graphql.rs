/// Errors for standard GraphQL lexers
pub mod error;

/// Abstract syntax tree (AST) lexers for GraphQL
pub mod syntactic;

/// Concrete syntax tree (CST) lexers for GraphQL
pub mod lossless;

mod handlers;

#[cfg(test)]
mod tests;
