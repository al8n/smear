/// Concrete syntax tree (CST) GraphQL token,
/// Use this if you need to preserve all original source information, e.g. for formatting or linting.
pub mod cst;

/// Abstract syntax tree (AST) GraphQL token, skip [ignored tokens](https://spec.graphql.org/draft/#Ignored) directly while lexing,
/// and try to lex as much as possible in one pass.
pub mod ast;

pub use string_lexer::{BlockString, InlineString};

mod handlers;
mod string_lexer;

#[cfg(test)]
mod tests;
