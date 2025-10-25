//! Concrete Syntax Tree (CST) support for GraphQL and GraphQLx.
//!
//! This module provides `SyntaxKind` enums that can be used with the `rowan` crate
//! to build lossless Concrete Syntax Trees. Unlike ASTs which discard trivia
//! (whitespace, comments), CSTs preserve all source information for tooling like
//! formatters, linters, and IDEs.
//!
//! ## Feature Gate
//!
//! This module is only available when the `cst` feature is enabled:
//!
//! ```toml
//! [dependencies]
//! smear-scaffold = { version = "*", features = ["cst"] }
//! ```
//!
//! ## Rowan Integration
//!
//! Both `GraphQLSyntaxKind` and `GraphQLxSyntaxKind` implement `From<T> for rowan::SyntaxKind`,
//! allowing them to be used directly with rowan's red-green tree implementation.

#[cfg(feature = "cst")]
pub mod graphql;

#[cfg(feature = "cst")]
pub mod graphqlx;
