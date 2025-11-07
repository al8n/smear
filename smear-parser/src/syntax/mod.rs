//! Syntax type definitions for GraphQL and GraphQL-like languages.
//!
//! This module contains syntax type descriptors and their component enums
//! for comprehensive error reporting during parsing.


/// GraphQL syntax types and components
#[cfg(feature = "graphql")]
pub mod graphql;

/// GraphQLx syntax types and components
#[cfg(feature = "graphqlx")]
pub mod graphqlx;
