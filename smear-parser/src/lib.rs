#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![allow(clippy::double_parens)]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Re-export of smear-lexer for convenience
pub use smear_lexer as lexer;

/// Scaffold module - generic, reusable AST scaffolding for GraphQL-like languages.
///
/// This module provides generic traits, types, and utilities that can be used
/// to build parsers for GraphQL and GraphQL-like DSLs.
pub mod scaffold {
  /// Parser error traits
  pub mod error {
    pub use crate::scaffold_error::*;
  }

  /// The scaffold AST nodes
  pub mod ast {
    pub use crate::ast_scaffold::*;
  }

  /// Syntax type definitions for error reporting
  #[cfg(any(feature = "graphql", feature = "graphqlx"))]
  pub mod syntax {
    /// GraphQL syntax types and components
    #[cfg(feature = "graphql")]
    pub mod graphql {
      pub use crate::syntax::graphql::*;
    }

    /// GraphQLx syntax types and components
    #[cfg(feature = "graphqlx")]
    pub mod graphqlx {
      pub use crate::syntax::graphqlx::*;
    }
  }
}

/// Parser combinators for standard GraphQL (draft specification).
///
/// This module provides a complete implementation of the GraphQL draft specification parser,
/// including support for type system definitions, executable documents, and schema definitions.
///
/// ## Features
///
/// - **Type System Parsing**: All GraphQL type definitions
///   - Scalar types
///   - Object types with fields and implements
///   - Interface types
///   - Union types
///   - Enum types with values
///   - Input object types
///
/// - **Executable Documents**: Queries, mutations, subscriptions
///   - Named and anonymous operations
///   - Variables with default values
///   - Selection sets with fields, fragments, inline fragments
///   - Directives
///
/// - **Schema Definitions**: Root operation types
///   - Type extensions
///
/// ## Entry Points
///
/// - [`ast::Document`](graphql::ast::Document): Parse mixed type system + executable documents
/// - [`ast::TypeSystemDocument`](graphql::ast::TypeSystemDocument): Parse schema definitions
/// - [`ast::ExecutableDocument`](graphql::ast::ExecutableDocument): Parse queries/mutations
///
/// ## Example
///
/// ```rust,ignore
/// use smear_parser::graphql::ast::{TypeSystemDocument, ParseStr};
///
/// let schema = r#"
///   type User {
///     id: ID!
///     name: String!
///   }
///
///   type Query {
///     user(id: ID!): User
///   }
/// "#;
///
/// let doc = TypeSystemDocument::<&str>::parse_str(schema)?;
/// ```
#[cfg(feature = "graphql")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphql")))]
pub mod graphql;

/// Parser combinators for GraphQLx (extended GraphQL with generics, imports, etc.).
///
/// GraphQLx is an extended GraphQL dialect that adds powerful type system features
/// inspired by modern programming languages. It serves as a **demonstration** of how
/// to build custom GraphQL-like DSLs using Smear's scaffold architecture.
///
/// ## Extended Features
///
/// - **Imports**: Module system for schema composition
///   ```graphqlx
///   import { User, Post } from "./types.graphqlx"
///   import * as models from "./models.graphqlx"
///   ```
///
/// - **Generics**: Parameterized types with type parameters
///   ```graphqlx
///   type Container<T> {
///     value: T
///     count: Int
///   }
///   ```
///
/// - **Where Clauses**: Constraints on generic types
///   ```graphqlx
///   type Repository<T> where T: Node {
///     items: [T!]!
///   }
///   ```
///
/// - **Map Types**: Key-value collections with type safety
///   ```graphqlx
///   input Config {
///     settings: <String! => String!>!
///   }
///   ```
///
/// - **Set Types**: Unique collections of elements
///   ```graphqlx
///   input Friends {
///     ids: <String!>!
///   }
///   ```
///
/// - **Path Types**: Namespaced type references
///   ```graphqlx
///   type Query {
///     user: user::Profile
///     admin: ::admin::Account  # Fully qualified
///   }
///   ```
///
/// ## Example
///
/// ```rust,ignore
/// use smear_parser::graphqlx::ast::{TypeSystemDocument, ParseStr};
///
/// let schema = r#"
///   import { Node } from "./interfaces.graphqlx"
///
///   type Container<T> where T: Node {
///     items: [T!]!
///     count: Int!
///   }
/// "#;
///
/// let doc = TypeSystemDocument::<&str>::parse_str(schema)?;
/// ```
///
/// ## Note
///
/// GraphQLx requires the `unstable` feature flag as the syntax is still experimental.
#[cfg(feature = "graphqlx")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphqlx")))]
pub mod graphqlx;

// Scaffold internals (not public API, use via scaffold module)
mod scaffold_error;
mod ast_scaffold;

/// Common error types and traits for parser implementations.
pub mod error;

// /// Hint types for parsers.
// pub mod hints;

/// Syntax type definitions for error reporting
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod syntax;

/// Common value parsers shared between GraphQL and GraphQLx.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod value;

/// Common identifier parsers.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod ident;
