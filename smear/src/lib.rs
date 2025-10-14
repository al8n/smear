#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Error types and traits for GraphQL parsing.
///
/// This module provides comprehensive error handling for lexing and parsing operations,
/// including span information for precise error reporting.
///
/// ## Key Types
///
/// - [`UnexpectedToken`](error::UnexpectedToken): Represents an unexpected token error with span information
/// - Error traits for implementing custom error types
pub mod error;

/// Hint system for expected GraphQL tokens.
///
/// Provides hints about what tokens were expected during parsing, useful for
/// generating helpful error messages and IDE integration.
pub mod hints;

/// Punctuation tokens used in GraphQL and GraphQLX.
///
/// Defines all punctuation marks recognized by the lexer, including:
/// - Delimiters: `()`, `{}`, `[]`
/// - Operators: `!`, `=`, `@`, `$`, `|`, `&`
/// - Separators: `:`, `,`
/// - GraphQLX-specific: `::`, `=>`, `<`, `>`
pub mod punctuator;

/// Keyword tokens for GraphQL and GraphQLX.
///
/// Contains all reserved keywords recognized by the lexer:
/// - GraphQL: `query`, `mutation`, `subscription`, `fragment`, `type`, `interface`, etc.
/// - GraphQLX: `import`, `from`, `as`, `where`, `map`, `set`
pub mod keywords;

/// Scaffold structures for building GraphQL-like DSLs.
///
/// The scaffold module provides **generic, reusable AST node definitions** that serve as
/// building blocks for creating custom GraphQL-like Schema Definition Languages.
///
/// ## Design Philosophy
///
/// The scaffold layer is designed to be parametric over concrete types, allowing:
/// - Standard GraphQL to use simple names and types
/// - GraphQLX to add generics, imports, and path types
/// - **Custom DSLs to plug in their own extensions**
///
/// ## Key Components
///
/// - [`Document`](scaffold::Document): Generic document container
/// - [`TypeDefinition`](scaffold::TypeDefinition): Generic type definition scaffold
/// - [`ExecutableDefinition`](scaffold::ExecutableDefinition): Operations and fragments
/// - [`FieldDefinition`](scaffold::FieldDefinition), [`ArgumentsDefinition`](scaffold::ArgumentsDefinition): Field and argument scaffolds
///
/// ## Building Custom DSLs
///
/// The scaffold architecture enables you to create your own GraphQL-like languages:
///
/// ```rust,ignore
/// use smear::scaffold::{self, Parseable};
///
/// // Define custom definition types
/// #[derive(Debug)]
/// enum MyDefinition {
///   Standard(scaffold::TypeDefinition</* ... */>),
///   Custom(MyCustomNode),
/// }
///
/// // Implement Parseable to define parsing logic
/// impl Parseable for MyDefinition {
///   fn parser<E>() -> impl Parser</* ... */> {
///     // Your custom parser combinators
///   }
/// }
///
/// // Use standard Document with your custom definitions
/// type MyDocument = scaffold::Document<MyDefinition>;
/// ```
pub mod scaffold;

/// Lexers for GraphQL and GraphQL-like DSLs.
///
/// The lexer module converts source text into zero-copy tokens. It provides lexers for
/// both standard GraphQL and the extended GraphQLX dialect.
///
/// # Features
///
/// - **Zero-copy tokenization**: All tokens reference the original source
/// - **Generic source types**: Works with `&str`, `&[u8]`, `bytes::Bytes`, and more
/// - **Error recovery**: Can continue lexing after errors
///
/// # Modules
///
/// - [`graphql`](lexer::graphql): Standard GraphQL lexer (draft specification)
/// - [`graphqlx`](lexer::graphqlx): Extended GraphQLX lexer with generics, imports, etc.
///
/// # Example
///
/// ```rust,ignore
/// use smear::lexer::graphql::ast::AstToken;
/// use logosky::TokenStream;
///
/// let source = "query { user { id name } }";
/// let tokens = TokenStream::<AstToken<&str>>::new(source);
/// // Process tokens...
/// ```
pub mod lexer;

/// Parsers for GraphQL and GraphQL-like DSLs.
///
/// The parser module provides combinators for building Abstract Syntax Trees from token streams.
/// It implements parsers for both standard GraphQL and the extended GraphQLX dialect.
///
/// ## Architecture
///
/// The parser uses the **logosky** parser combinator library (built on chumsky) to compose
/// reusable parsing components into complete parsers.
///
/// ## Parsing Traits
///
/// - [`ParseStr`]: Parse from `&str` sources
/// - [`ParseBytesSlice`]: Parse from `&[u8]` sources
/// - [`ParseBytes`]: Parse from `bytes::Bytes` sources (requires `bytes` feature)
///
/// ## Modules
///
/// - [`graphql`](parser::graphql): Standard GraphQL parser (draft specification)
///   - Supports all type system definitions (scalar, object, interface, union, enum, input)
///   - Executable documents (queries, mutations, subscriptions, fragments)
///   - Type extensions and schema definitions
///
/// - [`graphqlx`](parser::graphqlx): Extended GraphQLX parser
///   - Everything from GraphQL, plus:
///   - Import statements (`import { Type } from "./file"`)
///   - Generics (`type Container<T>`)
///   - Where clauses (`where T: Interface`)
///   - Map types (`<Key => Value>`)
///   - Set types (`<Element>`)
///   - Path types (`namespace::Type`)
pub mod parser;

#[doc(hidden)]
pub mod __private {
  pub use logosky::{self, chumsky};
}
