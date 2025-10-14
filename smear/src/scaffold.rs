//! Scaffold structures for building GraphQL-like DSLs.
//!
//! The scaffold module provides the foundation for creating custom GraphQL-like Schema Definition Languages.
//! It offers **generic, reusable AST node definitions** that can be composed and extended to build
//! domain-specific languages with GraphQL-like syntax.
//!
//! # Architecture
//!
//! The scaffold follows a **parametric design** where AST nodes are generic over their component types.
//! This allows:
//!
//! - **Standard GraphQL** to use simple types (strings for names, basic types)
//! - **GraphQLX** to use extended types (generics, imports, path types)
//! - **Your Custom DSL** to plug in your own types and extensions
//!
//! # Core Concepts
//!
//! ## Documents
//!
//! [`Document<Definition>`](definitions::Document) is the top-level container for GraphQL documents:
//!
//! ```rust,ignore
//! pub struct Document<Definition, Definitions = Vec<Definition>> {
//!   definitions: Definitions,
//! }
//! ```
//!
//! ## Definition Types
//!
//! The scaffold provides generic definition enums:
//!
//! - [`TypeSystemDefinition`](definitions::TypeSystemDefinition): Schema, types, directives
//! - [`ExecutableDefinition`](definitions::ExecutableDefinition): Operations, fragments
//! - [`TypeDefinition`](definitions::TypeDefinition): Scalar, object, interface, union, enum, input
//!
//! ## Field and Argument Scaffolds
//!
//! - [`FieldDefinition`](definitions::FieldDefinition): Generic field definition with arguments
//! - [`ArgumentsDefinition`](definitions::ArgumentsDefinition): Argument lists
//! - [`InputValueDefinition`](definitions::InputValueDefinition): Input fields and arguments
//!
//! ## Type Scaffolds
//!
//! - [`NamedType`](definitions::NamedType): Simple type reference
//! - [`ListType`](definitions::ListType): Array types
//! - [`Type`](definitions::Type): Union of named, list, and non-null types
//!
//! # Building a Custom DSL
//!
//! Here's how to create your own GraphQL-like language:
//!
//! ```rust,ignore
//! use smear::scaffold::{self, Parseable};
//!
//! // 1. Define your custom AST node
//! #[derive(Debug)]
//! pub struct CustomDirective {
//!   name: String,
//!   custom_field: MyCustomType,
//! }
//!
//! // 2. Create a definition enum combining standard + custom
//! #[derive(Debug)]
//! pub enum MyDefinition {
//!   Standard(scaffold::TypeDefinition</* ... */>),
//!   CustomDirective(CustomDirective),
//! }
//!
//! // 3. Implement Parseable to define parsing logic
//! impl Parseable for MyDefinition {
//!   fn parser<E>() -> impl Parser<Token = MyToken, Output = Self, Error = E> {
//!     // Use parser combinators to parse your syntax
//!     choice((
//!       scaffold::TypeDefinition::parser().map(MyDefinition::Standard),
//!       custom_directive_parser().map(MyDefinition::CustomDirective),
//!     ))
//!   }
//! }
//!
//! // 4. Use Document with your custom definitions
//! pub type MyDocument = scaffold::Document<MyDefinition>;
//! ```
//!
//! # GraphQLX Example
//!
//! GraphQLX demonstrates this pattern by extending GraphQL with:
//!
//! - Import definitions (`ImportDefinition`)
//! - Generic type parameters (`TypeGenerics`)
//! - Where clauses (`WhereClause`)
//! - Set and Map types (`SetType`, `MapType`, `MapEntry`, `Map`, `Set`)
//! - Path types (`Path`)
//!
//! All built using the scaffold architecture!
//!
//! # Reusable Components
//!
//! The scaffold also provides reusable helper types:
//!
//! - [`And<A, B>`](and::And): Combine two parseable elements
//! - Language constructs for selections, directives, variables
//!
//! # Design Principles
//!
//! 1. **Parametric Polymorphism**: Types are generic over their components
//! 2. **Composition over Inheritance**: Combine small pieces into larger structures
//! 3. **Zero-Copy**: All scaffolds work with borrowed data where possible
//! 4. **Extensibility**: Easy to add new node types without modifying the scaffold

pub use and::*;
pub use definitions::*;
pub use lang::*;

mod and;
mod definitions;
mod lang;
