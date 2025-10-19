#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Hint system for expected GraphQL tokens.
///
/// Provides hints about what tokens were expected during parsing, useful for
/// generating helpful error messages and IDE integration.
pub mod hints;

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
pub use smear_scaffold as scaffold;

/// Lexers for GraphQL and GraphQL-like DSLs.
///
/// The lexer module converts source text into zero-copy tokens. It provides lexers for
/// both standard GraphQL and the extended GraphQLX dialect.
///
/// # Token Streams
///
/// Smear provides **two complementary token types** for different use cases:
///
/// ## SyntacticToken (Fast)
///
/// - **Skips trivia**: Automatically filters out whitespace, comments, and commas
/// - **Use for**: GraphQL servers, query execution, performance-critical parsing
/// - **Benefits**: Minimal memory footprint, maximum speed
///
/// ## LosslessToken (Complete)
///
/// - **Preserves trivia**: Includes all whitespace, comments, and formatting
/// - **Use for**: Code formatters, linters, IDEs, syntax highlighters
/// - **Benefits**: Perfect source reconstruction, access to all source information
///
/// # Features
///
/// - **Zero-copy tokenization**: All tokens reference the original source
/// - **Generic source types**: Works with `&str`, `&[u8]`, `bytes::Bytes`, `hipstr::{HipStr, HipByt}`, and more
/// - **Error recovery**: Can continue lexing after errors
/// - **Thread-safe**: Tokens are `Send + Sync` when using thread-safe source types
///
/// # Modules
///
/// - [`graphql`](lexer::graphql): Standard GraphQL lexer (draft specification)
///   - [`graphql::syntactic`](lexer::graphql::syntactic): Fast tokens (skips trivia)
///   - [`graphql::lossless`](lexer::graphql::lossless): Complete tokens (preserves all formatting)
///
/// - [`graphqlx`](lexer::graphqlx): Extended GraphQLX lexer with generics, imports, etc.
///   - [`graphqlx::syntactic`](lexer::graphqlx::syntactic): Fast tokens (skips trivia)
///   - [`graphqlx::lossless`](lexer::graphqlx::lossless): Complete tokens (preserves all formatting)
///
/// # Examples
///
/// ## Fast tokenization with SyntacticToken
///
/// ```rust,ignore
/// use smear::lexer::graphql::syntactic::SyntacticToken;
/// use logosky::TokenStream;
///
/// let source = "query { user { id name } }";
/// let tokens = TokenStream::<SyntacticToken<&str>>::new(source);
/// // Only syntactically significant tokens (whitespace automatically skipped)
/// ```
///
/// ## Complete tokenization with LosslessToken
///
/// ```rust,ignore
/// use smear::lexer::graphql::lossless::LosslessToken;
/// use logosky::TokenStream;
///
/// let source = "query { # comment\n  user { id }\n}";
/// let tokens = TokenStream::<LosslessToken<&str>>::new(source);
/// // ALL tokens including spaces, comments, exact formatting
/// ```
pub use smear_lexer as lexer;

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
pub use smear_parser as parser;

#[doc(hidden)]
pub mod __private {
  pub use logosky::{self, chumsky};
}
