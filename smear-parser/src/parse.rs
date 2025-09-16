use smear_utils::{Char, FromMapExtra, Slice, Source};

use chumsky::{extra::ParserExtra, prelude::Parser};

use std::vec::Vec;

/// Core trait for types that can be parsed from input tokens.
///
/// This trait defines the fundamental parsing capability for syntax tree nodes.
/// Types implementing this trait can create parsers that transform input tokens
/// into structured AST nodes, forming the foundation of the parsing system.
///
/// The trait is designed to work with chumsky's parser combinator framework,
/// providing type-safe, composable parsing with comprehensive error handling.
pub trait Parsable<S>: Sized {
  /// Creates a parser for this syntax tree node.
  ///
  /// This method returns a parser that can transform input tokens into an
  /// instance of the implementing type. The parser integrates with chumsky's
  /// combinator system and supports span tracking for error reporting.
  ///
  /// ## Type Parameters
  ///
  /// - `'src`: Lifetime of the source input being parsed
  /// - `S`: Span type for tracking source locations (must implement `FromMapExtra`)
  /// - `I`: Input source type (string, byte slice, etc.)
  /// - `E`: Parser extra data type for error handling and context
  ///
  /// ## Implementation Guidelines
  ///
  /// - Use `map_with` to capture span information for source location tracking
  /// - Leverage `recursive` for self-referential or mutually recursive structures
  /// - Combine smaller parsers using `choice`, `then`, and other combinators
  /// - Handle whitespace and comments using appropriate padding/skipping
  /// - Provide meaningful error messages through parser configuration
  fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I> + 'src,
    S: FromMapExtra<'src, I, E>;
}

/// Extension trait providing additional parsing utilities.
///
/// This trait automatically extends all `Parsable` types with convenient
/// methods for common parsing patterns, particularly handling whitespace
/// and comments that should be ignored during parsing.
///
/// ## Automatic Implementation
///
/// This trait is automatically implemented for all types that implement
/// `Parsable`, providing a consistent interface across the parsing system.
pub trait ParsableExt<S>: Parsable<S> {
  /// Creates a padded parser that ignores whitespace and comments.
  ///
  /// This method wraps the basic parser with padding that automatically
  /// handles ignored tokens (whitespace, comments, etc.) around the
  /// parsed content. This is essential for parsing languages like GraphQL
  /// where whitespace is generally not significant.
  ///
  /// ## Usage
  ///
  /// Use this method when parsing in contexts where the parsed element
  /// might be surrounded by whitespace or comments that should be ignored:
  ///
  /// ```ignore
  /// // Parse a field definition that might have surrounding whitespace
  /// let field_parser = FieldDefinition::padded_parser::<Span, _, _>();
  ///
  /// // Use in sequence where elements are separated by whitespace
  /// let fields_parser = FieldDefinition::padded_parser()
  ///     .repeated()
  ///     .collect::<Vec<_>>();
  /// ```
  ///
  /// ## Implementation Detail
  ///
  /// The padding uses [`ignored()`](super::lang::ignored) which typically handles:
  /// - Unicode whitespace characters (spaces, tabs, newlines)
  /// - Single-line comments (`# comment`)
  /// - Multi-line comments (if supported by the language)
  /// - Comma separators (in languages where they're optional)
  fn padded_parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I> + 'src,
    S: FromMapExtra<'src, I, E>,
  {
    <Self as Parsable<S>>::parser::<I, E>().padded_by(super::lang::ignored())
  }
}

impl<S, T> ParsableExt<S> for T where T: Parsable<S> {}

/// Trait for parsing string input into syntax tree nodes.
///
/// This trait provides convenient methods for parsing string literals into
/// AST nodes with various configuration options. It automatically implements
/// for all `Parsable` types, providing a consistent interface for string-based parsing.
///
/// ## Examples
///
/// ```ignore
/// use chumsky::extra::Err;
/// use chumsky::error::Rich;
///
/// type ParseError<'a> = Rich<'a, char>;
/// type Span = std::ops::Range<usize>;
///
/// // Basic parsing
/// let result = FieldDefinition::parse_str::<Span, Err<ParseError>>("name: String!");
///
/// // Padded parsing (handles whitespace)
/// let result = FieldDefinition::parse_str_padded::<Span, Err<ParseError>>(
///     "  name: String!  # with comment"
/// );
///
/// // With source tracking
/// let result = FieldDefinition::parse_str_with_source::<Span, Err<ParseError>>(
///     "name: String!"
/// );
/// ```
pub trait ParseStr<'src, S>: Parsable<S> {
  /// Parse a string directly without any padding or special handling.
  ///
  /// This method provides the most basic string parsing functionality.
  /// Use when the input is expected to contain only the target syntax
  /// without surrounding whitespace or comments.
  fn parse_str<E>(input: &'src str) -> Result<Self, Vec<E::Error>>
  where
    E: ParserExtra<'src, &'src str> + 'src,
    E::Context: Default,
    E::State: Default,
    S: FromMapExtra<'src, &'src str, E>,
  {
    Self::parser::<&'src str, E>().parse(input).into_result()
  }

  /// Parse a string with automatic whitespace and comment handling.
  ///
  /// This method wraps the parser with padding that ignores whitespace
  /// and comments around the target syntax. Use for parsing in contexts
  /// where the input might contain formatting.
  fn parse_str_padded<E>(input: &'src str) -> Result<Self, Vec<E::Error>>
  where
    E: ParserExtra<'src, &'src str> + 'src,
    E::Context: Default,
    E::State: Default,
    S: FromMapExtra<'src, &'src str, E>,
  {
    Self::padded_parser::<&'src str, E>()
      .parse(input)
      .into_result()
  }
}

impl<'src, S, T> ParseStr<'src, S> for T where T: Parsable<S> {}

/// Trait for parsing byte slice input into syntax tree nodes.
///
/// This trait mirrors `ParseStr` but works with byte slices instead of strings.
/// It's useful for parsing binary formats, UTF-8 encoded data, or when working
/// with data that hasn't been validated as UTF-8.
///
/// ## Examples
///
/// ```ignore
/// let graphql_bytes = b"type User { id: ID! name: String! }";
/// let result = TypeDefinition::parse_slice::<Span, Err<ParseError>>(graphql_bytes);
///
/// // With padding for formatted input
/// let formatted_bytes = b"  type User { id: ID! }  ";
/// let result = TypeDefinition::parse_slice_padded::<Span, Err<ParseError>>(formatted_bytes);
/// ```
pub trait ParseSlice<'src, S>: Parsable<S> {
  /// Parse a byte slice directly without padding.
  fn parse_slice<E>(input: &'src [u8]) -> Result<Self, Vec<E::Error>>
  where
    E: ParserExtra<'src, &'src [u8]> + 'src,
    E::Context: Default,
    E::State: Default,
    S: FromMapExtra<'src, &'src [u8], E>,
  {
    Self::parser::<&'src [u8], E>().parse(input).into_result()
  }

  /// Parse a byte slice with whitespace and comment handling.
  fn parse_slice_padded<E>(input: &'src [u8]) -> Result<Self, Vec<E::Error>>
  where
    E: ParserExtra<'src, &'src [u8]> + 'src,
    E::Context: Default,
    E::State: Default,
    S: FromMapExtra<'src, &'src [u8], E>,
  {
    Self::padded_parser::<&'src [u8], E>()
      .parse(input)
      .into_result()
  }
}

impl<'src, S, T> ParseSlice<'src, S> for T where T: Parsable<S> {}

#[cfg(feature = "bytes")]
#[cfg_attr(docsrs, doc(cfg(feature = "bytes")))]
pub use _bytes::ParseBytes;

#[cfg(feature = "bytes")]
mod _bytes {
  use super::*;
  use bytes::Bytes;

  /// Trait for parsing `Bytes` input into syntax tree nodes.
  ///
  /// This trait provides parsing capabilities for the `bytes::Bytes` type,
  /// which is commonly used in asynchronous and network programming contexts.
  /// `Bytes` provides efficient sharing of byte data without copying.
  ///
  /// ## Use Cases
  ///
  /// - Parsing GraphQL queries received over HTTP/WebSocket connections
  /// - Processing GraphQL in microservice architectures
  /// - Working with streaming or chunked GraphQL data
  /// - High-performance scenarios requiring minimal allocations
  ///
  /// ## Examples
  ///
  /// ```ignore
  /// use bytes::Bytes;
  ///
  /// let query_bytes = Bytes::from("{ user(id: \"123\") { name } }");
  /// let operation = OperationDefinition::parse_bytes::<Span, Err<ParseError>>(query_bytes)?;
  ///
  /// // With padding for network data that might have formatting
  /// let formatted_query = Bytes::from("  { user { name } }  ");
  /// let operation = OperationDefinition::parse_bytes_padded::<Span, Err<ParseError>>(
  ///     formatted_query
  /// )?;
  /// ```
  pub trait ParseBytes<S>: Parsable<S> {
    /// Parse `Bytes` directly without any padding or special handling.
    fn parse_bytes<'a, E>(input: Bytes) -> Result<Self, Vec<E::Error>>
    where
      E: ParserExtra<'a, Bytes> + 'a,
      E::Context: Default,
      E::State: Default,
      S: FromMapExtra<'a, Bytes, E>,
    {
      Self::parser::<Bytes, E>().parse(input).into_result()
    }

    /// Parse `Bytes` with automatic whitespace and comment handling.
    fn parse_bytes_padded<'a, E>(input: Bytes) -> Result<Self, Vec<E::Error>>
    where
      E: ParserExtra<'a, Bytes> + 'a,
      E::Context: Default,
      E::State: Default,
      S: FromMapExtra<'a, Bytes, E>,
    {
      Self::padded_parser::<Bytes, E>().parse(input).into_result()
    }
  }

  impl<S, T> ParseBytes<S> for T where T: Parsable<S> {}
}
