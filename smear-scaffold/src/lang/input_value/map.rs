use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser as _, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::error::UnclosedBraceError;

use smear_lexer::{keywords, punctuator::{FatArrow, LBrace, RBrace}};

use core::marker::PhantomData;
use std::vec::Vec;

/// A single entry in a GraphQLx map literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapEntry<Key, Value> {
  span: Span,
  key: Key,
  value: Value,
}

impl<Key, Value> AsSpan<Span> for MapEntry<Key, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value> IntoSpan<Span> for MapEntry<Key, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value> IntoComponents for MapEntry<Key, Value> {
  type Components = (Span, Key, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.value)
  }
}

impl<Key, Value> MapEntry<Key, Value> {
  /// Creates a new map entry with the given key and value.
  #[inline]
  const fn new(span: Span, key: Key, value: Value) -> Self {
    Self { span, key, value }
  }

  /// Returns the span of the map entry.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the key of the map entry.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns the value of the map entry.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for a single map entry using the provided key and value parsers.
  pub fn parser_with<'src, I, T, Error, KP, VP, E>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnclosedBraceError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    FatArrow: Parseable<'src, I, T, Error> + 'src,
    KP: Parser<'src, I, Key, E> + Clone + 'src,
    VP: Parser<'src, I, Value, E> + Clone + 'src,
  {
    key_parser
      .then_ignore(FatArrow::parser())
      .then(value_parser)
      .map_with(|(key, value), exa| Self::new(exa.span(), key, value))
  }
}

impl<'a, Key, Value, I, T, Error> Parseable<'a, I, T, Error> for MapEntry<Key, Value>
where
  Key: Parseable<'a, I, T, Error> + 'a,
  Value: Parseable<'a, I, T, Error> + 'a,
  FatArrow: Parseable<'a, I, T, Error> + 'a,
  Error: UnclosedBraceError,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Key::parser(), Value::parser())
  }
}

/// A GraphQLx map literal value.
///
/// Represents a complete map literal as defined by the GraphQLx specification.
/// Map literals are ordered collections of values enclosed in square brackets,
/// supporting any valid GraphQLx values including nested maps and objects.
///
/// ## Specification Rules
///
/// GraphQLx map literals follow these formatting rules:
/// - **Bracket delimiters**: Must be enclosed in `[` and `]`
/// - **Value separation**: Elements separated by whitespace (commas optional but conventional)
/// - **Trailing commas**: Allowed after the last element
/// - **Nested values**: Can contain any valid GraphQLx input values
/// - **Empty maps**: `[]` is a valid empty map
/// - **Whitespace handling**: Flexible whitespace and comments between elements
///
/// ## Grammar
///
/// ```text
/// Map ::= 'map' '{' Entries? '}'
/// Entries ::= MapEntry+
///
/// MapEntry ::= Key '=>' Value
/// ```
///
/// ## Generic Parameters
/// - `Key`: The type of keys in the map
/// - `Value`: The type of values contained in the map
/// - `Container`: The collection type (defaults to `Vec<MapEntry<Key, Value>>`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<MapEntry<Key, Value>>` (default): Standard dynamic array
/// - Any type implementing `chumsky::container::Container<MapEntry<Key, Value>>`
///
/// ## Component Structure
///
/// Each map literal contains:
/// - **Overall span**: Covers the entire map including brackets
/// - **Left bracket**: The opening `[` token with its position
/// - **Right bracket**: The closing `]` token with its position  
/// - **Values**: The parsed elements in their container
#[derive(Debug, Clone)]
pub struct Map<Key, Value, Container = Vec<MapEntry<Key, Value>>> {
  span: Span,
  entries: Container,
  _m: PhantomData<MapEntry<Key, Value>>,
}

impl<Key, Value, Container> Map<Key, Value, Container> {
  /// Creates a new map literal with the given span and entries.
  #[inline]
  pub(crate) const fn new(span: Span, entries: Container) -> Self {
    Self {
      span,
      entries,
      _m: PhantomData,
    }
  }

  /// Returns the span covering the entire map literal.
  ///
  /// This span includes the opening and closing brackets as well as all contained values.
  /// It is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the parsed map elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the map literal.
  #[inline]
  pub const fn entries(&self) -> &Container {
    &self.entries
  }

  /// Returns the entries as a slice, if the container supports it.
  #[inline]
  pub fn entries_slice(&self) -> &[MapEntry<Key, Value>]
  where
    Container: AsRef<[MapEntry<Key, Value>]>,
  {
    self.entries().as_ref()
  }

  /// Creates a parser for GraphQLx map literals with customizable value parsing.
  ///
  /// This parser handles the complete map syntax including brackets and
  /// enforces proper structure. It uses the provided `value_parser` to parse
  /// each individual element within the map.
  ///
  /// ## Error Handling
  ///
  /// If the closing bracket is missing, the parser invokes the provided
  /// `on_missing_rbracket` function to generate a custom error message.
  /// This allows for context-specific error reporting.
  pub fn parser_with<'src, I, T, Error, KP, VP, E>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnclosedBraceError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    KP: Parser<'src, I, Key, E> + Clone + 'src,
    VP: Parser<'src, I, Value, E> + Clone + 'src,
    Container: chumsky::container::Container<MapEntry<Key, Value>>,
    FatArrow: Parseable<'src, I, T, Error>,
    keywords::Map: Parseable<'src, I, T, Error>,
    LBrace: Parseable<'src, I, T, Error>,
    RBrace: Parseable<'src, I, T, Error>,
  {
    keywords::Map::parser()
      .then(LBrace::parser())
      .ignore_then(
        MapEntry::parser_with(key_parser, value_parser)
          .repeated()
          .collect(),
      )
      .then(RBrace::parser().or_not())
      .try_map(move |(entries, r), span| match r {
        Some(_) => Ok(Map::new(span, entries)),
        None => Err(Error::unclosed_brace(span)),
      })
  }
}

impl<Key, Value, Container> AsSpan<Span> for Map<Key, Value, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Container> IntoSpan<Span> for Map<Key, Value, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, Container> IntoComponents for Map<Key, Value, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.entries)
  }
}

impl<'a, Key, Value, Container, I, T, Error> Parseable<'a, I, T, Error>
  for Map<Key, Value, Container>
where
  Error: UnclosedBraceError + 'a,
  Key: Parseable<'a, I, T, Error> + 'a,
  Value: Parseable<'a, I, T, Error> + 'a,
  Container: chumsky::container::Container<MapEntry<Key, Value>> + 'a,
  LBrace: Parseable<'a, I, T, Error> + 'a,
  RBrace: Parseable<'a, I, T, Error> + 'a,
  FatArrow: Parseable<'a, I, T, Error> + 'a,
  smear_lexer::keywords::Map: Parseable<'a, I, T, Error> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Key::parser(), Value::parser())
  }
}
