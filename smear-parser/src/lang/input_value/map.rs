use chumsky::{container::Container, extra::ParserExtra, prelude::*};

use crate::{
  lang::{
    ignored,
    punct::{Colon, LAngle, RAngle},
  },
  source::*,
};

use core::marker::PhantomData;

/// A single key-value entry within a GraphQL map literal.
///
/// Represents a key-value pair within a map literal, following a custom
/// syntax for structured data representation. Each entry consists of a
/// key, a colon separator, and a value, with optional whitespace and
/// comments allowed around each component.
///
/// ## Grammar
///
/// ```text
/// MapEntry ::= Key ':' Value
/// ```
#[derive(Debug, Clone, Copy)]
pub struct MapEntry<Key, Value, Span> {
  span: Span,
  key: Key,
  colon: Colon<Span>,
  value: Value,
}

impl<Key, Value, Span> AsRef<Span> for MapEntry<Key, Value, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Span> IntoSpan<Span> for MapEntry<Key, Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, Span> IntoComponents for MapEntry<Key, Value, Span> {
  type Components = (Span, Key, Colon<Span>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.colon, self.value)
  }
}

impl<Key, Value, Span> MapEntry<Key, Value, Span> {
  /// Returns the source span of the entire map entry.
  ///
  /// This span covers from the first character of the key through
  /// the last character of the value, providing the complete source
  /// location for error reporting and source mapping.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the colon separator token.
  ///
  /// This provides access to the `:` character that separates the key
  /// from its value, including its exact source position. Useful for
  /// syntax highlighting and precise error reporting.
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }
  /// Returns the entry key.
  ///
  /// This provides access to the key component that identifies this
  /// entry within the map. The key can be any valid type depending
  /// on the specific map syntax being parsed.
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns the entry value.
  ///
  /// This provides access to the value assigned to this key. The value
  /// can be any valid type including scalars, collections, or nested
  /// structures depending on the parsing context.
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for map entries with custom key and value parsers.
  ///
  /// This parser handles the complete map entry syntax including the
  /// key, colon separator, and value. It manages whitespace around the
  /// colon according to the map's formatting rules.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the map entry.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, KP, VP>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    KP: Parser<'src, I, Key, E> + Clone,
    VP: Parser<'src, I, Value, E> + Clone,
  {
    key_parser
      .then(Colon::parser().padded_by(ignored()))
      .then(value_parser)
      .map_with(|((key, colon), value), sp| Self {
        key,
        colon,
        value,
        span: Span::from_map_extra(sp),
      })
  }
}

/// A map literal with angle bracket delimiters.
///
/// Represents a complete map literal using angle bracket syntax (`< >`). Map
/// literals are collections of key-value pairs that provide structured data
/// representation with a different syntax from standard object notation.
///
/// ## Specification Rules
///
/// Map literals follow these formatting rules:
/// - **Angle bracket delimiters**: Must be enclosed in `<` and `>`
/// - **Entry format**: Each entry follows `key: value` syntax
/// - **Entry separation**: Entries separated by whitespace (commas optional)
/// - **Empty map syntax**: Empty maps use `<:>` sentinel syntax
/// - **Flexible whitespace**: Whitespace and comments allowed throughout
///
/// ## Grammar
///
/// ```text
/// Map ::= '<' MapEntries? | ':' '>'
/// MapEntries ::= MapEntry+
/// MapEntry ::= Key ':' Value
/// ```
///
/// ## Examples
///
/// **Valid map literals:**
///
/// ```text
/// <:>                           // Empty map (sentinel syntax)
/// < "name": "John" >              // Single entry
/// < "name": "John", "age": "25" >     // Multiple entries with commas
/// < "name": "John", "age": "25" >      // Multiple entries with spaces
/// < "name": "John", "age": "25", >    // Trailing comma allowed
/// <
///   "name": "John",
///   "age": "25",
///   "active": "true"
/// > // Multi-line format
///
/// // Nested structures
/// <
///   "settings": < "theme": "dark", "debug": "false" >
/// >
/// ```
#[derive(Debug, Clone, Copy)]
pub struct Map<Key, Value, Span, C = Vec<MapEntry<Key, Value, Span>>> {
  span: Span,
  l_angle: LAngle<Span>,
  fields: C,
  r_angle: RAngle<Span>,
  _key: PhantomData<Key>,
  _value: PhantomData<Value>,
}

impl<Key, Value, Span, C> AsRef<Span> for Map<Key, Value, Span, C> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Span, C> IntoSpan<Span> for Map<Key, Value, Span, C> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, Span, C> IntoComponents for Map<Key, Value, Span, C> {
  type Components = (Span, LAngle<Span>, C, RAngle<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_angle, self.fields, self.r_angle)
  }
}

impl<Key, Value, Span, C> Map<Key, Value, Span, C> {
  /// Returns the source span of the entire map literal.
  ///
  /// This span covers from the opening angle bracket through the closing
  /// angle bracket, including all entries and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete map text.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the opening angle bracket token.
  ///
  /// This provides access to the `<` character that begins the map,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and precise error reporting at map boundaries.
  pub const fn l_angle(&self) -> &LAngle<Span> {
    &self.l_angle
  }
  /// Returns the container holding the map entries.
  ///
  /// This provides access to all key-value pairs that were successfully parsed
  /// from the map literal. The container type can be customized to optimize
  /// for different usage patterns and access requirements.
  pub const fn fields(&self) -> &C {
    &self.fields
  }
  /// Returns the closing angle bracket token.
  ///
  /// This provides access to the `>` character that ends the map,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and detecting incomplete maps.
  pub const fn r_angle(&self) -> &RAngle<Span> {
    &self.r_angle
  }

  /// Creates a parser for map literals with custom key and value parsers.
  ///
  /// This is the core parsing function that accepts key and value parsers and
  /// creates a complete map parser. It handles all map syntax including angle
  /// brackets, entry parsing, whitespace management, and the special empty
  /// map sentinel syntax.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the map.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, KP, VP>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    KP: Parser<'src, I, Key, E> + Clone,
    VP: Parser<'src, I, Value, E> + Clone,
    C: Container<MapEntry<Key, Value, Span>>,
  {
    LAngle::<Span>::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty sentinel: "<:>"
        Colon::<Span>::parser()
          .padded_by(ignored())
          .then(RAngle::<Span>::parser())
          .map(|_| C::default()),
        // Non-empty: one-or-more entries; commas live in `ws`
        MapEntry::<Key, Value, Span>::parser_with(key_parser, value_parser)
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect::<C>(),
      )))
      .then_ignore(ignored())
      .then(RAngle::parser())
      .map_with(|((l_angle, fields), r_angle), sp| Self {
        span: Span::from_map_extra(sp),
        l_angle,
        r_angle,
        fields,
        _key: PhantomData,
        _value: PhantomData,
      })
  }
}
