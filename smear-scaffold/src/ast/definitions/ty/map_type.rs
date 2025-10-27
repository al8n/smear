use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use smear_lexer::punctuator::{Bang, FatArrow, LAngle, RAngle};

/// Represents a GraphQLx map type with optional non-null modifier.
///
/// Map types represent a collection of key-value pairs in GraphQLx. They wrap
/// another type (the element type) to indicate that fields of this type return
/// multiple values of the wrapped type.
///
/// Map types support complex nullability semantics:
/// - The map itself can be null or non-null
/// - The key and value within the map can be null or non-null
/// - These nullability rules are independent and composable
///
/// ## Grammar
/// ```text
/// MapType : < Key !? => Value !? > !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct MapType<Key, Value> {
  span: Span,
  key: Key,
  value: Value,
  required: bool,
}

impl<Key, Value> AsSpan<Span> for MapType<Key, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value> IntoSpan<Span> for MapType<Key, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value> IntoComponents for MapType<Key, Value> {
  type Components = (Span, Key, Value, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.value, self.required)
  }
}

impl<Key, Value> MapType<Key, Value> {
  /// Creates a new map type instance.
  #[inline]
  pub const fn new(span: Span, key: Key, value: Value, required: bool) -> Self {
    Self {
      span,
      key,
      value,
      required,
    }
  }

  /// Returns a reference to the span covering the entire map type.
  ///
  /// The span includes the brackets, element type, and optional bang modifier.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the key type contained within the map.
  ///
  /// This is the type of individual elements in the map. It can be any
  /// valid GraphQLx type including named types, other map types, or even
  /// nested map types for multi-dimensional arrays.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns a reference to the value type contained within the map.
  ///
  /// This is the type of individual elements in the map. It can be any
  /// valid GraphQLx type including named types, other map types, or even
  /// nested map types for multi-dimensional arrays.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns whether the map type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }

  /// Creates a parser for map types using the provided element type parser.
  ///
  /// This parser handles the complete map type syntax including brackets,
  /// element type parsing, and optional bang modifier. The element type
  /// parsing is delegated to the provided parser for flexibility.
  ///
  /// ## Parameters
  /// - `parser`: Parser for the element type within the map
  ///
  /// ## Grammar Handled
  /// ```text
  /// MapType : < Key!? => Value !? > !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// <String => Int>        # Nullable map of nullable strings
  /// <String => Int!>!      # Non-null map of non-null strings
  /// <<ID! => User>>               # Nested map type
  /// <ID! => User>                  # Nullable map of non-null IDs
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, KP, VP>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    FatArrow: Parseable<'a, I, T, Error> + 'a,
    KP: Parser<'a, I, Key, E> + Clone,
    VP: Parser<'a, I, Value, E> + Clone,
  {
    LAngle::parser()
      .ignore_then(
        key_parser
          .then_ignore(FatArrow::parser())
          .then(value_parser),
      )
      .then_ignore(RAngle::parser())
      .then(Bang::parser().or_not())
      .map_with(|((key, value), bang), exa| Self {
        span: exa.span(),
        key,
        value,
        required: bang.is_some(),
      })
  }
}

impl<'a, Key, Value, I, T, Error> Parseable<'a, I, T, Error> for MapType<Key, Value>
where
  Key: Parseable<'a, I, T, Error>,
  Value: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  FatArrow: Parseable<'a, I, T, Error> + 'a,
  Bang: Parseable<'a, I, T, Error> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Key::parser(), Value::parser())
  }
}
