use chumsky::{IterParser as _, Parser, extra::ParserExtra};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};

use crate::{
  error::UnclosedObjectValueError,
  lang::punctuator::{Colon, LBrace, RBrace},
  source::{IntoComponents, IntoSpan},
};

use core::marker::PhantomData;

/// A single field within a GraphQL input object literal.
///
/// Represents a name-value pair within an object literal, following the
/// GraphQL specification for input object fields. Each field consists of
/// a field name, a colon separator, and a value, with optional whitespace
/// and comments allowed around each component.
///
/// ## Grammar
///
/// ```text
/// ObjectField ::= Name ':' Value
/// ```
///
/// ## Examples
///
/// ```text
/// name: "John"              // String field
/// age: 25                   // Integer field  
/// active: true              // Boolean field
/// tags: ["user", "admin"]   // List field
/// profile: { bio: "..." }   // Nested object field
/// settings: null            // Null field
/// ```
///
/// ## Component Structure
///
/// Each field contains:
/// - **Overall span**: Covers from field name through the value
/// - **Field name**: A GraphQL name identifier
/// - **Colon separator**: The `:` token with its position
/// - **Field value**: The value assigned to this field
#[derive(Debug, Clone, Copy)]
pub struct ObjectField<Name, InputValue> {
  span: Span,
  name: Name,
  value: InputValue,
}

impl<Name, InputValue> AsRef<Span> for ObjectField<Name, InputValue> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, InputValue> IntoSpan<Span> for ObjectField<Name, InputValue> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, InputValue> IntoComponents for ObjectField<Name, InputValue> {
  type Components = (Span, Name, InputValue);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.value)
  }
}

impl<Name, InputValue> ObjectField<Name, InputValue> {
  #[inline]
  pub(crate) const fn new(span: Span, name: Name, value: InputValue) -> Self {
    Self { span, name, value }
  }

  /// Returns the source span of the entire field.
  ///
  /// This span covers from the first character of the field name through
  /// the last character of the field value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the field name.
  ///
  /// This provides access to the GraphQL name that identifies this field
  /// within the object. The name follows standard GraphQL identifier rules
  /// and cannot be a reserved keyword.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the field value.
  ///
  /// This provides access to the value assigned to this field. The value
  /// can be any valid GraphQL input value type including scalars, enums,
  /// lists, nested objects, or null.
  #[inline]
  pub const fn value(&self) -> &InputValue {
    &self.value
  }

  /// Creates a parser for GraphQL list literals with customizable value parsing.
  ///
  /// This parser handles the complete list syntax including brackets and
  /// enforces proper structure. It uses the provided `value_parser` to parse
  /// each individual element within the list.
  ///
  /// ## Error Handling
  ///
  /// If the closing bracket is missing, the parser invokes the provided
  /// `on_missing_rbracket` function to generate a custom error message.
  /// This allows for context-specific error reporting.
  pub fn parser_with<'src, I, T, Error, NP, VP, E>(
    name_parser: NP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    VP: Parser<'src, I, InputValue, E> + Clone + 'src,
    NP: Parser<'src, I, Name, E> + Clone + 'src,
    Colon: Parseable<'src, I, T, Error>,
  {
    name_parser
      .then_ignore(Colon::parser())
      .then(value_parser)
      .map_with(|(name, value), exa| Self::new(exa.span(), name, value))
  }
}

impl<'a, Name, InputValue, I, T, Error> Parseable<'a, I, T, Error> for ObjectField<Name, InputValue>
where
  Name: Parseable<'a, I, T, Error>,
  InputValue: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Name::parser(), InputValue::parser())
  }
}

/// A GraphQL input object literal value.
///
/// Represents a complete input object literal as defined by the GraphQL
/// specification. Input objects are unordered collections of name-value pairs
/// enclosed in curly braces, providing structured input for GraphQL operations.
///
/// ## Specification Rules
///
/// GraphQL input object literals follow these formatting rules:
/// - **Brace delimiters**: Must be enclosed in `{` and `}`
/// - **Field format**: Each field follows `name: value` syntax
/// - **Field separation**: Fields separated by whitespace (commas optional but conventional)
/// - **Trailing commas**: Allowed after the last field
/// - **Unique names**: Field names should be unique within the object (semantic validation)
/// - **Flexible whitespace**: Whitespace and comments allowed throughout
///
/// ## Grammar
///
/// ```text
/// Object ::= '{' ObjectFields? '}'
/// ObjectFields ::= ObjectField+
/// ObjectField ::= Name ':' Value
/// ```
///
/// ## Constant Context Support
///
/// This type supports GraphQL's constant vs variable distinction through
/// compile-time validation. The parser can be configured to only accept
/// constant values (for default values and directive arguments) or to
/// allow variables (for query arguments and mutation inputs).
///
/// ## Examples
///
/// **Valid object literals:**
/// ```text
/// {}                          // Empty object
/// { name: "John" }            // Single field
/// { name: "John", age: 25 }   // Multiple fields with commas
/// { name: "John" age: 25 }    // Multiple fields with spaces
/// { name: "John", age: 25, }  // Trailing comma allowed
/// {
///   name: "John",
///   age: 25,
///   active: true
/// }                           // Multi-line format
///
/// // Constant context (default values)
/// {
///   name: "default",
///   count: 10,
///   enabled: true
/// }
///
/// // Variable context (query arguments)
/// {
///   name: $userName,
///   filter: { status: ACTIVE },
///   limit: 100
/// }
/// ```
///
/// ## Generic Parameters
///
/// - `InputValue`: The type of values contained in object fields
/// - `Src`: The source slice type (typically `&str`)
/// - `Span`: The span type for position information
/// - `Container`: The collection type for fields (defaults to `Vec`, can be customized)
///
/// ## Component Structure
///
/// Each object literal contains:
/// - **Overall span**: Covers the entire object including braces
/// - **Left brace**: The opening `{` token with its position
/// - **Right brace**: The closing `}` token with its position
/// - **Fields**: The collection of name-value pairs
///
/// ## Trait Implementations
///
/// This type implements the standard span traits:
/// - [`AsSpan`]: Provides access to the source span
/// - [`IntoSpan`]: Enables consuming the object to extract its span
/// - [`IntoComponents`]: Allows decomposition into constituent parts
///
/// The component tuple contains: `(span, l_brace, fields, r_brace)`
///
/// ## Usage in GraphQL
///
/// Object literals appear throughout GraphQL:
/// - **Query arguments**: `user(filter: { status: ACTIVE, role: "admin" })`
/// - **Variable values**: `{ "input": { "title": "Post", "content": "..." } }`
/// - **Default values**: `field(config: InputType = { debug: false })`
/// - **Nested inputs**: Complex input structures with multiple levels
///
/// Spec: [Input Object Values](https://spec.graphql.org/draft/#sec-Input-Object-Values)
#[derive(Debug, Clone)]
pub struct Object<Name, Value, Container = Vec<ObjectField<Name, Value>>> {
  span: Span,
  fields: Container,
  _m: PhantomData<(Name, Value)>,
}

impl<Name, Value, Container> AsRef<Span> for Object<Name, Value, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, InputValue, Container> IntoSpan<Span> for Object<Name, InputValue, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, InputValue, Container> IntoComponents for Object<Name, InputValue, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.fields)
  }
}

impl<Name, InputValue, Container> Object<Name, InputValue, Container> {
  /// Creates a new object literal with the given span and fields.
  #[inline(always)]
  pub const fn new(span: Span, fields: Container) -> Self {
    Self {
      span,
      fields,
      _m: PhantomData,
    }
  }

  /// Returns the source span of the entire object literal.
  ///
  /// This span covers from the opening brace through the closing brace,
  /// including all fields and whitespace within. Useful for error reporting,
  /// source mapping, and extracting the complete object text.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the object fields.
  ///
  /// This provides access to all fields that were successfully parsed
  /// from the object literal.
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }

  /// Creates a parser for GraphQL object literals with customizable field parsing.
  ///
  /// This parser handles the complete object syntax including braces and
  /// enforces proper structure. It uses the provided `field_parser` to parse
  /// each individual field within the object.
  ///
  /// ## Error Handling
  ///
  /// If the closing brace is missing, the parser invokes the provided
  /// `on_missing_rbrace` function to generate a custom error message.
  /// This allows for context-specific error reporting.
  pub fn parser_with<'src, I, T, Error, NP, VP, E>(
    name_parser: NP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: UnclosedObjectValueError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone + 'src,
    VP: Parser<'src, I, InputValue, E> + Clone + 'src,
    Container: chumsky::container::Container<ObjectField<Name, InputValue>>,
    Colon: Parseable<'src, I, T, Error>,
    LBrace: Parseable<'src, I, T, Error>,
    RBrace: Parseable<'src, I, T, Error>,
  {
    LBrace::parser()
      .ignore_then(
        ObjectField::parser_with(name_parser, value_parser)
          .repeated()
          .collect(),
      )
      .then(RBrace::parser().or_not())
      .try_map(move |(values, r), span| match r {
        Some(_) => Ok(Self::new(span, values)),
        None => Err(Error::unclosed_object(span)),
      })
  }
}

impl<'a, Name, InputValue, Container, I, T, Error> Parseable<'a, I, T, Error>
  for Object<Name, InputValue, Container>
where
  Name: Parseable<'a, I, T, Error>,
  InputValue: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<ObjectField<Name, InputValue>>,
  Error: UnclosedObjectValueError,
  Colon: Parseable<'a, I, T, Error>,
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Name::parser(), InputValue::parser())
  }
}
