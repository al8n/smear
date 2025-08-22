use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  super::{
    convert::*,
    language::ignored::ignored,
    name::Name,
    source::{Char, Slice, Source},
  },
  punct::{Colon, LBrace, RBrace},
};

/// A single field within a GraphQL input object literal.
///
/// Represents a name-value pair within an object literal, following the
/// GraphQL specification for input object fields. Each field consists of
/// a field name, a colon separator, and a value, with optional whitespace
/// and comments allowed around each component.
///
/// ## Format
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
#[derive(Debug, Clone)]
pub struct ObjectValueField<InputValue, Span> {
  span: Span,
  name: Name<Span>,
  colon: Colon<Span>,
  value: InputValue,
}

impl<InputValue, Span> AsRef<Span> for ObjectValueField<InputValue, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<InputValue, Span> IntoSpan<Span> for ObjectValueField<InputValue, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValue, Span> IntoComponents for ObjectValueField<InputValue, Span> {
  type Components = (Span, Name<Span>, Colon<Span>, InputValue);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<InputValue, Span> ObjectValueField<InputValue, Span> {
  /// Returns the source span of the entire field.
  ///
  /// This span covers from the first character of the field name through
  /// the last character of the field value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the colon separator token.
  ///
  /// This provides access to the `:` character that separates the field
  /// name from its value, including its exact source position. Useful
  /// for syntax highlighting and precise error reporting.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns the field name.
  ///
  /// This provides access to the GraphQL name that identifies this field
  /// within the object. The name follows standard GraphQL identifier rules
  /// and cannot be a reserved keyword.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
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

  /// Creates a parser for object fields with compile-time constant validation.
  ///
  /// This parser handles the complete object field syntax including the
  /// field name, colon separator, and field value. It manages whitespace
  /// and comments around each component according to GraphQL rules, and
  /// enforces constant vs variable context requirements at compile time.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the object.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, InputValue, E> + Clone,
  {
    Name::parser()
      .then(Colon::parser().padded_by(ignored()))
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Span::from_map_extra(sp),
        name,
        colon,
        value,
      })
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
/// ## Format
///
/// ```text
/// ObjectValue ::= '{' ObjectFields? '}'
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
pub struct ObjectValue<Field, Span, Container = std::vec::Vec<Field>> {
  span: Span,
  l_brace: LBrace<Span>,
  r_brace: RBrace<Span>,
  fields: Container,
  _field: core::marker::PhantomData<Field>,
}

impl<Field, Span, Container> AsRef<Span> for ObjectValue<Field, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Field, Span, Container> IntoSpan<Span> for ObjectValue<Field, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Field, Span, Container> IntoComponents for ObjectValue<Field, Span, Container> {
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.fields, self.r_brace)
  }
}

impl<Field, Span, Container> ObjectValue<Field, Span, Container> {
  /// Returns the source span of the entire object literal.
  ///
  /// This span covers from the opening brace through the closing brace,
  /// including all fields and whitespace within. Useful for error reporting,
  /// source mapping, and extracting the complete object text.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the opening brace token.
  ///
  /// This provides access to the `{` character that begins the object,
  /// including its exact source position. Useful for syntax highlighting,
  /// brace matching, and precise error reporting at object boundaries.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns the container holding the object fields.
  ///
  /// This provides access to all fields that were successfully parsed
  /// from the object literal.
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }

  /// Returns the closing brace token.
  ///
  /// This provides access to the `}` character that ends the object,
  /// including its exact source position. Useful for syntax highlighting,
  /// brace matching, and detecting incomplete objects.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Creates a parser for object literals with compile-time constant validation.
  ///
  /// This is the core parsing function that accepts any value parser and
  /// creates a complete object parser. It handles all GraphQL object syntax
  /// including braces, field parsing, whitespace, optional commas, and empty
  /// objects, while enforcing constant vs variable context requirements.
  ///
  /// Spec: [Object Value](https://spec.graphql.org/draft/#sec-Object-Value)
  pub fn parser_with<'src, I, E, P>(field_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Field, E> + Clone,
    Container: chumsky::container::Container<Field>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty fast path: immediately '}'
        RBrace::parser().map(|r| (Container::default(), r)),
        // Non-empty: one-or-more fields; commas are in `ws`
        field_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect()
          .then(RBrace::parser()),
      )))
      .map_with(|(l_brace, (fields, r_brace)), sp| Self {
        span: Span::from_map_extra(sp),
        l_brace,
        r_brace,
        fields,
        _field: core::marker::PhantomData,
      })
  }
}
