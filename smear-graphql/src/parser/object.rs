use logosky::utils::Span;
use smear_parser::source::{IntoComponents, IntoSpan};

use crate::parser::punctuator::{LBrace, RBrace};

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
pub struct Object<Field> {
  span: Span,
  l_brace: LBrace,
  r_brace: RBrace,
  fields: Vec<Field>,
}

impl<Field> AsRef<Span> for Object<Field> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Field> IntoSpan<Span> for Object<Field> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Field> IntoComponents for Object<Field> {
  type Components = (Span, LBrace, Vec<Field>, RBrace);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.fields, self.r_brace)
  }
}

impl<Field> Object<Field> {
  #[inline(always)]
  pub(crate) const fn new(span: Span, l: LBrace, fields: Vec<Field>, r: RBrace) -> Self {
    Self {
      span,
      l_brace: l,
      r_brace: r,
      fields,
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

  /// Returns the opening brace token.
  ///
  /// This provides access to the `{` character that begins the object,
  /// including its exact source position. Useful for syntax highlighting,
  /// brace matching, and precise error reporting at object boundaries.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace {
    &self.l_brace
  }

  /// Returns the container holding the object fields.
  ///
  /// This provides access to all fields that were successfully parsed
  /// from the object literal.
  #[inline]
  pub const fn fields(&self) -> &[Field] {
    self.fields.as_slice()
  }

  /// Returns the closing brace token.
  ///
  /// This provides access to the `}` character that ends the object,
  /// including its exact source position. Useful for syntax highlighting,
  /// brace matching, and detecting incomplete objects.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace {
    &self.r_brace
  }
}
