use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};



use core::marker::PhantomData;
use std::vec::Vec;

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

impl<Name, InputValue> AsSpan<Span> for ObjectField<Name, InputValue> {
  #[inline]
  fn as_span(&self) -> &Span {
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
  /// Creates a new object field with the given span, name, and value.
  #[inline]
  pub const fn new(span: Span, name: Name, value: InputValue) -> Self {
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

impl<Name, Value, Container> AsSpan<Span> for Object<Name, Value, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
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
}
