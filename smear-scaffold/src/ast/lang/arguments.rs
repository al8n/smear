use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use core::marker::PhantomData;

use smear_lexer::punctuator::{LParen, RParen};

use std::vec::Vec;

/// A single named argument in a GraphQL operation or directive.
///
/// Represents a name-value pair used to pass parameters to GraphQL fields,
/// directives, or other language constructs. Arguments follow the standard
/// GraphQL syntax of a name identifier followed by a colon and a value.
///
/// ## Grammar
///
/// ```text
/// Argument ::= Name ':' Value
/// ```
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument)
#[derive(Debug, Clone, Copy)]
pub struct Argument<Name, Value> {
  span: Span,
  name: Name,
  value: Value,
}

impl<Name, Value> AsSpan<Span> for Argument<Name, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Value> IntoSpan<Span> for Argument<Name, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Value> IntoComponents for Argument<Name, Value> {
  type Components = (Span, Name, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.value)
  }
}

impl<Name, Value> Argument<Name, Value> {
  /// Creates a new argument.
  #[inline]
  pub const fn new(span: Span, name: Name, value: Value) -> Self {
    Self { span, name, value }
  }

  /// Returns the source span of the entire argument.
  ///
  /// This span covers from the first character of the argument name through
  /// the last character of the argument value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the argument name identifier.
  ///
  /// This provides access to the GraphQL name that identifies this argument.
  /// The name follows standard GraphQL identifier rules and is used to match
  /// the argument with its expected parameter in the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the argument value.
  ///
  /// This provides access to the value assigned to this argument. The value
  /// can be any valid GraphQL input value including scalars, enums, objects,
  /// lists, variables, or null depending on the argument's expected type.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

/// A collection of arguments enclosed in parentheses.
///
/// Represents an argument list as used in GraphQL fields, directives, and other
/// language constructs. Arguments are enclosed in parentheses and separated by
/// whitespace, providing a structured way to pass parameters in GraphQL operations.
///
/// ## Specification Rules
///
/// GraphQL argument lists follow these formatting rules:
/// - **Parenthesis delimiters**: Must be enclosed in `(` and `)`
/// - **Argument format**: Each argument follows `name: value` syntax
/// - **Argument separation**: Arguments separated by whitespace (commas optional)
/// - **Non-empty requirement**: Argument lists must contain at least one argument
/// - **Flexible whitespace**: Whitespace and comments allowed throughout
/// - **Unique names**: Argument names should be unique within the list (semantic validation)
///
/// ## Grammar
///
/// ```text
/// Arguments ::= '(' Argument+ ')'
/// Argument ::= Name ':' Value
/// ```
///
/// ## Generic Parameters
///
/// - `Arg`: The type representing individual arguments
/// - `Span`: The span type for position information
/// - `Container`: The collection type for arguments (defaults to `Vec`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Argument<Value, Span>>` (default): Standard dynamic array
/// - Any type implementing `tokora::container::Container<Arg>`
///
/// ## Component Structure
///
/// Each argument list contains:
/// - **Overall span**: Covers the entire argument list including parentheses
/// - **Left parenthesis**: The opening `(` token with its position
/// - **Right parenthesis**: The closing `)` token with its position
/// - **Arguments**: The collection of parsed arguments
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments)
#[derive(Debug, Clone, Copy)]
pub struct Arguments<Arg, Container = Vec<Arg>> {
  span: Span,
  l_paren: LParen,
  arguments: Container,
  r_paren: RParen,
  _arg: PhantomData<Arg>,
}

impl<Arg, Container> AsSpan<Span> for Arguments<Arg, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Arg, Container> IntoSpan<Span> for Arguments<Arg, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Arg, Container> IntoComponents for Arguments<Arg, Container> {
  type Components = (Span, LParen, Container, RParen);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.arguments, self.r_paren)
  }
}

impl<Arg, Container> Arguments<Arg, Container> {
  /// Creates a new arguments list.
  #[inline]
  pub const fn new(span: Span, l_paren: LParen, arguments: Container, r_paren: RParen) -> Self {
    Self {
      span,
      l_paren,
      arguments,
      r_paren,
      _arg: PhantomData,
    }
  }

  /// Returns the source span of the entire argument list.
  ///
  /// This span covers from the opening parenthesis through the closing
  /// parenthesis, including all arguments and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete argument text.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the opening parenthesis token.
  ///
  /// This provides access to the `(` character that begins the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and precise error reporting at argument boundaries.
  #[inline]
  pub const fn l_paren(&self) -> &LParen {
    &self.l_paren
  }

  /// Returns the closing parenthesis token.
  ///
  /// This provides access to the `)` character that ends the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and detecting incomplete argument lists.
  #[inline]
  pub const fn r_paren(&self) -> &RParen {
    &self.r_paren
  }

  /// Returns the container holding the arguments.
  ///
  /// This provides access to all arguments that were successfully parsed
  /// from the argument list.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }
}
