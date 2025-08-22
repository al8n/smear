use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  super::{char::Char, convert::*, name::Name, source::Source, spanned::Spanned},
  punct::{Colon, LBrace, RBrace},
};

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
/// - [`AsSpanned`]: Provides access to the source span
/// - [`IntoSpanned`]: Enables consuming the object to extract its span
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
pub struct ObjectValueField<InputValue, Src, Span> {
  span: Spanned<Src, Span>,
  name: Name<Src, Span>,
  colon: Colon<Src, Span>,
  value: InputValue,
}

impl<InputValue, Src, Span> AsSpanned<Src, Span> for ObjectValueField<InputValue, Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    &self.span
  }
}

impl<InputValue, Src, Span> IntoSpanned<Src, Span> for ObjectValueField<InputValue, Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<InputValue, Src, Span> IntoComponents for ObjectValueField<InputValue, Src, Span> {
  type Components = (
    Spanned<Src, Span>,
    Name<Src, Span>,
    Colon<Src, Span>,
    InputValue,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<InputValue, Src, Span> ObjectValueField<InputValue, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Src, Span> {
    &self.colon
  }
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }
  #[inline]
  pub const fn value(&self) -> &InputValue {
    &self.value
  }

  /// name ':' value  — only **trailing** ignored so repeats stop cleanly at `}`.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    P: Parser<'src, I, InputValue, E> + Clone,
  {
    let ws = super::ignored::ignored();

    Name::parser()
      .then(Colon::parser().padded_by(ws.clone()))
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Spanned::from(sp),
        name,
        colon,
        value,
      })
  }
}

/// Input Object Value: `{` (fields)? `}`
#[derive(Debug, Clone)]
pub struct ObjectValue<Field, Src, Span, Container = std::vec::Vec<Field>> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  r_brace: RBrace<Src, Span>,
  fields: Container,
  _field: core::marker::PhantomData<Field>,
}

impl<Field, Src, Span, Container> AsSpanned<Src, Span>
  for ObjectValue<Field, Src, Span, Container>
{
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    &self.span
  }
}

impl<Field, Src, Span, Container> IntoSpanned<Src, Span>
  for ObjectValue<Field, Src, Span, Container>
{
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<Field, Src, Span, Container> IntoComponents for ObjectValue<Field, Src, Span, Container> {
  type Components = (
    Spanned<Src, Span>,
    LBrace<Src, Span>,
    Container,
    RBrace<Src, Span>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.fields, self.r_brace)
  }
}

impl<Field, Src, Span, Container> ObjectValue<Field, Src, Span, Container> {
  /// Returns the source span of the entire object literal.
  ///
  /// This span covers from the opening brace through the closing brace,
  /// including all fields and whitespace within. Useful for error reporting,
  /// source mapping, and extracting the complete object text.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the opening brace token.
  ///
  /// This provides access to the `{` character that begins the object,
  /// including its exact source position. Useful for syntax highlighting,
  /// brace matching, and precise error reporting at object boundaries.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    &self.l_brace
  }

  /// Returns the closing brace token.
  ///
  /// This provides access to the `}` character that ends the object,
  /// including its exact source position. Useful for syntax highlighting,
  /// brace matching, and detecting incomplete objects.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    &self.r_brace
  }

  /// Returns the container holding the object fields.
  ///
  /// This provides access to all fields that were successfully parsed
  /// from the object literal.
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
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
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    P: Parser<'src, I, Field, E> + Clone,
    Container: chumsky::container::Container<Field>,
  {
    let ws = super::ignored::ignored();
    let open = LBrace::parser();
    let close = RBrace::parser();

    open
      .then_ignore(ws.clone())
      .then(choice((
        // Empty fast path: immediately '}'
        close.clone().map(|r| (Container::default(), r)),
        // Non-empty: one-or-more fields; commas are in `ws`
        field_parser
          .padded_by(ws)
          .repeated()
          .at_least(1)
          .collect()
          .then(close),
      )))
      .map_with(|(l_brace, (fields, r_brace)), sp| Self {
        span: Spanned::from(sp),
        l_brace,
        r_brace,
        fields,
        _field: core::marker::PhantomData,
      })
  }
}
