use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::lang::punctuator::Colon;

/// Represents a GraphQL input value definition.
///
/// An input value definition specifies a parameter that can be provided to a field,
/// directive, or input object. It defines the parameter's name, type, optional default
/// value, optional description, and optional directives. Input value definitions are
/// the building blocks for creating structured input interfaces in GraphQL.
///
/// Input value definitions are used in multiple contexts within GraphQL:
/// - **Field arguments**: Parameters for field selection in object and interface types
/// - **Directive arguments**: Parameters for directive applications
/// - **Input object fields**: Fields within input object type definitions
/// - **Variable definitions**: Operation variable specifications
///
/// ## Examples
///
/// ```text
/// # Simple input value definition (field argument)
/// id: ID!
///
/// # Input value with description
/// """
/// The unique identifier for the user
/// """
/// id: ID!
///
/// # Input value with default value
/// first: Int = 10
///
/// # Input value with directives
/// email: String @constraint(format: "email")
///
/// # Complex input value with all components
/// """
/// Search query with advanced filtering options.
/// Supports wildcard matching and field-specific searches.
/// """
/// query: String! = "*" @constraint(minLength: 1, maxLength: 1000) @sanitize
///
/// # Input values in different contexts:
///
/// # Field arguments
/// type User {
///   posts(
///     first: Int = 10
///     after: String
///     filter: PostFilter
///   ): PostConnection!
/// }
///
/// # Directive arguments
/// directive @auth(
///   requires: Role = USER
///   scopes: [String!]
/// ) on FIELD_DEFINITION
///
/// # Input object fields
/// input CreateUserInput {
///   name: String!
///   email: String! @constraint(format: "email")
///   age: Int @constraint(min: 0, max: 150)
///   preferences: UserPreferencesInput = {
///     theme: LIGHT
///     notifications: true
///   }
/// }
///
/// # Variable definitions
/// query GetUser(
///   $id: ID!
///   $includeProfile: Boolean = false
///   $limit: Int = 20 @constraint(min: 1, max: 100)
/// ) {
///   user(id: $id) {
///     name
///     profile @include(if: $includeProfile) {
///       bio
///     }
///     posts(first: $limit) {
///       title
///     }
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Type` - The type representing the input value's GraphQL type (e.g., String!, [Int], etc.)
/// * `DefaultValue` - The type representing the optional default value
/// * `Directives` - The type representing directives applied to the input value
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// InputValueDefinition : Name : Type DefaultValue? Directives?
/// ```
///
/// Spec: [InputValueDefinition](https://spec.graphql.org/draft/#InputValueDefinition)
#[derive(Debug, Clone, Copy)]
pub struct InputValueDefinition<Name, Type, DefaultValue, Directives> {
  span: Span,
  name: Name,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Name, Type, DefaultValue, Directives> AsSpan<Span>
  for InputValueDefinition<Name, Type, DefaultValue, Directives>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Type, DefaultValue, Directives> IntoSpan<Span>
  for InputValueDefinition<Name, Type, DefaultValue, Directives>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Type, DefaultValue, Directives> IntoComponents
  for InputValueDefinition<Name, Type, DefaultValue, Directives>
{
  type Components = (Span, Name, Type, Option<DefaultValue>, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.ty,
      self.default_value,
      self.directives,
    )
  }
}

impl<Name, Type, DefaultValue, Directives>
  InputValueDefinition<Name, Type, DefaultValue, Directives>
{
  /// Returns a reference to the span covering the entire input value definition.
  ///
  /// The span includes the optional description, name, colon, type, optional
  /// default value, and optional directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the input value name.
  ///
  /// This is the identifier used to reference this input value when providing
  /// arguments to fields, directives, or input objects. The name must be a
  /// valid GraphQL identifier and should be unique within its context.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the input value's type.
  ///
  /// The type specifies what kind of data this input value accepts. It can be
  /// a scalar type, enum, input object, or wrapper types (list, non-null).
  /// The type system ensures type safety for all input data.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns a reference to the optional default value.
  ///
  /// The default value is used when the input value is not provided by the client.
  /// Default values must be compatible with the input value's type and are
  /// particularly useful for optional parameters to provide sensible fallbacks.
  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }

  /// Returns a reference to the optional directives applied to this input value.
  ///
  /// Directives provide metadata or specify behavior for the input value,
  /// such as validation rules, constraints, deprecation information, or
  /// custom processing instructions. They are particularly useful for
  /// input validation and security.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Creates a parser that can parse a complete input value definition.
  ///
  /// This parser handles the full input value definition syntax including all
  /// optional and required components. The parsing of type, default value, and
  /// directives is delegated to the provided parsers, allowing for flexibility
  /// in handling different contexts and validation requirements.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, NP, TP, VP, DP>(
    name_parser: NP,
    type_parser: TP,
    default_const_value_parser: VP,
    const_directives_parser: DP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Colon: Parseable<'a, I, T, Error> + 'a,
    NP: Parser<'a, I, Name, E> + Clone,
    TP: Parser<'a, I, Type, E> + Clone,
    DP: Parser<'a, I, Directives, E> + Clone,
    VP: Parser<'a, I, DefaultValue, E> + Clone,
  {
    name_parser
      .then_ignore(Colon::parser())
      .then(type_parser)
      .then(default_const_value_parser.or_not())
      .then(const_directives_parser.or_not())
      .map_with(|(((name, ty), default_value), directives), exa| Self {
        span: exa.span(),
        name,
        ty,
        default_value,
        directives,
      })
  }
}

impl<'a, Name, Type, DefaultValue, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for InputValueDefinition<Name, Type, DefaultValue, Directives>
where
  Name: Parseable<'a, I, T, Error>,
  Type: Parseable<'a, I, T, Error>,
  DefaultValue: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
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
    Self::parser_with(
      Name::parser(),
      Type::parser(),
      DefaultValue::parser(),
      Directives::parser(),
    )
  }
}
