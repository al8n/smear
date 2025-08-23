use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  lang::{ignored, punct::Colon, Name, StringValue},
  source::{Char, Slice, Source},
  convert::*,
};

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
/// InputValueDefinition : Description? Name : Type DefaultValue? Directives?
/// ```
/// 
/// Spec: [InputValueDefinition](https://spec.graphql.org/draft/#InputValueDefinition)
#[derive(Debug, Clone)]
pub struct InputValueDefinition<Type, DefaultValue, Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  name: Name<Span>,
  colon: Colon<Span>,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Type, DefaultValue, Directives, Span> AsRef<Span>
  for InputValueDefinition<Type, DefaultValue, Directives, Span>
{
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Type, DefaultValue, Directives, Span> IntoSpan<Span>
  for InputValueDefinition<Type, DefaultValue, Directives, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, DefaultValue, Directives, Span> IntoComponents
  for InputValueDefinition<Type, DefaultValue, Directives, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    Name<Span>,
    Colon<Span>,
    Type,
    Option<DefaultValue>,
    Option<Directives>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.name,
      self.colon,
      self.ty,
      self.default_value,
      self.directives,
    )
  }
}

impl<Type, DefaultValue, Directives, Span>
  InputValueDefinition<Type, DefaultValue, Directives, Span>
{
  /// Returns a reference to the span covering the entire input value definition.
  /// 
  /// The span includes the optional description, name, colon, type, optional
  /// default value, and optional directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the input value definition.
  /// 
  /// The description provides documentation for the input value and appears before
  /// the value name. It can be either a single-line string or a block string, and
  /// helps developers understand the purpose and usage of the parameter.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the input value name.
  /// 
  /// This is the identifier used to reference this input value when providing
  /// arguments to fields, directives, or input objects. The name must be a
  /// valid GraphQL identifier and should be unique within its context.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the colon separator between name and type.
  /// 
  /// The colon is a required part of input value definition syntax that separates
  /// the parameter name from its type specification.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
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
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the input value definition.
  ///
  /// The parser uses `.padded_by(ignored())` for the colon, type, and default value
  /// to handle whitespace gracefully around these components.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, E, TP, VP, DP>(
    type_parser: TP,
    default_const_value_parser: VP,
    const_directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    VP: Parser<'src, I, DefaultValue, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(Name::parser())
      .then(Colon::parser().padded_by(ignored()))
      .then(type_parser.padded_by(ignored()))
      .then(default_const_value_parser.padded_by(ignored()).or_not())
      .then(const_directives_parser.or_not())
      .map_with(
        |(((((description, name), colon), ty), default_value), directives), span| Self {
          span: Span::from_map_extra(span),
          description,
          name,
          colon,
          ty,
          default_value,
          directives,
        },
      )
  }
}
