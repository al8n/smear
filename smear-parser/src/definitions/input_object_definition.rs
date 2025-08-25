use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{ignored, keywords, Const, Name, StringValue},
  source::{Char, Slice, Source},
};

/// Represents a complete input object type definition in GraphQL schema.
///
/// An input object type definition specifies a complex input type that can be used
/// as arguments in GraphQL operations. Input object types are essential for handling
/// structured data in mutations, complex query parameters, and nested input scenarios.
///
/// Input objects serve as the primary mechanism for accepting complex, structured
/// data from GraphQL clients in a type-safe manner.
///
/// ## GraphQL Input Object Philosophy
///
/// Input object types support GraphQL's approach to structured data input:
/// - **Type safety**: Ensuring client data matches expected structure
/// - **Validation**: Built-in validation through type definitions and directives
/// - **Documentation**: Clear interfaces for client developers
/// - **Composition**: Building complex inputs from simpler components
/// - **Flexibility**: Supporting optional fields and default values
/// - **Evolution**: Schema evolution through field additions and deprecation
///
/// ## Examples
///
/// ```text
/// # Simple input object definition
/// input CreateUserInput {
///   name: String!
///   email: String!
///   age: Int
/// }
///
/// # Input object with description
/// """
/// Input data for creating a new user account.
/// All required fields must be provided.
/// """
/// input CreateUserInput {
///   """
///   The user's full display name
///   """
///   name: String!
///   
///   """
///   Unique email address for the account
///   """
///   email: String!
///   
///   """
///   Optional age (must be positive if provided)
///   """
///   age: Int
/// }
///
/// # Input object with directives
/// input CreateUserInput @auth(requires: ADMIN) {
///   name: String! @constraint(minLength: 1, maxLength: 100)
///   email: String! @constraint(format: "email") @unique
///   password: String! @sensitive @constraint(minLength: 8)
/// }
///
/// # Complex nested input object
/// """
/// Comprehensive input for user registration with profile data
/// """
/// input UserRegistrationInput @rateLimit(max: 5, window: "1h") {
///   """
///   Basic account information
///   """
///   account: AccountInput!
///   
///   """
///   Optional profile information
///   """
///   profile: UserProfileInput
///   
///   """
///   Initial preferences with sensible defaults
///   """
///   preferences: UserPreferencesInput = {
///     theme: LIGHT
///     notifications: true
///     language: "en"
///   }
///   
///   """
///   Terms of service acceptance (required)
///   """
///   acceptTerms: Boolean! = false
/// }
///
/// # Input object definition without fields (for extensions)
/// input UserInput @directive
/// ```
///
/// # Usage in Operations
///
/// ```text
/// # Using input objects in mutations
/// mutation CreateUser($input: CreateUserInput!) {
///   createUser(input: $input) {
///     id
///     name
///     email
///   }
/// }
///
/// # Variables provided by client
/// {
///   "input": {
///     "name": "John Doe",
///     "email": "john@example.com",
///     "age": 30
///   }
/// }
///
/// # Using input objects in queries
/// query SearchUsers($filter: UserFilterInput, $pagination: PaginationInput) {
///   users(filter: $filter, pagination: $pagination) {
///     edges {
///       node {
///         id
///         name
///       }
///     }
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `FieldsDefinition` - The type representing the input object's field definitions
/// * `Directives` - The type representing directives applied to the input object
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// InputObjectTypeDefinition : Description? input Name Directives? InputFieldsDefinition?
/// ```
///
/// Spec: [Input Object Type Definition](https://spec.graphql.org/draft/#sec-Input-Object-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct InputObjectTypeDefinition<Directives, FieldsDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  input: keywords::Input<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
  fields: Option<FieldsDefinition>,
}

impl<Directives, FieldsDefinition, Span> AsRef<Span>
  for InputObjectTypeDefinition<Directives, FieldsDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, FieldsDefinition, Span> IntoSpan<Span>
  for InputObjectTypeDefinition<Directives, FieldsDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, FieldsDefinition, Span> IntoComponents
  for InputObjectTypeDefinition<Directives, FieldsDefinition, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Input<Span>,
    Name<Span>,
    Option<Directives>,
    Option<FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.input,
      self.name,
      self.directives,
      self.fields,
    )
  }
}

impl<Directives, FieldsDefinition, Span>
  InputObjectTypeDefinition<Directives, FieldsDefinition, Span>
{
  /// Returns a reference to the span covering the entire input object definition.
  ///
  /// The span includes the optional description, input keyword, name, optional
  /// directives, and optional fields definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the input object definition.
  ///
  /// The description provides documentation for the input object type and appears
  /// before the input keyword. It can be either a single-line string or a block string.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the `input` keyword.
  ///
  /// This provides access to the exact location and span information of the
  /// input keyword that defines this type.
  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Span> {
    &self.input
  }

  /// Returns a reference to the name of the input object type.
  ///
  /// This is the identifier that will be used to reference this input object type
  /// in other parts of the schema and in GraphQL operations. Input object type
  /// names must be unique within the schema.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the optional directives applied to this input object.
  ///
  /// Directives provide metadata or specify behavior for the input object type,
  /// such as access control, validation rules, rate limiting, or custom processing.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the optional fields definition.
  ///
  /// The fields definition contains all the input fields available on this input object.
  /// It may be absent in input object definitions that are meant to be extended later.
  #[inline]
  pub const fn fields(&self) -> Option<&FieldsDefinition> {
    self.fields.as_ref()
  }

  /// Creates a parser that can parse a complete input object definition.
  ///
  /// This parser handles the full input object definition syntax including all
  /// optional components. The parsing of fields definition and directives is
  /// delegated to the provided parser functions.
  #[inline]
  pub fn parser_with<'src, I, E, DP, FP>(
    directives_parser: DP,
    input_fields_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    Directives: Const<true>,
    FieldsDefinition: Const<true>,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Input::parser().then_ignore(ignored()))
      .then(Name::parser())
      .then(directives_parser.padded_by(ignored()).or_not())
      .then(input_fields_definition_parser.padded_by(ignored()).or_not())
      .map_with(
        |((((description, input), name), directives), fields), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          name,
          input,
          directives,
          fields,
        },
      )
      .padded_by(ignored())
  }
}

/// Represents the content of an input object type extension.
///
/// Input object extensions can add new fields to an existing input object type
/// and/or add new directives to the type. This enum represents the different
/// kinds of content that can be added in an input object extension.
///
/// ## GraphQL Extension Context
///
/// GraphQL allows extending existing types to add new functionality without
/// modifying the original type definition. For input object types, extensions can:
/// - Add new input fields (with optional additional directives on the type)
/// - Add only directives to the type (without new fields)
///
/// ## Examples
///
/// ```text
/// # Extension adding fields only
/// extend input CreateUserInput {
///   phoneNumber: String
///   address: AddressInput
/// }
///
/// # Extension adding directives and fields
/// extend input CreateUserInput @rateLimit(max: 10) {
///   socialSecurityNumber: String @sensitive
///   emergencyContact: ContactInput
/// }
///
/// # Extension adding only directives
/// extend input CreateUserInput @auth(requires: ADMIN) @audit
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the input object extension
/// * `FieldsDefinition` - The type representing the new input fields being added
#[derive(Debug, Clone, Copy)]
pub enum InputObjectTypeExtensionContent<Directives, FieldsDefinition> {
  /// Extension that adds only directives to the input object type without new fields
  Directives(Directives),
  /// Extension that adds new input fields, optionally with additional directives on the type
  Fields {
    /// Optional directives to add to the input object type itself
    directives: Option<Directives>,
    /// New input fields to add to the input object type
    fields: FieldsDefinition,
  },
}

impl<Directives, FieldsDefinition> InputObjectTypeExtensionContent<Directives, FieldsDefinition> {
  /// Creates a parser that can parse input object extension content.
  ///
  /// This parser handles both types of input object extensions: those that add fields
  /// (optionally with directives) and those that add only directives.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the extension content.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, DP, FP>(
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Directives: Const<true>,
    FieldsDefinition: Const<true>,
    DP: Parser<'src, I, Directives, E> + Clone,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
  {
    choice((
      directives_parser()
        .then_ignore(ignored())
        .or_not()
        .then(fields_definition_parser())
        .map(|(directives, fields)| Self::Fields { directives, fields }),
      directives_parser().map(Self::Directives),
    ))
  }
}

/// Represents a complete input object type extension in GraphQL schema.
///
/// An input object extension allows adding new fields and/or directives to an
/// existing input object type without modifying the original definition. This is
/// particularly useful in schema composition scenarios where different parts
/// of a system need to extend shared input types.
///
/// ## GraphQL Extension Philosophy
///
/// Extensions support GraphQL's modular approach to schema development:
/// - **Non-destructive**: Extensions don't modify original definitions
/// - **Additive**: Extensions can only add new capabilities
/// - **Composable**: Multiple extensions can be applied to the same type
/// - **Modular**: Different services can extend shared input types
///
/// ## Examples
///
/// ```text
/// # Simple input object extension adding fields
/// extend input CreateUserInput {
///   phoneNumber: String
///   address: AddressInput
///   preferences: UserPreferencesInput
/// }
///
/// # Input object extension adding directives to the type and new fields
/// extend input CreateUserInput @rateLimit(max: 5, window: "1h") {
///   socialSecurityNumber: String @sensitive
///   backgroundCheck: BackgroundCheckInput @auth(requires: ADMIN)
/// }
///
/// # Input object extension adding only directives to the type
/// extend input CreateUserInput @auth(requires: VERIFIED_USER) @audit(level: HIGH)
///
/// # Complex input object extension with validation
/// extend input CreateUserInput @validate(schema: "strict") {
///   """
///   Government-issued ID number for identity verification
///   """
///   governmentId: String @sensitive @constraint(pattern: "^[A-Z0-9-]+$")
///   
///   """
///   Proof of address documents
///   """
///   addressProof: [DocumentInput!] @constraint(maxItems: 3)
///   
///   """
///   Emergency contact information (required for premium accounts)
///   """
///   emergencyContact: EmergencyContactInput @include(if: $isPremium)
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied in the extension
/// * `FieldsDefinition` - The type representing the new input fields being added
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// InputObjectTypeExtension : extend input Name ( Directives InputFieldsDefinition? | InputFieldsDefinition )
/// ```
///
/// Spec: [Input Object Type Extension](https://spec.graphql.org/draft/#sec-Input-Object-Type-Extension)
#[derive(Debug, Clone, Copy)]
pub struct InputObjectTypeExtension<Directives, FieldsDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  input: keywords::Input<Span>,
  name: Name<Span>,
  content: InputObjectTypeExtensionContent<Directives, FieldsDefinition>,
}

impl<Directives, FieldsDefinition, Span> AsRef<Span>
  for InputObjectTypeExtension<Directives, FieldsDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, FieldsDefinition, Span> IntoSpan<Span>
  for InputObjectTypeExtension<Directives, FieldsDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, FieldsDefinition, Span> IntoComponents
  for InputObjectTypeExtension<Directives, FieldsDefinition, Span>
{
  type Components = (
    Span,
    keywords::Extend<Span>,
    keywords::Input<Span>,
    Name<Span>,
    InputObjectTypeExtensionContent<Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.extend, self.input, self.name, self.content)
  }
}

impl<Directives, FieldsDefinition, Span>
  InputObjectTypeExtension<Directives, FieldsDefinition, Span>
{
  /// Returns a reference to the span covering the entire input object extension.
  ///
  /// The span includes the extend keyword, input keyword, name, and all
  /// extension content (directives and/or input fields).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `extend` keyword.
  ///
  /// This provides access to the exact location and span information of the
  /// extend keyword that starts the extension definition.
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  /// Returns a reference to the `input` keyword.
  ///
  /// This provides access to the exact location and span information of the
  /// input keyword that specifies the type being extended.
  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Span> {
    &self.input
  }

  /// Returns a reference to the name of the input object type being extended.
  ///
  /// This must match the name of an existing input object type in the schema
  /// for the extension to be valid.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the extension content.
  ///
  /// The content specifies what is being added to the input object type:
  /// either new fields (optionally with directives), or just directives.
  #[inline]
  pub const fn content(&self) -> &InputObjectTypeExtensionContent<Directives, FieldsDefinition> {
    &self.content
  }

  /// Creates a parser that can parse a complete input object extension.
  ///
  /// This parser handles the full input object extension syntax including the extend
  /// and input keywords, target input object name, and extension content.
  #[inline]
  pub fn parser_with<'src, I, E, DP, FP>(
    directives_parser: impl Fn() -> DP,
    input_fields_definition_parser: impl Fn() -> FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    Directives: Const<true>,
    FieldsDefinition: Const<true>,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Input::parser())
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()))
      .then(InputObjectTypeExtensionContent::parser_with(
        directives_parser,
        input_fields_definition_parser,
      ))
      .map_with(|(((extend, input), name), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend,
        input,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
