use logosky::{
  LogoStream, Logos, Source, Token,
  chumsky::{Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{error::UnexpectedEndOfInputObjectExtensionError, hints::InputObjectTypeExtensionHint};

use smear_lexer::keywords::{Extend, Input};

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
/// InputObjectTypeDefinition : input Name Directives? InputFieldsDefinition?
/// ```
///
/// Spec: [Input Object Type Definition](https://spec.graphql.org/draft/#sec-Input-Object-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct InputObjectTypeDefinition<Name, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
  fields: Option<FieldsDefinition>,
}

impl<Name, Directives, FieldsDefinition> AsSpan<Span>
  for InputObjectTypeDefinition<Name, Directives, FieldsDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, FieldsDefinition> IntoSpan<Span>
  for InputObjectTypeDefinition<Name, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, FieldsDefinition> IntoComponents
  for InputObjectTypeDefinition<Name, Directives, FieldsDefinition>
{
  type Components = (Span, Name, Option<Directives>, Option<FieldsDefinition>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives, self.fields)
  }
}

impl<Name, Directives, FieldsDefinition>
  InputObjectTypeDefinition<Name, Directives, FieldsDefinition>
{
  /// Creates a new `InputObjectTypeDefinition` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    directives: Option<Directives>,
    fields: Option<FieldsDefinition>,
  ) -> Self {
    Self {
      span,
      name,
      directives,
      fields,
    }
  }

  /// Returns a reference to the span covering the entire input object definition.
  ///
  /// The span includes the optional description, input keyword, name, optional
  /// directives, and optional fields definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the input object type.
  ///
  /// This is the identifier that will be used to reference this input object type
  /// in other parts of the schema and in GraphQL operations. Input object type
  /// names must be unique within the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
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
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.fields.as_ref()
  }

  /// Creates a parser that can parse a complete input object definition.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, DP, FP>(
    name_parser: NP,
    directives_parser: DP,
    input_fields_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Input: Parseable<'src, I, T, Error> + 'src,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Input::parser()
      .ignore_then(Self::content_parser_with(
        name_parser,
        directives_parser,
        input_fields_definition_parser,
      ))
      .map_with(|(name, directives, fields), exa| Self::new(exa.span(), name, directives, fields))
  }

  /// Creates a parser for input object type definitions without the leading `input` keyword.
  #[inline]
  pub fn content_parser_with<'src, I, T, Error, E, NP, DP, FP>(
    name_parser: NP,
    directives_parser: DP,
    input_fields_definition_parser: FP,
  ) -> impl Parser<'src, I, (Name, Option<Directives>, Option<FieldsDefinition>), E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    name_parser
      .then(directives_parser.or_not())
      .then(input_fields_definition_parser.or_not())
      .map(|((name, directives), fields)| (name, directives, fields))
  }
}

impl<'a, Name, Directives, FieldsDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for InputObjectTypeDefinition<Name, Directives, FieldsDefinition>
where
  Name: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
  Input: Parseable<'a, I, T, Error>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(
      <Name as Parseable<I, T, Error>>::parser(),
      <Directives as Parseable<I, T, Error>>::parser(),
      <FieldsDefinition as Parseable<I, T, Error>>::parser(),
    )
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
pub enum InputObjectTypeExtensionData<Directives, FieldsDefinition> {
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

impl<Directives, FieldsDefinition> InputObjectTypeExtensionData<Directives, FieldsDefinition> {
  /// Returns the directives associated with this extension content, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns the fields definition associated with this extension content, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    match self {
      Self::Directives(_) => None,
      Self::Fields { fields, .. } => Some(fields),
    }
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
pub struct InputObjectTypeExtension<Name, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  data: InputObjectTypeExtensionData<Directives, FieldsDefinition>,
}

impl<Name, Directives, FieldsDefinition> AsSpan<Span>
  for InputObjectTypeExtension<Name, Directives, FieldsDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, FieldsDefinition> IntoSpan<Span>
  for InputObjectTypeExtension<Name, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, FieldsDefinition> IntoComponents
  for InputObjectTypeExtension<Name, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Name,
    InputObjectTypeExtensionData<Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.data)
  }
}

impl<Name, Directives, FieldsDefinition>
  InputObjectTypeExtension<Name, Directives, FieldsDefinition>
{
  /// Creates a new `InputObjectTypeExtension` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: InputObjectTypeExtensionData<Directives, FieldsDefinition>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns a reference to the span covering the entire input object extension.
  ///
  /// The span includes the extend keyword, input keyword, name, and all
  /// extension content (directives and/or input fields).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the input object type being extended.
  ///
  /// This must match the name of an existing input object type in the schema
  /// for the extension to be valid.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns directives associated with the extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the fields definition associated with the extension, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.data.fields_definition()
  }

  /// Returns a reference to the extension data.
  ///
  /// The content specifies what is being added to the input object type:
  /// either new fields (optionally with directives), or just directives.
  #[inline]
  pub const fn data(&self) -> &InputObjectTypeExtensionData<Directives, FieldsDefinition> {
    &self.data
  }

  /// Creates a parser that can parse a complete input object extension.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, NP, DP, FP>(
    name_parser: NP,
    directives_parser: DP,
    input_fields_definition_parser: FP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: UnexpectedEndOfInputObjectExtensionError + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Extend: Parseable<'a, I, T, Error> + 'a,
    Input: Parseable<'a, I, T, Error> + 'a,
    NP: Parser<'a, I, Name, E> + Clone,
    FP: Parser<'a, I, FieldsDefinition, E> + Clone,
    DP: Parser<'a, I, Directives, E> + Clone,
  {
    Extend::parser()
      .then(Input::parser())
      .ignore_then(Self::content_parser_with(
        name_parser,
        directives_parser,
        input_fields_definition_parser,
      ))
      .map_with(|(name, data), exa| Self::new(exa.span(), name, data))
  }

  /// Creates a parser for input object type extensions without the leading `extend` and `input` keywords.
  #[inline]
  pub fn content_parser_with<'a, I, T, Error, E, NP, DP, FP>(
    name_parser: NP,
    directives_parser: DP,
    input_fields_definition_parser: FP,
  ) -> impl Parser<
    'a,
    I,
    (
      Name,
      InputObjectTypeExtensionData<Directives, FieldsDefinition>,
    ),
    E,
  > + Clone
  where
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: UnexpectedEndOfInputObjectExtensionError + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    NP: Parser<'a, I, Name, E> + Clone,
    FP: Parser<'a, I, FieldsDefinition, E> + Clone,
    DP: Parser<'a, I, Directives, E> + Clone,
  {
    name_parser
      .then(directives_parser.or_not())
      .then(input_fields_definition_parser.or_not())
      .try_map_with(|((name, directives), fields), exa| {
        let data = match (directives, fields) {
          (directives, Some(fields)) => InputObjectTypeExtensionData::Fields { directives, fields },
          (Some(directives), None) => InputObjectTypeExtensionData::Directives(directives),
          (None, None) => {
            return Err(Error::unexpected_end_of_input_object_extension(
              exa.span(),
              InputObjectTypeExtensionHint::DirectivesOrInputFieldsDefinition,
            ));
          }
        };
        Ok((name, data))
      })
  }
}

impl<'a, Name, Directives, FieldsDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for InputObjectTypeExtension<Name, Directives, FieldsDefinition>
where
  Error: UnexpectedEndOfInputObjectExtensionError,
  Name: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
  Extend: Parseable<'a, I, T, Error>,
  Input: Parseable<'a, I, T, Error>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: UnexpectedEndOfInputObjectExtensionError + 'a,
  {
    Self::parser_with(
      <Name as Parseable<I, T, Error>>::parser(),
      <Directives as Parseable<I, T, Error>>::parser(),
      <FieldsDefinition as Parseable<I, T, Error>>::parser(),
    )
  }
}
