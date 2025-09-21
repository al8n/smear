use chumsky::{extra::ParserExtra, prelude::*};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::{IntoComponents, IntoSpan};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::lang::punctuator::{Colon, LBrace, RBrace};

/// Represents a single field definition in a GraphQL object, interface, or input type.
///
/// A field definition specifies a field that can be selected in GraphQL operations.
/// It includes the field name, return type, optional arguments, optional description,
/// and optional directives that provide metadata or behavior for the field.
///
/// Field definitions are the building blocks of GraphQL types, defining what data
/// can be queried and how it can be accessed.
///
/// ## Examples
///
/// ```text
/// # Simple field definition
/// name: String
///
/// # Field with description
/// """
/// The user's full name as it appears in the system
/// """
/// name: String
///
/// # Field with arguments
/// posts(first: Int, after: String): [Post!]!
///
/// # Field with directives
/// email: String @deprecated(reason: "Use contactEmail instead")
///
/// # Complex field with all components
/// """
/// Retrieves paginated posts for the user with optional filtering
/// """
/// posts(
///   """
///   Maximum number of posts to return
///   """
///   first: Int = 10
///   
///   """
///   Cursor for pagination
///   """
///   after: String
///   
///   """
///   Filter posts by status
///   """
///   status: PostStatus
/// ): PostConnection! @auth(requires: USER) @rateLimit(max: 100)
/// ```
///
/// ## Type Parameters
///
/// * `Args` - The type representing the field's arguments definition
/// * `Type` - The type representing the field's return type
/// * `Directives` - The type representing directives applied to the field
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// FieldDefinition : Name ArgumentsDefinition? : Type Directives?
/// ```
///
/// Spec: [Field Definition](https://spec.graphql.org/draft/#sec-Field-Definition)
#[derive(Debug, Clone, Copy)]
pub struct FieldDefinition<Name, Arguments, Type, Directives> {
  span: Span,
  name: Name,
  arguments_definition: Option<Arguments>,
  ty: Type,
  directives: Option<Directives>,
}

impl<Name, Arguments, Type, Directives> AsRef<Span>
  for FieldDefinition<Name, Arguments, Type, Directives>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Arguments, Type, Directives> IntoSpan<Span>
  for FieldDefinition<Name, Arguments, Type, Directives>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Arguments, Type, Directives> IntoComponents
  for FieldDefinition<Name, Arguments, Type, Directives>
{
  type Components = (Span, Name, Option<Arguments>, Type, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.arguments_definition,
      self.ty,
      self.directives,
    )
  }
}

impl<Name, Arguments, Type, Directives> FieldDefinition<Name, Arguments, Type, Directives> {
  /// Returns a reference to the span covering the entire field definition.
  ///
  /// The span includes the optional description, field name, optional arguments,
  /// colon, type, and optional directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the field name.
  ///
  /// This is the identifier that will be used to select this field in
  /// GraphQL operations. Field names must be valid GraphQL identifiers.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional arguments definition.
  ///
  /// The arguments definition specifies what parameters can be provided
  /// when selecting this field in GraphQL operations. Fields can have
  /// zero or more arguments with names, types, and optional default values.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Arguments> {
    self.arguments_definition.as_ref()
  }

  /// Returns a reference to the field's return type.
  ///
  /// The type specifies what kind of data this field returns when selected.
  /// It can be a scalar type, object type, interface, union, enum, or a
  /// wrapper type (list or non-null).
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns a reference to the optional directives applied to this field.
  ///
  /// Directives provide metadata or specify behavior for the field,
  /// such as deprecation information, access control, or custom processing.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Creates a parser that can parse a complete field definition.
  ///
  /// This parser handles the full field definition syntax including all optional
  /// components. The parsing of arguments, type, and directives is delegated to
  /// the provided parser functions.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the field definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, NP, AP, TP, DP>(
    name_parser: NP,
    args_definition_parser: AP,
    type_parser: TP,
    directives_parser: DP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Colon: Parseable<'a, I, T, Error> + 'a,
    NP: Parser<'a, I, Name, E> + Clone,
    AP: Parser<'a, I, Arguments, E> + Clone,
    TP: Parser<'a, I, Type, E> + Clone,
    DP: Parser<'a, I, Directives, E> + Clone,
  {
    name_parser
      .then(args_definition_parser.or_not())
      .then_ignore(Colon::parser())
      .then(type_parser)
      .then(directives_parser.or_not())
      .map_with(
        |(((name, arguments_definition), ty), directives), exa| Self {
          span: exa.span(),
          name,
          arguments_definition,
          ty,
          directives,
        },
      )
  }
}

impl<'a, Name, Arguments, Type, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for FieldDefinition<Name, Arguments, Type, Directives>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Name: Parseable<'a, I, T, Error>,
  Arguments: Parseable<'a, I, T, Error>,
  Type: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error> + 'static,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      Name::parser(),
      Arguments::parser(),
      Type::parser(),
      Directives::parser(),
    )
  }
}

/// Represents a collection of field definitions in a GraphQL type definition.
///
/// A fields definition is a braced collection of one or more field definitions
/// that specify what fields are available on an object type, interface type,
/// or input type. This structure maintains source span information and provides
/// access to all individual field definitions.
///
/// Fields definitions are required for object and interface types, and define
/// the complete API surface that GraphQL operations can access.
///
/// ## Examples
///
/// ```text
/// # Simple fields definition
/// {
///   id: ID!
///   name: String
///   email: String
/// }
///
/// # Fields with descriptions and arguments
/// {
///   """
///   The unique identifier for the user
///   """
///   id: ID!
///   
///   """
///   The user's display name
///   """
///   name: String @deprecated(reason: "Use fullName instead")
///   
///   """
///   Get the user's posts with pagination support
///   """
///   posts(
///     first: Int = 10
///     after: String
///   ): PostConnection! @auth(requires: USER)
///   
///   """
///   The user's profile information
///   """
///   profile: UserProfile
/// }
/// ```
///
/// ## Type Parameters
///
/// * `FieldDefinition` - The type representing individual field definitions
/// * `Span` - The type representing source location information
/// * `Container` - The container type for storing field definitions (defaults to `Vec<FieldDefinition>`)
///
/// ## Grammar
///
/// ```text
/// FieldsDefinition : { FieldDefinition+ }
/// ```
///
/// Note: At least one field definition is required (the `+` indicates one-or-more).
/// Empty fields definitions `{}` are not valid in GraphQL.
///
/// Spec: [Fields Definition](https://spec.graphql.org/draft/#sec-Fields-Definition)
#[derive(Debug, Clone, Copy)]
pub struct FieldsDefinition<FieldDefinition, Container = Vec<FieldDefinition>> {
  span: Span,
  fields: Container,
  _m: PhantomData<FieldDefinition>,
}

impl<FieldDefinition, Container> AsRef<Span> for FieldsDefinition<FieldDefinition, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<FieldDefinition, Container> IntoSpan<Span> for FieldsDefinition<FieldDefinition, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<FieldDefinition, Container> IntoComponents for FieldsDefinition<FieldDefinition, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.fields)
  }
}

impl<FieldDefinition, Container> FieldsDefinition<FieldDefinition, Container> {
  /// Returns a reference to the span covering the entire fields definition.
  ///
  /// The span includes the opening brace, all field definitions, and the closing brace.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all field definitions.
  ///
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of field definitions.
  #[inline]
  pub const fn field_definitions(&self) -> &Container {
    &self.fields
  }

  /// Consumes the fields definition and returns the underlying container of field definitions.
  ///
  /// This method takes ownership of the entire `FieldsDefinition` structure and
  /// extracts just the container holding the field collection.
  #[inline]
  pub fn into_field_definitions(self) -> Container {
    self.fields
  }

  /// Creates a parser that can parse a fields definition with custom field parsing.
  ///
  /// This parser handles the complete fields definition syntax including the braces
  /// and ensures at least one field definition is present.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the fields definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'a, I, T, Error, E, P>(
    field_definition_parser: P,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    P: Parser<'a, I, FieldDefinition, E> + Clone,
    LBrace: Parseable<'a, I, T, Error> + 'a,
    RBrace: Parseable<'a, I, T, Error> + 'a,
    Container: chumsky::container::Container<FieldDefinition>,
  {
    LBrace::parser()
      .ignore_then(field_definition_parser.repeated().at_least(1).collect())
      .then_ignore(RBrace::parser())
      .map_with(|fields, exa| Self {
        span: exa.span(),
        fields,
        _m: PhantomData,
      })
  }
}

impl<'a, FieldDefinition, Container, I, T, Error> Parseable<'a, I, T, Error>
  for FieldsDefinition<FieldDefinition, Container>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  FieldDefinition: Parseable<'a, I, T, Error>,
  LBrace: Parseable<'a, I, T, Error> + 'a,
  RBrace: Parseable<'a, I, T, Error> + 'a,
  Container: chumsky::container::Container<FieldDefinition>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(FieldDefinition::parser())
  }
}
