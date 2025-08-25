use core::marker::PhantomData;

use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{
    ignored,
    punct::{Colon, LBrace, RBrace},
    Const, Name, StringValue,
  },
  source::{Char, Slice, Source},
};

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
/// FieldDefinition : Description? Name ArgumentsDefinition? : Type Directives?
/// ```
///
/// Spec: [Field Definition](https://spec.graphql.org/draft/#sec-Field-Definition)
#[derive(Debug, Clone, Copy)]
pub struct FieldDefinition<Args, Type, Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  name: Name<Span>,
  arguments_definition: Option<Args>,
  colon: Colon<Span>,
  ty: Type,
  directives: Option<Directives>,
}

impl<Args, Type, Directives, Span> AsRef<Span> for FieldDefinition<Args, Type, Directives, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Args, Type, Directives, Span> IntoSpan<Span>
  for FieldDefinition<Args, Type, Directives, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Args, Type, Directives, Span> IntoComponents
  for FieldDefinition<Args, Type, Directives, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    Name<Span>,
    Option<Args>,
    Colon<Span>,
    Type,
    Option<Directives>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.name,
      self.arguments_definition,
      self.colon,
      self.ty,
      self.directives,
    )
  }
}

impl<Args, Type, Directives, Span> FieldDefinition<Args, Type, Directives, Span> {
  /// Returns a reference to the span covering the entire field definition.
  ///
  /// The span includes the optional description, field name, optional arguments,
  /// colon, type, and optional directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the field definition.
  ///
  /// The description provides documentation for the field and appears before
  /// the field name. It can be either a single-line string or a block string.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the field name.
  ///
  /// This is the identifier that will be used to select this field in
  /// GraphQL operations. Field names must be valid GraphQL identifiers.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the optional arguments definition.
  ///
  /// The arguments definition specifies what parameters can be provided
  /// when selecting this field in GraphQL operations. Fields can have
  /// zero or more arguments with names, types, and optional default values.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Args> {
    self.arguments_definition.as_ref()
  }

  /// Returns a reference to the colon separator between field name and type.
  ///
  /// The colon is a required part of field definition syntax that separates
  /// the field identifier from its type specification.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
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
  pub fn parser_with<'src, I, E, AP, TP, DP>(
    args_definition_parser: AP,
    type_parser: TP,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    Args: Const<true>,
    Directives: Const<true>,
    AP: Parser<'src, I, Args, E> + Clone,
    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then(Name::parser().padded_by(ignored()))
      .then(args_definition_parser.or_not())
      .then(Colon::parser().padded_by(ignored()))
      .then(type_parser)
      .then(ignored().ignore_then(directives_parser).or_not())
      .map_with(
        |(((((description, name), arguments_definition), colon), ty), directives), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          name,
          arguments_definition,
          colon,
          ty,
          directives,
        },
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
pub struct FieldsDefinition<FieldDefinition, Span, Container = Vec<FieldDefinition>> {
  span: Span,
  l_brace: LBrace<Span>,
  r_brace: RBrace<Span>,
  fields: Container,
  _m: PhantomData<FieldDefinition>,
}

impl<FieldDefinition, Span, Container> AsRef<Span>
  for FieldsDefinition<FieldDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<FieldDefinition, Span, Container> IntoSpan<Span>
  for FieldsDefinition<FieldDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<FieldDefinition, Span, Container> IntoComponents
  for FieldsDefinition<FieldDefinition, Span, Container>
{
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.fields, self.r_brace)
  }
}

impl<FieldDefinition, Span, Container> FieldsDefinition<FieldDefinition, Span, Container> {
  /// Returns a reference to the span covering the entire fields definition.
  ///
  /// The span includes the opening brace, all field definitions, and the closing brace.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the opening left brace (`{`) of the fields definition.
  ///
  /// This provides access to the exact location and span information of the
  /// opening delimiter.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns a reference to the closing right brace (`}`) of the fields definition.
  ///
  /// This provides access to the exact location and span information of the
  /// closing delimiter.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Returns a reference to the container holding all field definitions.
  ///
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of field definitions.
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }

  /// Consumes the fields definition and returns the underlying container of field definitions.
  ///
  /// This method takes ownership of the entire `FieldsDefinition` structure and
  /// extracts just the container holding the field collection.
  #[inline]
  pub fn into_fields(self) -> Container {
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
  pub fn parser_with<'src, I, E, P>(
    field_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    FieldDefinition: Const<true>,
    P: Parser<'src, I, FieldDefinition, E> + Clone,
    Container: chumsky::container::Container<FieldDefinition>,
  {
    LBrace::parser()
      .then(
        field_definition_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RBrace::parser())
      .map_with(|((l_brace, fields), r_brace), sp| Self {
        span: Span::from_map_extra(sp),
        l_brace,
        r_brace,
        fields,
        _m: PhantomData,
      })
  }
}
