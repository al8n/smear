use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::Bang;

/// Represents a named GraphQL type with optional non-null modifier.
///
/// Named types are the foundation of GraphQL's type system, referencing
/// concrete types by name. They can represent any defined type in the schema:
/// scalars, objects, interfaces, unions, enums, or input objects.
///
/// The optional bang (`!`) suffix makes the type non-null, meaning it cannot
/// return `null` values and must always provide a valid value of the specified type.
///
/// ## Examples
///
/// ```text
/// # Nullable named types
/// String          # Can be null or a string value
/// User            # Can be null or a User object
/// PostStatus      # Can be null or a PostStatus enum value
///
/// # Non-null named types  
/// String!         # Must be a string value, never null
/// User!           # Must be a User object, never null
/// ID!             # Must be an ID value, never null
///
/// # Usage in field definitions
/// type User {
///   id: ID!                    # Required ID
///   name: String!              # Required name
///   email: String              # Optional email (can be null)
///   avatar: Image              # Optional avatar image
///   status: UserStatus!        # Required status enum
/// }
/// ```
///
/// ## Nullability Semantics
/// - **Without `!`**: Field can return `null` or a valid value
/// - **With `!`**: Field must always return a valid value, never `null`
/// - **Error Handling**: Non-null fields that would return `null` cause query errors
/// - **Schema Evolution**: Adding `!` to existing fields is a breaking change
///
/// ## Grammar
/// ```text
/// NamedType : Name !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct NamedType<Name> {
  span: Span,
  name: Name,
  required: bool,
}

impl<Name> AsSpan<Span> for NamedType<Name> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for NamedType<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for NamedType<Name> {
  type Components = (Span, Name, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.required)
  }
}

impl<Name> NamedType<Name> {
  /// Returns a reference to the span covering the entire named type.
  ///
  /// The span includes the type name and optional bang modifier,
  /// providing complete source location information.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the type.
  ///
  /// This is the identifier that references a type defined elsewhere in the schema.
  /// Type names must follow GraphQL naming conventions and resolve to valid
  /// schema types (scalars, objects, interfaces, unions, enums, or input objects).
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns whether the named type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }

  /// Creates a parser for named types.
  ///
  /// This parser recognizes a type name followed by an optional bang (`!`) modifier.
  /// It handles whitespace between the name and bang appropriately.
  ///
  /// ## Grammar Handled
  /// ```text
  /// NamedType : Name !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// String      # Nullable string type
  /// String!     # Non-null string type
  /// User        # Nullable User object type
  /// ID!         # Non-null ID scalar type
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, NP>(name_parser: NP) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    NP: Parser<'a, I, Name, E> + Clone,
  {
    name_parser
      .then(Bang::parser().or_not())
      .map_with(|(name, bang), exa| Self {
        span: exa.span(),
        name,
        required: bang.is_some(),
      })
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for NamedType<Name>
where
  Name: Parseable<'a, I, T, Error> + 'a,
  Bang: Parseable<'a, I, T, Error> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Name::parser())
  }
}
