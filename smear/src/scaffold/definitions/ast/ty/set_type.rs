use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{Bang, LAngle, RAngle};

/// Represents a GraphQLx set type with optional non-null modifier.
///
/// Set types represent arrays or collections of values in GraphQL. They wrap
/// another type (the element type) to indicate that fields of this type return
/// multiple values of the wrapped type.
///
/// Set types support complex nullability semantics:
/// - The set itself can be null or non-null
/// - The elements within the set can be null or non-null
/// - These nullability rules are independent and composable
///
/// ## Examples
///
/// ```text
/// # Nullable set of nullable strings
/// <String>         # Can be null, or a set containing strings and nulls
///
/// # Non-null set of nullable strings
/// <String>!        # Must be a set (never null), but can contain nulls
///
/// # Nullable set of non-null strings
/// <String!>        # Can be null, or a set containing only strings (no nulls)
///
/// # Non-null set of non-null strings
/// <String!>!       # Must be a set containing only strings (no nulls anywhere)
///
/// # Nested set types
/// <<String>>       # Set of sets of strings
/// <User!>!         # Non-null set of non-null User objects
/// <<String!>!>!    # Non-null set of non-null sets of non-null strings
/// ```
///
/// ## Nullability Combinations
///
/// | Type Syntax | Set Nullability | Element Nullability |
/// |-------------|------------------|---------------------|
/// | `<String>`  | Nullable | Nullable |
/// | `<String>!` | Non-null | Nullable |
/// | `<String!>` | Nullable | Non-null |
/// | `<String!>!`| Non-null | Non-null |
///
/// ## Use Cases
/// - **Collections**: Arrays of objects, IDs, or scalar values
/// - **Relationships**: One-to-many relationships in object types
/// - **Batch Operations**: Multiple inputs or outputs in mutations
/// - **Search Results**: Variable-length result sets
/// - **Tags/Categories**: Multiple classifications or labels
///
/// ## Grammar
/// ```text
/// SetType : < Type > !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct SetType<Type> {
  span: Span,
  ty: Type,
  required: bool,
}

impl<Type> AsSpan<Span> for SetType<Type> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type> IntoSpan<Span> for SetType<Type> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type> IntoComponents for SetType<Type> {
  type Components = (Span, Type, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ty, self.required)
  }
}

impl<Type> SetType<Type> {
  #[inline]
  pub(crate) const fn new(span: Span, ty: Type, required: bool) -> Self {
    Self { span, ty, required }
  }

  /// Returns a reference to the span covering the entire set type.
  ///
  /// The span includes the brackets, element type, and optional bang modifier.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the element type contained within the set.
  ///
  /// This is the type of individual elements in the set. It can be any
  /// valid GraphQLx type including named types, other set types, or even
  /// nested set types for multi-dimensional arrays.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns whether the set type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }

  /// Creates a parser for set types using the provided element type parser.
  ///
  /// This parser handles the complete set type syntax including brackets,
  /// element type parsing, and optional bang modifier. The element type
  /// parsing is delegated to the provided parser for flexibility.
  ///
  /// ## Parameters
  /// - `parser`: Parser for the element type within the set
  ///
  /// ## Grammar Handled
  /// ```text
  /// SetType : [ Type ] !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// <String>        # Nullable set of nullable strings
  /// <String!>!      # Non-null set of non-null strings
  /// <<User>>        # Nested set type
  /// <ID!>           # Nullable set of non-null IDs
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, P>(parser: P) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    P: Parser<'a, I, Type, E> + Clone,
  {
    LAngle::parser()
      .ignore_then(parser)
      .then_ignore(RAngle::parser())
      .then(Bang::parser().or_not())
      .map_with(|(ty, bang), exa| Self {
        span: exa.span(),
        ty,
        required: bang.is_some(),
      })
  }
}

impl<'a, Type, I, T, Error> Parseable<'a, I, T, Error> for SetType<Type>
where
  Type: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
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
    Self::parser_with(Type::parser())
  }
}
