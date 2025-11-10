use core::marker::PhantomData;

use logosky::{
  Logos, Source, Token,
  chumsky::{LogoStream, Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, syntax::AstNode},
};

use smear_lexer::punctuator::{Bang, LBracket, RBracket};

/// Represents a GraphQL list type with optional non-null modifier.
///
/// List types represent arrays or collections of values in GraphQL. They wrap
/// another type (the element type) to indicate that fields of this type return
/// multiple values of the wrapped type.
///
/// List types support complex nullability semantics:
/// - The list itself can be null or non-null
/// - The elements within the list can be null or non-null
/// - These nullability rules are independent and composable
///
/// ## Examples
///
/// ```text
/// # Nullable list of nullable strings
/// [String]         # Can be null, or a list containing strings and nulls
///
/// # Non-null list of nullable strings
/// [String]!        # Must be a list (never null), but can contain nulls
///
/// # Nullable list of non-null strings
/// [String!]        # Can be null, or a list containing only strings (no nulls)
///
/// # Non-null list of non-null strings
/// [String!]!       # Must be a list containing only strings (no nulls anywhere)
///
/// # Nested list types
/// [[String]]       # List of lists of strings
/// [User!]!         # Non-null list of non-null User objects
/// [[String!]!]!    # Non-null list of non-null lists of non-null strings
/// ```
///
/// ## Nullability Combinations
///
/// | Type Syntax | List Nullability | Element Nullability | Example Values |
/// |-------------|------------------|---------------------|----------------|
/// | `[String]`  | Nullable | Nullable | `null`, `["a", null, "c"]` |
/// | `[String]!` | Non-null | Nullable | `["a", null, "c"]`, `[]` |
/// | `[String!]` | Nullable | Non-null | `null`, `["a", "b", "c"]` |
/// | `[String!]!`| Non-null | Non-null | `["a", "b", "c"]`, `[]` |
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
/// ListType : [ Type ] !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ListType<Type, Lang = ()> {
  span: Span,
  ty: Type,
  required: bool,
  _lang: PhantomData<Lang>,
}

impl<Type, Lang> AsSpan<Span> for ListType<Type, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, Lang> IntoSpan<Span> for ListType<Type, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, Lang> IntoComponents for ListType<Type, Lang> {
  type Components = (Span, Type, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ty, self.required)
  }
}

impl<Type, Lang> ListType<Type, Lang> {
  /// Returns a reference to the span covering the entire list type.
  ///
  /// The span includes the brackets, element type, and optional bang modifier.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the element type contained within the list.
  ///
  /// This is the type of individual elements in the list. It can be any
  /// valid GraphQL type including named types, other list types, or even
  /// nested list types for multi-dimensional arrays.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns whether the list type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }

  /// Creates a parser for list types using the provided element type parser.
  ///
  /// This parser handles the complete list type syntax including brackets,
  /// element type parsing, and optional bang modifier. The element type
  /// parsing is delegated to the provided parser for flexibility.
  ///
  /// ## Parameters
  /// - `parser`: Parser for the element type within the list
  ///
  /// ## Grammar Handled
  /// ```text
  /// ListType : [ Type ] !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// [String]        # Nullable list of nullable strings
  /// [String!]!      # Non-null list of non-null strings
  /// [[User]]        # Nested list type
  /// [ID!]           # Nullable list of non-null IDs
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, P>(parser: P) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBracket: Parseable<'a, I, T, Error> + 'a,
    RBracket: Parseable<'a, I, T, Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    P: Parser<'a, I, Type, E> + Clone,
  {
    LBracket::parser()
      .ignore_then(parser)
      .then_ignore(RBracket::parser())
      .then(Bang::parser().or_not())
      .map_with(|(ty, bang), exa| Self {
        span: exa.span(),
        ty,
        required: bang.is_some(),
        _lang: PhantomData,
      })
  }
}

impl<'a, Type, I, T, Error, Lang> Parseable<'a, I, T, Error> for ListType<Type, Lang>
where
  Type: Parseable<'a, I, T, Error>,
  LBracket: Parseable<'a, I, T, Error> + 'a,
  RBracket: Parseable<'a, I, T, Error> + 'a,
  Bang: Parseable<'a, I, T, Error> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Type::parser())
  }
}

// ============================================================================
// AstNode implementations (GraphQL)
// ============================================================================

/// Implements `AstNode` for `ListType` in the GraphQL language.
///
/// This associates the list type AST node with its corresponding syntax type,
/// enabling type-safe error handling and generic parser implementation.
impl<Type, Lang> AstNode<smear_lexer::graphql::GraphQL> for ListType<Type, Lang> {
  type Syntax = crate::syntax::graphql::ListTypeSyntax;
}
