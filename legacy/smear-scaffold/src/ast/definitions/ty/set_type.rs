use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

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
  /// Creates a new set type instance.
  #[inline]
  pub const fn new(span: Span, ty: Type, required: bool) -> Self {
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
}
