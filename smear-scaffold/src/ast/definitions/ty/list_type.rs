use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};


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
pub struct ListType<Type> {
  span: Span,
  ty: Type,
  required: bool,
}

impl<Type> AsSpan<Span> for ListType<Type> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type> IntoSpan<Span> for ListType<Type> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type> IntoComponents for ListType<Type> {
  type Components = (Span, Type, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ty, self.required)
  }
}

impl<Type> ListType<Type> {
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
}
