use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

use crate::cst::Padding;

/// CST representation of a GraphQL list type with optional non-null modifier.
///
/// Unlike the AST version, preserves:
/// - Left bracket `[` with its padding
/// - The element type with its padding
/// - Right bracket `]` with its padding
/// - Optional bang `!` with its padding
///
/// ## Grammar
/// ```text
/// ListType : [ Type ] !?
/// ```
///
/// ## Examples
/// ```text
/// [String]         # nullable list
/// [String!]!       # non-null list of non-null items
/// [ String ]       # preserves internal spacing
/// ```
#[derive(Debug, Clone)]
pub struct ListType<Type, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the left bracket
  lbracket_padding: Padding<S, TriviaContainer>,
  /// The element type
  ty: Type,
  /// Padding around the right bracket
  rbracket_padding: Padding<S, TriviaContainer>,
  /// Optional bang token with its padding
  bang: Option<Padding<S, TriviaContainer>>,
}

impl<Type, S, TriviaContainer> AsSpan<Span> for ListType<Type, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, S, TriviaContainer> IntoSpan<Span> for ListType<Type, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, S, TriviaContainer> IntoComponents for ListType<Type, S, TriviaContainer> {
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Type,
    Padding<S, TriviaContainer>,
    Option<Padding<S, TriviaContainer>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.lbracket_padding,
      self.ty,
      self.rbracket_padding,
      self.bang,
    )
  }
}

impl<Type, S, TriviaContainer> ListType<Type, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST ListType.
  pub fn new(span: Span, ty: Type) -> Self {
    Self {
      span,
      lbracket_padding: Padding::new(),
      ty,
      rbracket_padding: Padding::new(),
      bang: None,
    }
  }

  /// Creates a new CST ListType with all components.
  pub const fn with_parts(
    span: Span,
    lbracket_padding: Padding<S, TriviaContainer>,
    ty: Type,
    rbracket_padding: Padding<S, TriviaContainer>,
    bang: Option<Padding<S, TriviaContainer>>,
  ) -> Self {
    Self {
      span,
      lbracket_padding,
      ty,
      rbracket_padding,
      bang,
    }
  }
}

impl<Type, S, TriviaContainer> ListType<Type, S, TriviaContainer> {
  /// Returns the span covering the entire list type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left bracket padding.
  #[inline]
  pub const fn lbracket_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lbracket_padding
  }

  /// Returns a reference to the element type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns a reference to the right bracket padding.
  #[inline]
  pub const fn rbracket_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rbracket_padding
  }

  /// Returns a reference to the bang padding, if present.
  #[inline]
  pub const fn bang(&self) -> Option<&Padding<S, TriviaContainer>> {
    self.bang.as_ref()
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub const fn required(&self) -> bool {
    self.bang.is_some()
  }
}
