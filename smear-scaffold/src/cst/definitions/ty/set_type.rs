use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

use crate::cst::Padding;

/// CST representation of a GraphQL set type: `<Type>!?`
///
/// Preserves all tokens including angle brackets and bang.
///
/// ## Grammar
/// ```text
/// SetType : < Type > !?
/// ```
#[derive(Debug, Clone)]
pub struct SetType<Type, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the left angle bracket
  langle_padding: Padding<S, TriviaContainer>,
  /// The element type
  ty: Type,
  /// Padding around the right angle bracket
  rangle_padding: Padding<S, TriviaContainer>,
  /// Optional bang token with its padding
  bang: Option<Padding<S, TriviaContainer>>,
}

impl<Type, S, TriviaContainer> AsSpan<Span> for SetType<Type, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, S, TriviaContainer> IntoSpan<Span> for SetType<Type, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, S, TriviaContainer> IntoComponents for SetType<Type, S, TriviaContainer> {
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
      self.langle_padding,
      self.ty,
      self.rangle_padding,
      self.bang,
    )
  }
}

impl<Type, S, TriviaContainer> SetType<Type, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST SetType.
  pub fn new(span: Span, ty: Type) -> Self {
    Self {
      span,
      langle_padding: Padding::new(),
      ty,
      rangle_padding: Padding::new(),
      bang: None,
    }
  }

  /// Creates a new CST SetType with all components.
  pub const fn with_parts(
    span: Span,
    langle_padding: Padding<S, TriviaContainer>,
    ty: Type,
    rangle_padding: Padding<S, TriviaContainer>,
    bang: Option<Padding<S, TriviaContainer>>,
  ) -> Self {
    Self {
      span,
      langle_padding,
      ty,
      rangle_padding,
      bang,
    }
  }
}

impl<Type, S, TriviaContainer> SetType<Type, S, TriviaContainer> {
  /// Returns the span covering the entire set type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left angle bracket padding.
  #[inline]
  pub const fn langle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.langle_padding
  }

  /// Returns a reference to the element type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns a reference to the right angle bracket padding.
  #[inline]
  pub const fn rangle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rangle_padding
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
