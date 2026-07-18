use derive_more::{From, Into};
use smear_lexer::graphqlx::LitFloat;
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

type FloatValueAlias<S> = crate::value::FloatValue<LitFloat<S>>;

/// A floating-point value in GraphQLx.
///
/// Wraps the shared [`FloatValue`](crate::value::FloatValue) carrier over the
/// radix-preserving [`LitFloat`] payload (decimal / hex), so the original radix
/// survives into the AST.
#[derive(Debug, Clone, Copy, From, Into)]
pub struct FloatValue<S>(FloatValueAlias<S>);

impl<S> AsSpan<Span> for FloatValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for FloatValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for FloatValue<S> {
  type Components = (Span, LitFloat<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> FloatValue<S> {
  /// Creates a new float value from its span and radix-preserving payload.
  #[inline]
  pub(crate) const fn new(span: Span, value: LitFloat<S>) -> Self {
    Self(FloatValueAlias::new(span, value))
  }

  /// Returns a reference to the span covering the entire float value.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the literal float value reference.
  #[inline]
  pub const fn value_ref(&self) -> &LitFloat<S> {
    self.0.source_ref()
  }

  /// Returns the float value.
  #[inline]
  pub const fn value(self) -> LitFloat<S>
  where
    S: Copy,
  {
    self.0.source()
  }
}
