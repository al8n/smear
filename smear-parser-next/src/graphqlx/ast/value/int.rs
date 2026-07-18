use derive_more::{From, Into};
use smear_lexer::graphqlx::LitInt;
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

type IntValueAlias<S> = crate::value::IntValue<LitInt<S>>;

/// An integer value in GraphQLx.
///
/// Wraps the shared [`IntValue`](crate::value::IntValue) carrier over the
/// radix-preserving [`LitInt`] payload (decimal / hex / binary / octal), so the
/// original radix survives into the AST.
#[derive(Debug, Clone, Copy, From, Into)]
pub struct IntValue<S>(IntValueAlias<S>);

impl<S> AsSpan<Span> for IntValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for IntValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for IntValue<S> {
  type Components = (Span, LitInt<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> IntValue<S> {
  /// Creates a new integer value from its span and radix-preserving payload.
  #[inline]
  pub(crate) const fn new(span: Span, value: LitInt<S>) -> Self {
    Self(IntValueAlias::new(span, value))
  }

  /// Returns a reference to the span covering the entire integer value.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the literal integer value reference.
  #[inline]
  pub const fn value_ref(&self) -> &LitInt<S> {
    self.0.source_ref()
  }

  /// Returns the integer value.
  #[inline]
  pub const fn value(self) -> LitInt<S>
  where
    S: Copy,
  {
    self.0.source()
  }
}
