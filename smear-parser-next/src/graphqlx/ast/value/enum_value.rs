use derive_more::{From, Into};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::super::{DefaultVec, Path};
use crate::ident::Ident;

/// An enum value in GraphQLx.
///
/// Unlike GraphQL, a GraphQLx enum value is a [`Path`] (`a::b::C`), so it wraps the
/// shared [`EnumValue`](crate::value::EnumValue) carrier over `Path<S>` rather than
/// a bare source slice.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct EnumValue<S>(crate::value::EnumValue<Path<S>>);

impl<S> AsSpan<Span> for EnumValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for EnumValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for EnumValue<S> {
  type Components = (Span, bool, DefaultVec<Ident<S>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    let (_, path) = self.0.into_components();
    path.into_components()
  }
}

impl<S> EnumValue<S> {
  /// Creates a new enum value from its `::`-path.
  #[inline]
  pub(crate) const fn new(path: Path<S>) -> Self {
    Self(crate::value::EnumValue::new(*path.span(), path))
  }

  /// Returns the span of the enum value.
  #[inline]
  pub fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the `::`-path of the enum value.
  #[inline]
  pub const fn value(&self) -> &Path<S> {
    self.0.source_ref()
  }
}
