//! Name-node carrier shared by the dialect ASTs.
//!
//! This wrapper distinguishes a dialect's nominal name node from tokora's
//! general-purpose [`Ident`](tokora::types::Ident), while preserving the
//! dialect marker in its type.

use core::ops::{Deref, DerefMut};

use tokora::{
  SimpleSpan,
  error::ErrorNode,
  span::{AsSpan, IntoSpan},
  types::Ident,
  utils::IntoComponents,
};

/// A dialect-branded name identifier.
///
/// Dialect AST assemblies specialize `Lang` to their own marker type. The
/// wrapper otherwise preserves the identifier's source and span types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Name<S: ?Sized, Span = SimpleSpan, Lang: ?Sized = ()>(Ident<S, Span, Lang>);

impl<S, Span, Lang: ?Sized> Name<S, Span, Lang> {
  /// Creates a valid dialect name.
  #[inline]
  pub const fn new(span: Span, source: S) -> Self {
    Self(Ident::new(span, source))
  }

  /// Unwraps this name into the underlying identifier.
  #[inline]
  pub fn into_ident(self) -> Ident<S, Span, Lang> {
    self.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> Deref for Name<S, Span, Lang> {
  type Target = Ident<S, Span, Lang>;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> DerefMut for Name<S, Span, Lang> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> AsRef<Ident<S, Span, Lang>> for Name<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &Ident<S, Span, Lang> {
    &self.0
  }
}

impl<S, Span, Lang: ?Sized> From<Ident<S, Span, Lang>> for Name<S, Span, Lang> {
  #[inline]
  fn from(ident: Ident<S, Span, Lang>) -> Self {
    Self(ident)
  }
}

impl<S, Span, Lang: ?Sized> From<Name<S, Span, Lang>> for Ident<S, Span, Lang> {
  #[inline]
  fn from(name: Name<S, Span, Lang>) -> Self {
    name.0
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> AsSpan<Span> for Name<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for Name<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.into_components().0
  }
}

impl<S, Span, Lang: ?Sized> IntoComponents for Name<S, Span, Lang> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Span, Lang: ?Sized> ErrorNode<Span> for Name<S, Span, Lang>
where
  Ident<S, Span, Lang>: ErrorNode<Span>,
{
  #[inline]
  fn error(span: Span) -> Self {
    Self(Ident::error(span))
  }

  #[inline]
  fn missing(span: Span) -> Self {
    Self(Ident::missing(span))
  }
}

#[cfg(test)]
mod tests {
  use tokora::{span::AsSpan, types::Ident, utils::IntoComponents};

  use super::Name;

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  trait OtherLanguage {}

  #[test]
  fn carrier_preserves_an_arbitrary_unsized_language_marker() {
    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), "field");
    assert_eq!(name.as_span(), &CustomSpan(1));
    assert_eq!(name.source_ref(), &"field");

    let ident: Ident<_, CustomSpan, dyn OtherLanguage> = name.into();
    assert_eq!(ident.as_span(), &CustomSpan(1));

    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(2), "field");
    assert_eq!(name.into_components(), (CustomSpan(2), "field"));
  }
}
