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

/// A dialect-branded fragment name.
///
/// GraphQL-family dialects use this nominal wrapper for the `Name but not on`
/// grammar position. Its constructor is kept within this crate so syntactic
/// productions are the single place that establish that exclusion.
///
/// Vanilla GraphQL fragments use this wrapper; GraphQLx has no fragment
/// grammar.
#[cfg(feature = "graphql")]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FragmentName<S: ?Sized, Span = SimpleSpan, Lang: ?Sized = ()>(Name<S, Span, Lang>);

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> FragmentName<S, Span, Lang> {
  #[inline]
  pub(crate) const fn new(span: Span, source: S) -> Self {
    Self(Name::new(span, source))
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> Deref for FragmentName<S, Span, Lang> {
  type Target = Name<S, Span, Lang>;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> DerefMut for FragmentName<S, Span, Lang> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> AsRef<Name<S, Span, Lang>> for FragmentName<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &Name<S, Span, Lang> {
    &self.0
  }
}

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> PartialEq<S> for FragmentName<S, Span, Lang>
where
  S: PartialEq,
{
  #[inline]
  fn eq(&self, other: &S) -> bool {
    self.source().eq(other)
  }
}

#[cfg(feature = "graphql")]
impl<S: ?Sized, Span, Lang: ?Sized> AsSpan<Span> for FragmentName<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> IntoSpan<Span> for FragmentName<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

#[cfg(feature = "graphql")]
impl<S, Span, Lang: ?Sized> IntoComponents for FragmentName<S, Span, Lang> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Span, Lang: ?Sized> Name<S, Span, Lang> {
  /// Creates a dialect name from a span and a spelling.
  ///
  /// It does not check the spelling, and the name says "valid" only of what the *productions*
  /// build: a name that was lexed is a name, because the lexer's token production is draft
  /// §2.1.9's. Nothing establishes that for a name assembled here, and a consumer that reads one
  /// as text — `graphql_proto`'s response keys, its variable lookups — has to answer that question
  /// itself rather than assume this constructor answered it.
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

impl<S: ?Sized, Span, Lang: ?Sized> Name<S, Span, Lang> {
  /// Returns the name's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    self.0.source_ref()
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
  use std::string::String;

  use tokora::{span::AsSpan, types::Ident, utils::IntoComponents};

  use super::Name;

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  trait OtherLanguage {}

  #[test]
  fn carrier_preserves_an_arbitrary_unsized_language_marker() {
    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), "field");
    assert_eq!(name.as_span(), &CustomSpan(1));
    assert_eq!(name.source(), &"field");

    let ident: Ident<_, CustomSpan, dyn OtherLanguage> = name.into();
    assert_eq!(ident.as_span(), &CustomSpan(1));

    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(2), "field");
    assert_eq!(name.into_components(), (CustomSpan(2), "field"));
  }

  #[test]
  fn source_borrows_non_copy_source() {
    let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), String::from("field"));
    let source: &String = name.source();
    assert_eq!(source, "field");
  }
}
