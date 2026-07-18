//! The [`Ident`] carrier — the identifier node the dialect `Name` types alias.
//!
//! Copied type-only from the frozen `smear-parser` crate (`src/ident.rs`): the
//! span-plus-source identifier every `Name` node is built on, with the span and
//! source accessors and the `str`/`[u8]` equivalence impls the productions and
//! tests lean on. The parser atoms yield tokora's own
//! [`Ident`](tokora::types::Ident); an assembly maps that into this node.

// `new` is the crate-private substrate the Wave 1+ productions mint `Name` nodes
// with; it has no in-crate caller until those productions land.
#![allow(dead_code)]

use core::fmt::Display;

use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{
    IntoComponents,
    cmp::Equivalent,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

/// An identifier.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Ident<V, S = Span> {
  span: S,
  value: V,
}

impl<V, S> Ident<V, S> {
  /// Creates a new identifier with the given span and value.
  ///
  /// The span represents the location of the identifier in the source text,
  /// and the value is the actual source text.
  #[inline]
  pub(crate) const fn new(span: S, value: V) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }

  /// Returns the underlying source value.
  #[inline]
  pub const fn source(&self) -> V
  where
    V: Copy,
  {
    self.value
  }

  /// Returns reference of the underlying source value.
  #[inline(always)]
  pub const fn source_ref(&self) -> &V {
    &self.value
  }
}

impl<V, S> AsSpan<S> for Ident<V, S> {
  #[inline]
  fn as_span(&self) -> &S {
    self.span()
  }
}

impl<V, S> IntoSpan<S> for Ident<V, S> {
  #[inline]
  fn into_span(self) -> S {
    self.span
  }
}

impl<V, S> IntoComponents for Ident<V, S> {
  type Components = (S, V);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<V, S> Display for Ident<V, S>
where
  V: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<V, S> core::ops::Deref for Ident<V, S> {
  type Target = V;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<V, S> DisplayCompact for Ident<V, S>
where
  V: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.display().fmt(f)
  }
}

impl<V, S> DisplayPretty for Ident<V, S>
where
  V: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.display().fmt(f)
  }
}

impl<V, S> PartialEq<str> for Ident<V, S>
where
  str: Equivalent<V>,
{
  #[inline]
  fn eq(&self, other: &str) -> bool {
    other.equivalent(&self.value)
  }
}

impl<V, S> PartialEq<&str> for Ident<V, S>
where
  str: Equivalent<V>,
{
  #[inline]
  fn eq(&self, other: &&str) -> bool {
    other.equivalent(&self.value)
  }
}

impl<V, S> PartialEq<Ident<V, S>> for str
where
  str: Equivalent<V>,
{
  #[inline]
  fn eq(&self, other: &Ident<V, S>) -> bool {
    self.equivalent(&other.value)
  }
}

impl<V, S> PartialEq<Ident<V, S>> for &str
where
  str: Equivalent<V>,
{
  #[inline]
  fn eq(&self, other: &Ident<V, S>) -> bool {
    self.equivalent(&other.value)
  }
}

impl<V, S> PartialEq<[u8]> for Ident<V, S>
where
  [u8]: Equivalent<V>,
{
  #[inline]
  fn eq(&self, other: &[u8]) -> bool {
    other.equivalent(&self.value)
  }
}

impl<V, S> PartialEq<Ident<V, S>> for [u8]
where
  [u8]: Equivalent<V>,
{
  #[inline]
  fn eq(&self, other: &Ident<V, S>) -> bool {
    self.equivalent(&other.value)
  }
}

impl<V, S> Equivalent<Ident<V, S>> for str
where
  str: Equivalent<V>,
{
  #[inline]
  fn equivalent(&self, other: &Ident<V, S>) -> bool {
    self.equivalent(other.source_ref())
  }
}

impl<V, S> Equivalent<Ident<V, S>> for [u8]
where
  [u8]: Equivalent<V>,
{
  #[inline]
  fn equivalent(&self, other: &Ident<V, S>) -> bool {
    self.equivalent(other.source_ref())
  }
}

impl<V, S> Equivalent<[u8]> for Ident<V, S>
where
  [u8]: Equivalent<V>,
{
  #[inline]
  fn equivalent(&self, other: &[u8]) -> bool {
    other.equivalent(self.source_ref())
  }
}
