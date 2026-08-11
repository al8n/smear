//! Namespaced-path carrier shared by GraphQL-family dialect ASTs.

use core::{fmt, marker::PhantomData};
use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents, human_display::DisplayHuman},
};

/// A `::`-separated path of dialect-branded names.
///
/// A path retains its complete span and whether it began with the leading
/// `::` that marks it as fully qualified. The collection type is configurable
/// so AST consumers can use an arena-backed or fixed-size representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path<Name, Span = SimpleSpan, Container = Vec<Name>> {
  span: Span,
  segments: Container,
  fully_qualified: bool,
  _name: PhantomData<Name>,
}

impl<Name, Span, Container> Path<Name, Span, Container> {
  /// Creates a path from its full span, name segments, and qualification flag.
  #[inline]
  pub const fn new(span: Span, segments: Container, fully_qualified: bool) -> Self {
    Self {
      span,
      segments,
      fully_qualified,
      _name: PhantomData,
    }
  }

  /// Returns the span covering the complete path.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the path segments as a slice.
  #[inline]
  pub fn segments(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.segments.as_ref()
  }

  /// Consumes the path and returns its segment container.
  #[inline]
  pub fn into_segments(self) -> Container {
    self.segments
  }

  /// Returns whether this path has a leading `::` qualifier.
  #[inline]
  pub const fn is_fully_qualified(&self) -> bool {
    self.fully_qualified
  }
}

impl<Name, Span, Container> AsSpan<Span> for Path<Name, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span, Container> IntoSpan<Span> for Path<Name, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span, Container> IntoComponents for Path<Name, Span, Container> {
  type Components = (Span, bool, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.fully_qualified, self.segments)
  }
}

impl<Name, Span> From<Name> for Path<Name, Span>
where
  Name: AsSpan<Span>,
  Span: Clone,
{
  #[inline]
  fn from(name: Name) -> Self {
    Self::new(name.as_span().clone(), Vec::from([name]), false)
  }
}

impl<Name, Span, Container> fmt::Display for Path<Name, Span, Container>
where
  Name: fmt::Display,
  Container: AsRef<[Name]>,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.fully_qualified {
      f.write_str("::")?;
    }
    for (index, segment) in self.segments().iter().enumerate() {
      if index != 0 {
        f.write_str("::")?;
      }
      segment.fmt(f)?;
    }
    Ok(())
  }
}

impl<Name, Span, Container> DisplayHuman for Path<Name, Span, Container>
where
  Name: fmt::Display,
  Container: AsRef<[Name]>,
{
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self, f)
  }
}

#[cfg(test)]
mod tests;
