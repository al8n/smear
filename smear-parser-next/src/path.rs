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
mod tests {
  use tokora::{
    SimpleSpan,
    span::{AsSpan, IntoSpan},
    types::Ident,
    utils::IntoComponents,
  };

  use super::Path;

  #[test]
  fn path_projects_and_consumes_segments() {
    let path = Path::new(SimpleSpan::new(0, 7), vec!["one", "two"], true);
    assert_eq!(path.segments(), &["one", "two"]);
    assert!(path.is_fully_qualified());
    assert_eq!(path.to_string(), "::one::two");
    assert_eq!(path.as_span(), &SimpleSpan::new(0, 7));
    assert_eq!(path.clone().into_span(), SimpleSpan::new(0, 7));
    assert_eq!(
      path.clone().into_components(),
      (SimpleSpan::new(0, 7), true, vec!["one", "two"])
    );
    assert_eq!(path.into_segments(), vec!["one", "two"]);
  }

  #[test]
  fn one_name_path_inherits_the_name_span() {
    let name = Ident::<_, _, ()>::new(SimpleSpan::new(4, 8), "Item");
    let path: Path<_> = name.into();
    assert_eq!(path.segments().len(), 1);
    assert_eq!(path.span(), &SimpleSpan::new(4, 8));
    assert!(!path.is_fully_qualified());
  }
}
