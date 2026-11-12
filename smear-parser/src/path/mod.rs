//! Namespaced-path carrier shared by GraphQL-family dialect ASTs.

use core::{fmt, marker::PhantomData, num::NonZeroUsize};
use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{DisplayHuman, IntoComponents},
};

/// Where a [`Path`](crate::graphqlx::ast::Path) starts resolving: the module it is written in, or a
/// root its first tokens name.
///
/// ```text
/// PathRoot ::= "~" "::"          -- the own package's root module
///            | "^" "::"          -- a dependency package: the first segment names it
///            | ( ".." "::" )+    -- the parent module, once per ".."
/// ```
///
/// A root's span runs from its first token to its last `::`. Which roots make sense where — a
/// `..::` at the package root, say — is analysis's to decide: the parser accepts every root in
/// every path position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum PathRoot<Span = SimpleSpan> {
  /// No root: the path resolves in the current module's scope.
  Relative,
  /// `~::`, the own package's root module.
  Package(Span),
  /// `^::`, a dependency package; the path's first segment names the package.
  Dependency(Span),
  /// `..::`, written `count` times; each one is the parent of the module before it.
  Parent {
    /// From the first `..` to the last `::`.
    span: Span,
    /// How many `..::` the root is written with.
    count: NonZeroUsize,
  },
}

impl<Span> PathRoot<Span> {
  /// Returns the root's span, or `None` for a [`Relative`](Self::Relative) path.
  #[inline]
  pub const fn span(&self) -> Option<&Span> {
    match self {
      Self::Relative => None,
      Self::Package(span) | Self::Dependency(span) | Self::Parent { span, .. } => Some(span),
    }
  }

  /// Returns whether the path has no root and resolves in the current module's scope.
  #[inline]
  pub const fn is_relative(&self) -> bool {
    matches!(self, Self::Relative)
  }
}

/// Writes the root as it is spelled before a path's first segment, `::` included: nothing, `~::`,
/// `^::`, or `..::` once per level.
impl<Span> fmt::Display for PathRoot<Span> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Relative => Ok(()),
      Self::Package(_) => f.write_str("~::"),
      Self::Dependency(_) => f.write_str("^::"),
      Self::Parent { count, .. } => {
        for _ in 0..count.get() {
          f.write_str("..::")?;
        }
        Ok(())
      }
    }
  }
}

/// A `::`-separated path of dialect-branded names, under an optional root.
///
/// A path retains its complete span — which starts at its root's first token when it has one — its
/// [`PathRoot`], and its segments. `segments` is never empty: a parsed path holds at least the name
/// after its root. The collection type is configurable so AST consumers can use an arena-backed or
/// fixed-size representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path<Name, Span = SimpleSpan, Container = Vec<Name>> {
  span: Span,
  root: PathRoot<Span>,
  segments: Container,
  _name: PhantomData<Name>,
}

impl<Name, Span, Container> Path<Name, Span, Container> {
  /// Creates a path from its full span, its root and its name segments.
  #[inline]
  pub const fn new(span: Span, root: PathRoot<Span>, segments: Container) -> Self {
    Self {
      span,
      root,
      segments,
      _name: PhantomData,
    }
  }

  /// Returns the span covering the complete path, root included.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the root the path starts at.
  #[inline]
  pub const fn root(&self) -> &PathRoot<Span> {
    &self.root
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
  type Components = (Span, PathRoot<Span>, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.root, self.segments)
  }
}

impl<Name, Span> From<Name> for Path<Name, Span>
where
  Name: AsSpan<Span>,
  Span: Clone,
{
  /// A one-segment [`Relative`](PathRoot::Relative) path, spanning the name.
  #[inline]
  fn from(name: Name) -> Self {
    Self::new(
      name.as_span().clone(),
      PathRoot::Relative,
      Vec::from([name]),
    )
  }
}

impl<Name, Span, Container> fmt::Display for Path<Name, Span, Container>
where
  Name: fmt::Display,
  Container: AsRef<[Name]>,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&self.root, f)?;
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
