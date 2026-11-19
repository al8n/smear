//! Namespaced-path carrier shared by GraphQL-family dialect ASTs.

use core::{fmt, marker::PhantomData, num::NonZeroUsize};
use std::vec::Vec;

use derive_more::{IsVariant, TryUnwrap, Unwrap};
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum PathRoot<Span = SimpleSpan> {
  /// No root: the path resolves in the current module's scope.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Relative,
  /// `~::`, the own package's root module.
  Package(Span),
  /// `^::`, a dependency package; the path's first segment names the package.
  Dependency(Span),
  /// `..::`, written its payload's `count` times; each one is the parent of the module before it.
  Parent(ParentRoot<Span>),
}

impl<Span> PathRoot<Span> {
  /// Returns the root's span, or `None` for a [`Relative`](Self::Relative) path.
  #[inline]
  pub const fn span(&self) -> Option<&Span> {
    match self {
      Self::Relative => None,
      Self::Package(span) | Self::Dependency(span) => Some(span),
      Self::Parent(root) => Some(root.span()),
    }
  }
}

/// `..::`, written [`count`](Self::count) times: the parent of the module a path is written in,
/// once per `..`.
///
/// [`PathRoot::Parent`]'s payload. Built with [`new`](Self::new); edited in place with the `set_*`
/// setters or by value with the `with_*` ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParentRoot<Span = SimpleSpan> {
  pub(crate) span: Span,
  pub(crate) count: NonZeroUsize,
}

impl<Span> ParentRoot<Span> {
  /// Creates the root from its span and how many `..::` it is written with.
  #[inline]
  pub const fn new(span: Span, count: NonZeroUsize) -> Self {
    Self { span, count }
  }

  /// Returns the span, from the first `..` to the last `::`.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns how many `..::` the root is written with.
  #[inline]
  pub const fn count(&self) -> NonZeroUsize {
    self.count
  }

  /// Replaces the span.
  #[inline]
  pub fn set_span(&mut self, span: Span) -> &mut Self {
    self.span = span;
    self
  }

  /// Replaces how many `..::` the root is written with.
  #[inline]
  pub const fn set_count(&mut self, count: NonZeroUsize) -> &mut Self {
    self.count = count;
    self
  }

  /// Returns the root with its span replaced.
  #[inline]
  #[must_use]
  pub fn with_span(mut self, span: Span) -> Self {
    self.span = span;
    self
  }

  /// Returns the root with its count replaced.
  #[inline]
  #[must_use]
  pub const fn with_count(mut self, count: NonZeroUsize) -> Self {
    self.count = count;
    self
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
      Self::Parent(root) => {
        for _ in 0..root.count().get() {
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

/// `Path` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathComponents<Span, Container> {
  /// The span covering the complete path, root included.
  pub span: Span,
  /// The root the path starts at.
  pub root: PathRoot<Span>,
  /// The path segments.
  pub segments: Container,
}

impl<Name, Span, Container> IntoComponents for Path<Name, Span, Container> {
  type Components = PathComponents<Span, Container>;

  #[inline]
  fn into_components(self) -> Self::Components {
    PathComponents {
      span: self.span,
      root: self.root,
      segments: self.segments,
    }
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
