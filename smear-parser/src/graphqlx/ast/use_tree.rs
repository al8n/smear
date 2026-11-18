//! GraphQLx `use` item AST nodes.
//!
//! ```text
//! UseItem      ::= "use" UseTree
//! UseTree      ::= UsePath ( "as" Name )?          -- one binding
//!                | UseScope "*"                     -- glob
//!                | UseScope "{" UseTree+ "}"        -- group; commas are trivia
//! UsePath      ::= Path
//!                | StringSource ( "::" Name )*
//! UseScope     ::= PathRoot
//!                | Path "::"
//!                | StringSource ( "::" Name )* "::"
//! StringSource ::= InlineStringValue
//! ```
//!
//! A [`UseTree`] is its [`UsePrefix`] — what its names resolve against — and its
//! [`UseTreeKind`]: one binding, a glob, or a group of nested trees. Inside a group every tree's
//! prefix is a [`Relative`](PathRoot::Relative) path.

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{DefaultVec, InlineStringValue, Leaf, Name, Nested, Path, PathRoot};
use crate::value::{Absent, NestNode, Nestable, Sealed, Worklist};

/// What a [`UseTree`]'s names resolve against.
///
/// A path binds its last segment and a string source binds its last name, or the name of the
/// directory it points at when no name follows it; `as` renames either. Which binding a source
/// gives is analysis's to decide — the parser accepts `use "./vision"` with or without `as`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum UsePrefix<S, Span = SimpleSpan> {
  /// A bare root — `~::`, `^::` or `..::` repeated — with nothing after it but a glob or a group:
  /// `use ~::*`, `use ^::{a, b}`.
  Root(PathRoot<Span>),
  /// A path, which is never empty. Inside a group it is always a
  /// [`Relative`](PathRoot::Relative) one.
  Path(Path<S, Span>),
  /// A string source and the names written after it: `use "./vision"`,
  /// `use "../shared"::MediaId`.
  Source(SourcePrefix<S, Span>),
}

/// A string source and the names written after it: [`UsePrefix::Source`]'s payload.
///
/// `use "./vision"`, `use "../shared"::MediaId`. Built with [`new`](Self::new); edited in place
/// with the `set_*` setters or by value with the `with_*` ones.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourcePrefix<S, Span = SimpleSpan> {
  pub(crate) source: InlineStringValue<S, Span>,
  pub(crate) segments: DefaultVec<Name<S, Span>>,
}

impl<S, Span> SourcePrefix<S, Span> {
  /// Creates the prefix from the inline string naming the module and the names after it.
  #[inline]
  pub const fn new(
    source: InlineStringValue<S, Span>,
    segments: DefaultVec<Name<S, Span>>,
  ) -> Self {
    Self { source, segments }
  }

  /// Returns the inline string naming the module; never a block string.
  #[inline]
  pub const fn source(&self) -> &InlineStringValue<S, Span> {
    &self.source
  }

  /// Returns the `::`-separated names after the source, possibly none.
  #[inline]
  pub fn segments(&self) -> &[Name<S, Span>] {
    &self.segments
  }

  /// Replaces the inline string naming the module.
  #[inline]
  pub fn set_source(&mut self, source: InlineStringValue<S, Span>) -> &mut Self {
    self.source = source;
    self
  }

  /// Replaces the names after the source.
  #[inline]
  pub fn set_segments(&mut self, segments: DefaultVec<Name<S, Span>>) -> &mut Self {
    self.segments = segments;
    self
  }

  /// Returns the prefix with its source replaced.
  #[inline]
  #[must_use]
  pub fn with_source(mut self, source: InlineStringValue<S, Span>) -> Self {
    self.source = source;
    self
  }

  /// Returns the prefix with its names replaced.
  #[inline]
  #[must_use]
  pub fn with_segments(mut self, segments: DefaultVec<Name<S, Span>>) -> Self {
    self.segments = segments;
    self
  }
}

/// What a [`UseTree`] brings into scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum UseTreeKind<S, Span = SimpleSpan> {
  /// The one item its prefix names, under its own name or under the alias the payload holds: the
  /// name written after `as`, if one was.
  Simple(Option<Name<S, Span>>),
  /// `*`: every public item of the module its prefix names.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Glob,
  /// `{ … }`: one nested tree per member, each relative to the prefix. Never empty.
  ///
  /// A group holds trees that may hold groups in turn, so the container is [`Nested`] rather than
  /// a `Vec`: a tree nested however deep is released by a loop rather than by one native frame per
  /// level.
  Group(Nested<UseTree<S, Span>>),
}

/// One use tree: a prefix and what it binds.
///
/// # This type declares no `Drop`, and that is load-bearing
///
/// A group holds trees, every constructor here is public, and a caller can grow the nesting in a
/// loop that no parser sees, so the release is [`Nested`]'s and walks the trees off the native
/// stack — the shape [`SelectionSet`](super::SelectionSet) has. A `Drop` here would cost the
/// by-value [`IntoComponents`] destructuring to `E0509`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseTree<S, Span = SimpleSpan> {
  span: Span,
  prefix: UsePrefix<S, Span>,
  kind: UseTreeKind<S, Span>,
}

impl<S, Span> UseTree<S, Span> {
  /// Creates a use tree from its complete span, its prefix and what it binds.
  ///
  /// `S` and `Span` are [`Leaf`]: a tree is released by a loop that drops every name, path and
  /// source in place, so neither may own a node of its own (`al8n/smear#176`).
  #[inline]
  pub const fn new(span: Span, prefix: UsePrefix<S, Span>, kind: UseTreeKind<S, Span>) -> Self
  where
    S: Leaf,
    Span: Leaf,
  {
    Self { span, prefix, kind }
  }

  /// Returns the complete tree span, from its prefix's first token to its own last token.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns what the tree's names resolve against.
  #[inline]
  pub const fn prefix(&self) -> &UsePrefix<S, Span> {
    &self.prefix
  }

  /// Returns what the tree brings into scope.
  #[inline]
  pub const fn kind(&self) -> &UseTreeKind<S, Span> {
    &self.kind
  }
}

impl<S, Span> AsSpan<Span> for UseTree<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for UseTree<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `UseTree` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseTreeComponents<S, Span> {
  /// The complete tree span, from its prefix's first token to its own last token.
  pub span: Span,
  /// What the tree's names resolve against.
  pub prefix: UsePrefix<S, Span>,
  /// What the tree brings into scope.
  pub kind: UseTreeKind<S, Span>,
}

impl<S, Span> IntoComponents for UseTree<S, Span> {
  type Components = UseTreeComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    UseTreeComponents {
      span: self.span,
      prefix: self.prefix,
      kind: self.kind,
    }
  }
}

impl<S, Span> Sealed for UseTree<S, Span> {}

/// A tree's children are trees, which a group's container holds directly; neither carrier lane
/// exists here.
impl<S, Span> NestNode for UseTree<S, Span> {
  type Field = Absent<Self>;
  type Entry = Absent<Self>;
}

impl<S, Span> Nestable for UseTree<S, Span> {
  /// The use tree: rank 1. It holds names, paths and a string source, which are leaves, and no
  /// value, type or selection node.
  const RANK: u8 = 1;

  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    // The span and the prefix are dropped in place, and both are leaves: a prefix holds a root, a
    // path, or a string source and its names, none of which owns a tree. The match has no wildcard
    // arm although the enum is `#[non_exhaustive]`, so a new kind is a compile error here.
    match self.kind {
      UseTreeKind::Simple(_) | UseTreeKind::Glob => {}
      UseTreeKind::Group(trees) => worklist.adopt(trees.into_vec()),
    }
  }
}

/// A complete `use UseTree` item.
///
/// No description may precede one.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseDefinition<S, Span = SimpleSpan> {
  span: Span,
  tree: UseTree<S, Span>,
}

impl<S, Span> UseDefinition<S, Span> {
  /// Creates a use item from its complete span, `use` included, and its tree.
  #[inline]
  pub const fn new(span: Span, tree: UseTree<S, Span>) -> Self {
    Self { span, tree }
  }

  /// Returns the complete item span, from `use` to the tree's last token.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the item's tree.
  #[inline]
  pub const fn tree(&self) -> &UseTree<S, Span> {
    &self.tree
  }
}

impl<S, Span> AsSpan<Span> for UseDefinition<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for UseDefinition<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `UseDefinition` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseDefinitionComponents<S, Span> {
  /// The complete item span, from `use` to the tree's last token.
  pub span: Span,
  /// The item's tree.
  pub tree: UseTree<S, Span>,
}

impl<S, Span> IntoComponents for UseDefinition<S, Span> {
  type Components = UseDefinitionComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    UseDefinitionComponents {
      span: self.span,
      tree: self.tree,
    }
  }
}
