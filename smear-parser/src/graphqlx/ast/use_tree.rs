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
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::IsVariant)]
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
  Source {
    /// The inline string naming the module; never a block string.
    source: InlineStringValue<S, Span>,
    /// The `::`-separated names after the source, possibly none.
    segments: DefaultVec<Name<S, Span>>,
  },
}

/// What a [`UseTree`] brings into scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::IsVariant)]
#[non_exhaustive]
pub enum UseTreeKind<S, Span = SimpleSpan> {
  /// The one item its prefix names, under its own name or under `alias`.
  Simple {
    /// The name written after `as`, if one was.
    alias: Option<Name<S, Span>>,
  },
  /// `*`: every public item of the module its prefix names.
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

impl<S, Span> IntoComponents for UseTree<S, Span> {
  type Components = (Span, UsePrefix<S, Span>, UseTreeKind<S, Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.prefix, self.kind)
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
      UseTreeKind::Simple { .. } | UseTreeKind::Glob => {}
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

impl<S, Span> IntoComponents for UseDefinition<S, Span> {
  type Components = (Span, UseTree<S, Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.tree)
  }
}
