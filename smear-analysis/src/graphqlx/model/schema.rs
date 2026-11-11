//! The schema: its root operation types, its applications, its description and its contributions.

use crate::graphqlx::id::{DefId, TypeExprId};

/// One of the three root operations a schema has a type for.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum RootOperation {
  /// `query`.
  Query,
  /// `mutation`.
  Mutation,
  /// `subscription`.
  Subscription,
}

impl RootOperation {
  /// Its slot among the schema's roots.
  pub(super) const fn index(self) -> usize {
    match self {
      Self::Query => 0,
      Self::Mutation => 1,
      Self::Subscription => 2,
    }
  }
}

/// A root operation's type, and how the schema came to have it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Root {
  /// Written in a schema definition or extension, as `query: Q` is: the type occurrence, which is
  /// that occurrence's `Unresolved` when it did not resolve.
  Written(TypeExprId),
  /// Presumed, with no schema definition, from the definition GraphQL's default root names.
  Presumed(DefId),
}
