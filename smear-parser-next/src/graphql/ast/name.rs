//! GraphQL-specialized [`Name`] AST node alias.
//!
//! This binds the shared nominal name carrier to the [`GraphQL`] dialect
//! marker.

use tokora::SimpleSpan;

use crate::graphql::GraphQL;

/// A GraphQL name identifier.
///
/// Represents a valid GraphQL name as defined by the specification. Names are
/// used throughout GraphQL for field names, type names, argument names,
/// directive names, and other identifiers.
///
/// ## Grammar
///
/// ```text
/// Name ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
pub type Name<S, Span = SimpleSpan> = crate::name::Name<S, Span, GraphQL>;

#[cfg(test)]
mod tests {
  use super::Name;

  #[test]
  fn alias_accepts_an_unsized_source_carrier() {
    fn accepts_unsized<T: ?Sized>() {}

    accepts_unsized::<Name<str>>();
  }
}
