use tokora::SimpleSpan;

/// A GraphQL name identifier.
///
/// Represents a valid GraphQL name as defined by the specification. Names are
/// used throughout GraphQL for field names, type names, argument names, directive
/// names, and other identifiers.
///
/// ## Grammar
///
/// ```text
/// Name ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
pub type Name<V, S = SimpleSpan> = crate::ident::Ident<V, S>;
