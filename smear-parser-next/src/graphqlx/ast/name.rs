use tokora::SimpleSpan;

/// A GraphQLx name identifier.
///
/// Represents a valid GraphQLx name. Names are used throughout the grammar for
/// field names, type names, argument names, directive names, path segments, and
/// other identifiers.
///
/// ## Grammar
///
/// ```text
/// Name ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
pub type Name<V, S = SimpleSpan> = crate::ident::Ident<V, S>;
