//! The GraphQL lossless parser suite: a `rowan` CST over the trivia-surfacing lexer.

pub use crate::graphql::kinds::GraphQLLang;

/// A GraphQL lossless syntax node.
pub type SyntaxNode = rowan::SyntaxNode<GraphQLLang>;
/// A GraphQL lossless syntax token.
pub type SyntaxToken = rowan::SyntaxToken<GraphQLLang>;
/// A node-or-token in the GraphQL lossless CST.
pub type SyntaxElement = rowan::SyntaxElement<GraphQLLang>;
