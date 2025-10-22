use logosky::cst::{CstNode, CstElement, error::CastNodeError};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::keywords::{Mutation, Query, Subscription};

/// Represents the three fundamental operation types in GraphQL.
///
/// Operation types form the foundation of GraphQL's execution model and define the semantic
/// contract between clients and servers.
///
/// ## Grammar
///
/// ```text
/// OperationType : one of
///   query mutation subscription
/// ```
///
/// Spec: [Operation Type](https://spec.graphql.org/draft/#OperationType)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OperationType<Lang>
where
  Lang: Language,
{
  /// Represents a GraphQL query operation for read-only data retrieval.
  Query(Query<TextRange, SyntaxToken<Lang>>),

  /// Represents a GraphQL mutation operation for write operations and data modification.
  Mutation(Mutation<TextRange, SyntaxToken<Lang>>),

  /// Represents a GraphQL subscription operation for real-time streaming data.
  Subscription(Subscription<TextRange, SyntaxToken<Lang>>),
}

impl<Lang> OperationType<Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  /// Tries to create an `OperationType` from the given syntax node.
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, CastNodeError<Self>> {
    Self::try_cast_node(syntax)
  }

  /// Returns the span of the operation type keyword.
  #[inline]
  pub fn span(&self) -> TextRange {
    match self {
      Self::Query(q) => *q.span(),
      Self::Mutation(m) => *m.span(),
      Self::Subscription(s) => *s.span(),
    }
  }

  /// Returns the syntax node of the operation type.
  #[inline]
  pub fn syntax(&self) -> &SyntaxToken<Lang> {
    match self {
      Self::Query(q) => q.content(),
      Self::Mutation(m) => m.content(),
      Self::Subscription(s) => s.content(),
    }
  }

  /// Returns the operation type as a string slice.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query(_) => "query",
      Self::Mutation(_) => "mutation",
      Self::Subscription(_) => "subscription",
    }
  }
}
