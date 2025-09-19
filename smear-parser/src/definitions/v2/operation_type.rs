use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  lang::keywords::{Mutation, Query, Subscription},
  source::*,
};

/// Represents the three fundamental operation types in GraphQL that define how clients interact with a GraphQL service.
///
/// Operation types form the foundation of GraphQL's execution model and define the semantic
/// contract between clients and servers. Each operation type serves a distinct purpose in
/// the GraphQL ecosystem and follows specific execution semantics that ensure predictable
/// and reliable API behavior.
///
/// ## Grammar
///
/// ```text
/// OperationType : one of
///   query mutation subscription
/// ```
///
/// Spec: [Operation Type](https://spec.graphql.org/draft/#OperationType)
#[derive(
  Debug,
  Clone,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationType<Span> {
  /// Represents a GraphQL query operation for read-only data retrieval.
  ///
  /// Query operations are the most fundamental operation type in GraphQL, designed for
  /// fetching data without causing side effects. They embody GraphQL's approach to
  /// declarative data fetching where clients specify exactly what data they need.
  Query(Query<Span>),

  /// Represents a GraphQL mutation operation for write operations and data modification.
  ///
  /// Mutation operations are designed for operations that modify server state, create
  /// or update data, or trigger side effects. They provide controlled, predictable
  /// mechanisms for changing data while maintaining GraphQL's type safety guarantees.
  Mutation(Mutation<Span>),

  /// Represents a GraphQL subscription operation for real-time streaming data.
  ///
  /// Subscription operations enable real-time communication between clients and servers
  /// by establishing persistent connections that stream data updates over time. They
  /// extend GraphQL's request-response model to support event-driven architectures
  /// and real-time user experiences.
  Subscription(Subscription<Span>),
}

impl<Span> AsRef<Span> for OperationType<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for OperationType<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(q) => q.into_span(),
      Self::Mutation(m) => m.into_span(),
      Self::Subscription(s) => s.into_span(),
    }
  }
}

impl<Span> OperationType<Span> {
  /// Returns a reference to the span of the operation type keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(q) => q.span(),
      Self::Mutation(m) => m.span(),
      Self::Subscription(s) => s.span(),
    }
  }

  /// Creates a parser that can recognize any of the three GraphQL operation types.
  ///
  /// This parser uses a choice combinator to attempt parsing each operation type
  /// keyword in sequence, returning the first successful match. The parser is
  /// designed to be efficient and provide clear error messages when none of
  /// the operation types match.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the operation type.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    choice((
      Query::parser().map(Self::Query),
      Mutation::parser().map(Self::Mutation),
      Subscription::parser().map(Self::Subscription),
    ))
  }
}
