use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{
  AsSpan, IntoSpan, Span,
  human_display::DisplayHuman,
  sdl_display::{DisplayCompact, DisplayPretty},
  syntax_tree_display::DisplaySyntaxTree,
};

use smear_lexer::keywords::{Mutation, Query, Subscription};

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
#[derive(Debug, Display, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationType {
  /// Represents a GraphQL query operation for read-only data retrieval.
  ///
  /// Query operations are the most fundamental operation type in GraphQL, designed for
  /// fetching data without causing side effects. They embody GraphQL's approach to
  /// declarative data fetching where clients specify exactly what data they need.
  #[display("query")]
  Query(Query),

  /// Represents a GraphQL mutation operation for write operations and data modification.
  ///
  /// Mutation operations are designed for operations that modify server state, create
  /// or update data, or trigger side effects. They provide controlled, predictable
  /// mechanisms for changing data while maintaining GraphQL's type safety guarantees.
  #[display("mutation")]
  Mutation(Mutation),

  /// Represents a GraphQL subscription operation for real-time streaming data.
  ///
  /// Subscription operations enable real-time communication between clients and servers
  /// by establishing persistent connections that stream data updates over time. They
  /// extend GraphQL's request-response model to support event-driven architectures
  /// and real-time user experiences.
  #[display("subscription")]
  Subscription(Subscription),
}

impl core::borrow::Borrow<str> for OperationType {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

impl AsRef<str> for OperationType {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl AsSpan<Span> for OperationType {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for OperationType {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(q) => q.into_span(),
      Self::Mutation(m) => m.into_span(),
      Self::Subscription(s) => s.into_span(),
    }
  }
}

impl OperationType {
  /// Returns a reference to the span of the operation type keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(q) => q.span(),
      Self::Mutation(m) => m.span(),
      Self::Subscription(s) => s.span(),
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

  #[inline]
  fn syntax_tree_name(&self) -> &'static str {
    match self {
      Self::Query(_) => "query_KW",
      Self::Mutation(_) => "mutation_KW",
      Self::Subscription(_) => "subscription_KW",
    }
  }
}

impl DisplayCompact for OperationType {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self, f)
  }
}

impl DisplayPretty for OperationType {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
  }
}

impl DisplayHuman for OperationType {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl DisplaySyntaxTree for OperationType {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    let span = self.span();
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- {}@{}..{}",
      self.syntax_tree_name(),
      span.start(),
      span.end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      span.start(),
      span.end(),
      self.as_str(),
    )
  }
}
