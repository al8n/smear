use logosky::utils::{AsSpan, IntoSpan, Span};

/// CST representation of operation type: Query, Mutation, or Subscription
///
/// Preserves the keyword token with its trivia
#[derive(Debug, Clone)]
pub enum OperationType<S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  Query(crate::cst::Padding<S, TriviaContainer>),
  Mutation(crate::cst::Padding<S, TriviaContainer>),
  Subscription(crate::cst::Padding<S, TriviaContainer>),
}

impl<S, TriviaContainer> OperationType<S, TriviaContainer> {
  pub const fn padding(&self) -> &crate::cst::Padding<S, TriviaContainer> {
    match self {
      Self::Query(p) | Self::Mutation(p) | Self::Subscription(p) => p,
    }
  }
}
