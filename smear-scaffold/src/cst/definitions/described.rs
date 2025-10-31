use logosky::utils::Span;

/// CST representation of a described element (with optional description string)
///
/// Preserves the description string token with its trivia
pub struct Described<T, Description, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  description: Option<Description>,
  value: T,
  _marker: core::marker::PhantomData<(S, TriviaContainer)>,
}

impl<T, Description, S, TriviaContainer> Described<T, Description, S, TriviaContainer> {
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn description(&self) -> Option<&Description> {
    self.description.as_ref()
  }
  pub const fn value(&self) -> &T {
    &self.value
  }
}
