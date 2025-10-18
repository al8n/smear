use logosky::utils::Span;

/// An error about unterminated spread operator.
pub trait UnterminatedSpreadOperatorError {
  /// Creates a new error indicating that an unterminated spread operator was encountered.
  fn unterminated_spread_operator(span: Span) -> Self;
}

/// An state error which can occur when lexing.
pub trait BadStateError {
  /// The actual state error type.
  type StateError;

  /// Creates a new error indicating that a state related error was encountered.
  fn bad_state(span: Span, error: Self::StateError) -> Self;
}
