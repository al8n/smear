use logosky::utils::Span;

use crate::hints::VariableValueHint;

/// An error which can occur when parsing a variable value.
pub trait ParseVariableValueError<Name> {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing a variable value.
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self;

  /// Creates a new error indicating that a dollar token is missing
  /// while parsing a variable value.
  fn missing_dollar_token(name: Name, span: Span) -> Self;
}
