use logosky::utils::Span;

/// An error which can occur when parsing a variable value and the dollar sign is missing.
///
/// Note: For incomplete variable values, use `From<IncompleteSyntax<VariableValueSyntax>>` instead.
pub trait MissingDollarTokenError<Name> {
  /// Creates a new error indicating that a dollar token is missing
  /// while parsing a variable value (e.g., `foo` instead of `$foo`).
  fn missing_dollar_token(name: Name, span: Span) -> Self;
}
