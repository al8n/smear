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

/// An unexpected token error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedToken<T, TK> {
  found: Option<T>,
  expected: TK,
}

impl<T, TK> UnexpectedToken<T, TK> {
  /// Creates an unexpected token error without found token.
  #[inline]
  pub const fn new(expected: TK) -> Self {
    Self::maybe_found(None, expected)
  }

  /// Creates a new unexpected token error.
  #[inline]
  pub const fn maybe_found(found: Option<T>, expected: TK) -> Self {
    Self { found, expected }
  }

  /// Creates a new unexpected token error with found token.
  #[inline]
  pub const fn with_found(found: T, expected: TK) -> Self {
    Self::maybe_found(Some(found), expected)
  }
}

/// An unexpected keyword error.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedKeyword<S> {
  found: S,
  expected: &'static str,
}

impl<S> UnexpectedKeyword<S> {
  /// Creates a new unexpected keyword error.
  #[inline]
  pub const fn new(found: S, expected_kw: &'static str) -> Self {
    Self {
      found,
      expected: expected_kw,
    }
  }

  /// Returns the found keyword.
  #[inline]
  pub const fn found(&self) -> &S {
    &self.found
  }

  /// Returns the name of the expected keyword.
  #[inline]
  pub const fn expected(&self) -> &'static str {
    self.expected
  }
}
