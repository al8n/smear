use logosky::utils::Span;

/// An error which can occur when an unexpected token is encountered.
pub trait UnexpectedTokenError {
  /// The actual token type.
  type Token<'a>
  where
    Self: 'a;
  /// The expected token kind type.
  type TokenKind;

  /// Creates a new error indicating that an unexpected token was encountered
  /// while parsing.
  fn unexpected_token<'a>(found: Self::Token<'a>, expected: Self::TokenKind, span: Span) -> Self
  where
    Self: 'a;
}

/// An error which can occur when an unclosed brace is encountered.
pub trait UnclosedBraceError {
  /// Creates a new error indicating that an unclosed brace value was encountered.
  fn unclosed_brace(span: Span) -> Self;
}

/// An error which can occur when an unclosed bracket is encountered.
pub trait UnclosedBracketError {
  /// Creates a new error indicating that an unclosed bracket value was encountered.
  fn unclosed_bracket(span: Span) -> Self;
}

/// An error which can occur when parsing a fragment path
pub trait InvalidFragmentTypePath {
  /// Creates a new error indicating that an invalid fragment path.
  fn invalid_fragment_type_path(span: Span) -> Self;
}
