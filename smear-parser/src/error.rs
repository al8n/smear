use derive_more::{Display, IsVariant};
use logosky::utils::Span;

/// Hints for parsing a variable value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum VariableValueHint {
  /// A [`Name`](crate::parser::ast::Name) was expected.
  #[display("name")]
  Name,
  /// A [`Dollar`](crate::parser::ast::Dollar) was expected.
  #[display("dollar")]
  Dollar,
}

/// An error which can occur when parsing a variable value.
pub trait ParseVariableValueError<Name> {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing a variable value.
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self;

  /// Creates a new error indicating that a dollar token is missing
  /// while parsing a variable value.
  fn missing_dollar_token(name: Name, span: Span) -> Self;
}

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
