use logosky::utils::Span;
use smear_parser::error::{ParseVariableValueError, VariableValueHint, UnexpectedEndOfObjectExtensionError, ObjectTypeExtensionHint, UnexpectedEndOfInterfaceExtensionError, InterfaceTypeExtensionHint};

use super::*;

impl<'a> ParseVariableValueError<Name<&'a str>> for FastTokenError<'a, &'a str> {
  #[inline]
  fn missing_dollar_token(name: Name<&'a str>, span: Span) -> Self {
    Self::unexpected_token(Token::Identifier(name.source()), TokenKind::Dollar, span)
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::unexpected_end_of_variable_value(hint, span)
  }
}

impl<'a> ParseVariableValueError<Name<&'a str>> for FastTokenErrors<'a, &'a str> {
  #[inline]
  fn missing_dollar_token(name: Name<&'a str>, span: Span) -> Self {
    <FastTokenError<'a, &'a str> as ParseVariableValueError<Name<&'a str>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <FastTokenError<'a, &'a str> as ParseVariableValueError<Name<&'a str>>>::unexpected_end_of_variable_value(hint, span).into()
  }
}

impl<'a> UnexpectedEndOfObjectExtensionError for FastTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    Self::unexpected_end_of_object_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfObjectExtensionError for FastTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    <FastTokenError<'a, &'a str> as UnexpectedEndOfObjectExtensionError>::unexpected_end_of_object_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfInterfaceExtensionError for FastTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    Self::unexpected_end_of_interface_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfInterfaceExtensionError for FastTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    <FastTokenError<'a, &'a str> as UnexpectedEndOfInterfaceExtensionError>::unexpected_end_of_interface_extension(span, hint).into()
  }
}
