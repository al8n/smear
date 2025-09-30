use logosky::utils::Span;
use smear_parser::error::{
  EnumTypeExtensionHint, InputObjectTypeExtensionHint, InterfaceTypeExtensionHint,
  ObjectTypeExtensionHint, ParseVariableValueError, SchemaExtensionHint, UnclosedListValueError,
  UnclosedObjectValueError, UnexpectedEndOfEnumExtensionError,
  UnexpectedEndOfInputObjectExtensionError, UnexpectedEndOfInterfaceExtensionError,
  UnexpectedEndOfObjectExtensionError, UnexpectedEndOfSchemaExtensionError,
  UnexpectedEndOfUnionExtensionError, UnionTypeExtensionHint, VariableValueHint,
};

use super::*;

impl<'a> ParseVariableValueError<Name<&'a str>> for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn missing_dollar_token(name: Name<&'a str>, span: Span) -> Self {
    Self::unexpected_token(
      StrAstToken::Identifier(name.slice()),
      TokenKind::Dollar,
      span,
    )
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::unexpected_end_of_variable_value(hint, span)
  }
}

impl<'a> ParseVariableValueError<Name<&'a str>> for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn missing_dollar_token(name: Name<&'a str>, span: Span) -> Self {
    <StrAstTokenError<'a, &'a str> as ParseVariableValueError<Name<&'a str>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <StrAstTokenError<'a, &'a str> as ParseVariableValueError<Name<&'a str>>>::unexpected_end_of_variable_value(hint, span).into()
  }
}

impl<'a> UnexpectedEndOfObjectExtensionError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    Self::unexpected_end_of_object_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfObjectExtensionError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    <StrAstTokenError<'a, &'a str> as UnexpectedEndOfObjectExtensionError>::unexpected_end_of_object_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfInterfaceExtensionError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    Self::unexpected_end_of_interface_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfInterfaceExtensionError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    <StrAstTokenError<'a, &'a str> as UnexpectedEndOfInterfaceExtensionError>::unexpected_end_of_interface_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfEnumExtensionError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::unexpected_end_of_enum_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfEnumExtensionError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    <StrAstTokenError<'a, &'a str> as UnexpectedEndOfEnumExtensionError>::unexpected_end_of_enum_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfInputObjectExtensionError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::unexpected_end_of_input_object_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfInputObjectExtensionError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    <StrAstTokenError<'a, &'a str> as UnexpectedEndOfInputObjectExtensionError>::unexpected_end_of_input_object_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfSchemaExtensionError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::unexpected_end_of_schema_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfSchemaExtensionError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    <StrAstTokenError<'a, &'a str> as UnexpectedEndOfSchemaExtensionError>::unexpected_end_of_schema_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfUnionExtensionError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::unexpected_end_of_union_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfUnionExtensionError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    <StrAstTokenError<'a, &'a str> as UnexpectedEndOfUnionExtensionError>::unexpected_end_of_union_extension(span, hint).into()
  }
}

impl<'a> UnclosedObjectValueError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unclosed_object(span: Span) -> Self {
    Self::unclosed_object(span)
  }
}

impl<'a> UnclosedObjectValueError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unclosed_object(span: Span) -> Self {
    <StrAstTokenError<'a, &'a str> as UnclosedObjectValueError>::unclosed_object(span).into()
  }
}

impl<'a> UnclosedListValueError for StrAstTokenError<'a, &'a str> {
  #[inline]
  fn unclosed_list(span: Span) -> Self {
    Self::unclosed_list(span)
  }
}

impl<'a> UnclosedListValueError for StrAstTokenErrors<'a, &'a str> {
  #[inline]
  fn unclosed_list(span: Span) -> Self {
    <StrAstTokenError<'a, &'a str> as UnclosedListValueError>::unclosed_list(span).into()
  }
}
