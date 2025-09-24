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

impl<'a> ParseVariableValueError<Name<&'a str>> for AstTokenError<'a, &'a str> {
  #[inline]
  fn missing_dollar_token(name: Name<&'a str>, span: Span) -> Self {
    Self::unexpected_token(AstToken::Identifier(name.slice()), TokenKind::Dollar, span)
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::unexpected_end_of_variable_value(hint, span)
  }
}

impl<'a> ParseVariableValueError<Name<&'a str>> for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn missing_dollar_token(name: Name<&'a str>, span: Span) -> Self {
    <AstTokenError<'a, &'a str> as ParseVariableValueError<Name<&'a str>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <AstTokenError<'a, &'a str> as ParseVariableValueError<Name<&'a str>>>::unexpected_end_of_variable_value(hint, span).into()
  }
}

impl<'a> UnexpectedEndOfObjectExtensionError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    Self::unexpected_end_of_object_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfObjectExtensionError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    <AstTokenError<'a, &'a str> as UnexpectedEndOfObjectExtensionError>::unexpected_end_of_object_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfInterfaceExtensionError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    Self::unexpected_end_of_interface_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfInterfaceExtensionError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    <AstTokenError<'a, &'a str> as UnexpectedEndOfInterfaceExtensionError>::unexpected_end_of_interface_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfEnumExtensionError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::unexpected_end_of_enum_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfEnumExtensionError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    <AstTokenError<'a, &'a str> as UnexpectedEndOfEnumExtensionError>::unexpected_end_of_enum_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfInputObjectExtensionError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::unexpected_end_of_input_object_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfInputObjectExtensionError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    <AstTokenError<'a, &'a str> as UnexpectedEndOfInputObjectExtensionError>::unexpected_end_of_input_object_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfSchemaExtensionError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::unexpected_end_of_schema_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfSchemaExtensionError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    <AstTokenError<'a, &'a str> as UnexpectedEndOfSchemaExtensionError>::unexpected_end_of_schema_extension(span, hint).into()
  }
}

impl<'a> UnexpectedEndOfUnionExtensionError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::unexpected_end_of_union_extension(span, hint)
  }
}

impl<'a> UnexpectedEndOfUnionExtensionError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    <AstTokenError<'a, &'a str> as UnexpectedEndOfUnionExtensionError>::unexpected_end_of_union_extension(span, hint).into()
  }
}

impl<'a> UnclosedObjectValueError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unclosed_object(span: Span) -> Self {
    Self::unclosed_object(span)
  }
}

impl<'a> UnclosedObjectValueError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unclosed_object(span: Span) -> Self {
    <AstTokenError<'a, &'a str> as UnclosedObjectValueError>::unclosed_object(span).into()
  }
}

impl<'a> UnclosedListValueError for AstTokenError<'a, &'a str> {
  #[inline]
  fn unclosed_list(span: Span) -> Self {
    Self::unclosed_list(span)
  }
}

impl<'a> UnclosedListValueError for AstTokenErrors<'a, &'a str> {
  #[inline]
  fn unclosed_list(span: Span) -> Self {
    <AstTokenError<'a, &'a str> as UnclosedListValueError>::unclosed_list(span).into()
  }
}
