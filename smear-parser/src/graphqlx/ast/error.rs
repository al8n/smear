use crate::{
  error::ParseVariableValueError,
  hints::{
    EnumTypeExtensionHint, InputObjectTypeExtensionHint, InterfaceTypeExtensionHint,
    ObjectTypeExtensionHint, SchemaExtensionHint, UnionTypeExtensionHint, VariableValueHint,
  },
};
use smear_lexer::tokit::{
  SimpleSpan as Span,
  utils::IntoComponents,
};
use smear_scaffold::error::{
  InvalidFragmentTypePath, UnclosedBraceError, UnclosedBracketError,
  UnexpectedEndOfEnumExtensionError, UnexpectedEndOfInputObjectExtensionError,
  UnexpectedEndOfInterfaceExtensionError, UnexpectedEndOfObjectExtensionError,
  UnexpectedEndOfSchemaExtensionError, UnexpectedEndOfUnionExtensionError,
};

use crate::ident::Ident;
use super::*;

impl<S> ParseVariableValueError<Ident<S>> for SyntacticTokenError<S> {
  #[inline]
  fn missing_dollar_token(name: Ident<S>, span: Span) -> Self {
    Self::unexpected_token(
      SyntacticToken::Identifier(name.into_components().1),
      Expectation::Dollar,
      span,
    )
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::unexpected_end_of_variable_value(hint, span)
  }
}

impl<S> ParseVariableValueError<Ident<S>> for SyntacticTokenErrors<S> {
  #[inline]
  fn missing_dollar_token(name: Ident<S>, span: Span) -> Self {
    <SyntacticTokenError<S> as ParseVariableValueError<Ident<S>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <SyntacticTokenError<S> as ParseVariableValueError<Ident<S>>>::unexpected_end_of_variable_value(
      hint, span,
    )
    .into()
  }
}

impl<S> UnexpectedEndOfObjectExtensionError for SyntacticTokenError<S> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    Self::unexpected_end_of_object_extension(span, hint)
  }
}

impl<S> UnexpectedEndOfObjectExtensionError for SyntacticTokenErrors<S> {
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    <SyntacticTokenError<S> as UnexpectedEndOfObjectExtensionError>::unexpected_end_of_object_extension(span, hint).into()
  }
}

impl<S> UnexpectedEndOfInterfaceExtensionError for SyntacticTokenError<S> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    Self::unexpected_end_of_interface_extension(span, hint)
  }
}

impl<S> UnexpectedEndOfInterfaceExtensionError for SyntacticTokenErrors<S> {
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    <SyntacticTokenError<S> as UnexpectedEndOfInterfaceExtensionError>::unexpected_end_of_interface_extension(span, hint).into()
  }
}

impl<S> UnexpectedEndOfEnumExtensionError for SyntacticTokenError<S> {
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::unexpected_end_of_enum_extension(span, hint)
  }
}

impl<S> UnexpectedEndOfEnumExtensionError for SyntacticTokenErrors<S> {
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    <SyntacticTokenError<S> as UnexpectedEndOfEnumExtensionError>::unexpected_end_of_enum_extension(
      span, hint,
    )
    .into()
  }
}

impl<S> UnexpectedEndOfInputObjectExtensionError for SyntacticTokenError<S> {
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::unexpected_end_of_input_object_extension(span, hint)
  }
}

impl<S> UnexpectedEndOfInputObjectExtensionError for SyntacticTokenErrors<S> {
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    <SyntacticTokenError<S> as UnexpectedEndOfInputObjectExtensionError>::unexpected_end_of_input_object_extension(span, hint).into()
  }
}

impl<S> UnexpectedEndOfSchemaExtensionError for SyntacticTokenError<S> {
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::unexpected_end_of_schema_extension(span, hint)
  }
}

impl<S> UnexpectedEndOfSchemaExtensionError for SyntacticTokenErrors<S> {
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    <SyntacticTokenError<S> as UnexpectedEndOfSchemaExtensionError>::unexpected_end_of_schema_extension(span, hint).into()
  }
}

impl<S> UnexpectedEndOfUnionExtensionError for SyntacticTokenError<S> {
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::unexpected_end_of_union_extension(span, hint)
  }
}

impl<S> UnexpectedEndOfUnionExtensionError for SyntacticTokenErrors<S> {
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    <SyntacticTokenError<S> as UnexpectedEndOfUnionExtensionError>::unexpected_end_of_union_extension(
      span, hint,
    )
    .into()
  }
}

impl<S> UnclosedBraceError for SyntacticTokenError<S> {
  #[inline]
  fn unclosed_brace(span: Span) -> Self {
    Self::unclosed_brace(span)
  }
}

impl<S> UnclosedBraceError for SyntacticTokenErrors<S> {
  #[inline]
  fn unclosed_brace(span: Span) -> Self {
    <SyntacticTokenError<S> as UnclosedBraceError>::unclosed_brace(span).into()
  }
}

impl<S> UnclosedBracketError for SyntacticTokenError<S> {
  #[inline]
  fn unclosed_bracket(span: Span) -> Self {
    Self::unclosed_bracket(span)
  }
}

impl<S> UnclosedBracketError for SyntacticTokenErrors<S> {
  #[inline]
  fn unclosed_bracket(span: Span) -> Self {
    <SyntacticTokenError<S> as UnclosedBracketError>::unclosed_bracket(span).into()
  }
}

impl<S> InvalidFragmentTypePath for SyntacticTokenError<S> {
  fn invalid_fragment_type_path(span: Span) -> Self {
    Self::invalid_fragment_type_path(span)
  }
}

impl<S> InvalidFragmentTypePath for SyntacticTokenErrors<S> {
  fn invalid_fragment_type_path(span: Span) -> Self {
    <SyntacticTokenError<S> as InvalidFragmentTypePath>::invalid_fragment_type_path(span).into()
  }
}
