use logosky::{
  Token,
  utils::{IntoComponents, Span},
};
use smear_parser::error::{
  EnumTypeExtensionHint, InputObjectTypeExtensionHint, InterfaceTypeExtensionHint,
  ObjectTypeExtensionHint, ParseVariableValueError, SchemaExtensionHint, UnclosedListValueError,
  UnclosedObjectValueError, UnexpectedEndOfEnumExtensionError,
  UnexpectedEndOfInputObjectExtensionError, UnexpectedEndOfInterfaceExtensionError,
  UnexpectedEndOfObjectExtensionError, UnexpectedEndOfSchemaExtensionError,
  UnexpectedEndOfUnionExtensionError, UnionTypeExtensionHint, VariableValueHint,
};

use super::*;

impl<'a, S> ParseVariableValueError<Name<S>> for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Name<S>, span: Span) -> Self {
    Self::unexpected_token(
      AstToken::Identifier(name.into_components().1),
      TokenKind::Dollar,
      span,
    )
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::unexpected_end_of_variable_value(hint, span)
  }
}

impl<'a, S> ParseVariableValueError<Name<S>> for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Name<S>, span: Span) -> Self {
    <AstTokenError<'a, S> as ParseVariableValueError<Name<S>>>::missing_dollar_token(name, span)
      .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <AstTokenError<'a, S> as ParseVariableValueError<Name<S>>>::unexpected_end_of_variable_value(
      hint, span,
    )
    .into()
  }
}

impl<'a, S> UnexpectedEndOfObjectExtensionError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    Self::unexpected_end_of_object_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfObjectExtensionError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    <AstTokenError<'a, S> as UnexpectedEndOfObjectExtensionError>::unexpected_end_of_object_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfInterfaceExtensionError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    Self::unexpected_end_of_interface_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfInterfaceExtensionError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    <AstTokenError<'a, S> as UnexpectedEndOfInterfaceExtensionError>::unexpected_end_of_interface_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfEnumExtensionError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::unexpected_end_of_enum_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfEnumExtensionError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    <AstTokenError<'a, S> as UnexpectedEndOfEnumExtensionError>::unexpected_end_of_enum_extension(
      span, hint,
    )
    .into()
  }
}

impl<'a, S> UnexpectedEndOfInputObjectExtensionError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::unexpected_end_of_input_object_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfInputObjectExtensionError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    <AstTokenError<'a, S> as UnexpectedEndOfInputObjectExtensionError>::unexpected_end_of_input_object_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfSchemaExtensionError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::unexpected_end_of_schema_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfSchemaExtensionError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    <AstTokenError<'a, S> as UnexpectedEndOfSchemaExtensionError>::unexpected_end_of_schema_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfUnionExtensionError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::unexpected_end_of_union_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfUnionExtensionError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    <AstTokenError<'a, S> as UnexpectedEndOfUnionExtensionError>::unexpected_end_of_union_extension(
      span, hint,
    )
    .into()
  }
}

impl<'a, S> UnclosedObjectValueError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_object(span: Span) -> Self {
    Self::unclosed_object(span)
  }
}

impl<'a, S> UnclosedObjectValueError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_object(span: Span) -> Self {
    <AstTokenError<'a, S> as UnclosedObjectValueError>::unclosed_object(span).into()
  }
}

impl<'a, S> UnclosedListValueError for AstTokenError<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_list(span: Span) -> Self {
    Self::unclosed_list(span)
  }
}

impl<'a, S> UnclosedListValueError for AstTokenErrors<'a, S>
where
  AstToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_list(span: Span) -> Self {
    <AstTokenError<'a, S> as UnclosedListValueError>::unclosed_list(span).into()
  }
}
