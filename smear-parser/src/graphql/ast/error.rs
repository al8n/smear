use crate::{
  error::ParseVariableValueError,
  hints::{
    EnumTypeExtensionHint, InputObjectTypeExtensionHint, InterfaceTypeExtensionHint,
    ObjectTypeExtensionHint, SchemaExtensionHint, UnionTypeExtensionHint, VariableValueHint,
  },
};
use logosky::{
  Token,
  utils::{IntoComponents, Span},
};
use smear_scaffold::error::{
  UnclosedBraceError, UnclosedBracketError, UnexpectedEndOfEnumExtensionError,
  UnexpectedEndOfInputObjectExtensionError, UnexpectedEndOfInterfaceExtensionError,
  UnexpectedEndOfObjectExtensionError, UnexpectedEndOfSchemaExtensionError,
  UnexpectedEndOfUnionExtensionError,
};

use super::*;

impl<'a, S> ParseVariableValueError<Name<S>> for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Name<S>, span: Span) -> Self {
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

impl<'a, S> ParseVariableValueError<Name<S>> for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn missing_dollar_token(name: Name<S>, span: Span) -> Self {
    <SyntacticTokenError<'a, S> as ParseVariableValueError<Name<S>>>::missing_dollar_token(
      name, span,
    )
    .into()
  }

  #[inline]
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    <SyntacticTokenError<'a, S> as ParseVariableValueError<Name<S>>>::unexpected_end_of_variable_value(
      hint, span,
    )
    .into()
  }
}

impl<'a, S> UnexpectedEndOfObjectExtensionError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    Self::unexpected_end_of_object_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfObjectExtensionError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self {
    <SyntacticTokenError<'a, S> as UnexpectedEndOfObjectExtensionError>::unexpected_end_of_object_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfInterfaceExtensionError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    Self::unexpected_end_of_interface_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfInterfaceExtensionError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self {
    <SyntacticTokenError<'a, S> as UnexpectedEndOfInterfaceExtensionError>::unexpected_end_of_interface_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfEnumExtensionError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::unexpected_end_of_enum_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfEnumExtensionError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    <SyntacticTokenError<'a, S> as UnexpectedEndOfEnumExtensionError>::unexpected_end_of_enum_extension(
      span, hint,
    )
    .into()
  }
}

impl<'a, S> UnexpectedEndOfInputObjectExtensionError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::unexpected_end_of_input_object_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfInputObjectExtensionError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    <SyntacticTokenError<'a, S> as UnexpectedEndOfInputObjectExtensionError>::unexpected_end_of_input_object_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfSchemaExtensionError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::unexpected_end_of_schema_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfSchemaExtensionError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    <SyntacticTokenError<'a, S> as UnexpectedEndOfSchemaExtensionError>::unexpected_end_of_schema_extension(span, hint).into()
  }
}

impl<'a, S> UnexpectedEndOfUnionExtensionError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::unexpected_end_of_union_extension(span, hint)
  }
}

impl<'a, S> UnexpectedEndOfUnionExtensionError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    <SyntacticTokenError<'a, S> as UnexpectedEndOfUnionExtensionError>::unexpected_end_of_union_extension(
      span, hint,
    )
    .into()
  }
}

impl<'a, S> UnclosedBraceError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_brace(span: Span) -> Self {
    Self::unclosed_object(span)
  }
}

impl<'a, S> UnclosedBraceError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_brace(span: Span) -> Self {
    <SyntacticTokenError<'a, S> as UnclosedBraceError>::unclosed_brace(span).into()
  }
}

impl<'a, S> UnclosedBracketError for SyntacticTokenError<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_bracket(span: Span) -> Self {
    Self::unclosed_list(span)
  }
}

impl<'a, S> UnclosedBracketError for SyntacticTokenErrors<'a, S>
where
  SyntacticToken<S>: Token<'a>,
{
  #[inline]
  fn unclosed_bracket(span: Span) -> Self {
    <SyntacticTokenError<'a, S> as UnclosedBracketError>::unclosed_bracket(span).into()
  }
}
