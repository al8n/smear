use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::super::{
  char::Char,
  language::{ignored::ignored, input_value::StringValue},
  name::Name,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct NamedOperationDefinition<
  OperationType,
  VariableDefinitions,
  Directives,
  SelectionSet,
  Src,
  Span,
> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  operation_type: OperationType,
  name: Option<Name<Src, Span>>,
  variable_definitions: Option<VariableDefinitions>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<OperationType, VariableDefinitions, Directives, SelectionSet, Src, Span>
  NamedOperationDefinition<OperationType, VariableDefinitions, Directives, SelectionSet, Src, Span>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  #[inline]
  pub const fn name(&self) -> Option<&Name<Src, Span>> {
    self.name.as_ref()
  }

  #[inline]
  pub const fn variable_definitions(&self) -> Option<&VariableDefinitions> {
    self.variable_definitions.as_ref()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    Option<StringValue<Src, Span>>,
    OperationType,
    Option<Name<Src, Span>>,
    Option<VariableDefinitions>,
    Option<Directives>,
    SelectionSet,
  ) {
    (
      self.span,
      self.description,
      self.operation_type,
      self.name,
      self.variable_definitions,
      self.directives,
      self.selection_set,
    )
  }

  #[inline]
  pub fn parser_with<'src, I, E, OP, VP, DP, SP>(
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    OP: Parser<'src, I, OperationType, E> + Clone,
    VP: Parser<'src, I, VariableDefinitions, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(operation_type_parser)
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()).or_not())
      .then(variable_definitions_parser.then_ignore(ignored()).or_not())
      .then(directives_parser.then_ignore(ignored()).or_not())
      .then(selection_set_parser)
      .map_with(
        |(
          ((((description, operation_type), name), variable_definitions), directives),
          selection_set,
        ),
         sp| {
          Self {
            span: Spanned::from(sp),
            description,
            operation_type,
            name,
            variable_definitions,
            directives,
            selection_set,
          }
        },
      )
  }
}

#[derive(
  Debug,
  Clone,
  derive_more::IsVariant,
  derive_more::From,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationDefinition<
  OperationType,
  VariableDefinitions,
  Directives,
  SelectionSet,
  Src,
  Span,
> {
  Named(
    NamedOperationDefinition<
      OperationType,
      VariableDefinitions,
      Directives,
      SelectionSet,
      Src,
      Span,
    >,
  ),
  Shorten(SelectionSet),
}

impl<OperationType, VariableDefinitions, Directives, SelectionSet, Src, Span>
  OperationDefinition<OperationType, VariableDefinitions, Directives, SelectionSet, Src, Span>
{
  #[inline]
  pub fn parser_with<'src, I, E, OP, VP, DP, SP>(
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    OP: Parser<'src, I, OperationType, E> + Clone,
    VP: Parser<'src, I, VariableDefinitions, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    choice((
      NamedOperationDefinition::parser_with(
        operation_type_parser,
        variable_definitions_parser,
        directives_parser,
        selection_set_parser.clone(),
      )
      .map(Self::Named),
      selection_set_parser.map(Self::Shorten),
    ))
  }
}
