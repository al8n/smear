use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  char::Char,
  language::{ignored::ignored, input_value::StringValue},
  name::Name,
  source::Source,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct NamedOperationDefinition<
  OperationType,
  VariableDefinitions,
  Directives,
  SelectionSet,
  Span,
> {
  span: Span,
  description: Option<StringValue<Span>>,
  operation_type: OperationType,
  name: Option<Name<Span>>,
  variable_definitions: Option<VariableDefinitions>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<OperationType, VariableDefinitions, Directives, SelectionSet, Span>
  NamedOperationDefinition<OperationType, VariableDefinitions, Directives, SelectionSet, Span>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  #[inline]
  pub const fn name(&self) -> Option<&Name<Span>> {
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
    Span,
    Option<StringValue<Span>>,
    OperationType,
    Option<Name<Span>>,
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
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
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
            span: Spanned::from_map_extra(sp),
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
pub enum OperationDefinition<OperationType, VariableDefinitions, Directives, SelectionSet, Span> {
  Named(
    NamedOperationDefinition<OperationType, VariableDefinitions, Directives, SelectionSet, Span>,
  ),
  Shorten(SelectionSet),
}

impl<OperationType, VariableDefinitions, Directives, SelectionSet, Span>
  OperationDefinition<OperationType, VariableDefinitions, Directives, SelectionSet, Span>
{
  #[inline]
  pub fn parser_with<'src, I, E, OP, VP, DP, SP>(
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

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
