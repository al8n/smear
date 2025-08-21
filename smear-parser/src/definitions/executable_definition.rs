use chumsky::{extra::ParserExtra, input::Input, prelude::*};

#[derive(Debug, Clone, derive_more::IsVariant, derive_more::Unwrap, derive_more::TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDefinition<OperationDefinition, FragmentDefinition> {
  Operation(OperationDefinition),
  Fragment(FragmentDefinition),
}

impl<OperationDefinition, FragmentDefinition>
  ExecutableDefinition<OperationDefinition, FragmentDefinition>
{
  pub fn parser_with<'src, I, E, OP, FP>(
    operation_definition_parser: OP,
    fragment_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Input<'src>,
    E: ParserExtra<'src, I>,
    OP: Parser<'src, I, OperationDefinition, E> + Clone,
    FP: Parser<'src, I, FragmentDefinition, E> + Clone,
  {
    choice((
      operation_definition_parser.map(Self::Operation),
      fragment_definition_parser.map(Self::Fragment),
    ))
  }
}
