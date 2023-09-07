use chumsky::{extra::ParserExtra, input::Input, prelude::*};

#[derive(Debug, Clone, derive_more::IsVariant, derive_more::Unwrap, derive_more::TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinition<SchemaDefinition, TypeDefinition, DirectiveDefinition> {
  Schema(SchemaDefinition),
  Type(TypeDefinition),
  Directive(DirectiveDefinition),
}

impl<SchemaDefinition, TypeDefinition, DirectiveDefinition>
  TypeSystemDefinition<SchemaDefinition, TypeDefinition, DirectiveDefinition>
{
  pub fn parser_with<'src, I, E, SP, TP, DP>(
    schema_definition_parser: SP,
    type_definition_parser: TP,
    directive_definition_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Input<'src>,
    E: ParserExtra<'src, I>,
    SP: Parser<'src, I, SchemaDefinition, E> + Clone,
    TP: Parser<'src, I, TypeDefinition, E> + Clone,
    DP: Parser<'src, I, DirectiveDefinition, E> + Clone,
  {
    choice((
      schema_definition_parser.map(Self::Schema),
      type_definition_parser.map(Self::Type),
      directive_definition_parser.map(Self::Directive),
    ))
  }
}
