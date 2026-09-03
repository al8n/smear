//! GraphQLx scalar type definitions.

use super::*;

pub(super) fn scalar_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ScalarTypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let name = definition_name(inp)?;
  let directives = optional_const_directives(inp)?;
  Ok(ScalarTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    directives,
  ))
}

definition_parser!(
  /// Parses a GraphQLx scalar type definition.
  ///
  /// See the [GraphQL Scalar Type Definition specification](https://spec.graphql.org/draft/#ScalarTypeDefinition).
  pub scalar_type_definition,
  inp,
  ScalarTypeDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Scalar)?.start();
    scalar_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a GraphQLx scalar type definition.
  ///
  /// See the [GraphQL Scalar Type Definition specification](https://spec.graphql.org/draft/#ScalarTypeDefinition).
  S,
  ScalarTypeDefinition<S>,
  scalar_type_definition,
  [contextual]
);
