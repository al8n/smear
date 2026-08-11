//! SDL scalar type-definition parsing.

use super::*;

pub(super) fn scalar_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ScalarTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let name = take_name(inp)?;
  let directives = optional_const_directives(inp)?;
  Ok(ScalarTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    directives,
  ))
}

definition_parser!(
  /// Parses a scalar type definition.
  ///
  /// See the [GraphQL Scalar Type Definition specification](https://spec.graphql.org/draft/#ScalarTypeDefinition).
  pub scalar_type_definition,
  inp,
  ScalarTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Scalar)?.start();
    scalar_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a scalar type definition.
  ///
  /// See the [GraphQL Scalar Type Definition specification](https://spec.graphql.org/draft/#ScalarTypeDefinition).
  S,
  ScalarTypeDefinition<S>,
  scalar_type_definition,
  [contextual]
);
