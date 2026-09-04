//! SDL schema definition and root-operation parsing.

use super::*;

definition_parser!(
  /// Parses an SDL root operation type definition.
  ///
  /// See the [GraphQL Root Operation Type Definition specification](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
  pub root_operation_type_definition,
  inp,
  RootOperationTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let node_start = extent_start(inp)?;
    let operation_type = operation_type(inp)?;
    take_colon(inp)?;
    let name = take_name(inp)?;
    Ok(RootOperationTypeDefinition::new(
      extent_since(inp, node_start),
      operation_type,
      name,
    ))
  }
);

definition_parser!(
  /// Parses a nonempty SDL root-operation-types definition.
  ///
  /// See the [GraphQL Root Operation Types Definition specification](https://spec.graphql.org/draft/#RootOperationTypesDefinition).
  pub root_operation_types_definition,
  inp,
  RootOperationTypesDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    root_operation_type_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| RootOperationTypesDefinition::new(span, data))
  }
);

/// Enters a schema-definition tail after its `schema` keyword was consumed.
pub(super) fn schema_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<SchemaDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = optional_const_directives(inp)?;
  let root_operation_types_definition = root_operation_types_definition(inp)?;
  Ok(SchemaDefinition::new(
    SimpleSpan::new(start, root_operation_types_definition.span().end()),
    directives,
    root_operation_types_definition,
  ))
}

definition_parser!(
  /// Parses a schema definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  pub schema_definition,
  inp,
  SchemaDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Schema)?.start();
    schema_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses an SDL root operation type definition.
  ///
  /// See the [GraphQL Root Operation Type Definition specification](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
  S,
  RootOperationTypeDefinition<S>,
  root_operation_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL root-operation-types definition.
  ///
  /// See the [GraphQL Root Operation Types Definition specification](https://spec.graphql.org/draft/#RootOperationTypesDefinition).
  S,
  RootOperationTypesDefinition<S>,
  root_operation_types_definition,
  [contextual]
);

impl_definition_api!(
  /// Parses a schema definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  S,
  SchemaDefinition<S>,
  schema_definition,
  [contextual]
);
