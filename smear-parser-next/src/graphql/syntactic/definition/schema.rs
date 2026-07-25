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
    let cursor = *inp.cursor();
    let operation_type = operation_type(inp)?;
    take_colon(inp)?;
    let name = take_name(inp)?;
    Ok(RootOperationTypeDefinition::new(
      inp.span_since(&cursor),
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
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| RootOperationTypesDefinition::new(span, data))
  }
);

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
    let directives = optional_const_directives(inp)?;
    let root_operation_types_definition = root_operation_types_definition(inp)?;
    Ok(SchemaDefinition::new(
      SimpleSpan::new(start, root_operation_types_definition.span().end()),
      directives,
      root_operation_types_definition,
    ))
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
