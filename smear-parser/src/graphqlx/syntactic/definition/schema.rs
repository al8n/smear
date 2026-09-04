//! GraphQLx schema definitions and root operation mappings.

use super::*;

fn operation_type<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<super::super::ast::OperationType, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next_or_stop()? {
    Some(Spanned { span, data: token }) => match keyword_of(&token) {
      Some(ContextualKeyword::Query) => Ok(super::super::ast::OperationType::Query(span)),
      Some(ContextualKeyword::Mutation) => Ok(super::super::ast::OperationType::Mutation(span)),
      Some(ContextualKeyword::Subscription) => {
        Ok(super::super::ast::OperationType::Subscription(span))
      }
      _ => Err(
        DialectGraphqlxError::unexpected_token(
          token.kind(),
          Expectation::Keyword("query, mutation, or subscription"),
          span,
        )
        .into(),
      ),
    },
    None => expected_definition_phase(
      inp,
      Expectation::Keyword("query, mutation, or subscription"),
    ),
  }
}

definition_parser!(
  /// Parses one GraphQLx root operation type definition.
  ///
  /// GraphQLx permits a qualified type path after the colon.
  ///
  /// See the [GraphQL Root Operation Type Definition specification](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
  pub root_operation_type_definition,
  inp,
  RootOperationTypeDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let node_start = extent_start(inp)?;
    let operation_type = operation_type(inp)?;
    take_colon(inp)?;
    let name = type_path(inp)?;
    Ok(RootOperationTypeDefinition::new(
      extent_since(inp, node_start),
      operation_type,
      name,
    ))
  }
);

definition_parser!(
  /// Parses a nonempty GraphQLx root-operation-types definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  pub root_operation_types_definition,
  inp,
  RootOperationTypesDefinition<GraphqlxSlice<'inp, Src>>,
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

definition_parser!(
  /// Attempts root operation type definitions without consuming when `{` is absent.
  pub(super) try_root_operation_types_definition,
  inp,
  ParseAttempt<RootOperationTypesDefinition<GraphqlxSlice<'inp, Src>>>,
  [contextual],
  {
    root_operation_types_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

pub(super) fn schema_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<SchemaDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
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
  /// Parses a GraphQLx schema definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  pub schema_definition,
  inp,
  SchemaDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Schema)?.start();
    schema_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses one GraphQLx root operation type definition.
  ///
  /// GraphQLx permits a qualified type path after the colon.
  ///
  /// See the [GraphQL Root Operation Type Definition specification](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
  S,
  RootOperationTypeDefinition<S>,
  root_operation_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty GraphQLx root-operation-types definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  S,
  RootOperationTypesDefinition<S>,
  root_operation_types_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx schema definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  S,
  SchemaDefinition<S>,
  schema_definition,
  [contextual]
);
