//! GraphQLx enum type definitions.

use super::*;

fn take_enum_value<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next_or_stop()? {
    Some(Spanned { span, data: token })
      if token.is_identifier()
        && !matches!(
          keyword_of(&token),
          Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null)
        ) =>
    {
      let GraphqlxToken::<'inp, Src>::Identifier(value) = token else {
        unreachable!("enum-value guard accepted a non-identifier token")
      };
      Ok(Name::new(span, value))
    }
    Some(Spanned { span, data: token }) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), Expectation::EnumValue, span).into())
    }
    None => expected_definition_phase(inp, Expectation::EnumValue),
  }
}

definition_parser!(
  /// Parses one GraphQLx SDL enum-value definition.
  ///
  /// See the [GraphQL Enum Value Definition specification](https://spec.graphql.org/draft/#EnumValueDefinition).
  pub enum_value_definition,
  inp,
  EnumValueDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let node_start = extent_start(inp)?;
    let description = description(inp)?;
    let value = take_enum_value(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = extent_since(inp, node_start);
    Ok(Described::new(
      span,
      description,
      EnumValueDefinitionCore::new(span, value, directives),
    ))
  }
);

definition_parser!(
  /// Parses a nonempty GraphQLx SDL enum-values definition.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  pub enum_values_definition,
  inp,
  EnumValuesDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    enum_value_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| EnumValuesDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an enum-values definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  pub try_enum_values_definition,
  inp,
  ParseAttempt<EnumValuesDefinition<GraphqlxSlice<'inp, Src>>>,
  [contextual],
  {
    enum_values_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

pub(super) fn enum_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<EnumTypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let name = definition_name(inp)?;
  let directives = optional_const_directives(inp)?;
  let enum_values_definition: Option<EnumValuesDefinition<GraphqlxSlice<'inp, Src>>> =
    try_enum_values_definition(inp)?.into();
  Ok(EnumTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    directives,
    enum_values_definition,
  ))
}

definition_parser!(
  /// Parses a GraphQLx enum type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Enum Type Definition specification](https://spec.graphql.org/draft/#EnumTypeDefinition).
  pub enum_type_definition,
  inp,
  EnumTypeDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Enum)?.start();
    enum_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses one GraphQLx SDL enum-value definition.
  ///
  /// See the [GraphQL Enum Value Definition specification](https://spec.graphql.org/draft/#EnumValueDefinition).
  S,
  EnumValueDefinition<S>,
  enum_value_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty GraphQLx SDL enum-values definition.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  S,
  EnumValuesDefinition<S>,
  enum_values_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx enum type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Enum Type Definition specification](https://spec.graphql.org/draft/#EnumTypeDefinition).
  S,
  EnumTypeDefinition<S>,
  enum_type_definition,
  [contextual]
);
