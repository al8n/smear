//! SDL enum type-definition parsing.

use super::*;

fn take_enum_value<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_definition_phase(inp, Expectation::EnumValue, |token| {
    token.is_identifier()
      && !matches!(
        token.downcast_ref(),
        Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null)
      )
  })?;
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(value) => Ok(Name::new(span, value)),
        token => Err(
          DialectGraphqlError::unexpected_token(token.kind(), Expectation::EnumValue, span).into(),
        ),
      }
    }
    None => expected_definition_phase(inp, Expectation::EnumValue),
  }
}

definition_parser!(
  /// Parses an SDL enum-value definition.
  ///
  /// `true`, `false`, and `null` are rejected as required by `EnumValue`.
  /// See the [GraphQL Enum Value Definition specification](https://spec.graphql.org/draft/#EnumValueDefinition).
  pub enum_value_definition,
  inp,
  EnumValueDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let value = take_enum_value(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = inp.span_since(&cursor);
    Ok(Described::new(
      span,
      description,
      EnumValueDefinitionCore::new(span, value, directives),
    ))
  }
);

definition_parser!(
  /// Parses a nonempty SDL enum-values definition.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  pub enum_values_definition,
  inp,
  EnumValuesDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    enum_value_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| EnumValuesDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL enum-values definition, declining when `{` is absent.
  pub try_enum_values_definition,
  inp,
  ParseAttempt<EnumValuesDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    enum_values_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

pub(super) fn enum_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<EnumTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let cursor = *inp.cursor();
  let name = take_name(inp)?;
  let directives = optional_const_directives(inp)?;
  let enum_values_definition = match try_enum_values_definition(inp)? {
    ParseAttempt::Accept(values) => Some(values),
    ParseAttempt::Decline => None,
  };
  Ok(EnumTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    directives,
    enum_values_definition,
  ))
}

definition_parser!(
  /// Parses an enum type definition.
  ///
  /// See the [GraphQL Enum Type Definition specification](https://spec.graphql.org/draft/#EnumTypeDefinition).
  pub enum_type_definition,
  inp,
  EnumTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Enum)?.start();
    enum_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses an SDL enum value definition.
  ///
  /// See the [GraphQL Enum Value Definition specification](https://spec.graphql.org/draft/#EnumValueDefinition).
  S,
  EnumValueDefinition<S>,
  enum_value_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL enum-values definition.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  S,
  EnumValuesDefinition<S>,
  enum_values_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL enum-values definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  S,
  EnumValuesDefinition<S>,
  try_enum_values_definition,
  [contextual]
);

impl_definition_api!(
  /// Parses an enum type definition.
  ///
  /// See the [GraphQL Enum Type Definition specification](https://spec.graphql.org/draft/#EnumTypeDefinition).
  S,
  EnumTypeDefinition<S>,
  enum_type_definition,
  [contextual]
);
