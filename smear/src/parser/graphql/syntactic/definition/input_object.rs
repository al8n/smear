//! SDL input-object type-definition parsing.

use super::*;

definition_parser!(
  /// Parses a nonempty SDL input-fields definition.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  pub input_fields_definition,
  inp,
  InputFieldsDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    input_value_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| InputFieldsDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL input-fields definition, declining when `{` is absent.
  pub try_input_fields_definition,
  inp,
  ParseAttempt<InputFieldsDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    input_fields_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

pub(super) fn input_object_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InputObjectTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
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
  let fields_definition = match try_input_fields_definition(inp)? {
    ParseAttempt::Accept(fields) => Some(fields),
    ParseAttempt::Decline => None,
  };
  Ok(InputObjectTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    directives,
    fields_definition,
  ))
}

definition_parser!(
  /// Parses an input object type definition.
  ///
  /// See the [GraphQL Input Object Type Definition specification](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  pub input_object_type_definition,
  inp,
  InputObjectTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Input)?.start();
    input_object_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a nonempty SDL input-fields definition.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  S,
  InputFieldsDefinition<S>,
  input_fields_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL input-fields definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  S,
  InputFieldsDefinition<S>,
  try_input_fields_definition,
  [contextual]
);

impl_definition_api!(
  /// Parses an input object type definition.
  ///
  /// See the [GraphQL Input Object Type Definition specification](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  S,
  InputObjectTypeDefinition<S>,
  input_object_type_definition,
  [contextual]
);
