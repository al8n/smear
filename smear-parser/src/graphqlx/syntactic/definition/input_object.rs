//! GraphQLx input-object type definitions.

use super::*;

definition_parser!(
  /// Parses a nonempty GraphQLx input-fields definition.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  pub input_fields_definition,
  inp,
  InputFieldsDefinition<GraphqlxSlice<'inp, Src>>,
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
  /// Attempts an input-fields definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  pub try_input_fields_definition,
  inp,
  ParseAttempt<InputFieldsDefinition<GraphqlxSlice<'inp, Src>>>,
  [contextual],
  {
    input_fields_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

pub(super) fn input_object_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InputObjectTypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => Some(where_clause),
    ParseAttempt::Decline => None,
  };
  let fields_definition = match (where_clause, try_input_fields_definition(inp)?) {
    (Some(where_clause), ParseAttempt::Accept(fields)) => Some(Constrained::new(
      SimpleSpan::new(where_clause.span().start(), fields.span().end()),
      Some(where_clause),
      fields,
    )),
    (Some(_), ParseAttempt::Decline) => {
      return expected_definition_phase(inp, Expectation::LBrace);
    }
    (None, ParseAttempt::Accept(fields)) => {
      let span = *fields.span();
      Some(Constrained::new(span, None, fields))
    }
    (None, ParseAttempt::Decline) => None,
  };
  Ok(InputObjectTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    directives,
    fields_definition,
  ))
}

definition_parser!(
  /// Parses a GraphQLx input object type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Input Object Type Definition specification](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  pub input_object_type_definition,
  inp,
  InputObjectTypeDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Input)?.start();
    input_object_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a nonempty GraphQLx input-fields definition.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  S,
  InputFieldsDefinition<S>,
  input_fields_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx input object type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Input Object Type Definition specification](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  S,
  InputObjectTypeDefinition<S>,
  input_object_type_definition,
  [contextual]
);
