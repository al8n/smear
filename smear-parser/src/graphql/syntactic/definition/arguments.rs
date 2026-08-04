//! SDL arguments-definition parsing.

use super::*;

fn decide_paren_tail<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token) if token.token().is_r_paren() => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_lparen_opener<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token) if token.token().is_l_paren() => Action::Continue,
    _ => Action::Stop,
  })
}

definition_parser!(
  /// Parses a nonempty SDL arguments definition.
  ///
  /// See the [GraphQL Arguments Definition specification](https://spec.graphql.org/draft/#ArgumentsDefinition).
  pub arguments_definition,
  inp,
  ArgumentsDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    input_value_definition
      .repeated_while::<_, U1>(decide_paren_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ArgumentsDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL arguments definition, declining when `(` is absent.
  pub try_arguments_definition,
  inp,
  ParseAttempt<ArgumentsDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    arguments_definition
      .peek_then_try::<_, U1>(decide_lparen_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

impl_definition_api!(
  /// Parses a nonempty SDL arguments definition.
  ///
  /// See the [GraphQL Arguments Definition specification](https://spec.graphql.org/draft/#ArgumentsDefinition).
  S,
  ArgumentsDefinition<S>,
  arguments_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL arguments definition without consuming when `(` is absent.
  ///
  /// See the [GraphQL Arguments Definition specification](https://spec.graphql.org/draft/#ArgumentsDefinition).
  S,
  ArgumentsDefinition<S>,
  try_arguments_definition,
  [contextual]
);
