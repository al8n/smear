//! SDL implemented-interface parsing.

use super::*;

fn implements_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlToken<'inp, Src>,
        <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let Spanned {
    span,
    data: interfaces,
  } = take_name
    // `separated_by_ampersand_while` currently defaults its separator marker's
    // language to `()`. Keep the native engine while selecting GraphQL explicitly.
    .separated_while::<Ampersand<(), (), GraphQL>, _, U1>(decide_identifier_tail::<Src, Ctx>)
    .allow_leading()
    .at_least(1)
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)?;
  Ok(ImplementInterfaces::new(
    SimpleSpan::new(start, span.end()),
    interfaces,
  ))
}

definition_parser!(
  /// Parses an SDL `implements` clause.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  pub implements,
  inp,
  ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Implements)?.start();
    implements_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Attempts an SDL `implements` clause, declining when `implements` is absent.
  pub try_implements,
  inp,
  ParseAttempt<ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>>,
  [contextual],
  {
    match try_implements_keyword(inp)? {
      ParseAttempt::Accept(keyword) => {
        implements_after_keyword(inp, keyword.span().start()).map(ParseAttempt::Accept)
      }
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

impl_definition_api!(
  /// Parses an SDL `implements` clause.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  S,
  ImplementInterfaces<Name<S>>,
  implements,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL `implements` clause without consuming when the keyword is absent.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  S,
  ImplementInterfaces<Name<S>>,
  try_implements,
  [contextual]
);
