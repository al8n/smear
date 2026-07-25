//! SDL interface type-definition parsing.

use super::*;

pub(super) fn interface_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InterfaceTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  let cursor = *inp.cursor();
  let name = take_name(inp)?;
  let implements = match try_implements(inp)? {
    ParseAttempt::Accept(implements) => Some(implements),
    ParseAttempt::Decline => None,
  };
  let directives = optional_const_directives(inp)?;
  let fields_definition = match try_fields_definition(inp)? {
    ParseAttempt::Accept(fields) => Some(fields),
    ParseAttempt::Decline => None,
  };
  Ok(InterfaceTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    implements,
    directives,
    fields_definition,
  ))
}

definition_parser!(
  /// Parses an interface type definition.
  ///
  /// See the [GraphQL Interface Type Definition specification](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  pub interface_type_definition,
  inp,
  InterfaceTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Interface)?.start();
    interface_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses an interface type definition.
  ///
  /// See the [GraphQL Interface Type Definition specification](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  S,
  InterfaceTypeDefinition<S>,
  interface_type_definition,
  [contextual]
);
