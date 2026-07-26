//! GraphQLx interface type definitions.

use super::*;

pub(super) fn interface_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InterfaceTypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let cursor = *inp.cursor();
  let name = definition_name(inp)?;
  let implements: Option<ImplementInterfaces<GraphqlxSlice<'inp, Src>>> =
    try_implements(inp)?.into();
  let directives = optional_const_directives(inp)?;
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => Some(where_clause),
    ParseAttempt::Decline => None,
  };
  let fields_definition = match (where_clause, try_fields_definition(inp)?) {
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
  Ok(InterfaceTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    implements,
    directives,
    fields_definition,
  ))
}

definition_parser!(
  /// Parses a GraphQLx interface type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Interface Type Definition specification](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  pub interface_type_definition,
  inp,
  InterfaceTypeDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Interface)?.start();
    interface_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a GraphQLx interface type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Interface Type Definition specification](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  S,
  InterfaceTypeDefinition<S>,
  interface_type_definition,
  [contextual]
);
