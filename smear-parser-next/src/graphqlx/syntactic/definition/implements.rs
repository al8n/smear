//! GraphQLx `implements` clauses.

use super::*;
use crate::combinator::{ampersand, try_ampersand};

fn implements_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ImplementInterfaces<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
    > + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let _leading = try_ampersand(inp)?;
  let first = type_path(inp)?;
  let interfaces: Vec<TypePath<GraphqlxSlice<'inp, Src>>> = ampersand
    .ignore_then(type_path)
    .repeated_while::<_, U1>(decide_ampersand_tail::<Src, Ctx>)
    .collect_with(Vec::from([first]))
    .parse_input(inp)?;
  let end = interfaces
    .last()
    .expect("implements clauses contain their first path")
    .span()
    .end();
  Ok(ImplementInterfaces::new(
    SimpleSpan::new(start, end),
    interfaces,
  ))
}

definition_parser!(
  /// Parses a GraphQLx `implements` clause with path-capable interfaces.
  ///
  /// Qualified interface paths are a GraphQLx extension.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  pub implements,
  inp,
  ImplementInterfaces<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Implements)?.start();
    implements_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Attempts a GraphQLx `implements` clause without consuming when absent.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  pub try_implements,
  inp,
  ParseAttempt<ImplementInterfaces<GraphqlxSlice<'inp, Src>>>,
  [contextual],
  {
    match try_contextual_keyword(inp, ContextualKeyword::Implements)? {
      ParseAttempt::Accept(span) => implements_after_keyword(inp, span.start()).map(ParseAttempt::Accept),
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

impl_definition_api!(
  /// Parses a GraphQLx `implements` clause.
  ///
  /// Qualified interface paths are a GraphQLx extension.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  S,
  ImplementInterfaces<S>,
  implements,
  [contextual]
);
