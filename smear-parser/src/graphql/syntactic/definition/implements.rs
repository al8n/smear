//! SDL implemented-interface parsing.

use super::*;
use crate::combinator::{ampersand, try_ampersand};

fn implements_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let _leading = try_ampersand(inp)?;
  let first = take_name(inp)?;
  let tail: Vec<Name<GraphqlSlice<'inp, Src>>> = ampersand
    .then(take_name)
    .map(|(_, name)| name)
    .repeated_while::<_, U1>(decide_ampersand_tail::<Src, Ctx>)
    .collect_with(Vec::new())
    .parse_input(inp)?;
  let end = tail
    .last()
    .map_or_else(|| first.span().end(), |name| name.span().end());
  let mut interfaces = Vec::with_capacity(tail.len() + 1);
  interfaces.push(first);
  interfaces.extend(tail);
  Ok(ImplementInterfaces::new(
    SimpleSpan::new(start, end),
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
