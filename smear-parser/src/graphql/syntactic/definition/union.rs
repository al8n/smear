//! SDL union type-definition parsing.

use super::*;
use crate::combinator::{extent_end, pipe, try_pipe};

fn take_equal<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<tokora::punct::Equal<SimpleSpan, (), GraphQL>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_definition_phase(inp, Expectation::Equal, |token| token.is_equal())?;
  equal(inp)
}

fn union_members_after_equal<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UnionMemberTypes<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let _leading = try_pipe(inp)?;
  let first = take_name(inp)?;
  let tail: Vec<Name<GraphqlSlice<'inp, Src>>> = pipe
    .then(take_name)
    .map(|(_, name)| name)
    .repeated_while::<_, U1>(decide_pipe_tail::<Src, Ctx>)
    .collect_with(Vec::new())
    .parse_input(inp)?;
  let end = tail
    .last()
    .map_or_else(|| first.span().end(), |name| name.span().end());
  let mut members = Vec::with_capacity(tail.len() + 1);
  members.push(first);
  members.extend(tail);
  Ok(UnionMemberTypes::new(SimpleSpan::new(start, end), members))
}

definition_parser!(
  /// Parses an SDL union-members clause.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  pub union_members,
  inp,
  UnionMemberTypes<Name<GraphqlSlice<'inp, Src>>>,
  [],
  {
    let equal = take_equal(inp)?;
    union_members_after_equal(inp, equal.span().start())
  }
);

definition_parser!(
  /// Attempts an SDL union-members clause, declining when `=` is absent.
  pub try_union_members,
  inp,
  ParseAttempt<UnionMemberTypes<Name<GraphqlSlice<'inp, Src>>>>,
  [],
  {
    match try_equal(inp)? {
      ParseAttempt::Accept(equal) => {
        union_members_after_equal(inp, equal.span().start()).map(ParseAttempt::Accept)
      }
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

pub(super) fn union_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UnionTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
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
  let member_types = match try_union_members(inp)? {
    ParseAttempt::Accept(member_types) => Some(member_types),
    ParseAttempt::Decline => None,
  };
  Ok(UnionTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    directives,
    member_types,
  ))
}

definition_parser!(
  /// Parses a union type definition.
  ///
  /// See the [GraphQL Union Type Definition specification](https://spec.graphql.org/draft/#UnionTypeDefinition).
  pub union_type_definition,
  inp,
  UnionTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Union)?.start();
    union_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses an SDL union-members clause.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  S,
  UnionMemberTypes<Name<S>>,
  union_members,
  []
);
impl_definition_try_api!(
  /// Attempts an SDL union-members clause without consuming when `=` is absent.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  S,
  UnionMemberTypes<Name<S>>,
  try_union_members,
  []
);

impl_definition_api!(
  /// Parses a union type definition.
  ///
  /// See the [GraphQL Union Type Definition specification](https://spec.graphql.org/draft/#UnionTypeDefinition).
  S,
  UnionTypeDefinition<S>,
  union_type_definition,
  [contextual]
);
