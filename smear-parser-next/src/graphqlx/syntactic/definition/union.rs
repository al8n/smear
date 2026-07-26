//! GraphQLx union type definitions and member paths.

use super::*;
use crate::combinator::{pipe, try_pipe};

fn union_members_after_equal<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UnionMemberTypes<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
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
  let _leading = try_pipe(inp)?;
  let first = type_path(inp)?;
  let members: Vec<TypePath<GraphqlxSlice<'inp, Src>>> = pipe
    .ignore_then(type_path)
    .repeated_while::<_, U1>(decide_pipe_tail::<Src, Ctx>)
    .collect_with(Vec::from([first]))
    .parse_input(inp)?;
  let end = members
    .last()
    .expect("union member clauses contain their first path")
    .span()
    .end();
  Ok(UnionMemberTypes::new(SimpleSpan::new(start, end), members))
}

definition_parser!(
  /// Parses a committed GraphQLx union member clause.
  ///
  /// Qualified member paths are a GraphQLx extension.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  pub union_members,
  inp,
  UnionMemberTypes<GraphqlxSlice<'inp, Src>>,
  [],
  {
    match try_equal(inp)? {
      ParseAttempt::Accept(equal) => union_members_after_equal(inp, equal.span().start()),
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Equal),
    }
  }
);

definition_parser!(
  /// Attempts a GraphQLx union member clause without consuming when `=` is absent.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  pub try_union_members,
  inp,
  ParseAttempt<UnionMemberTypes<GraphqlxSlice<'inp, Src>>>,
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
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UnionTypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let directives = optional_const_directives(inp)?;
  let members = try_union_members(inp)?;
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => {
      if matches!(&members, ParseAttempt::Decline) {
        return expected_definition_phase(inp, Expectation::Equal);
      }
      Some(where_clause)
    }
    ParseAttempt::Decline => None,
  };
  let member_types = match (members, where_clause) {
    (ParseAttempt::Accept(members), Some(where_clause)) => Some(Constrained::new(
      SimpleSpan::new(members.span().start(), where_clause.span().end()),
      Some(where_clause),
      members,
    )),
    (ParseAttempt::Accept(members), None) => {
      let span = *members.span();
      Some(Constrained::new(span, None, members))
    }
    (ParseAttempt::Decline, None) => None,
    (ParseAttempt::Decline, Some(_)) => unreachable!("a where clause requires union members"),
  };
  Ok(UnionTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    directives,
    member_types,
  ))
}

definition_parser!(
  /// Parses a GraphQLx union type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Union Type Definition specification](https://spec.graphql.org/draft/#UnionTypeDefinition).
  pub union_type_definition,
  inp,
  UnionTypeDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Union)?.start();
    union_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a GraphQLx union member clause.
  ///
  /// Qualified member paths are a GraphQLx extension.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  S,
  UnionMemberTypes<S>,
  union_members,
  []
);
impl_definition_api!(
  /// Parses a GraphQLx union type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Union Type Definition specification](https://spec.graphql.org/draft/#UnionTypeDefinition).
  S,
  UnionTypeDefinition<S>,
  union_type_definition,
  [contextual]
);
