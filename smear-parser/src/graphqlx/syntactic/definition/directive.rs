//! GraphQLx SDL directive definitions and directive locations.

use super::*;
use crate::combinator::{pipe, try_pipe};

#[inline]
fn classify_location(keyword: ContextualKeyword, span: SimpleSpan) -> Option<Location> {
  Some(match keyword {
    ContextualKeyword::QueryLocation => ExecutableDirectiveLocation::query(span).into(),
    ContextualKeyword::MutationLocation => ExecutableDirectiveLocation::mutation(span).into(),
    ContextualKeyword::SubscriptionLocation => {
      ExecutableDirectiveLocation::subscription(span).into()
    }
    ContextualKeyword::FieldLocation => ExecutableDirectiveLocation::field(span).into(),
    ContextualKeyword::FragmentDefinitionLocation => {
      ExecutableDirectiveLocation::fragment_definition(span).into()
    }
    ContextualKeyword::FragmentSpreadLocation => {
      ExecutableDirectiveLocation::fragment_spread(span).into()
    }
    ContextualKeyword::InlineFragmentLocation => {
      ExecutableDirectiveLocation::inline_fragment(span).into()
    }
    ContextualKeyword::VariableDefinitionLocation => {
      ExecutableDirectiveLocation::variable_definition(span).into()
    }
    ContextualKeyword::SchemaLocation => TypeSystemDirectiveLocation::schema(span).into(),
    ContextualKeyword::ScalarLocation => TypeSystemDirectiveLocation::scalar(span).into(),
    ContextualKeyword::ObjectLocation => TypeSystemDirectiveLocation::object(span).into(),
    ContextualKeyword::FieldDefinitionLocation => {
      TypeSystemDirectiveLocation::field_definition(span).into()
    }
    ContextualKeyword::ArgumentDefinitionLocation => {
      TypeSystemDirectiveLocation::argument_definition(span).into()
    }
    ContextualKeyword::InterfaceLocation => TypeSystemDirectiveLocation::interface(span).into(),
    ContextualKeyword::UnionLocation => TypeSystemDirectiveLocation::union(span).into(),
    ContextualKeyword::EnumLocation => TypeSystemDirectiveLocation::r#enum(span).into(),
    ContextualKeyword::EnumValueLocation => TypeSystemDirectiveLocation::enum_value(span).into(),
    ContextualKeyword::InputObjectLocation => {
      TypeSystemDirectiveLocation::input_object(span).into()
    }
    ContextualKeyword::InputFieldDefinitionLocation => {
      TypeSystemDirectiveLocation::input_field_definition(span).into()
    }
    _ => return None,
  })
}

definition_parser!(
  /// Parses one GraphQLx directive location.
  ///
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub location,
  inp,
  Location,
  [contextual],
  {
    match inp.next_or_stop()? {
      Some(Spanned { span, data: token }) => match keyword_of(&token)
        .and_then(|keyword| classify_location(keyword, span))
      {
        Some(location) => Ok(location),
        None => Err(
          DialectGraphqlxError::unexpected_token(
            token.kind(),
            Expectation::Keyword("directive location"),
            span,
          )
          .into(),
        ),
      },
      None => expected_definition_phase(inp, Expectation::Keyword("directive location")),
    }
  }
);

definition_parser!(
  /// Parses a nonempty GraphQLx directive-locations list.
  ///
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub directive_locations,
  inp,
  DirectiveLocations,
  [contextual],
  {
    let leading = try_pipe(inp)?;
    let first = location(inp)?;
    let start = match leading {
      ParseAttempt::Accept(pipe) => pipe.span().start(),
      ParseAttempt::Decline => first.span().start(),
    };
    let locations: Vec<Location> = pipe
      .ignore_then(location)
      .repeated_while::<_, U1>(decide_pipe_tail::<Src, Ctx>)
      .collect_with(Vec::from([first]))
      .parse_input(inp)?;
    let end = locations
      .last()
      .expect("directive location lists contain their first location")
      .span()
      .end();
    Ok(DirectiveLocations::new(SimpleSpan::new(start, end), locations))
  }
);

pub(super) fn directive_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<DirectiveDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_at(inp)? {
    ParseAttempt::Accept(_) => {}
    ParseAttempt::Decline => return expected_definition_phase(inp, Expectation::At),
  }
  let name = definition_name(inp)?;
  let arguments_definition: Option<ArgumentsDefinition<GraphqlxSlice<'inp, Src>>> =
    try_arguments_definition(inp)?.into();
  let repeatable = matches!(
    try_contextual_keyword(inp, ContextualKeyword::Repeatable)?,
    ParseAttempt::Accept(_)
  );
  take_contextual_keyword(inp, ContextualKeyword::On)?;
  let locations = directive_locations(inp)?;
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => Some(where_clause),
    ParseAttempt::Decline => None,
  };
  let locations = match where_clause {
    Some(where_clause) => Constrained::new(
      SimpleSpan::new(locations.span().start(), where_clause.span().end()),
      Some(where_clause),
      locations,
    ),
    None => {
      let span = *locations.span();
      Constrained::new(span, None, locations)
    }
  };
  let end = locations.span().end();
  Ok(DirectiveDefinition::new(
    SimpleSpan::new(start, end),
    name,
    arguments_definition,
    repeatable,
    locations,
  ))
}

definition_parser!(
  /// Parses a GraphQLx directive definition.
  ///
  /// GraphQLx accepts its extended type and constant-value forms in directive
  /// arguments.
  ///
  /// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition).
  pub directive_definition,
  inp,
  DirectiveDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Directive)?.start();
    directive_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a GraphQLx directive definition.
  ///
  /// GraphQLx accepts its extended type and constant-value forms in directive
  /// arguments.
  ///
  /// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition).
  S,
  DirectiveDefinition<S>,
  directive_definition,
  [contextual]
);

impl DirectiveLocations {
  /// Parses a nonempty GraphQLx directive-locations list.
  ///
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize> + ?Sized,
    GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
  {
    directive_locations(inp)
  }
}

impl Location {
  /// Parses one GraphQLx directive location.
  ///
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize> + ?Sized,
    GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
  {
    location(inp)
  }
}
