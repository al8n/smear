//! SDL directive definition and location parsing.

use super::*;

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

#[inline]
fn is_location_keyword(keyword: ContextualKeyword) -> bool {
  matches!(
    keyword,
    ContextualKeyword::QueryLocation
      | ContextualKeyword::MutationLocation
      | ContextualKeyword::SubscriptionLocation
      | ContextualKeyword::FieldLocation
      | ContextualKeyword::FragmentDefinitionLocation
      | ContextualKeyword::FragmentSpreadLocation
      | ContextualKeyword::InlineFragmentLocation
      | ContextualKeyword::VariableDefinitionLocation
      | ContextualKeyword::SchemaLocation
      | ContextualKeyword::ScalarLocation
      | ContextualKeyword::ObjectLocation
      | ContextualKeyword::FieldDefinitionLocation
      | ContextualKeyword::ArgumentDefinitionLocation
      | ContextualKeyword::InterfaceLocation
      | ContextualKeyword::UnionLocation
      | ContextualKeyword::EnumLocation
      | ContextualKeyword::EnumValueLocation
      | ContextualKeyword::InputObjectLocation
      | ContextualKeyword::InputFieldDefinitionLocation
  )
}

fn decide_directive_location_tail<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token)
      if token
        .token()
        .downcast_ref()
        .is_some_and(is_location_keyword) =>
    {
      Action::Continue
    }
    Some(_) | None => Action::Stop,
  })
}

definition_parser!(
  /// Parses one GraphQL directive location.
  ///
  /// It consumes one token and classifies its contextual keyword once.
  /// See the [GraphQL Directive Location specification](https://spec.graphql.org/draft/#DirectiveLocation).
  pub location,
  inp,
  Location,
  [contextual],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token
          .downcast_ref()
          .and_then(|keyword| classify_location(keyword, span))
        {
          Some(location) => Ok(location),
          None => Err(
            DialectGraphqlError::unexpected_token(
              token.kind(),
              Expectation::DirectiveLocation,
              span,
            )
            .into(),
          ),
        }
      }
      None => expected_definition_phase(inp, Expectation::DirectiveLocation),
    }
  }
);

definition_parser!(
  /// Parses a nonempty SDL directive-locations list.
  ///
  /// A leading `|` is accepted; a trailing `|` remains a separator diagnostic.
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub directive_locations,
  inp,
  DirectiveLocations<Location>,
  [contextual],
  {
    let Spanned {
      span,
      data: locations,
    } = location
      .separated_while::<Pipe<(), (), GraphQL>, _, U1>(
        decide_directive_location_tail::<Src, Ctx>,
      )
      .allow_leading()
      .at_least(1)
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)?;
    Ok(DirectiveLocations::new(span, locations))
  }
);

/// Enters a directive-definition tail after its `directive` keyword was consumed.
pub(super) fn directive_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<DirectiveDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
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
  guard_definition_phase(inp, Expectation::At, |token| token.is_at())?;
  at(inp)?;
  let name = take_name(inp)?;
  let arguments_definition = match try_arguments_definition(inp)? {
    ParseAttempt::Accept(arguments) => Some(arguments),
    ParseAttempt::Decline => None,
  };
  let repeatable = matches!(try_repeatable_keyword(inp)?, ParseAttempt::Accept(_));
  take_contextual_keyword(inp, ContextualKeyword::On)?;
  let locations = directive_locations(inp)?;
  Ok(DirectiveDefinition::new(
    SimpleSpan::new(start, locations.span().end()),
    name,
    arguments_definition,
    repeatable,
    locations,
  ))
}

definition_parser!(
  /// Parses a directive definition.
  ///
  /// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition).
  pub directive_definition,
  inp,
  DirectiveDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Directive)?.start();
    directive_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses a directive definition.
  ///
  /// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition).
  S,
  DirectiveDefinition<S>,
  directive_definition,
  [contextual]
);

impl DirectiveLocations<Location> {
  /// Parses a nonempty SDL directive-locations list.
  ///
  /// The lexer source is inferred from `inp`.
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
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
    directive_locations(inp)
  }
}

impl Location {
  /// Parses one SDL directive location.
  ///
  /// The lexer source is inferred from `inp`.
  /// See the [GraphQL Directive Location specification](https://spec.graphql.org/draft/#DirectiveLocation).
  pub fn graphql<'inp, Src, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
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
    location(inp)
  }
}
