//! GraphQL SDL type-system definition and document productions.

use super::*;

/// Enters a type-system-definition tail after its leading keyword was consumed
/// and classified by fused dispatch.
pub(crate) fn type_system_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeSystemDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match keyword {
    ContextualKeyword::Schema => {
      super::schema::schema_after_keyword(inp, start).map(TypeSystemDefinition::Schema)
    }
    ContextualKeyword::Directive => {
      super::directive::directive_after_keyword(inp, start).map(TypeSystemDefinition::Directive)
    }
    keyword @ (ContextualKeyword::Scalar
    | ContextualKeyword::Type
    | ContextualKeyword::Interface
    | ContextualKeyword::Union
    | ContextualKeyword::Enum
    | ContextualKeyword::Input) => {
      super::type_definition::type_definition_after_keyword(inp, keyword, start)
        .map(TypeSystemDefinition::Type)
    }
    _ => expected_definition_phase(inp, Expectation::Keyword("type-system definition")),
  }
}

/// Reports a type-system-definition head that a fused arm already consumed.
fn unexpected_consumed_definition_head<'inp, Src, Ctx, T>(
  span: SimpleSpan,
  kind: SyntacticTokenKind,
) -> Result<T, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  Err(
    DialectGraphqlError::unexpected_token(
      kind,
      Expectation::Keyword("type-system definition"),
      span,
    )
    .into(),
  )
}

definition_parser!(
  /// Parses one GraphQL type-system definition by fused single-consumption dispatch.
  ///
  /// This production does not consume a description itself; use
  /// [`described_type_system_definition`] or
  /// [`type_system_definition_or_extension`] for an optional leading description.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  pub type_system_definition,
  inp,
  TypeSystemDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
            Some(
              keyword @ (ContextualKeyword::Schema
              | ContextualKeyword::Directive
              | ContextualKeyword::Scalar
              | ContextualKeyword::Type
              | ContextualKeyword::Interface
              | ContextualKeyword::Union
              | ContextualKeyword::Enum
              | ContextualKeyword::Input),
            ) => type_system_definition_after_keyword(inp, keyword, span.start()),
            _ => unexpected_consumed_definition_head::<Src, Ctx, _>(span, kind),
          },
          _ => unreachable!("fused type-system-definition arm received a non-identifier token"),
        }
      };
    match (identifier_head_arm,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(definition) => Ok(definition),
      ParseAttempt::Decline => {
        expected_definition_phase(inp, Expectation::Keyword("type-system definition"))
      }
    }
  }
);

definition_parser!(
  /// Parses a type-system definition with one optional leading description.
  ///
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description)
  /// and [Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  pub described_type_system_definition,
  inp,
  Described<TypeSystemDefinition<GraphqlSlice<'inp, Src>>, GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let node_start = extent_start(inp)?;
    let description = description(inp)?;
    let definition = type_system_definition(inp)?;
    Ok(Described::new(
      extent_since(inp, node_start),
      description,
      definition,
    ))
  }
);

fn described_definition_after_string<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  description: StringValue<GraphqlSlice<'inp, Src>>,
) -> Result<DescribedTypeSystemDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let description_start = description.span().start();
  let mut description = Some(description);
  let identifier_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let kind = token.kind();
      let keyword = token.downcast_ref();
      let description = description
        .take()
        .expect("fused description arm executes at most once");
      match token {
        GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
          Some(
            keyword @ (ContextualKeyword::Schema
            | ContextualKeyword::Directive
            | ContextualKeyword::Scalar
            | ContextualKeyword::Type
            | ContextualKeyword::Interface
            | ContextualKeyword::Union
            | ContextualKeyword::Enum
            | ContextualKeyword::Input),
          ) => type_system_definition_after_keyword(inp, keyword, span.start()).map(|definition| {
            let end = definition.as_span().end();
            Described::new(
              SimpleSpan::new(description_start, end),
              Some(description),
              definition,
            )
          }),
          _ => unexpected_consumed_definition_head::<Src, Ctx, _>(span, kind),
        },
        _ => unreachable!("fused described-definition arm received a non-identifier token"),
      }
    };
  match (identifier_head_arm,)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(definition) => Ok(definition),
    ParseAttempt::Decline => {
      expected_definition_phase(inp, Expectation::Keyword("type-system definition"))
    }
  }
}

definition_parser!(
  /// Parses a type-system definition or extension.
  ///
  /// The first token is fused-dispatched once. String heads create a description
  /// and commit the next head to a definition; `extend` commits the next head to
  /// an extension; ordinary definition keywords enter their tails directly.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition)
  /// and [Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub type_system_definition_or_extension,
  inp,
  TypeSystemDefinitionOrExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let inline_string_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitInlineStr(value) => {
          described_definition_after_string(inp, StringValue::new(span, value.into()))
            .map(TypeSystemDefinitionOrExtension::Definition)
        }
        _ => unreachable!("fused type-system entry arm received a non-inline-string token"),
      };
    let block_string_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitBlockStr(value) => {
          described_definition_after_string(inp, StringValue::new(span, value.into()))
            .map(TypeSystemDefinitionOrExtension::Definition)
        }
        _ => unreachable!("fused type-system entry arm received a non-block-string token"),
      };
    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
            Some(ContextualKeyword::Extend) => {
              super::extension::type_system_extension_after_extend(inp, span.start())
                .map(TypeSystemDefinitionOrExtension::Extension)
            }
            Some(
              keyword @ (ContextualKeyword::Schema
              | ContextualKeyword::Directive
              | ContextualKeyword::Scalar
              | ContextualKeyword::Type
              | ContextualKeyword::Interface
              | ContextualKeyword::Union
              | ContextualKeyword::Enum
              | ContextualKeyword::Input),
            ) => type_system_definition_after_keyword(inp, keyword, span.start()).map(|definition| {
              let span = *definition.as_span();
              TypeSystemDefinitionOrExtension::Definition(Described::new(span, None, definition))
            }),
            _ => Err(
              DialectGraphqlError::unexpected_token(
                kind,
                Expectation::Keyword("type-system definition or extension"),
                span,
              )
              .into(),
            ),
          },
          _ => unreachable!("fused type-system entry arm received a non-identifier token"),
        }
      };

    match (inline_string_head_arm, block_string_head_arm, identifier_head_arm)
      .fused_dispatch_on_kind(&[
        SyntacticTokenKind::InlineString,
        SyntacticTokenKind::BlockString,
        SyntacticTokenKind::Identifier,
      ])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(entry) => Ok(entry),
      ParseAttempt::Decline => {
        expected_definition_phase(inp, Expectation::Keyword("type-system definition or extension"))
      }
    }
  }
);

fn decide_type_system_definition_or_extension_head<'inp, Src, Ctx>(
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
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

definition_parser!(
  /// Parses a nonempty GraphQL type-system document.
  ///
  /// See the [GraphQL Type System Document specification](https://spec.graphql.org/draft/#TypeSystemDocument).
  pub type_system_document,
  inp,
  TypeSystemDocument<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let Spanned { span, data: definitions }: Spanned<
      Vec<TypeSystemDefinitionOrExtension<GraphqlSlice<'inp, Src>>>,
      SimpleSpan,
    > = type_system_definition_or_extension
      .repeated_while::<_, U1>(decide_type_system_definition_or_extension_head::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(
        |definitions: Vec<TypeSystemDefinitionOrExtension<GraphqlSlice<'inp, Src>>>| definitions,
      )
      .token_spanned()
      .parse_input(inp)?;
    Ok(TypeSystemDocument::new(span, definitions))
  }
);

impl_definition_api!(
  /// Parses one GraphQL type-system definition.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  S,
  TypeSystemDefinition<S>,
  type_system_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a type-system definition with one optional leading description.
  ///
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description).
  S,
  Described<TypeSystemDefinition<S>, S>,
  described_type_system_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a type-system definition or extension.
  ///
  /// See the [GraphQL Type System Document specification](https://spec.graphql.org/draft/#TypeSystemDocument).
  S,
  TypeSystemDefinitionOrExtension<S>,
  type_system_definition_or_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty GraphQL type-system document.
  ///
  /// See the [GraphQL Type System Document specification](https://spec.graphql.org/draft/#TypeSystemDocument).
  S,
  TypeSystemDocument<S>,
  type_system_document,
  [contextual]
);
