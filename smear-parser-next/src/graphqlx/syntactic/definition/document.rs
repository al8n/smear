//! GraphQLx type-system definition and import-aware document productions.

use super::*;
use crate::graphqlx::ast::ImportOrTypeSystemDefinitionOrExtension;

/// Enters a type-system-definition tail after its leading keyword was consumed
/// and classified by fused dispatch.
pub(crate) fn type_system_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeSystemDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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

fn described_after_string<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  description: StringValue<GraphqlxSlice<'inp, Src>>,
) -> Result<DescribedTypeSystemDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let start = description.span().start();
  type_system_definition(inp).map(|definition| {
    let end = definition.span().end();
    Described::new(SimpleSpan::new(start, end), Some(description), definition)
  })
}

definition_parser!(
  /// Parses one GraphQLx type-system definition with fused head dispatch.
  ///
  /// GraphQLx extends named definition positions with generic headers,
  /// qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  pub type_system_definition,
  inp,
  TypeSystemDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let identifier_head =
      |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = keyword_of(&token);
        match token {
          GraphqlxToken::<'inp, Src>::Identifier(_) => match keyword {
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
            _ => Err(
              DialectGraphqlxError::unexpected_token(
                kind,
                Expectation::Keyword("type-system definition"),
                span,
              )
              .into(),
            ),
          },
          _ => unreachable!("fused GraphQLx type-system-definition arm received a non-identifier token"),
        }
      };
    match (identifier_head,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(definition) => Ok(definition),
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("type-system definition")),
    }
  }
);

definition_parser!(
  /// Parses a GraphQLx type-system definition with one optional description.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  pub described_type_system_definition,
  inp,
  DescribedTypeSystemDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    description
      .then(type_system_definition)
      .spanned()
      .parse_input(inp)
      .map(|Spanned {
        span,
        data: (description, definition),
      }| Described::new(span, description, definition))
  }
);

definition_parser!(
  /// Parses a GraphQLx type-system definition or extension with deterministic
  /// one-token head dispatch.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition)
  /// and [Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub type_system_definition_or_extension,
  inp,
  TypeSystemDefinitionOrExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let inline_string_head =
      |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
        GraphqlxToken::<'inp, Src>::LitInlineStr(value) => {
          described_after_string(inp, StringValue::new(span, value.into()))
            .map(TypeSystemDefinitionOrExtension::Definition)
        }
        _ => unreachable!("fused GraphQLx type-system-entry arm received a non-inline string"),
      };
    let block_string_head =
      |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
        GraphqlxToken::<'inp, Src>::LitBlockStr(value) => {
          described_after_string(inp, StringValue::new(span, value.into()))
            .map(TypeSystemDefinitionOrExtension::Definition)
        }
        _ => unreachable!("fused GraphQLx type-system-entry arm received a non-block string"),
      };
    let identifier_head =
      |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = keyword_of(&token);
        match token {
          GraphqlxToken::<'inp, Src>::Identifier(_) => match keyword {
            Some(ContextualKeyword::Extend) => super::extension::type_system_extension_after_extend(
              inp,
              span.start(),
            )
            .map(TypeSystemDefinitionOrExtension::Extension),
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
              let span = *definition.span();
              TypeSystemDefinitionOrExtension::Definition(Described::new(span, None, definition))
            }),
            _ => Err(
              DialectGraphqlxError::unexpected_token(
                kind,
                Expectation::Keyword("type-system definition or extension"),
                span,
              )
              .into(),
            ),
          },
          _ => unreachable!("fused GraphQLx type-system entry arm received a non-identifier token"),
        }
      };
    match (inline_string_head, block_string_head, identifier_head)
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

/// Parses one import-aware GraphQLx type-system document entry by fused head
/// dispatch. Descriptions can only precede definitions, never imports or
/// extensions.
///
/// Imports are a GraphQLx extension. See the
/// [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition)
/// and [Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
pub fn import_or_type_system_definition_or_extension<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  ImportOrTypeSystemDefinitionOrExtension<GraphqlxSlice<'inp, Src>>,
  GraphqlxError<'inp, Src, Ctx>,
>
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
  let inline_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInlineStr(value) => {
        described_after_string(inp, StringValue::new(span, value.into()))
          .map(ImportOrTypeSystemDefinitionOrExtension::Definition)
      }
      _ => unreachable!("fused GraphQLx type-system-document arm received a non-inline string"),
    };
  let block_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitBlockStr(value) => {
        described_after_string(inp, StringValue::new(span, value.into()))
          .map(ImportOrTypeSystemDefinitionOrExtension::Definition)
      }
      _ => unreachable!("fused GraphQLx type-system-document arm received a non-block string"),
    };
  let identifier_head = |Spanned { span, data: token }: Spanned<
    GraphqlxToken<'inp, Src>,
    SimpleSpan,
  >,
                         inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
    let kind = token.kind();
    let keyword = keyword_of(&token);
    match token {
      GraphqlxToken::<'inp, Src>::Identifier(_) => match keyword {
        Some(ContextualKeyword::Import) => {
          super::super::import::import_definition_after_keyword(inp, span.start())
            .map(ImportOrTypeSystemDefinitionOrExtension::Import)
        }
        Some(ContextualKeyword::Extend) => {
          super::extension::type_system_extension_after_extend(inp, span.start())
            .map(ImportOrTypeSystemDefinitionOrExtension::Extension)
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
          let span = *definition.span();
          ImportOrTypeSystemDefinitionOrExtension::Definition(Described::new(
            span, None, definition,
          ))
        }),
        _ => Err(
          DialectGraphqlxError::unexpected_token(
            kind,
            Expectation::Keyword("import or type-system definition or extension"),
            span,
          )
          .into(),
        ),
      },
      _ => unreachable!("fused GraphQLx type-system-document arm received a non-identifier token"),
    }
  };
  match (inline_string_head, block_string_head, identifier_head)
    .fused_dispatch_on_kind(&[
      SyntacticTokenKind::InlineString,
      SyntacticTokenKind::BlockString,
      SyntacticTokenKind::Identifier,
    ])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(entry) => Ok(entry),
    ParseAttempt::Decline => expected_definition_phase(
      inp,
      Expectation::Keyword("import or type-system definition or extension"),
    ),
  }
}

fn decide_document_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

definition_parser!(
  /// Parses a nonempty import-aware GraphQLx type-system document.
  ///
  /// Imports, generic headers, qualified paths, and `where` clauses are
  /// GraphQLx extensions.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  pub type_system_document,
  inp,
  TypeSystemDocument<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let Spanned { span, data: entries } = import_or_type_system_definition_or_extension
      .repeated_while::<_, U1>(decide_document_head::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(
        |entries: Vec<ImportOrTypeSystemDefinitionOrExtension<GraphqlxSlice<'inp, Src>>>| entries,
      )
      .spanned()
      .parse_input(inp)?;
    Ok(TypeSystemDocument::new(span, entries))
  }
);

impl_definition_api!(
  /// Parses one GraphQLx type-system definition.
  ///
  /// GraphQLx extends named definition positions with generic headers,
  /// qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  S,
  TypeSystemDefinition<S>,
  type_system_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx type-system definition with an optional description.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition).
  S,
  DescribedTypeSystemDefinition<S>,
  described_type_system_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx type-system definition or extension.
  ///
  /// See the [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition)
  /// and [Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  TypeSystemDefinitionOrExtension<S>,
  type_system_definition_or_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty import-aware GraphQLx type-system document.
  ///
  /// Imports, generic headers, qualified paths, and `where` clauses are
  /// GraphQLx extensions.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  TypeSystemDocument<S>,
  type_system_document,
  [contextual]
);
impl_definition_api!(
  /// Parses one import-aware GraphQLx type-system document entry.
  ///
  /// Imports are a GraphQLx extension.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  ImportOrTypeSystemDefinitionOrExtension<S>,
  import_or_type_system_definition_or_extension,
  [contextual]
);
