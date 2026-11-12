//! GraphQLx type-system definition, inline module and type-system document productions.
//!
//! # What bounds a module's nesting, and what bounds its body
//!
//! A module's body is the one recursive position here: it holds [`type_system_item`]s, and a
//! module is one. The body is entered only once its `{` has been peeked, so every nested module
//! consumes a `{` before its items are read — under a recording emitter the brace delimiter would
//! report a missing opener and read items anyway, with nothing counting the level. The lexer counts
//! every such `{` against the nesting ceiling. Each turn of the body's repetition runs one item,
//! whose head the item's own dispatch consumes, or refuses with an `Err`, so the body makes
//! progress on every turn it takes.

use super::*;
use crate::graphqlx::ast::{ModDefinition, Nested, TypeSystemItem};

/// Enters a type-system-definition tail after its leading keyword was consumed
/// and classified by fused dispatch.
pub(crate) fn type_system_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeSystemDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
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
    _ => expected_definition_phase(inp, Expectation::Production("a type-system definition")),
  }
}

fn described_after_string<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  description: StringValue<GraphqlxSlice<'inp, Src>>,
) -> Result<DescribedTypeSystemDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = description.span().start();
  type_system_definition(inp).map(|definition| {
    let end = definition.span().end();
    Described::new(SimpleSpan::new(start, end), Some(description), definition)
  })
}

/// A type-system item after its leading description: a module, whose description is its own, or a
/// type-system definition, which the description decorates.
fn described_item_after_string<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  description: StringValue<GraphqlxSlice<'inp, Src>>,
) -> Result<TypeSystemItem<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  // `peek_head_map`, not a raw `peek`: a scanner stop must not read as "no `mod` here" and send a
  // module down the definition path — smear issue #177.
  let module = inp.peek_head_map(|token| keyword_of(token.data) == Some(ContextualKeyword::Mod))?;
  if module == Some(true) {
    take_contextual_keyword(inp, ContextualKeyword::Mod)?;
    let start = description.span().start();
    return mod_definition_after_keyword(inp, start, Some(description)).map(TypeSystemItem::Mod);
  }
  described_after_string(inp, description).map(TypeSystemItem::Definition)
}

/// A module's body, `{ ModItem* }`, and the span of its braces.
///
/// **Precondition: the head is a `{` its caller has peeked** — see the module docs for why the body
/// may not be entered on anything else. An empty body is legal: a module with nothing in it is a
/// namespace.
fn mod_body<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  (SimpleSpan, Nested<TypeSystemItem<GraphqlxSlice<'inp, Src>>>),
  GraphqlxError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| type_system_item(inp))
    .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
    .delimited_by_braces()
    // `Nested`, as a use group collects into: the container is where a deep module's release
    // lives.
    .collect_with(Nested::empty())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| (span, data))
}

/// Parses `Name { ModItem* }` after a module's `mod` was consumed by a fused dispatcher.
///
/// `start` is where the module begins — its description's start when it has one — and the
/// description is the module's own.
pub(crate) fn mod_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
  description: Option<StringValue<GraphqlxSlice<'inp, Src>>>,
) -> Result<ModDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let name = take_name(inp)?;
  // The `{` is confirmed here, before the body is entered, and refused with the report the brace
  // delimiter would give: expected `{`, at the token found in its place.
  if !matches!(
    super::super::peek_kind(inp)?,
    Some(SyntacticTokenKind::LBrace)
  ) {
    return expected_definition_phase(inp, Expectation::LBrace);
  }
  let (body, items) = mod_body(inp)?;
  Ok(ModDefinition::new(
    SimpleSpan::new(start, body.end()),
    description,
    name,
    items,
  ))
}

definition_parser!(
  /// Parses a GraphQLx inline module, `Description? mod Name { ModItem* }`.
  ///
  /// A module's items are a type-system document's: `use` items, modules, definitions and
  /// extensions. This is a GraphQLx dialect production with no corresponding GraphQL draft
  /// production.
  pub mod_definition,
  inp,
  ModDefinition<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let description = description(inp)?;
    let keyword = take_contextual_keyword(inp, ContextualKeyword::Mod)?;
    let start = description
      .as_ref()
      .map_or_else(|| keyword.start(), |description| description.span().start());
    mod_definition_after_keyword(inp, start, description)
  }
);

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
                Expectation::Production("a type-system definition"),
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
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Production("a type-system definition")),
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
      .token_spanned()
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
                Expectation::Production("a type-system definition or an extension"),
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
        expected_definition_phase(inp, Expectation::Production("a type-system definition or an extension"))
      }
    }
  }
);

/// Parses one GraphQLx type-system document entry — a `use` item, an inline module, a type-system
/// definition or an extension — by fused head dispatch. Descriptions can only precede modules and
/// definitions, never `use` items or extensions.
///
/// The same entry is a module's item, so a module's body is a run of these. `use` items and
/// modules are GraphQLx extensions. See the
/// [GraphQL Type System Definition specification](https://spec.graphql.org/draft/#TypeSystemDefinition)
/// and [Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
pub fn type_system_item<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeSystemItem<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let inline_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitInlineStr(value) => {
        described_item_after_string(inp, StringValue::new(span, value.into()))
      }
      _ => unreachable!("fused GraphQLx type-system-document arm received a non-inline string"),
    };
  let block_string_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::LitBlockStr(value) => {
        described_item_after_string(inp, StringValue::new(span, value.into()))
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
        Some(ContextualKeyword::Use) => {
          super::super::use_tree::use_definition_after_keyword(inp, span.start())
            .map(TypeSystemItem::Use)
        }
        Some(ContextualKeyword::Mod) => {
          mod_definition_after_keyword(inp, span.start(), None).map(TypeSystemItem::Mod)
        }
        Some(ContextualKeyword::Extend) => {
          super::extension::type_system_extension_after_extend(inp, span.start())
            .map(TypeSystemItem::Extension)
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
          TypeSystemItem::Definition(Described::new(span, None, definition))
        }),
        _ => Err(
          DialectGraphqlxError::unexpected_token(
            kind,
            Expectation::Production(
              "a use item, a module, a type-system definition or an extension",
            ),
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
      Expectation::Production("a use item, a module, a type-system definition or an extension"),
    ),
  }
}

fn decide_document_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
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
  /// Parses a nonempty GraphQLx type-system document.
  ///
  /// `use` items, inline modules, generic headers, rooted paths, and `where`
  /// clauses are GraphQLx extensions.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  pub type_system_document,
  inp,
  TypeSystemDocument<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let Spanned { span, data: entries } = type_system_item
      .repeated_while::<_, U1>(decide_document_head::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(|entries: Vec<TypeSystemItem<GraphqlxSlice<'inp, Src>>>| entries)
      .token_spanned()
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
  /// Parses a nonempty GraphQLx type-system document.
  ///
  /// `use` items, inline modules, generic headers, rooted paths, and `where`
  /// clauses are GraphQLx extensions.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  TypeSystemDocument<S>,
  type_system_document,
  [contextual]
);
impl_definition_api!(
  /// Parses one GraphQLx type-system document entry: a `use` item, an inline module, a type-system
  /// definition or an extension.
  ///
  /// `use` items and inline modules are GraphQLx extensions.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  TypeSystemItem<S>,
  type_system_item,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx inline module, `Description? mod Name { ModItem* }`.
  ///
  /// Inline modules are a GraphQLx extension with no GraphQL counterpart.
  S,
  ModDefinition<S>,
  mod_definition,
  [contextual]
);
