//! SDL named type-definition dispatch.

use super::*;

/// Enters a named type-definition tail after its contextual keyword was already
/// consumed and classified by a fused dispatcher.
pub(super) fn type_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
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
    ContextualKeyword::Scalar => {
      super::scalar::scalar_after_keyword(inp, start).map(TypeDefinition::Scalar)
    }
    ContextualKeyword::Type => {
      super::object::object_after_keyword(inp, start).map(TypeDefinition::Object)
    }
    ContextualKeyword::Interface => {
      super::interface::interface_after_keyword(inp, start).map(TypeDefinition::Interface)
    }
    ContextualKeyword::Union => {
      super::union::union_after_keyword(inp, start).map(TypeDefinition::Union)
    }
    ContextualKeyword::Enum => {
      super::enum_type::enum_after_keyword(inp, start).map(TypeDefinition::Enum)
    }
    ContextualKeyword::Input => {
      super::input_object::input_object_after_keyword(inp, start).map(TypeDefinition::InputObject)
    }
    _ => expected_definition_phase(inp, Expectation::Keyword("type definition")),
  }
}

definition_parser!(
  /// Parses a GraphQL named type definition by fused contextual-keyword dispatch.
  ///
  /// The dispatcher consumes the identifier once, downcasts it once to a
  /// contextual keyword, and enters the selected committed tail without asking a
  /// subparser to inspect the same head again.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  pub type_definition,
  inp,
  TypeDefinition<GraphqlSlice<'inp, Src>>,
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
              keyword @ (ContextualKeyword::Scalar
              | ContextualKeyword::Type
              | ContextualKeyword::Interface
              | ContextualKeyword::Union
              | ContextualKeyword::Enum
              | ContextualKeyword::Input),
            ) => type_definition_after_keyword(inp, keyword, span.start()),
            _ => Err(
              DialectGraphqlError::unexpected_token(
                kind,
                Expectation::Keyword("type definition"),
                span,
              )
              .into(),
            ),
          },
          _ => unreachable!("fused type-definition arm received a non-identifier token"),
        }
      };

    match (identifier_head_arm,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(definition) => Ok(definition),
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("type definition")),
    }
  }
);

definition_parser!(
  /// Parses a type definition with its optional leading description.
  ///
  /// Once a string description is accepted, an invalid following head is a
  /// committed type-definition diagnostic.
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  pub described_type_definition,
  inp,
  Described<TypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let node_start = extent_start(inp)?;
    let description = description(inp)?;
    let definition = type_definition(inp)?;
    Ok(Described::new(
      extent_since(inp, node_start),
      description,
      definition,
    ))
  }
);

impl_definition_api!(
  /// Parses a named SDL type definition.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  S,
  TypeDefinition<S>,
  type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a named SDL type definition with its optional description.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  S,
  Described<TypeDefinition<S>, S>,
  described_type_definition,
  [contextual]
);
