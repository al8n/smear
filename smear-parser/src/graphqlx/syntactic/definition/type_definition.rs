//! GraphQLx named type-definition dispatch.

use super::*;

/// Enters a named type-definition tail after the leading keyword was consumed
/// and classified by fused dispatch.
pub(super) fn type_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  /// Parses one GraphQLx named type definition with fused one-token dispatch.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses to
  /// the corresponding GraphQL type-definition forms.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  pub type_definition,
  inp,
  TypeDefinition<GraphqlxSlice<'inp, Src>>,
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
              keyword @ (ContextualKeyword::Scalar
              | ContextualKeyword::Type
              | ContextualKeyword::Interface
              | ContextualKeyword::Union
              | ContextualKeyword::Enum
              | ContextualKeyword::Input),
            ) => type_definition_after_keyword(inp, keyword, span.start()),
            _ => Err(
              DialectGraphqlxError::unexpected_token(
                kind,
                Expectation::Keyword("type definition"),
                span,
              )
              .into(),
            ),
          },
          _ => unreachable!("fused GraphQLx type-definition arm received a non-identifier token"),
        }
      };
    match (identifier_head,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(definition) => Ok(definition),
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("type definition")),
    }
  }
);

definition_parser!(
  /// Parses one GraphQLx named type definition with an optional description.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  pub described_type_definition,
  inp,
  Described<TypeDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxSlice<'inp, Src>>,
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
  /// Parses one GraphQLx named type definition.
  ///
  /// GraphQLx adds generic headers, qualified paths, and `where` clauses to
  /// the corresponding GraphQL type-definition forms.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  S,
  TypeDefinition<S>,
  type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses one GraphQLx named type definition with an optional description.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  S,
  Described<TypeDefinition<S>, S>,
  described_type_definition,
  [contextual]
);
