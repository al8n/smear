//! SDL type-definition dispatch.

use super::*;

#[derive(Debug, Copy, Clone)]
enum TypeDefinitionHead {
  Scalar,
  Object,
  Interface,
  Union,
  Enum,
  InputObject,
}

#[derive(Debug, Copy, Clone)]
enum ClassifiedTypeDefinitionHead {
  Accepted(TypeDefinitionHead, SimpleSpan, SyntacticTokenKind),
  Rejected(Option<(SimpleSpan, SyntacticTokenKind)>),
}

impl ClassifiedTypeDefinitionHead {
  #[inline]
  const fn found(self) -> Option<(SimpleSpan, SyntacticTokenKind)> {
    match self {
      Self::Accepted(_, span, kind) => Some((span, kind)),
      Self::Rejected(found) => found,
    }
  }
}

/// Classifies the leading type-definition keyword exactly once without consuming it.
#[inline]
fn classify_type_definition_head<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ClassifiedTypeDefinitionHead, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  let mut peeked = inp.peek::<U1>()?;
  Ok(match peeked.pop_front() {
    Some(token) => {
      let span = *token.span();
      let kind = token.token().kind();
      let head = match token.token().downcast_ref() {
        Some(ContextualKeyword::Scalar) => Some(TypeDefinitionHead::Scalar),
        Some(ContextualKeyword::Type) => Some(TypeDefinitionHead::Object),
        Some(ContextualKeyword::Interface) => Some(TypeDefinitionHead::Interface),
        Some(ContextualKeyword::Union) => Some(TypeDefinitionHead::Union),
        Some(ContextualKeyword::Enum) => Some(TypeDefinitionHead::Enum),
        Some(ContextualKeyword::Input) => Some(TypeDefinitionHead::InputObject),
        _ => None,
      };
      match head {
        Some(head) => ClassifiedTypeDefinitionHead::Accepted(head, span, kind),
        None => ClassifiedTypeDefinitionHead::Rejected(Some((span, kind))),
      }
    }
    None => ClassifiedTypeDefinitionHead::Rejected(None),
  })
}

fn expected_classified_type_definition_head<'inp, Src, Ctx, T>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  found: Option<(SimpleSpan, SyntacticTokenKind)>,
) -> Result<T, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  const EXPECTED: Expectation = Expectation::Keyword("type definition");
  match found {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, EXPECTED, span).into()),
    None => {
      let offset = *inp.offset();
      Err(
        DialectGraphqlError::maybe_unexpected_token(
          None,
          EXPECTED,
          SimpleSpan::new(offset, offset),
        )
        .into(),
      )
    }
  }
}

/// Consumes an identifier head already accepted by the type-definition classifier.
///
/// This intentionally validates only the token shape: the contextual-keyword
/// spelling was already classified, and rechecking it here would make dispatch
/// do the same work twice.
fn take_classified_type_definition_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<SimpleSpan, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(_) => Ok(span),
        token => Err(
          DialectGraphqlError::unexpected_token(
            token.kind(),
            Expectation::Keyword("type definition"),
            span,
          )
          .into(),
        ),
      }
    }
    None => expected_classified_type_definition_head(inp, None),
  }
}

definition_parser!(
  /// Parses a GraphQL named type definition by deterministic contextual-keyword
  /// dispatch. The selected branch consumes the already classified identifier and
  /// then enters its committed tail.
  ///
  /// See the [GraphQL Type Definition specification](https://spec.graphql.org/draft/#TypeDefinition).
  pub type_definition,
  inp,
  TypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let classified = classify_type_definition_head(inp)?;
    let found = classified.found();
    let (head, start) = match classified {
      ClassifiedTypeDefinitionHead::Accepted(head, span, _) => (head, span.start()),
      ClassifiedTypeDefinitionHead::Rejected(_) => {
        return expected_classified_type_definition_head(inp, found);
      }
    };
    let branch: Branch<5> = match head {
      TypeDefinitionHead::Scalar => Branch::B0,
      TypeDefinitionHead::Object => Branch::B1,
      TypeDefinitionHead::Interface => Branch::B2,
      TypeDefinitionHead::Union => Branch::B3,
      TypeDefinitionHead::Enum => Branch::B4,
      TypeDefinitionHead::InputObject => Branch::B5,
    };
    let mut tails = (
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        super::scalar::scalar_after_keyword(inp, start).map(TypeDefinition::Scalar)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        super::object::object_after_keyword(inp, start).map(TypeDefinition::Object)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        super::interface::interface_after_keyword(inp, start).map(TypeDefinition::Interface)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        super::union::union_after_keyword(inp, start).map(TypeDefinition::Union)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        super::enum_type::enum_after_keyword(inp, start).map(TypeDefinition::Enum)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        super::input_object::input_object_after_keyword(inp, start).map(TypeDefinition::InputObject)
      },
    );
    tails.parse_choice(inp, &branch)
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
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let definition = type_definition(inp)?;
    Ok(Described::new(
      inp.span_since(&cursor),
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
