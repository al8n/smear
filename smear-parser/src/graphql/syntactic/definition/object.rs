//! SDL object type-definition parsing.

use super::*;

pub(super) fn object_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ObjectTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
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
  let implements = match try_implements(inp)? {
    ParseAttempt::Accept(implements) => Some(implements),
    ParseAttempt::Decline => None,
  };
  let directives = optional_const_directives(inp)?;
  let fields_definition = match try_fields_definition(inp)? {
    ParseAttempt::Accept(fields) => Some(fields),
    ParseAttempt::Decline => None,
  };
  Ok(ObjectTypeDefinition::new(
    SimpleSpan::new(start, extent_end(inp)),
    name,
    implements,
    directives,
    fields_definition,
  ))
}

definition_parser!(
  /// Parses an object type definition.
  ///
  /// See the [GraphQL Object Type Definition specification](https://spec.graphql.org/draft/#ObjectTypeDefinition).
  pub object_type_definition,
  inp,
  ObjectTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Type)?.start();
    object_after_keyword(inp, start)
  }
);

impl_definition_api!(
  /// Parses an object type definition.
  ///
  /// See the [GraphQL Object Type Definition specification](https://spec.graphql.org/draft/#ObjectTypeDefinition).
  S,
  ObjectTypeDefinition<S>,
  object_type_definition,
  [contextual]
);
