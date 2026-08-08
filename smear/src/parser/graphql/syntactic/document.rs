//! Full GraphQL document productions.
//!
//! This root layer combines executable definitions with SDL definitions and
//! extensions. Identifier heads use fused dispatch so their contextual keyword
//! is scanned and consumed once before entering a committed tail. Query
//! shorthand is the deliberate exception: it peeks `{` without consuming it,
//! then delegates to the native selection-set parser so Tokora owns the exact
//! delimiter recovery and `Unclosed<Brace>` behavior.

use std::vec::Vec;

use crate::lexer::{
  graphql::{ContextualKeyword, syntactic::SyntacticTokenKind},
  keywords::{Fragment, Mutation, Query, Subscription},
};
use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::{AsSpan, Spanned},
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  definition::{type_system_definition_after_keyword, type_system_extension_after_extend},
  executable::{fragment_definition_body, named_operation_after_classified_head},
  peeks_where,
  selection::selection_set,
};
use crate::parser::{
  combinator::{ParseCtx, TokenSpannedExt, extent_since, extent_start},
  graphql::{
    GraphQL,
    ast::{
      Definition, DefinitionOrExtension, Described, DescribedDefinition, Document,
      ExecutableDefinition, OperationDefinition, OperationType, StringValue,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

macro_rules! document_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

/// Reports an identifier head that a fused root dispatcher already consumed.
fn unexpected_consumed_head<'inp, Src, Ctx, T>(
  span: SimpleSpan,
  kind: SyntacticTokenKind,
  expected: Expectation,
) -> Result<T, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  Err(DialectGraphqlError::unexpected_token(kind, expected, span).into())
}

/// Emits a root-document expectation while leaving a rejected head available.
fn expected_document_phase<'inp, Src, Ctx, T>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<T, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  let rejected = {
    let mut peeked = inp.peek::<U1>()?;
    peeked
      .pop_front()
      .map(|token| (*token.span(), token.token().kind()))
  };

  match rejected {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, expected, span).into()),
    None => Err(
      DialectGraphqlError::maybe_unexpected_token(None, expected, SimpleSpan::new(offset, offset))
        .into(),
    ),
  }
}

/// Refuses the shorthand operation in a position a description has already committed to.
///
/// `OperationDefinition : SelectionSet` is the one definition alternative the grammar
/// gives no `Description?` slot, and both root dispatchers reach it through a `{`
/// predispatch that would otherwise swallow the token. The `{` is reported where it
/// stands and left unconsumed, so a caller resumes at the selection set rather than
/// inside it.
fn refuse_described_shorthand<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<(), GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  if peeks_where(inp, |token| token.is_l_brace())? {
    return expected_document_phase(inp, expected);
  }
  Ok(())
}

/// Enters a full-definition tail after a contextual keyword was fused and
/// consumed from the input.
fn definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  span: SimpleSpan,
  expected: Expectation,
) -> Result<Definition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
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
    ContextualKeyword::Query => named_operation_after_classified_head(
      inp,
      span.start(),
      OperationType::Query(Query::new(span)),
    )
    .map(ExecutableDefinition::Operation)
    .map(Definition::Executable),
    ContextualKeyword::Mutation => named_operation_after_classified_head(
      inp,
      span.start(),
      OperationType::Mutation(Mutation::new(span)),
    )
    .map(ExecutableDefinition::Operation)
    .map(Definition::Executable),
    ContextualKeyword::Subscription => named_operation_after_classified_head(
      inp,
      span.start(),
      OperationType::Subscription(Subscription::new(span)),
    )
    .map(ExecutableDefinition::Operation)
    .map(Definition::Executable),
    ContextualKeyword::Fragment => fragment_definition_body(inp, Fragment::new(span))
      .map(ExecutableDefinition::Fragment)
      .map(Definition::Executable),
    keyword @ (ContextualKeyword::Schema
    | ContextualKeyword::Directive
    | ContextualKeyword::Scalar
    | ContextualKeyword::Type
    | ContextualKeyword::Interface
    | ContextualKeyword::Union
    | ContextualKeyword::Enum
    | ContextualKeyword::Input) => {
      type_system_definition_after_keyword(inp, keyword, span.start()).map(Definition::TypeSystem)
    }
    _ => unexpected_consumed_head::<Src, Ctx, _>(span, SyntacticTokenKind::Identifier, expected),
  }
}

/// Builds a shorthand executable definition without consuming the `{` in a
/// root dispatcher.
///
/// The native selection-set parser retains ownership of the opener and closer,
/// including its typed delimiter recovery.
fn shorthand_definition<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Definition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  selection_set(inp)
    .map(OperationDefinition::Shorthand)
    .map(ExecutableDefinition::Operation)
    .map(Definition::Executable)
}

document_parser!(
  /// Parses one full GraphQL definition.
  ///
  /// The root dispatcher accepts an executable operation or fragment, or a
  /// type-system definition. Type-system extensions are excluded; use
  /// [`definition_or_extension`] for document entries.
  ///
  /// Identifier heads are fused and consumed exactly once. Query shorthand
  /// deliberately leaves `{` for the native selection-set parser so its
  /// delimiter diagnostics remain unchanged.
  ///
  /// See the [GraphQL Definition specification](https://spec.graphql.org/draft/#Definition).
  pub definition,
  inp,
  Definition<GraphqlSlice<'inp, Src>>,
  {
    if peeks_where(inp, |token| token.is_l_brace())? {
      return shorthand_definition(inp);
    }

    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
            Some(keyword) => {
              definition_after_keyword(inp, keyword, span, Expectation::Keyword("definition"))
            }
            None => unexpected_consumed_head::<Src, Ctx, _>(
              span,
              kind,
              Expectation::Keyword("definition"),
            ),
          },
          _ => unreachable!("fused full-definition arm received a non-identifier token"),
        }
      };
    match (identifier_head_arm,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(definition) => Ok(definition),
      ParseAttempt::Decline => expected_document_phase(inp, Expectation::Keyword("definition")),
    }
  }
);

document_parser!(
  /// Parses one full GraphQL definition with an optional leading description.
  ///
  /// A description commits the next production to a *keyworded* definition. It
  /// never reaches a type-system extension, and it never reaches the shorthand
  /// `OperationDefinition : SelectionSet`, which is the one definition
  /// alternative the grammar gives no `Description?` slot.
  ///
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description)
  /// and [Definition specification](https://spec.graphql.org/draft/#Definition).
  pub described_definition,
  inp,
  DescribedDefinition<GraphqlSlice<'inp, Src>>,
  {
    let node_start = extent_start(inp)?;
    let description = match StringValue::try_graphql(inp)? {
      ParseAttempt::Accept(value) => Some(value),
      ParseAttempt::Decline => None,
    };
    // `Keyword("definition")` is the expectation the described-`extend` rejection already
    // raises from this root: after a description, one of the twelve definition keywords.
    if description.is_some() {
      refuse_described_shorthand(inp, Expectation::Keyword("definition"))?;
    }
    let definition = definition(inp)?;
    Ok(Described::new(
      extent_since(inp, node_start),
      description,
      definition,
    ))
  }
);

document_parser!(
  /// Parses one full GraphQL definition or type-system extension.
  ///
  /// String heads are fused and commit their following head to a definition;
  /// `extend` is fused and enters the extension tail directly. A shorthand
  /// executable operation remains a non-consuming `{` predispatch so the
  /// native selection-set parser owns delimiter recovery.
  ///
  /// See the [GraphQL Definition specification](https://spec.graphql.org/draft/#Definition)
  /// and [Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub definition_or_extension,
  inp,
  DefinitionOrExtension<GraphqlSlice<'inp, Src>>,
  {
    if peeks_where(inp, |token| token.is_l_brace())? {
      let definition = shorthand_definition(inp)?;
      let span = *definition.as_span();
      return Ok(DefinitionOrExtension::Definition(Described::new(
        span,
        None,
        definition,
      )));
    }

    let inline_string_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitInlineStr(value) => {
          let description = StringValue::new(span, value.into());
          refuse_described_shorthand(inp, Expectation::Keyword("definition or extension"))?;
          let definition = definition(inp)?;
          let span = SimpleSpan::new(span.start(), definition.as_span().end());
          Ok(DefinitionOrExtension::Definition(Described::new(
            span,
            Some(description),
            definition,
          )))
        }
        _ => unreachable!("fused full-document arm received a non-inline-string token"),
      };
    let block_string_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
        GraphqlToken::<'inp, Src>::LitBlockStr(value) => {
          let description = StringValue::new(span, value.into());
          refuse_described_shorthand(inp, Expectation::Keyword("definition or extension"))?;
          let definition = definition(inp)?;
          let span = SimpleSpan::new(span.start(), definition.as_span().end());
          Ok(DefinitionOrExtension::Definition(Described::new(
            span,
            Some(description),
            definition,
          )))
        }
        _ => unreachable!("fused full-document arm received a non-block-string token"),
      };
    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
            Some(ContextualKeyword::Extend) => {
              type_system_extension_after_extend(inp, span.start())
                .map(DefinitionOrExtension::Extension)
            }
            Some(keyword) => definition_after_keyword(
              inp,
              keyword,
              span,
              Expectation::Keyword("definition or extension"),
            )
            .map(|definition| {
              let span = *definition.as_span();
              DefinitionOrExtension::Definition(Described::new(span, None, definition))
            }),
            None => unexpected_consumed_head::<Src, Ctx, _>(
              span,
              kind,
              Expectation::Keyword("definition or extension"),
            ),
          },
          _ => unreachable!("fused full-document arm received a non-identifier token"),
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
        expected_document_phase(inp, Expectation::Keyword("definition or extension"))
      }
    }
  }
);

/// Continues a full-document repetition through every token; its entry parser
/// owns committed head diagnostics, while end of input closes the document.
fn decide_definition_or_extension_head<'inp, Src, Ctx>(
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

document_parser!(
  /// Parses a nonempty full GraphQL document.
  ///
  /// A document may mix executable definitions, type-system definitions, and
  /// type-system extensions. Every keyworded definition may open with its own
  /// description; the shorthand operation and the extensions may not.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  pub document,
  inp,
  Document<GraphqlSlice<'inp, Src>>,
  {
    let Spanned { span, data: definitions }: Spanned<
      Vec<DefinitionOrExtension<GraphqlSlice<'inp, Src>>>,
      SimpleSpan,
    > = definition_or_extension
      .repeated_while::<_, U1>(decide_definition_or_extension_head::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(|definitions: Vec<DefinitionOrExtension<GraphqlSlice<'inp, Src>>>| definitions)
      .token_spanned()
      .parse_input(inp)?;
    Ok(Document::new(span, definitions))
  }
);

macro_rules! impl_document_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_document_api!(
  /// Parses one full GraphQL definition.
  ///
  /// See the [GraphQL Definition specification](https://spec.graphql.org/draft/#Definition).
  S,
  Definition<S>,
  definition
);

impl_document_api!(
  /// Parses one full GraphQL definition with an optional leading description.
  ///
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description).
  S,
  DescribedDefinition<S>,
  described_definition
);

impl_document_api!(
  /// Parses one full GraphQL definition or type-system extension.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  DefinitionOrExtension<S>,
  definition_or_extension
);

impl_document_api!(
  /// Parses a nonempty full GraphQL document.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  Document<S>,
  document
);

#[cfg(test)]
mod tests;
