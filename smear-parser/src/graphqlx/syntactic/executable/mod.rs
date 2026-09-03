//! GraphQLx executable-definition productions.
//!
//! Executable heads are classified with one lookahead token, then a
//! deterministic [`ParseChoice`] consumes the selected keyword exactly once.
//! Operation and fragment tails add GraphQLx generic headers, paths, and
//! optional `where` clauses without using transactional parsing.

use std::vec::Vec;

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};
use tokora::{
  Accumulator, Branch, EmitterView, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source,
  Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  punct::{Colon, Dollar},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken,
  directive::{const_directives, directives},
  generic::{
    executable_definition_name, try_definition_name, try_executable_definition_type_generics,
    try_where_clause,
  },
  import::import_definition_after_keyword,
  keyword_of,
  selection::{selection_set, type_condition},
  ty::ty,
  unexpected_here,
  value::default_value,
};
use crate::{
  combinator::{
    ParseCtx, TokenSpannedExt, extent_end, extent_since, extent_start, try_colon, try_dollar,
  },
  graphqlx::{
    GraphQLx,
    ast::{
      Constrained, DescribedExecutableDefinition, DescribedVariableDefinition,
      ExecutableDefinition, ExecutableDefinitionHeader, ExecutableDocument, FragmentDefinition,
      ImportOrExecutableDefinition, NamedOperationDefinition, OperationDefinition, OperationType,
      SelectionSet, StringValue, VariableDefinition, VariableValue, VariablesDefinition,
      WhereClause,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! executable_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [$($bounds:tt)*], $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      $($bounds)*
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum OperationHead {
  Query,
  Mutation,
  Subscription,
}

/// A pre-classified executable-definition head for fused document dispatch.
#[derive(Debug, Copy, Clone)]
pub(crate) enum ExecutableDefinitionHead {
  Shorthand,
  Operation(OperationHead),
  Fragment,
}

/// A pre-classified top-level executable-document entry.
#[derive(Debug, Copy, Clone)]
enum ImportOrExecutableHead {
  Import,
  Executable(ExecutableDefinitionHead),
}

/// Classifies the first executable-definition token without consuming it.
#[inline]
pub(crate) fn classify_executable_head<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<ExecutableDefinitionHead>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  // `peek_head_map`, not a raw `peek`: this classifier's `None` is its word for "the document ended
  // here", and a window the scanner truncated is byte-for-byte one a short document produces —
  // smear issue #177, Codex round 1.
  Ok(
    inp
      .peek_head_map(|token| match token.data {
        GraphqlxToken::<'inp, Src>::LBrace => Some(ExecutableDefinitionHead::Shorthand),
        token => match keyword_of(token) {
          Some(ContextualKeyword::Query) => {
            Some(ExecutableDefinitionHead::Operation(OperationHead::Query))
          }
          Some(ContextualKeyword::Mutation) => {
            Some(ExecutableDefinitionHead::Operation(OperationHead::Mutation))
          }
          Some(ContextualKeyword::Subscription) => Some(ExecutableDefinitionHead::Operation(
            OperationHead::Subscription,
          )),
          Some(ContextualKeyword::Fragment) => Some(ExecutableDefinitionHead::Fragment),
          _ => None,
        },
      })?
      .flatten(),
  )
}

/// Classifies an import-aware executable-document entry without consuming it.
///
/// This runs after the caller has tried to consume an optional description, so
/// imports and every ordinary executable head can enter their already-
/// classified tails directly.
#[inline]
fn classify_import_or_executable_head<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<ImportOrExecutableHead>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  // `peek_head_map`, not a raw `peek`: this classifier's `None` is its word for "the document ended
  // here", and a window the scanner truncated is byte-for-byte one a short document produces —
  // smear issue #177, Codex round 1.
  Ok(
    inp
      .peek_head_map(|token| match token.data {
        GraphqlxToken::<'inp, Src>::LBrace => Some(ImportOrExecutableHead::Executable(
          ExecutableDefinitionHead::Shorthand,
        )),
        token => match keyword_of(token) {
          Some(ContextualKeyword::Import) => Some(ImportOrExecutableHead::Import),
          Some(ContextualKeyword::Query) => Some(ImportOrExecutableHead::Executable(
            ExecutableDefinitionHead::Operation(OperationHead::Query),
          )),
          Some(ContextualKeyword::Mutation) => Some(ImportOrExecutableHead::Executable(
            ExecutableDefinitionHead::Operation(OperationHead::Mutation),
          )),
          Some(ContextualKeyword::Subscription) => Some(ImportOrExecutableHead::Executable(
            ExecutableDefinitionHead::Operation(OperationHead::Subscription),
          )),
          Some(ContextualKeyword::Fragment) => Some(ImportOrExecutableHead::Executable(
            ExecutableDefinitionHead::Fragment,
          )),
          _ => None,
        },
      })?
      .flatten(),
  )
}

/// Refuses the shorthand operation in a position a description has already committed to.
///
/// `OperationDefinition : Description? OperationType … | SelectionSet` — the second
/// alternative has no `Description?` slot, so a described definition must open with a
/// keyword. The `{` is reported where it stands and left unconsumed, so a caller resumes
/// at the selection set rather than inside it.
#[inline]
pub(crate) fn refuse_described_shorthand<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<(), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  if super::peek_kind(inp)? == Some(SyntacticTokenKind::LBrace) {
    return unexpected_here(inp, expected);
  }
  Ok(())
}

#[inline]
fn operation_type_from_head(head: OperationHead, span: SimpleSpan) -> OperationType {
  match head {
    OperationHead::Query => OperationType::Query(span),
    OperationHead::Mutation => OperationType::Mutation(span),
    OperationHead::Subscription => OperationType::Subscription(span),
  }
}

/// Consumes an identifier after a deterministic classifier selected it.
#[inline]
fn take_classified_identifier<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<(SimpleSpan, GraphqlxSlice<'inp, Src>), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next_or_stop()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::Identifier(source),
    }) => Ok((span, source)),
    Some(Spanned { span, data: token }) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
    }
    None => unexpected_here(inp, expected),
  }
}

fn take_name<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<super::ast::Name<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match super::try_name(inp)? {
    ParseAttempt::Accept(name) => Ok(name),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Name),
  }
}

/// Parses a committed `$` with the local variable-definition diagnostic.
fn take_dollar<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Dollar<SimpleSpan, (), GraphQLx>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_dollar(inp)? {
    ParseAttempt::Accept(dollar) => Ok(dollar),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Dollar),
  }
}

/// Parses a committed `:` with the local variable-definition diagnostic.
fn take_colon<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Colon<SimpleSpan, (), GraphQLx>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_colon(inp)? {
    ParseAttempt::Accept(colon) => Ok(colon),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Colon),
  }
}

fn try_description<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<StringValue<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(
    match inp.try_expect_map_or_stop(|token| {
      matches!(
        token.data(),
        GraphqlxToken::<'inp, Src>::LitInlineStr(_) | GraphqlxToken::<'inp, Src>::LitBlockStr(_)
      )
      .then_some(())
    })? {
      Some((
        (),
        Spanned {
          span,
          data: GraphqlxToken::<'inp, Src>::LitInlineStr(value),
        },
      )) => ParseAttempt::Accept(StringValue::new(span, value.into())),
      Some((
        (),
        Spanned {
          span,
          data: GraphqlxToken::<'inp, Src>::LitBlockStr(value),
        },
      )) => ParseAttempt::Accept(StringValue::new(span, value.into())),
      Some(_) => unreachable!("description probe consumed a non-string token"),
      None => ParseAttempt::Decline,
    },
  )
}

fn variable_definition_core<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<VariableDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  take_dollar
    .ignore_then(take_name)
    .token_spanned()
    .map(|Spanned { span, data }| VariableValue::new(span, data))
    .then_ignore(take_colon)
    .then(ty)
    .then(default_value)
    .then(const_directives)
    .token_spanned()
    .map(
      |Spanned {
         span,
         data: (((variable, ty), default), directives),
       }| {
        let directives = (!directives.directives().is_empty()).then_some(directives);
        VariableDefinition::new(span, variable, ty, default, directives)
      },
    )
    .parse_input(inp)
}

executable_parser!(
  /// Parses a GraphQLx variable definition with an optional description.
  ///
  /// GraphQLx permits a leading description and its extended type, value, and
  /// directive forms while retaining GraphQL's committed variable phases.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  pub variable_definition,
  inp,
  DescribedVariableDefinition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let node_start = extent_start(inp)?;
    let description = match try_description(inp)? {
      ParseAttempt::Accept(description) => Some(description),
      ParseAttempt::Decline => None,
    };
    let definition = variable_definition_core(inp)?;
    Ok(DescribedVariableDefinition::new(
      extent_since(inp, node_start),
      description,
      definition,
    ))
  }
);

fn decide_variable_definition_tail<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if token.token().is_r_paren() => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

/// Continues an optional variables-definition attempt only when its `(` opener
/// is present, leaving every other token for the caller.
fn decide_variables_definition_opener<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if token.token().is_l_paren() => Action::Continue,
    _ => Action::Stop,
  })
}

executable_parser!(
  /// Parses optional GraphQLx variable definitions.
  ///
  /// An absent `(` yields an empty zero-width collection. Once the opener is
  /// present, the collection requires at least one complete definition.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  pub variables_definition,
  inp,
  VariablesDefinition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::combinator::extent`.
    let start = extent_end(inp);
    let parsed = (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| variable_definition(inp))
      .repeated_while::<_, U1>(decide_variable_definition_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .token_spanned()
      .peek_then_try::<_, U1>(decide_variables_definition_opener::<Src, Ctx>)
      .try_parse_input(inp)?;
    Ok(match parsed {
      ParseAttempt::Accept(Spanned { span, data }) => VariablesDefinition::new(span, data),
      ParseAttempt::Decline => VariablesDefinition::new(SimpleSpan::new(start, start), Vec::new()),
    })
  }
);

fn constrained_selection_set<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  Constrained<SelectionSet<GraphqlxSlice<'inp, Src>>, WhereClause<GraphqlxSlice<'inp, Src>>>,
  GraphqlxError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let where_clause: Option<WhereClause<GraphqlxSlice<'inp, Src>>> = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => Some(where_clause),
    ParseAttempt::Decline => None,
  };
  let selection_set = selection_set(inp)?;
  let start = where_clause.as_ref().map_or_else(
    || selection_set.span().start(),
    |where_clause| where_clause.span().start(),
  );
  Ok(Constrained::new(
    SimpleSpan::new(start, selection_set.span().end()),
    where_clause,
    selection_set,
  ))
}

/// Parses the committed tail of a named operation after its keyword is consumed.
pub(crate) fn named_operation_after_head<'inp, Src, Ctx>(
  start: usize,
  operation_type: OperationType,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<OperationDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let name = try_definition_name(inp).map(Into::into)?;
  let variables = variables_definition(inp)?;
  let variables = (!variables.variable_definitions().is_empty()).then_some(variables);
  let directives = directives(inp)?;
  let directives = (!directives.directives().is_empty()).then_some(directives);
  let selection_set = constrained_selection_set(inp)?;
  Ok(OperationDefinition::Named(NamedOperationDefinition::new(
    SimpleSpan::new(start, selection_set.span().end()),
    operation_type,
    name,
    variables,
    directives,
    selection_set,
  )))
}

executable_parser!(
  /// Parses a GraphQLx operation keyword.
  ///
  /// One-token dispatch recognizes `query`, `mutation`, or `subscription`.
  ///
  /// See the [GraphQL Operation Type specification](https://spec.graphql.org/draft/#OperationType).
  pub operation_type,
  inp,
  OperationType,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let branch: Branch<2> = match classify_executable_head(inp)? {
      Some(ExecutableDefinitionHead::Operation(OperationHead::Query)) => Branch::B0,
      Some(ExecutableDefinitionHead::Operation(OperationHead::Mutation)) => Branch::B1,
      Some(ExecutableDefinitionHead::Operation(OperationHead::Subscription)) => Branch::B2,
      _ => return unexpected_here(inp, Expectation::Keyword("query, mutation, or subscription")),
    };

    let mut tails = (
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::Keyword("query"))?;
        Ok(OperationType::Query(span))
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::Keyword("mutation"))?;
        Ok(OperationType::Mutation(span))
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::Keyword("subscription"))?;
        Ok(OperationType::Subscription(span))
      },
    );
    tails.parse_choice(inp, &branch)
  }
);

fn fragment_definition_after_keyword<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<FragmentDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let implementation_generics = try_executable_definition_type_generics(inp)?;
  let name = executable_definition_name(inp)?;
  let header = ExecutableDefinitionHeader::new(
    SimpleSpan::new(start, name.span().end()),
    implementation_generics,
    name,
  );
  let condition = type_condition(inp)?;
  let directives = directives(inp)?;
  let directives = (!directives.directives().is_empty()).then_some(directives);
  let selection_set = constrained_selection_set(inp)?;
  Ok(FragmentDefinition::new(
    SimpleSpan::new(start, selection_set.span().end()),
    header,
    condition,
    directives,
    selection_set,
  ))
}

/// Builds an executable definition after a fused caller classified its head.
///
/// The selected head remains in the input for this helper to consume. This lets
/// a document-level dispatcher share its first-token classification with the
/// executable tail without reclassifying the token. Shorthand selection sets
/// keep their `{` opener for the selection-set parser.
pub(crate) fn executable_definition_after_head<'inp, Src, Ctx>(
  head: ExecutableDefinitionHead,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ExecutableDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match head {
    ExecutableDefinitionHead::Shorthand => selection_set(inp)
      .map(OperationDefinition::Shorthand)
      .map(ExecutableDefinition::Operation),
    ExecutableDefinitionHead::Operation(head) => {
      let (span, _) = take_classified_identifier(inp, Expectation::Keyword("operation type"))?;
      named_operation_after_head(span.start(), operation_type_from_head(head, span), inp)
        .map(ExecutableDefinition::Operation)
    }
    ExecutableDefinitionHead::Fragment => {
      let (span, _) = take_classified_identifier(inp, Expectation::Keyword("fragment"))?;
      fragment_definition_after_keyword(span.start(), inp).map(ExecutableDefinition::Fragment)
    }
  }
}

executable_parser!(
  /// Parses a GraphQLx operation definition.
  ///
  /// Shorthand operations follow GraphQL. Named operations may use GraphQLx
  /// generic definition names and an optional `where` constraint before their
  /// selection set.
  ///
  /// See the [GraphQL Operation Definition specification](https://spec.graphql.org/draft/#OperationDefinition).
  pub operation_definition,
  inp,
  OperationDefinition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let mut named_head = None;
    let branch: Branch<1> = match classify_executable_head(inp)? {
      Some(ExecutableDefinitionHead::Shorthand) => Branch::B0,
      Some(ExecutableDefinitionHead::Operation(head)) => {
        named_head = Some(head);
        Branch::B1
      }
      _ => return unexpected_here(inp, Expectation::Keyword("operation definition")),
    };

    let mut tails = (
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        selection_set(inp).map(OperationDefinition::Shorthand)
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::Keyword("operation type"))?;
        let head = named_head.expect("named operation branch retains its classified head");
        named_operation_after_head(span.start(), operation_type_from_head(head, span), inp)
      },
    );
    tails.parse_choice(inp, &branch)
  }
);

executable_parser!(
  /// Parses a committed GraphQLx fragment definition.
  ///
  /// GraphQLx adds implementation generics, qualified or generic type paths,
  /// and optional `where` constraints around the GraphQL fragment structure.
  ///
  /// See the [GraphQL Fragment Definition specification](https://spec.graphql.org/draft/#FragmentDefinition).
  pub fragment_definition,
  inp,
  FragmentDefinition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    match classify_executable_head(inp)? {
      Some(ExecutableDefinitionHead::Fragment) => {
        let (span, _) = take_classified_identifier(inp, Expectation::Keyword("fragment"))?;
        fragment_definition_after_keyword(span.start(), inp)
      }
      _ => unexpected_here(inp, Expectation::Keyword("fragment")),
    }
  }
);

/// Parses one executable definition without a leading description.
///
/// This is deliberately shared by the public inner-node API and the described
/// wrapper. The classifier picks one branch before that branch consumes its
/// head, so `query`, `mutation`, `subscription`, and `fragment` never restart
/// an atom parser after dispatch.
fn executable_definition_core<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ExecutableDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let mut named_head = None;
  let branch: Branch<2> = match classify_executable_head(inp)? {
    Some(ExecutableDefinitionHead::Shorthand) => Branch::B0,
    Some(ExecutableDefinitionHead::Operation(head)) => {
      named_head = Some(head);
      Branch::B1
    }
    Some(ExecutableDefinitionHead::Fragment) => Branch::B2,
    None => return unexpected_here(inp, Expectation::Keyword("executable definition")),
  };

  let mut tails = (
    |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      executable_definition_after_head(ExecutableDefinitionHead::Shorthand, inp)
    },
    |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      let head = named_head.expect("named executable branch retains its classified head");
      executable_definition_after_head(ExecutableDefinitionHead::Operation(head), inp)
    },
    |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      executable_definition_after_head(ExecutableDefinitionHead::Fragment, inp)
    },
  );
  tails.parse_choice(inp, &branch)
}

executable_parser!(
  /// Parses a GraphQLx executable definition with an optional description.
  ///
  /// A description is accepted only as the leading atom; after it is consumed,
  /// the definition head is committed to a deterministic executable branch.
  /// GraphQLx tracks the GraphQL executable grammar here: the keyworded
  /// alternatives carry a `Description?` and the shorthand
  /// `OperationDefinition : SelectionSet` does not.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  pub executable_definition,
  inp,
  DescribedExecutableDefinition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let node_start = extent_start(inp)?;
    let description = match try_description(inp)? {
      ParseAttempt::Accept(description) => Some(description),
      ParseAttempt::Decline => None,
    };
    if description.is_some() {
      refuse_described_shorthand(inp, Expectation::OperationTypeOrFragment)?;
    }
    let definition = executable_definition_core(inp)?;
    Ok(DescribedExecutableDefinition::new(
      extent_since(inp, node_start),
      description,
      definition,
    ))
  }
);

executable_parser!(
  /// Parses an executable definition or a GraphQLx import with one head probe.
  ///
  /// `import` is a GraphQLx extension; the executable alternative follows the
  /// GraphQL executable-definition grammar.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  pub import_or_executable_definition,
  inp,
  ImportOrExecutableDefinition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    match try_description(inp)? {
      ParseAttempt::Accept(description) => {
        refuse_described_shorthand(inp, Expectation::OperationTypeOrFragment)?;
        let definition = executable_definition_core(inp)?;
        Ok(ImportOrExecutableDefinition::Definition(
          DescribedExecutableDefinition::new(
            SimpleSpan::new(description.span().start(), definition.span().end()),
            Some(description),
            definition,
          ),
        ))
      }
      ParseAttempt::Decline => match classify_import_or_executable_head(inp)? {
        Some(ImportOrExecutableHead::Import) => {
          let (span, _) = take_classified_identifier(inp, Expectation::Import)?;
          import_definition_after_keyword(inp, span.start()).map(ImportOrExecutableDefinition::Import)
        }
        Some(ImportOrExecutableHead::Executable(head)) => {
          let definition = executable_definition_after_head(head, inp)?;
          let span = *definition.span();
          Ok(ImportOrExecutableDefinition::Definition(
            DescribedExecutableDefinition::new(span, None, definition),
          ))
        }
        None => unexpected_here(inp, Expectation::Keyword("import or executable definition")),
      }
    }
  }
);

fn decide_executable_document_tail<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(if peeked.pop_front().is_some() {
    Action::Continue
  } else {
    Action::Stop
  })
}

executable_parser!(
  /// Parses a nonempty GraphQLx executable document.
  ///
  /// Each entry may be an import or a described executable definition. The
  /// document has no enclosing delimiter, so every remaining token continues
  /// into the committed entry parser and receives that parser's local error.
  /// `import` is the GraphQLx extension; a leading definition description is
  /// ordinary GraphQL, and never decorates an import or a shorthand operation.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  pub executable_document,
  inp,
  ExecutableDocument<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let Spanned { span, data: definitions }: Spanned<
      Vec<ImportOrExecutableDefinition<GraphqlxSlice<'inp, Src>>>,
      SimpleSpan,
    > = import_or_executable_definition
      .repeated_while::<_, U1>(decide_executable_document_tail::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(
        |definitions: Vec<ImportOrExecutableDefinition<GraphqlxSlice<'inp, Src>>>| definitions,
      )
      .token_spanned()
      .parse_input(inp)?;
    Ok(ExecutableDocument::new(span, definitions))
  }
);

macro_rules! impl_executable_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
    impl<$slice> $node {
      $(#[$meta])*
      /// The lexer source is inferred from `inp`.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
        GraphqlxLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlxToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        $($bounds)*
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_executable_api!(
  /// Parses a GraphQLx variable definition without its optional description.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  VariableDefinition<S>,
  variable_definition_core,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses a GraphQLx variable definition with its optional description.
  ///
  /// `VariableDefinition : Description? Variable : Type DefaultValue?
  /// Directives[Const]?`.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  DescribedVariableDefinition<S>,
  variable_definition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses optional GraphQLx variable definitions.
  ///
  /// An absent `(` yields an empty zero-width collection.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  VariablesDefinition<S>,
  variables_definition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses a GraphQLx operation definition.
  ///
  /// Named operations may use GraphQLx generics and `where` constraints.
  ///
  /// See the [GraphQL Operation Definition specification](https://spec.graphql.org/draft/#OperationDefinition).
  S,
  OperationDefinition<S>,
  operation_definition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses a GraphQLx fragment definition.
  ///
  /// GraphQLx extends fragment headers and type conditions with its generic
  /// and qualified type-path syntax.
  ///
  /// See the [GraphQL Fragment Definition specification](https://spec.graphql.org/draft/#FragmentDefinition).
  S,
  FragmentDefinition<S>,
  fragment_definition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses one GraphQLx executable definition without an optional description.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  S,
  ExecutableDefinition<S>,
  executable_definition_core,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses one GraphQLx executable definition with an optional description.
  ///
  /// The shorthand `OperationDefinition : SelectionSet` has no `Description?`
  /// slot and is refused once one was read.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  S,
  DescribedExecutableDefinition<S>,
  executable_definition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses one GraphQLx executable definition or import.
  ///
  /// `import` is a GraphQLx extension.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  S,
  ImportOrExecutableDefinition<S>,
  import_or_executable_definition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_executable_api!(
  /// Parses a nonempty GraphQLx executable document.
  ///
  /// `import` is the GraphQLx extension; a leading definition description is
  /// ordinary GraphQL.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  S,
  ExecutableDocument<S>,
  executable_document,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl OperationType {
  /// Parses a GraphQLx operation keyword.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Operation Type specification](https://spec.graphql.org/draft/#OperationType).
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize> + ?Sized,
    GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
    GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
    GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
  {
    operation_type(inp)
  }
}

#[cfg(test)]
mod tests;
