//! GraphQL executable-definition productions.
//!
//! These parsers are concrete over [`GraphqlLexer`] and
//! construct slice-typed GraphQL AST nodes. Optional collections remain concrete
//! empty collections at their own layer; their parents turn an empty collection
//! into `None` where the AST grammar makes that distinction meaningful. Real
//! executable alternatives use one-token lookahead and deterministic
//! [`ParseChoice`] branches that consume their classified heads directly before
//! entering committed tails.

use std::vec::Vec;

use crate::lexer::{
  graphql::{ContextualKeyword, syntactic::SyntacticTokenKind},
  keywords::{Fragment, Mutation, Query, Subscription},
};
use tokora::{
  Accumulator, Branch, EmitterView, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{Action, parens},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  directive::{const_directives, directives},
  fragment_name, peeks_where,
  selection::{selection_set, type_condition},
  ty::ty,
  value::default_value,
};
use crate::parser::{
  combinator::{ParseCtx, TokenSpannedExt, extent_end, extent_since, extent_start},
  graphql::{
    GraphQL,
    ast::{
      DescribedExecutableDefinition, DescribedVariableDefinition, ExecutableDefinition,
      ExecutableDocument, FragmentDefinition, Name, NamedOperationDefinition, OperationDefinition,
      OperationType, StringValue, VariableDefinition, VariableValue, VariablesDefinition,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

macro_rules! executable_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [$($bounds:tt)*], $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      $($bounds)*
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

/// Checks a committed executable-production phase without consuming a rejected
/// token. Only its span and kind cross the lookahead boundary.
fn guard_executable_phase<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
  mut accepts: impl FnMut(&GraphqlToken<'inp, Src>) -> bool,
) -> Result<(), GraphqlError<'inp, Src, Ctx>>
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
    match peeked.pop_front() {
      Some(token) if accepts(token.token()) => return Ok(()),
      Some(token) => Some((*token.span(), token.token().kind())),
      None => None,
    }
  };

  match rejected {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, expected, span).into()),
    None => Err(
      DialectGraphqlError::maybe_unexpected_token(None, expected, SimpleSpan::new(offset, offset))
        .into(),
    ),
  }
}

#[derive(Debug, Copy, Clone)]
enum OperationHead {
  Query,
  Mutation,
  Subscription,
}

#[derive(Debug, Copy, Clone)]
enum ExecutableHead {
  Shorthand,
  Operation(OperationHead),
  Fragment,
}

#[derive(Debug, Copy, Clone)]
enum ClassifiedExecutableHead {
  Accepted(ExecutableHead, SimpleSpan, SyntacticTokenKind),
  Rejected(Option<(SimpleSpan, SyntacticTokenKind)>),
}

impl ClassifiedExecutableHead {
  #[inline]
  const fn found(self) -> Option<(SimpleSpan, SyntacticTokenKind)> {
    match self {
      Self::Accepted(_, span, kind) => Some((span, kind)),
      Self::Rejected(found) => found,
    }
  }
}

/// Classifies the first executable-definition token without consuming it.
#[inline]
fn classify_executable_head<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ClassifiedExecutableHead, GraphqlError<'inp, Src, Ctx>>
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
      let token = token.token();
      let head = match token.downcast_ref() {
        Some(ContextualKeyword::Query) => Some(ExecutableHead::Operation(OperationHead::Query)),
        Some(ContextualKeyword::Mutation) => {
          Some(ExecutableHead::Operation(OperationHead::Mutation))
        }
        Some(ContextualKeyword::Subscription) => {
          Some(ExecutableHead::Operation(OperationHead::Subscription))
        }
        Some(ContextualKeyword::Fragment) => Some(ExecutableHead::Fragment),
        _ if matches!(token, GraphqlToken::<'inp, Src>::LBrace) => Some(ExecutableHead::Shorthand),
        _ => None,
      };
      match head {
        Some(head) => ClassifiedExecutableHead::Accepted(head, span, kind),
        None => ClassifiedExecutableHead::Rejected(Some((span, kind))),
      }
    }
    None => ClassifiedExecutableHead::Rejected(None),
  })
}

/// Builds an operation type from a head already classified by executable
/// dispatch, without inspecting source text again.
#[inline]
fn operation_type_from_classified(head: OperationHead, span: SimpleSpan) -> OperationType {
  match head {
    OperationHead::Query => OperationType::Query(Query::new(span)),
    OperationHead::Mutation => OperationType::Mutation(Mutation::new(span)),
    OperationHead::Subscription => OperationType::Subscription(Subscription::new(span)),
  }
}

/// Emits the caller's local expectation from one classified executable head.
fn expected_classified_executable_head<'inp, Src, Ctx, T>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
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
  match found {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, expected, span).into()),
    None => {
      let offset = *inp.offset();
      Err(
        DialectGraphqlError::maybe_unexpected_token(
          None,
          expected,
          SimpleSpan::new(offset, offset),
        )
        .into(),
      )
    }
  }
}

fn take_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_executable_phase(inp, Expectation::Name, |token| token.is_identifier())?;
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(value) => Ok(Name::new(span, value)),
        other => Err(UnexpectedToken::of(span).with_found(other).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

/// Consumes an identifier already classified by an executable dispatch.
///
/// This deliberately skips a declining atom: the classifier has already
/// established the token's structural role. Any impossible cache/input mismatch
/// remains a typed diagnostic instead of a panic.
#[inline]
fn take_classified_identifier<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<(SimpleSpan, GraphqlSlice<'inp, Src>), GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(source) => Ok((span, source)),
        token => Err(DialectGraphqlError::unexpected_token(token.kind(), expected, span).into()),
      }
    }
    None => {
      let offset = *inp.offset();
      Err(
        DialectGraphqlError::maybe_unexpected_token(
          None,
          expected,
          SimpleSpan::new(offset, offset),
        )
        .into(),
      )
    }
  }
}

/// Optionally consumes an operation name after peeking its identifier once.
#[inline]
fn try_take_classified_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Option<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let has_name = {
    let mut peeked = inp.peek::<U1>()?;
    matches!(
      peeked.pop_front(),
      Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::Identifier(_))
    )
  };
  if has_name {
    let (span, source) = take_classified_identifier(inp, Expectation::Name)?;
    Ok(Some(Name::new(span, source)))
  } else {
    Ok(None)
  }
}

fn take_dollar<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<SimpleSpan, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_executable_phase(inp, Expectation::Dollar, |token| token.is_dollar())?;
  match inp.next()? {
    Some(spanned) => Ok(spanned.into_span()),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn take_colon<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<(), GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_executable_phase(inp, Expectation::Colon, |token| token.is_colon())?;
  match inp.next()? {
    Some(_) => Ok(()),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn take_fragment<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Fragment, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_executable_phase(inp, Expectation::Keyword("fragment"), |token| {
    token.downcast_ref() == Some(ContextualKeyword::Fragment)
  })?;
  match inp.next()? {
    Some(spanned) => Ok(Fragment::new(spanned.into_span())),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn variable_definition_core<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<VariableDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let node_start = extent_start(inp)?;
  take_dollar(inp)?;
  let name = take_name(inp)?;
  let variable = VariableValue::new(extent_since(inp, node_start), name);
  take_colon(inp)?;
  guard_executable_phase(inp, Expectation::Type, |token| {
    token.is_identifier() || token.is_l_bracket()
  })?;
  let ty = ty(inp)?;
  let default_value = default_value(inp)?;
  let directives = const_directives(inp)?;
  let directives = if directives.directives().is_empty() {
    None
  } else {
    Some(directives)
  };
  Ok(VariableDefinition::new(
    extent_since(inp, node_start),
    variable,
    ty,
    default_value,
    directives,
  ))
}

executable_parser!(
  /// Parses a described GraphQL variable definition.
  ///
  /// `VariableDefinition : Description? Variable : Type DefaultValue?
  /// Directives[Const]?` — the leading string is the production's own optional
  /// description. The variable, colon, type, default-value, and directive phases
  /// remain committed and receive local diagnostics.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  pub variable_definition,
  inp,
  DescribedVariableDefinition<GraphqlSlice<'inp, Src>>,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let node_start = extent_start(inp)?;
    let description = match StringValue::try_graphql(inp)? {
      ParseAttempt::Accept(value) => Some(value),
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

fn is_variable_definition_head<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(
    token,
    GraphqlToken::<'inp, Src>::Dollar
      | GraphqlToken::<'inp, Src>::LitInlineStr(_)
      | GraphqlToken::<'inp, Src>::LitBlockStr(_)
  )
}

fn decide_variable_definition_head<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::RParen) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

executable_parser!(
  /// Parses a GraphQL variables collection.
  ///
  /// If `(` is absent, this consumes nothing and returns an empty zero-width
  /// collection. Once `(` is present, one or more variable definitions are
  /// required and a missing closer emits the typed `Unclosed<Paren>` error.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  pub variables_definition,
  inp,
  VariablesDefinition<GraphqlSlice<'inp, Src>>,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::parser::combinator::extent`.
    let start = extent_end(inp);
    if !peeks_where(inp, |token| token.is_l_paren())? {
      return Ok(VariablesDefinition::new(
        SimpleSpan::new(start, start),
        Vec::new(),
      ));
    }

    // Not the `Delimited`'s own span: tokora builds that one from the lookahead cursor at both
    // ends, so it opens before the `(`'s leading trivia and closes wherever the peek past `)`
    // landed. See `crate::parser::combinator::extent`.
    let node_start = extent_start(inp)?;
    let delimited = parens(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_executable_phase(
        inp,
        Expectation::VariableDefinition,
        is_variable_definition_head::<Src>,
      )?;
      let first = variable_definition(inp)?;
      let mut rest: Vec<DescribedVariableDefinition<GraphqlSlice<'inp, Src>>> = variable_definition
        .repeated_while::<_, U1>(decide_variable_definition_head::<Src, Ctx>)
        .collect_with(Vec::new())
        .parse_input(inp)?;
      rest.insert(0, first);
      Ok(rest)
    })(inp)?;
    let (_, _open, _close, definitions) = delimited.into_components();
    Ok(VariablesDefinition::new(
      extent_since(inp, node_start),
      definitions,
    ))
  }
);

/// Parses the committed tail of a named operation after its keyword is consumed.
#[inline]
pub(super) fn named_operation_after_classified_head<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
  operation_type: OperationType,
) -> Result<OperationDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let name = try_take_classified_name(inp)?;
  let variables = variables_definition(inp)?;
  let variables = (!variables.variable_definitions().is_empty()).then_some(variables);
  let directives = directives(inp)?;
  let directives = (!directives.directives().is_empty()).then_some(directives);
  guard_executable_phase(inp, Expectation::LBrace, |token| token.is_l_brace())?;
  let selection_set = selection_set(inp)?;
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
  /// Parses a GraphQL operation type.
  ///
  /// One-token dispatch selects the matching operation keyword branch, which
  /// consumes the classified identifier directly.
  ///
  /// See the [GraphQL Operation Type specification](https://spec.graphql.org/draft/#OperationType).
  pub operation_type,
  inp,
  OperationType,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let classified = classify_executable_head(inp)?;
    let found = classified.found();
    let branch: Branch<2> = match classified {
      ClassifiedExecutableHead::Accepted(ExecutableHead::Operation(OperationHead::Query), ..) => {
        Branch::B0
      }
      ClassifiedExecutableHead::Accepted(ExecutableHead::Operation(OperationHead::Mutation), ..) => {
        Branch::B1
      }
      ClassifiedExecutableHead::Accepted(
        ExecutableHead::Operation(OperationHead::Subscription),
        ..,
      ) => Branch::B2,
      _ => {
        return expected_classified_executable_head(inp, Expectation::OperationType, found);
      }
    };

    let mut tails = (
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::OperationType)?;
        Ok(OperationType::Query(Query::new(span)))
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::OperationType)?;
        Ok(OperationType::Mutation(Mutation::new(span)))
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::OperationType)?;
        Ok(OperationType::Subscription(Subscription::new(span)))
      },
    );
    tails.parse_choice(inp, &branch)
  }
);

executable_parser!(
  /// Parses a GraphQL operation definition.
  ///
  /// One-token dispatch selects shorthand or a named operation keyword. Each
  /// branch consumes its classified head directly before entering the committed
  /// named-operation tail; absent variable and directive collections are stored
  /// as `None` on that parent node.
  ///
  /// See the [GraphQL Operation Definition specification](https://spec.graphql.org/draft/#OperationDefinition).
  pub operation_definition,
  inp,
  OperationDefinition<GraphqlSlice<'inp, Src>>,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    // A real node start, so the **first token's** start: `inp.offset()` would report the end of a
    // token the caller had already peeked. See `crate::parser::combinator::extent`.
    let start = extent_start(inp)?;
    let classified = classify_executable_head(inp)?;
    let found = classified.found();
    let mut named_head = None;
    let branch: Branch<1> = match classified {
      ClassifiedExecutableHead::Accepted(ExecutableHead::Shorthand, ..) => Branch::B0,
      ClassifiedExecutableHead::Accepted(ExecutableHead::Operation(head), ..) => {
        named_head = Some(head);
        Branch::B1
      }
      _ => {
        return expected_classified_executable_head(inp, Expectation::OperationType, found);
      }
    };

    let mut tails = (
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        selection_set(inp).map(OperationDefinition::Shorthand)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::OperationType)?;
        let head = named_head.expect("named operation branch has an operation head");
        let operation_type = operation_type_from_classified(head, span);
        named_operation_after_classified_head(inp, start, operation_type)
      },
    );
    tails.parse_choice(inp, &branch)
  }
);

pub(super) fn fragment_definition_body<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: Fragment,
) -> Result<FragmentDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let name = fragment_name(inp)?;
  let type_condition = type_condition(inp)?;
  let directives = directives(inp)?;
  let directives = if directives.directives().is_empty() {
    None
  } else {
    Some(directives)
  };
  guard_executable_phase(inp, Expectation::LBrace, |token| token.is_l_brace())?;
  let selection_set = selection_set(inp)?;
  Ok(FragmentDefinition::new(
    SimpleSpan::new(keyword.span().start(), selection_set.span().end()),
    name,
    type_condition,
    directives,
    selection_set,
  ))
}

executable_parser!(
  /// Parses a named GraphQL fragment definition.
  ///
  /// See the [GraphQL Fragment Definition specification](https://spec.graphql.org/draft/#FragmentDefinition).
  pub fragment_definition,
  inp,
  FragmentDefinition<GraphqlSlice<'inp, Src>>,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let keyword = take_fragment(inp)?;
    fragment_definition_body(inp, keyword)
  }
);

executable_parser!(
  /// Parses one GraphQL executable definition.
  ///
  /// Both keyworded alternatives open with an optional description —
  /// `OperationDefinition : Description? OperationType …` and
  /// `FragmentDefinition : Description? fragment …`. After an accepted
  /// description, the following definition head is committed. One-token dispatch
  /// then chooses shorthand, a named operation, or a fragment; the shorthand
  /// `OperationDefinition : SelectionSet` has no `Description?` slot and is
  /// refused once one was read. Each branch consumes its classified head directly
  /// before entering the corresponding committed tail; rejected heads stay
  /// unconsumed for recovery.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition)
  /// for the wrapped inner definition.
  pub executable_definition,
  inp,
  DescribedExecutableDefinition<GraphqlSlice<'inp, Src>>,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let node_start = extent_start(inp)?;
    let description = match StringValue::try_graphql(inp)? {
      ParseAttempt::Accept(value) => Some(value),
      ParseAttempt::Decline => None,
    };
    let classified = classify_executable_head(inp)?;
    let found = classified.found();
    let mut named_head = None;
    let branch: Branch<2> = match classified {
      // Only the keyworded alternatives carry a `Description?`; the shorthand
      // `OperationDefinition : SelectionSet` does not. The `{` is reported where it
      // stands and left unconsumed, so a caller resumes at the selection set rather
      // than inside it.
      ClassifiedExecutableHead::Accepted(ExecutableHead::Shorthand, ..)
        if description.is_some() =>
      {
        return expected_classified_executable_head(
          inp,
          Expectation::OperationTypeOrFragment,
          found,
        );
      }
      ClassifiedExecutableHead::Accepted(ExecutableHead::Shorthand, ..) => Branch::B0,
      ClassifiedExecutableHead::Accepted(ExecutableHead::Operation(head), span, ..) => {
        named_head = Some((head, span.start()));
        Branch::B1
      }
      ClassifiedExecutableHead::Accepted(ExecutableHead::Fragment, ..) => Branch::B2,
      ClassifiedExecutableHead::Rejected(_) => {
        return expected_classified_executable_head(
          inp,
          Expectation::ExecutableDefinition,
          found,
        );
      }
    };

    let mut tails = (
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        selection_set(inp)
          .map(OperationDefinition::Shorthand)
          .map(ExecutableDefinition::Operation)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, _) = take_classified_identifier(inp, Expectation::OperationType)?;
        let (head, start) = named_head.expect("named operation branch has an operation head");
        let operation_type = operation_type_from_classified(head, span);
        named_operation_after_classified_head(inp, start, operation_type)
        .map(ExecutableDefinition::Operation)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let (span, _) =
          take_classified_identifier(inp, Expectation::Keyword("fragment"))?;
        fragment_definition_body(inp, Fragment::new(span)).map(ExecutableDefinition::Fragment)
      },
    );
    let definition = tails.parse_choice(inp, &branch)?;
    Ok(DescribedExecutableDefinition::new(
      extent_since(inp, node_start),
      description,
      definition,
    ))
  }
);

fn decide_executable_definition_head<'inp, Src, Ctx>(
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

executable_parser!(
  /// Parses a nonempty GraphQL executable document.
  ///
  /// A document has no enclosing delimiter. Each definition may open with its own
  /// optional description, which the keyworded alternatives carry and the
  /// shorthand does not.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition)
  /// for each wrapped inner definition.
  pub executable_document,
  inp,
  ExecutableDocument<GraphqlSlice<'inp, Src>>,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let Spanned { span, data: definitions }: Spanned<
      Vec<DescribedExecutableDefinition<GraphqlSlice<'inp, Src>>>,
      SimpleSpan,
    > = executable_definition
      .repeated_while::<_, U1>(decide_executable_definition_head::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(|definitions: Vec<DescribedExecutableDefinition<GraphqlSlice<'inp, Src>>>| definitions)
      .token_spanned()
      .parse_input(inp)?;
    Ok(ExecutableDocument::new(span, definitions))
  }
);

macro_rules! impl_executable_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
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
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        $($bounds)*
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_executable_api!(
  /// Parses a variable definition without its optional leading description.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  VariableDefinition<S>,
  variable_definition_core,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl_executable_api!(
  /// Parses a variable definition with its optional leading description.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  DescribedVariableDefinition<S>,
  variable_definition,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl_executable_api!(
  /// Parses a variables collection, returning an empty zero-width collection when `(` is absent.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  VariablesDefinition<S>,
  variables_definition,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl_executable_api!(
  /// Parses an operation definition.
  ///
  /// See the [GraphQL Operation Definition specification](https://spec.graphql.org/draft/#OperationDefinition).
  S,
  OperationDefinition<S>,
  operation_definition,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl_executable_api!(
  /// Parses a fragment definition.
  ///
  /// See the [GraphQL Fragment Definition specification](https://spec.graphql.org/draft/#FragmentDefinition).
  S,
  FragmentDefinition<S>,
  fragment_definition,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl_executable_api!(
  /// Parses an executable definition with its optional leading description.
  ///
  /// The shorthand `OperationDefinition : SelectionSet` has no `Description?`
  /// slot and is refused once one was read.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition)
  /// for the wrapped inner definition.
  S,
  DescribedExecutableDefinition<S>,
  executable_definition,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl_executable_api!(
  /// Parses a nonempty executable document whose definitions may each open with
  /// their own optional description.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition)
  /// for each wrapped inner definition.
  S,
  ExecutableDocument<S>,
  executable_document,
  [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

impl OperationType {
  /// Parses a GraphQL operation type.
  ///
  /// The lexer source is inferred from `inp`.
  ///
  /// See the [GraphQL Operation Type specification](https://spec.graphql.org/draft/#OperationType).
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
    GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
  {
    operation_type(inp)
  }
}

#[cfg(test)]
mod tests;
