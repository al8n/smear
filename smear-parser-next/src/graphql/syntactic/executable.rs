//! GraphQL executable-definition productions.
//!
//! These parsers are concrete over [`GraphqlLexer`] and
//! construct slice-typed GraphQL AST nodes. Optional collections remain concrete
//! empty collections at their own layer; their parents turn an empty collection
//! into `None` where the AST grammar makes that distinction meaningful.

use std::vec::Vec;

use smear_lexer::keywords::{Fragment, Mutation, On, Query, Subscription};
use tokora::{
  Accumulator, Lexer, ParseInput, SimpleSpan, Slice, Source, Token,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::{Action, parens},
  punct::{Brace, Bracket, Paren},
  try_parse_input::ParseAttempt,
  utils::typenum::U1,
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  directive::{const_directives, directives},
  fragment_name, peeks_where,
  selection::selection_set,
  ty::ty,
  value::default_value,
};
use crate::{
  combinator::{Equivalent, ParseCtx},
  graphql::{
    GraphQL,
    ast::{
      DescribedVariableDefinition, ExecutableDefinition, ExecutableDocument, FragmentDefinition,
      Name, NamedOperationDefinition, OperationDefinition, OperationType, StringValue,
      TypeCondition, VariableDefinition, VariableValue, VariablesDefinition,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

macro_rules! executable_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      str: Equivalent<GraphqlSlice<'inp, Src>>,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
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
        >
        + From<Unclosed<Paren, SimpleSpan, GraphQL>>
        + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
        + From<Unclosed<Brace, SimpleSpan, GraphQL>>
        + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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

#[inline]
fn is_name<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(token, GraphqlToken::<'inp, Src>::Identifier(_))
}

#[inline]
fn is_type_head<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(
    token,
    GraphqlToken::<'inp, Src>::Identifier(_) | GraphqlToken::<'inp, Src>::LBracket
  )
}

#[inline]
fn is_dollar<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(token, GraphqlToken::<'inp, Src>::Dollar)
}

#[inline]
fn is_colon<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(token, GraphqlToken::<'inp, Src>::Colon)
}

#[inline]
fn is_lparen<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(token, GraphqlToken::<'inp, Src>::LParen)
}

#[inline]
fn is_lbrace<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
{
  matches!(token, GraphqlToken::<'inp, Src>::LBrace)
}

#[inline]
fn is_fragment<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  matches!(token, GraphqlToken::<'inp, Src>::Identifier(value) if "fragment".equivalent(value))
}

#[inline]
fn is_on<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  matches!(token, GraphqlToken::<'inp, Src>::Identifier(value) if "on".equivalent(value))
}

#[inline]
fn is_operation_type<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  matches!(token, GraphqlToken::<'inp, Src>::Identifier(value)
    if "query".equivalent(value)
      || "mutation".equivalent(value)
      || "subscription".equivalent(value))
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
  guard_executable_phase(inp, Expectation::Name, is_name::<Src>)?;
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

fn try_take_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Option<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
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
  let has_name = {
    let mut peeked = inp.peek::<U1>()?;
    peeked
      .pop_front()
      .is_some_and(|token| is_name::<Src>(token.token()))
  };
  if has_name {
    take_name(inp).map(Some)
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
  guard_executable_phase(inp, Expectation::Dollar, is_dollar::<Src>)?;
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
  guard_executable_phase(inp, Expectation::Colon, is_colon::<Src>)?;
  match inp.next()? {
    Some(_) => Ok(()),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn take_operation_type<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<OperationType, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
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
  guard_executable_phase(inp, Expectation::OperationType, is_operation_type::<Src>)?;
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(value) if "query".equivalent(&value) => {
          Ok(OperationType::Query(Query::new(span)))
        }
        GraphqlToken::<'inp, Src>::Identifier(value) if "mutation".equivalent(&value) => {
          Ok(OperationType::Mutation(Mutation::new(span)))
        }
        GraphqlToken::<'inp, Src>::Identifier(value) if "subscription".equivalent(&value) => {
          Ok(OperationType::Subscription(Subscription::new(span)))
        }
        other => Err(UnexpectedToken::of(span).with_found(other).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn take_fragment<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Fragment, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
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
  guard_executable_phase(inp, Expectation::Keyword("fragment"), is_fragment::<Src>)?;
  match inp.next()? {
    Some(spanned) => Ok(Fragment::new(spanned.into_span())),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn take_on<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<On, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
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
  guard_executable_phase(inp, Expectation::Keyword("on"), is_on::<Src>)?;
  match inp.next()? {
    Some(spanned) => Ok(On::new(spanned.into_span())),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn variable_definition_core<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<VariableDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let cursor = *inp.cursor();
  take_dollar(inp)?;
  let name = take_name(inp)?;
  let variable = VariableValue::new(inp.span_since(&cursor), name);
  take_colon(inp)?;
  guard_executable_phase(inp, Expectation::Type, is_type_head::<Src>)?;
  let ty = ty(inp)?;
  let default_value = default_value(inp)?;
  let directives = const_directives(inp)?;
  let directives = if directives.directives().is_empty() {
    None
  } else {
    Some(directives)
  };
  Ok(VariableDefinition::new(
    inp.span_since(&cursor),
    variable,
    ty,
    default_value,
    directives,
  ))
}

executable_parser!(
  /// Parses a described GraphQL variable definition.
  ///
  /// The optional leading string description is frozen-parser compatibility;
  /// the variable, colon, type, default-value, and directive phases remain
  /// committed and receive local diagnostics.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  pub variable_definition,
  inp,
  DescribedVariableDefinition<GraphqlSlice<'inp, Src>>,
  {
    let cursor = *inp.cursor();
    let description = match StringValue::try_graphql(inp)? {
      ParseAttempt::Accept(value) => Some(value),
      ParseAttempt::Decline => None,
    };
    let definition = variable_definition_core(inp)?;
    Ok(DescribedVariableDefinition::new(
      inp.span_since(&cursor),
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
  _: &mut Ctx::Emitter,
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
  {
    let start = *inp.offset();
    if !peeks_where(inp, is_lparen::<Src>)? {
      return Ok(VariablesDefinition::new(
        SimpleSpan::new(start, start),
        Vec::new(),
      ));
    }

    parens(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
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
    })(inp)
    .map(|delimited| {
      let (span, _open, _close, definitions) = delimited.into_components();
      VariablesDefinition::new(span, definitions)
    })
  }
);

executable_parser!(
  /// Parses a GraphQL operation type.
  ///
  /// See the [GraphQL Operation Type specification](https://spec.graphql.org/draft/#OperationType).
  pub operation_type,
  inp,
  OperationType,
  { take_operation_type(inp) }
);

executable_parser!(
  /// Parses a GraphQL operation definition.
  ///
  /// A leading selection set is query shorthand. Otherwise the operation keyword
  /// commits the named form; absent variable and directive collections are stored
  /// as `None` on that parent node.
  ///
  /// See the [GraphQL Operation Definition specification](https://spec.graphql.org/draft/#OperationDefinition).
  pub operation_definition,
  inp,
  OperationDefinition<GraphqlSlice<'inp, Src>>,
  {
    if peeks_where(inp, is_lbrace::<Src>)? {
      return selection_set(inp).map(OperationDefinition::Shorthand);
    }

    let cursor = *inp.cursor();
    let operation_type = take_operation_type(inp)?;
    let name = try_take_name(inp)?;
    let variables = variables_definition(inp)?;
    let variables = if variables.variable_definitions().is_empty() {
      None
    } else {
      Some(variables)
    };
    let directives = directives(inp)?;
    let directives = if directives.directives().is_empty() {
      None
    } else {
      Some(directives)
    };
    guard_executable_phase(inp, Expectation::LBrace, is_lbrace::<Src>)?;
    let selection_set = selection_set(inp)?;
    Ok(OperationDefinition::Named(NamedOperationDefinition::new(
      inp.span_since(&cursor),
      operation_type,
      name,
      variables,
      directives,
      selection_set,
    )))
  }
);

fn fragment_definition_body<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: Fragment,
) -> Result<FragmentDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let name = fragment_name(inp)?;
  let on = take_on(inp)?;
  let type_name = take_name(inp)?;
  let type_condition = TypeCondition::new(
    SimpleSpan::new(on.span().start(), type_name.span().end()),
    type_name,
  );
  let directives = directives(inp)?;
  let directives = if directives.directives().is_empty() {
    None
  } else {
    Some(directives)
  };
  guard_executable_phase(inp, Expectation::LBrace, is_lbrace::<Src>)?;
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
  {
    let keyword = take_fragment(inp)?;
    fragment_definition_body(inp, keyword)
  }
);

#[inline]
fn is_executable_definition_head<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  is_lbrace::<Src>(token) || is_fragment::<Src>(token) || is_operation_type::<Src>(token)
}

executable_parser!(
  /// Parses one GraphQL executable definition.
  ///
  /// The first token chooses between a fragment definition and an operation;
  /// rejected heads stay unconsumed for recovery.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  pub executable_definition,
  inp,
  ExecutableDefinition<GraphqlSlice<'inp, Src>>,
  {
    guard_executable_phase(
      inp,
      Expectation::ExecutableDefinition,
      is_executable_definition_head::<Src>,
    )?;
    if peeks_where(inp, is_fragment::<Src>)? {
      fragment_definition(inp).map(ExecutableDefinition::Fragment)
    } else {
      operation_definition(inp).map(ExecutableDefinition::Operation)
    }
  }
);

fn decide_executable_definition_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
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
  /// A document has no enclosing delimiter, so it commits to its first
  /// executable definition and then parses every remaining definition.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  pub executable_document,
  inp,
  ExecutableDocument<GraphqlSlice<'inp, Src>>,
  {
    let cursor = *inp.cursor();
    guard_executable_phase(
      inp,
      Expectation::ExecutableDefinition,
      is_executable_definition_head::<Src>,
    )?;
    let first = executable_definition(inp)?;
    let mut definitions: Vec<ExecutableDefinition<GraphqlSlice<'inp, Src>>> = executable_definition
      .repeated_while::<_, U1>(decide_executable_definition_head::<Src, Ctx>)
      .collect_with(Vec::new())
      .parse_input(inp)?;
    definitions.insert(0, first);
    Ok(ExecutableDocument::new(inp.span_since(&cursor), definitions))
  }
);

macro_rules! impl_executable_api {
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
        str: Equivalent<$slice>,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
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
          >
          + From<Unclosed<Paren, SimpleSpan, GraphQL>>
          + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_executable_api!(
  /// Parses a variable definition without an optional compatibility description.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  VariableDefinition<S>,
  variable_definition_core
);

impl_executable_api!(
  /// Parses a variable definition with its optional frozen-compatibility description.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  DescribedVariableDefinition<S>,
  variable_definition
);

impl_executable_api!(
  /// Parses a variables collection, returning an empty zero-width collection when `(` is absent.
  ///
  /// See the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).
  S,
  VariablesDefinition<S>,
  variables_definition
);

impl_executable_api!(
  /// Parses an operation definition.
  ///
  /// See the [GraphQL Operation Definition specification](https://spec.graphql.org/draft/#OperationDefinition).
  S,
  OperationDefinition<S>,
  operation_definition
);

impl_executable_api!(
  /// Parses a fragment definition.
  ///
  /// See the [GraphQL Fragment Definition specification](https://spec.graphql.org/draft/#FragmentDefinition).
  S,
  FragmentDefinition<S>,
  fragment_definition
);

impl_executable_api!(
  /// Parses an executable definition.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  S,
  ExecutableDefinition<S>,
  executable_definition
);

impl_executable_api!(
  /// Parses a nonempty executable document.
  ///
  /// See the [GraphQL Executable Definitions specification](https://spec.graphql.org/draft/#ExecutableDefinition).
  S,
  ExecutableDocument<S>,
  executable_document
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
    str: Equivalent<GraphqlSlice<'inp, Src>>,
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
      > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
      + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
      + From<Unclosed<Brace, SimpleSpan, GraphQL>>
      + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
  {
    operation_type(inp)
  }
}

#[cfg(test)]
mod tests;
