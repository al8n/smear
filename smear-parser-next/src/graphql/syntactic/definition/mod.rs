//! GraphQL SDL definition productions over the concrete syntactic lexer.
//!
//! These parsers follow the same rules as the executable and value suites: each
//! production is concrete over [`GraphqlLexer`], collection openers have a
//! declining `try_` entry point, and type-definition dispatch classifies its
//! contextual-keyword head once before entering a committed branch.

use std::vec::Vec;

use smear_lexer::graphql::{ContextualKeyword, syntactic::SyntacticTokenKind};
use tokora::{
  Accumulator, Branch, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Ampersand, Brace, Bracket, Paren, Pipe},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  directive::const_directives, executable::operation_type, name as parse_name, ty::ty,
  value::default_value,
};
use crate::{
  combinator::{ParseCtx, at, colon, equal, try_equal},
  graphql::{
    GraphQL,
    ast::{
      ArgumentsDefinition, ConstDirectives, Described, DirectiveDefinition, DirectiveLocations,
      EnumTypeDefinition, EnumValueDefinition, EnumValuesDefinition, FieldDefinition,
      FieldsDefinition, ImplementInterfaces, InputFieldsDefinition, InputObjectTypeDefinition,
      InputValueDefinition, InterfaceTypeDefinition, Location, Name, ObjectTypeDefinition,
      RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
      SchemaDefinition, StringValue, TypeDefinition, UnionMemberTypes, UnionTypeDefinition,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
    keyword::{try_implements as try_implements_keyword, try_repeatable as try_repeatable_keyword},
  },
  type_system::{
    EnumValueDefinition as EnumValueDefinitionCore, ExecutableDirectiveLocation,
    FieldDefinition as FieldDefinitionCore, InputValueDefinition as InputValueDefinitionCore,
    TypeSystemDirectiveLocation,
  },
};

macro_rules! definition_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [], $body:block) => {
    definition_parser!(@impl $(#[$meta])* $visibility $name, $input, $output, [], $body);
  };
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [contextual], $body:block) => {
    definition_parser!(
      @impl
      $(#[$meta])*
      $visibility $name,
      $input,
      $output,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  (@impl $(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [$($bounds:tt)*], $body:block) => {
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

/// Emits a dialect expectation while leaving a mismatching token in the input.
fn expected_definition_phase<'inp, Src, Ctx, T>(
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

/// Checks a committed definition phase without consuming a rejected token.
fn guard_definition_phase<'inp, Src, Ctx>(
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
  guard_definition_phase(inp, Expectation::Name, |token| token.is_identifier())?;
  parse_name(inp)
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
  guard_definition_phase(inp, Expectation::Colon, |token| token.is_colon())?;
  colon(inp).map(|_| ())
}

fn take_type<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<crate::graphql::ast::Type<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
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
    > + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  guard_definition_phase(inp, Expectation::Type, |token| {
    token.is_identifier() || token.is_l_bracket()
  })?;
  ty(inp)
}

fn take_contextual_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
) -> Result<SimpleSpan, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  guard_definition_phase(inp, Expectation::Keyword(keyword.as_str()), |token| {
    token.downcast_ref() == Some(keyword)
  })?;
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(_) => Ok(span),
        token => Err(
          DialectGraphqlError::unexpected_token(
            token.kind(),
            Expectation::Keyword(keyword.as_str()),
            span,
          )
          .into(),
        ),
      }
    }
    None => expected_definition_phase(inp, Expectation::Keyword(keyword.as_str())),
  }
}

fn take_enum_value<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  guard_definition_phase(inp, Expectation::EnumValue, |token| {
    token.is_identifier()
      && !matches!(
        token.downcast_ref(),
        Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null)
      )
  })?;
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(value) => Ok(Name::new(span, value)),
        token => Err(
          DialectGraphqlError::unexpected_token(token.kind(), Expectation::EnumValue, span).into(),
        ),
      }
    }
    None => expected_definition_phase(inp, Expectation::EnumValue),
  }
}

fn optional_const_directives<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Option<ConstDirectives<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  let directives = const_directives(inp)?;
  Ok((!directives.directives().is_empty()).then_some(directives))
}

definition_parser!(
  /// Parses an optional SDL description.
  ///
  /// A non-string head is left available for the following committed production.
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description).
  pub description,
  inp,
  Option<StringValue<GraphqlSlice<'inp, Src>>>,
  [],
  {
    match StringValue::try_graphql(inp)? {
      ParseAttempt::Accept(value) => Ok(Some(value)),
      ParseAttempt::Decline => Ok(None),
    }
  }
);

definition_parser!(
  /// Parses an SDL input value definition.
  ///
  /// See the [GraphQL Input Value Definition specification](https://spec.graphql.org/draft/#InputValueDefinition).
  pub input_value_definition,
  inp,
  InputValueDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let name = take_name(inp)?;
    take_colon(inp)?;
    let ty = take_type(inp)?;
    let default_value = default_value(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = inp.span_since(&cursor);
    Ok(Described::new(
      span,
      description,
      InputValueDefinitionCore::new(span, name, ty, default_value, directives),
    ))
  }
);

fn decide_paren_tail<'inp, Src, Ctx>(
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
    Some(token) if token.token().is_r_paren() => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_brace_tail<'inp, Src, Ctx>(
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
    Some(token) if token.token().is_r_brace() => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_lparen_opener<'inp, Src, Ctx>(
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
    Some(token) if token.token().is_l_paren() => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_lbrace_opener<'inp, Src, Ctx>(
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
    Some(token) if token.token().is_l_brace() => Action::Continue,
    _ => Action::Stop,
  })
}

definition_parser!(
  /// Parses a nonempty SDL arguments definition.
  ///
  /// See the [GraphQL Arguments Definition specification](https://spec.graphql.org/draft/#ArgumentsDefinition).
  pub arguments_definition,
  inp,
  ArgumentsDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    input_value_definition
      .repeated_while::<_, U1>(decide_paren_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ArgumentsDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL arguments definition, declining when `(` is absent.
  pub try_arguments_definition,
  inp,
  ParseAttempt<ArgumentsDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    arguments_definition
      .peek_then_try::<_, U1>(decide_lparen_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

definition_parser!(
  /// Parses an SDL field definition.
  ///
  /// See the [GraphQL Field Definition specification](https://spec.graphql.org/draft/#FieldDefinition).
  pub field_definition,
  inp,
  FieldDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let name = take_name(inp)?;
    let arguments_definition = try_arguments_definition(inp)?.into();
    take_colon(inp)?;
    let ty = take_type(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = inp.span_since(&cursor);
    Ok(Described::new(
      span,
      description,
      FieldDefinitionCore::new(span, name, arguments_definition, ty, directives),
    ))
  }
);

definition_parser!(
  /// Parses a nonempty SDL fields definition.
  ///
  /// See the [GraphQL Fields Definition specification](https://spec.graphql.org/draft/#FieldsDefinition).
  pub fields_definition,
  inp,
  FieldsDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    field_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| FieldsDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL fields definition, declining when `{` is absent.
  pub try_fields_definition,
  inp,
  ParseAttempt<FieldsDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    fields_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

definition_parser!(
  /// Parses a nonempty SDL input-fields definition.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  pub input_fields_definition,
  inp,
  InputFieldsDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    input_value_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| InputFieldsDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL input-fields definition, declining when `{` is absent.
  pub try_input_fields_definition,
  inp,
  ParseAttempt<InputFieldsDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    input_fields_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

fn decide_identifier_tail<'inp, Src, Ctx>(
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
    Some(token) if token.token().is_identifier() => Action::Continue,
    _ => Action::Stop,
  })
}

fn implements_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let Spanned {
    span,
    data: interfaces,
  } = take_name
    // `separated_by_ampersand_while` currently defaults its separator marker's
    // language to `()`. Keep the native engine while selecting GraphQL explicitly.
    .separated_while::<Ampersand<(), (), GraphQL>, _, U1>(decide_identifier_tail::<Src, Ctx>)
    .allow_leading()
    .at_least(1)
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)?;
  Ok(ImplementInterfaces::new(
    SimpleSpan::new(start, span.end()),
    interfaces,
  ))
}

definition_parser!(
  /// Parses an SDL `implements` clause.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  pub implements,
  inp,
  ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Implements)?.start();
    implements_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Attempts an SDL `implements` clause, declining when `implements` is absent.
  pub try_implements,
  inp,
  ParseAttempt<ImplementInterfaces<Name<GraphqlSlice<'inp, Src>>>>,
  [contextual],
  {
    match try_implements_keyword(inp)? {
      ParseAttempt::Accept(keyword) => {
        implements_after_keyword(inp, keyword.span().start()).map(ParseAttempt::Accept)
      }
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

fn take_equal<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<tokora::punct::Equal<SimpleSpan, (), GraphQL>, GraphqlError<'inp, Src, Ctx>>
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
  guard_definition_phase(inp, Expectation::Equal, |token| token.is_equal())?;
  equal(inp)
}

fn union_members_after_equal<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UnionMemberTypes<Name<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let Spanned {
    span,
    data: members,
  } = take_name
    .separated_while::<Pipe<(), (), GraphQL>, _, U1>(decide_identifier_tail::<Src, Ctx>)
    .allow_leading()
    .at_least(1)
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)?;
  Ok(UnionMemberTypes::new(
    SimpleSpan::new(start, span.end()),
    members,
  ))
}

definition_parser!(
  /// Parses an SDL union-members clause.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  pub union_members,
  inp,
  UnionMemberTypes<Name<GraphqlSlice<'inp, Src>>>,
  [],
  {
    let equal = take_equal(inp)?;
    union_members_after_equal(inp, equal.span().start())
  }
);

definition_parser!(
  /// Attempts an SDL union-members clause, declining when `=` is absent.
  pub try_union_members,
  inp,
  ParseAttempt<UnionMemberTypes<Name<GraphqlSlice<'inp, Src>>>>,
  [],
  {
    match try_equal(inp)? {
      ParseAttempt::Accept(equal) => {
        union_members_after_equal(inp, equal.span().start()).map(ParseAttempt::Accept)
      }
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

#[inline]
fn classify_location(keyword: ContextualKeyword, span: SimpleSpan) -> Option<Location> {
  Some(match keyword {
    ContextualKeyword::QueryLocation => ExecutableDirectiveLocation::query(span).into(),
    ContextualKeyword::MutationLocation => ExecutableDirectiveLocation::mutation(span).into(),
    ContextualKeyword::SubscriptionLocation => {
      ExecutableDirectiveLocation::subscription(span).into()
    }
    ContextualKeyword::FieldLocation => ExecutableDirectiveLocation::field(span).into(),
    ContextualKeyword::FragmentDefinitionLocation => {
      ExecutableDirectiveLocation::fragment_definition(span).into()
    }
    ContextualKeyword::FragmentSpreadLocation => {
      ExecutableDirectiveLocation::fragment_spread(span).into()
    }
    ContextualKeyword::InlineFragmentLocation => {
      ExecutableDirectiveLocation::inline_fragment(span).into()
    }
    ContextualKeyword::VariableDefinitionLocation => {
      ExecutableDirectiveLocation::variable_definition(span).into()
    }
    ContextualKeyword::SchemaLocation => TypeSystemDirectiveLocation::schema(span).into(),
    ContextualKeyword::ScalarLocation => TypeSystemDirectiveLocation::scalar(span).into(),
    ContextualKeyword::ObjectLocation => TypeSystemDirectiveLocation::object(span).into(),
    ContextualKeyword::FieldDefinitionLocation => {
      TypeSystemDirectiveLocation::field_definition(span).into()
    }
    ContextualKeyword::ArgumentDefinitionLocation => {
      TypeSystemDirectiveLocation::argument_definition(span).into()
    }
    ContextualKeyword::InterfaceLocation => TypeSystemDirectiveLocation::interface(span).into(),
    ContextualKeyword::UnionLocation => TypeSystemDirectiveLocation::union(span).into(),
    ContextualKeyword::EnumLocation => TypeSystemDirectiveLocation::r#enum(span).into(),
    ContextualKeyword::EnumValueLocation => TypeSystemDirectiveLocation::enum_value(span).into(),
    ContextualKeyword::InputObjectLocation => {
      TypeSystemDirectiveLocation::input_object(span).into()
    }
    ContextualKeyword::InputFieldDefinitionLocation => {
      TypeSystemDirectiveLocation::input_field_definition(span).into()
    }
    _ => return None,
  })
}

definition_parser!(
  /// Parses one GraphQL directive location.
  ///
  /// It consumes one token and classifies its contextual keyword once.
  /// See the [GraphQL Directive Location specification](https://spec.graphql.org/draft/#DirectiveLocation).
  pub location,
  inp,
  Location,
  [contextual],
  {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        match token
          .downcast_ref()
          .and_then(|keyword| classify_location(keyword, span))
        {
          Some(location) => Ok(location),
          None => Err(
            DialectGraphqlError::unexpected_token(
              token.kind(),
              Expectation::DirectiveLocation,
              span,
            )
            .into(),
          ),
        }
      }
      None => expected_definition_phase(inp, Expectation::DirectiveLocation),
    }
  }
);

definition_parser!(
  /// Parses a nonempty SDL directive-locations list.
  ///
  /// A leading `|` is accepted; a trailing `|` remains a separator diagnostic.
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
  pub directive_locations,
  inp,
  DirectiveLocations<Location>,
  [contextual],
  {
    let Spanned {
      span,
      data: locations,
    } = location
      .separated_while::<Pipe<(), (), GraphQL>, _, U1>(decide_identifier_tail::<Src, Ctx>)
      .allow_leading()
      .at_least(1)
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)?;
    Ok(DirectiveLocations::new(span, locations))
  }
);

definition_parser!(
  /// Parses an SDL enum-value definition.
  ///
  /// `true`, `false`, and `null` are rejected as required by `EnumValue`.
  /// See the [GraphQL Enum Value Definition specification](https://spec.graphql.org/draft/#EnumValueDefinition).
  pub enum_value_definition,
  inp,
  EnumValueDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let value = take_enum_value(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = inp.span_since(&cursor);
    Ok(Described::new(
      span,
      description,
      EnumValueDefinitionCore::new(span, value, directives),
    ))
  }
);

definition_parser!(
  /// Parses a nonempty SDL enum-values definition.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  pub enum_values_definition,
  inp,
  EnumValuesDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    enum_value_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| EnumValuesDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL enum-values definition, declining when `{` is absent.
  pub try_enum_values_definition,
  inp,
  ParseAttempt<EnumValuesDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    enum_values_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

definition_parser!(
  /// Parses an SDL root operation type definition.
  ///
  /// See the [GraphQL Root Operation Type Definition specification](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
  pub root_operation_type_definition,
  inp,
  RootOperationTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let operation_type = operation_type(inp)?;
    take_colon(inp)?;
    let name = take_name(inp)?;
    Ok(RootOperationTypeDefinition::new(
      inp.span_since(&cursor),
      operation_type,
      name,
    ))
  }
);

definition_parser!(
  /// Parses a nonempty SDL root-operation-types definition.
  ///
  /// See the [GraphQL Root Operation Types Definition specification](https://spec.graphql.org/draft/#RootOperationTypesDefinition).
  pub root_operation_types_definition,
  inp,
  RootOperationTypesDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    root_operation_type_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| RootOperationTypesDefinition::new(span, data))
  }
);

fn scalar_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ScalarTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  let name = take_name(inp)?;
  let directives = optional_const_directives(inp)?;
  Ok(ScalarTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    directives,
  ))
}

fn object_after_keyword<'inp, Src, Ctx>(
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
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    implements,
    directives,
    fields_definition,
  ))
}

fn interface_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InterfaceTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  Ok(InterfaceTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    implements,
    directives,
    fields_definition,
  ))
}

fn union_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UnionTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  let name = take_name(inp)?;
  let directives = optional_const_directives(inp)?;
  let member_types = match try_union_members(inp)? {
    ParseAttempt::Accept(member_types) => Some(member_types),
    ParseAttempt::Decline => None,
  };
  Ok(UnionTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    directives,
    member_types,
  ))
}

fn enum_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<EnumTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  let name = take_name(inp)?;
  let directives = optional_const_directives(inp)?;
  let enum_values_definition = match try_enum_values_definition(inp)? {
    ParseAttempt::Accept(values) => Some(values),
    ParseAttempt::Decline => None,
  };
  Ok(EnumTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    directives,
    enum_values_definition,
  ))
}

fn input_object_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InputObjectTypeDefinition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  let name = take_name(inp)?;
  let directives = optional_const_directives(inp)?;
  let fields_definition = match try_input_fields_definition(inp)? {
    ParseAttempt::Accept(fields) => Some(fields),
    ParseAttempt::Decline => None,
  };
  Ok(InputObjectTypeDefinition::new(
    SimpleSpan::new(start, inp.span_since(&cursor).end()),
    name,
    directives,
    fields_definition,
  ))
}

definition_parser!(
  /// Parses a scalar type definition.
  ///
  /// See the [GraphQL Scalar Type Definition specification](https://spec.graphql.org/draft/#ScalarTypeDefinition).
  pub scalar_type_definition,
  inp,
  ScalarTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Scalar)?.start();
    scalar_after_keyword(inp, start)
  }
);

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

definition_parser!(
  /// Parses an interface type definition.
  ///
  /// See the [GraphQL Interface Type Definition specification](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  pub interface_type_definition,
  inp,
  InterfaceTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Interface)?.start();
    interface_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses a union type definition.
  ///
  /// See the [GraphQL Union Type Definition specification](https://spec.graphql.org/draft/#UnionTypeDefinition).
  pub union_type_definition,
  inp,
  UnionTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Union)?.start();
    union_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses an enum type definition.
  ///
  /// See the [GraphQL Enum Type Definition specification](https://spec.graphql.org/draft/#EnumTypeDefinition).
  pub enum_type_definition,
  inp,
  EnumTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Enum)?.start();
    enum_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses an input object type definition.
  ///
  /// See the [GraphQL Input Object Type Definition specification](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  pub input_object_type_definition,
  inp,
  InputObjectTypeDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Input)?.start();
    input_object_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses a directive definition.
  ///
  /// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition).
  pub directive_definition,
  inp,
  DirectiveDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Directive)?.start();
    guard_definition_phase(inp, Expectation::At, |token| token.is_at())?;
    at(inp)?;
    let name = take_name(inp)?;
    let arguments_definition = match try_arguments_definition(inp)? {
      ParseAttempt::Accept(arguments) => Some(arguments),
      ParseAttempt::Decline => None,
    };
    let repeatable = matches!(try_repeatable_keyword(inp)?, ParseAttempt::Accept(_));
    take_contextual_keyword(inp, ContextualKeyword::On)?;
    let locations = directive_locations(inp)?;
    Ok(DirectiveDefinition::new(
      SimpleSpan::new(start, locations.span().end()),
      name,
      arguments_definition,
      repeatable,
      locations,
    ))
  }
);

definition_parser!(
  /// Parses a schema definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  pub schema_definition,
  inp,
  SchemaDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Schema)?.start();
    let directives = optional_const_directives(inp)?;
    let root_operation_types_definition = root_operation_types_definition(inp)?;
    Ok(SchemaDefinition::new(
      SimpleSpan::new(start, root_operation_types_definition.span().end()),
      directives,
      root_operation_types_definition,
    ))
  }
);

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
        scalar_after_keyword(inp, start).map(TypeDefinition::Scalar)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        object_after_keyword(inp, start).map(TypeDefinition::Object)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        interface_after_keyword(inp, start).map(TypeDefinition::Interface)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        union_after_keyword(inp, start).map(TypeDefinition::Union)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        enum_after_keyword(inp, start).map(TypeDefinition::Enum)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        take_classified_type_definition_keyword(inp)?;
        input_object_after_keyword(inp, start).map(TypeDefinition::InputObject)
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

macro_rules! impl_definition_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, []) => {
    impl_definition_api!(@impl $(#[$meta])* $slice, $node, $parser, []);
  };
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [contextual]) => {
    impl_definition_api!(
      @impl
      $(#[$meta])*
      $slice,
      $node,
      $parser,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  (@impl $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
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

macro_rules! impl_definition_try_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, []) => {
    impl_definition_try_api!(@impl $(#[$meta])* $slice, $node, $parser, []);
  };
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [contextual]) => {
    impl_definition_try_api!(
      @impl
      $(#[$meta])*
      $slice,
      $node,
      $parser,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  (@impl $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn try_graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
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

impl_definition_api!(
  /// Parses an SDL input value definition.
  ///
  /// See the [GraphQL Input Value Definition specification](https://spec.graphql.org/draft/#InputValueDefinition).
  S,
  InputValueDefinition<S>,
  input_value_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL arguments definition.
  ///
  /// See the [GraphQL Arguments Definition specification](https://spec.graphql.org/draft/#ArgumentsDefinition).
  S,
  ArgumentsDefinition<S>,
  arguments_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL arguments definition without consuming when `(` is absent.
  ///
  /// See the [GraphQL Arguments Definition specification](https://spec.graphql.org/draft/#ArgumentsDefinition).
  S,
  ArgumentsDefinition<S>,
  try_arguments_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses an SDL field definition.
  ///
  /// See the [GraphQL Field Definition specification](https://spec.graphql.org/draft/#FieldDefinition).
  S,
  FieldDefinition<S>,
  field_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL fields definition.
  ///
  /// See the [GraphQL Fields Definition specification](https://spec.graphql.org/draft/#FieldsDefinition).
  S,
  FieldsDefinition<S>,
  fields_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL fields definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Fields Definition specification](https://spec.graphql.org/draft/#FieldsDefinition).
  S,
  FieldsDefinition<S>,
  try_fields_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL input-fields definition.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  S,
  InputFieldsDefinition<S>,
  input_fields_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL input-fields definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Input Fields Definition specification](https://spec.graphql.org/draft/#InputFieldsDefinition).
  S,
  InputFieldsDefinition<S>,
  try_input_fields_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses an SDL `implements` clause.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  S,
  ImplementInterfaces<Name<S>>,
  implements,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL `implements` clause without consuming when the keyword is absent.
  ///
  /// See the [GraphQL Implements Interfaces specification](https://spec.graphql.org/draft/#ImplementsInterfaces).
  S,
  ImplementInterfaces<Name<S>>,
  try_implements,
  [contextual]
);
impl_definition_api!(
  /// Parses an SDL union-members clause.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  S,
  UnionMemberTypes<Name<S>>,
  union_members,
  []
);
impl_definition_try_api!(
  /// Attempts an SDL union-members clause without consuming when `=` is absent.
  ///
  /// See the [GraphQL Union Member Types specification](https://spec.graphql.org/draft/#UnionMemberTypes).
  S,
  UnionMemberTypes<Name<S>>,
  try_union_members,
  []
);
impl_definition_api!(
  /// Parses an SDL enum value definition.
  ///
  /// See the [GraphQL Enum Value Definition specification](https://spec.graphql.org/draft/#EnumValueDefinition).
  S,
  EnumValueDefinition<S>,
  enum_value_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL enum-values definition.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  S,
  EnumValuesDefinition<S>,
  enum_values_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL enum-values definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Enum Values Definition specification](https://spec.graphql.org/draft/#EnumValuesDefinition).
  S,
  EnumValuesDefinition<S>,
  try_enum_values_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses an SDL root operation type definition.
  ///
  /// See the [GraphQL Root Operation Type Definition specification](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
  S,
  RootOperationTypeDefinition<S>,
  root_operation_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL root-operation-types definition.
  ///
  /// See the [GraphQL Root Operation Types Definition specification](https://spec.graphql.org/draft/#RootOperationTypesDefinition).
  S,
  RootOperationTypesDefinition<S>,
  root_operation_types_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a scalar type definition.
  ///
  /// See the [GraphQL Scalar Type Definition specification](https://spec.graphql.org/draft/#ScalarTypeDefinition).
  S,
  ScalarTypeDefinition<S>,
  scalar_type_definition,
  [contextual]
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
impl_definition_api!(
  /// Parses an interface type definition.
  ///
  /// See the [GraphQL Interface Type Definition specification](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  S,
  InterfaceTypeDefinition<S>,
  interface_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a union type definition.
  ///
  /// See the [GraphQL Union Type Definition specification](https://spec.graphql.org/draft/#UnionTypeDefinition).
  S,
  UnionTypeDefinition<S>,
  union_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses an enum type definition.
  ///
  /// See the [GraphQL Enum Type Definition specification](https://spec.graphql.org/draft/#EnumTypeDefinition).
  S,
  EnumTypeDefinition<S>,
  enum_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses an input object type definition.
  ///
  /// See the [GraphQL Input Object Type Definition specification](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  S,
  InputObjectTypeDefinition<S>,
  input_object_type_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a directive definition.
  ///
  /// See the [GraphQL Directive Definition specification](https://spec.graphql.org/draft/#DirectiveDefinition).
  S,
  DirectiveDefinition<S>,
  directive_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a schema definition.
  ///
  /// See the [GraphQL Schema Definition specification](https://spec.graphql.org/draft/#SchemaDefinition).
  S,
  SchemaDefinition<S>,
  schema_definition,
  [contextual]
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

impl DirectiveLocations<Location> {
  /// Parses a nonempty SDL directive-locations list.
  ///
  /// The lexer source is inferred from `inp`.
  /// See the [GraphQL Directive Locations specification](https://spec.graphql.org/draft/#DirectiveLocations).
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
    directive_locations(inp)
  }
}

impl Location {
  /// Parses one SDL directive location.
  ///
  /// The lexer source is inferred from `inp`.
  /// See the [GraphQL Directive Location specification](https://spec.graphql.org/draft/#DirectiveLocation).
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
    location(inp)
  }
}

#[cfg(test)]
mod tests;
