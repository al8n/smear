//! GraphQLx generic-header, type-path, and `where`-constraint productions.
//!
//! These productions assemble the generic AST carriers shared by GraphQLx SDL
//! and executable definitions. Optional suffixes inspect their opener once and
//! commit after it is accepted; `where` continuation uses deterministic
//! non-consuming path-plus-colon lookahead so a following definition remains
//! available to its caller.

use std::vec::Vec;

use tokora::{
  Accumulator, Lexer, ParseInput, SimpleSpan, Slice, Source, Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Angle, Bracket},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{
    DowncastRef,
    typenum::{U1, U2},
  },
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of, path,
  ty::try_type_generics, unexpected_here,
};
use crate::{
  combinator::{ParseCtx, ampersand, colon, try_equal},
  graphqlx::{
    GraphQLx,
    ast::{
      DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam, ExecutableDefinitionName,
      ExecutableDefinitionTypeGenerics, ExtensionName, ExtensionTypeGenerics, ExtensionTypeParam,
      TypePath, WhereClause, WherePredicate,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! generic_parser {
  (
    $(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty,
    token_bounds = [$($token_bounds:tt)*];
    error_bounds = [$($error_bounds:tt)*];
    $body:block
  ) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> $($token_bounds)*,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
        + From<
          UnexpectedToken<
            'inp,
            GraphqlxToken<'inp, Src>,
            <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
            SimpleSpan,
            GraphQLx,
          >,
        > $($error_bounds)*
        + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

fn expected_generic_phase<'inp, Src, Ctx, Output>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<Output, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  unexpected_here(inp, expected)
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
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match super::try_name(inp)? {
    ParseAttempt::Accept(name) => Ok(name),
    ParseAttempt::Decline => expected_generic_phase(inp, Expectation::Name),
  }
}

pub(crate) fn try_where<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<SimpleSpan>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(
    match inp.try_expect_map(|token| {
      (keyword_of(token.data()) == Some(ContextualKeyword::Where)).then_some(())
    })? {
      Some((_, token)) => ParseAttempt::Accept(token.span()),
      None => ParseAttempt::Decline,
    },
  )
}

fn decide_angle_member<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RAngle) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_langle_opener<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if token.token().is_l_angle() => Action::Continue,
    _ => Action::Stop,
  })
}

fn decide_ampersand_tail<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if token.token().is_ampersand() => Action::Continue,
    _ => Action::Stop,
  })
}

fn try_definition_type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<DefinitionTypeGenerics<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  definition_type_generics
    .peek_then_try::<_, U1>(decide_langle_opener::<Src, Ctx>)
    .try_parse_input(inp)
    .map(Into::into)
}

fn try_extension_type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<ExtensionTypeGenerics<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  extension_type_generics
    .peek_then_try::<_, U1>(decide_langle_opener::<Src, Ctx>)
    .try_parse_input(inp)
    .map(Into::into)
}

pub(crate) fn try_executable_definition_type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<ExecutableDefinitionTypeGenerics<GraphqlxSlice<'inp, Src>>>,
  GraphqlxError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  executable_definition_type_generics
    .peek_then_try::<_, U1>(decide_langle_opener::<Src, Ctx>)
    .try_parse_input(inp)
    .map(Into::into)
}

generic_parser!(
  /// Parses one committed GraphQLx definition generic parameter.
  ///
  /// The parameter is a name with an optional `= Type` default.
  pub definition_type_param,
  inp,
  DefinitionTypeParam<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    let cursor = *inp.cursor();
    let name = take_name(inp)?;
    let default = match try_equal(inp)? {
      ParseAttempt::Accept(_) => Some(super::ty::ty(inp)?),
      ParseAttempt::Decline => None,
    };
    Ok(DefinitionTypeParam::new(
      inp.span_since(&cursor),
      name,
      default,
    ))
  }
);

generic_parser!(
  /// Parses one nonempty angle-delimited GraphQLx definition generic list.
  pub definition_type_generics,
  inp,
  DefinitionTypeGenerics<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| definition_type_param(inp))
      .repeated_while::<_, U1>(decide_angle_member::<Src, Ctx>)
      .at_least(1)
      .delimited_by_angles()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| DefinitionTypeGenerics::new(span, data))
  }
);

generic_parser!(
  /// Parses one committed GraphQLx extension generic argument.
  pub extension_type_param,
  inp,
  ExtensionTypeParam<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [];
  {
    let name = take_name(inp)?;
    Ok(ExtensionTypeParam::new(name.span(), name))
  }
);

generic_parser!(
  /// Parses one nonempty angle-delimited GraphQLx extension generic list.
  pub extension_type_generics,
  inp,
  ExtensionTypeGenerics<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
  {
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| extension_type_param(inp))
      .repeated_while::<_, U1>(decide_angle_member::<Src, Ctx>)
      .at_least(1)
      .delimited_by_angles()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ExtensionTypeGenerics::new(span, data))
  }
);

generic_parser!(
  /// Parses one nonempty angle-delimited GraphQLx executable generic list.
  pub executable_definition_type_generics,
  inp,
  ExecutableDefinitionTypeGenerics<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
  {
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| take_name(inp))
      .repeated_while::<_, U1>(decide_angle_member::<Src, Ctx>)
      .at_least(1)
      .delimited_by_angles()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ExecutableDefinitionTypeGenerics::new(span, data))
  }
);

generic_parser!(
  /// Attempts a GraphQLx definition name without consuming on a non-name head.
  pub(crate) try_definition_name,
  inp,
  ParseAttempt<DefinitionName<GraphqlxSlice<'inp, Src>>>,
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    let name = match super::try_name(inp)? {
      ParseAttempt::Accept(name) => name,
      ParseAttempt::Decline => return Ok(ParseAttempt::Decline),
    };
    let generics = try_definition_type_generics(inp)?;
    let end = generics.as_ref().map_or_else(|| name.span().end(), |generics| generics.span().end());
    Ok(ParseAttempt::Accept(DefinitionName::new(
      SimpleSpan::new(name.span().start(), end),
      name,
      generics,
    )))
  }
);

generic_parser!(
  /// Parses a committed GraphQLx definition name with optional declared generic parameters.
  pub definition_name,
  inp,
  DefinitionName<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    match try_definition_name(inp)? {
      ParseAttempt::Accept(name) => Ok(name),
      ParseAttempt::Decline => expected_generic_phase(inp, Expectation::Name),
    }
  }
);

generic_parser!(
  /// Parses a GraphQLx extension path with optional generic arguments.
  pub extension_name,
  inp,
  ExtensionName<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
  {
    let cursor = *inp.cursor();
    let path = path(inp)?;
    let generics = try_extension_type_generics(inp)?;
    Ok(ExtensionName::new(inp.span_since(&cursor), path, generics))
  }
);

generic_parser!(
  /// Parses a GraphQLx executable definition name with optional generic names.
  pub executable_definition_name,
  inp,
  ExecutableDefinitionName<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
  {
    let cursor = *inp.cursor();
    let name = take_name(inp)?;
    let generics = try_executable_definition_type_generics(inp)?;
    Ok(ExecutableDefinitionName::new(
      inp.span_since(&cursor),
      name,
      generics,
    ))
  }
);

generic_parser!(
  /// Parses a GraphQLx path followed by optional recursive type arguments.
  pub type_path,
  inp,
  TypePath<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    let cursor = *inp.cursor();
    let path = path(inp)?;
    let generics = try_type_generics(inp)?;
    Ok(TypePath::new(inp.span_since(&cursor), path, generics))
  }
);

generic_parser!(
  /// Parses one committed GraphQLx `where` predicate.
  ///
  /// A predicate is `TypePath ':' TypePath ('&' TypePath)*`.
  pub where_predicate,
  inp,
  WherePredicate<GraphqlxSlice<'inp, Src>>,
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    let bounded_type = type_path(inp)?;
    colon(inp)?;
    let first = type_path(inp)?;
    let bounds: Vec<TypePath<GraphqlxSlice<'inp, Src>>> = ampersand
      .ignore_then(type_path)
      .repeated_while::<_, U1>(decide_ampersand_tail::<Src, Ctx>)
      .collect_with(Vec::from([first]))
      .parse_input(inp)?;
    let end = bounds
      .last()
      .expect("where predicates contain their first bound")
      .span()
      .end();
    Ok(WherePredicate::new(
      SimpleSpan::new(bounded_type.span().start(), end),
      bounded_type,
      bounds,
    ))
  }
);

fn predicate_head_in_window<'inp, Src>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U2>,
) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
{
  let first = peeked.pop_front().map(|token| token.token().kind());
  let second = peeked.pop_front().map(|token| token.token().kind());
  matches!(
    (first, second),
    (
      Some(SyntacticTokenKind::Identifier),
      Some(
        SyntacticTokenKind::Colon | SyntacticTokenKind::PathSeparator | SyntacticTokenKind::LAngle
      )
    ) | (
      Some(SyntacticTokenKind::PathSeparator),
      Some(SyntacticTokenKind::Identifier)
    )
  )
}

fn decide_where_predicate_tail<'inp, Src, Ctx>(
  peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U2>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(if predicate_head_in_window(peeked) {
    Action::Continue
  } else {
    Action::Stop
  })
}

/// Enters a GraphQLx `where` clause after its keyword has been consumed.
///
/// This tail is shared by committed and optional callers, so accepting
/// `where` never requires a second classification of the same token.
pub(crate) fn where_clause_after_where<'inp, Src, Ctx>(
  where_span: SimpleSpan,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<WhereClause<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let first = where_predicate(inp)?;
  let predicates: Vec<WherePredicate<GraphqlxSlice<'inp, Src>>> = where_predicate
    .repeated_while::<_, U2>(decide_where_predicate_tail::<Src, Ctx>)
    .collect_with(Vec::from([first]))
    .parse_input(inp)?;
  let end = predicates
    .last()
    .expect("a GraphQLx where clause has one predicate")
    .span()
    .end();
  Ok(WhereClause::new(
    SimpleSpan::new(where_span.start(), end),
    predicates,
  ))
}

generic_parser!(
  /// Attempts a GraphQLx `where` clause without consuming on a non-`where` head.
  pub(crate) try_where_clause,
  inp,
  ParseAttempt<WhereClause<GraphqlxSlice<'inp, Src>>>,
  token_bounds = [+ DowncastRef<ContextualKeyword>];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    match try_where(inp)? {
      ParseAttempt::Accept(where_span) => {
        where_clause_after_where(where_span, inp).map(ParseAttempt::Accept)
      }
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

generic_parser!(
  /// Parses a committed GraphQLx `where` clause with one or more predicates.
  ///
  /// Continuation recognizes only a structural type-path prefix before parsing,
  /// leaving the next ordinary definition untouched.
  pub where_clause,
  inp,
  WhereClause<GraphqlxSlice<'inp, Src>>,
  token_bounds = [+ DowncastRef<ContextualKeyword>];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
  {
    match try_where_clause(inp)? {
      ParseAttempt::Accept(where_clause) => Ok(where_clause),
      ParseAttempt::Decline => expected_generic_phase(inp, Expectation::Keyword("where")),
    }
  }
);

macro_rules! impl_generic_api {
  (
    $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident;
    token_bounds = [$($token_bounds:tt)*];
    error_bounds = [$($error_bounds:tt)*];
  ) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> $($token_bounds)*,
        GraphqlxLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlxToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLx,
            >,
          > $($error_bounds)*
          + From<DialectGraphqlxError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_generic_api!(
  /// Parses one committed GraphQLx definition generic parameter.
  S,
  DefinitionTypeParam<S>,
  definition_type_param;
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
);
impl_generic_api!(
  /// Parses one nonempty angle-delimited GraphQLx definition generic list.
  S,
  DefinitionTypeGenerics<S>,
  definition_type_generics;
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
);
impl_generic_api!(
  /// Parses one committed GraphQLx extension generic argument.
  S,
  ExtensionTypeParam<S>,
  extension_type_param;
  token_bounds = [];
  error_bounds = [];
);
impl_generic_api!(
  /// Parses one nonempty angle-delimited GraphQLx extension generic list.
  S,
  ExtensionTypeGenerics<S>,
  extension_type_generics;
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
);
impl_generic_api!(
  /// Parses one nonempty angle-delimited GraphQLx executable generic list.
  S,
  ExecutableDefinitionTypeGenerics<S>,
  executable_definition_type_generics;
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
);
impl_generic_api!(
  /// Parses a GraphQLx definition name with optional declared generic parameters.
  S,
  DefinitionName<S>,
  definition_name;
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
);
impl_generic_api!(
  /// Parses a GraphQLx extension path with optional generic arguments.
  S,
  ExtensionName<S>,
  extension_name;
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
);
impl_generic_api!(
  /// Parses a GraphQLx executable definition name with optional generic names.
  S,
  ExecutableDefinitionName<S>,
  executable_definition_name;
  token_bounds = [];
  error_bounds = [+ From<Unclosed<Angle, SimpleSpan, GraphQLx>>];
);
impl_generic_api!(
  /// Parses a GraphQLx path followed by optional recursive type arguments.
  S,
  TypePath<S>,
  type_path;
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
);
impl_generic_api!(
  /// Parses one committed GraphQLx `where` predicate.
  S,
  WherePredicate<S>,
  where_predicate;
  token_bounds = [];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
);
impl_generic_api!(
  /// Parses one committed GraphQLx `where` clause.
  S,
  WhereClause<S>,
  where_clause;
  token_bounds = [+ DowncastRef<ContextualKeyword>];
  error_bounds = [
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
  ];
);

#[cfg(test)]
mod tests;
