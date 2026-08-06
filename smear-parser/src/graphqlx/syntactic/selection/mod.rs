//! GraphQLx selection productions.
//!
//! Selection dispatch consumes a field or spread head exactly once. A spread
//! then classifies its second token into a named spread, typed inline fragment,
//! or untyped inline fragment before entering the committed recursive tail.

use std::vec::Vec;

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};
use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, argument::arguments,
  directive::directives, keyword_of, path_after_first, ty::try_type_generics, unexpected_here,
};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, try_colon, try_spread},
  graphqlx::{
    GraphQLx,
    ast::{
      Alias, Directives, Field, FragmentSpread, InlineFragment, Name, Selection, SelectionSet,
      TypeCondition, TypePath,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! selection_parser {
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

fn take_name<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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

fn take_spread<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_spread(inp)? {
    ParseAttempt::Accept(spread) => Ok(*spread.span()),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Spread),
  }
}

fn try_on<'inp, Src, Ctx>(
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
      (keyword_of(token.data()) == Some(ContextualKeyword::On)).then_some(())
    })? {
      Some((_, token)) => ParseAttempt::Accept(token.span()),
      None => ParseAttempt::Decline,
    },
  )
}

fn take_on<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_on(inp)? {
    ParseAttempt::Accept(span) => Ok(span),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Keyword("on")),
  }
}

fn type_path_after_first<'inp, Src, Ctx>(
  start: usize,
  first: Name<GraphqlxSlice<'inp, Src>>,
  fully_qualified: bool,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypePath<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let path = path_after_first(start, first, fully_qualified, inp)?;
  let end = path.span().end();
  let generics = try_type_generics(inp)?;
  let end = generics
    .as_ref()
    .map_or(end, |generics| generics.span().end());
  Ok(TypePath::new(SimpleSpan::new(start, end), path, generics))
}

fn type_condition_after_on<'inp, Src, Ctx>(
  on: SimpleSpan,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCondition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let type_path = super::generic::type_path(inp)?;
  Ok(TypeCondition::new(
    SimpleSpan::new(on.start(), type_path.span().end()),
    type_path,
  ))
}

selection_parser!(
  /// Parses a committed GraphQLx type condition (`on TypePath`).
  ///
  /// GraphQLx extends the standard named-type position with a qualified,
  /// generic-capable [`TypePath`].
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  pub type_condition,
  inp,
  TypeCondition<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    take_on
      .ignore_then(super::generic::type_path)
      .token_spanned()
      .map(|Spanned { span, data }| TypeCondition::new(span, data))
      .parse_input(inp)
  }
);

selection_parser!(
  /// Attempts a GraphQLx type condition without consuming a non-`on` head.
  ///
  /// Once `on` is consumed, parsing commits to the GraphQLx [`TypePath`]
  /// extension of the standard type-condition grammar.
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  pub try_type_condition,
  inp,
  ParseAttempt<TypeCondition<GraphqlxSlice<'inp, Src>>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    match try_on(inp)? {
      ParseAttempt::Accept(on) => type_condition_after_on(on, inp).map(ParseAttempt::Accept),
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

fn field_after_name<'inp, Src, Ctx>(
  first_name: Name<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Field<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let start = first_name.span().start();
  let (alias, name) = match try_colon(inp)? {
    ParseAttempt::Accept(colon) => {
      let alias = Alias::new(SimpleSpan::new(start, colon.span().end()), first_name);
      (Some(alias), take_name(inp)?)
    }
    ParseAttempt::Decline => (None, first_name),
  };

  let mut end = name.span().end();
  let arguments = arguments(inp)?;
  if arguments.span().start() != arguments.span().end() {
    end = arguments.span().end();
  }
  let arguments = (!arguments.arguments().is_empty()).then_some(arguments);

  let directives = directives(inp)?;
  if directives.span().start() != directives.span().end() {
    end = directives.span().end();
  }
  let directives = (!directives.directives().is_empty()).then_some(directives);

  let selection_set = match try_selection_set(inp)? {
    ParseAttempt::Accept(selection_set) => {
      end = selection_set.span().end();
      Some(selection_set)
    }
    ParseAttempt::Decline => None,
  };

  Ok(Field::new(
    SimpleSpan::new(start, end),
    alias,
    name,
    arguments,
    directives,
    selection_set,
  ))
}

selection_parser!(
  /// Parses a GraphQLx field (`Alias? Name Arguments? Directives? SelectionSet?`).
  ///
  /// The field structure follows GraphQL; nested arguments, directives, and
  /// selection sets use their GraphQLx productions.
  ///
  /// See the [GraphQL Field specification](https://spec.graphql.org/draft/#Field).
  pub field,
  inp,
  Field<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let name = take_name(inp)?;
    field_after_name(name, inp)
  }
);

fn fragment_spread_after_path<'inp, Src, Ctx>(
  start: usize,
  path: TypePath<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<FragmentSpread<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let mut end = path.span().end();
  let directives = directives(inp)?;
  if directives.span().start() != directives.span().end() {
    end = directives.span().end();
  }
  let directives = (!directives.directives().is_empty()).then_some(directives);
  Ok(FragmentSpread::new(
    SimpleSpan::new(start, end),
    path,
    directives,
  ))
}

fn inline_fragment_after_directives<'inp, Src, Ctx>(
  start: usize,
  type_condition: Option<TypeCondition<GraphqlxSlice<'inp, Src>>>,
  directives: Directives<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<InlineFragment<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let directives = (!directives.directives().is_empty()).then_some(directives);
  let selection_set = selection_set(inp)?;
  Ok(InlineFragment::new(
    SimpleSpan::new(start, selection_set.span().end()),
    type_condition,
    directives,
    selection_set,
  ))
}

fn inline_fragment_after_type_condition<'inp, Src, Ctx>(
  start: usize,
  type_condition: TypeCondition<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<InlineFragment<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let directives = directives(inp)?;
  inline_fragment_after_directives(start, Some(type_condition), directives, inp)
}

fn untyped_inline_fragment_after_spread<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<InlineFragment<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  let directives = directives(inp)?;
  inline_fragment_after_directives(start, None, directives, inp)
}

fn fragment_type_path_after_separator<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypePath<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let first = take_name(inp)?;
  type_path_after_first(start, first, true, inp)
}

fn selection_after_spread<'inp, Src, Ctx>(
  start: usize,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Selection<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  match inp.try_expect_map(|token| {
    matches!(token.data(), GraphqlxToken::<'inp, Src>::Identifier(_))
      .then(|| keyword_of(token.data()) == Some(ContextualKeyword::On))
  })? {
    Some((true, Spanned { span, .. })) => type_condition_after_on(span, inp)
      .and_then(|condition| inline_fragment_after_type_condition(start, condition, inp))
      .map(Selection::InlineFragment),
    Some((
      false,
      Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::Identifier(source),
      },
    )) => {
      let path = type_path_after_first(span.start(), Name::new(span, source), false, inp)?;
      fragment_spread_after_path(start, path, inp).map(Selection::FragmentSpread)
    }
    Some(_) => unreachable!("identifier selection consumed a non-identifier token"),
    None => match super::peek_kind(inp)? {
      Some(SyntacticTokenKind::PathSeparator) => match inp.next()? {
        Some(Spanned {
          span,
          data: GraphqlxToken::<'inp, Src>::PathSeparator,
        }) => {
          let path = fragment_type_path_after_separator(span.start(), inp)?;
          fragment_spread_after_path(start, path, inp).map(Selection::FragmentSpread)
        }
        Some(Spanned { span, data: token }) => {
          Err(DialectGraphqlxError::unexpected_token(token.kind(), Expectation::Path, span).into())
        }
        None => unexpected_here(inp, Expectation::Path),
      },
      Some(SyntacticTokenKind::At | SyntacticTokenKind::LBrace) => {
        untyped_inline_fragment_after_spread(start, inp).map(Selection::InlineFragment)
      }
      _ => unexpected_here(inp, Expectation::Path),
    },
  }
}

fn try_selection<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<Selection<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
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
  let field_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Identifier(source) => {
        field_after_name(Name::new(span, source), inp).map(Selection::Field)
      }
      _ => unreachable!("fused GraphQLx field arm received a non-identifier token"),
    };
  let spread_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Spread => selection_after_spread(span.start(), inp),
      _ => unreachable!("fused GraphQLx spread arm received a non-spread token"),
    };

  (field_head, spread_head)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier, SyntacticTokenKind::Spread])
    .try_parse_input(inp)
}

selection_parser!(
  /// Parses a GraphQLx field, named fragment spread, or inline fragment.
  ///
  /// Deterministic dispatch consumes the field or spread head once. GraphQLx
  /// fragment branches additionally support qualified and generic type paths.
  ///
  /// See the [GraphQL Selection specification](https://spec.graphql.org/draft/#Selection).
  pub selection,
  inp,
  Selection<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    match try_selection(inp)? {
      ParseAttempt::Accept(selection) => Ok(selection),
      ParseAttempt::Decline => unexpected_here(inp, Expectation::Name),
    }
  }
);

fn fragment_type_path<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypePath<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
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
  match inp.try_expect_map(|token| {
    matches!(token.data(), GraphqlxToken::<'inp, Src>::Identifier(_))
      .then(|| keyword_of(token.data()) == Some(ContextualKeyword::On))
  })? {
    Some((true, Spanned { span, data: token })) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), Expectation::Path, span).into())
    }
    Some((
      false,
      Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::Identifier(source),
      },
    )) => type_path_after_first(span.start(), Name::new(span, source), false, inp),
    Some(_) => unreachable!("fragment-path identifier probe consumed a non-identifier token"),
    None => match inp.next()? {
      Some(Spanned {
        span,
        data: GraphqlxToken::<'inp, Src>::PathSeparator,
      }) => fragment_type_path_after_separator(span.start(), inp),
      Some(Spanned { span, data: token }) => {
        Err(DialectGraphqlxError::unexpected_token(token.kind(), Expectation::Path, span).into())
      }
      None => unexpected_here(inp, Expectation::Path),
    },
  }
}

selection_parser!(
  /// Parses a committed GraphQLx named fragment spread.
  ///
  /// GraphQLx extends the fragment-name position to a qualified,
  /// generic-capable [`TypePath`].
  ///
  /// See the [GraphQL Fragment Spread specification](https://spec.graphql.org/draft/#FragmentSpread).
  pub fragment_spread,
  inp,
  FragmentSpread<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let spread = take_spread(inp)?;
    let path = fragment_type_path(inp)?;
    fragment_spread_after_path(spread.start(), path, inp)
  }
);

selection_parser!(
  /// Parses a committed GraphQLx inline fragment.
  ///
  /// Its optional type condition uses GraphQLx [`TypePath`] syntax rather
  /// than only the standard named type.
  ///
  /// See the [GraphQL Inline Fragment specification](https://spec.graphql.org/draft/#InlineFragment).
  pub inline_fragment,
  inp,
  InlineFragment<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    let spread = take_spread(inp)?;
    match try_on(inp)? {
      ParseAttempt::Accept(on) => {
        let condition = type_condition_after_on(on, inp)?;
        inline_fragment_after_type_condition(spread.start(), condition, inp)
      }
      ParseAttempt::Decline => untyped_inline_fragment_after_spread(spread.start(), inp),
    }
  }
);

/// Continues through every token so the committed [`selection`] parser emits
/// its local diagnostic for an invalid collection item. The delimited
/// repetition probes `}` before consulting this decision.
fn decide_selection_set_tail<'inp, Src, Ctx>(
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
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

/// Continues an optional selection-set attempt only when its `{` opener is
/// present, leaving every other token for the caller.
fn decide_selection_set_opener<'inp, Src, Ctx>(
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
    Some(token) if token.token().is_l_brace() => Action::Continue,
    _ => Action::Stop,
  })
}

selection_parser!(
  committed_selection_set,
  inp,
  SelectionSet<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  {
    selection
      .repeated_while::<_, U1>(decide_selection_set_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| SelectionSet::new(span, data))
  }
);

fn try_selection_set<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<SelectionSet<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
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
  committed_selection_set
    .peek_then_try::<_, U1>(decide_selection_set_opener::<Src, Ctx>)
    .try_parse_input(inp)
}

selection_parser!(
  /// Parses a nonempty GraphQLx selection set (`{ Selection+ }`).
  ///
  /// The collection is committed after `{`; it requires at least one
  /// selection and preserves the delimiter's `Unclosed<Brace>` diagnostic.
  ///
  /// See the [GraphQL Selection Sets specification](https://spec.graphql.org/draft/#SelectionSet).
  pub selection_set,
  inp,
  SelectionSet<GraphqlxSlice<'inp, Src>>,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
  { committed_selection_set(inp) }
);

macro_rules! impl_selection_api {
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

macro_rules! impl_selection_try_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
    impl<$slice> $node {
      $(#[$meta])*
      /// Attempts this GraphQLx production without consuming a head mismatch.
      pub fn try_graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<ParseAttempt<Self>, GraphqlxError<'inp, Src, Ctx>>
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

impl_selection_api!(
  /// Parses a committed GraphQLx field.
  ///
  /// See the [GraphQL Field specification](https://spec.graphql.org/draft/#Field).
  S,
  Field<S>,
  field,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_selection_api!(
  /// Parses a committed GraphQLx type condition.
  ///
  /// GraphQLx accepts a qualified, generic-capable type path after `on`.
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  S,
  TypeCondition<S>,
  type_condition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_selection_try_api!(
  /// Attempts a GraphQLx type condition.
  ///
  /// Declines without consuming unless the head is `on`.
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  S,
  TypeCondition<S>,
  try_type_condition,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_selection_api!(
  /// Parses a committed GraphQLx fragment spread.
  ///
  /// GraphQLx permits a qualified, generic-capable fragment type path.
  ///
  /// See the [GraphQL Fragment Spread specification](https://spec.graphql.org/draft/#FragmentSpread).
  S,
  FragmentSpread<S>,
  fragment_spread,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_selection_api!(
  /// Parses a committed GraphQLx inline fragment.
  ///
  /// See the [GraphQL Inline Fragment specification](https://spec.graphql.org/draft/#InlineFragment).
  S,
  InlineFragment<S>,
  inline_fragment,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_selection_api!(
  /// Parses a committed GraphQLx selection.
  ///
  /// See the [GraphQL Selection specification](https://spec.graphql.org/draft/#Selection).
  S,
  Selection<S>,
  selection,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);
impl_selection_api!(
  /// Parses a nonempty GraphQLx selection set.
  ///
  /// See the [GraphQL Selection Sets specification](https://spec.graphql.org/draft/#SelectionSet).
  S,
  SelectionSet<S>,
  selection_set,
  [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
);

#[cfg(test)]
mod tests;
