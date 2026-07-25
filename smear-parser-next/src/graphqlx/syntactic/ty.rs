//! GraphQLX recursive type-reference productions.
//!
//! GraphQLX extends GraphQL's named/list references with paths, generic type
//! arguments, set types (`<T>`), and map types (`<K => V>`). A trailing `!`
//! is folded into the node it immediately follows.

use std::{boxed::Box, vec::Vec};
use tokora::{
  Accumulator, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Angle, Bracket},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::typenum::U1,
};

use smear_lexer::graphqlx::syntactic::SyntacticTokenKind;

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, next_is,
  path_after_first, peek_kind, unexpected_here,
};
use crate::{
  combinator::{ParseCtx, try_bang, try_fat_arrow},
  graphqlx::{
    GraphQLX,
    ast::{DefinitionTypePath, Type, TypeGenerics},
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

enum TypeCore<S> {
  Path(super::ast::Path<S>, Option<TypeGenerics<S>>),
  List(Type<S>),
  Set(Type<S>),
  Map(Type<S>, Type<S>),
}

fn type_head_kind(kind: SyntacticTokenKind) -> bool {
  matches!(
    kind,
    SyntacticTokenKind::Identifier
      | SyntacticTokenKind::PathSeparator
      | SyntacticTokenKind::LBracket
      | SyntacticTokenKind::LAngle
  )
}

fn decide_type_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
{
  Ok(match peeked.pop_front() {
    Some(token) if type_head_kind(token.token().kind()) => Action::Continue,
    _ => Action::Stop,
  })
}

fn type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeGenerics<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| ty(inp))
    .repeated_while::<_, U1>(decide_type_head::<_, Ctx>)
    .at_least(1)
    .delimited_by_angles()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| TypeGenerics::new(span, data))
}

fn try_type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<TypeGenerics<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  if next_is(inp, |token| {
    matches!(token, GraphqlxToken::<'inp, Src>::LAngle)
  })? {
    type_generics(inp).map(Some)
  } else {
    Ok(None)
  }
}

fn path_type_after_path<'inp, Src, Ctx>(
  path: super::ast::Path<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCore<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let generics = try_type_generics(inp)?;
  Ok(TypeCore::Path(path, generics))
}

fn list_type_core<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCore<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| ty(inp))
    .delimited_by_brackets()
    .parse_input(inp)
    .map(|delimited| TypeCore::List(delimited.into_data()))
}

fn angle_type_core<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCore<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
    let key_or_element = ty(inp)?;
    match try_fat_arrow(inp)? {
      ParseAttempt::Accept(_) => ty(inp).map(|value| TypeCore::Map(key_or_element, value)),
      ParseAttempt::Decline => Ok(TypeCore::Set(key_or_element)),
    }
  })
  .delimited_by_angles()
  .parse_input(inp)
  .map(|delimited| delimited.into_data())
}

/// Parses a committed recursive GraphQLX type reference.
pub fn ty<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Type<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>:
    Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLX,
      >,
    > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let cursor = *inp.cursor();
  let identifier_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Identifier(source) => {
        let first = super::ast::Name::new(span, source);
        let path = path_after_first(span.start(), first, false, inp)?;
        path_type_after_path(path, inp)
      }
      _ => unreachable!("fused GraphQLX type dispatch received a non-identifier token"),
    };
  let path_separator_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::PathSeparator => {
        let first = super::name(inp)?;
        let path = path_after_first(span.start(), first, true, inp)?;
        path_type_after_path(path, inp)
      }
      _ => unreachable!("fused GraphQLX type dispatch received a non-path-separator token"),
    };
  let core = match (identifier_head, path_separator_head)
    .fused_dispatch_on_kind(&[
      SyntacticTokenKind::Identifier,
      SyntacticTokenKind::PathSeparator,
    ])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(core) => core,
    ParseAttempt::Decline => match peek_kind(inp)? {
      Some(SyntacticTokenKind::LBracket) => list_type_core(inp)?,
      Some(SyntacticTokenKind::LAngle) => angle_type_core(inp)?,
      _ => {
        unexpected_here(inp, Expectation::Type)?;
        unreachable!("unexpected_here always returns an error")
      }
    },
  };
  let required = matches!(try_bang(inp)?, ParseAttempt::Accept(_));
  let span = inp.span_since(&cursor);
  Ok(match core {
    TypeCore::Path(path, generics) => {
      Type::Path(DefinitionTypePath::new(span, path, generics, required))
    }
    TypeCore::List(element) => {
      Type::List(Box::new(crate::ty::ListType::new(span, element, required)))
    }
    TypeCore::Set(element) => Type::Set(Box::new(crate::ty::SetType::new(span, element, required))),
    TypeCore::Map(key, value) => Type::Map(Box::new(crate::ty::MapType::new(
      span, key, value, required,
    ))),
  })
}

impl<S> Type<S> {
  /// Parses one committed GraphQLX type reference.
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxToken<'inp, Src>:
      Token<'inp, Kind = SyntacticTokenKind> + tokora::token::PunctuatorToken<'inp>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLX>,
    GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLX>>
      + From<
        UnexpectedToken<
          'inp,
          GraphqlxToken<'inp, Src>,
          <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
          SimpleSpan,
          GraphQLX,
        >,
      > + From<Unclosed<Angle, SimpleSpan, GraphQLX>>
      + From<Unclosed<Bracket, SimpleSpan, GraphQLX>>
      + From<DialectGraphqlxError<S>>,
  {
    ty(inp)
  }
}
