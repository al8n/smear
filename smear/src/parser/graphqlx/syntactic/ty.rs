//! GraphQLx recursive type-reference productions.
//!
//! GraphQLx extends GraphQL's named/list references with paths, generic type
//! arguments, set types (`<T>`), and map types (`<K => V>`). A trailing `!`
//! is folded into the node it immediately follows.
//!
//! See the [GraphQL Type References specification](https://spec.graphql.org/draft/#sec-Type-References).

use std::{boxed::Box, vec::Vec};
use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::typenum::U1,
};

use crate::lexer::graphqlx::syntactic::SyntacticTokenKind;

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, path_after_first,
  peek_kind, unexpected_here,
};
use crate::parser::{
  combinator::{ParseCtx, TokenSpannedExt, extent_since, extent_start, try_bang, try_fat_arrow},
  graphqlx::{
    GraphQLx,
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
    Some(token) if type_head_kind(token.token().kind()) => Action::Continue,
    _ => Action::Stop,
  })
}

/// Parses one nonempty angle-delimited GraphQLx type-argument list.
///
/// This helper is shared by GraphQLx productions that attach optional type
/// arguments to a path, rather than duplicating recursive type parsing.
pub(crate) fn type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeGenerics<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| ty(inp))
    .repeated_while::<_, U1>(decide_type_head::<_, Ctx>)
    .at_least(1)
    .delimited_by_angles()
    .collect_with(Vec::new())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| TypeGenerics::new(span, data))
}

fn decide_type_generics_opener<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::LAngle) => Action::Continue,
    _ => Action::Stop,
  })
}

/// Attempts an optional GraphQLx type-argument list without consuming when
/// the next token is not an opening angle bracket.
pub(crate) fn try_type_generics<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<TypeGenerics<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  type_generics
    .peek_then_try::<_, U1>(decide_type_generics_opener::<Src, Ctx>)
    .try_parse_input(inp)
    .map(|attempt| match attempt {
      ParseAttempt::Accept(generics) => Some(generics),
      ParseAttempt::Decline => None,
    })
}

fn path_type_after_path<'inp, Src, Ctx>(
  path: super::ast::Path<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<TypeCore<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
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
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
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

/// Parses a committed recursive GraphQLx type reference.
///
/// GraphQLx retains GraphQL's named/list references and trailing `!`, while
/// extending named types to paths with optional type arguments and adding set
/// (`<T>`) and map (`<K => V>`) references.
///
/// See the [GraphQL Type References specification](https://spec.graphql.org/draft/#sec-Type-References).
pub fn ty<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Type<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let node_start = extent_start(inp)?;
  let identifier_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Identifier(source) => {
        let first = super::ast::Name::new(span, source);
        let path = path_after_first(span.start(), first, false, inp)?;
        path_type_after_path(path, inp)
      }
      _ => unreachable!("fused GraphQLx type dispatch received a non-identifier token"),
    };
  let path_separator_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::PathSeparator => {
        let first = super::name(inp)?;
        let path = path_after_first(span.start(), first, true, inp)?;
        path_type_after_path(path, inp)
      }
      _ => unreachable!("fused GraphQLx type dispatch received a non-path-separator token"),
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
      _ => return unexpected_here(inp, Expectation::Type),
    },
  };
  let required = matches!(try_bang(inp)?, ParseAttempt::Accept(_));
  let span = extent_since(inp, node_start);
  Ok(match core {
    TypeCore::Path(path, generics) => {
      Type::Path(DefinitionTypePath::new(span, path, generics, required))
    }
    TypeCore::List(element) => Type::List(Box::new(crate::parser::ty::ListType::new(
      span, element, required,
    ))),
    TypeCore::Set(element) => Type::Set(Box::new(crate::parser::ty::SetType::new(
      span, element, required,
    ))),
    TypeCore::Map(key, value) => Type::Map(Box::new(crate::parser::ty::MapType::new(
      span, key, value, required,
    ))),
  })
}

impl<S> Type<S> {
  /// Parses one committed GraphQLx type reference.
  ///
  /// The parser follows GraphQL's type-reference structure, extended with
  /// GraphQLx paths, generic type arguments, set types, and map types.
  ///
  /// See [`ty`] and the [GraphQL Type References specification](https://spec.graphql.org/draft/#sec-Type-References).
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    ty(inp)
  }
}
