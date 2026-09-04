//! GraphQLx import productions.
//!
//! Imports are GraphQLx dialect productions: `import` accepts a wildcard or a
//! nonempty braced member list, an optional `as` path, and a `from` inline
//! string source. They have no corresponding GraphQL draft production.

use std::vec::Vec;
use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of,
  unexpected_here,
};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, asterisk},
  graphqlx::{
    GraphQLx,
    ast::{
      ImportClause, ImportDefinition, ImportList, ImportMember, NamedSpecifier, WildcardSpecifier,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! import_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    #[doc = "Parses this committed GraphQLx import production."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
        + DowncastRef<ContextualKeyword>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

fn contextual_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  expected: Expectation,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next_or_stop()? {
    Some(Spanned { span, data: token }) if keyword_of(&token) == Some(keyword) => Ok(span),
    Some(Spanned { span, data: token }) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
    }
    None => unexpected_here(inp, expected),
  }
}

fn optional_alias<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<super::ast::Path<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.try_expect_map_or_stop(|token| {
    (keyword_of(token.data()) == Some(ContextualKeyword::As)).then_some(())
  })? {
    Some(_) => super::path(inp).map(Some),
    None => Ok(None),
  }
}

fn named_specifier_after_name<'inp, Src, Ctx>(
  name: super::ast::Name<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<NamedSpecifier<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = name.span().start();
  let alias = optional_alias(inp)?;
  let end = alias
    .as_ref()
    .map_or_else(|| name.span().end(), |path| path.span().end());
  Ok(NamedSpecifier::new(
    SimpleSpan::new(start, end),
    name,
    alias,
  ))
}

fn wildcard_specifier_after_asterisk<'inp, Src, Ctx>(
  asterisk_span: SimpleSpan,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<WildcardSpecifier<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let alias = optional_alias(inp)?;
  let end = alias
    .as_ref()
    .map_or_else(|| asterisk_span.end(), |path| path.span().end());
  Ok(WildcardSpecifier::new(
    SimpleSpan::new(asterisk_span.start(), end),
    alias,
  ))
}

import_parser!(
  /// Parses a committed GraphQLx named import specifier (`Name (as Path)?`).
  ///
  /// This is a GraphQLx dialect production with no corresponding GraphQL draft
  /// production.
  pub named_specifier,
  inp,
  NamedSpecifier<GraphqlxSlice<'inp, Src>>,
  {
    let name = super::name(inp)?;
    named_specifier_after_name(name, inp)
  }
);

import_parser!(
  /// Parses a committed GraphQLx wildcard import specifier (`* (as Path)?`).
  ///
  /// This is a GraphQLx dialect production with no corresponding GraphQL draft
  /// production.
  pub wildcard_specifier,
  inp,
  WildcardSpecifier<GraphqlxSlice<'inp, Src>>,
  {
    let asterisk = asterisk(inp)?;
    wildcard_specifier_after_asterisk(*asterisk.span(), inp)
  }
);

import_parser!(
  /// Parses one committed GraphQLx import member.
  ///
  /// A member is either a named or wildcard GraphQLx import specifier; this is
  /// a dialect production with no corresponding GraphQL draft production.
  pub import_member,
  inp,
  ImportMember<GraphqlxSlice<'inp, Src>>,
  {
  let named_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Identifier(source) => {
        named_specifier_after_name(super::ast::Name::new(span, source), inp)
          .map(ImportMember::Named)
      }
      _ => unreachable!("fused GraphQLx import-member dispatch received a non-identifier token"),
    };
  let wildcard_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Asterisk => {
        wildcard_specifier_after_asterisk(span, inp).map(ImportMember::Wildcard)
      }
      _ => unreachable!("fused GraphQLx import-member dispatch received a non-asterisk token"),
    };

  match (named_head, wildcard_head)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier, SyntacticTokenKind::Asterisk])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(member) => Ok(member),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::ImportMember),
  }
  }
);

fn decide_import_member<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_import_list_opener<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::LBrace) => Action::Continue,
    _ => Action::Stop,
  })
}

import_parser!(
  /// Parses a committed nonempty braced GraphQLx import list.
  ///
  /// This is a GraphQLx dialect production with no corresponding GraphQL draft
  /// production.
  pub import_list,
  inp,
  ImportList<GraphqlxSlice<'inp, Src>>,
  {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| import_member(inp))
    .repeated_while::<_, U1>(decide_import_member::<_, Ctx>)
    .at_least(1)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ImportList::new(span, data))
  }
);

import_parser!(
  /// Parses a committed GraphQLx import clause.
  ///
  /// A clause is either a wildcard specifier or a nonempty braced import list.
  /// This is a GraphQLx dialect production with no corresponding GraphQL draft
  /// production.
  pub import_clause,
  inp,
  ImportClause<GraphqlxSlice<'inp, Src>>,
  {
  let wildcard_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| match token {
      GraphqlxToken::<'inp, Src>::Asterisk => {
        wildcard_specifier_after_asterisk(span, inp).map(ImportClause::Wildcard)
      }
      _ => unreachable!("fused GraphQLx import-clause dispatch received a non-asterisk token"),
    };

  match (wildcard_head,)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Asterisk])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(clause) => Ok(clause),
    ParseAttempt::Decline => import_list
      .peek_then_try::<_, U1>(decide_import_list_opener::<Src, Ctx>)
      .try_parse_input(inp)
      .and_then(|attempt| match attempt {
        ParseAttempt::Accept(list) => Ok(ImportClause::List(list)),
        ParseAttempt::Decline => unexpected_here(inp, Expectation::ImportClause),
      }),
  }
  }
);

/// Parses `import importClause from InlineStringValue`.
pub(crate) fn import_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ImportDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let clause = import_clause(inp)?;
  contextual_keyword(inp, ContextualKeyword::From, Expectation::From)?;
  let file = super::value::inline_string_value(inp)?;
  Ok(ImportDefinition::new(
    SimpleSpan::new(start, file.span().end()),
    clause,
    file,
  ))
}

/// Parses `import importClause from InlineStringValue`.
///
/// This is a GraphQLx dialect production with no corresponding GraphQL draft
/// production.
pub fn import_definition<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ImportDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = contextual_keyword(inp, ContextualKeyword::Import, Expectation::Import)?.start();
  import_definition_after_keyword(inp, start)
}

macro_rules! graphqlx_import_api {
  ($(#[$meta:meta])* $node:ty, $parse:ident) => {
    impl<S> $node {
      $(#[$meta])*
      /// Parses this committed GraphQLx import production.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + DowncastRef<ContextualKeyword>,
        GraphqlxLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlxToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
          >,
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
      {
        $parse(inp)
      }
    }
  };
}

graphqlx_import_api!(
  /// Parses a committed GraphQLx named import specifier.
  ///
  /// See [`named_specifier`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  NamedSpecifier<S>,
  named_specifier
);
graphqlx_import_api!(
  /// Parses a committed GraphQLx wildcard import specifier.
  ///
  /// See [`wildcard_specifier`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  WildcardSpecifier<S>,
  wildcard_specifier
);
graphqlx_import_api!(
  /// Parses one committed GraphQLx import member.
  ///
  /// See [`import_member`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  ImportMember<S>,
  import_member
);
graphqlx_import_api!(
  /// Parses a committed GraphQLx import list.
  ///
  /// See [`import_list`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  ImportList<S>,
  import_list
);
graphqlx_import_api!(
  /// Parses a committed GraphQLx import clause.
  ///
  /// See [`import_clause`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  ImportClause<S>,
  import_clause
);
graphqlx_import_api!(
  /// Parses a committed GraphQLx import definition.
  ///
  /// See [`import_definition`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  ImportDefinition<S>,
  import_definition
);
