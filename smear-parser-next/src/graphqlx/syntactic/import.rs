//! GraphQLx import productions.

use std::vec::Vec;
use tokora::{
  Accumulator, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::Brace,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of, peek_kind,
  unexpected_here,
};
use crate::{
  combinator::{ParseCtx, asterisk},
  graphqlx::{
    GraphQLx,
    ast::{
      ImportClause, ImportDefinition, ImportList, ImportMember, NamedSpecifier, WildcardSpecifier,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! import_parser {
  (@impl [$($unclosed:ty),*] $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    #[doc = "Parses this committed GraphQLx import production."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
        + tokora::token::PunctuatorToken<'inp>
        + DowncastRef<ContextualKeyword>,
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
        > $(+ From<$unclosed>)*
        + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
  (plain $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    import_parser!(@impl [] $visibility $name, $input, $output, $body);
  };
  (brace $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    import_parser!(
      @impl [Unclosed<Brace, SimpleSpan, GraphQLx>]
      $visibility $name, $input, $output, $body
    );
  };
}

fn contextual_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  expected: Expectation,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
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
  match inp.next()? {
    Some(Spanned { span, data: token }) if keyword_of(&token) == Some(keyword) => Ok(span),
    Some(Spanned { span, data: token }) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
    }
    None => {
      unexpected_here(inp, expected)?;
      unreachable!("unexpected_here always returns an error")
    }
  }
}

fn optional_alias<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<super::ast::Path<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
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
  match inp.try_expect_map(|token| {
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
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
  let alias = optional_alias(inp)?;
  let end = alias
    .as_ref()
    .map_or_else(|| asterisk_span.end(), |path| path.span().end());
  Ok(WildcardSpecifier::new(
    SimpleSpan::new(asterisk_span.start(), end),
    alias,
  ))
}

import_parser!(plain pub named_specifier, inp, NamedSpecifier<GraphqlxSlice<'inp, Src>>, {
  let name = super::name(inp)?;
  named_specifier_after_name(name, inp)
});

import_parser!(plain pub wildcard_specifier, inp, WildcardSpecifier<GraphqlxSlice<'inp, Src>>, {
  let asterisk = asterisk(inp)?;
  wildcard_specifier_after_asterisk(*asterisk.span(), inp)
});

import_parser!(plain pub import_member, inp, ImportMember<GraphqlxSlice<'inp, Src>>, {
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
    ParseAttempt::Decline => {
      unexpected_here(inp, Expectation::ImportMember)?;
      unreachable!("unexpected_here always returns an error")
    }
  }
});

fn decide_import_member<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

import_parser!(brace pub import_list, inp, ImportList<GraphqlxSlice<'inp, Src>>, {
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| import_member(inp))
    .repeated_while::<_, U1>(decide_import_member::<_, Ctx>)
    .at_least(1)
    .delimited_by_braces()
    .collect_with(Vec::new())
    .spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| ImportList::new(span, data))
});

import_parser!(brace pub import_clause, inp, ImportClause<GraphqlxSlice<'inp, Src>>, {
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
    ParseAttempt::Decline => match peek_kind(inp)? {
      Some(SyntacticTokenKind::LBrace) => import_list(inp).map(ImportClause::List),
      _ => {
        unexpected_here(inp, Expectation::ImportClause)?;
        unreachable!("unexpected_here always returns an error")
      }
    },
  }
});

/// Parses `import importClause from InlineStringValue`.
pub fn import_definition<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ImportDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
    + tokora::token::PunctuatorToken<'inp>
    + DowncastRef<ContextualKeyword>,
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
    > + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let cursor = *inp.cursor();
  contextual_keyword(inp, ContextualKeyword::Import, Expectation::Import)?;
  let clause = import_clause(inp)?;
  contextual_keyword(inp, ContextualKeyword::From, Expectation::From)?;
  let file = super::value::inline_string_value(inp)?;
  Ok(ImportDefinition::new(inp.span_since(&cursor), clause, file))
}

macro_rules! graphqlx_import_api {
  (@impl [$($unclosed:ty),*] $node:ty, $parse:ident) => {
    impl<S> $node {
      /// Parses this committed GraphQLx import production.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + tokora::token::PunctuatorToken<'inp>
          + DowncastRef<ContextualKeyword>,
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
          > $(+ From<$unclosed>)*
          + From<DialectGraphqlxError<S>>,
      {
        $parse(inp)
      }
    }
  };
  (plain $node:ty, $parse:ident) => {
    graphqlx_import_api!(@impl [] $node, $parse);
  };
  (brace $node:ty, $parse:ident) => {
    graphqlx_import_api!(@impl [Unclosed<Brace, SimpleSpan, GraphQLx>] $node, $parse);
  };
}

graphqlx_import_api!(plain NamedSpecifier<S>, named_specifier);
graphqlx_import_api!(plain WildcardSpecifier<S>, wildcard_specifier);
graphqlx_import_api!(plain ImportMember<S>, import_member);
graphqlx_import_api!(brace ImportList<S>, import_list);
graphqlx_import_api!(brace ImportClause<S>, import_clause);
graphqlx_import_api!(brace ImportDefinition<S>, import_definition);
