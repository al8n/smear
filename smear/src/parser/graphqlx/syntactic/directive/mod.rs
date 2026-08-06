//! GraphQLx executable and constant directive productions.
//!
//! Directives commit after `@`; their names are generic-capable GraphQLx type
//! paths. Absent directive runs leave input untouched and return a zero-width
//! empty collection. Empty argument lists are represented as `None` on the
//! containing directive while still contributing to its span.
//! GraphQLx extends the directive name from GraphQL's name to a generic-capable
//! type path and admits GraphQLx input values in arguments.
//!
//! See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).

use std::vec::Vec;

use tokora::{
  Lexer, ParseInput, SimpleSpan, Slice, Source, Token, punct::At, span::Spanned,
  try_parse_input::ParseAttempt, utils::DowncastRef,
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken,
  argument::{arguments, const_arguments},
  path_after_first, try_name,
  ty::try_type_generics,
  unexpected_here,
};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, extent_end, extent_since, try_at, try_double_colon},
  graphqlx::{
    GraphQLx,
    ast::{ConstDirective, ConstDirectives, Directive, Directives, TypePath},
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! directive_parser {
  (
    $(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty;
    $body:block
  ) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
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

fn take_at<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<At<SimpleSpan, (), GraphQLx>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_at(inp)? {
    ParseAttempt::Accept(at) => Ok(at),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::At),
  }
}

fn take_directive_type_path<'inp, Src, Ctx>(
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
  let separator = try_double_colon(inp)?;
  let fully_qualified = matches!(&separator, ParseAttempt::Accept(_));
  let first = match try_name(inp)? {
    ParseAttempt::Accept(name) => name,
    ParseAttempt::Decline => return unexpected_here(inp, Expectation::Path),
  };
  // Already the first token's start — `::` when the path is fully qualified, the leading segment
  // otherwise — so this is `extent_start` by another route and does not need to peek again.
  let start = match separator {
    ParseAttempt::Accept(separator) => separator.span().start(),
    ParseAttempt::Decline => first.span().start(),
  };
  let path = path_after_first(start, first, fully_qualified, inp)?;
  let generics = try_type_generics(inp)?;
  Ok(TypePath::new(extent_since(inp, start), path, generics))
}

fn directive_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<Directive<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  take_directive_type_path
    .then(arguments)
    .token_spanned()
    .map(
      |Spanned {
         span,
         data: (name, arguments),
       }| {
        Directive::new(
          SimpleSpan::new(start, span.end()),
          name,
          (!arguments.arguments().is_empty()).then_some(arguments),
        )
      },
    )
    .parse_input(inp)
}

fn const_directive_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<ConstDirective<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  take_directive_type_path
    .then(const_arguments)
    .token_spanned()
    .map(
      |Spanned {
         span,
         data: (name, arguments),
       }| {
        ConstDirective::new(
          SimpleSpan::new(start, span.end()),
          name,
          (!arguments.arguments().is_empty()).then_some(arguments),
        )
      },
    )
    .parse_input(inp)
}

fn directives_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  first_at: At<SimpleSpan, (), GraphQLx>,
) -> Result<Directives<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = first_at.span().start();
  let mut parsed = Vec::from([directive_after_at(inp, start)?]);
  while let ParseAttempt::Accept(at) = try_at(inp)? {
    parsed.push(directive_after_at(inp, at.span().start())?);
  }
  let end = parsed
    .last()
    .expect("a directive run always contains its first directive")
    .span()
    .end();
  Ok(Directives::new(SimpleSpan::new(start, end), parsed))
}

fn const_directives_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  first_at: At<SimpleSpan, (), GraphQLx>,
) -> Result<ConstDirectives<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = first_at.span().start();
  let mut parsed = Vec::from([const_directive_after_at(inp, start)?]);
  while let ParseAttempt::Accept(at) = try_at(inp)? {
    parsed.push(const_directive_after_at(inp, at.span().start())?);
  }
  let end = parsed
    .last()
    .expect("a directive run always contains its first directive")
    .span()
    .end();
  Ok(ConstDirectives::new(SimpleSpan::new(start, end), parsed))
}

directive_parser!(
  /// Parses one committed executable GraphQLx `Directive` production.
  ///
  /// Its `@` and optional-argument structure follows GraphQL. GraphQLx extends
  /// the directive name to a generic-capable type path and permits GraphQLx
  /// input values in arguments.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub directive,
  inp,
  Directive<GraphqlxSlice<'inp, Src>>;
  {
    let at = take_at(inp)?;
    directive_after_at(inp, at.span().start())
  }
);

directive_parser!(
  /// Parses one committed constant GraphQLx `Directive` production.
  ///
  /// This has the GraphQL directive shape, with GraphQLx type paths for names
  /// and GraphQLx constant input values in arguments.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub const_directive,
  inp,
  ConstDirective<GraphqlxSlice<'inp, Src>>;
  {
    let at = take_at(inp)?;
    const_directive_after_at(inp, at.span().start())
  }
);

directive_parser!(
  /// Parses an executable GraphQLx `Directives` collection.
  ///
  /// If `@` is absent, this returns an empty zero-width collection without
  /// consuming input. Once a directive head begins, every following `@` is
  /// committed to a complete directive.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub directives,
  inp,
  Directives<GraphqlxSlice<'inp, Src>>;
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::combinator::extent`.
    let start = extent_end(inp);
    match try_at(inp)? {
      ParseAttempt::Accept(at) => directives_after_at(inp, at),
      ParseAttempt::Decline => Ok(Directives::new(SimpleSpan::new(start, start), Vec::new())),
    }
  }
);

directive_parser!(
  /// Parses a constant GraphQLx `Directives` collection.
  ///
  /// If `@` is absent, this returns an empty zero-width collection without
  /// consuming input. Once a directive head begins, every following `@` is
  /// committed to a complete directive and variables are rejected in arguments.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub const_directives,
  inp,
  ConstDirectives<GraphqlxSlice<'inp, Src>>;
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::combinator::extent`.
    let start = extent_end(inp);
    match try_at(inp)? {
      ParseAttempt::Accept(at) => const_directives_after_at(inp, at),
      ParseAttempt::Decline => Ok(ConstDirectives::new(
        SimpleSpan::new(start, start),
        Vec::new(),
      )),
    }
  }
);

macro_rules! impl_directive_api {
  (
    $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident;
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
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_directive_api!(
  /// Parses one committed executable GraphQLx directive.
  ///
  /// GraphQLx extends directive names to generic-capable type paths and admits
  /// extended input values in arguments.
  ///
  /// See [`directive`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  Directive<S>,
  directive;
);
impl_directive_api!(
  /// Parses one committed constant GraphQLx directive.
  ///
  /// GraphQLx extends directive names to generic-capable type paths and admits
  /// extended constant input values in arguments.
  ///
  /// See [`const_directive`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  ConstDirective<S>,
  const_directive;
);
impl_directive_api!(
  /// Parses executable GraphQLx directives, returning an empty zero-width list
  /// without consuming input when `@` is absent.
  ///
  /// Each directive uses GraphQLx's generic-capable type-path name.
  ///
  /// See [`directives`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  Directives<S>,
  directives;
);
impl_directive_api!(
  /// Parses constant GraphQLx directives, returning an empty zero-width list
  /// without consuming input when `@` is absent.
  ///
  /// Each directive uses GraphQLx's generic-capable type-path name.
  ///
  /// See [`const_directives`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  ConstDirectives<S>,
  const_directives;
);

#[cfg(test)]
mod tests;
