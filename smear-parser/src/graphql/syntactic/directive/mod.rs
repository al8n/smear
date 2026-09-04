//! GraphQL executable and constant directive productions.
//!
//! Singular directives diagnose their committed `@` and name phases locally.
//! Absent directive runs produce an empty, zero-width collection without consuming;
//! after an `@` they greedily parse every following directive head.
//! Associated entry points are exposed directly on the GraphQL AST directive
//! types, with the lexer source inferred from the parser input.

use std::vec::Vec;

use tokora::{Lexer, SimpleSpan, Slice, Source, try_parse_input::ParseAttempt, utils::DowncastRef};

use smear_lexer::graphql::ContextualKeyword;

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  argument::{arguments, const_arguments},
  try_name,
};
use crate::{
  combinator::{ParseCtx, extent_end, try_at},
  graphql::{
    GraphQL,
    ast::{ConstDirective, ConstDirectives, Directive, Directives, Name},
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

macro_rules! directive_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

fn expected_directive_phase<'inp, Src, Ctx, T>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<T, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  // `peek_head_map`, not a raw `peek`: a window the scanner truncated is byte-for-byte a window a
  // short document produces, and reading the first as the second reports a stop as an absent token
  // — smear issue #177, Codex round 1. The terminal end-of-input error it raises instead carries
  // tokora's mark into this dialect's `TerminalEndOfInput`.
  let rejected = inp.peek_head_map(|token| (*token.span, token.data.kind()))?;

  match rejected {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, expected, span).into()),
    None => Err(
      DialectGraphqlError::maybe_unexpected_token(None, expected, SimpleSpan::new(offset, offset))
        .into(),
    ),
  }
}

fn take_at<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<tokora::punct::At<SimpleSpan, (), GraphQL>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match try_at(inp)? {
    ParseAttempt::Accept(at) => Ok(at),
    ParseAttempt::Decline => expected_directive_phase(inp, Expectation::At),
  }
}

fn take_directive_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match try_name(inp)? {
    ParseAttempt::Accept(name) => Ok(name),
    ParseAttempt::Decline => expected_directive_phase(inp, Expectation::Name),
  }
}

/// Parses a directive after its `@` has already been consumed by [`try_at`].
pub(super) fn directive_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  at: tokora::punct::At<SimpleSpan, (), GraphQL>,
) -> Result<Directive<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let start = at.span().start();
  let name = take_directive_name(inp)?;
  let mut end = name.span().end();
  let arguments = arguments(inp)?;
  if arguments.span().start() != arguments.span().end() {
    end = arguments.span().end();
  }
  let arguments = (!arguments.arguments().is_empty()).then_some(arguments);
  Ok(Directive::new(SimpleSpan::new(start, end), name, arguments))
}

/// Parses an executable directive run after its first `@` has been consumed.
/// Every further `@` is likewise consumed by [`try_at`] before its tail parses.
pub(super) fn directives_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  at: tokora::punct::At<SimpleSpan, (), GraphQL>,
) -> Result<Directives<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let start = at.span().start();
  let first = directive_after_at(inp, at)?;
  let mut end = first.span().end();
  let mut directives = Vec::new();
  directives.push(first);

  while let ParseAttempt::Accept(at) = try_at(inp)? {
    let directive = directive_after_at(inp, at)?;
    end = directive.span().end();
    directives.push(directive);
  }

  Ok(Directives::new(SimpleSpan::new(start, end), directives))
}

fn const_directive_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  at: tokora::punct::At<SimpleSpan, (), GraphQL>,
) -> Result<ConstDirective<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let start = at.span().start();
  let name = take_directive_name(inp)?;
  let mut end = name.span().end();
  let arguments = const_arguments(inp)?;
  if arguments.span().start() != arguments.span().end() {
    end = arguments.span().end();
  }
  let arguments = (!arguments.arguments().is_empty()).then_some(arguments);
  Ok(ConstDirective::new(
    SimpleSpan::new(start, end),
    name,
    arguments,
  ))
}

fn const_directives_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  at: tokora::punct::At<SimpleSpan, (), GraphQL>,
) -> Result<ConstDirectives<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let start = at.span().start();
  let first = const_directive_after_at(inp, at)?;
  let mut end = first.span().end();
  let mut directives = Vec::new();
  directives.push(first);

  while let ParseAttempt::Accept(at) = try_at(inp)? {
    let directive = const_directive_after_at(inp, at)?;
    end = directive.span().end();
    directives.push(directive);
  }

  Ok(ConstDirectives::new(
    SimpleSpan::new(start, end),
    directives,
  ))
}

directive_parser!(
  /// Parses one executable GraphQL `Directive` production.
  ///
  /// This parser is committed: it requires `@` and a directive name, then parses
  /// arguments when `(` follows. An absent or explicitly empty argument collection
  /// is stored as `None` on the directive node.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub directive,
  inp,
  Directive<GraphqlSlice<'inp, Src>>,
  {
    let at = take_at(inp)?;
    directive_after_at(inp, at)
  }
);

directive_parser!(
  /// Parses one constant GraphQL `Directive` production.
  ///
  /// This parser has the same committed `@` and name phases as [`directive`], but
  /// rejects variables inside directive arguments. Empty arguments are stored as
  /// `None` on the directive node.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub const_directive,
  inp,
  ConstDirective<GraphqlSlice<'inp, Src>>,
  {
    let at = take_at(inp)?;
    const_directive_after_at(inp, at)
  }
);

directive_parser!(
  /// Parses an executable GraphQL `Directives` collection.
  ///
  /// If the next token is not `@`, this parser consumes nothing and returns an
  /// empty collection with a zero-width span. After `@`, every directive head is
  /// committed and a malformed directive reports its phase-specific diagnostic.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub directives,
  inp,
  Directives<GraphqlSlice<'inp, Src>>,
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
  /// Parses a constant GraphQL `Directives` collection.
  ///
  /// If the next token is not `@`, this parser consumes nothing and returns an
  /// empty collection with a zero-width span. After `@`, every directive head is
  /// committed, and argument values reject variables.
  ///
  /// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  pub const_directives,
  inp,
  ConstDirectives<GraphqlSlice<'inp, Src>>,
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
    $(#[$meta:meta])*
    $slice:ident,
    $node:ty,
    $parser:ident
  ) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp + $crate::value::Leaf,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_directive_api!(
  /// Parses one committed executable GraphQL directive.
  ///
  /// See [`directive`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  Directive<S>,
  directive
);

impl_directive_api!(
  /// Parses one committed constant GraphQL directive.
  ///
  /// See [`const_directive`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  ConstDirective<S>,
  const_directive
);

impl_directive_api!(
  /// Parses executable GraphQL directives, returning an empty zero-width collection
  /// without consuming when `@` is absent.
  ///
  /// See [`directives`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  Directives<S>,
  directives
);

impl_directive_api!(
  /// Parses constant GraphQL directives, returning an empty zero-width collection
  /// without consuming when `@` is absent.
  ///
  /// See [`const_directives`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  S,
  ConstDirectives<S>,
  const_directives
);

#[cfg(test)]
mod tests;
