//! GraphQL executable and constant directive productions.
//!
//! Singular directives diagnose their committed `@` and name phases locally.
//! Absent directive runs produce an empty, zero-width collection without consuming;
//! after an `@` they greedily parse every following directive head.

use std::vec::Vec;

use tokora::{
  Accumulator, Lexer, ParseInput, SimpleSpan, Slice, Source, Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Brace, Bracket, Paren},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::typenum::U1,
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  argument::{arguments, const_arguments},
};
use crate::{
  combinator::{Equivalent, ParseCtx, at, ident},
  graphql::{
    GraphQL,
    ast::{ConstDirective, ConstDirectives, Directive, Directives},
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
        + From<tokora::error::Unclosed<Paren, SimpleSpan, GraphQL>>
        + From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
        + From<tokora::error::Unclosed<Brace, SimpleSpan, GraphQL>>
        + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

fn guard_directive_phase<'inp, Src, Ctx>(
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
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_directive_phase(inp, Expectation::At, |token| {
        matches!(token, GraphqlToken::<'inp, Src>::At)
      })?;
      at(inp)
    })
      .ignore_then(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_directive_phase(inp, Expectation::Name, |token| {
          matches!(token, GraphqlToken::<'inp, Src>::Identifier(_))
        })?;
        ident(inp)
      })
      .then(arguments)
      .spanned()
      .map(|Spanned { span, data: (name, arguments) }| {
        let arguments = if arguments.arguments().is_empty() {
          None
        } else {
          Some(arguments)
        };
        Directive::new(span, name, arguments)
      })
      .parse_input(inp)
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
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_directive_phase(inp, Expectation::At, |token| {
        matches!(token, GraphqlToken::<'inp, Src>::At)
      })?;
      at(inp)
    })
      .ignore_then(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_directive_phase(inp, Expectation::Name, |token| {
          matches!(token, GraphqlToken::<'inp, Src>::Identifier(_))
        })?;
        ident(inp)
      })
      .then(const_arguments)
      .spanned()
      .map(|Spanned { span, data: (name, arguments) }| {
        let arguments = if arguments.arguments().is_empty() {
          None
        } else {
          Some(arguments)
        };
        Directive::new(span, name, arguments)
      })
      .parse_input(inp)
  }
);

fn decide_directive_head<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::At) => Action::Continue,
    _ => Action::Stop,
  })
}

directive_parser!(
  committed_directives,
  inp,
  Directives<GraphqlSlice<'inp, Src>>,
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| -> Result<
      Vec<Directive<GraphqlSlice<'inp, Src>>>,
      GraphqlError<'inp, Src, Ctx>,
    > {
      directive
        .repeated_while::<_, U1>(decide_directive_head::<Src, Ctx>)
        .collect_with(Vec::<Directive<GraphqlSlice<'inp, Src>>>::new())
        .parse_input(inp)
    })
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| Directives::new(span, data))
  }
);

directive_parser!(
  committed_const_directives,
  inp,
  ConstDirectives<GraphqlSlice<'inp, Src>>,
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| -> Result<
      Vec<ConstDirective<GraphqlSlice<'inp, Src>>>,
      GraphqlError<'inp, Src, Ctx>,
    > {
      const_directive
        .repeated_while::<_, U1>(decide_directive_head::<Src, Ctx>)
        .collect_with(Vec::<ConstDirective<GraphqlSlice<'inp, Src>>>::new())
        .parse_input(inp)
    })
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| Directives::new(span, data))
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
    let start = *inp.offset();
    committed_directives
      .peek_then_try::<_, U1>(decide_directive_head::<Src, Ctx>)
      .try_parse_input(inp)
      .map(|attempt| match attempt {
        ParseAttempt::Accept(directives) => directives,
        ParseAttempt::Decline => Directives::new(SimpleSpan::new(start, start), Vec::new()),
      })
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
    let start = *inp.offset();
    committed_const_directives
      .peek_then_try::<_, U1>(decide_directive_head::<Src, Ctx>)
      .try_parse_input(inp)
      .map(|attempt| match attempt {
        ParseAttempt::Accept(directives) => directives,
        ParseAttempt::Decline => Directives::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

macro_rules! impl_directive_graphql {
  (
    $(#[$meta:meta])*
    $node:ty,
    $parser:ident
  ) => {
    impl<S> $node {
      $(#[$meta])*
      pub fn graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp,
        str: Equivalent<S>,
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
          + From<tokora::error::Unclosed<Paren, SimpleSpan, GraphQL>>
          + From<tokora::error::Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<tokora::error::Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<S>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_directive_graphql!(
  /// Parses one committed executable GraphQL directive.
  ///
  /// See [`directive`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  Directive<S>,
  directive
);

impl_directive_graphql!(
  /// Parses one committed constant GraphQL directive.
  ///
  /// See [`const_directive`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  Directive<S, crate::graphql::ast::ConstArguments<S>>,
  const_directive
);

impl_directive_graphql!(
  /// Parses executable GraphQL directives, returning an empty zero-width collection
  /// without consuming when `@` is absent.
  ///
  /// See [`directives`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  Directives<S>,
  directives
);

impl_directive_graphql!(
  /// Parses constant GraphQL directives, returning an empty zero-width collection
  /// without consuming when `@` is absent.
  ///
  /// See [`const_directives`] and the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
  Directives<S, ConstDirective<S>>,
  const_directives
);

#[cfg(test)]
mod tests;
