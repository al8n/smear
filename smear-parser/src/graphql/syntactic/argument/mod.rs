//! GraphQL executable and constant argument productions.
//!
//! Singular arguments are committed `Name ':' Value` parsers with phase-local
//! diagnostics. Absent argument lists produce an empty, zero-width collection
//! without consuming; after a `(` opener they remain committed through `)`.
//! Associated entry points are exposed directly on the GraphQL AST argument
//! types, with the lexer source inferred from the parser input.

use std::vec::Vec;

use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, SimpleSpan, Slice, Source, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphql::ContextualKeyword;

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken, name,
  value::{HeadKind, const_value, value, value_head_kind},
};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, colon, extent_end},
  graphql::{
    GraphQL,
    ast::{Argument, ArgumentList, Arguments, ConstArgument, ConstArguments},
    error::{Expectation, GraphqlError as DialectGraphqlError},
  },
};

macro_rules! argument_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
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
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

fn guard_argument_phase<'inp, Src, Ctx>(
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

argument_parser!(
  /// Parses one executable GraphQL `Argument` production.
  ///
  /// This parser is committed: it requires a name, `:`, and an executable input
  /// value, reporting the phase-specific diagnostic when any part is malformed.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub argument,
  inp,
  Argument<GraphqlSlice<'inp, Src>>,
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_argument_phase(inp, Expectation::Name, |token| {
        matches!(token, GraphqlToken::<'inp, Src>::Identifier(_))
      })?;
      name(inp)
    })
      .then_ignore(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_argument_phase(inp, Expectation::Colon, |token| {
          matches!(token, GraphqlToken::<'inp, Src>::Colon)
        })?;
        colon(inp)
      })
      .then(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_argument_phase(inp, Expectation::InputValue, |token| {
          value_head_kind::<Src>(token).is_some()
        })?;
        value(inp)
      })
      .token_spanned()
      .map(|Spanned { span, data: (name, value) }| Argument::new(span, name, value))
      .parse_input(inp)
  }
);

argument_parser!(
  /// Parses one constant GraphQL `Argument` production.
  ///
  /// This parser is committed and rejects variables in the value position while
  /// retaining the same name and colon diagnostics as [`argument`].
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub const_argument,
  inp,
  ConstArgument<GraphqlSlice<'inp, Src>>,
  {
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      guard_argument_phase(inp, Expectation::Name, |token| {
        matches!(token, GraphqlToken::<'inp, Src>::Identifier(_))
      })?;
      name(inp)
    })
      .then_ignore(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_argument_phase(inp, Expectation::Colon, |token| {
          matches!(token, GraphqlToken::<'inp, Src>::Colon)
        })?;
        colon(inp)
      })
      .then(|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        guard_argument_phase(inp, Expectation::ConstInputValue, |token| {
          !matches!(value_head_kind::<Src>(token), Some(HeadKind::Dollar) | None)
        })?;
        const_value(inp)
      })
      .token_spanned()
      .map(|Spanned { span, data: (name, value) }| ConstArgument::new(span, name, value))
      .parse_input(inp)
  }
);

fn decide_argument_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::RParen) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_arguments_head<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlLexer<'inp, Src>, Ctx::Emitter, GraphQL>,
) -> Result<Action, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::LParen) => Action::Continue,
    _ => Action::Stop,
  })
}

argument_parser!(
  /// The `(`-committed half of [`arguments`], reachable on its own so a caller that has
  /// already classified the head as `(` enters it without probing for the opener again.
  pub(super) committed_arguments,
  inp,
  Arguments<GraphqlSlice<'inp, Src>>,
  {
    argument
      .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx>)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ArgumentList::new(span, data))
  }
);

argument_parser!(
  committed_const_arguments,
  inp,
  ConstArguments<GraphqlSlice<'inp, Src>>,
  {
    const_argument
      .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx>)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ArgumentList::new(span, data))
  }
);

argument_parser!(
  /// Parses an executable GraphQL `Arguments` collection.
  ///
  /// If the next token is not `(`, this parser consumes nothing and returns an
  /// empty collection with a zero-width span. After `(` it is committed through
  /// `)`. The lenient empty spelling `()` is accepted with its real delimiter span.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub arguments,
  inp,
  Arguments<GraphqlSlice<'inp, Src>>,
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::combinator::extent`.
    let start = extent_end(inp);
    committed_arguments
      .peek_then_try::<_, U1>(decide_arguments_head::<Src, Ctx>)
      .try_parse_input(inp)
      .map(|attempt| match attempt {
        ParseAttempt::Accept(arguments) => arguments,
        ParseAttempt::Decline => ArgumentList::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

argument_parser!(
  /// Parses a constant GraphQL `Arguments` collection.
  ///
  /// If the next token is not `(`, this parser consumes nothing and returns an
  /// empty collection with a zero-width span. After `(` it is committed through
  /// `)`, and its argument values reject variables. The lenient empty spelling
  /// `()` is accepted with its real delimiter span.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub const_arguments,
  inp,
  ConstArguments<GraphqlSlice<'inp, Src>>,
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::combinator::extent`.
    let start = extent_end(inp);
    committed_const_arguments
      .peek_then_try::<_, U1>(decide_arguments_head::<Src, Ctx>)
      .try_parse_input(inp)
      .map(|attempt| match attempt {
        ParseAttempt::Accept(arguments) => arguments,
        ParseAttempt::Decline => ArgumentList::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

macro_rules! impl_argument_api {
  (
    $(#[$meta:meta])*
    $slice:ident,
    $node:ty,
    $parser:ident $(,)?
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
        $slice: Slice<'inp> + Clone + 'inp,
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

impl_argument_api!(
  /// Parses one committed executable GraphQL argument.
  ///
  /// See [`argument`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  Argument<S>,
  argument,
);

impl_argument_api!(
  /// Parses one committed constant GraphQL argument.
  ///
  /// See [`const_argument`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  ConstArgument<S>,
  const_argument,
);

impl_argument_api!(
  /// Parses executable GraphQL arguments, returning an empty zero-width collection
  /// without consuming when `(` is absent.
  ///
  /// See [`arguments`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  Arguments<S>,
  arguments,
);

impl_argument_api!(
  /// Parses constant GraphQL arguments, returning an empty zero-width collection
  /// without consuming when `(` is absent.
  ///
  /// See [`const_arguments`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  ConstArguments<S>,
  const_arguments,
);

#[cfg(test)]
mod tests;
