//! GraphQL executable and constant directive productions.
//!
//! Singular directives diagnose their committed `@` and name phases locally.
//! Absent directive runs produce an empty, zero-width collection without consuming;
//! after an `@` they greedily parse every following directive head.

use std::vec::Vec;

use smear_scaffold::ast as scaffold;
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
  ($visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    #[doc = "Parses this GraphQL directive production with the concrete syntactic lexer."]
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
        scaffold::Directive::new(span, name, arguments)
      })
      .parse_input(inp)
  }
);

directive_parser!(
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
        scaffold::Directive::new(span, name, arguments)
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
      .map(|Spanned { span, data }| scaffold::Directives::new(span, data))
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
      .map(|Spanned { span, data }| scaffold::Directives::new(span, data))
  }
);

directive_parser!(
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
        ParseAttempt::Decline => scaffold::Directives::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

directive_parser!(
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
        ParseAttempt::Decline => scaffold::Directives::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

#[cfg(test)]
mod tests;
