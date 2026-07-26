//! GraphQLx executable and constant argument productions.
//!
//! Singular arguments commit to `Name ':' Value` and report diagnostics at the
//! malformed phase. Absent collections leave input untouched and yield an
//! empty zero-width list; after `(` they commit through the matching `)`.
//! GraphQLx retains GraphQL's argument structure while admitting GraphQLx input
//! values in the value position.
//!
//! See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).

use std::vec::Vec;

use tokora::{
  Accumulator, Lexer, ParseInput, SimpleSpan, Slice, Source, Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Brace, Bracket, Paren},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, try_name,
  unexpected_here,
  value::{const_value, value},
};
use crate::{
  combinator::{ParseCtx, try_colon},
  graphqlx::{
    GraphQLx,
    ast::{Argument, Arguments, ConstArgument, ConstArguments},
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! argument_parser {
  (
    $(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty;
    error_bounds = [$($error_bounds:tt)*];
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
      GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
        + From<
          UnexpectedToken<
            'inp,
            GraphqlxToken<'inp, Src>,
            <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
            SimpleSpan,
            GraphQLx,
          >,
        > $($error_bounds)*
        + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

argument_parser!(
  /// Parses one committed executable GraphQLx `Argument` production.
  ///
  /// The `Name ':' Value` structure follows GraphQL, while `Value` admits the
  /// GraphQLx input-value extensions.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub argument,
  inp,
  Argument<GraphqlxSlice<'inp, Src>>;
  error_bounds = [
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
  {
    let cursor = *inp.cursor();
    let name = match try_name(inp)? {
      ParseAttempt::Accept(name) => name,
      ParseAttempt::Decline => return unexpected_here(inp, Expectation::Name),
    };
    match try_colon(inp)? {
      ParseAttempt::Accept(_) => {}
      ParseAttempt::Decline => return unexpected_here(inp, Expectation::Colon),
    }
    let value = value(inp)?;
    Ok(Argument::new(inp.span_since(&cursor), name, value))
  }
);

argument_parser!(
  /// Parses one committed constant GraphQLx `Argument` production.
  ///
  /// This follows GraphQL's constant-argument form, rejecting variables while
  /// admitting GraphQLx constant input-value extensions.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub const_argument,
  inp,
  ConstArgument<GraphqlxSlice<'inp, Src>>;
  error_bounds = [
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
  {
    let cursor = *inp.cursor();
    let name = match try_name(inp)? {
      ParseAttempt::Accept(name) => name,
      ParseAttempt::Decline => return unexpected_here(inp, Expectation::Name),
    };
    match try_colon(inp)? {
      ParseAttempt::Accept(_) => {}
      ParseAttempt::Decline => return unexpected_here(inp, Expectation::Colon),
    }
    let value = const_value(inp)?;
    Ok(ConstArgument::new(inp.span_since(&cursor), name, value))
  }
);

fn decide_argument_head<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RParen) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

fn decide_arguments_head<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::LParen) => Action::Continue,
    _ => Action::Stop,
  })
}

argument_parser!(
  committed_arguments,
  inp,
  Arguments<GraphqlxSlice<'inp, Src>>;
  error_bounds = [
    + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
  {
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| argument(inp))
      .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx>)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| Arguments::new(span, data))
  }
);

argument_parser!(
  committed_const_arguments,
  inp,
  ConstArguments<GraphqlxSlice<'inp, Src>>;
  error_bounds = [
    + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
  {
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_argument(inp))
      .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx>)
      .delimited_by_parens()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| ConstArguments::new(span, data))
  }
);

argument_parser!(
  /// Parses an executable GraphQLx `Arguments` collection.
  ///
  /// When `(` is absent, this returns an empty zero-width collection without
  /// consuming input. Once the opener is present, malformed members and a
  /// missing `)` are committed errors.
  ///
  /// The collection shape follows GraphQL; its members admit GraphQLx input
  /// values.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub arguments,
  inp,
  Arguments<GraphqlxSlice<'inp, Src>>;
  error_bounds = [
    + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
  {
    let start = *inp.offset();
    committed_arguments
      .peek_then_try::<_, U1>(decide_arguments_head::<Src, Ctx>)
      .try_parse_input(inp)
      .map(|attempt| match attempt {
        ParseAttempt::Accept(arguments) => arguments,
        ParseAttempt::Decline => Arguments::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

argument_parser!(
  /// Parses a constant GraphQLx `Arguments` collection.
  ///
  /// When `(` is absent, this returns an empty zero-width collection without
  /// consuming input. Once the opener is present, malformed members and a
  /// missing `)` are committed errors, and variables are rejected in values.
  ///
  /// The collection shape follows GraphQL; its members admit GraphQLx constant
  /// input-value extensions.
  ///
  /// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  pub const_arguments,
  inp,
  ConstArguments<GraphqlxSlice<'inp, Src>>;
  error_bounds = [
    + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
  {
    let start = *inp.offset();
    committed_const_arguments
      .peek_then_try::<_, U1>(decide_arguments_head::<Src, Ctx>)
      .try_parse_input(inp)
      .map(|attempt| match attempt {
        ParseAttempt::Accept(arguments) => arguments,
        ParseAttempt::Decline => ConstArguments::new(SimpleSpan::new(start, start), Vec::new()),
      })
  }
);

macro_rules! impl_argument_api {
  (
    $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident;
    error_bounds = [$($error_bounds:tt)*];
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
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLx,
            >,
          > $($error_bounds)*
          + From<DialectGraphqlxError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_argument_api!(
  /// Parses one committed executable GraphQLx argument.
  ///
  /// GraphQLx preserves GraphQL's argument structure and admits extended input
  /// values.
  ///
  /// See [`argument`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  Argument<S>,
  argument;
  error_bounds = [
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
);
impl_argument_api!(
  /// Parses one committed constant GraphQLx argument.
  ///
  /// GraphQLx preserves GraphQL's constant-argument structure and admits
  /// extended constant input values.
  ///
  /// See [`const_argument`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  ConstArgument<S>,
  const_argument;
  error_bounds = [
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
);
impl_argument_api!(
  /// Parses executable GraphQLx arguments, returning an empty zero-width list
  /// without consuming input when `(` is absent.
  ///
  /// Its members admit GraphQLx input-value extensions.
  ///
  /// See [`arguments`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  Arguments<S>,
  arguments;
  error_bounds = [
    + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
);
impl_argument_api!(
  /// Parses constant GraphQLx arguments, returning an empty zero-width list
  /// without consuming input when `(` is absent.
  ///
  /// Its members admit GraphQLx constant input-value extensions.
  ///
  /// See [`const_arguments`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  ConstArguments<S>,
  const_arguments;
  error_bounds = [
    + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
  ];
);

#[cfg(test)]
mod tests;
