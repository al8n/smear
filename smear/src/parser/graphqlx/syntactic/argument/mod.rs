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
  Accumulator, EmitterView, Lexer, ParseInput, SimpleSpan, Slice, Source, Token,
  cache::{Peeked, PeekedTokenExt},
  parser::{Action, try_parens},
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use crate::lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, try_name,
  unexpected_here,
  value::{const_value, value},
};
use crate::parser::{
  combinator::{ParseCtx, TokenSpannedExt, extent_end, extent_since, extent_start, try_colon},
  graphqlx::{
    GraphQLx,
    ast::{Argument, Arguments, ConstArgument, ConstArguments, Name},
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! argument_parser {
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

fn take_argument_name<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_name(inp)? {
    ParseAttempt::Accept(name) => Ok(name),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Name),
  }
}

fn take_argument_colon<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<(), GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match try_colon(inp)? {
    ParseAttempt::Accept(_) => Ok(()),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Colon),
  }
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
  {
    take_argument_name
      .then_ignore(take_argument_colon)
      .then(value)
      .token_spanned()
      .map(|Spanned { span, data: (name, value) }| Argument::new(span, name, value))
      .parse_input(inp)
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
  {
    take_argument_name
      .then_ignore(take_argument_colon)
      .then(const_value)
      .token_spanned()
      .map(|Spanned { span, data: (name, value) }| ConstArgument::new(span, name, value))
      .parse_input(inp)
  }
);

fn decide_argument_head<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RParen) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

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
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::parser::combinator::extent`.
    let start = extent_end(inp);
    // Not the `Delimited`'s own span: tokora builds that one from the lookahead cursor at both
    // ends, so it opens before the `(`'s leading trivia and closes wherever the peek past `)`
    // landed. See `crate::parser::combinator::extent`.
    let node_start = extent_start(inp)?;
    let parsed = try_parens::<_, _, _, _, Vec<Argument<GraphqlxSlice<'inp, Src>>>, _>(
      (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| argument(inp))
        .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx>)
        .collect_with(Vec::new()),
    )(inp)?;
    Ok(match parsed {
      Some(arguments) => {
        let (_, _, _, arguments) = arguments.into_components();
        Arguments::new(extent_since(inp, node_start), arguments)
      }
      None => Arguments::new(SimpleSpan::new(start, start), Vec::new()),
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
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See `crate::parser::combinator::extent`.
    let start = extent_end(inp);
    // Not the `Delimited`'s own span — see `arguments` above and `crate::parser::combinator::extent`.
    let node_start = extent_start(inp)?;
    let parsed = try_parens::<_, _, _, _, Vec<ConstArgument<GraphqlxSlice<'inp, Src>>>, _>(
      (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| const_argument(inp))
        .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx>)
        .collect_with(Vec::new()),
    )(inp)?;
    Ok(match parsed {
      Some(arguments) => {
        let (_, _, _, arguments) = arguments.into_components();
        ConstArguments::new(extent_since(inp, node_start), arguments)
      }
      None => ConstArguments::new(SimpleSpan::new(start, start), Vec::new()),
    })
  }
);

macro_rules! impl_argument_api {
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
);

#[cfg(test)]
mod tests;
