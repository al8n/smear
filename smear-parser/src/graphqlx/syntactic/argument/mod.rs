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
  Accumulator, Action, EmitterView, Lexer, ParseAttempt, ParseInput, SimpleSpan, Slice, Source,
  Token,
  cache::{Peeked, PeekedTokenExt},
  combinator::{parens, try_parens},
  span::Spanned,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, try_name,
  unexpected_here,
  value::{const_value, expression_refused, goes_on_with_an_expression, schema_value, value},
};
use crate::{
  combinator::{ParseCtx, try_colon},
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
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
        + DowncastRef<ContextualKeyword>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
        State = smear_lexer::graphqlx::syntactic::SyntacticState,
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
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
      .spanned_trimmed()
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
  { const_argument_body::<Src, Ctx, false>(inp) }
);

/// The constant argument both constant grammars read, its value read by an executable document's
/// constant grammar, or by a schema position's when `SCHEMA` holds.
#[inline(always)]
fn const_argument_body<'inp, Src, Ctx, const SCHEMA: bool>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstArgument<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  take_argument_name
    .then_ignore(take_argument_colon)
    .then(|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      if SCHEMA {
        schema_value(inp)
      } else {
        const_value(inp)
      }
    })
    .spanned_trimmed()
    .map(
      |Spanned {
         span,
         data: (name, value),
       }| ConstArgument::new(span, name, value),
    )
    .parse_input(inp)
}

/// Parses one committed GraphQLx `Argument` production in a schema position: an argument of a
/// directive that a type-system definition or extension applies.
///
/// Every argument [`const_argument`] reads is one here, and its value is a schema value: a typed
/// object or a typed value is one too.
pub fn schema_argument<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstArgument<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  const_argument_body::<Src, Ctx, true>(inp)
}

/// The member test of an argument list: a `)` ends the arguments, and in a schema position, where
/// `SCHEMA` holds, what goes on with an expression is refused at the token the test peeked, against
/// the block it offers.
fn decide_argument_head<'inp, Src, Ctx, const SCHEMA: bool>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RParen) => Ok(Action::Stop),
    Some(token)
      if SCHEMA && goes_on_with_an_expression::<Src>(token.token().kind(), token.token()) =>
    {
      Err(expression_refused::<Src, Ctx>(
        token.token().kind(),
        *token.span(),
      ))
    }
    Some(_) => Ok(Action::Continue),
    None => Ok(Action::Stop),
  }
}

argument_parser!(
  /// The `(`-committed half of [`arguments`], reachable on its own so a caller that has
  /// already classified the head as `(` enters it without probing for the opener again.
  pub(super) committed_arguments,
  inp,
  Arguments<GraphqlxSlice<'inp, Src>>;
  {
    // Not the `Delimited`'s own span: tokora builds that one from the lookahead cursor at both
    // ends, so it opens before the `(`'s leading trivia and closes wherever the peek past `)`
    // landed. See tokora's `InputRef::cursor_trimmed`.
    let node_start = inp.cursor_trimmed()?;
    let parsed = parens::<_, _, _, _, Vec<Argument<GraphqlxSlice<'inp, Src>>>, _>(
      (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| argument(inp))
        .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx, false>)
        .collect_with(Vec::new()),
    )(inp)?;
    let (_, _, _, arguments) = parsed.into_components();
    Ok(Arguments::new(inp.span_since_trimmed(&node_start), arguments))
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
  {
    // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
    // token, so a caller that left a peek in the cache would anchor this absent collection past
    // the token that follows it. See tokora's `InputRef::span_since_trimmed`.
    let start = inp.span().end();
    if inp.peek_kind()? == Some(SyntacticTokenKind::LParen) {
      committed_arguments(inp)
    } else {
      Ok(Arguments::new(SimpleSpan::new(start, start), Vec::new()))
    }
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
  { const_arguments_body::<Src, Ctx, false>(inp) }
);

/// The constant argument list both constant grammars read, each argument read by
/// [`const_argument`], or by [`schema_argument`] when `SCHEMA` holds.
#[inline(always)]
fn const_arguments_body<'inp, Src, Ctx, const SCHEMA: bool>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstArguments<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  // The **committed** end, not `inp.offset()`: `offset()` reports the end of the newest *lexed*
  // token, so a caller that left a peek in the cache would anchor this absent collection past
  // the token that follows it. See tokora's `InputRef::span_since_trimmed`.
  let start = inp.span().end();
  // Not the `Delimited`'s own span — see `arguments` above and tokora's
  // `InputRef::cursor_trimmed`.
  let node_start = inp.cursor_trimmed()?;
  let parsed = try_parens::<_, _, _, _, Vec<ConstArgument<GraphqlxSlice<'inp, Src>>>, _>(
    (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      if SCHEMA {
        schema_argument(inp)
      } else {
        const_argument(inp)
      }
    })
    .repeated_while::<_, U1>(decide_argument_head::<Src, Ctx, SCHEMA>)
    .collect_with(Vec::new()),
  )(inp)?;
  Ok(match parsed {
    Some(arguments) => {
      let (_, _, _, arguments) = arguments.into_components();
      ConstArguments::new(inp.span_since_trimmed(&node_start), arguments)
    }
    None => ConstArguments::new(SimpleSpan::new(start, start), Vec::new()),
  })
}

/// Parses a GraphQLx `Arguments` collection in a schema position: the arguments of a directive that
/// a type-system definition or extension applies.
///
/// When `(` is absent, this returns an empty zero-width collection without consuming input. Every
/// collection [`const_arguments`] reads is one here, and each argument is a [`schema_argument`].
pub fn schema_arguments<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ConstArguments<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  <GraphqlxSlice<'inp, Src> as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>: Lexer<
      'inp,
      Source = Src,
      Token = GraphqlxToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = smear_lexer::graphqlx::syntactic::SyntacticState,
    >,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  const_arguments_body::<Src, Ctx, true>(inp)
}

macro_rules! impl_argument_api {
  (
    $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident;
  ) => {
    impl_argument_api!(@impl graphqlx; $(#[$meta])* $slice, $node, $parser;);
  };
  // A schema position's twin, under the method name the invocation gives.
  (
    $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident as $method:ident;
  ) => {
    impl_argument_api!(@impl $method; $(#[$meta])* $slice, $node, $parser;);
  };
  (
    @impl $method:ident; $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident;
  ) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn $method<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp + $crate::value::Leaf,
        <$slice as Slice<'inp>>::Char: tokora::utils::IsAsciiChar,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + DowncastRef<ContextualKeyword>,
        GraphqlxLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlxToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
          State = smear_lexer::graphqlx::syntactic::SyntacticState,
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
impl_argument_api!(
  /// Parses one committed GraphQLx argument in a schema position.
  ///
  /// See [`schema_argument`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  ConstArgument<S>,
  schema_argument as graphqlx_schema;
);
impl_argument_api!(
  /// Parses GraphQLx arguments in a schema position, returning an empty zero-width list without
  /// consuming input when `(` is absent.
  ///
  /// See [`schema_arguments`] and the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
  S,
  ConstArguments<S>,
  schema_arguments as graphqlx_schema;
);

#[cfg(test)]
mod tests;
