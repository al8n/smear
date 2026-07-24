//! GraphQL selection productions.
//!
//! These parsers are concrete over [`GraphqlLexer`] and
//! construct slice-typed GraphQL AST nodes. Selection heads are consumed by
//! declining atoms and passed through deterministic [`ParseChoice`] branches
//! into committed tails, so dispatch never classifies an accepted token and
//! then asks another parser to classify it.

use std::vec::Vec;

use smear_lexer::keywords::On;
use tokora::{
  Branch, Lexer, ParseChoice, SimpleSpan, Slice, Source, Token,
  cache::PeekedTokenExt,
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::try_braces,
  punct::{Brace, Bracket, Paren},
  try_parse_input::ParseAttempt,
  utils::{IntoComponents, typenum::U1},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  argument::arguments,
  directive::{directives, directives_after_at},
  fragment_name, try_name,
};
use crate::{
  combinator::{Equivalent, ParseCtx, try_at, try_colon, try_spread},
  graphql::{
    GraphQL,
    ast::{
      Alias, Directives, Field, FragmentName, FragmentSpread, InlineFragment, Name, Selection,
      SelectionSet, TypeCondition,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
    keyword::try_on,
  },
};

macro_rules! selection_parser {
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
        + From<Unclosed<Paren, SimpleSpan, GraphQL>>
        + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
        + From<Unclosed<Brace, SimpleSpan, GraphQL>>
        + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

/// Emits a committed selection-production diagnostic without consuming the
/// rejected token. This runs only after a declining atom established that the
/// required head is absent.
fn expected_selection_phase<'inp, Src, Ctx, T>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<T, GraphqlError<'inp, Src, Ctx>>
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
    peeked
      .pop_front()
      .map(|token| (*token.span(), token.token().kind()))
  };

  match rejected {
    Some((span, kind)) => Err(DialectGraphqlError::unexpected_token(kind, expected, span).into()),
    None => Err(
      DialectGraphqlError::maybe_unexpected_token(None, expected, SimpleSpan::new(offset, offset))
        .into(),
    ),
  }
}

fn take_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Name<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match try_name(inp)? {
    ParseAttempt::Accept(name) => Ok(name),
    ParseAttempt::Decline => expected_selection_phase(inp, Expectation::Name),
  }
}

fn take_spread<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<SimpleSpan, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match try_spread(inp)? {
    ParseAttempt::Accept(spread) => Ok(*spread.span()),
    ParseAttempt::Decline => expected_selection_phase(inp, Expectation::Spread),
  }
}

fn take_on<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<On, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match try_on(inp)? {
    ParseAttempt::Accept(on) => Ok(on),
    ParseAttempt::Decline => expected_selection_phase(inp, Expectation::Keyword("on")),
  }
}

/// Parses the committed name tail of a type condition after `on` is consumed.
fn type_condition_after_on<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  on: On,
) -> Result<TypeCondition<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let name = take_name(inp)?;
  Ok(TypeCondition::new(
    SimpleSpan::new(on.span().start(), name.span().end()),
    name,
  ))
}

selection_parser!(
  /// Parses a committed GraphQL type condition (`on NamedType`).
  ///
  /// The name phase accepts `on` itself, as `NamedType` has no fragment-name
  /// exclusion.
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  pub type_condition,
  inp,
  TypeCondition<GraphqlSlice<'inp, Src>>,
  {
    let on = take_on(inp)?;
    type_condition_after_on(inp, on)
  }
);

selection_parser!(
  /// Attempts a GraphQL type condition (`on NamedType`).
  ///
  /// Declines without consuming unless the first token is `on`. Once `on` is
  /// present, parsing commits to the named type.
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  pub try_type_condition,
  inp,
  ParseAttempt<TypeCondition<GraphqlSlice<'inp, Src>>>,
  {
    match try_on(inp)? {
      ParseAttempt::Accept(on) => type_condition_after_on(inp, on).map(ParseAttempt::Accept),
      ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    }
  }
);

/// Parses a field tail after its first name is already consumed.
fn field_after_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  first_name: Name<GraphqlSlice<'inp, Src>>,
) -> Result<Field<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let start = first_name.span().start();
  let (alias, name) = match try_colon(inp)? {
    ParseAttempt::Accept(colon) => {
      let alias = Alias::new(SimpleSpan::new(start, colon.span().end()), first_name);
      (Some(alias), take_name(inp)?)
    }
    ParseAttempt::Decline => (None, first_name),
  };

  let mut end = name.span().end();
  let arguments = arguments(inp)?;
  if arguments.span().start() != arguments.span().end() {
    end = arguments.span().end();
  }
  let arguments = (!arguments.arguments().is_empty()).then_some(arguments);

  let directives = directives(inp)?;
  if directives.span().start() != directives.span().end() {
    end = directives.span().end();
  }
  let directives = (!directives.directives().is_empty()).then_some(directives);

  let selection_set = match try_selection_set(inp)? {
    ParseAttempt::Accept(selection_set) => {
      end = selection_set.span().end();
      Some(selection_set)
    }
    ParseAttempt::Decline => None,
  };

  Ok(Field::new(
    SimpleSpan::new(start, end),
    alias,
    name,
    arguments,
    directives,
    selection_set,
  ))
}

selection_parser!(
  /// Parses a GraphQL field (`Alias? Name Arguments? Directives? SelectionSet?`).
  ///
  /// The first name becomes an alias only after a committed `:`; the second name
  /// is then required and receives a local `name` diagnostic if it is absent.
  /// Empty arguments and directive collections are represented by `None` on the
  /// field node.
  ///
  /// See the [GraphQL Field specification](https://spec.graphql.org/draft/#Field).
  pub field,
  inp,
  Field<GraphqlSlice<'inp, Src>>,
  {
    let first_name = take_name(inp)?;
    field_after_name(inp, first_name)
  }
);

fn try_selection<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<Selection<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let mut name = None;
  let mut spread = None;
  let branch: Branch<1> = match try_name(inp)? {
    ParseAttempt::Accept(accepted) => {
      name = Some(accepted);
      Branch::B0
    }
    ParseAttempt::Decline => match try_spread(inp)? {
      ParseAttempt::Accept(accepted) => {
        spread = Some(*accepted.span());
        Branch::B1
      }
      ParseAttempt::Decline => return Ok(ParseAttempt::Decline),
    },
  };

  let mut tails = (
    |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      field_after_name(
        inp,
        name
          .take()
          .expect("selected field branch stores its consumed name"),
      )
      .map(Selection::Field)
    },
    |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      selection_after_spread(
        inp,
        spread
          .take()
          .expect("selected spread branch stores its consumed span"),
      )
    },
  );
  tails.parse_choice(inp, &branch).map(ParseAttempt::Accept)
}

selection_parser!(
  /// Parses a GraphQL selection — a field, fragment spread, or inline fragment.
  ///
  /// A deterministic choice routes the first accepted, already-consumed head
  /// directly to its committed tail. A rejected or absent head is left available
  /// for a parent recovery boundary.
  ///
  /// See the [GraphQL Selection specification](https://spec.graphql.org/draft/#Selection).
  pub selection,
  inp,
  Selection<GraphqlSlice<'inp, Src>>,
  {
    match try_selection(inp)? {
      ParseAttempt::Accept(selection) => Ok(selection),
      ParseAttempt::Decline => expected_selection_phase(inp, Expectation::Selection),
    }
  }
);

fn fragment_spread_after_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
  name: FragmentName<GraphqlSlice<'inp, Src>>,
) -> Result<FragmentSpread<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let mut end = name.span().end();
  let directives = directives(inp)?;
  if directives.span().start() != directives.span().end() {
    end = directives.span().end();
  }
  let directives = (!directives.directives().is_empty()).then_some(directives);
  Ok(FragmentSpread::new(
    SimpleSpan::new(start, end),
    name,
    directives,
  ))
}

selection_parser!(
  /// Parses a committed named GraphQL fragment spread (`... FragmentName Directives?`).
  ///
  /// Empty directives are represented by `None` on the AST node.
  ///
  /// See the [GraphQL Fragment Spread specification](https://spec.graphql.org/draft/#FragmentSpread).
  pub fragment_spread,
  inp,
  FragmentSpread<GraphqlSlice<'inp, Src>>,
  {
    let spread = take_spread(inp)?;
    let name = fragment_name(inp)?;
    fragment_spread_after_name(inp, spread.start(), name)
  }
);

fn inline_fragment_after_directives<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
  type_condition: Option<TypeCondition<GraphqlSlice<'inp, Src>>>,
  directives: Directives<GraphqlSlice<'inp, Src>>,
) -> Result<InlineFragment<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = (!directives.directives().is_empty()).then_some(directives);
  let selection_set = selection_set(inp)?;
  Ok(InlineFragment::new(
    SimpleSpan::new(start, selection_set.span().end()),
    type_condition,
    directives,
    selection_set,
  ))
}

fn typed_inline_fragment_after_type_condition<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
  type_condition: TypeCondition<GraphqlSlice<'inp, Src>>,
) -> Result<InlineFragment<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = directives(inp)?;
  inline_fragment_after_directives(inp, start, Some(type_condition), directives)
}

fn untyped_inline_fragment_after_at<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
  at: tokora::punct::At<SimpleSpan, (), GraphQL>,
) -> Result<InlineFragment<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = directives_after_at(inp, at)?;
  inline_fragment_after_directives(inp, start, None, directives)
}

fn untyped_inline_fragment_after_spread<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InlineFragment<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = directives(inp)?;
  inline_fragment_after_directives(inp, start, None, directives)
}

fn untyped_inline_fragment_from_selection_set<S>(
  start: usize,
  selection_set: SelectionSet<S>,
) -> InlineFragment<S> {
  InlineFragment::new(
    SimpleSpan::new(start, selection_set.span().end()),
    None,
    None,
    selection_set,
  )
}

fn selection_after_spread<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  spread: SimpleSpan,
) -> Result<Selection<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let mut on = None;
  let mut name = None;
  let mut at = None;
  let mut selection_set = None;
  let branch: Branch<3> = match try_name(inp)? {
    ParseAttempt::Accept(accepted) if "on".equivalent(accepted.source()) => {
      on = Some(On::new(accepted.span()));
      Branch::B0
    }
    ParseAttempt::Accept(accepted) => {
      let (span, source) = accepted.into_components();
      name = Some(FragmentName::new(span, source));
      Branch::B1
    }
    ParseAttempt::Decline => match try_at(inp)? {
      ParseAttempt::Accept(accepted) => {
        at = Some(accepted);
        Branch::B2
      }
      ParseAttempt::Decline => match try_selection_set(inp)? {
        ParseAttempt::Accept(accepted) => {
          selection_set = Some(accepted);
          Branch::B3
        }
        ParseAttempt::Decline => {
          return expected_selection_phase(inp, Expectation::FragmentName);
        }
      },
    },
  };

  let mut tails = (
    |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let type_condition = type_condition_after_on(
        inp,
        on.take()
          .expect("selected type-condition branch stores its consumed `on`"),
      )?;
      typed_inline_fragment_after_type_condition(inp, spread.start(), type_condition)
        .map(Selection::InlineFragment)
    },
    |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      fragment_spread_after_name(
        inp,
        spread.start(),
        name
          .take()
          .expect("selected fragment-spread branch stores its consumed name"),
      )
      .map(Selection::FragmentSpread)
    },
    |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      untyped_inline_fragment_after_at(
        inp,
        spread.start(),
        at.take()
          .expect("selected inline-fragment branch stores its consumed `@`"),
      )
      .map(Selection::InlineFragment)
    },
    |_inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      Ok(Selection::InlineFragment(
        untyped_inline_fragment_from_selection_set(
          spread.start(),
          selection_set
            .take()
            .expect("selected inline-fragment branch stores its selection set"),
        ),
      ))
    },
  );
  tails.parse_choice(inp, &branch)
}

selection_parser!(
  /// Parses a committed GraphQL inline fragment (`... TypeCondition? Directives? SelectionSet`).
  ///
  /// A deterministic choice routes the `on` atom, when accepted, into the
  /// typed tail exactly once. Every other tail uses the untyped fragment core.
  ///
  /// See the [GraphQL Inline Fragment specification](https://spec.graphql.org/draft/#InlineFragment).
  pub inline_fragment,
  inp,
  InlineFragment<GraphqlSlice<'inp, Src>>,
  {
    let spread = take_spread(inp)?;
    let mut on = None;
    let branch: Branch<1> = match try_on(inp)? {
      ParseAttempt::Accept(accepted) => {
        on = Some(accepted);
        Branch::B0
      }
      ParseAttempt::Decline => Branch::B1,
    };

    let mut tails = (
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let type_condition = type_condition_after_on(
          inp,
          on.take()
            .expect("selected inline-fragment branch stores its consumed `on`"),
        )?;
        typed_inline_fragment_after_type_condition(inp, spread.start(), type_condition)
      },
      |inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        untyped_inline_fragment_after_spread(inp, spread.start())
      },
    );
    tails.parse_choice(inp, &branch)
  }
);

fn selection_set_contents<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<Vec<Selection<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let first = selection(inp)?;
  let mut selections = Vec::new();
  selections.push(first);

  loop {
    match try_selection(inp)? {
      ParseAttempt::Accept(selection) => selections.push(selection),
      ParseAttempt::Decline => {
        let stop = {
          let mut peeked = inp.peek::<U1>()?;
          match peeked.pop_front() {
            Some(token) => token.token().is_r_brace(),
            None => true,
          }
        };
        if stop {
          break;
        }
        return expected_selection_phase(inp, Expectation::Selection);
      }
    }
  }

  Ok(selections)
}

/// Attempts a nonempty GraphQL selection set. It declines without consuming
/// when `{` is absent; once an opener is consumed, Tokora owns close and
/// `Unclosed<Brace>` handling.
fn try_selection_set<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<SelectionSet<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
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
    > + From<Unclosed<Paren, SimpleSpan, GraphQL>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
    + From<Unclosed<Brace, SimpleSpan, GraphQL>>
    + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  match try_braces(selection_set_contents::<Src, Ctx>)(inp)? {
    Some(delimited) => {
      let (span, _open, _close, selections) = delimited.into_components();
      Ok(ParseAttempt::Accept(SelectionSet::new(span, selections)))
    }
    None => Ok(ParseAttempt::Decline),
  }
}

selection_parser!(
  /// Parses a nonempty GraphQL selection set (`{ Selection+ }`).
  ///
  /// The first selection commits the `+` cardinality. The delimiter combinator
  /// emits `Unclosed<Brace>` when a real opener has no closer.
  ///
  /// See the [GraphQL Selection Sets specification](https://spec.graphql.org/draft/#SelectionSet).
  pub selection_set,
  inp,
  SelectionSet<GraphqlSlice<'inp, Src>>,
  {
    match try_selection_set(inp)? {
      ParseAttempt::Accept(selection_set) => Ok(selection_set),
      ParseAttempt::Decline => expected_selection_phase(inp, Expectation::LBrace),
    }
  }
);

macro_rules! impl_selection_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident) => {
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
        str: Equivalent<$slice>,
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
          + From<Unclosed<Paren, SimpleSpan, GraphQL>>
          + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

macro_rules! impl_selection_try_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn try_graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        str: Equivalent<$slice>,
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
          + From<Unclosed<Paren, SimpleSpan, GraphQL>>
          + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_selection_api!(
  /// Parses a committed GraphQL field.
  ///
  /// See the [GraphQL Field specification](https://spec.graphql.org/draft/#Field).
  S,
  Field<S>,
  field
);

impl_selection_api!(
  /// Parses a committed GraphQL type condition (`on NamedType`).
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  S,
  TypeCondition<S>,
  type_condition
);

impl_selection_try_api!(
  /// Attempts a GraphQL type condition (`on NamedType`).
  ///
  /// Declines without consuming on a head mismatch.
  ///
  /// See the [GraphQL Type Condition specification](https://spec.graphql.org/draft/#TypeCondition).
  S,
  TypeCondition<S>,
  try_type_condition
);

impl_selection_api!(
  /// Parses a committed named GraphQL fragment spread.
  ///
  /// See the [GraphQL Fragment Spread specification](https://spec.graphql.org/draft/#FragmentSpread).
  S,
  FragmentSpread<S>,
  fragment_spread
);

impl_selection_api!(
  /// Parses a committed GraphQL inline fragment.
  ///
  /// See the [GraphQL Inline Fragment specification](https://spec.graphql.org/draft/#InlineFragment).
  S,
  InlineFragment<S>,
  inline_fragment
);

impl_selection_api!(
  /// Parses one committed GraphQL selection.
  ///
  /// See the [GraphQL Selection specification](https://spec.graphql.org/draft/#Selection).
  S,
  Selection<S>,
  selection
);

impl_selection_api!(
  /// Parses a nonempty GraphQL selection set.
  ///
  /// See the [GraphQL Selection Sets specification](https://spec.graphql.org/draft/#SelectionSet).
  S,
  SelectionSet<S>,
  selection_set
);

#[cfg(test)]
mod tests;
