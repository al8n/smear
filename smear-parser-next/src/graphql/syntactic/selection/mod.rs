//! GraphQL selection productions.
//!
//! These parsers are concrete over [`GraphqlLexer`] and
//! construct slice-typed GraphQL AST nodes. A fixed two-token dispatcher keeps
//! the `...` fork local: `... on` begins a typed inline fragment, `... @` and
//! `... {` begin untyped inline fragments, and every other spread tail remains
//! a branded `FragmentName` parse.

use std::vec::Vec;

use smear_lexer::keywords::On;
use tokora::{
  Accumulator, Branch, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source, Token,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Brace, Bracket, Paren},
  utils::typenum::{U1, U2},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken, argument::arguments,
  directive::directives, fragment_name, name, peeks_where,
};
use crate::{
  combinator::{Equivalent, ParseCtx},
  graphql::{
    GraphQL,
    ast::{
      Alias, Field, FragmentSpread, InlineFragment, Name, Selection, SelectionSet, TypeCondition,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
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

/// Checks a committed selection-production phase without consuming a rejected
/// token. Lookahead carries only the rejected token's span and kind into the
/// diagnostic, never its source slice or the token itself.
fn guard_selection_phase<'inp, Src, Ctx>(
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

#[inline]
fn is_on<'inp, Src>(token: &GraphqlToken<'inp, Src>) -> bool
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  str: Equivalent<GraphqlSlice<'inp, Src>>,
{
  matches!(token, GraphqlToken::<'inp, Src>::Identifier(value) if "on".equivalent(value))
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
  guard_selection_phase(inp, Expectation::Name, |token| token.is_identifier())?;
  name(inp)
}

fn take_colon<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<(), GraphqlError<'inp, Src, Ctx>>
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
  guard_selection_phase(inp, Expectation::Colon, |token| token.is_colon())?;
  match inp.next()? {
    Some(_) => Ok(()),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
}

fn take_spread<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<(), GraphqlError<'inp, Src, Ctx>>
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
  guard_selection_phase(inp, Expectation::Spread, |token| token.is_spread())?;
  match inp.next()? {
    Some(_) => Ok(()),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
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
  guard_selection_phase(inp, Expectation::Keyword("on"), is_on::<Src>)?;
  match inp.next()? {
    Some(spanned) => Ok(On::new(spanned.into_span())),
    None => Err(UnexpectedEot::eot_of(*inp.offset()).into()),
  }
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
    let name = take_name(inp)?;
    Ok(TypeCondition::new(
      SimpleSpan::new(on.span().start(), name.span().end()),
      name,
    ))
  }
);

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
    let cursor = *inp.cursor();
    let first_name = take_name(inp)?;
    let (alias, name) = if peeks_where(inp, |token| token.is_colon())? {
      take_colon(inp)?;
      let alias = Alias::new(inp.span_since(&cursor), first_name);
      (Some(alias), take_name(inp)?)
    } else {
      (None, first_name)
    };

    let arguments = arguments(inp)?;
    let arguments = if arguments.arguments().is_empty() {
      None
    } else {
      Some(arguments)
    };
    let directives = directives(inp)?;
    let directives = if directives.directives().is_empty() {
      None
    } else {
      Some(directives)
    };
    let selection_set = if peeks_where(inp, |token| token.is_l_brace())? {
      Some(selection_set(inp)?)
    } else {
      None
    };

    Ok(Field::new(
      inp.span_since(&cursor),
      alias,
      name,
      arguments,
      directives,
      selection_set,
    ))
  }
);

selection_parser!(
  /// Parses a GraphQL selection — a field, fragment spread, or inline fragment.
  ///
  /// A fixed two-token dispatch selects a field, fragment spread, typed inline
  /// fragment, or untyped inline fragment. A rejected or absent first token is
  /// non-consuming so a parent can recover at the same position; all other
  /// spread tails commit to the fragment-spread branch and retain its local
  /// `FragmentName` diagnostic.
  ///
  /// See the [GraphQL Selection specification](https://spec.graphql.org/draft/#Selection).
  pub selection,
  inp,
  Selection<GraphqlSlice<'inp, Src>>,
  {
    let offset = *inp.offset();
    (
      field.map(Selection::Field),
      fragment_spread.map(Selection::FragmentSpread),
      typed_inline_fragment.map(Selection::InlineFragment),
      untyped_inline_fragment.map(Selection::InlineFragment),
    )
      .peek_then_choice::<_, U2>(|mut peeked, _| {
        let Some(head) = peeked.pop_front() else {
          return Err(
            DialectGraphqlError::maybe_unexpected_token(
              None,
              Expectation::Selection,
              SimpleSpan::new(offset, offset),
            )
            .into(),
          );
        };

        match head.token() {
          token if token.is_identifier() => Ok(Branch::B0),
          token if token.is_spread() => match peeked.pop_front() {
            Some(next) if is_on::<Src>(next.token()) => Ok(Branch::B2),
            Some(next) if next.token().is_at() || next.token().is_l_brace() => Ok(Branch::B3),
            _ => Ok(Branch::B1),
          },
          token => Err(
            DialectGraphqlError::unexpected_token(
              token.kind(),
              Expectation::Selection,
              *head.span(),
            )
            .into(),
          ),
        }
      })
      .parse_input(inp)
  }
);

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
    let cursor = *inp.cursor();
    take_spread(inp)?;
    let name = fragment_name(inp)?;
    let directives = directives(inp)?;
    let directives = if directives.directives().is_empty() {
      None
    } else {
      Some(directives)
    };
    Ok(FragmentSpread::new(
      inp.span_since(&cursor),
      name,
      directives,
    ))
  }
);

/// Parses the typed inline-fragment core after a `... on` head.
fn typed_inline_fragment<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
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
  let cursor = *inp.cursor();
  take_spread(inp)?;
  let type_condition = type_condition(inp)?;
  let directives = directives(inp)?;
  let directives = if directives.directives().is_empty() {
    None
  } else {
    Some(directives)
  };
  guard_selection_phase(inp, Expectation::LBrace, |token| token.is_l_brace())?;
  let selection_set = selection_set(inp)?;
  Ok(InlineFragment::new(
    inp.span_since(&cursor),
    Some(type_condition),
    directives,
    selection_set,
  ))
}

/// Parses the untyped inline-fragment core after a non-`on` tail.
fn untyped_inline_fragment<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
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
  let cursor = *inp.cursor();
  take_spread(inp)?;
  let directives = directives(inp)?;
  let directives = if directives.directives().is_empty() {
    None
  } else {
    Some(directives)
  };
  guard_selection_phase(inp, Expectation::LBrace, |token| token.is_l_brace())?;
  let selection_set = selection_set(inp)?;
  Ok(InlineFragment::new(
    inp.span_since(&cursor),
    None,
    directives,
    selection_set,
  ))
}

selection_parser!(
  /// Parses a committed GraphQL inline fragment (`... TypeCondition? Directives? SelectionSet`).
  ///
  /// A fixed two-token dispatch chooses the typed form only for `... on`; every
  /// other input enters the untyped core, which retains its local spread and
  /// selection-set diagnostics without backtracking.
  ///
  /// See the [GraphQL Inline Fragment specification](https://spec.graphql.org/draft/#InlineFragment).
  pub inline_fragment,
  inp,
  InlineFragment<GraphqlSlice<'inp, Src>>,
  {
    (typed_inline_fragment, untyped_inline_fragment)
      .peek_then_choice::<_, U2>(|mut peeked, _| {
        let typed = match peeked.pop_front() {
          Some(head) if head.token().is_spread() => {
            matches!(peeked.pop_front(), Some(next) if is_on::<Src>(next.token()))
          }
          _ => false,
        };
        Ok(if typed { Branch::B0 } else { Branch::B1 })
      })
      .parse_input(inp)
  }
);

fn decide_selection_head<'inp, Src, Ctx>(
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
    Some(token) if matches!(token.token(), GraphqlToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
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
    guard_selection_phase(inp, Expectation::LBrace, |token| token.is_l_brace())?;
    (|inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| -> Result<
      Vec<Selection<GraphqlSlice<'inp, Src>>>,
      GraphqlError<'inp, Src, Ctx>,
    > {
      let first = selection(inp)?;
      let mut rest: Vec<Selection<GraphqlSlice<'inp, Src>>> = selection
        .repeated_while::<_, U1>(decide_selection_head::<Src, Ctx>)
        .collect_with(Vec::new())
        .parse_input(inp)?;
      rest.insert(0, first);
      Ok(rest)
    })
      .delimited_by_braces()
      .parse_input(inp)
      .map(|delimited| {
        let (span, _open, _close, selections) = delimited.into_components();
        SelectionSet::new(span, selections)
      })
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
