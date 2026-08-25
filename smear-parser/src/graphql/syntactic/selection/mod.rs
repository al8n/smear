//! GraphQL selection productions.
//!
//! These parsers are concrete over [`GraphqlLexer`] and
//! construct slice-typed GraphQL AST nodes. Selection dispatch lexes its first
//! token once with [`ParseTokenChoice::fused_dispatch_on_kind`], then passes the
//! already-consumed head to its selected arm before entering a committed tail.
//! This avoids staging the head only to classify it again. The field *tail* is
//! fused the same way, on `field_after_name`'s one-classification-per-position
//! rule.

use smear_lexer::{
  graphql::{ContextualKeyword, syntactic::SyntacticTokenKind},
  keywords::On,
};
use tokora::{
  Accumulator, Branch, EmitterView, Lexer, ParseChoice, ParseInput, ParseTokenChoice, SimpleSpan,
  Slice, Source, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  argument::committed_arguments,
  directive::{directives, directives_after_at},
  fragment_name, try_name,
};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, at, try_colon, try_spread},
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
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

/// Emits a committed selection-production diagnostic without consuming the
/// rejected token. This runs only after a declining probe or classifier
/// established that the required head is absent.
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
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
///
/// Every tail position is decided by **one** classification of **one** token, which then
/// jumps straight into the committed production that classification selected: `(`
/// arguments, `@` directives, `{` sub-selection, anything else closes the field. The four
/// optional tails used to run as four independent probes asking that same lookahead token
/// four different yes/no questions — one read of its kind answers all of them — and the
/// two collection tails additionally built an empty carrier per absent tail only to
/// span-test and `is_empty`-test it away. On a field-dense document no tail is ever taken,
/// so that was the whole of the work.
///
/// [`try_colon`] keeps a probe of its own rather than joining the classification, because
/// it is the consume-in-place form: an alias costs one lex with no cache round trip, and
/// a field without one leaves the token at the cache front, where [`peek_kind`] then reads
/// it in place. Folding it into the classification would trade the aliased field's saved
/// round trip for nothing.
///
/// [`peek_kind`]: tokora::InputRef::peek_kind
fn field_after_name<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  first_name: Name<GraphqlSlice<'inp, Src>>,
) -> Result<Field<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
  let mut head = inp.peek_kind()?;

  let arguments = if head == Some(SyntacticTokenKind::LParen) {
    let arguments = committed_arguments(inp)?;
    end = arguments.span().end();
    head = inp.peek_kind()?;
    // The lenient `()` spelling keeps its real delimiter span in `end` and still stores
    // `None`, exactly as the empty-carrier form did.
    (!arguments.arguments().is_empty()).then_some(arguments)
  } else {
    None
  };

  let directives = if head == Some(SyntacticTokenKind::At) {
    // A run entered on `@` always holds at least one directive, so the emptiness test the
    // carrier form needed here cannot fire.
    let at = at(inp)?;
    let directives = directives_after_at(inp, at)?;
    end = directives.span().end();
    head = inp.peek_kind()?;
    Some(directives)
  } else {
    None
  };

  let selection_set = if head == Some(SyntacticTokenKind::LBrace) {
    let selection_set = committed_selection_set(inp)?;
    end = selection_set.span().end();
    Some(selection_set)
  } else {
    None
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

#[inline]
fn try_selection<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<Selection<GraphqlSlice<'inp, Src>>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let field_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| match token {
      GraphqlToken::<'inp, Src>::Identifier(source) => {
        field_after_name(inp, Name::new(span, source)).map(Selection::Field)
      }
      _ => unreachable!("fused field arm received a non-identifier token"),
    };
  let spread_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      if !matches!(token, GraphqlToken::<'inp, Src>::Spread) {
        unreachable!("fused spread arm received a non-spread token");
      }

      let name = inp.try_expect_map(|token| {
        let token = token.into_data();
        matches!(token, GraphqlToken::<'inp, Src>::Identifier(_)).then(|| {
          <GraphqlToken<'inp, Src> as DowncastRef<ContextualKeyword>>::downcast_ref(token)
            == Some(ContextualKeyword::On)
        })
      })?;
      match name {
        Some((
          is_on,
          Spanned {
            span: name_span,
            data: token,
          },
        )) => match token {
          GraphqlToken::<'inp, Src>::Identifier(_) if is_on => {
            let type_condition = type_condition_after_on(inp, On::new(name_span))?;
            typed_inline_fragment_after_type_condition(inp, span.start(), type_condition)
              .map(Selection::InlineFragment)
          }
          GraphqlToken::<'inp, Src>::Identifier(source) => {
            let name = FragmentName::new(name_span, source);
            fragment_spread_after_name(inp, span.start(), name).map(Selection::FragmentSpread)
          }
          _ => unreachable!("identifier expectation consumed a non-identifier token"),
        },
        None => {
          let untyped = {
            let mut peeked = inp.peek::<U1>()?;
            match peeked.pop_front() {
              Some(token) => matches!(
                token.token(),
                GraphqlToken::<'inp, Src>::At | GraphqlToken::<'inp, Src>::LBrace
              ),
              None => false,
            }
          };
          if untyped {
            untyped_inline_fragment_after_spread(inp, span.start()).map(Selection::InlineFragment)
          } else {
            expected_selection_phase(inp, Expectation::FragmentName)
          }
        }
      }
    };
  (field_head_arm, spread_head_arm)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier, SyntacticTokenKind::Spread])
    .try_parse_input(inp)
}

selection_parser!(
  /// Parses a GraphQL selection — a field, fragment spread, or inline fragment.
  ///
  /// A fused deterministic choice lexes its first token once, then passes that
  /// head to the selected arm before entering a committed tail. A rejected or
  /// absent first head is left available for a parent recovery boundary.
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
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = directives(inp)?;
  inline_fragment_after_directives(inp, start, Some(type_condition), directives)
}

fn untyped_inline_fragment_after_spread<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<InlineFragment<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let directives = directives(inp)?;
  inline_fragment_after_directives(inp, start, None, directives)
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

/// Continues through every non-closer token so the committed [`selection`]
/// parser emits its local diagnostic for an invalid collection item. The
/// delimited repetition probes `}` before consulting this decision.
fn decide_selection_set_tail<'inp, Src, Ctx>(
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
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

selection_parser!(
  committed_selection_set,
  inp,
  SelectionSet<GraphqlSlice<'inp, Src>>,
  {
    selection
      .repeated_while::<_, U1>(decide_selection_set_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      // `Nested`, by way of `Default`, exactly as the value productions collect theirs: the
      // container is where a selection set's release lives.
      .collect_with(Default::default())
      .token_spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| SelectionSet::new(span, data))
  }
);

selection_parser!(
  /// Parses a nonempty GraphQL selection set (`{ Selection+ }`).
  ///
  /// The native `at_least(1)` repetition enforces the `+` cardinality. The
  /// delimiter combinator emits `Unclosed<Brace>` when a real opener has no
  /// closer.
  ///
  /// See the [GraphQL Selection Sets specification](https://spec.graphql.org/draft/#SelectionSet).
  pub selection_set,
  inp,
  SelectionSet<GraphqlSlice<'inp, Src>>,
  {
    committed_selection_set(inp)
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
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<$slice>>,
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
        GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<$slice>>,
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
