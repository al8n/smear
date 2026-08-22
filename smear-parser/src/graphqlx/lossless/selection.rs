//! Selection productions: `Alias`, `Field`, `TypeCondition`, `FragmentSpread`, `InlineFragment`,
//! `SelectionSet`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # `TypeCondition` **is** a node here, where GraphQL's kind space has none
//!
//! GraphQL's lossless `type_condition` opens nothing and lets the condition surface as the
//! `NamedType` after the keyword, because its space has no image for one. GraphQLx's does —
//! `graphqlx::ast::TypeCondition` is a carrier the census found and admitted — so the `on` and the
//! [`TypePath`](K::TypePath) after it are one region and the keyword is *inside* it. This is a
//! difference the divergence table does not list and the kind space does, and the kind space wins.
//!
//! # The two ambiguities this file exists to resolve, and the two extra arms GraphQLx adds
//!
//! - **`Alias` versus a bare field name.** Both start with an `Identifier` and only the following
//!   `:` tells them apart, which is what [`tokora::parser::node_at`] exists for: mint a mark,
//!   commit the name, and hand the mark to a *declining* probe that spends it only if the `:` is
//!   there. An `eat_if` plus an unconditional wrap cannot express this — the `:` would be
//!   committed outside the wrap's parser.
//! - **`FragmentSpread` versus `InlineFragment`,** whose kind is known only after the token *after*
//!   the `...` is read — and, for an `Identifier`, only after its **spelling** is read. The same
//!   mark-plus-`node_at` idiom serves, so the `...` lands inside its node rather than beside it.
//!
//! Divergence 11 gives that dispatch **four** arms against GraphQL's three
//! (`graphqlx/syntactic/selection/mod.rs:409-447`): `on` ⇒ inline fragment, `Identifier` ⇒ spread,
//! **`PathSeparator` ⇒ fully qualified spread**, `At | LBrace` ⇒ untyped inline fragment. And a
//! spread's target is a `TypePath`, so `... ns::F<Int>` is one spread rather than a spread
//! followed by junk.
//!
//! # A `Field` ends with its trailing trivia inside it
//!
//! A field's last three components are all optional, so learning that they are absent means
//! crossing whatever follows the field — while the field's node is still open. A node's extent is
//! `[mark, now]`; `cst_finish` has no `_at` form, so a node cannot be ended in the past. Text
//! fidelity is untouched; only the node's own `text_range` is one trivia run longer.
//! `SelectionSet` is immune, its `}` ending the node before anything after it is read.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, TryParseInput as _, try_parse_input::ParseAttempt};

use crate::graphqlx::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, Keyword,
  directive::{arguments, directives},
  recover,
  recover::{SELECTION_HEADS, SPREAD_TAIL_HEADS, TYPE_CONDITION_HEADS, opener_span},
  trivia::{eat_if, expect, peek_as, peek_kind, try_eat},
  ty::type_path,
  value::Constness,
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `on TypePath` — the type condition of an inline fragment or a fragment definition.
  ///
  /// **Opens its own [`TypeCondition`](K::TypeCondition) node**, unlike GraphQL's twin; see the
  /// module docs for why the two dialects differ here.
  ///
  /// Both halves are recovered rather than required, and both recoveries consume nothing: `on`
  /// lexes as an ordinary `Identifier`, so a name that is not `on` is far more likely to be the
  /// condition's type with the keyword missing than it is to be junk — and eating it would cost
  /// the very node a diagnostic wants to point at.
  ///
  /// When *neither* half is here the node is not opened at all. An empty zero-width
  /// `TypeCondition` would be a region covering nothing, and the caller — a fragment definition
  /// whose `on` is missing entirely — still has its selection set to parse.
  fn type_condition<'inp, Src, Ctx>(inp) {
    // The peek crosses the leading trivia before any node opens, so the condition starts at its
    // own first token. `peek_as` and `peek_kind` share the one skip and the one cached token.
    let on = peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::On);
    if !on && !matches!(
      peek_kind::<Src, Ctx>(inp)?,
      Some(Kind::Identifier | Kind::PathSeparator)
    ) {
      return recover::report_unexpected::<Src, Ctx>(inp, TYPE_CONDITION_HEADS);
    }
    node(
      K::TypeCondition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if on {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        } else {
          recover::report_unexpected::<Src, Ctx>(inp, TYPE_CONDITION_HEADS)?;
        }
        match peek_kind::<Src, Ctx>(inp)? {
          Some(Kind::Identifier | Kind::PathSeparator) => type_path::<Src, Ctx>(inp),
          // `on` with nothing after it. Reported, and again nothing is consumed: whatever is here
          // is the next component's, not this one's.
          _ => recover::report_unexpected::<Src, Ctx>(inp, TYPE_CONDITION_HEADS),
        }
      },
    )
    .parse_input(inp)
  }

  /// `Alias? Name Arguments? Directives? SelectionSet?`
  ///
  /// **Precondition: the head is an `Identifier`.** [`selection`] decides that, and its peek is
  /// what commits the field's leading trivia — so the alias mark and `Field`'s own mark are minted
  /// at the same position and nothing sits between them.
  fn field<'inp, Src, Ctx>(inp) {
    node(
      K::Field.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        // An inert mark — one buffer slot, promising nothing. Unspent, it materializes into
        // nothing, which is precisely what a field with no alias needs.
        let mark = inp.cst_mark();
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // `node_at(mark, kind, parser)` takes THREE args: it wraps a parser rather than being a
        // bare wrap instruction, and `try_eat` is the declining probe that decides whether the
        // mark is spent at all. The probe's own skip is what lets `a # c\n: b` still be an alias —
        // the `:` is decided long after the name was committed.
        if let ParseAttempt::Accept(()) = node_at(
          mark,
          K::Alias.raw(),
          |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
            try_eat::<Src, Ctx>(inp, Kind::Colon)
          },
        )
        .try_parse_input(inp)?
        {
          // The colon was there: the name already committed is retro-wrapped as `Alias` together
          // with the colon, and the real field name follows.
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        }
        // Each optional tail is dispatched on a peek rather than attempted, so its opener is
        // consumed *inside* the node it belongs to and an absent one opens nothing.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          arguments::<Src, Ctx>(inp, Constness::NonConst)?;
        }
        directives::<Src, Ctx>(inp, Constness::NonConst)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          selection_set::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `... TypePath Directives?` or `... TypeCondition? Directives? SelectionSet`
  ///
  /// **Precondition: the head is `...`.** [`selection`] decides that, and its peek is what commits
  /// the leading trivia — so this mark rides at the `...` itself and the retro-wrap starts at its
  /// own first token rather than at the previous selection's trailing whitespace.
  ///
  /// The node kind is not known when the `...` is committed, which is why the `...` is committed
  /// first and the mark spent afterwards. On the fall-through arm the mark is simply left unspent:
  /// there is no shape to name, so the `...` stays a direct child of the enclosing selection set
  /// rather than being wrapped in a node that claims a tail it does not have.
  fn spread_selection<'inp, Src, Ctx>(inp) {
    let mark = inp.cst_mark();
    expect::<Src, Ctx>(inp, Kind::Spread)?;
    match peek_kind::<Src, Ctx>(inp)? {
      // A name — and only its spelling separates the two shapes.
      Some(Kind::Identifier) => {
        if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::On) {
          node_at(
            mark,
            K::InlineFragment.raw(),
            |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
              type_condition::<Src, Ctx>(inp)?;
              directives::<Src, Ctx>(inp, Constness::NonConst)?;
              selection_set::<Src, Ctx>(inp)
            },
          )
          .parse_input(inp)
        } else {
          fragment_spread_tail::<Src, Ctx>(inp, mark)
        }
      }
      // GraphQLx only: a `::` after the `...` is a fully qualified fragment name. GraphQL has no
      // such head, so this arm has no counterpart to port and is the one a port would be missing —
      // after which `... ::ns::F` would report and become an `Error` node that still round-trips.
      Some(Kind::PathSeparator) => fragment_spread_tail::<Src, Ctx>(inp, mark),
      // No type condition, but a selection set (with or without directives) — an inline fragment
      // on the enclosing type.
      Some(Kind::At | Kind::LBrace) => node_at(
        mark,
        K::InlineFragment.raw(),
        |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
          directives::<Src, Ctx>(inp, Constness::NonConst)?;
          selection_set::<Src, Ctx>(inp)
        },
      )
      .parse_input(inp),
      // Nothing continues this `...`. Report and consume **nothing**: the token here — very often
      // the enclosing set's own `}` — belongs to the caller, and `unexpected` would eat it (a
      // closer is a sync point) and leave the set hunting a closer it had swallowed. Progress is
      // already guaranteed by the `...` this production consumed.
      _ => recover::report_unexpected::<Src, Ctx>(inp, SPREAD_TAIL_HEADS),
    }
  }

  /// The `TypePath Directives?` tail of a named fragment spread, retro-wrapped at `mark`.
  ///
  /// Two of [`spread_selection`]'s four arms reach it, which is the whole reason it is a
  /// production and not two copies: an `Identifier` head and a `PathSeparator` head differ only in
  /// where the path starts, and `type_path` already handles both.
  fn fragment_spread_tail<'inp, Src, Ctx>(inp, mark: tokora::cst::event::EventMark) {
    node_at(
      mark,
      K::FragmentSpread.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        type_path::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::NonConst)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on the selection head. Opens **no** node of its own; the chosen production opens its
  /// own — and this peek is what commits the leading trivia both of them rely on.
  fn selection<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier) => field::<Src, Ctx>(inp),
      Some(Kind::Spread) => spread_selection::<Src, Ctx>(inp),
      _ => recover::unexpected::<Src, Ctx>(inp, SELECTION_HEADS),
    }
  }

  /// `{ Selection+ }`
  ///
  /// The loop terminates because every arm consumes at least one token whenever input remains:
  /// [`selection`] dispatches only on heads its arms consume, and `unexpected` guarantees progress
  /// on everything else.
  fn selection_set<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::SelectionSet.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        // `SelectionSet: { Selection+ }` — the empty form is reported, and the report consumes
        // nothing so the `}` is still the loop's to eat. Checked here rather than after the loop,
        // because afterwards the diagnostic would point past the closer it is about.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, SELECTION_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            // Unterminated at end of input: report and return `Ok`, so the enclosing `node` still
            // closes and the rest of the file keeps its structure.
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::Spread) => selection::<Src, Ctx>(inp)?,
            // The head is checked here rather than left to `selection`'s own dispatch, so the two
            // agree on which heads reach a production at all; `selection`'s fall-through stays as
            // the guard for a caller that is not this loop.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, SELECTION_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one selection production over a `&str` and hand back the tree it built, for
  /// `tests/lossless_x_selection.rs`.
  mod test_support;

  /// `super::field` over `src`.
  fn parse_field => field;

  /// `super::type_condition` over `src` — the node GraphQL's kind space has no image for.
  fn parse_type_condition => type_condition;

  /// `super::selection` over `src` — the entry `selection_set` uses, and the only door to
  /// `spread_selection` and its four arms.
  fn parse_selection => selection;

  /// `super::selection_set` over `src`.
  fn parse_selection_set => selection_set;
}
