//! Selection productions: `Alias`, `Field`, `SelectionSet`, `FragmentSpread`, `InlineFragment`.
//!
//! The conventions are `value.rs`'s and are not repeated here — the two kind spaces, the
//! `::<Src, Ctx>` on every generic call, and the fully spelled `node(…)` closure parameter.
//!
//! # The two ambiguities this file exists to resolve
//!
//! Both are decided by a token the grammar puts *after* the tokens it re-classifies, and
//! neither needs a second peek to settle:
//!
//! - **`Alias` versus a bare field name.** Both start with an `Identifier` and only the
//!   following `:` tells them apart. That is exactly the shape
//!   [`tokora::parser::node_at`] exists for: mint a mark, commit the name, and hand the mark to
//!   a *declining* probe that spends it only if the
//!   `:` is there. On a decline the mark is left unspent and no `Alias` node exists. An
//!   `eat_if` plus an unconditional wrap cannot express this — the `:` would be committed
//!   outside the wrap's parser, and a `?` between the two would strand a half-decided mark.
//! - **`FragmentSpread` versus `InlineFragment`.** Both start `...`, and the node kind is known
//!   only after the token *after* the `...` is read — and, for an `Identifier`, only after its
//!   **spelling** is read, since `on` is a contextual keyword the lexer hands back as an
//!   ordinary name. The same mark-plus-`node_at` idiom serves: the `...` is committed first and
//!   retro-wrapped once the kind is known, so it lands inside its node rather than beside it.
//!
//! **Neither needs a two-token peek**, and the atom set deliberately has none. A peek that
//! crossed trivia without committing it would hand the tree's ordering to a second buffering
//! layer beside the sink's own mark/rollback discipline.
//!
//! # A `Field` ends with its trailing trivia inside it
//!
//! Task 6's `Directives` established the law and every shape with an optional tail inherits it:
//! a `Field`'s last three components are all optional, so learning that they are absent means
//! crossing whatever follows the field — while the field's node is still open. A node's extent
//! is `[mark, now]`; `cst_start_at` takes a mark but `cst_finish` has no `_at` form, so a node
//! cannot be ended in the past. Text fidelity is untouched; only the node's own `text_range` is
//! one trivia run longer. `SelectionSet` is immune, its `}` ending the node before anything
//! after it is read. Pinned by `a_field_ends_with_its_trailing_trivia_inside_it`.
//!
//! # Divergences from `apollo-parser`, decided rather than inherited
//!
//! `apollo-parser`'s `grammar/selection.rs`, `grammar/field.rs` and `grammar/fragment.rs` are
//! the closest comparable implementations and agree on the shape of all five productions. They
//! differ in four places:
//!
//! - **There is no `TypeCondition` node.** apollo wraps `on NamedType` in a `TYPE_CONDITION`
//!   node; this suite's kind space has no image for one, so the condition surfaces as the
//!   `NamedType` after the keyword and `type_condition` opens nothing. A typed accessor
//!   reaching for the condition reads that `NamedType` — one node fewer, and the `on` token is
//!   still there for a formatter to find.
//! - **An empty `{}` is reported.** apollo reports it too, but the load-bearing reason here is
//!   that `syntactic/` does: gate 1 compares the two suites' verdicts input by input. It is the
//!   opposite ruling from `Arguments`, where `syntactic/` documents the lenient `()` as
//!   accepted and Task 6 followed it — the two are decided one production at a time, against
//!   the sibling suite rather than against a single house rule.
//! - **A `...` with no tail consumes nothing.** apollo reports and moves on; here the report is
//!   `recover::report_unexpected`, which deliberately leaves the offending token for the
//!   enclosing selection set. Eating it would swallow the set's own `}`.
//! - **Junk inside a selection set is attributed to an `Error` node**, and a garbage *run* is
//!   skipped as one nesting-aware region — the same two divergences `value.rs` records for a
//!   list value, for the same reasons.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{ParseInput as _, TryParseInput as _, try_parse_input::ParseAttempt};

use crate::graphql::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlLosslessInput,
  directive::{arguments, directives},
  recover,
  recover::{SELECTION_HEADS, SPREAD_TAIL_HEADS, TYPE_CONDITION_HEADS, opener_span},
  trivia::{eat_if, expect, peek_as, peek_kind, try_eat},
  ty::named_type,
  value::Constness,
};

lossless_production! {
  /// `on NamedType` — the type condition of an inline fragment or a fragment definition.
  ///
  /// **Opens no node of its own**, the kind space having no `TypeCondition` image; the
  /// condition surfaces as the `NamedType` this production delegates to.
  ///
  /// Both halves are recovered rather than required, and both recoveries consume nothing:
  /// `on` lexes as an ordinary `Identifier`, so a name that is not `on` is far more likely to
  /// be the condition's type with the keyword missing than it is to be junk — and eating it
  /// would cost the very node a diagnostic wants to point at.
  fn type_condition<'inp, Src, Ctx>(inp) {
    // `peek_kind` cannot see a spelling, and `on` has no token kind of its own.
    if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::On) {
      expect::<Src, Ctx>(inp, Kind::Identifier)?;
    } else {
      recover::report_unexpected::<Src, Ctx>(inp, TYPE_CONDITION_HEADS)?;
    }
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
      named_type::<Src, Ctx>(inp)
    } else {
      // `on` with nothing after it. Reported, and again nothing is consumed: whatever is here
      // is the next component's, not this one's.
      recover::report_unexpected::<Src, Ctx>(inp, TYPE_CONDITION_HEADS)
    }
  }

  /// `Alias? Name Arguments? Directives? SelectionSet?`
  ///
  /// **Precondition: the head is an `Identifier`.** [`selection`] decides that, and its peek is
  /// what commits the field's leading trivia — so the alias mark and `Field`'s own mark are
  /// minted at the same position and nothing sits between them. The same precondition
  /// `boolean_value` states, for the same reason: re-reading the head here would answer a
  /// question already answered.
  fn field<'inp, Src, Ctx>(inp) {
    node(
      K::Field.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        // An inert mark — one buffer slot, promising nothing. Unspent, it materializes into
        // nothing, which is precisely what a field with no alias needs.
        let mark = inp.cst_mark();
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // `node_at(mark, kind, parser)` takes THREE args: it wraps a parser rather than being
        // a bare wrap instruction, and `try_eat` is the declining probe that decides whether
        // the mark is spent at all. The probe's own `skip_while` is what lets `a # c\n: b`
        // still be an alias — the `:` is decided long after the name was committed.
        if let ParseAttempt::Accept(()) = node_at(
          mark,
          K::Alias.raw(),
          |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
            try_eat::<Src, Ctx>(inp, Kind::Colon)
          },
        )
        .try_parse_input(inp)?
        {
          // The colon was there: the name already committed is retro-wrapped as `Alias`
          // together with the colon, and the real field name follows.
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

  /// `... FragmentName Directives?` or `... TypeCondition? Directives? SelectionSet`
  ///
  /// **Precondition: the head is `...`.** [`selection`] decides that, and its peek is what
  /// commits the leading trivia — so this mark rides at the `...` itself and the retro-wrap
  /// starts at its own first token rather than at the previous selection's trailing whitespace.
  ///
  /// The node kind is not known when the `...` is committed, which is why the `...` is
  /// committed first and the mark spent afterwards. On the fall-through arm the mark is simply
  /// left unspent: there is no shape to name, so the `...` stays a direct child of the
  /// enclosing selection set rather than being wrapped in a node that claims a tail it does not
  /// have.
  fn spread_selection<'inp, Src, Ctx>(inp) {
    let mark = inp.cst_mark();
    expect::<Src, Ctx>(inp, Kind::Spread)?;
    match peek_kind::<Src, Ctx>(inp)? {
      // A name — and only its spelling separates the two shapes.
      Some(Kind::Identifier) => {
        if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::On) {
          node_at(
            mark,
            K::InlineFragment.raw(),
            |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
              type_condition::<Src, Ctx>(inp)?;
              directives::<Src, Ctx>(inp, Constness::NonConst)?;
              selection_set::<Src, Ctx>(inp)
            },
          )
          .parse_input(inp)
        } else {
          node_at(
            mark,
            K::FragmentSpread.raw(),
            |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
              expect::<Src, Ctx>(inp, Kind::Identifier)?;
              directives::<Src, Ctx>(inp, Constness::NonConst)
            },
          )
          .parse_input(inp)
        }
      }
      // No type condition, but a selection set (with or without directives) — an inline
      // fragment on the enclosing type.
      Some(Kind::At | Kind::LBrace) => node_at(
        mark,
        K::InlineFragment.raw(),
        |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
          directives::<Src, Ctx>(inp, Constness::NonConst)?;
          selection_set::<Src, Ctx>(inp)
        },
      )
      .parse_input(inp),
      // Nothing continues this `...`. Report and consume **nothing**: the token here — very
      // often the enclosing set's own `}` — belongs to the caller, and `unexpected` would eat
      // it (a closer is a sync point) and leave the set hunting a closer it had swallowed.
      // Progress is already guaranteed by the `...` this production consumed.
      _ => recover::report_unexpected::<Src, Ctx>(inp, SPREAD_TAIL_HEADS),
    }
  }

  /// Dispatch on the selection head. Opens **no** node of its own; the chosen production opens
  /// its own — and this peek is what commits the leading trivia both of them rely on.
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
  /// `selection` dispatches only on heads its arms consume, and `unexpected` guarantees
  /// progress on everything else. Deleting that guarantee hangs the suite rather than failing
  /// it.
  fn selection_set<'inp, Src, Ctx>(inp) {
    node(
      K::SelectionSet.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        // `SelectionSet: { Selection+ }` — the empty form is reported, and the report consumes
        // nothing so the `}` is still the loop's to eat. Checked here rather than after the
        // loop, because afterwards the diagnostic would point past the closer it is about.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, SELECTION_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            // Unterminated at end of input: report and return `Ok`, so the enclosing `node`
            // still closes and the rest of the file keeps its structure. There is nothing left
            // to skip and nothing to wrap — see `recover::unclosed_object`.
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::Spread) => selection::<Src, Ctx>(inp)?,
            // The head is checked here rather than left to `selection`'s own dispatch, so the
            // two agree on which heads reach a production at all; `selection`'s fall-through
            // stays as the guard for a caller that is not this loop.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, SELECTION_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }
}

lossless_drivers! {
  /// Drivers that run one selection production over a `&str` and hand back the tree it built,
  /// for `tests/lossless_selection.rs`.
  mod test_support;

  /// `super::field` over `src`.
  fn parse_field => field;

  /// `super::selection` over `src` — the entry `selection_set` uses, and the only door to
  /// `spread_selection`.
  fn parse_selection => selection;

  /// `super::selection_set` over `src`.
  fn parse_selection_set => selection_set;
}
