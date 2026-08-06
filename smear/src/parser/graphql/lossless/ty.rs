//! Type-reference productions: `NamedType`, `ListType` and the retro-wrapped `NonNullType`.
//!
//! The conventions are `value.rs`'s, and they are not repeated here: node kinds are
//! [`SyntaxKind`](crate::parser::graphql::kinds::SyntaxKind) (`K::…`) while every `expect(…)` argument
//! is a `LosslessTokenKind` (`Kind::…`); every generic call carries `::<Src, Ctx>`, because
//! `Src` reaches the input only through the associated-type projection `Src::Slice<'inp>` and
//! is therefore not inferable; and every `node(…)` closure spells its parameter in full,
//! because `ParseInput`'s blanket closure impl is higher-ranked in the input's second lifetime.
//!
//! # `NonNullType` is the one shape that is not a straight `node(…)`
//!
//! `Type := (NamedType | ListType) "!"?` — the `!` is a **suffix**, so the inner type is
//! already committed to the tree before anyone knows whether a wrapper exists. That is what
//! [`tokora::parser::node_at`] is for: mint an inert mark, parse the inner type, and
//! hand the mark to a *declining* parser that spends it only if the `!` is there. The mark and
//! the token that justifies spending it are then read by the same call, so no `?` can come
//! between them and strand a half-decided wrap. An unspent mark materializes into nothing.
//!
//! # Divergences from `apollo-parser`, decided rather than inherited
//!
//! `apollo-parser`'s `grammar/ty.rs` is the closest comparable implementation and agrees on the
//! retro-wrap (its `checkpoint.wrap_node(NON_NULL_TYPE)` is `node_at` by another name). It
//! differs in three places:
//!
//! - **The mark is minted after the head peek, not before it.** apollo takes its checkpoint
//!   first and skips ignored tokens inside the branch, so leading whitespace lands *inside* the
//!   `NonNullType`. Here the dispatch peek crosses the leading trivia first and the mark is
//!   minted after it, so a `NonNullType` starts at its own first token. Pinned by
//!   `trivia_does_not_change_a_type_reference_shape`, which asserts the node's text.
//! - **A head that starts no type is recovered, not returned as an error.** apollo pops the
//!   offending token and reports; this suite routes it to `recover::unexpected`, which
//!   reports *and* attributes the skipped region to an `Error` node, and — the load-bearing
//!   half — guarantees progress, so a `type_ref` called from inside a loop cannot stall.
//! - **`ListType` keeps looking for its `]`.** apollo parses the inner type and then `expect`s
//!   the bracket, so `[Int Foo]` leaves `Foo]` to the caller. Here junk between the element and
//!   the closer is attributed to an `Error` node and the list still closes on its own
//!   delimiter — the same trade `value.rs` makes inside a list, for the same reason: a
//!   malformed type should cost its own node, not the rest of the file.

use crate::lexer::graphql::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, TryParseInput as _};

use crate::parser::graphql::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlLosslessInput, recover,
  recover::{TYPE_HEADS, opener_span},
  trivia::{eat_if, expect, peek_kind, try_eat},
};

use crate::parser::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphql::lossless;

  /// `Name`
  ///
  /// # The trivia is committed before the node opens, and that is this production's own job
  ///
  /// `node(…)` opens the node and *then* runs the inner parser, so `expect`'s trivia skip would
  /// run inside a node that is already open and the leading trivia would land in the node's
  /// range — a `NamedType` spanning `" Q"` rather than `"Q"`. The leading [`peek_kind`] crosses
  /// it first, at a point where the enclosing node is still the innermost one open, so the
  /// trivia attaches beside the `NamedType` instead of inside it. It reads no answer, because
  /// there is no dispatch to make here: the skip *is* the point, and [`peek_kind`] is the atom
  /// set's only door to it.
  ///
  /// Six of `named_type`'s seven call sites already crossed that trivia at a dispatch peek of
  /// their own — [`type_ref`] below, and `implements_interfaces`, `union_member_types` and
  /// `type_condition`, each of which peeks for an `Identifier` before it delegates — so for them
  /// the skip finds nothing and the peek is served straight out of the cache. The seventh,
  /// `root_operation_type_definition`, delegates with no peek in front of it and is where the
  /// boundary landed on the wrong side of the space; gate 5 found it by eye and pinned it, and
  /// `a_root_operation_type_does_not_start_at_the_trivia_before_it` is the assertion that keeps
  /// it fixed.
  ///
  /// **Stated here rather than as a precondition on the callers**, which is the divergence worth
  /// naming: `field` and `implements_interfaces` document a head their caller must have peeked,
  /// and that is right for a production with one dispatcher. `named_type` has seven call sites
  /// across three files and is about to be copied wholesale into `graphqlx`, so the guarantee is
  /// cheaper to hold once, here, than to re-establish at every site that will ever reach it.
  fn named_type<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::NamedType.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `[ Type ]`
  ///
  /// Exactly one inner type, then the closer. The loop exists only for what may sit *between*
  /// them: it consumes nothing on the well-formed path, and on the malformed one it attributes
  /// each junk region to an `Error` node until the `]` or end of input arrives. `unexpected`
  /// consumes at least one token whenever input remains, which is this loop's whole termination
  /// argument.
  fn list_type<'inp, Src, Ctx>(inp) {
    node(
      K::ListType.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
        // The element type. `type_ref` recovers on a head that starts no type, so this call
        // reports rather than aborting the list — and it, too, always makes progress.
        type_ref::<Src, Ctx>(inp)?;
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBracket)? {
            return Ok(());
          }
          if peek_kind::<Src, Ctx>(inp)?.is_none() {
            // Unterminated at end of input: report and return `Ok`, so the enclosing `node`
            // still closes and the rest of the file keeps its structure. There is nothing left
            // to skip and nothing to wrap — see `recover::unclosed_list`.
            return recover::unclosed_list::<Src, Ctx>(inp, open);
          }
          recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS)?;
        }
      },
    )
    .parse_input(inp)
  }

  /// `(NamedType | ListType) "!"?`
  ///
  /// Opens no node of its own for the inner type — the chosen production opens that — and one
  /// `NonNullType` retro-wrap around it if a `!` follows.
  fn type_ref<'inp, Src, Ctx>(inp) {
    // The head peek first, so the leading trivia it crosses is committed BEFORE the mark and
    // therefore lands outside any wrap this call makes.
    let head = peek_kind::<Src, Ctx>(inp)?;
    // `cst_mark` is the emitter's operation, reached through the handle's own forwarder — the
    // emitter itself is not handed out. An inert mark: one buffer slot, promising nothing; an
    // unspent one materializes into nothing.
    let mark = inp.cst_mark();
    match head {
      Some(Kind::LBracket) => list_type::<Src, Ctx>(inp)?,
      Some(Kind::Identifier) => named_type::<Src, Ctx>(inp)?,
      // No type starts here. Report and consume, exactly as `value`'s dispatcher does: an `Err`
      // would abort every enclosing production, and the mark is simply left unspent — there is
      // no inner type for a `NonNullType` to wrap, so the `!` probe is skipped too.
      _ => return recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS),
    }
    // `node_at(mark, kind, parser)` takes THREE args: it wraps a parser rather than being a bare
    // wrap instruction, and `try_eat` is the declining probe that decides whether the mark is
    // spent at all. On a decline nothing is wrapped and no `NonNullType` exists.
    node_at(mark, K::NonNullType.raw(), |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
      try_eat::<Src, Ctx>(inp, Kind::Bang)
    })
    .try_parse_input(inp)?;
    Ok(())
  }
}

lossless_drivers! {
  dialect = graphql::lossless;

  /// Drivers that run one type-reference production over a `&str` and hand back the tree it
  /// built, for `tests/lossless_ty_directive.rs`.
  mod test_support;

  /// `super::type_ref` over `src` — the entry every type position uses, and the only door to
  /// `named_type`, `list_type` and the `NonNullType` retro-wrap.
  fn parse_type_ref => type_ref;
}
