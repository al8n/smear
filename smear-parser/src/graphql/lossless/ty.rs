//! Type-reference productions: `NamedType`, `ListType` and the retro-wrapped `NonNullType`.
//!
//! The conventions are `value.rs`'s, and they are not repeated here: node kinds are
//! [`SyntaxKind`](crate::graphql::kinds::SyntaxKind) (`K::…`) while every `expect(…)` argument
//! is a `LosslessTokenKind` (`Kind::…`); every generic call carries `::<Src, Ctx>`, because
//! `Src` reaches the input only through the associated-type projection `Src::Slice<'inp>` and
//! is therefore not inferable; and every `node(…)` closure spells its parameter in full,
//! because `ParseInput`'s blanket closure impl is higher-ranked in the input's second lifetime.
//!
//! # `NonNullType` is the one shape that is not a straight `node(…)`
//!
//! `Type := (NamedType | ListType) "!"?` — the `!` is a **suffix**, so the inner type is
//! already committed to the tree before anyone knows whether a wrapper exists. That is what
//! [`node_at`] is for: mint an inert mark, parse the inner type, and
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

use smear_lexer::graphql::lossless::LosslessTokenKind as Kind;
use tokora::{
  ParseInput as _, TryParseInput as _,
  emitter::CstEmitter as _,
  parser::{node, node_at},
};

use crate::graphql::kinds::SyntaxKind as K;

use super::{
  GraphqlLosslessInput, recover,
  recover::{TYPE_HEADS, opener_span},
  trivia::{eat_if, expect, peek_kind, try_eat},
};

lossless_production! {
  /// `Name`
  fn named_type<'inp, Src, Ctx>(inp) {
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
    // `cst_mark` is on the EMITTER, not on the input, and is reached through `InputRef::emitter`.
    // An inert mark: one buffer slot, promising nothing; an unspent one materializes into
    // nothing.
    let mark = inp.emitter().cst_mark();
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
  /// Drivers that run one type-reference production over a `&str` and hand back the tree it
  /// built, for `tests/lossless_ty_directive.rs`.
  mod test_support;

  /// `super::type_ref` over `src` — the entry every type position uses, and the only door to
  /// `named_type`, `list_type` and the `NonNullType` retro-wrap.
  fn parse_type_ref => type_ref;
}
