//! The three document roots and the entry dispatchers over them.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Divergence 22: a description may precede a **keyworded definition**, and nothing else
//!
//! `entry_after_description` commits to a *keyworded definition* — executable or type-system —
//! and never reaches a `use` item, an extension, or the shorthand operation, so
//! `"doc" use ~::x::*`, `"doc" extend type T @d` and `"doc" { f }` are all grammar errors.
//! The last of the three is the grammar's own: `OperationDefinition : Description? OperationType
//! … | SelectionSet` gives the second alternative no `Description?` slot. All three re-print byte
//! for byte and all three build every node they would have built undescribed, so **only the
//! verdict moves**: no round-trip gate and no golden tree can see the difference, which is why
//! the rule is written out at each dispatcher and tested by verdict.
//!
//! The offending entry is still parsed in full. A lossless consumer needs the nodes to point the
//! diagnostic at, and the alternative — refusing the entry — would cost the rest of the document to
//! a resync.
//!
//! **A module's description is its own**, as a definition's is: the mark the dispatcher minted
//! before the string is spent on the [`ModDefinition`](K::ModDefinition), so the string lands
//! inside it.
//!
//! # A module is an entry of two roots, and the third reports it
//!
//! The mixed and the SDL-only roots take a `mod` as an entry, and a module's body takes the SDL
//! root's entries. The executable root does not: there a `mod` is reported and the module is still
//! built, for the reason above, and the projection refuses it as a child that root has no place
//! for.
//!
//! # What bounds a module's nesting, and what bounds its body
//!
//! A body is the one recursive position here. It opens through `descend` before it reads its `{`,
//! so the parse's own nesting budget counts every level whatever the lexer's tally reads, and it
//! is entered only once its `{` has been peeked. Each turn of the body's loop runs an item, whose
//! head the item's own production consumes, or skips a head that starts no item through
//! `recover::unexpected`, which consumes at least one token; an item that returns `Err` unwinds to
//! the document root, which stays the only place a resync happens.
//!
//! # One mark, two branches, and that is what forces the ordering
//!
//! An entry's dispatcher mints its mark **before** anything is committed, because the same mark has
//! to be able to cover a description on the definition branch and the `extend` on the extension
//! branch. That is why `extension.rs`'s own `extension` production — which mints its own mark — is
//! not the door used here.
//!
//! # A document is the whole file
//!
//! Each root is an undelimited repetition, so its terminating peek crosses the trailing trivia
//! while the node is still open. Here that is the right answer rather than a tolerated one: a
//! document *is* the file, leading and trailing trivia included.
//!
//! # Why the door drains beside `document`
//!
//! The door behind [`super::parse_document`] discards its production's result, so an `Err`
//! escaping the document production leaves the rest of the source with no committed token.
//! Materialization is `Cst::finish_partial`, which would tile that tail as one `gap_kind` token
//! carrying the tail's original text: no committed token in it, and no lexer errors but those a
//! lookahead had already raised over it — a truncated, under-reported parse. The door drains
//! whatever an escape left behind — lexing it and committing its tokens, unless a refusal ended
//! the document — which is what reports the tail instead.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, cst::EventMark};

use crate::graphqlx::kinds::SyntaxKind as K;

use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, Keyword,
  definition::type_system_definition_at,
  executable::{executable_definition_at, starts_executable_keyword},
  extension::type_system_extension,
  recover,
  recover::{
    DEFINITION_HEADS, DESCRIBED_DEFINITION_HEADS, EXECUTABLE_ENTRY_HEADS, MOD_BODY_HEADS,
    MOD_ITEM_HEADS, NAME_HEADS, TYPE_SYSTEM_ENTRY_HEADS, is_closer, opener_span,
    starts_description,
  },
  trivia::{eat_if, expect, peek_as, peek_kind},
  use_tree::use_definition,
};

use crate::lossless::depth::{self, RootTurn};

use crate::lossless::lossless_production;

/// Whether `keyword` opens an item of a module's body: a `use` item, a module, a type-system
/// definition or an extension.
///
/// The SDL root's entries, which is what a body holds. An executable definition's keyword is not
/// among them, so a query or a fragment in a body is skipped as a head that starts no item.
#[inline]
const fn starts_mod_item(keyword: Option<Keyword>) -> bool {
  matches!(
    keyword,
    Some(
      Keyword::Use
        | Keyword::Mod
        | Keyword::Extend
        | Keyword::Schema
        | Keyword::Directive
        | Keyword::Scalar
        | Keyword::Type
        | Keyword::Interface
        | Keyword::Union
        | Keyword::Enum
        | Keyword::Input
    )
  )
}

lossless_production! {
  dialect = graphqlx::lossless;

  /// `Description? mod Name { ModItem* }`
  ///
  /// **Precondition: `mark` was minted before the description, which is already committed, and the
  /// head is an `Identifier` spelled `mod`.** The entry dispatchers establish both, and spending the
  /// mark here is what puts the description inside the module it belongs to.
  ///
  /// A missing name and a missing body are each reported without consuming, so whatever stands
  /// there is still the next entry's.
  fn mod_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ModDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        } else {
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        // The `{` is confirmed here, before the body is entered.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          return mod_body::<Src, Ctx>(inp);
        }
        recover::report_unexpected::<Src, Ctx>(inp, MOD_BODY_HEADS)
      },
    )
    .parse_input(inp)
  }

  /// `{ ModItem* }` — a module's body. Opens no node: the braces and the items are children of the
  /// module that holds them.
  ///
  /// **Precondition: the head is a `{` its caller has peeked.** The frame is taken first, so the
  /// nesting budget counts every body before its `{` is read. An empty body is legal — a module
  /// with nothing in it is a namespace — so nothing is reported for one.
  fn mod_body<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    expect::<Src, Ctx>(inp, Kind::LBrace)?;
    let open = opener_span(inp.span().end());
    loop {
      if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
        return Ok(());
      }
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_object::<Src, Ctx>(inp, open),
        Some(Kind::InlineString | Kind::BlockString) => type_system_item::<Src, Ctx>(inp)?,
        Some(Kind::Identifier) => {
          if starts_mod_item(peek_as::<Src, Ctx, Keyword>(inp)?) {
            type_system_item::<Src, Ctx>(inp)?
          } else {
            // An executable definition's keyword, or any other name. Consumes at least one token,
            // which is this loop's termination argument.
            recover::unexpected::<Src, Ctx>(inp, MOD_ITEM_HEADS)?
          }
        }
        // A punctuator no item starts with. Consumes at least one token, as above.
        Some(_) => recover::unexpected::<Src, Ctx>(inp, MOD_ITEM_HEADS)?,
      }
    }
  }

  /// One entry of a mixed [`document`]: a `use` item, a module, an extension, or a described
  /// definition.
  ///
  /// The `described` flag is what carries divergence 22 into the two arms that must refuse it. It
  /// is computed before the description is committed, because afterwards the head peek answers
  /// about the keyword rather than about the string.
  fn document_entry_item<'inp, Src, Ctx>(inp) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    let described = starts_description(head);
    if described {
      super::executable::description::<Src, Ctx>(inp)?;
    }
    match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::Use) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, DEFINITION_HEADS)?;
        }
        // The mark is left unspent: a `use` item is never described, so there is nothing for it
        // to wrap, and an unspent mark materializes into nothing.
        use_definition::<Src, Ctx>(inp)
      }
      // A module may be described, and the description is its own.
      Some(Keyword::Mod) => mod_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Extend) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, DEFINITION_HEADS)?;
        }
        // The mark **is** spent here even when a description was refused, so the `extend` lands
        // inside the extension it opens. A described extension therefore leaves its description
        // outside the node, which is the shape the diagnostic describes.
        let extend_mark = inp.cst_mark();
        super::trivia::expect::<Src, Ctx>(inp, Kind::Identifier)?;
        type_system_extension::<Src, Ctx>(inp, extend_mark)
      }
      _ => definition_after_description::<Src, Ctx>(inp, mark, described),
    }
  }

  /// The definition half of the mixed entry dispatcher: executable or type-system, spending
  /// `mark`. Opens no node of its own.
  fn definition_after_description<'inp, Src, Ctx>(inp, mark: EventMark, described: bool) {
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      // The shorthand operation, which has no keyword at all — and which a description may not
      // precede: `OperationDefinition : SelectionSet` is the one definition alternative with no
      // `Description?` slot, so `syntactic/`'s described path rejects the combination and this
      // one must. The operation is still parsed, for the reason the two arms above record.
      if described {
        recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_DEFINITION_HEADS)?;
      }
      return executable_definition_at::<Src, Ctx>(inp, mark);
    }
    let keyword = peek_as::<Src, Ctx, Keyword>(inp)?;
    if starts_executable_keyword(keyword) {
      return executable_definition_at::<Src, Ctx>(inp, mark);
    }
    if keyword.is_some() {
      return type_system_definition_at::<Src, Ctx>(inp, mark);
    }
    // A name that is none of the twelve, or no name at all. Reported and skipped — `unexpected`
    // consumes at least one token whenever input remains, which is the document loops' only
    // termination argument. The mark is left unspent and materializes into nothing.
    recover::unexpected::<Src, Ctx>(inp, DEFINITION_HEADS)
  }

  /// One entry of an [`executable_document`](super::executable::executable_document): a `use` item
  /// or a described executable definition.
  ///
  /// A `mod` is not an entry of this root, and it is reported here and still built — see the
  /// module docs.
  fn use_or_executable_definition<'inp, Src, Ctx>(inp) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    let described = starts_description(head);
    if described {
      super::executable::description::<Src, Ctx>(inp)?;
    }
    match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::Use) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, EXECUTABLE_ENTRY_HEADS)?;
        }
        return use_definition::<Src, Ctx>(inp);
      }
      Some(Keyword::Mod) => {
        recover::report_unexpected::<Src, Ctx>(inp, EXECUTABLE_ENTRY_HEADS)?;
        return mod_definition::<Src, Ctx>(inp, mark);
      }
      _ => {}
    }
    // The shorthand operation carries no `Description?` slot — see
    // [`definition_after_description`], which refuses the same pair from the mixed root.
    if described && peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_DEFINITION_HEADS)?;
    }
    executable_definition_at::<Src, Ctx>(inp, mark)
  }

  /// One entry of a [`type_system_document`], and one item of a module's body: a `use` item, a
  /// module, an extension, or a described type-system definition.
  fn type_system_item<'inp, Src, Ctx>(inp) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    let described = starts_description(head);
    if described {
      super::executable::description::<Src, Ctx>(inp)?;
    }
    match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::Use) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS)?;
        }
        use_definition::<Src, Ctx>(inp)
      }
      Some(Keyword::Mod) => mod_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Extend) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS)?;
        }
        let extend_mark = inp.cst_mark();
        super::trivia::expect::<Src, Ctx>(inp, Kind::Identifier)?;
        type_system_extension::<Src, Ctx>(inp, extend_mark)
      }
      Some(_) => type_system_definition_at::<Src, Ctx>(inp, mark),
      None => {
        // After a description, a closing delimiter is the closer of the body this item stands
        // in: reported and left, so a module ends where it was written. The description was this
        // turn's progress. With nothing consumed the head is skipped whatever it is, because the
        // roots' loops end only if every turn consumes.
        if described && peek_kind::<Src, Ctx>(inp)?.is_some_and(is_closer) {
          return recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS);
        }
        recover::unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS)
      }
    }
  }

  /// `DocumentItem+` — the mixed root [`super::parse_document`] parses.
  ///
  /// The empty form is reported: `syntactic/` rejects an empty input, and gate 1 compares verdicts.
  /// A failed entry is caught and resynchronised past, which is the only place in this suite that
  /// happens — every production below returns `Err` and lets it unwind to here.
  ///
  /// **Except a nesting refusal, which is a resource refusal rather than a syntax error and ends
  /// the document.** Resynchronising past one re-reads the abandoned nest at the document level
  /// and reports every closer of it — smear issue #169.
  /// [`depth::descend`](crate::lossless::depth::descend)'s `The refusal ends the document` note
  /// carries the reasoning and the measurements.
  ///
  /// The exception is a **call**, not a predicate written out here — smear issue #178. See
  /// GraphQL's `document` and [`depth::root_turn`](crate::lossless::depth::root_turn).
  ///
  /// `stop` carries that same per-entry verdict out to the door's drain, which must not
  /// re-derive it from a counter whose span is the whole root — smear PR #189, and GraphQL's
  /// `document` carries the mechanism.
  fn document<'inp, Src, Ctx>(inp, stop: &mut depth::RootStop) {
    node(
      K::Document.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, DEFINITION_HEADS);
        }
        // This peek is also what crosses the trailing trivia — see the module docs.
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          match depth::root_turn(inp, stop, document_entry_item::<Src, Ctx>) {
            RootTurn::Parsed { .. } => {}
            RootTurn::EndsTheDocument { error } => return Err(error),
            RootTurn::Recoverable { .. } => recover::resync_to_definition::<Src, Ctx>(inp)?,
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `TypeSystemItem+` — the SDL-only root.
  ///
  /// [`document`]'s loop over the SDL-only dispatcher, written out rather than shared: a
  /// higher-ranked `fn` parameter would be the only way to abstract over the three dispatchers, and
  /// it buys eight lines at the cost of a signature no reader can check at a glance.
  ///
  /// The SDL-only root, off [`super::parse_document`]'s mixed-form path.
  /// [`super::parse_type_system_document`] is its shipped entry point — the one a schema-only
  /// consumer calls so that an executable definition is rejected by the parser, at the parser's
  /// own position, rather than by hand afterwards.
  fn type_system_document<'inp, Src, Ctx>(inp, stop: &mut depth::RootStop) {
    node(
      K::TypeSystemDocument.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS);
        }
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          // The refusal arm [`document`]'s note explains, through the same call and the same slot.
          match depth::root_turn(inp, stop, type_system_item::<Src, Ctx>) {
            RootTurn::Parsed { .. } => {}
            RootTurn::EndsTheDocument { error } => return Err(error),
            RootTurn::Recoverable { .. } => recover::resync_to_definition::<Src, Ctx>(inp)?,
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }
}
