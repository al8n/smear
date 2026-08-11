//! The three document roots and the entry dispatchers over them.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Divergence 22: a description may precede a **keyworded definition**, and nothing else
//!
//! `entry_after_description` commits to a *keyworded definition* — executable or type-system —
//! and never reaches an import, an extension, or the shorthand operation, so
//! `"doc" import * from "x"`, `"doc" extend type T @d` and `"doc" { f }` are all grammar errors.
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
//! # Why `document_entry` exists beside `document`
//!
//! [`super::parse_document`] discards its parser's result, so an `Err` escaping the document
//! production would leave the rest of the source uncommitted and `finish` would refuse it as an
//! `UncoveredGap`. The entry drains whatever an escape left behind, which turns the one failure
//! mode `parse_document` cannot report into a reportable parse.

use crate::lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, cst::event::EventMark};

use crate::parser::graphqlx::kinds::SyntaxKind as K;

use super::coverage::node;

use super::{
  GraphqlxLosslessInput, Keyword,
  definition::type_system_definition_at,
  executable::{executable_definition_at, starts_executable_keyword},
  extension::type_system_extension,
  import::import_definition,
  recover,
  recover::{
    DEFINITION_HEADS, DESCRIBED_DEFINITION_HEADS, EXECUTABLE_ENTRY_HEADS, TYPE_SYSTEM_ENTRY_HEADS,
    starts_description,
  },
  trivia::{peek_as, peek_kind},
};

use crate::parser::lossless::lossless_production;

lossless_production! {
  dialect = graphqlx::lossless;

  /// One entry of a mixed [`document`]: an import, an extension, or a described definition.
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
      Some(Keyword::Import) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, DEFINITION_HEADS)?;
        }
        // The mark is left unspent: an import is never described, so there is nothing for it to
        // wrap, and an unspent mark materializes into nothing.
        import_definition::<Src, Ctx>(inp)
      }
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

  /// One entry of an [`executable_document`](super::executable::executable_document): an import or
  /// a described executable definition.
  fn import_or_executable_definition<'inp, Src, Ctx>(inp) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    let described = starts_description(head);
    if described {
      super::executable::description::<Src, Ctx>(inp)?;
    }
    if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Import) {
      if described {
        recover::report_unexpected::<Src, Ctx>(inp, EXECUTABLE_ENTRY_HEADS)?;
      }
      return import_definition::<Src, Ctx>(inp);
    }
    // The shorthand operation carries no `Description?` slot — see
    // [`definition_after_description`], which refuses the same pair from the mixed root.
    if described && peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_DEFINITION_HEADS)?;
    }
    executable_definition_at::<Src, Ctx>(inp, mark)
  }

  /// One entry of a [`type_system_document`]: an import, an extension, or a described type-system
  /// definition.
  fn import_or_type_system_definition_or_extension<'inp, Src, Ctx>(inp) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    let described = starts_description(head);
    if described {
      super::executable::description::<Src, Ctx>(inp)?;
    }
    match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::Import) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS)?;
        }
        import_definition::<Src, Ctx>(inp)
      }
      Some(Keyword::Extend) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS)?;
        }
        let extend_mark = inp.cst_mark();
        super::trivia::expect::<Src, Ctx>(inp, Kind::Identifier)?;
        type_system_extension::<Src, Ctx>(inp, extend_mark)
      }
      Some(_) => type_system_definition_at::<Src, Ctx>(inp, mark),
      None => recover::unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS),
    }
  }

  /// `ImportOrDefinitionOrExtension+` — the mixed root [`super::parse_document`] parses.
  ///
  /// The empty form is reported: `syntactic/` rejects an empty input, and gate 1 compares verdicts.
  /// A failed entry is caught and resynchronised past, which is the only place in this suite that
  /// happens — every production below returns `Err` and lets it unwind to here.
  fn document<'inp, Src, Ctx>(inp) {
    node(
      K::Document.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, DEFINITION_HEADS);
        }
        // This peek is also what crosses the trailing trivia — see the module docs.
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          if document_entry_item::<Src, Ctx>(inp).is_err() {
            recover::resync_to_definition::<Src, Ctx>(inp)?;
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `ImportOrTypeSystemDefinitionOrExtension+` — the SDL-only root.
  ///
  /// [`document`]'s loop over the SDL-only dispatcher, written out rather than shared: a
  /// higher-ranked `fn` parameter would be the only way to abstract over the three dispatchers, and
  /// it buys eight lines at the cost of a signature no reader can check at a glance.
  ///
  /// The SDL-only root, off [`super::parse_document`]'s mixed-form path.
  /// [`super::parse_type_system_document`] is its shipped entry point — the one a schema-only
  /// consumer calls so that an executable definition is rejected by the parser, at the parser's
  /// own position, rather than by hand afterwards.
  fn type_system_document<'inp, Src, Ctx>(inp) {
    node(
      K::TypeSystemDocument.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_ENTRY_HEADS);
        }
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          if import_or_type_system_definition_or_extension::<Src, Ctx>(inp).is_err() {
            recover::resync_to_definition::<Src, Ctx>(inp)?;
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// [`document`], then a drain — the production [`super::parse_document`] applies.
  ///
  /// See the module docs for why the drain is not optional.
  fn document_entry<'inp, Src, Ctx>(inp) {
    let out = document::<Src, Ctx>(inp);
    inp.skip_while(|_| true)?;
    out
  }

  /// [`type_system_document`], then a drain — the production
  /// [`super::parse_type_system_document`] applies.
  ///
  /// See the module docs for why the drain is not optional; the SDL-only loop catches and
  /// resynchronises exactly as the mixed one does, so an `Err` can still escape it.
  fn type_system_document_entry<'inp, Src, Ctx>(inp) {
    let out = type_system_document::<Src, Ctx>(inp);
    inp.skip_while(|_| true)?;
    out
  }
}
