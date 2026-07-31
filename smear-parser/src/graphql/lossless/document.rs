//! Document-level productions: the seven type-system extensions, the two document roots, and
//! the dispatchers that choose between everything this suite can parse.
//!
//! The conventions are `value.rs`'s and are not repeated here — the two kind spaces, the
//! `::<Src, Ctx>` on every generic call, and the fully spelled `node(…)` closure parameter.
//!
//! # An extension is a retro-wrap, for the same reason a described definition is
//!
//! `extend` says only that *some* extension follows; the keyword after it decides which. The
//! atom set has no two-token peek, so `type_system_extension` mints a mark, commits the
//! `extend`, reads the shape keyword and hands the mark to the production it chose — which
//! spends it, so the `extend` lands **inside** its extension node rather than beside it.
//! `apollo-parser` reaches the same tree with `peek_data_n(2)`; see `definition.rs`'s module
//! docs for why that road is closed here.
//!
//! The mark threads through from the *document* dispatcher rather than being minted by
//! `type_system_extension` itself, so a description written before an `extend` — which
//! `syntactic/` rejects, and which is reported here — still lands inside the extension it was
//! written for instead of orphaned beside it.
//!
//! # This is the suite's only catch-resync-continue point
//!
//! Every other production propagates a genuinely unrecoverable `Err` — `argument`'s missing
//! `:`, `field_definition`'s missing type — because the enclosing shape has nothing left to
//! recover into. The two document loops are where that unwinding stops: they catch the `Err`,
//! call `recover::resync_to_definition`, and carry on with the next definition.
//!
//! Two things had to be true for that to work, and neither was before this task:
//!
//! - **The failure has to have been reported.** `expect` now emits at
//!   the point of failure as well as returning; before, a parse that failed outright recorded
//!   no diagnostic at all and [`Parse::has_errors`](super::Parse::has_errors) read `false` —
//!   which is the verdict the acceptance-parity gate compares against `syntactic/`.
//! - **The remainder has to be committed.** `document_entry` drains whatever an escaping
//!   `Err` left behind, because `Sink::finish` refuses any source byte that no committed token
//!   covers and no lexer-error diagnostic explains (`FinishError::UncoveredGap`). The old
//!   `parse_str` bound the driver's result to `_out` and drained nothing, so the first
//!   reachable `Err` would have been a panic in materialization rather than a reportable parse.
//!
//! # A document ends with its trailing trivia inside it
//!
//! Task 6's law, at the top level: the loop's terminating peek must cross the trailing trivia
//! to learn no further definition follows, and it crosses it while the node is open. Here that
//! is not even a compromise — a document *is* the whole file, leading and trailing trivia
//! included, so the node covering every byte is the right answer rather than a tolerated one.
//!
//! # Divergences from `apollo-parser`, decided rather than inherited
//!
//! `apollo-parser`'s `grammar/document.rs` and `grammar/extensions.rs` agree on the dispatch
//! and on all seven extension shapes. They differ in three places:
//!
//! - **An extension with no body is reported.** apollo accepts `extend type T` (its optional
//!   components are each independently optional); `syntactic/`'s `extension.rs` errors on every
//!   bare form, and gate 1 compares the two suites' verdicts input by input.
//! - **A description before an `extend` is reported.** `syntactic/`'s
//!   `described_definition_after_string` matches only the eight definition keywords, so the
//!   combination is rejected there. The extension is still parsed, so the tree keeps the nodes a
//!   diagnostic points at.
//! - **A failed definition costs one `Error` node, not the rest of the file.** apollo's
//!   `err_and_pop` drops one token per turn; the resync here skips the whole wreck as one
//!   nesting-aware region and reports it once.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{
  ParseInput as _,
  cst::event::EventMark,
  emitter::CstEmitter as _,
  parser::{node, node_at},
};

use crate::graphql::kinds::SyntaxKind as K;

use super::{
  GraphqlLosslessInput,
  definition::{
    description, enum_values_definition, fields_definition, implements_interfaces,
    input_fields_definition, root_operation_type_definitions, starts_description,
    type_system_definition, union_member_types,
  },
  directive::directives,
  executable::{fragment_definition, operation_definition},
  recover,
  recover::{
    BLOCK_EXTENSION_TAIL_HEADS, DEFINITION_HEADS, OBJECT_EXTENSION_TAIL_HEADS,
    SCALAR_EXTENSION_TAIL_HEADS, TYPE_EXTENSION_HEADS, TYPE_SYSTEM_DEFINITION_HEADS,
    UNION_EXTENSION_TAIL_HEADS,
  },
  trivia::{expect, peek_as, peek_kind},
  value::Constness,
};

lossless_production! {
  /// `extend scalar Name Directives`
  ///
  /// The one extension whose directives are **mandatory** — the grammar gives it no other
  /// tail — so an absent `@` is reported rather than skipped.
  ///
  /// **Precondition: `mark` rides at the `extend`, which is already committed, and the head is
  /// an `Identifier` spelled `scalar`.** [`type_system_extension`] establishes both.
  fn scalar_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ScalarTypeExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect_two_names::<Src, Ctx>(inp)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
          directives::<Src, Ctx>(inp, Constness::Const)
        } else {
          // Reported, and nothing is consumed: whatever is here starts the next definition.
          recover::report_unexpected::<Src, Ctx>(inp, SCALAR_EXTENSION_TAIL_HEADS)
        }
      },
    )
    .parse_input(inp)
  }

  /// `extend type Name (ImplementsInterfaces? Directives? FieldsDefinition | … | ImplementsInterfaces)`
  ///
  /// The three spec alternatives collapse to "each component is optional, but at least one must
  /// be present" — which is exactly what `syntactic/`'s `(None, None, None) => …` arm decides,
  /// and it is far easier to read that way than as three sequences sharing a prefix.
  fn object_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ObjectTypeExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect_two_names::<Src, Ctx>(inp)?;
        object_extension_body::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `extend interface Name …` — [`object_type_extension`]'s shape, keyword for keyword.
  fn interface_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InterfaceTypeExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect_two_names::<Src, Ctx>(inp)?;
        object_extension_body::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `ImplementsInterfaces? Directives? FieldsDefinition?`, at least one present. Opens no node.
  fn object_extension_body<'inp, Src, Ctx>(inp) {
    let mut present = false;
    if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::Implements) {
      implements_interfaces::<Src, Ctx>(inp)?;
      present = true;
    }
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
      directives::<Src, Ctx>(inp, Constness::Const)?;
      present = true;
    }
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      fields_definition::<Src, Ctx>(inp)?;
      present = true;
    }
    if !present {
      // Reported, and nothing is consumed: whatever is here starts the next definition.
      recover::report_unexpected::<Src, Ctx>(inp, OBJECT_EXTENSION_TAIL_HEADS)?;
    }
    Ok(())
  }

  /// `extend union Name (Directives? UnionMemberTypes | Directives)`
  fn union_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::UnionTypeExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect_two_names::<Src, Ctx>(inp)?;
        let mut present = false;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
          directives::<Src, Ctx>(inp, Constness::Const)?;
          present = true;
        }
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Equal) {
          union_member_types::<Src, Ctx>(inp)?;
          present = true;
        }
        if !present {
          recover::report_unexpected::<Src, Ctx>(inp, UNION_EXTENSION_TAIL_HEADS)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `extend enum Name (Directives? EnumValuesDefinition | Directives)`
  fn enum_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::EnumTypeExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect_two_names::<Src, Ctx>(inp)?;
        let mut present = false;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
          directives::<Src, Ctx>(inp, Constness::Const)?;
          present = true;
        }
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          enum_values_definition::<Src, Ctx>(inp)?;
          present = true;
        }
        if !present {
          recover::report_unexpected::<Src, Ctx>(inp, BLOCK_EXTENSION_TAIL_HEADS)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `extend input Name (Directives? InputFieldsDefinition | Directives)`
  fn input_object_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InputObjectTypeExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect_two_names::<Src, Ctx>(inp)?;
        let mut present = false;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
          directives::<Src, Ctx>(inp, Constness::Const)?;
          present = true;
        }
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          input_fields_definition::<Src, Ctx>(inp)?;
          present = true;
        }
        if !present {
          recover::report_unexpected::<Src, Ctx>(inp, BLOCK_EXTENSION_TAIL_HEADS)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `extend schema (Directives? RootOperationTypeDefinitions | Directives)`
  ///
  /// The only extension with no name of its own — a schema has nothing to be called.
  fn schema_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::SchemaExtension.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        let mut present = false;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
          directives::<Src, Ctx>(inp, Constness::Const)?;
          present = true;
        }
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          root_operation_type_definitions::<Src, Ctx>(inp)?;
          present = true;
        }
        if !present {
          recover::report_unexpected::<Src, Ctx>(inp, BLOCK_EXTENSION_TAIL_HEADS)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// The shape keyword and the extended type's name — the two `Identifier`s every extension but
  /// [`schema_extension`] opens with, once the `extend` is behind it.
  fn expect_two_names<'inp, Src, Ctx>(inp) {
    expect::<Src, Ctx>(inp, Kind::Identifier)?;
    expect::<Src, Ctx>(inp, Kind::Identifier)
  }

  /// `extend …` — consume the keyword, then dispatch on the shape keyword after it, spending
  /// `mark` on the node chosen. Opens **no** node of its own.
  ///
  /// **Precondition: the head is an `Identifier` spelled `extend`,** and `mark` was minted at or
  /// before it.
  fn type_system_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    expect::<Src, Ctx>(inp, Kind::Identifier)?;
    match peek_as::<Src, Ctx, ContextualKeyword>(inp)? {
      Some(ContextualKeyword::Scalar) => scalar_type_extension::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Type) => object_type_extension::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Interface) => interface_type_extension::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Union) => union_type_extension::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Enum) => enum_type_extension::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Input) => input_object_type_extension::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Schema) => schema_extension::<Src, Ctx>(inp, mark),
      // Nothing this `extend` can extend. The mark is left unspent — an unspent mark
      // materializes into nothing — so the `extend` stays a direct child of the document rather
      // than being wrapped in an extension node that names a shape it does not have. Progress is
      // already guaranteed by the keyword this production consumed.
      _ => recover::report_unexpected::<Src, Ctx>(inp, TYPE_EXTENSION_HEADS),
    }
  }

  /// One definition of any kind — executable, type-system, or extension. Opens **no** node of
  /// its own, and this is the one place a description is read for the mixed document.
  fn definition<'inp, Src, Ctx>(inp) {
    // The head peek first, so the leading trivia it crosses is committed BEFORE the mark and
    // lands outside whatever node the mark anchors — Task 6's ruling for `NonNullType`.
    //
    // **Measured, not assumed: that ordering is currently redundant here.** Every caller is a
    // document loop whose own `peek_kind` condition has already crossed the leading trivia, so
    // swapping these two lines moves nothing and no assertion goes red (mutation, silent-pass
    // mode 3). It is kept because the peek is load-bearing anyway — `starts_description` needs
    // its answer — and because a production whose node placement depends on a caller's internals
    // is one refactor away from being wrong.
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.emitter().cst_mark();
    let described = starts_description(head);
    if described {
      description::<Src, Ctx>(inp)?;
    }
    match peek_kind::<Src, Ctx>(inp)? {
      // The shorthand operation, which has no keyword at all — and which a description may
      // still precede: `syntactic/`'s described path routes through `definition`, whose own
      // first check is for the `{`. It is only `extend` that the described path never reaches.
      Some(Kind::LBrace) => operation_definition::<Src, Ctx>(inp, mark),
      Some(Kind::Identifier) => match peek_as::<Src, Ctx, ContextualKeyword>(inp)? {
        Some(
          ContextualKeyword::Query | ContextualKeyword::Mutation | ContextualKeyword::Subscription,
        ) => operation_definition::<Src, Ctx>(inp, mark),
        Some(ContextualKeyword::Fragment) => fragment_definition::<Src, Ctx>(inp, mark),
        Some(ContextualKeyword::Extend) => {
          if described {
            // `syntactic/`'s described path commits to a *definition* and never reaches an
            // extension, so the combination is rejected there and must be here. The extension
            // is still parsed: a lossless consumer needs the nodes to point the diagnostic at.
            recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_DEFINITION_HEADS)?;
          }
          type_system_extension::<Src, Ctx>(inp, mark)
        }
        _ => type_system_definition::<Src, Ctx>(inp, mark),
      },
      _ => recover::unexpected::<Src, Ctx>(inp, DEFINITION_HEADS),
    }
  }

  /// One type-system definition or extension. Opens no node of its own.
  ///
  /// [`definition`] without the three executable arms — which is the whole difference between a
  /// `TypeSystemDocument` and a `Document`.
  fn type_system_definition_or_extension<'inp, Src, Ctx>(inp) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.emitter().cst_mark();
    let described = starts_description(head);
    if described {
      description::<Src, Ctx>(inp)?;
    }
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier) => {
        if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::Extend) {
          if described {
            recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_DEFINITION_HEADS)?;
          }
          type_system_extension::<Src, Ctx>(inp, mark)
        } else {
          type_system_definition::<Src, Ctx>(inp, mark)
        }
      }
      _ => recover::unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_DEFINITION_HEADS),
    }
  }

  /// `Definition+` — the root of a mixed executable-plus-type-system document.
  ///
  /// The empty form is reported: `syntactic/`'s `document` is `.at_least(1)` ("nonempty"), and
  /// gate 1 compares verdicts. The node is opened either way, so a caller always finds a
  /// document to walk.
  ///
  /// # Termination
  ///
  /// Every turn consumes at least one token whenever input remains. [`definition`] either parses
  /// one (each opening with a committed keyword, brace or string) or routes an unrecognised head
  /// to [`recover::unexpected`], which guarantees progress; and the catch arm goes through
  /// [`recover::resync_to_definition`], which guarantees it too. **Deleting either guarantee
  /// hangs the suite rather than failing it.**
  ///
  /// # Why the caught `Err` is dropped rather than propagated
  ///
  /// It has already been reported — [`expect`] emits at the point of
  /// failure — so the value carries nothing this loop can add, and propagating it would abandon
  /// every definition after the first mistake. That is the whole difference between a lossless
  /// parser and a recogniser.
  fn document<'inp, Src, Ctx>(inp) {
    node(
      K::Document.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, DEFINITION_HEADS);
        }
        // This peek is also what crosses the trailing trivia — see the module docs.
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          if definition::<Src, Ctx>(inp).is_err() {
            recover::resync_to_definition::<Src, Ctx>(inp)?;
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `TypeSystemDefinitionOrExtension+` — the SDL-only root.
  ///
  /// [`document`]'s loop over the SDL-only dispatcher, written out rather than shared: a
  /// higher-ranked `fn` parameter would be the only way to abstract over the two dispatchers,
  /// and it buys eight lines at the cost of a signature no reader can check at a glance.
  ///
  /// Not reachable from [`parse_str`](super::parse_str), which parses the mixed form; the driver
  /// under `test_support` is its entry, and a schema-only consumer would call it directly.
  fn type_system_document<'inp, Src, Ctx>(inp) {
    node(
      K::TypeSystemDocument.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_DEFINITION_HEADS);
        }
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          if type_system_definition_or_extension::<Src, Ctx>(inp).is_err() {
            recover::resync_to_definition::<Src, Ctx>(inp)?;
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// [`document`] plus the drain that makes materialization total.
  ///
  /// [`Sink::finish`](tokora::cst::Sink::finish) refuses any source byte that no committed token
  /// covers and no lexer-error diagnostic explains, and an `Err` escaping [`document`] leaves
  /// the rest of the source uncommitted. Draining here turns that into a reportable parse rather
  /// than a panic in [`parse_str`](super::parse_str) — the defect Task 5 recorded and could not
  /// reach, `document` having been a stub.
  fn document_entry<'inp, Src, Ctx>(inp) {
    let out = document::<Src, Ctx>(inp);
    inp.skip_while(|_| true)?;
    out
  }
}

lossless_drivers! {
  /// Drivers that run one document-level production over a `&str` and hand back the tree it
  /// built, for `tests/lossless_document.rs`.
  mod test_support;

  /// `super::type_system_document` over `src` — the SDL-only root, which
  /// [`parse_str`](super::super::parse_str) does not reach.
  fn parse_type_system_document => type_system_document;
}
