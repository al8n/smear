//! The seven type-system extensions, and the `extend` dispatcher over them.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Every extension is a retro-wrap, and the reason is the `extend`, not a description
//!
//! An extension's node kind is decided by the keyword **after** `extend`, so the `extend` is
//! committed before the kind is known and the node is opened at a mark minted in front of it.
//! Definitions retro-wrap for a description; these retro-wrap for a shape keyword, and the
//! mechanism is the same three events either way.
//!
//! **An extension is never described.** Divergence 22: `entry_after_description` commits to a
//! *definition* and never reaches an extension (`graphqlx/syntactic/document.rs:210-221`), so
//! `"doc" extend type T` is a grammar error rather than a described extension. The check is the
//! document's and is written there, because that is the only place the two are adjacent.
//!
//! # Every extension needs at least one component, and each one says so differently
//!
//! `syntactic/`'s six tails each end in a `(None, None, …) => Err` arm naming its own expectation,
//! and a scalar extension goes further: its directives are **mandatory**, not merely one of
//! several alternatives (`extension.rs:48-51`). A bare `extend type T` re-prints as perfectly
//! valid text, so the verdict is the only witness — which is why the emptiness check is written
//! out at every site rather than left to the components' own optionality.
//!
//! # Divergences 16 and 17 have their extension twins here
//!
//! `constrained_extension_fields` (`extension.rs:88`) and `constrained_extension_input_fields`
//! (`extension.rs:342`) are the two sites the design omitted from divergence 16's list, and
//! `extension.rs:215-217` is divergence 17's twin. All three are the same rules the definitions
//! carry, reached through the same two helpers, so a rule fixed at one site is fixed at both.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, cst::event::EventMark};

use crate::graphqlx::kinds::SyntaxKind as K;

// `node_at` comes from `coverage`, not from `tokora::parser`; see any sibling production file.
use super::coverage::node_at;

use super::{
  GraphqlxLosslessInput, Keyword,
  definition::{
    constrained_fields, constrained_union_members, enum_values_definition, implements_interfaces,
    root_operation_types_definition,
  },
  directive::directives,
  generic::extension_name,
  recover,
  recover::{EXTENSION_TAIL_HEADS, TYPE_EXTENSION_HEADS},
  trivia::{expect, peek_as, peek_kind},
  value::Constness,
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `extend scalar ExtensionName Directives[Const]`
  ///
  /// **Precondition: `mark` was minted before the `extend`, which is already committed, and the
  /// head is an `Identifier` spelled `scalar`.** [`type_system_extension`] establishes both.
  ///
  /// The directives are **required**, which is the one extension whose single component is not
  /// merely one alternative among several.
  fn scalar_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ScalarTypeExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        extension_name::<Src, Ctx>(inp)?;
        if peek_kind::<Src, Ctx>(inp)? != Some(Kind::At) {
          return recover::report_unexpected::<Src, Ctx>(inp, EXTENSION_TAIL_HEADS);
        }
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `extend type ExtensionName ImplementInterfaces? Directives[Const]? WhereClause?
  /// FieldsDefinition?`, at least one of them present.
  fn object_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ObjectTypeExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        extension_name::<Src, Ctx>(inp)?;
        object_extension_body::<Src, Ctx>(inp, false)
      },
    )
    .parse_input(inp)
  }

  /// The `interface` form of [`object_type_extension`]'s shape, sharing its body.
  fn interface_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InterfaceTypeExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        extension_name::<Src, Ctx>(inp)?;
        object_extension_body::<Src, Ctx>(inp, false)
      },
    )
    .parse_input(inp)
  }

  /// `extend input ExtensionName Directives[Const]? WhereClause? InputFieldsDefinition?`
  ///
  /// [`object_type_extension`]'s body without the `implements` clause, which the input-object
  /// grammar does not have.
  fn input_object_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InputObjectTypeExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        extension_name::<Src, Ctx>(inp)?;
        object_extension_body::<Src, Ctx>(inp, true)
      },
    )
    .parse_input(inp)
  }

  /// The tail an object, interface and input-object extension share. Opens no node of its own.
  ///
  /// `input_object` selects the block, exactly as
  /// [`constrained_fields`](super::definition::constrained_fields) does — that helper is the
  /// definitions' and is reached here rather than copied, which is what makes divergence 16 hold
  /// at all five of its sites by construction instead of by five separate edits.
  ///
  /// The emptiness check is `has_any`, computed from the three peeks rather than from the parse:
  /// each component may legitimately be absent and only their *conjunction* is an error, which is
  /// the same cross-component shape as the `where` rule and is invisible to the text.
  fn object_extension_body<'inp, Src, Ctx>(inp, input_object: bool) {
    let implements = !input_object
      && peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Implements);
    if implements {
      implements_interfaces::<Src, Ctx>(inp)?;
    }
    let has_directives = peek_kind::<Src, Ctx>(inp)? == Some(Kind::At);
    directives::<Src, Ctx>(inp, Constness::Const)?;
    let constrained = peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Where);
    let has_block = constrained || peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace);
    constrained_fields::<Src, Ctx>(inp, input_object)?;
    if implements || has_directives || has_block {
      return Ok(());
    }
    recover::report_unexpected::<Src, Ctx>(inp, EXTENSION_TAIL_HEADS)
  }

  /// `extend union ExtensionName Directives[Const]? UnionMemberTypes? WhereClause?`
  ///
  /// Divergence 17's extension twin, reached through the same helper the definition uses.
  fn union_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::UnionTypeExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        extension_name::<Src, Ctx>(inp)?;
        let has_directives = peek_kind::<Src, Ctx>(inp)? == Some(Kind::At);
        directives::<Src, Ctx>(inp, Constness::Const)?;
        let has_members = peek_kind::<Src, Ctx>(inp)? == Some(Kind::Equal);
        constrained_union_members::<Src, Ctx>(inp)?;
        if has_directives || has_members {
          return Ok(());
        }
        recover::report_unexpected::<Src, Ctx>(inp, EXTENSION_TAIL_HEADS)
      },
    )
    .parse_input(inp)
  }

  /// `extend enum ExtensionName Directives[Const]? EnumValuesDefinition?`
  ///
  /// No `where` clause, exactly as the enum *definition* has none.
  fn enum_type_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::EnumTypeExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        extension_name::<Src, Ctx>(inp)?;
        let has_directives = peek_kind::<Src, Ctx>(inp)? == Some(Kind::At);
        directives::<Src, Ctx>(inp, Constness::Const)?;
        let has_values = peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace);
        if has_values {
          enum_values_definition::<Src, Ctx>(inp)?;
        }
        if has_directives || has_values {
          return Ok(());
        }
        recover::report_unexpected::<Src, Ctx>(inp, EXTENSION_TAIL_HEADS)
      },
    )
    .parse_input(inp)
  }

  /// `extend schema Directives[Const]? RootOperationTypesDefinition?`
  ///
  /// The one extension with **no name at all**, and the block is optional here where the schema
  /// *definition* requires it.
  fn schema_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::SchemaExtension.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        let has_directives = peek_kind::<Src, Ctx>(inp)? == Some(Kind::At);
        directives::<Src, Ctx>(inp, Constness::Const)?;
        let has_block = peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace);
        if has_block {
          root_operation_types_definition::<Src, Ctx>(inp)?;
        }
        if has_directives || has_block {
          return Ok(());
        }
        recover::report_unexpected::<Src, Ctx>(inp, EXTENSION_TAIL_HEADS)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on the shape keyword after `extend`, spending `mark` on the node it chooses. Opens
  /// **no** node of its own.
  ///
  /// **Precondition: `mark` was minted before the `extend`, which is already committed.** Both the
  /// driver and the document dispatchers establish that; the keyword is what decides the kind, so
  /// the `extend` cannot be inside a node opened before it is read.
  fn type_system_extension<'inp, Src, Ctx>(inp, mark: EventMark) {
    match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::Scalar) => scalar_type_extension::<Src, Ctx>(inp, mark),
      Some(Keyword::Type) => object_type_extension::<Src, Ctx>(inp, mark),
      Some(Keyword::Interface) => interface_type_extension::<Src, Ctx>(inp, mark),
      Some(Keyword::Union) => union_type_extension::<Src, Ctx>(inp, mark),
      Some(Keyword::Enum) => enum_type_extension::<Src, Ctx>(inp, mark),
      Some(Keyword::Input) => input_object_type_extension::<Src, Ctx>(inp, mark),
      Some(Keyword::Schema) => schema_extension::<Src, Ctx>(inp, mark),
      // A name that is none of the seven, or no name at all. Reported and skipped — `unexpected`
      // consumes at least one token whenever input remains, which is the document loops' only
      // termination argument. The mark is left unspent and materializes into nothing.
      _ => recover::unexpected::<Src, Ctx>(inp, TYPE_EXTENSION_HEADS),
    }
  }

  /// `extend TypeSystemExtension` — the whole shape, from its own keyword.
  ///
  /// The mark is minted here, so a caller that has already read the `extend` uses
  /// [`type_system_extension`] instead. Only the drivers and a direct consumer reach this door;
  /// `document.rs`'s dispatchers mint their own mark before the `extend`, because the same mark
  /// has to be able to cover a description on the *other* branch.
  // "Only the drivers and a direct consumer" is one caller, the drivers being `pub(crate)`'s only
  // reach — so the gate takes it. See `lossless_drivers!`.
  #[cfg_attr(not(feature = "test-support"), allow(dead_code))]
  fn extension<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    expect::<Src, Ctx>(inp, Kind::Identifier)?;
    type_system_extension::<Src, Ctx>(inp, mark)
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one extension over a `&str` and hand back the tree it built, for
  /// `tests/lossless_x_document.rs`.
  mod test_support;

  /// `super::extension` over `src` — the entry every extension position uses, and the only door
  /// to the seven `*_type_extension` productions.
  fn parse_type_system_extension => extension;

  /// `super::extension` over `src` again, under the name divergence 17's test reads.
  fn parse_union_type_extension => extension;
}
