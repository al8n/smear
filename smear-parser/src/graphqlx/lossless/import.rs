//! The import productions: `NamedSpecifier`, `WildcardSpecifier`, `ImportList` and
//! `ImportDefinition` — divergence 21.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Nothing in this file is a port
//!
//! GraphQL has no import statement at all, so there is no production to fork and no
//! `apollo-parser` counterpart to diverge from. Every shape below is read off
//! `graphqlx/syntactic/import.rs` directly.
//!
//! # Three of the six grammar names have no node, and that is the census's ruling
//!
//! `ImportClause` and `ImportMember` are **choices** — a clause is a wildcard or a list, a member
//! is a named or a wildcard specifier — and the tree records the alternative that won, so a
//! wrapper would nest exactly one child. `import`, `from` and `as` are **contextual keywords**,
//! ordinary identifier tokens to the lexer, and each is a bare token child of the node it opens.
//! What is left is the four node kinds this file builds.
//!
//! # The source narrows on the **token**, not on the node
//!
//! `import_definition_after_keyword` calls `inline_string_value`
//! (`graphqlx/syntactic/import.rs:315`), so `from """x"""` is a grammar error. The node above it is
//! a [`StringValue`](K::StringValue) either way: the census records exactly this for
//! `InlineStringValue` — "the import's `from` clause is the only position that narrows to it, and
//! it narrows on the token, not on the node". A block string is therefore reported **and still
//! built**, the same trade every other rejected-but-kept shape in this suite makes.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::graphqlx::kinds::SyntaxKind as K;

// `node` comes from `coverage`, not from `tokora::parser`. Behind `feature = "lossless-coverage"`
// it is that same combinator plus the per-node-kind hit counter gate 2 measures its reach with, so
// a production cannot open a node without being counted; without the feature it is tokora's own,
// re-exported unchanged.
use super::coverage::node;

use super::{
  GraphqlxLosslessInput, Keyword, recover,
  recover::{IMPORT_CLAUSE_HEADS, IMPORT_MEMBER_HEADS, IMPORT_SOURCE_HEADS, opener_span},
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::path,
  value::string_value,
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `(as Path)?` — the alias both specifier forms may carry. Opens no node of its own.
  ///
  /// The alias is a whole [`Path`](K::Path), not a name: `optional_alias` calls `path`
  /// (`graphqlx/syntactic/import.rs:100`), so `A as ns::B` renames into a namespace.
  fn optional_alias<'inp, Src, Ctx>(inp) {
    if peek_as::<Src, Ctx, Keyword>(inp)? != Some(Keyword::As) {
      return Ok(());
    }
    expect::<Src, Ctx>(inp, Kind::Identifier)?;
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier | Kind::PathSeparator) => path::<Src, Ctx>(inp),
      // `as` with nothing after it. Reported without consuming: whatever is here belongs to the
      // enclosing list, which is about to read it as the next member or as its own `}`.
      _ => recover::report_unexpected::<Src, Ctx>(inp, super::recover::PATH_HEADS),
    }
  }

  /// `Name (as Path)?`
  ///
  /// **Precondition: the head is an `Identifier`.** Both callers dispatch on it, and their peek is
  /// what commits the leading trivia so the node starts at its own first token.
  fn named_specifier<'inp, Src, Ctx>(inp) {
    node(
      K::NamedSpecifier.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        optional_alias::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `* (as Path)?`
  ///
  /// **Precondition: the head is `*`.** The wildcard reaches the tree from two positions — a
  /// member of a list, and a whole clause — and it is the same node in both.
  fn wildcard_specifier<'inp, Src, Ctx>(inp) {
    node(
      K::WildcardSpecifier.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Asterisk)?;
        optional_alias::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on an import member's head. Opens **no** node of its own; the chosen specifier opens
  /// its own — and this peek is what commits the leading trivia both rely on.
  fn import_member<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier) => named_specifier::<Src, Ctx>(inp),
      Some(Kind::Asterisk) => wildcard_specifier::<Src, Ctx>(inp),
      _ => recover::unexpected::<Src, Ctx>(inp, IMPORT_MEMBER_HEADS),
    }
  }

  /// `{ ImportMember+ }`
  ///
  /// Nonempty (`import.rs:257` is `.at_least(1)`), and the empty form is reported with the
  /// non-consuming recovery so the `}` stays the loop's.
  fn import_list<'inp, Src, Ctx>(inp) {
    node(
      K::ImportList.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, IMPORT_MEMBER_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::Asterisk) => import_member::<Src, Ctx>(inp)?,
            // The head is checked here rather than left to `import_member`'s dispatch, so the two
            // agree on which heads reach a specifier at all.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, IMPORT_MEMBER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `WildcardSpecifier | ImportList` — a choice, so this opens **no** node of its own.
  ///
  /// The wildcard arm is the same [`wildcard_specifier`] a list member reaches, alias and all:
  /// `import * as ns from "x"` is a whole-module import renamed into `ns`.
  fn import_clause<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Asterisk) => wildcard_specifier::<Src, Ctx>(inp),
      Some(Kind::LBrace) => import_list::<Src, Ctx>(inp),
      _ => recover::unexpected::<Src, Ctx>(inp, IMPORT_CLAUSE_HEADS),
    }
  }

  /// `import ImportClause from InlineStringValue`
  ///
  /// **Precondition: the head is an `Identifier` spelled `import`.** Task 14's document
  /// dispatchers decide that; the keyword is consumed with a plain `expect` and not re-tested.
  ///
  /// An import carries **no description**: the census's divergence 22 says a description may
  /// precede only an executable or a type-system definition, which is why this production takes no
  /// mark and is not a retro-wrap.
  fn import_definition<'inp, Src, Ctx>(inp) {
    node(
      K::ImportDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        import_clause::<Src, Ctx>(inp)?;
        if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::From) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        } else {
          // Reported, and nothing is consumed: the string that may still be there is the source,
          // and eating it would cost the node a diagnostic wants to point at.
          recover::report_unexpected::<Src, Ctx>(inp, super::recover::NAME_HEADS)?;
        }
        match peek_kind::<Src, Ctx>(inp)? {
          Some(Kind::InlineString) => string_value::<Src, Ctx>(inp),
          // A block string: reported for the narrowing, and still built as a `StringValue` — the
          // narrowing is the token's and the node above it is the same either way.
          Some(Kind::BlockString) => {
            recover::report_unexpected::<Src, Ctx>(inp, IMPORT_SOURCE_HEADS)?;
            string_value::<Src, Ctx>(inp)
          }
          _ => recover::report_unexpected::<Src, Ctx>(inp, IMPORT_SOURCE_HEADS),
        }
      },
    )
    .parse_input(inp)
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one import production over a `&str` and hand back the tree it built, for
  /// `tests/lossless_x_import.rs`.
  mod test_support;

  /// `super::import_member` over `src` — the choice, and the only door to both specifiers.
  fn parse_import_member => import_member;

  /// `super::import_list` over `src`.
  fn parse_import_list => import_list;

  /// `super::import_clause` over `src` — the other choice.
  fn parse_import_clause => import_clause;

  /// `super::import_definition` over `src` — the entry Task 14's document dispatchers use.
  fn parse_import_definition => import_definition;
}
