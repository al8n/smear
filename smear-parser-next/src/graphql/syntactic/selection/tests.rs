//! Selection production tests — `field`, `selection`, `selection_set`, and the
//! `...` fragment fork (`FragmentSpread` / `InlineFragment`).
//!
//! Mirrors the `argument`/`value` harness: every case drives the real GraphQL
//! syntactic lexer under a `Fatal<GraphqlErrors>` context, accept cases run the full
//! source matrix, reject cases assert the error family, and a table-driven oracle
//! pins the frozen `smear-parser` `parse_field`/`parse_selection`/
//! `parse_selection_set` verdicts — excluding the empty-`{}` row, which parser-next
//! rejects per the spec-cardinality rule (plan Amendment 2).

use smear_lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, utils::cmp::Equivalent};

use super::{field, selection, selection_set};
use crate::graphql::{
  ast::{Field, Selection, SelectionSet},
  error::GraphqlErrors,
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_bytes(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`,
/// applying the generic `check` fn to each accepted AST — the source matrix.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

/// Asserts `parser` rejects `src` over both `str` and `[u8]`.
macro_rules! reject_all {
  ($parser:expr, $src:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $src).is_err(),
      "str should reject: {:?}",
      $src
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $src.as_bytes()).is_err(),
      "slice should reject: {:?}",
      $src
    );
  }};
}

// ─── field ─────────────────────────────────────────────────────────────────────

#[test]
fn field_accepts_bare_name() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    assert!(f.alias().is_none());
    assert!("name".equivalent(f.name().source_ref()));
    assert!(f.arguments().is_none());
    assert!(f.directives().is_none());
    assert!(f.selection_set().is_none());
    assert_eq!(f.span().start(), 0);
  }
  accept_all!(field, "name", check);
}

#[test]
fn field_accepts_alias() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    let alias = f.alias().expect("alias present");
    assert!("user".equivalent(alias.name().source_ref()));
    assert!("profile".equivalent(f.name().source_ref()));
  }
  accept_all!(field, "user: profile", check);
}

#[test]
fn field_accepts_arguments_and_directives() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    assert!("user".equivalent(f.name().source_ref()));
    assert_eq!(f.arguments().expect("args").arguments().len(), 1);
    assert_eq!(f.directives().expect("dirs").directives().len(), 1);
  }
  accept_all!(field, "user(id: 1) @include(if: true)", check);
}

#[test]
fn field_accepts_nested_selection_set() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    let ss = f.selection_set().expect("selection set");
    assert_eq!(ss.selections().len(), 2);
  }
  accept_all!(field, "user { id name }", check);
}

#[test]
fn field_accepts_alias_with_everything() {
  fn check<S: AsRef<[u8]>>(f: Field<S>) {
    assert!("u".equivalent(f.alias().expect("alias").name().source_ref()));
    assert!("user".equivalent(f.name().source_ref()));
    assert!(f.arguments().is_some());
    assert!(f.directives().is_some());
    assert_eq!(f.selection_set().expect("ss").selections().len(), 1);
  }
  accept_all!(field, "u: user(id: 1) @skip(if: false) { id }", check);
}

#[test]
fn field_rejects_missing_name() {
  reject_all!(field, "");
  reject_all!(field, "{ id }");
}

#[test]
fn field_rejects_alias_without_second_name() {
  reject_all!(field, "user:");
  reject_all!(field, "user: { id }");
}

// ─── selection ───────────────────────────────────────────────────────────────

#[test]
fn selection_dispatches_field() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    assert!(s.is_field());
  }
  accept_all!(selection, "name", check);
  accept_all!(selection, "alias: name(x: 1)", check);
}

#[test]
fn selection_dispatches_fragment_spread() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let fs = s.unwrap_fragment_spread_ref();
    assert!("UserFields".equivalent(fs.name().source_ref()));
  }
  accept_all!(selection, "...UserFields", check);
}

#[test]
fn selection_fragment_spread_with_directives() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let fs = s.unwrap_fragment_spread_ref();
    assert!(fs.directives().is_some());
  }
  accept_all!(selection, "...UserFields @include(if: true)", check);
}

#[test]
fn selection_dispatches_inline_fragment_with_type_condition() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    let tc = ifr.type_condition().expect("type condition");
    assert!("User".equivalent(tc.name().source_ref()));
    assert_eq!(ifr.selection_set().selections().len(), 1);
  }
  accept_all!(selection, "... on User { id }", check);
}

#[test]
fn selection_dispatches_inline_fragment_without_type_condition() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    assert!(ifr.type_condition().is_none());
    assert_eq!(ifr.selection_set().selections().len(), 1);
  }
  accept_all!(selection, "... { id }", check);
}

#[test]
fn selection_inline_fragment_with_directives_no_type_condition() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    assert!(ifr.type_condition().is_none());
    assert!(ifr.directives().is_some());
  }
  accept_all!(selection, "... @skip(if: false) { id }", check);
}

#[test]
fn selection_inline_fragment_with_type_condition_and_directives() {
  fn check<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    assert!(ifr.type_condition().is_some());
    assert!(ifr.directives().is_some());
  }
  accept_all!(selection, "... on User @skip(if: false) { id }", check);
}

#[test]
fn selection_rejects_bare_spread() {
  // `...` with no fragment name, type condition, `{`, or `@`.
  reject_all!(selection, "...");
  // Inline fragment with a type condition requires the type name and a selection set.
  reject_all!(selection, "... on");
  reject_all!(selection, "... on User");
  // A spread followed by a non-name (an int) is neither a fragment spread nor an
  // inline fragment.
  reject_all!(selection, "... 123");
}

#[test]
fn fragment_spread_named_on_is_unrepresentable() {
  // `FragmentName : Name but not on`, spread side. The `...`-fork rules `on` out
  // FIRST (`try_on` commits the inline-fragment arm), so a fragment spread named
  // `on` is structurally unrepresentable; the spread arm's `fragment_name` atom is
  // defense in depth. Behavior per input shape, pinned:
  //
  //   `... on X { f }`   -> inline fragment, type condition `X`
  //   `... on on { f }`  -> inline fragment, type condition `on` (the FIRST `on` is
  //                         always the keyword; `NamedType` has no exclusion)
  //   `... on { f }`     -> ERROR (inline arm committed; `{` is no type name) —
  //                         never a fragment spread named `on`
  //   `... on @d { f }`  -> ERROR (inline arm committed; `@` is no type name)
  //   `... on`           -> ERROR (end of input at the type name)
  fn tc_x<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    let tc = ifr.type_condition().expect("type condition");
    assert!("X".equivalent(tc.name().source_ref()));
  }
  fn tc_on<S: AsRef<[u8]>>(s: Selection<S>) {
    let ifr = s.unwrap_inline_fragment_ref();
    let tc = ifr.type_condition().expect("type condition");
    assert!("on".equivalent(tc.name().source_ref()));
  }
  accept_all!(selection, "... on X { f }", tc_x);
  accept_all!(selection, "... on on { f }", tc_on);
  reject_all!(selection, "... on { f }");
  reject_all!(selection, "... on @d { f }");
  reject_all!(selection, "... on");
}

// ─── selection_set ─────────────────────────────────────────────────────────────

#[test]
fn selection_set_accepts_single() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    assert_eq!(ss.selections().len(), 1);
    assert!(ss.selections()[0].is_field());
  }
  accept_all!(selection_set, "{ id }", check);
}

#[test]
fn selection_set_accepts_multiple_mixed() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    assert_eq!(ss.selections().len(), 3);
    assert!(ss.selections()[0].is_field());
    assert!(ss.selections()[1].is_fragment_spread());
    assert!(ss.selections()[2].is_inline_fragment());
  }
  accept_all!(selection_set, "{ id ...Frag ... on T { x } }", check);
}

#[test]
fn selection_set_accepts_commas_as_trivia() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    assert_eq!(ss.selections().len(), 2);
  }
  accept_all!(selection_set, "{ id, name }", check);
}

#[test]
fn selection_set_accepts_nested() {
  fn check<S: AsRef<[u8]>>(ss: SelectionSet<S>) {
    let outer = &ss.selections()[0];
    let f = outer.unwrap_field_ref();
    assert_eq!(f.selection_set().expect("nested").selections().len(), 2);
  }
  accept_all!(selection_set, "{ user { id name } }", check);
}

#[test]
fn selection_set_empty_braces_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `SelectionSet : { Selection+ }` demands
  // one-or-more, so an empty `{}` errors — a documented deviation from the frozen
  // parser, whose `while` loop accepted it.
  reject_all!(selection_set, "{}");
  reject_all!(selection_set, "{ }");
  // The rejection is the committed selection's unexpected-token at the `}`.
  let family = match drive_str(|inp| selection_set(inp).map(|_| ()), "{}") {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  };
  assert!(family);
}

#[test]
fn selection_set_rejects_unterminated() {
  reject_all!(selection_set, "{ id");
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_field` produces.
const FIELD_ORACLE: &[(&str, bool)] = &[
  ("name", true),
  ("user: profile", true),
  ("user(id: 1)", true),
  ("name @deprecated", true),
  ("user { id }", true),
  ("primaryUser: user(id: 1) @include(if: true) { id }", true),
  ("", false),
  ("user:", false),
  ("{ id }", false),
];

#[test]
fn field_matches_frozen_verdicts() {
  for (src, accept) in FIELD_ORACLE {
    assert_eq!(
      drive_str(|inp| field(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str field({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| field(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice field({src:?})"
    );
  }
}

/// Accept/reject verdicts for `selection` (field / spread / inline dispatch).
const SELECTION_ORACLE: &[(&str, bool)] = &[
  ("id", true),
  ("...Frag", true),
  ("...Frag @dir", true),
  ("... on User { id }", true),
  ("... { id }", true),
  ("... @skip(if: true) { id }", true),
  ("...", false),
  ("... on User", false),
];

#[test]
fn selection_matches_frozen_verdicts() {
  for (src, accept) in SELECTION_ORACLE {
    assert_eq!(
      drive_str(|inp| selection(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str selection({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| selection(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice selection({src:?})"
    );
  }
}

/// Accept/reject verdicts for `selection_set`. The oracle EXCLUDES the empty-`{}`
/// row: frozen accepts it, parser-next rejects it per the spec-cardinality rule
/// (plan Amendment 2) — the deviation is pinned by
/// `selection_set_empty_braces_error_per_spec`, not re-blessed here.
const SELECTION_SET_ORACLE: &[(&str, bool)] = &[
  ("{ id }", true),
  ("{ id name }", true),
  ("{ id, name }", true),
  ("{ user { id } }", true),
  ("{ ...Frag }", true),
  ("{ ... on T { x } }", true),
  ("{ id", false),
  ("id }", false),
];

#[test]
fn selection_set_matches_frozen_verdicts() {
  for (src, accept) in SELECTION_SET_ORACLE {
    assert_eq!(
      drive_str(|inp| selection_set(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str selection_set({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| selection_set(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice selection_set({src:?})"
    );
  }
}
